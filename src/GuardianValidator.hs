{-# LANGUAGE TemplateHaskell #-}

module GuardianValidator (validator, PWitnessDatum (PWitnessDatum), PWitnessParametersD (..)) where

import Plutarch.Api.V2 (PAddress, PCurrencySymbol, PScriptHash, PScriptPurpose (PSpending), PTxInInfo, PValidator)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Prelude
import PlutusTx qualified

import Collection.Utils (phasScriptHash, pheadSingleton, ppositiveSymbolValueOf, ptryOwnInput, (#>))
import Plutarch.Api.V1.Address (PCredential (PScriptCredential))
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC)
import Plutarch.Lift (
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import PlutusLedgerApi.V2 (Address, BuiltinByteString, CurrencySymbol, ScriptHash)

data WitnessDatum = WitnessDatum
  { btcSent :: Integer
  , btcAddress :: BuiltinByteString
  , adaAddr :: Address
  }
  deriving stock (Generic, Show)
PlutusTx.unstableMakeIsData ''WitnessDatum

data PWitnessDatum (s :: S)
  = PWitnessDatum
      ( Term
          s
          ( PDataRecord
              '[ "bridgeAmt" ':= PInteger
               , "otherChainAddr" ':= PByteString
               , "cardanoAddr" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PWitnessDatum where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PWitnessDatum where
  type PLifted PWitnessDatum = WitnessDatum

deriving via
  (DerivePConstantViaData WitnessDatum PWitnessDatum)
  instance
    (PConstantDecl WitnessDatum)

instance PTryFrom PData PWitnessDatum

data GuardianRedeemer
  = ApproveWrap
  | DenyWrap
  deriving stock (Generic, Show)
PlutusTx.unstableMakeIsData ''GuardianRedeemer

-- FIXME: ID-302 Redeemer is not used in validator

data PGuardianRedeemer (s :: S)
  = PApproveWrap (Term s (PDataRecord '[]))
  | PDenyWrap (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PGuardianRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PGuardianRedeemer where
  type PLifted PGuardianRedeemer = GuardianRedeemer

deriving via
  (DerivePConstantViaData GuardianRedeemer PGuardianRedeemer)
  instance
    (PConstantDecl GuardianRedeemer)

data WitnessParameters = WitnessParameters
  { multisigVH :: ScriptHash
  , multisigCert :: CurrencySymbol
  }
  deriving stock (Generic, Show)

data PWitnessParametersD (s :: S)
  = PWitnessParametersD
      ( Term
          s
          ( PDataRecord
              '[ "multisigVH" ':= PAsData PScriptHash
               , "multisigCert" ':= PAsData PCurrencySymbol
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PWitnessParametersD where
  type DPTStrat _ = PlutusTypeData

-- FIXME: ID-302 Redeemer is not used, therefore this validator can be used to aprove and deny wrap requests when an input datum has invalid information
validator :: Term s ((PAsData PScriptHash) :--> (PAsData PCurrencySymbol) :--> PValidator)
validator = phoistAcyclic $
  plam $ \multisigVH multisigCert _ _ ctx -> unTermCont $ do
    contextFields <- pletFieldsC @["txInfo", "purpose"] ctx
    PSpending ownRef' <- pmatchC contextFields.purpose
    ownRef <- pletC $ pfield @"_0" # ownRef'

    txInfoFields <- pletFieldsC @'["inputs", "outputs"] contextFields.txInfo

    ownInput <- pletC $ ptryOwnInput # txInfoFields.inputs # ownRef
    ownInputFields <- pletFieldsC @'["address"] ownInput
    PScriptCredential ownInputScriptCredential <- pmatchC (pfield @"credential" # ownInputFields.address)
    ownValHash <- pletC (pfield @"_0" # ownInputScriptCredential)

    pmultisigValHash <- pletC $ pfromData multisigVH
    let noScriptOutputs = pnull #$ pfilter # (phasScriptHash # ownValHash) # txInfoFields.outputs
        -- FIXME: ID-104 multisig input could be a reference input instead
        isSigInp = plam (\txinp -> pletFields @'["resolved"] txinp $ \txInFields -> phasScriptHash # pmultisigValHash # txInFields.resolved)
        sigInput' :: Term _ PTxInInfo = pheadSingleton #$ pfilter # isSigInp # txInfoFields.inputs
        sigInput = pfield @"resolved" # sigInput'

    sigInputF <- pletFieldsC @'["value"] sigInput
    let checkSigInp = ppositiveSymbolValueOf # pfromData multisigCert # sigInputF.value #> 0

    pure $
      popaque $
        pif
          ( (ptraceIfFalse "GuardianValidator f1" noScriptOutputs)
              #&& (ptraceIfFalse "GuardianValidator f2" checkSigInp)
          )
          (pconstant ())
          perror
