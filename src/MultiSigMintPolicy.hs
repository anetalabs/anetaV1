{-# LANGUAGE TemplateHaskell #-}

module MultiSigMintPolicy (policy) where

import MultiSigValidator (PMultisigDatum)
import Plutarch.Api.V2 (PMintingPolicy, POutputDatum (POutputDatum), PPubKeyHash, PScriptHash, PScriptPurpose (PMinting), PTxOutRef)
import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext (pfromPDatum)
import Plutarch.Prelude

import Collection.Utils (phasInput, phasScriptHash, pheadSingleton, pnegativeSymbolValueOf, ppositiveSymbolValueOf)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC)

policy :: Term s ((PAsData PPubKeyHash) :--> (PAsData PScriptHash) :--> PTxOutRef :--> PMintingPolicy)
policy = phoistAcyclic $
  plam $ \guardianSignerPKH multisigVH oref _ context -> unTermCont $ do
    contextFields <- pletFieldsC @["txInfo", "purpose"] context
    PMinting ownPolicyId' <- pmatchC contextFields.purpose
    ownPolicyId <- pletC $ pfield @"_0" # ownPolicyId'
    txInfoFields <- pletFieldsC @["inputs", "outputs", "mint"] contextFields.txInfo
    mintedRTs <- pletC $ ppositiveSymbolValueOf # ownPolicyId # txInfoFields.mint
    burnedRTs <- pletC $ pnegativeSymbolValueOf # ownPolicyId # txInfoFields.mint

    let msOutput = pheadSingleton # (pfilter # (phasScriptHash # (pfromData multisigVH)) # txInfoFields.outputs)
    msOutputFields <- pletFieldsC @["value", "datum"] msOutput

    POutputDatum msOutputDatum' <- pmatchC msOutputFields.datum
    let msOutputDatum = pfromPDatum @PMultisigDatum # (pfield @"outputDatum" # msOutputDatum')
    msDatumF <- pletFieldsC @["keys", "requiredCount"] msOutputDatum
    let correctDatum = (pheadSingleton # msDatumF.keys #== guardianSignerPKH) #&& pfromData msDatumF.requiredCount #== 1
        isUtxoSpent = phasInput # txInfoFields.inputs # oref
    pure $
      popaque $
        pif
          ( ptraceIfFalse "MultiSigMintPolicy f1" (mintedRTs #== 1)
              #&& ptraceIfFalse "MultiSigMintPolicy f2" (burnedRTs #== 0)
              #&& ptraceIfFalse "MultiSigMintPolicy f3" (correctDatum)
              #&& ptraceIfFalse "MultiSigMintPolicy f4" (isUtxoSpent)
          )
          (pconstant ())
          perror
