{-# LANGUAGE GADTs #-}

module Phkit.Analysis
  ( PhBwdAnalysis(..)
  , PhBwdPass
  , PhBwdTransfer
  , PhFwdAnalysis(..)
  , PhFwdPass
  , PhFwdTransfer
  , aOnceLattice
  , aOnceBTransfer
  , aOnceFTransfer
  , fwdAnalysisResultOf
  , fwdAnalysisWith
  , bwdAnalysisResultOf
  , bwdAnalysisWith
  , iSetAnalysisResultOf
  , mkPhNameFactMapLattice
  , phInitWithNone
  ) where

import qualified Compiler.Hoopl as CH
import qualified Control.Monad as CM
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Debug.Trace as DT
import qualified LLVM.General.AST as LGA

import Phkit.Phire

--------------------------------------------------------------------------------
-- Ph-specialized Types
--------------------------------------------------------------------------------
type PhFwdPass = CH.FwdPass NameLabelMapFuelM PhInstruction

type PhBwdPass = CH.BwdPass NameLabelMapFuelM PhInstruction

type PhDataflowLattice = CH.DataflowLattice

type PhNameFactMapLattice f = CH.DataflowLattice (DM.Map LGA.Name f)

type PhFwdTransfer = CH.FwdTransfer PhInstruction

type PhBwdTransfer = CH.BwdTransfer PhInstruction

data PhFwdAnalysis f = PhFwdAnalysis
  { phFALattice :: PhDataflowLattice f
  , phFAInit :: PhFunctionParams -> Maybe f
  , phFATransfer :: PhFwdTransfer f
  }

data PhBwdAnalysis f = PhBwdAnalysis
  { phBALattice :: PhDataflowLattice f
  , phBAInit :: PhFunctionParams -> Maybe f
  , phBATransfer :: PhBwdTransfer f
  }

phInitWithNone :: PhFunctionParams -> Maybe f
phInitWithNone _ = Nothing

--------------------------------------------------------------------------------
-- Generic functions
--------------------------------------------------------------------------------
mkPhNameFactMapLattice :: CH.DataflowLattice f -> PhNameFactMapLattice f
mkPhNameFactMapLattice per_name_fact_lattice =
  CH.DataflowLattice
  { CH.fact_name = CH.fact_name per_name_fact_lattice ++ "NameMap"
  , CH.fact_bot = DM.empty
  , CH.fact_join =
    \label (CH.OldFact oldF) (CH.NewFact newF) ->
       DM.foldWithKey (foldValue label) (CH.NoChange, oldF) newF
  }
  where
    foldValue label k v (currChanged, currF) =
      let currV = DM.findWithDefault (CH.fact_bot per_name_fact_lattice) k currF
          (newChanged, newV) =
            CH.fact_join
              per_name_fact_lattice
              label
              (CH.OldFact currV)
              (CH.NewFact v)
      in case currChanged of
           CH.SomeChange -> (CH.SomeChange, DM.insert k newV currF)
           CH.NoChange -> (newChanged, DM.insert k newV currF)

fwdAnalysisResultOf :: LGA.Module
                    -> PhFwdAnalysis f
                    -> [(LGA.Name, DM.Map LGA.Name f)]
fwdAnalysisResultOf modu PhFwdAnalysis {phFALattice = lattice
                                       ,phFAInit = initFun
                                       ,phFATransfer = transfer} =
  let mPhModule = phModuleFromModule modu
      mPhFunctions = fmap phModuleFunctions mPhModule
      analysis = fwdAnalysisWith lattice transfer
      analyzeFun PhFunction {phFunctionName = fname
                            ,phFunctionEntry = fentry
                            ,phFunctionBody = fbody
                            ,phFunctionParams = fparams} =
        let initFact =
              case initFun fparams of
                Just f -> CH.mapSingleton fentry f
                Nothing -> CH.noFacts
        in do (_, fb, _) <-
                CH.analyzeAndRewriteFwd analysis (CH.JustC fentry) fbody initFact
              return (fname, fb)
      mFactBaseList = CH.liftFuel mPhFunctions >>= mapM analyzeFun
  in finalizeFactBase mFactBaseList

bwdAnalysisResultOf :: LGA.Module
                    -> PhBwdAnalysis f
                    -> [(LGA.Name, DM.Map LGA.Name f)]
bwdAnalysisResultOf modu PhBwdAnalysis {phBALattice = lattice
                                       ,phBAInit = initFun
                                       ,phBATransfer = transfer} =
  let mPhModule = phModuleFromModule modu
      mPhFunctions = fmap phModuleFunctions mPhModule
      analysis = bwdAnalysisWith lattice transfer
      analyzeFun PhFunction {phFunctionName = fname
                            ,phFunctionEntry = fentry
                            ,phFunctionBody = fbody
                            ,phFunctionParams = fparams} =
        let initFact =
              case initFun fparams of
                Just f -> CH.mapSingleton fentry f
                Nothing -> CH.noFacts
        in do (_, fb, _) <-
                CH.analyzeAndRewriteBwd analysis (CH.JustC fentry) fbody initFact
              return (fname, fb)
      mFactBaseList = CH.liftFuel mPhFunctions >>= mapM analyzeFun
  in finalizeFactBase mFactBaseList

fwdAnalysisWith :: CH.DataflowLattice f -> PhFwdTransfer f -> PhFwdPass f
fwdAnalysisWith lattice transfer =
  CH.FwdPass
  { CH.fp_lattice = lattice
  , CH.fp_transfer = transfer
  , CH.fp_rewrite = CH.noFwdRewrite
  }

bwdAnalysisWith :: CH.DataflowLattice f -> PhBwdTransfer f -> PhBwdPass f
bwdAnalysisWith lattice transfer =
  CH.BwdPass
  { CH.bp_lattice = lattice
  , CH.bp_transfer = transfer
  , CH.bp_rewrite = CH.noBwdRewrite
  }

-- dbgFwdAnalysisOf :: Show f => PhFwdPass f -> PhFwdPass f
-- dbgFwdAnalysisOf = CH.debugFwdJoins DT.trace (const True)
--------------------------------------------------------------------------------
-- Analyze-once analysis
--------------------------------------------------------------------------------
aOnceLattice :: CH.DataflowLattice Bool
aOnceLattice =
  CH.DataflowLattice
  { CH.fact_name = "Analyzed"
  , CH.fact_bot = False
  , CH.fact_join = joinFun
  }
  where
    joinFun _ (CH.OldFact old) (CH.NewFact new) =
      (CH.changeIf (old /= (old || new)), old || new)

aOnceFTransfer :: PhFwdTransfer Bool
aOnceFTransfer = CH.mkFTransfer trans
  where
    trans :: PhInstruction e x -> Bool -> CH.Fact x Bool
    trans NameInsn {} _ = True
    trans InsnInsn {} _ = True
    trans n@(TermInsn {}) _ = CH.distributeFact n True

aOnceBTransfer :: PhBwdTransfer Bool
aOnceBTransfer = CH.mkBTransfer trans
  where
    trans :: PhInstruction e x -> CH.Fact x Bool -> Bool
    trans NameInsn {} _ = True
    trans InsnInsn {} _ = True
    trans TermInsn {} _ = True

--------------------------------------------------------------------------------
-- Instruction set analysis
--------------------------------------------------------------------------------
type InsnStringSet = DS.Set String

iSetAnalysisResultOf :: LGA.Module -> InsnStringSet
iSetAnalysisResultOf theModule =
  DS.unions $
  concatMap (DM.elems . snd) $
  bwdAnalysisResultOf theModule $ PhBwdAnalysis iSetLattice phInitWithNone iSetTransfer

iSetLattice :: CH.DataflowLattice InsnStringSet
iSetLattice =
  CH.DataflowLattice
  { CH.fact_name = "Instruction string set"
  , CH.fact_bot = DS.empty
  , CH.fact_join = add
  }
  where
    add _ (CH.OldFact old) (CH.NewFact new) = (changed, combined)
      where
        combined = DS.union new old
        changed = CH.changeIf (DS.size combined > DS.size old)

iSetTransfer :: PhBwdTransfer InsnStringSet
iSetTransfer = CH.mkBTransfer collect
  where
    collect :: PhInstruction e x -> CH.Fact x InsnStringSet -> InsnStringSet
    collect (NameInsn _ _) f = f
    collect (InsnInsn (LGA.Do insn)) f = DS.insert (showInsn insn) f
    collect (InsnInsn (_ LGA.:= insn)) f = DS.insert (showInsn insn) f
    collect (TermInsn (LGA.Do term) _) fbase =
      DS.fromList $ showTerm term : concatMap DS.toAscList (CH.mapElems fbase)
    collect (TermInsn (_ LGA.:= term) _) fbase =
      DS.fromList $ showTerm term : concatMap DS.toAscList (CH.mapElems fbase)

showInsn :: LGA.Instruction -> String
showInsn LGA.Add {} = "Add"
showInsn LGA.FAdd {} = "FAdd"
showInsn LGA.Sub {} = "Sub"
showInsn LGA.FSub {} = "FSub"
showInsn LGA.Mul {} = "Mul"
showInsn LGA.FMul {} = "FMul"
showInsn LGA.UDiv {} = "UDiv"
showInsn LGA.SDiv {} = "SDiv"
showInsn LGA.FDiv {} = "FDiv"
showInsn LGA.URem {} = "URem"
showInsn LGA.SRem {} = "SRem"
showInsn LGA.FRem {} = "FRem"
showInsn LGA.Shl {} = "Shl"
showInsn LGA.LShr {} = "LShr"
showInsn LGA.AShr {} = "AShr"
showInsn LGA.And {} = "And"
showInsn LGA.Or {} = "Or"
showInsn LGA.Xor {} = "Xor"
showInsn LGA.Alloca {} = "Alloca"
showInsn LGA.Load {} = "Load"
showInsn LGA.Store {} = "Store"
showInsn LGA.GetElementPtr {} = "GetElementPtr"
showInsn LGA.Fence {} = "Fence"
showInsn LGA.CmpXchg {} = "CmpXchg"
showInsn LGA.AtomicRMW {} = "AtomicRMW"
showInsn LGA.Trunc {} = "Trunc"
showInsn LGA.ZExt {} = "ZExt"
showInsn LGA.SExt {} = "SExt"
showInsn LGA.FPToUI {} = "FPToUI"
showInsn LGA.FPToSI {} = "FPToSI"
showInsn LGA.UIToFP {} = "UIToFP"
showInsn LGA.SIToFP {} = "SIToFP"
showInsn LGA.FPTrunc {} = "FPTrunc"
showInsn LGA.FPExt {} = "FPExt"
showInsn LGA.PtrToInt {} = "PtrToInt"
showInsn LGA.IntToPtr {} = "IntToPtr"
showInsn LGA.BitCast {} = "BitCast"
showInsn LGA.AddrSpaceCast {} = "AddrSpaceCast"
showInsn LGA.ICmp {} = "ICmp"
showInsn LGA.FCmp {} = "FCmp"
showInsn LGA.Phi {} = "Phi"
showInsn LGA.Call {} = "Call"
showInsn LGA.Select {} = "Select"
showInsn LGA.VAArg {} = "VAArg"
showInsn LGA.ExtractElement {} = "ExtractElement"
showInsn LGA.InsertElement {} = "InsertElement"
showInsn LGA.ShuffleVector {} = "ShuffleVector"
showInsn LGA.ExtractValue {} = "ExtractValue"
showInsn LGA.InsertValue {} = "InsertValue"
showInsn LGA.LandingPad {} = "LandingPad"

showTerm :: LGA.Terminator -> String
showTerm LGA.Ret {} = "Ret"
showTerm LGA.CondBr {} = "CondBr"
showTerm LGA.Br {} = "Br"
showTerm LGA.Switch {} = "Switch"
showTerm LGA.IndirectBr {} = "IndirectBr"
showTerm LGA.Invoke {} = "Invoke"
showTerm LGA.Resume {} = "Resume"
showTerm LGA.Unreachable {} = "Unreachable"

--------------------------------------------------------------------------------
-- Private helper functions
--------------------------------------------------------------------------------
_secondIn3 :: (a, b, c) -> b
_secondIn3 (_, b, _) = b
