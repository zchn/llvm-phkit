{-# LANGUAGE GADTs #-}

module Phkit.SoftBound.Transfer (sbTransfer) where

import qualified Compiler.Hoopl as CH
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Word as DW
import qualified LLVM.General.AST as LGA
import qualified LLVM.General.AST.CallingConvention as LGACa
import qualified LLVM.General.AST.Constant as LGAC
import qualified LLVM.General.AST.Global as LGAG
import qualified LLVM.General.AST.Instruction as LGAI
import qualified LLVM.General.AST.Operand as LGAO
import qualified LLVM.General.AST.Type as LGAT
import LLVM.General.AST (Named(..))
import Phkit.Analysis
import Phkit.Phire
import Phkit.Transform
import Phkit.SoftBound.Common
import Phkit.SoftBound.Lang
import Phkit.SoftBound.Lattice

sbTransfer :: PhFwdTransfer SbFunctionFact
sbTransfer = CH.mkFTransfer trans
  where
    trans :: PhInstruction e x -> SbFunctionFact -> CH.Fact x SbFunctionFact
    trans NameInsn{} f = f
    trans (InsnInsn (LGA.Do insn)) f = f
    -- just for debugging
    trans (InsnInsn (n := insn)) f = DM.insert n (SbTracked n) f
    -- just for debugging
    trans ti@(TermInsn (n := term) _) f = 
        let newF = DM.insert n (SbTracked n) f
        in CH.distributeFact ti newF
    trans ti@TermInsn{} f = CH.distributeFact ti f
