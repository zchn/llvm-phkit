{-# LANGUAGE GADTs #-}

module Phkit.SoftBound.Rewrite (sbRewrite) where

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

sbRewrite :: PhFwdRewrite SbFunctionFact
sbRewrite =
    CH.mkFRewrite
        (\phI f ->
              fmap Just $ addSbInsts phI f)

addSbInsts :: PhInstruction e x
  -> SbFunctionFact
  -> NameLabelMapFuelM (CH.Graph PhInstruction e x)
addSbInsts phI@NameInsn{} _ = return $ phGUnit phI
addSbInsts phI@InsnInsn{} f = do
  (prefix, prefixedPhI) <- addSbPrefix phI f
  (suffix, instedPhI) <- addSbSuffix prefixedPhI f
  return $ prefix `CH.catGraphNodeOO` instedPhI `CH.gSplice` suffix
addSbInsts phI@TermInsn{} f = do
  (prefix, newPhI) <- addSbPrefix phI f
  return $ prefix `CH.catGraphNodeOC` newPhI

addSbPrefix :: PhInstruction CH.O x
  -> SbFunctionFact
  -> NameLabelMapFuelM (CH.Graph PhInstruction CH.O CH.O,
                        PhInstruction CH.O x)
addSbPrefix phI@InsnInsn{} f =
  -- TODO: Implement
  return (CH.GNil, phI)
addSbPrefix phI@TermInsn{} f =
  -- TODO: Implement
  return (CH.GNil, phI)

addSbSuffix :: PhInstruction CH.O CH.O
  -> SbFunctionFact
  -> NameLabelMapFuelM (CH.Graph PhInstruction CH.O CH.O, PhInstruction CH.O CH.O)
addSbSuffix phI@InsnInsn{} f =
  -- TODO: Implement
  return (CH.GNil, phI)
