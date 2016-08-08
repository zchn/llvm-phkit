{-# LANGUAGE GADTs #-}

module Phkit.Transform (PhFwdRewrite
                       ,bwdTransformResultOf
                       ,bwdRewriteResultOf
                       ,fwdTransformResultOf
                       ,fwdRewriteResultOf
                       ) where

import qualified Compiler.Hoopl as CH
import qualified LLVM.General.AST as LGA

import Phkit.Phire
import Phkit.Analysis


--------------------------------------------------------------------------------
-- Ph-specialized Types
--------------------------------------------------------------------------------
type PhFwdRewrite = CH.FwdRewrite NameLabelMapFuelM PhInstruction
type PhBwdRewrite = CH.BwdRewrite NameLabelMapFuelM PhInstruction

fwdRewriteResultOf :: LGA.Module -> PhFwdRewrite Bool -> LGA.Module
fwdRewriteResultOf modu =
  fwdTransformResultOf modu aOnceLattice aOnceFTransfer

fwdTransformResultOf :: LGA.Module -> CH.DataflowLattice f -> PhFwdTransfer f
  -> PhFwdRewrite f -> LGA.Module
fwdTransformResultOf modu lattice transfer rewrite =
  let mPhModule = phModuleFromModule modu
      mPhFunctions = fmap phModuleFunctions mPhModule
      fwdPass = CH.FwdPass { CH.fp_lattice = lattice
                           , CH.fp_transfer = transfer
                           , CH.fp_rewrite = rewrite }
      transformFun fun@PhFunction {
        phFunctionEntry = fentry, phFunctionBody = fbody } = do
        (newBody, _, _) <- CH.analyzeAndRewriteFwd fwdPass (CH.JustC fentry)
                           fbody CH.noFacts
        return fun { phFunctionBody = newBody }
  in
    finalizeModule $ CH.liftFuel (phModuleFromModule modu) >>=
    \phM@PhModule{ phModuleFunctions = funcs } -> do
      newFuncs <- mapM transformFun funcs
      return $ phModuleToModule $ phM { phModuleFunctions = newFuncs }


bwdRewriteResultOf :: LGA.Module -> PhBwdRewrite Bool -> LGA.Module
bwdRewriteResultOf modu =
  bwdTransformResultOf modu aOnceLattice aOnceBTransfer

bwdTransformResultOf :: LGA.Module -> CH.DataflowLattice f -> PhBwdTransfer f
  -> PhBwdRewrite f -> LGA.Module
bwdTransformResultOf modu lattice transfer rewrite =
  let mPhModule = phModuleFromModule modu
      mPhFunctions = fmap phModuleFunctions mPhModule
      bwdPass = CH.BwdPass { CH.bp_lattice = lattice
                           , CH.bp_transfer = transfer
                           , CH.bp_rewrite = rewrite }
      transformFun fun@PhFunction {
        phFunctionEntry = fentry, phFunctionBody = fbody } = do
        (newBody, _, _) <- CH.analyzeAndRewriteBwd bwdPass (CH.JustC fentry)
                           fbody CH.noFacts
        return fun { phFunctionBody = newBody }
  in
    finalizeModule $ CH.liftFuel (phModuleFromModule modu) >>=
    \phM@PhModule{ phModuleFunctions = funcs } -> do
      newFuncs <- mapM transformFun funcs
      return $ phModuleToModule $ phM { phModuleFunctions = newFuncs }

_firstIn3 :: (a, b, c) -> a
_firstIn3 (a, _, _) = a

runSimpleFuelMonad :: CH.SimpleFuelMonad t -> t
runSimpleFuelMonad mT =
  CH.runSimpleUniqueMonad $ CH.runWithFuel CH.infiniteFuel mT
