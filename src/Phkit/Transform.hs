module Phkit.Transform (PhFwdRewrite
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
type PhFwdRewrite = CH.FwdRewrite CH.SimpleFuelMonad PhInstruction
type PhBwdRewrite = CH.BwdRewrite CH.SimpleFuelMonad PhInstruction

fwdRewriteResultOf :: LGA.Module -> PhFwdRewrite Bool -> LGA.Module
fwdRewriteResultOf modu rewrite =
  fwdTransformResultOf modu aOnceLattice aOnceTransfer rewrite

fwdTransformResultOf :: LGA.Module -> CH.DataflowLattice f -> PhFwdTransfer f
  -> PhFwdRewrite f -> LGA.Module
fwdTransformResultOf modu lattice transfer rewrite =
  let mPhModule = phModuleFromModule modu
      mPhFunctions = fmap phModuleFunctions mPhModule
      fwdPass = CH.FwdPass { CH.fp_lattice = lattice
                           , CH.fp_transfer = transfer
                           , CH.fp_rewrite = rewrite }
      transformFun fun@PhFunction {
        phFunctionEntry = fentry, phFunctionBody = fbody } = fun {
        phFunctionBody = (_firstIn3 $ runSimpleFuelMonad $
                          CH.analyzeAndRewriteFwd fwdPass (CH.JustC fentry)
                          fbody CH.noFacts) }
  in
    finalizeModule $ phModuleFromModule modu >>=
    \phM@PhModule{ phModuleFunctions = funcs } ->
      return $ phModuleToModule $
      phM { phModuleFunctions = map transformFun funcs }

_firstIn3 :: (a, b, c) -> a
_firstIn3 (a, _, _) = a

runSimpleFuelMonad :: CH.SimpleFuelMonad t -> t
runSimpleFuelMonad mT =
  CH.runSimpleUniqueMonad $ CH.runWithFuel CH.infiniteFuel mT
