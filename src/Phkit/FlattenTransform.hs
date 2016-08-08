{-# LANGUAGE GADTs #-}

module Phkit.FlattenTransform (flattenResultOf) where


import qualified Compiler.Hoopl as CH
import qualified Control.Monad as CM
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Debug.Trace as DT
import qualified LLVM.General.AST as LGA
import qualified LLVM.General.AST.Constant as LGAC
import qualified LLVM.General.AST.InlineAssembly as LGAIn
import qualified LLVM.General.AST.ParameterAttribute as LGAP

import Phkit.Phire
import Phkit.Analysis
import Phkit.Transform

flattenResultOf :: LGA.Module -> LGA.Module
flattenResultOf modu = bwdRewriteResultOf modu (CH.mkBRewrite rewriteFun)
  where rewriteFun :: PhInstruction e x -> CH.Fact x Bool ->
          NameLabelMapFuelM (Maybe (CH.Graph PhInstruction e x))
        rewriteFun n@NameInsn{} _ = return $ Just $ phGUnit n
        rewriteFun n@InsnInsn{} _ = do
          (g, n') <- flattenExp n
          return $ Just (CH.catGraphNodeOO g n')
        rewriteFun n@TermInsn{} _ = do
          (g, n') <- flattenExp n
          return $ Just (CH.catGraphNodeOC g n')

class Flattenable t where
  flattenExp :: t -> NameLabelMapFuelM (CH.Graph PhInstruction CH.O CH.O, t)

instance Flattenable (PhInstruction e x) where
  flattenExp n@NameInsn{} = return (CH.GNil, n)
  flattenExp (InsnInsn insn) = do
    (g, insn') <- flattenExp insn
    return (g, (InsnInsn insn'))
  flattenExp (TermInsn term ls) = do
    (g, term') <- flattenExp term
    return (g, (TermInsn term' ls))

instance Flattenable t => Flattenable (LGA.Named t) where
  flattenExp (LGA.Do v) = do (g, v') <- flattenExp v
                             return (g, LGA.Do v')
  flattenExp (n LGA.:= v) = do (g, v') <- flattenExp v
                               return (g, n LGA.:= v')

instance Flattenable LGA.Instruction where
  flattenExp n@LGA.Add{ LGA.operand0 = op0, LGA.operand1 = op1 } = do
    (g0, op0') <- flattenExp op0; (g1, op1') <- flattenExp op1
    return (g0 CH.<*> g1, n { LGA.operand0 = op0', LGA.operand1 = op1'})
  flattenExp n@LGA.Load{ LGA.address = addr } = do
    (g, addr') <- flattenExp addr; return (g, n { LGA.address = addr'})
  flattenExp n@LGA.GetElementPtr{ LGA.address = addr, LGA.indices = ids } = do
    (ga, addr') <- flattenExp addr
    (gi, ids') <- flattenExp ids
    return (ga CH.<*> gi, n{ LGA.address = addr', LGA.indices = ids'})
  flattenExp n@LGA.ZExt { LGA.operand0 = op0 } = do
    (g, op0') <- flattenExp op0; return (g, n{ LGA.operand0 = op0' })
  flattenExp n@LGA.Store { LGA.address = addr, LGA.value = val } = do
    (ga, addr') <- flattenExp addr; (gv, val') <- flattenExp val;
    return (ga CH.<*> gv, n{ LGA.address = addr', LGA.value = val' })
  flattenExp n@LGA.Shl { LGA.operand0 = op0, LGA.operand1 = op1 } = do
    (g0, op0') <- flattenExp op0; (g1, op1') <- flattenExp op1;
    return (g0 CH.<*> g1, n{ LGA.operand0 = op0', LGA.operand1 = op1' })
  flattenExp n@LGA.Or { LGA.operand0 = op0, LGA.operand1 = op1 } = do
    (g0, op0') <- flattenExp op0; (g1, op1') <- flattenExp op1;
    return (g0 CH.<*> g1, n{ LGA.operand0 = op0', LGA.operand1 = op1' })
  flattenExp n@LGA.AShr { LGA.operand0 = op0, LGA.operand1 = op1 } = do
    (g0, op0') <- flattenExp op0; (g1, op1') <- flattenExp op1;
    return (g0 CH.<*> g1, n{ LGA.operand0 = op0', LGA.operand1 = op1' })
  flattenExp n@LGA.Phi{} = return (CH.GNil, n) -- do nothing for phi
  flattenExp n@LGA.BitCast { LGA.operand0 = op0 } = do
    (g0, op0') <- flattenExp op0; return (g0, n{ LGA.operand0 = op0' })
  flattenExp n@LGA.Call{ LGA.function = func, LGA.arguments = args } = do
    (gf, func') <- flattenExp func
    (ga, args') <- flattenExp args
    return (gf CH.<*> ga, n{ LGA.function =func', LGA.arguments = args' })

  flattenExp other = error $ "unexpected pattern matching LGA.Instruction: "
    ++ show other

instance Flattenable LGAIn.InlineAssembly where
  flattenExp other =  error $
    "unexpected pattern matching LGAIn.InlineAssembly: " ++ show other

instance Flattenable LGAP.ParameterAttribute where
  flattenExp p = return (CH.GNil, p)

instance Flattenable t => Flattenable [t] where
  flattenExp vals = do
    gvals <- mapM flattenExp vals
    let gvs = map fst gvals
        vals' = map snd gvals
    return (foldr (CH.<*>) CH.GNil gvs, vals')

instance (Flattenable a, Flattenable b) => Flattenable (Either a b) where
  flattenExp (Left a) = do
    (g, a') <- flattenExp a; return (g, Left a')
  flattenExp (Right b) = do
    (g, b') <- flattenExp b; return (g, Right b')

instance (Flattenable a, Flattenable b) => Flattenable (a, b) where
  flattenExp (va, vb) = do
    (gva, va') <- flattenExp va
    (gvb, vb') <- flattenExp vb
    return (gva CH.<*> gvb, (va, vb))

instance Flattenable LGA.Terminator where
  flattenExp term = return (CH.GNil, term)

instance Flattenable LGA.Operand where
  flattenExp o@LGA.LocalReference{} = return (CH.GNil, o)
  flattenExp o@LGA.MetadataStringOperand{} = return (CH.GNil, o)
  flattenExp o@LGA.MetadataNodeOperand{} = return (CH.GNil, o)
  flattenExp (LGA.ConstantOperand co) = do (g, co') <- flattenExp co
                                           return (g, LGA.ConstantOperand co')

instance Flattenable LGAC.Constant where
  flattenExp co@LGAC.Int{} = return (CH.GNil, co)
  flattenExp co@LGAC.BlockAddress{} = return (CH.GNil, co)
  flattenExp co@LGAC.BitCast{ LGAC.operand0 = op0 } = do
    (g, op0') <- flattenExp op0; return (g, co{ LGAC.operand0 = op0' })
  flattenExp co@LGAC.GlobalReference{} = return (CH.GNil, co)
  flattenExp other = error $ "unexpected pattern matching LGAC.Constant: "
    ++ show other
