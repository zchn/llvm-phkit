module Phkit.SoftBound.Common (withMetadata, successorFacts) where

import qualified Compiler.Hoopl as CH
import qualified Data.Maybe as DMa
import qualified LLVM.General.AST as LGA
import qualified LLVM.General.AST.AddrSpace as LGAA
import qualified LLVM.General.AST.CallingConvention as LGACa
import qualified LLVM.General.AST.Constant as LGAC
import qualified LLVM.General.AST.Global as LGAG
import qualified LLVM.General.AST.Instruction as LGAI
import qualified LLVM.General.AST.Operand as LGAO
import qualified LLVM.General.AST.Type as LGAT

withMetadata :: LGA.Instruction -> (String, String) -> LGA.Instruction
withMetadata insn kv = insn { LGA.metadata = LGA.metadata insn `withMeta` kv }

withMeta :: LGA.InstructionMetadata -> (String, String)
  -> LGA.InstructionMetadata
withMeta orig (k, v) =
  orig ++ [(k, LGA.MetadataNode [Just $ LGA.MetadataStringOperand v])]

-- | List of (unlabelled) facts from the successors of a last node. Adapted from
-- hoopl/src/Compiler/Hoopl/XUtil.hs
successorFacts :: CH.NonLocal n => n CH.O CH.C -> CH.FactBase f -> [f]
successorFacts n fb = DMa.catMaybes [ f | id <- CH.successors n, let f = CH.lookupFact id fb ]
