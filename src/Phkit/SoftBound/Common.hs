module Phkit.SoftBound.Common (withMetadata) where

import qualified LLVM.General.AST as LGA
import qualified LLVM.General.AST.AddrSpace as LGAA
import qualified LLVM.General.AST.CallingConvention as LGACa
import qualified LLVM.General.AST.Constant as LGAC
import qualified LLVM.General.AST.Global as LGAG
import qualified LLVM.General.AST.Instruction as LGAI
import qualified LLVM.General.AST.Operand as LGAO
import qualified LLVM.General.AST.Type as LGAT

withMetadata :: LGA.Instruction -> (String, String) -> LGA.Instruction
withMetadata insn kv = insn { LGA.metadata insn `withMeta` kv }
  
withMeta :: LGA.InstructionMetadata -> (String, String)
  -> LGA.InstructionMetadata
withMeta orig (k, v) =
  orig ++ [(k, LGA.MetadataNode [Just $ LGA.MetadataStringOperand v])]