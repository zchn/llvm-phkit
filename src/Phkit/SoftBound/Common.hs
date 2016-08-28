module Phkit.SoftBound.Common (withMetadata, sizeOfType, sizeOfPtrType, successorFacts) where

import qualified Compiler.Hoopl as CH
import qualified Data.Maybe as DMa
import qualified Data.Word as DW
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


sizeOfPtrType :: LGA.Type -> Int
sizeOfPtrType (LGAT.PointerType t _) = sizeOfType t
sizeOfPtrType _ = 254

sizeOfType :: LGA.Type -> Int
sizeOfType (LGAT.IntegerType nBits) = _nBitsToNBytes nBits
sizeOfType (LGAT.PointerType _ _) = 8 -- TODO(zchn): Improve
sizeOfType (LGAT.FloatingPointType nBits _) = _nBitsToNBytes nBits
sizeOfType (LGAT.VectorType eCount eType) = 
    fromInteger (toInteger eCount) * (sizeOfType eType)
sizeOfType (LGAT.StructureType _ eTypes) = sum $ map sizeOfType eTypes
sizeOfType (LGAT.ArrayType eCount eType) = 
    fromInteger (toInteger eCount) * (sizeOfType eType)
sizeOfType LGAT.NamedTypeReference{} = 255
sizeOfType LGAT.MetadataType{} = 255
sizeOfType LGAT.VoidType = 255
sizeOfType LGAT.FunctionType{} = 255

_nBitsToNBytes :: DW.Word32 -> Int
_nBitsToNBytes nBits = fromInteger ((toInteger nBits - 1) `div` 8 + 1)
