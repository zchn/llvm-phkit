{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Phkit.SoftBound.Common
  ( changeUnless
  , withMetadata
  , sizeOfType
  , sizeOfPtrType
  , successorFacts
  ) where

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

class HasMetadata a  where
  getMetadata :: a -> String -> Maybe String
  withMetadata :: a -> (String, String) -> a

instance HasMetadata LGA.Instruction where
  getMetadata insn k = getMetadata (LGA.metadata insn) k
  withMetadata insn kv =
    insn
    { LGA.metadata = LGA.metadata insn `withMetadata` kv
    }

instance HasMetadata LGA.Terminator where
  getMetadata term k = getMetadata (LGA.metadata' term) k
  withMetadata term kv =
    term
    { LGA.metadata' = LGA.metadata' term `withMetadata` kv
    }

instance HasMetadata LGA.InstructionMetadata where
  getMetadata [] _ = Nothing
  getMetadata ((k1, LGA.MetadataNode [Just (LGA.MetadataStringOperand v)]):t) k =
    if k1 == k
      then Just v
      else getMetadata t k
  getMetadata _ _ = Nothing
  withMetadata [] (k, v) =
    [(k, LGA.MetadataNode [Just $ LGA.MetadataStringOperand v])]
  withMetadata ((k1, v1@(LGA.MetadataNode [Just (LGA.MetadataStringOperand vs1)])):t) kv@(k, v) =
    if k1 == k
      then (k1, LGA.MetadataNode [Just $ LGA.MetadataStringOperand v]) : t
      else (k1, v1) : (t `withMetadata` kv)

-- | List of (unlabelled) facts from the successors of a last node. Adapted from
-- hoopl/src/Compiler/Hoopl/XUtil.hs
successorFacts
  :: CH.NonLocal n
  => n CH.O CH.C -> CH.FactBase f -> [f]
successorFacts n fb =
  DMa.catMaybes
    [ f
    | id <- CH.successors n 
    , let f = CH.lookupFact id fb ]

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
sizeOfType LGAT.NamedTypeReference {} = 255
sizeOfType LGAT.MetadataType {} = 255
sizeOfType LGAT.VoidType = 255
sizeOfType LGAT.FunctionType {} = 255

_nBitsToNBytes :: DW.Word32 -> Int
_nBitsToNBytes nBits = fromInteger ((toInteger nBits - 1) `div` 8 + 1)

changeUnless :: Bool -> CH.ChangeFlag
changeUnless bo = CH.changeIf (not bo)
