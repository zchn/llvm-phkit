module Phkit.SoftBound.Common (sbTrackType) where

import qualified LLVM.General.AST as LGA
import qualified LLVM.General.AST.AddrSpace as LGAA
import qualified LLVM.General.AST.CallingConvention as LGACa
import qualified LLVM.General.AST.Constant as LGAC
import qualified LLVM.General.AST.Global as LGAG
import qualified LLVM.General.AST.Instruction as LGAI
import qualified LLVM.General.AST.Operand as LGAO
import qualified LLVM.General.AST.Type as LGAT

sbTrackType :: LGAT.Type
sbTrackType = LGAT.StructureType {
  LGAT.isPacked = False,
  LGAT.elementTypes = [
      LGAT.PointerType {
          LGAT.pointerReferent = LGAT.VoidType,
          LGAT.pointerAddrSpace = LGAA.AddrSpace 0 },
      LGAT.PointerType {
          LGAT.pointerReferent = LGAT.VoidType,
          LGAT.pointerAddrSpace = LGAA.AddrSpace 0 }]}
