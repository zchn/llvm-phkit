{-|

The SoftBound language definition.

For tracking:

* ptr_meta = sbinit(ptr, size)
* ptr_meta = sbcopy(ptr, ptr_meta)
* sbsave(ptr_ptr, ptr_meta)
* ptr_meta = sbload(ptr_ptr)
* ptr_and_meta = sbfun_...
* ptr = sbextractptr(ptr_and_meta)
* ptr_meta = sbextractmeta(ptr, ptr_and_meta)

For checking:

* sbcheck(ptr, ptr_meta, ptr_size)

Function call translation:
> ptr = func(ptr2, i, m)
becomes
> ptr_and_meta = sbfun_func(ptr2_meta, i_meta, m_meta, ptr2, i, m)
> ptr = sbextractptr(ptr_and_meta)
> ptr_meta = sbextractmeta(ptr, ptr_and_meta)

-}
module Phkit.SoftBound.Lang
       (maybeGetCheckedPtr, maybeGetSavedPptr, maybeGetTrackedPtr,
        mkSbCheck, mkSbSave, mkSbLoad,
        sbMetaType)
       where

import qualified LLVM.General.AST as LGA
import qualified LLVM.General.AST.AddrSpace as LGAA
import qualified LLVM.General.AST.CallingConvention as LGACa
import qualified LLVM.General.AST.Constant as LGAC
import qualified LLVM.General.AST.Global as LGAG
import qualified LLVM.General.AST.Instruction as LGAI
import qualified LLVM.General.AST.Operand as LGAO
import qualified LLVM.General.AST.Type as LGAT

import LLVM.General.AST (Named(..))

sbPtrBaseType =
    LGAT.PointerType
    { LGAT.pointerReferent = LGAT.VoidType
    , LGAT.pointerAddrSpace = LGAA.AddrSpace 0
    }

sbPtrBoundType = sbPtrBaseType

-- | struct { (void *) ptr_base, (void *) ptr_bound}
sbMetaType
    :: LGAT.Type
sbMetaType = 
    LGAT.StructureType
    { LGAT.isPacked = False
    , LGAT.elementTypes = [sbPtrBaseType, sbPtrBoundType]
    }

_mkSbCall
    :: LGA.Type -- ^ resultType
    -> String -- ^ function name
    -> [LGA.Operand] -- ^ args
    -> LGA.Instruction
_mkSbCall retT name args = 
    LGAI.Call
    { LGAI.tailCallKind = Nothing
    , LGAI.callingConvention = LGACa.C
    , LGAI.returnAttributes = []
    , LGAI.function = Right $
      LGAO.ConstantOperand $
      LGAC.GlobalReference
          (LGAT.FunctionType
           { LGAT.resultType = retT
           , LGAT.argumentTypes = []
           , LGAT.isVarArg = False
           }) $
      LGA.Name name
    , LGAI.arguments = map
          (\o -> 
                (o, []))
          args
    , LGAI.functionAttributes = []
    , LGAI.metadata = []
    }

-- | sbcheck(ptr, ptr_meta, ptr_size)
mkSbCheck
    :: LGA.Name -> LGA.Name -> LGAC.Constant -> LGA.Instruction
mkSbCheck ptr ptr_meta ptr_size = 
    _mkSbCall
        LGAT.VoidType
        "sbcheck"
        [ LGAO.LocalReference LGAT.VoidType ptr
        , LGAO.LocalReference sbMetaType ptr_meta]

-- | Maybe (ptr, ptr_meta, ptr_size)
maybeGetCheckedPtr
    :: LGA.Instruction -> Maybe (LGA.Name, LGA.Name, Int)
maybeGetCheckedPtr (LGAI.Call{LGAI.function = Right (LGAO.ConstantOperand (LGAC.GlobalReference _ (LGA.Name "sbcheck"))),LGAI.arguments = [((LGAO.LocalReference _ arg_ptr),_),((LGAO.LocalReference _ arg_ptr_meta),_),((LGAO.ConstantOperand (LGAC.Int _ arg_ptr_size')),_)]}) = 
    Just (arg_ptr, arg_ptr_meta, fromInteger arg_ptr_size')
maybeGetCheckedPtr _ = Nothing

-- | sbsave(ptr_ptr, ptr_meta)
mkSbSave
    :: LGA.Name -> LGA.Name -> LGA.Instruction
mkSbSave ptr_ptr ptr_meta = 
    _mkSbCall
        LGAT.VoidType
        "sbsave"
        [ LGAO.LocalReference LGAT.VoidType ptr_ptr
        , LGAO.LocalReference sbMetaType ptr_meta]

-- | Maybe (ptr_ptr)
maybeGetSavedPptr
    :: LGA.Instruction -> Maybe LGA.Name
maybeGetSavedPptr (LGAI.Call{LGAI.function = Right (LGAO.ConstantOperand (LGAC.GlobalReference _ (LGA.Name "sbsave"))),LGAI.arguments = [((LGAO.LocalReference _ arg_pptr),_),_,_]}) = 
    Just arg_pptr
maybeGetSavedPptr _ = Nothing

-- | ptr_meta = sbinit(ptr, size)
mkSbInit
    :: LGA.Name -- ^ ptr_meta
    -> LGA.Name -- ^ ptr
    -> LGAC.Constant -- ^ size
    -> LGA.Named LGA.Instruction
mkSbInit ptr_meta ptr size = 
    ptr_meta :=
    _mkSbCall
        sbMetaType
        "sbinit"
        [LGAO.LocalReference LGAT.VoidType ptr, LGAO.ConstantOperand size]

-- | ptr_meta = sbcopy(ptr, ptr_meta)
mkSbCopy
    :: LGA.Name -> LGA.Name -> LGA.Name -> LGA.Named LGA.Instruction
mkSbCopy meta ptr meta0 = 
    meta :=
    _mkSbCall
        sbMetaType
        "sbcopy"
        [ LGAO.LocalReference LGAT.VoidType ptr
        , LGAO.LocalReference sbMetaType meta0]

-- | ptr_meta = sbload(ptr, ptr_ptr)
mkSbLoad
    :: LGA.Name -> LGA.Name -> LGA.Name -> LGA.Named LGA.Instruction
mkSbLoad meta ptr ptr_ptr = 
    meta :=
    _mkSbCall sbMetaType "sbload" [LGAO.LocalReference LGAT.VoidType ptr,
                                   LGAO.LocalReference LGAT.VoidType ptr_ptr]



-- | ptr_and_meta = sbfun_...
-- | ptr = sbextractptr(ptr_and_meta)
-- | ptr_meta = sbextractmeta(ptr, ptr_and_meta)

maybeGetTrackedPtr :: LGA.Named LGA.Instruction -> Maybe (LGA.Name, LGA.Name)
maybeGetTrackedPtr (tracker_name := (LGAI.Call{LGAI.function = Right (LGAO.ConstantOperand (LGAC.GlobalReference _ (LGA.Name "sbload"))),LGAI.arguments = [(LGAO.LocalReference _ arg_ptr,_),_]})) = 
    Just (arg_ptr, tracker_name)
-- maybeGetTrackedPtr (tracker_name := (LGAI.Call{LGAI.function = Right (LGAO.ConstantOperand (LGAC.GlobalReference _ (LGA.Name "sbupdate"))),LGAI.arguments = [(LGAO.LocalReference _ arg_ptr,_),_,_]})) = 
--     Just (arg_ptr, tracker_name)
maybeGetTrackedPtr other = Nothing