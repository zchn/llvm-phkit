{-# LANGUAGE GADTs #-}

module Phkit.SoftBound.Checker (softBoundAddCheckResultOf) where

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

import Phkit.Analysis
import Phkit.Phire
import Phkit.Transform
import Phkit.SoftBound.Lang

softBoundAddCheckResultOf :: LGA.Module -> LGA.Module
softBoundAddCheckResultOf modu = fwdTransformResultOf modu
        sbFunctionCheckLattice sbCheckTransfer softBoundRewrite


sbCheckTransfer :: PhFwdTransfer SbFunctionCheckFact
sbCheckTransfer = CH.mkFTransfer trans
  where
    trans :: PhInstruction e x -> SbFunctionCheckFact
      -> CH.Fact x SbFunctionCheckFact
    trans NameInsn{} f = f
    trans (InsnInsn (LGA.Do insn)) f =
      case maybeGetCheckedPtr insn of
        Just (ptr, ptr_meta, ptr_size) ->
          let pstate = DM.findWithDefault SbUnreachableTop ptr f
              newState = justJoinSbNameCheckState pstate (
                SbChecked ptr_meta (DS.singleton ptr_size) False)
          in
            DM.insert ptr newState f
        Nothing ->
          case maybeGetSavedPptr insn of
            Just pptr ->
              case DM.lookup pptr f of
                Just checkState -> DM.insert pptr (
                  checkState { sbSaved = True }) f
                Nothing -> f
            Nothing -> f
    -- just for debugging
    trans (InsnInsn (n LGA.:= insn)) f = DM.insert n (SbChecked n DS.empty False) f
    -- just for debugging
    trans ti@(TermInsn (n LGA.:= term) _) f =
      let newF = DM.insert n (SbChecked n DS.empty False) f in
        CH.distributeFact ti newF
    trans ti@TermInsn{} f = CH.distributeFact ti f

softBoundRewrite :: PhFwdRewrite SbFunctionCheckFact
softBoundRewrite =
  CH.mkFRewrite (\phI f -> fmap Just $ maybeAddCheck phI f)

maybeAddCheck :: (PhInstruction e x) -> SbFunctionCheckFact ->
  NameLabelMapFuelM (CH.Graph PhInstruction e x)
maybeAddCheck phI@NameInsn{} _ = return $ phGUnit phI
maybeAddCheck phI@InsnInsn{} f = return $ prependAllChecks
  (getChecks phI f) phI
maybeAddCheck phI@TermInsn{} f = return $ prependAllChecks
  (getChecks phI f) phI

prependAllChecks :: [LGA.Instruction] -> PhInstruction CH.O x ->
  CH.Graph PhInstruction CH.O x
prependAllChecks invokes tail =
  let folder :: LGA.Instruction -> CH.Graph PhInstruction CH.O x ->
        CH.Graph PhInstruction CH.O x
      folder invoke currG = CH.catNodeOOGraph (InsnInsn (LGA.Do invoke)) currG
  in
      foldr folder (phGUnit tail) invokes

class SbCheckable t where
  getChecks :: t -> SbFunctionCheckFact -> [LGA.Instruction]

instance SbCheckable (PhInstruction e x) where
  getChecks NameInsn{} _ = []
  getChecks (InsnInsn namedInsn) f = getChecks namedInsn f
  getChecks (TermInsn namedTerm _) f = getChecks namedTerm f

instance SbCheckable x => SbCheckable (LGAI.Named x) where
  getChecks (LGA.Do it) f = getChecks it f
  getChecks (_ LGA.:= it) f = getChecks it f

instance SbCheckable LGAI.Instruction where
  getChecks LGAI.Load { LGAI.address = addrOp } f = getChecks addrOp f
  getChecks LGAI.Store { LGAI.address = addrOp } f =
    let opChecks = getChecks addrOp f in
      case addrOp of
        LGAO.LocalReference t name -> (maybeAddSbSave name f) ++ opChecks
        LGAO.ConstantOperand (LGAC.GlobalReference t name) ->
          (maybeAddSbSave name f) ++ opChecks
        _ -> opChecks
    where
      maybeAddSbSave :: LGA.Name -> SbFunctionCheckFact -> [LGA.Instruction]
      maybeAddSbSave ptr_ptr fact =
        case DM.lookup ptr_ptr fact of
          Just (SbChecked ptr_meta size_set False) ->
            [mkSbSave ptr_ptr ptr_meta]
          _ -> []
  getChecks LGAI.CmpXchg { LGAI.address = addrOp} f = getChecks addrOp f
  getChecks LGAI.AtomicRMW { LGAI.address = addrOp } f = getChecks addrOp f
  getChecks LGAI.Call { LGAI.function = Right addrOp } f = getChecks addrOp f
  getChecks _ _ = []

instance SbCheckable LGAI.Terminator where
  -- TODO(zchn): Is this necessary?
  getChecks LGAI.IndirectBr { LGAI.operand0' = addrOp } f = getChecks addrOp f
  getChecks LGAI.Invoke { LGAI.function' = Right addrOp } f = getChecks addrOp f
  getChecks _ _ = []

instance SbCheckable LGAO.Operand where
  getChecks (LGAO.LocalReference t n) f =
    let pstate = DM.lookup n f
        psize = sizeOfPtrType t in
      case pstate of
        Just (SbChecked ptr_meta size_set _) ->
          if DS.member psize size_set then
            []
          else
            [ mkSbCheck n ptr_meta (LGAC.Int 8 $ toInteger $
                                              sizeOfPtrType t) ]
        _ -> []
  getChecks _ _ = []

sizeOfPtrType :: LGA.Type -> Int
sizeOfPtrType (LGAT.PointerType t _) = sizeOfType t
sizeOfPtrType _ = 254

sizeOfType :: LGA.Type -> Int
sizeOfType (LGAT.IntegerType nBits) = _nBitsToNBytes nBits
sizeOfType (LGAT.PointerType _ _) = 8 -- TODO(zchn): Improve
sizeOfType (LGAT.FloatingPointType nBits _) = _nBitsToNBytes nBits
sizeOfType (LGAT.VectorType eCount eType) = fromInteger (toInteger eCount) *
  (sizeOfType eType)
sizeOfType (LGAT.StructureType _ eTypes) = sum $ map sizeOfType eTypes
sizeOfType (LGAT.ArrayType eCount eType) = fromInteger (toInteger eCount) *
  (sizeOfType eType)
sizeOfType LGAT.NamedTypeReference{} = 255
sizeOfType LGAT.MetadataType{} = 255
sizeOfType LGAT.VoidType = 255
sizeOfType LGAT.FunctionType{} = 255

_nBitsToNBytes :: DW.Word32 -> Int
_nBitsToNBytes nBits =
  fromInteger ((toInteger nBits - 1) `div` 8 + 1)

-- State transition:
-- SbBottom -> SbChecked
data SbNameCheckState = SbBottom |
  SbChecked { sbCheckedMeta :: LGA.Name,
              sbCheckedSizes :: DS.Set Int,
              sbSaved :: Bool } | SbUnreachableTop
  deriving (Eq, Show)

sbNameCheckLattice :: CH.DataflowLattice SbNameCheckState
sbNameCheckLattice = CH.DataflowLattice {
  CH.fact_name = "sbNameCheckLattice",
  CH.fact_bot = SbBottom,
  CH.fact_join = \_ (CH.OldFact oldF) (CH.NewFact newF) ->
      joinSbNameCheckState oldF newF
  }

joinSbNameCheckState :: SbNameCheckState -> SbNameCheckState
  -> (CH.ChangeFlag, SbNameCheckState)
joinSbNameCheckState oldF newF =
  let joined = justJoinSbNameCheckState oldF newF in
    (CH.changeIf (joined /= oldF), joined)

justJoinSbNameCheckState :: SbNameCheckState -> SbNameCheckState
  -> SbNameCheckState
-- Equal, must be the first pattern
justJoinSbNameCheckState f1 f2 | f1 == f2 = f1
-- SbBottom
justJoinSbNameCheckState SbBottom f = f
-- SbChecked
justJoinSbNameCheckState (SbChecked meta1 ss1 sv1) (SbChecked meta2 ss2 sv2)
  | meta1 == meta2 = SbChecked meta1 (DS.union ss1 ss2) (sv1 || sv2)
justJoinSbNameCheckState SbChecked{} SbChecked{} = SbUnreachableTop
justJoinSbNameCheckState SbChecked{} SbUnreachableTop = SbUnreachableTop
-- Reverse, must be the last pattern
justJoinSbNameCheckState f1 f2 = justJoinSbNameCheckState f2 f1

type SbFunctionCheckFact = DM.Map LGA.Name SbNameCheckState

sbFunctionCheckLattice :: CH.DataflowLattice SbFunctionCheckFact
sbFunctionCheckLattice = mkPhNameFactMapLattice sbNameCheckLattice

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

changeUnless :: Bool -> CH.ChangeFlag
changeUnless bo = CH.changeIf (not bo)
