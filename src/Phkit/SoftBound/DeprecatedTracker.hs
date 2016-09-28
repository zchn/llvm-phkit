{-# LANGUAGE GADTs #-}

module Phkit.SoftBound.Tracker (softBoundAddTrackResultOf) where

import qualified Compiler.Hoopl as CH
import qualified Data.Map as DM
import qualified Data.Maybe as DMa
import qualified Data.Set as DS
import qualified Data.Word as DW
import qualified LLVM.General.AST as LGA
import qualified LLVM.General.AST.AddrSpace as LGAA
import qualified LLVM.General.AST.CallingConvention as LGACa
import qualified LLVM.General.AST.Constant as LGAC
import qualified LLVM.General.AST.Global as LGAG
import qualified LLVM.General.AST.Instruction as LGAI
import qualified LLVM.General.AST.Operand as LGAO
import qualified LLVM.General.AST.Type as LGAT

import LLVM.General.AST (Named(..))

import Phkit.Phire
import Phkit.Analysis
import Phkit.Transform
import Phkit.SoftBound.Common
import Phkit.SoftBound.Lang

softBoundAddTrackResultOf :: LGA.Module -> LGA.Module
softBoundAddTrackResultOf modu =
    bwdTransformResultOf
        modu
        sbFunctionTrackLattice
        sbTrackTransfer
        sbTrackRewrite

addTrackedPtrToFact :: LGA.Name -- ^ ptr
  -> LGA.Name -- ^ meta
  -> SbFunctionTrackFact -- ^ f
  -> SbFunctionTrackFact
addTrackedPtrToFact ptr meta f =
  let currSt = DM.findWithDefault SbTBottom ptr f
      newSt = justJoinSbNameTrackState currSt (SbTTracked meta)
  in DM.insert ptr newSt f

sbTrackTransfer :: PhBwdTransfer SbFunctionTrackFact
sbTrackTransfer = CH.mkBTransfer trans
  where
    trans :: PhInstruction e x
          -> CH.Fact x SbFunctionTrackFact
          -> SbFunctionTrackFact
    trans NameInsn{} f = f
    trans (InsnInsn n) f =
      case maybeGetTrackedPtr n of
        Just (ptr, ptr_meta) ->
          addTrackedPtrToFact ptr ptr_meta f
        Nothing -> f
    trans n@(TermInsn term ls) fbase =
        case ls of
            [] -> CH.fact_bot sbFunctionTrackLattice
            -- this is due to the limitation of CH.joinFacts, h shouldn't really
            -- matter here.
            (h:_) ->
                CH.joinFacts sbFunctionTrackLattice h $
                successorFacts n fbase

sbTrackRewrite :: PhBwdRewrite SbFunctionTrackFact
sbTrackRewrite =
    CH.mkBRewrite
        (\phI f ->
              fmap Just $ maybeAddTrack phI f)

maybeAddTrack
    :: PhInstruction e x
    -> CH.Fact x SbFunctionTrackFact
    -> NameLabelMapFuelM (CH.Graph PhInstruction e x)
maybeAddTrack phI@NameInsn{} _ = return $ phGUnit phI
maybeAddTrack phI@(InsnInsn (LGA.Do _)) f = return $ phGUnit phI
maybeAddTrack phI@(InsnInsn (ptr := LGA.Load { LGAI.address = LGAO.LocalReference _ ptr_ptr })) f =
  if DM.findWithDefault SbTBottom ptr f == SbTBottom then do
    ptr_meta <- CH.liftFuel freshName
    return $ (phGUnit phI) `CH.catGraphNodeOO` (InsnInsn $ mkSbLoad ptr_meta ptr ptr_ptr)
  else return $ phGUnit phI
maybeAddTrack phI@(InsnInsn (ptr := LGA.Alloca { LGA.allocatedType = ty, LGA.numElements = maybeC, LGA.alignment = align })) f =
  if DM.findWithDefault SbTBottom ptr f == SbTBottom then do
    ptr_meta <- CH.liftFuel freshName
    return $ (phGUnit phI) `CH.catGraphNodeOO` (InsnInsn $ mkSbInit ptr_meta ptr (sizeOfType ty) maybeC align)
  else return $ phGUnit phI
maybeAddTrack phI@(InsnInsn (name := insn)) f = return $ phGUnit phI
maybeAddTrack phI@(TermInsn (LGA.Do _) _) f = return $ phGUnit phI

class SbTrackable t  where
    getTrack :: t -> SbFunctionTrackFact -> NameLabelMapM [LGA.Instruction]

data SbNameTrackState
    = SbTBottom
    | SbTSbName
    | SbTTracked { sbName :: LGA.Name}
    | SbTUnreachableTop
    deriving (Eq,Show)

sbNameTrackLattice :: CH.DataflowLattice SbNameTrackState
sbNameTrackLattice =
    CH.DataflowLattice
    { CH.fact_name = "sbNameTrackLattice"
    , CH.fact_bot = SbTBottom
    , CH.fact_join = \_ (CH.OldFact oldF) (CH.NewFact newF) ->
                          joinSbNameTrackState oldF newF
    }

joinSbNameTrackState :: SbNameTrackState
                     -> SbNameTrackState
                     -> (CH.ChangeFlag, SbNameTrackState)
joinSbNameTrackState oldF newF =
    let joined = justJoinSbNameTrackState oldF newF
    in (CH.changeIf (joined /= oldF), joined)

justJoinSbNameTrackState :: SbNameTrackState
                         -> SbNameTrackState
                         -> SbNameTrackState
-- Equal, must be the first pattern
justJoinSbNameTrackState f1 f2
  | f1 == f2 = f1
-- SbTBottom
justJoinSbNameTrackState SbTBottom f = f
-- SbTSbName
justJoinSbNameTrackState SbTSbName SbTTracked{} = SbTUnreachableTop
justJoinSbNameTrackState SbTSbName SbTUnreachableTop = SbTUnreachableTop
-- SbTTracked, remember that the equaled case is already handle before here.
justJoinSbNameTrackState SbTTracked{} SbTTracked{} = SbTUnreachableTop
justJoinSbNameTrackState SbTTracked{} SbTUnreachableTop = SbTUnreachableTop
-- Reverse, must be the last pattern
justJoinSbNameTrackState f1 f2 = justJoinSbNameTrackState f2 f1

type SbFunctionTrackFact = DM.Map LGA.Name SbNameTrackState

sbFunctionTrackLattice :: CH.DataflowLattice SbFunctionTrackFact
sbFunctionTrackLattice = mkPhNameFactMapLattice sbNameTrackLattice
