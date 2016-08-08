{-# LANGUAGE GADTs #-}

module Phkit.SoftBound.Tracker (softBoundAddTrackResultOf) where

import qualified Compiler.Hoopl as CH
import qualified Data.Map as DM
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

import Phkit.Phire
import Phkit.Analysis
import Phkit.Transform

softBoundAddTrackResultOf :: LGA.Module -> LGA.Module
-- TODO: should implement an analysis to record all pointers' base and bound.
softBoundAddTrackResultOf modu = modu

sbTrackerTransfer :: PhBwdTransfer SbFunctionTrackFact
sbTrackerTransfer = CH.mkBTransfer trans
  where
    trans :: PhInstruction e x -> CH.Fact x SbFunctionTrackFact
      -> SbFunctionTrackFact
    trans NameInsn{} f = f
    trans InsnInsn{} f = f
    trans n@(TermInsn term ls) fbase =
      case ls of
        [] -> CH.fact_bot sbFunctionTrackLattice
        -- this is due to the limitation of CH.joinFacts, h shouldn't really
        -- matter here.
        (h : _) -> CH.joinFacts sbFunctionTrackLattice h $
             CH.successorFacts n fbase

maybeGetTrackedPtr :: LGA.Named LGA.Instruction ->
  Maybe (LGA.Name, LGA.Name)
maybeGetTrackedPtr (
  tracker_name LGA.:= (
      LGAI.Call { LGAI.function =
                    Right (LGAO.ConstantOperand (LGAC.GlobalReference _ (
                                                    LGA.Name "sbupdate"))),
              LGAI.arguments = [
                (LGAO.LocalReference _ arg_ptr, _),
                _, _] })) = Just (arg_ptr, tracker_name)
maybeGetTrackedPtr (
  tracker_name LGA.:= (
      LGAI.Call { LGAI.function =
                    Right (LGAO.ConstantOperand (LGAC.GlobalReference _ (
                                                    LGA.Name "sbload"))),
              LGAI.arguments = [
                (LGAO.LocalReference _ arg_ptr, _), _] }))
  = Just (arg_ptr, tracker_name)

maybeAddTrack :: PhInstruction e x -> SbFunctionTrackFact ->
  CH.SimpleFuelMonad (CH.Graph PhInstruction e x)
maybeAddTrack phI@NameInsn{} _ = return $ phGUnit phI
maybeAddTrack phI@(InsnInsn (LGA.Do _)) f = return $ phGUnit phI
maybeAddTrack phI@(InsnInsn (name LGA.:= insn)) f = return $ phGUnit phI
-- maybeAddTrack phI@(InsnInsn (name LGA.:= insn)) f = return $ appendTracks
--   phI (mkTrackCalls name insn)
maybeAddTrack phI@(TermInsn (LGA.Do _) _) f = return $ phGUnit phI

-- appendTracks :: PhInstruction e CH.O -> [LGA.Instruction] ->
--   CH.Graph PhInstruction e CH.O

-- mkTrackCalls :: LGA.Name -> LGA.Instruction -> [LGA.Instruction]


-- b = getB() | b_base, b_bound
-- c = getC() | c_base, c_bound
-- a = b + c  | a_base = b_base + c_base, a_bound = b_bound + c_bound
-- p = (void *)a | p_base = a_base, p_bound = a_bound

class SbTrackable t where
  getTrack :: t -> SbFunctionTrackFact ->
    NameLabelMapM [LGA.Instruction]

data SbNameTrackState = SbTBottom | SbTSbName |
  SbTTracked { sbName :: LGA.Name } | SbTUnreachableTop
  deriving (Eq, Show)

sbNameTrackLattice :: CH.DataflowLattice SbNameTrackState
sbNameTrackLattice = CH.DataflowLattice {
  CH.fact_name = "sbNameTrackLattice",
  CH.fact_bot = SbTBottom,
  CH.fact_join = \_ (CH.OldFact oldF) (CH.NewFact newF) ->
      joinSbNameTrackState oldF newF }

joinSbNameTrackState :: SbNameTrackState -> SbNameTrackState ->
  (CH.ChangeFlag, SbNameTrackState)
joinSbNameTrackState oldF newF =
  let joined = justJoinSbNameTrackState oldF newF in
    (CH.changeIf (joined /= oldF), joined)

justJoinSbNameTrackState :: SbNameTrackState -> SbNameTrackState ->
  SbNameTrackState
-- Equal, must be the first pattern
justJoinSbNameTrackState f1 f2 | f1 == f2 = f1
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
