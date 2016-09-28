{-# LANGUAGE GADTs #-}

module Phkit.SoftBound.Lattice
       (SbFunctionFact, SbNameState(..), sbFunctionLattice,
        justJoinSbNameState, joinSbNameState)
       where

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
import LLVM.General.AST (Named(..))
import Phkit.Analysis
import Phkit.Phire
import Phkit.Transform
import Phkit.SoftBound.Common
import Phkit.SoftBound.Lang

data SbNameState
    = SbBottom 
    | SbTracked { sbTrackedMeta :: LGA.Name}
    | SbSbName 
    | SbUnreachableTop 
    deriving (Eq,Show)

sbNameLattice :: CH.DataflowLattice SbNameState
sbNameLattice = 
    CH.DataflowLattice
    { CH.fact_name = "sbNameLattice"
    , CH.fact_bot = SbBottom
    , CH.fact_join = \_ (CH.OldFact oldF) (CH.NewFact newF) -> 
                          joinSbNameState oldF newF
    }

joinSbNameState :: SbNameState -> SbNameState -> (CH.ChangeFlag, SbNameState)
joinSbNameState oldF newF = 
    let joined = justJoinSbNameState oldF newF
    in (CH.changeIf (joined /= oldF), joined)

justJoinSbNameState :: SbNameState -> SbNameState -> SbNameState
-- Equal, must be the first pattern
justJoinSbNameState f1 f2
  | f1 == f2 = f1
-- SbBottom
justJoinSbNameState SbBottom f = f
-- SbTracked
justJoinSbNameState (SbTracked meta1) (SbTracked meta2)
  | meta1 == meta2 = SbTracked meta1
justJoinSbNameState SbTracked{} SbTracked{} = SbUnreachableTop
justJoinSbNameState SbTracked{} SbSbName{} = SbUnreachableTop
justJoinSbNameState SbTracked{} SbUnreachableTop = SbUnreachableTop
-- SbSbName
justJoinSbNameState SbSbName SbSbName = SbSbName
justJoinSbNameState SbSbName SbUnreachableTop = SbUnreachableTop
-- Reverse, must be the last pattern
justJoinSbNameState f1 f2 = justJoinSbNameState f2 f1

type SbFunctionFact = DM.Map LGA.Name SbNameState

sbFunctionLattice :: CH.DataflowLattice SbFunctionFact
sbFunctionLattice = mkPhNameFactMapLattice sbNameLattice
