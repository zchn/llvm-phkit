{-# LANGUAGE GADTs #-}

module Phkit.Phire ( finalizeFactBase
                   , finalizeModule
                   , NameLabelMapM
                   , PhModule(..)
                   , PhFunction(..)
                   , PhFunctionParams
                   , PhBody
                   , PhInstruction(..)
                   , phGUnit
                   , phModuleFromModule
                   , phModuleToModule
                   , testOnlyRunWithEmptyMap
                   ) where

import qualified Compiler.Hoopl as CH
import qualified Control.Applicative as CA
import qualified Control.Monad as CM
import qualified Data.Bimap as DB
import qualified Data.Either as DE
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.Word as DW
import qualified LLVM.General.AST as LGA
import qualified LLVM.General.AST.CallingConvention as LGAC
import qualified LLVM.General.AST.Constant as LGACo
import qualified LLVM.General.AST.DataLayout as LGAD
import qualified LLVM.General.AST.DLL as LGADL
import qualified LLVM.General.AST.FunctionAttribute as LGAF
import qualified LLVM.General.AST.InlineAssembly as LGAI
import qualified LLVM.General.AST.IntegerPredicate as LGAIn
import qualified LLVM.General.AST.Operand as LGAO
import qualified LLVM.General.AST.ParameterAttribute as LGAP
import qualified LLVM.General.AST.Linkage as LGAL
import qualified LLVM.General.AST.Visibility as LGAV

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

phGUnit :: PhInstruction e x -> PhGraph e x
phGUnit phiCO@NameInsn{} = CH.gUnitCO $ CH.BlockCO phiCO CH.BNil
phGUnit phiOO@InsnInsn{} = CH.gUnitOO $ CH.BMiddle phiOO
phGUnit phiOC@TermInsn{} = CH.gUnitOC $ CH.BlockOC CH.BNil phiOC

finalizeFactBase :: NameLabelMapM [(LGA.Name, CH.FactBase f)]
  -> [(LGA.Name, DM.Map LGA.Name f)]
finalizeFactBase mFunFactBases = runWithEmptyMap $
  mFunFactBases >>= (mapM phNameFactMapFromFactBase)

phNameFactMapFromFactBase :: (LGA.Name, CH.FactBase f)
                    -> NameLabelMapM (LGA.Name, DM.Map LGA.Name f)
phNameFactMapFromFactBase (name, fb) =
                      CH.mapFoldWithKey addToMMap (return (name, DM.empty)) fb
  where addToMMap k v mNameAndMap = do
          name <- nameFor k
          (fn, origMap) <- mNameAndMap
          return (fn, DM.insert name v origMap)

finalizeModule :: NameLabelMapM LGA.Module -> LGA.Module
finalizeModule mMod = runWithEmptyMap mMod

phModuleFromModule :: LGA.Module -> NameLabelMapM PhModule
phModuleFromModule modu@LGA.Module{LGA.moduleName = name,
                                   LGA.moduleDataLayout = layout,
                                   LGA.moduleTargetTriple = triple,
                                   LGA.moduleDefinitions = defs} = do
  funcsOrDefs <- phFunctionsOrDefsFromModule modu
  let funcs = DE.rights funcsOrDefs
      unusedDefs = DE.lefts funcsOrDefs
  return PhModule {
    phModuleName = name,
    phModuleFunctions = funcs,
    phModuleDataLayout = layout,
    phModuleTargetTriple = triple,
    phModuleUnusedDefinitions = unusedDefs }

phModuleToModule :: PhModule -> LGA.Module
phModuleToModule phm@PhModule {
    phModuleName = name, phModuleFunctions = funcs, phModuleDataLayout = layout,
    phModuleTargetTriple = triple, phModuleUnusedDefinitions = unused } =
  LGA.Module { LGA.moduleName = name, LGA.moduleDataLayout = layout,
               LGA.moduleTargetTriple = triple,
               LGA.moduleDefinitions = unused ++ map (
                 LGA.GlobalDefinition . phFunctionToGlobal) funcs}

phFunctionsOrDefsFromModule :: LGA.Module ->
  NameLabelMapM [Either LGA.Definition PhFunction]
phFunctionsOrDefsFromModule (LGA.Module { LGA.moduleDefinitions = defs }) =
    mapM phFunctionOrDefFromDefs defs

phFunctionOrDefFromDefs :: LGA.Definition ->
  NameLabelMapM (Either LGA.Definition PhFunction)
phFunctionOrDefFromDefs (LGA.GlobalDefinition (
                            LGA.Function p1 p2 p3 p4 p5 p6 name params p9 p10 p11
                              p12 p13 p14 bbs@(LGA.BasicBlock bbName _ _ : _)
                            )) = do
  entry <- labelFor bbName
  body <- phBodyFromBBs bbs
  newName <- normalize name
  return $ Right PhFunction{ phFunctionName = newName
                           , phFunctionEntry = entry
                           , phFunctionBody = body
                           , phFunctionParams = params
                           , phFunctionUnused = PhFunctionUnused p1 p2 p3 p4 p5
                                                p6 p9 p10 p11 p12 p13 p14}
phFunctionOrDefFromDefs d = return $ Left d

phFunctionToGlobal :: PhFunction -> LGA.Global
phFunctionToGlobal phf@PhFunction{
  phFunctionName = name, phFunctionEntry = entry, phFunctionBody = body,
  phFunctionParams = params,
  phFunctionUnused = PhFunctionUnused p1 p2 p3 p4 p5 p6 p9 p10 p11 p12
    p13 p14} = LGA.Function p1 p2 p3 p4 p5 p6 name params p9 p10 p11 p12 p13 p14 bbs
  where bbs = phBodyToBBs entry body

phBodyFromBBs :: [LGA.BasicBlock] -> NameLabelMapM PhBody
phBodyFromBBs bbs =
  do g <- foldl (CM.liftM2 (CH.|*><*|)) (return CH.emptyClosedGraph) (
       map phBodyFromBB bbs)
     getBody g
  where getBody graph = NameLabelMapM (\m -> return (m, graph))

phBodyFromBB :: LGA.BasicBlock -> NameLabelMapM PhBody
phBodyFromBB (LGA.BasicBlock name namedInstructions namedTerminator) =
  do label_for_bb <- labelFor name
     lbls_for_term <- case namedTerminator of
       _ LGA.:= t -> successorsOf t
       LGA.Do t -> successorsOf t
     firstInsn <- normalize $ NameInsn name label_for_bb
     middleInsns <- mapM (normalize . InsnInsn) namedInstructions
     lastInsn <- normalize $ TermInsn namedTerminator lbls_for_term
     return $ CH.mkFirst firstInsn CH.<*>
              CH.mkMiddles middleInsns CH.<*>
              CH.mkLast lastInsn

phBodyToBBs :: CH.Label -> PhBody -> [LGA.BasicBlock]
phBodyToBBs entry body =
  let enterableGraph =
        (CH.blockGraph $ CH.blockJoinTail CH.emptyBlock $
          TermInsn (LGA.Do $ LGA.Unreachable []) [entry])
        CH.|*><*| body
  in
    map phBlockToBB (CH.postorder_dfs enterableGraph)

phBlockToBB :: PhBlock -> LGA.BasicBlock
phBlockToBB phb =
  let (NameInsn name _, midB, TermInsn term _) = CH.blockSplit phb
      insnInsnList = CH.blockToList midB
  in
    LGA.BasicBlock name (map ph2insn insnInsnList) term
  where ph2insn :: PhInstruction CH.O CH.O -> LGA.Named LGA.Instruction
        ph2insn (InsnInsn r) = r

--------------------------------------------------------------------------------
-- The Program Hardening Intermediate Representation (Phire)
--------------------------------------------------------------------------------

data PhModule = PhModule {
  phModuleName :: String,
  phModuleFunctions :: [PhFunction],
  phModuleDataLayout :: Maybe LGAD.DataLayout,
  phModuleTargetTriple :: Maybe String,
  -- only stores parts of the LGA.Module not touched by Phkit
  phModuleUnusedDefinitions :: [LGA.Definition]
  }

instance Show PhModule where
  show m@PhModule {
    phModuleName = name, phModuleFunctions = funcs,
    phModuleDataLayout = layout, phModuleTargetTriple = triple,
    phModuleUnusedDefinitions = unused } =
    "PhModule { phMoudleName = " ++ show name
    ++ "\n phModuleDataLayout = " ++ show layout
    ++ "\n phModuleTargetTriple = " ++ show triple
    ++ "\n phModuleFunctions = {"
    ++ concatMap show funcs
    ++ "\n }"
    ++ "\n phModuleUnusedDefinitions = {"
    ++ concatMap show unused
    ++ "\n }"
    ++ "\n }"

type PhFunctionParams = ([LGA.Parameter], Bool) -- ^ snd indicates varargs

data PhFunction = PhFunction {
  phFunctionName :: LGA.Name,
  phFunctionEntry :: CH.Label,
  phFunctionBody :: PhBody,
  phFunctionParams :: PhFunctionParams,
  phFunctionUnused :: PhFunctionUnused
  }

-- only stores parts of the LGA.Function not touched by Phkit
data PhFunctionUnused = PhFunctionUnused LGAL.Linkage LGAV.Visibility
  (Maybe LGADL.StorageClass) LGAC.CallingConvention [LGAP.ParameterAttribute]
  LGA.Type [Either LGAF.GroupID LGAF.FunctionAttribute]
  (Maybe String) (Maybe String) DW.Word32 (Maybe String) (Maybe LGACo.Constant)

type PhGraph = CH.Graph PhInstruction

type PhBody = PhGraph CH.C CH.C

data PhInstruction e x where
  NameInsn :: LGA.Name -> CH.Label -> PhInstruction CH.C CH.O
  InsnInsn :: LGA.Named LGA.Instruction -> PhInstruction CH.O CH.O
  TermInsn :: LGA.Named LGA.Terminator -> [CH.Label] -> PhInstruction CH.O CH.C

instance Show PhFunction where
  show f = "PhFunction { phFunctionName = " ++ show (phFunctionName f)
    ++ "\n phFunctionEntry = " ++ show (phFunctionEntry f)
    ++ "\n phFunctionBody = " ++ CH.showGraph show (phFunctionBody f) ++ "\n}"

instance Show (PhInstruction e x) where
  show (NameInsn name l) = "NameInsn " ++ show name ++ " " ++ show l ++ "\n"
  show (InsnInsn namedInstruction) = "InsnInsn " ++ show namedInstruction ++
                                     "\n"
  show (TermInsn namedTerminator labels) = "TermInsn " ++ show namedTerminator
                                           ++ " " ++ show labels ++ "\n"

successorsOf :: LGA.Terminator -> NameLabelMapM [CH.Label]
successorsOf LGA.Ret {} = return []
successorsOf LGA.CondBr {LGA.trueDest = t, LGA.falseDest = f} = labelsFor [t, f]
successorsOf LGA.Br {LGA.dest = d} =  labelsFor [d]
successorsOf LGA.Switch {LGA.defaultDest = dd, LGA.dests = dlist} = labelsFor (
  dd : map snd dlist)
successorsOf LGA.IndirectBr {LGA.possibleDests = pd} = labelsFor pd
successorsOf LGA.Invoke {LGA.returnDest = retd, LGA.exceptionDest = excd} =
  labelsFor [retd, excd]
successorsOf LGA.Resume {} = labelsFor [LGA.Name "TODO:Resume"]
successorsOf LGA.Unreachable {} = labelsFor [LGA.Name "TODO:Unreachable"]

instance CH.NonLocal PhInstruction where
  entryLabel (NameInsn _ l) = l
  successors (TermInsn _ ls) = ls

type PhBlock = CH.Block PhInstruction CH.C CH.C

--------------------------------------------------------------------------------
-- Name normalization
--------------------------------------------------------------------------------

class Normalizable t where
  normalize :: t -> NameLabelMapM  t

normalize2 :: (Normalizable a) =>
  (a -> b) -> a -> (NameLabelMapM b)
normalize2 f a = fmap f (normalize a)

normalize3 :: (Normalizable a, Normalizable b) =>
  (a -> b -> c) -> a -> b -> (NameLabelMapM c)
normalize3 f a b = do
  f' <- normalize2 f a
  normalize2 f' b

normalize4 :: (Normalizable a, Normalizable b, Normalizable c) =>
  (a -> b -> c -> d) -> a -> b -> c -> (NameLabelMapM d)
normalize4 f a b c = do
  f' <- normalize3 f a b
  normalize2 f' c

normalize5 :: (Normalizable a, Normalizable b, Normalizable c, Normalizable d) =>
  (a -> b -> c -> d -> e) -> a -> b -> c -> d -> (NameLabelMapM e)
normalize5 f a b c d = do
  f' <- normalize4 f a b c
  normalize2 f' d

normalize6 :: (Normalizable a, Normalizable b, Normalizable c, Normalizable d,
               Normalizable e) =>
  (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> (NameLabelMapM f)
normalize6 f a b c d e = do
  f' <- normalize5 f a b c d
  normalize2 f' e

normalize7 :: (Normalizable a, Normalizable b, Normalizable c, Normalizable d,
               Normalizable e, Normalizable f) =>
  (a -> b -> c -> d -> e -> f -> z) -> a -> b -> c -> d -> e -> f
  -> (NameLabelMapM z)
normalize7 fun a b c d e f = do
  fun' <- normalize6 fun a b c d e
  normalize2 fun' f

normalize8 :: (Normalizable a, Normalizable b, Normalizable c, Normalizable d,
               Normalizable e, Normalizable f, Normalizable g) =>
  (a -> b -> c -> d -> e -> f -> g -> z) -> a -> b -> c -> d -> e -> f -> g
  -> (NameLabelMapM z)
normalize8 fun a b c d e f g = do
  fun' <- normalize7 fun a b c d e f
  normalize2 fun' g

normalize9 :: (Normalizable a, Normalizable b, Normalizable c, Normalizable d,
               Normalizable e, Normalizable f, Normalizable g, Normalizable h)
              =>
  (a -> b -> c -> d -> e -> f -> g -> h -> z) -> a -> b -> c -> d -> e -> f -> g
  -> h -> (NameLabelMapM z)
normalize9 fun a b c d e f g h = do
  fun' <- normalize8 fun a b c d e f g
  normalize2 fun' h

instance Normalizable a => Normalizable [a] where
  normalize ta = mapM normalize ta

instance Normalizable a => Normalizable (Maybe a) where
  normalize (Just a) = fmap Just (normalize a)
  normalize Nothing = return Nothing

instance (Normalizable a, Normalizable b) => Normalizable (a, b) where
  normalize (a, b) = do {
    a' <- normalize a; b' <- normalize b; return (a', b') }

instance (Normalizable a, Normalizable b) => Normalizable (Either a b) where
  normalize (Left a) = normalize2 Left a
  normalize (Right b) = normalize2 Right b

instance Normalizable (PhInstruction e x) where
  normalize (NameInsn n l) = normalize2 (`NameInsn` l) n
  normalize (InsnInsn insn) = normalize2 InsnInsn insn
  normalize (TermInsn term ls) = normalize2 (`TermInsn` ls) term

instance Normalizable a => Normalizable (LGA.Named a) where
  normalize (n LGA.:= a) = normalize3 (LGA.:=) n a
  normalize (LGA.Do a) = normalize2 LGA.Do a

instance Normalizable LGA.Instruction where
  normalize (LGA.AShr a b c d) = normalize5 LGA.AShr a b c d
  normalize (LGA.Add a b c d e) = normalize6 LGA.Add a b c d e
  normalize (LGA.Alloca a b c d) = normalize5 LGA.Alloca a b c d
  normalize (LGA.And a b c) = normalize4 LGA.And a b c
  normalize (LGA.BitCast a b c) = normalize4 LGA.BitCast a b c
  normalize (LGA.Call a b c d e f g) = normalize8 LGA.Call a b c d e f g
  normalize (LGA.FDiv a b c d) = normalize5 LGA.FDiv a b c d
  normalize (LGA.FPExt a b c) = normalize4 LGA.FPExt a b c
  normalize (LGA.FMul a b c d) = normalize5 LGA.FMul a b c d
  normalize (LGA.GetElementPtr a b c d) = normalize5 LGA.GetElementPtr a b c d
  normalize (LGA.ICmp a b c d) = normalize5 LGA.ICmp a b c d
  normalize (LGA.LShr a b c d) = normalize5 LGA.LShr a b c d
  normalize (LGA.Load a b c d e) = normalize6 LGA.Load a b c d e
  normalize (LGA.Mul a b c d e) = normalize6 LGA.Mul a b c d e
  normalize (LGA.Or a b c) = normalize4 LGA.Or a b c
  normalize (LGA.Phi a b c) = normalize4 LGA.Phi a b c
  normalize (LGA.PtrToInt a b c) = normalize4 LGA.PtrToInt a b c
  normalize (LGA.SDiv a b c d) = normalize5 LGA.SDiv a b c d
  normalize (LGA.SExt a b c) = normalize4 LGA.SExt a b c
  normalize (LGA.SIToFP a b c) = normalize4 LGA.SIToFP a b c
  normalize (LGA.Shl a b c d e) = normalize6 LGA.Shl a b c d e
  normalize (LGA.Store a b c d e f) = normalize7 LGA.Store a b c d e f
  normalize (LGA.Sub a b c d e) = normalize6 LGA.Sub a b c d e
  normalize (LGA.Trunc a b c) = normalize4 LGA.Trunc a b c
  normalize (LGA.Xor a b c) = normalize4 LGA.Xor a b c
  normalize (LGA.ZExt a b c) = normalize4 LGA.ZExt a b c
  normalize other = return (insnErrorIndicator other)

instance Normalizable LGA.Terminator where
  normalize (LGA.Br a b) = normalize3 LGA.Br a b
  normalize (LGA.CondBr a b c d) = normalize5 LGA.CondBr a b c d
  normalize (LGA.IndirectBr a b c) = normalize4 LGA.IndirectBr a b c
  normalize (LGA.Invoke a b c d e f g h) = normalize9 LGA.Invoke a b c d e f g h
  normalize (LGA.Resume a b) = normalize3 LGA.Resume a b
  normalize (LGA.Ret a b) = normalize3 LGA.Ret a b
  normalize (LGA.Switch a b c d) = normalize5 LGA.Switch a b c d
  normalize (LGA.Unreachable a) = normalize2 LGA.Unreachable a

instance Normalizable LGACo.Constant where
  normalize (LGACo.Array a b) = normalize3 LGACo.Array a b
  normalize (LGACo.BitCast a b) = normalize3 LGACo.BitCast a b
  normalize (LGACo.BlockAddress a b) = normalize3 LGACo.BlockAddress a b
  normalize (LGACo.GetElementPtr a b c) = normalize4 LGACo.GetElementPtr a b c
  normalize (LGACo.GlobalReference a b) = normalize3 LGACo.GlobalReference a b
  normalize (LGACo.Int a b) = normalize3 LGACo.Int a b
  normalize (LGACo.Null a) = normalize2 LGACo.Null a
  normalize (LGACo.Struct a b c) = normalize4 LGACo.Struct a b c
  normalize (LGACo.Undef a) = normalize2 LGACo.Undef a
  normalize (LGACo.Vector a) = normalize2 LGACo.Vector a
  normalize a@LGACo.Float{} = return a
  normalize other = return (constErrorIndicator other)

instance Normalizable LGA.Operand where
  normalize (LGA.LocalReference t n) = normalize3 LGA.LocalReference t n
  normalize (LGA.ConstantOperand c) = normalize2 LGA.ConstantOperand c
  normalize r@LGA.MetadataStringOperand{} = return r
  normalize (LGA.MetadataNodeOperand a) = normalize2 LGA.MetadataNodeOperand a

instance Normalizable LGA.Type where
  normalize (LGA.NamedTypeReference n) =
    fmap LGA.NamedTypeReference (normalize n)
  normalize other = return other

instance Normalizable LGAO.MetadataNode where
  normalize (LGAO.MetadataNode l) = fmap LGAO.MetadataNode (normalize l)
  normalize other@LGAO.MetadataNodeReference{} = return other

instance Normalizable Bool where
  normalize = return

instance Normalizable LGA.FastMathFlags where
  normalize = return

instance Normalizable LGAIn.IntegerPredicate where
  normalize = return

instance Normalizable DW.Word32 where
  normalize = return

instance Normalizable LGAF.FunctionAttribute where
  normalize = return

instance Normalizable LGA.SynchronizationScope where
  normalize = return

instance Normalizable Integer where
  normalize = return

instance Normalizable LGA.TailCallKind where
  normalize = return

instance Normalizable LGA.MemoryOrdering where
  normalize = return

instance Normalizable Char where
  normalize = return

instance Normalizable LGAF.GroupID where
  normalize = return

instance Normalizable LGAC.CallingConvention where
  normalize = return

instance Normalizable LGAP.ParameterAttribute where
  normalize = return

instance Normalizable LGAI.InlineAssembly where
  normalize = return

insnErrorIndicator :: LGA.Instruction -> LGA.Instruction
insnErrorIndicator insn = LGA.Fence {
  LGA.atomicity = (LGA.SingleThread, LGA.Unordered),
  LGA.metadata = [("PatternMatchFail!: " ++ (show insn),
                   LGAO.MetadataNodeReference (LGAO.MetadataNodeID 0))] }

constErrorIndicator :: LGACo.Constant -> LGACo.Constant
constErrorIndicator const =
  LGACo.Undef $ LGA.NamedTypeReference $ LGA.Name $
    "PatternMatchFail!:" ++ (show const)

--------------------------------------------------------------------------------
-- The NameLabelMapM monad
--------------------------------------------------------------------------------

type NameLabelMap = DB.Bimap LGA.Name CH.Label
data NameLabelMapM a = NameLabelMapM (NameLabelMap ->
                              CH.SimpleUniqueMonad (NameLabelMap, a))

freshName :: NameLabelMapM LGA.Name
freshName = NameLabelMapM f
  where f m = do
          l <- CH.freshLabel
          let (NameLabelMapM f') = nameFor l
          f' m

labelFor :: LGA.Name -> NameLabelMapM CH.Label
labelFor name = NameLabelMapM f
  where f m = case DB.lookup name m of
          Just l' -> return (m, l')
          Nothing -> do l' <- CH.freshLabel
                        let m' = DB.insert name l' m
                        return (m', l')

labelsFor :: [LGA.Name] -> NameLabelMapM [CH.Label]
labelsFor = mapM labelFor

phNameVarPrefix = "phv"
isNormalized :: LGA.Name -> Bool
isNormalized name@(LGA.Name nameStr) =
  DL.isPrefixOf phNameVarPrefix nameStr
isNormalized _ = False

nameFor :: CH.Label -> NameLabelMapM LGA.Name
nameFor label = NameLabelMapM f
  where f m = case DB.lookupR label m of
                Just n | not (isNormalized n) ->
                    return (m, n)
                _ ->
                  let n' = LGA.Name (phNameVarPrefix ++ show label)
                      m' = DB.insert n' label m
                  in
                    return (m', n')

namesFor :: [CH.Label] -> NameLabelMapM [LGA.Name]
namesFor labels = mapM nameFor labels

-- TODO(zchn): Optimize using isNormalized if necessary
instance Normalizable LGA.Name where
  normalize = labelFor CM.>=> nameFor

runWithEmptyMap :: NameLabelMapM a -> a
runWithEmptyMap (NameLabelMapM f) =
  let simpleUniquePair = f DB.empty
      (_, theA) = CH.runSimpleUniqueMonad simpleUniquePair in
  theA

testOnlyRunWithEmptyMap :: NameLabelMapM PhModule -> PhModule
testOnlyRunWithEmptyMap = runWithEmptyMap

instance CM.Monad NameLabelMapM where
    return = CA.pure
    NameLabelMapM f1 >>= k = NameLabelMapM $
      \m -> do (m', x) <- f1 m
               let (NameLabelMapM f2) = k x
               f2 m'

instance CM.Functor NameLabelMapM where
  fmap = CM.liftM

instance CA.Applicative NameLabelMapM where
  pure x = NameLabelMapM (\m -> return (m, x))
  (<*>) = CM.ap
