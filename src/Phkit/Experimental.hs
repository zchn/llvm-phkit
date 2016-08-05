module Phkit.Experimental
       (simpleLlvm, getBasicBlocks, getFunctionNames, listIndirectTransfer)
       where

import Data.Maybe (mapMaybe)

import LLVM.General.AST
       (BasicBlock(..),
        Definition(GlobalDefinition),
        Instruction(Call),
        Global(Function),
        Operand(ConstantOperand, LocalReference),
        Module,
        Name,
        Named((:=), Do),
        Terminator(..),
        moduleDefinitions,
        moduleName)

import LLVM.General.AST.InlineAssembly
       (InlineAssembly(InlineAssembly))

-- | 'simplellvm' prints the name of the module
simpleLlvm :: Module -> String
simpleLlvm = moduleName

-- | 'getFunctionNames' returns the list of function names in a Module
getFunctionNames :: Module -> [Name]
getFunctionNames theModule =
  mapMaybe maybeGetName (moduleDefinitions theModule)
 where
  maybeGetName (GlobalDefinition (Function _ _ _ _ _ _ name _ _ _ _ _ _ _ _))  =
    Just name
  maybeGetName _ = Nothing

data Statement = NonTermSt Instruction
               | TermSt Terminator
               deriving (Eq, Show)

-- | 'isIndirectTransfer'
isIndirectTransfer :: Statement -> Bool

isIndirectTransfer (NonTermSt (Call _ _ _ (Left InlineAssembly{}) _ _ _)) = True
isIndirectTransfer (NonTermSt (Call _ _ _ (Right (ConstantOperand _)) _ _ _)) = False
isIndirectTransfer (NonTermSt (Call _ _ _ (Right (LocalReference _ _)) _ _ _)) = True
isIndirectTransfer (NonTermSt Call{}) = True
isIndirectTransfer (NonTermSt _) = False

isIndirectTransfer (TermSt (Ret _ _)) = False
isIndirectTransfer (TermSt CondBr{}) = False
isIndirectTransfer (TermSt (Br _ _)) = False
isIndirectTransfer (TermSt Switch{}) = False
isIndirectTransfer (TermSt IndirectBr{}) = True
-- TODO(TermSt (zchn)): Think about Invoke, which is similar to call
-- but has multiple return addresses
isIndirectTransfer (TermSt Invoke{}) = False
isIndirectTransfer (TermSt (Resume _ _)) = False
isIndirectTransfer (TermSt (Unreachable _)) = False


getBasicBlocks :: Module -> [BasicBlock]
getBasicBlocks theModule =
  let definitions = moduleDefinitions theModule
  in
    concat $ mapMaybe
    (\def ->
      case def of
        (GlobalDefinition (Function _ _ _ _ _ _ _ _ _ _ _ _ _ _ bbs))
          -> Just bbs
        _ -> Nothing) definitions

getStatements :: BasicBlock -> [Statement]
getStatements (BasicBlock _ namedInstructions namedTerminator) =
  let unName (_ := a) = a
      unName (Do a) = a
  in
    map (NonTermSt . unName) namedInstructions ++ [TermSt (unName namedTerminator)]


-- | 'listIndirectTransfer' returns the list of indirect control flow
-- transfers
listIndirectTransfer :: Module -> [String]
listIndirectTransfer theModule =
    map show $ filter isIndirectTransfer $ concatMap getStatements $
      getBasicBlocks theModule
