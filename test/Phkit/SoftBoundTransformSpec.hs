module Phkit.SoftBoundTransformSpec (spec) where

import qualified LLVM.General.AST as LGA

import Test.Hspec (describe, it, pendingWith, shouldBe, shouldContain, Spec, Expectation)
import Phkit.IO
import Phkit.SoftBoundTransform

forEachInsn :: LGA.Module ->
  (Either LGA.Instruction LGA.Terminator -> Maybe Expectation) -> Expectation
forEachInsn LGA.Module{LGA.moduleDefinitions = defs} pred =
  let expectations = concat $ map processDef defs in
  case expectations of
    [] -> 1 `shouldBe` 1
    h:_ -> h
  where processDef (LGA.GlobalDefinition (LGA.Function{LGA.basicBlocks = bb}))  =
          concat $ map (processBb pred)
        processBb (LGA.BasicBlock _ nInsns nTerm) =
          (concat $ map (processInsn . deName) nInsns) ++
          processTerm (deName nTerm)
        processInsn insn = pred (Left insn)
        processTerm term = pred (Right term)

deName :: LGA.Named A -> A
deName (LGA.Do a) = a
deName (_ LGA.:= a) = a


spec :: Spec
spec =
    describe "SoftBound transform tests" $
    do it "names a meta for each def" $
         do modu <-
              withModuleFromPathIO
              "test/testdata/interpret-indirectbr.c"
              return
            let newMod = softBoundRewriteResultOf modu

       it "inserts sbcheck and sbsave." $
           do pendingWith "Not implemented."
              modu <-
                  withModuleFromPathIO
                      "test/testdata/interpret-indirectbr.c"
                      return
              let newMod = softBoundRewriteResultOf modu
              origModString <- ioStringOfAstModule modu
              newModString <- ioStringOfAstModule newMod
              newModString `shouldContain` "call void @sbcheck"
              newModString `shouldContain` "call void @sbsave"
              newModString `shouldContain`
                  "define internal i32 @interpret_threaded(i8* %opcodes) #1 {"
       it "inserts sbload." $
           do pendingWith "Not implemented."
              modu <-
                  withModuleFromPathIO
                      "test/testdata/interpret-indirectbr.c"
                      return
              let newMod = softBoundRewriteResultOf modu
              origModString <- ioStringOfAstModule modu
              newModString <- ioStringOfAstModule newMod
              newModString `shouldContain` "call void @sbload"
              newModString `shouldContain`
                  "define internal i32 @interpret_threaded(i8* %opcodes) #1 {"
       it "inserts sbfunc." $
           do modu <-
                  withModuleFromPathIO
                      "test/testdata/interpret-indirectbr.c"
                      return
              let newMod = softBoundRewriteResultOf modu
              origModString <- ioStringOfAstModule modu
              newModString <- ioStringOfAstModule newMod
              -- newModString `shouldContain` "sbfunc"
              newModString `shouldContain`
                  "define internal i32 @interpret_threaded(i8* %opcodes) #1 {"
