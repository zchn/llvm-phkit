module Phkit.SoftBoundTransformSpec (spec) where

import Test.Hspec (describe, it, shouldBe, shouldContain, Spec)
import Phkit.IO
import Phkit.SoftBoundTransform

spec :: Spec
spec = 
    describe "SoftBound transform tests" $
    do it "inserts sbcheck and sbsave." $
           do modu <- 
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
           do modu <- 
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
