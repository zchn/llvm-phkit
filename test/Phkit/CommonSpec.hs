module Phkit.CommonSpec (spec) where

import Test.Hspec (describe, it, shouldBe, Spec)

-- | Features under test
import Phkit.Common (prefix)

spec :: Spec
spec =
  describe "prefix tests" $
      it "converts pure Module to C Module then Assembly" $
      length prefix `shouldBe` 5
