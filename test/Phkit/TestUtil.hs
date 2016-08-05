module Phkit.TestUtil (linesDiff,
                       normalizeNumbers,
                       normalizeVars,
                       sortedLinesDiff) where

import qualified Data.Algorithm.DiffContext as DADC
import qualified Data.Algorithm.DiffOutput as DADO
import qualified Data.List as DL
import qualified Text.Regex as TR

linesDiff :: String -> String -> String
linesDiff s1 s2 =
  unlines $ map DADO.ppDiff $ DADC.getContextDiff 3 (lines s1) (lines s2)

sortedLinesDiff :: String -> String -> String
sortedLinesDiff s1 s2 =
  linesDiff (unlines $ DL.sort $ lines s1) (unlines $ DL.sort $ lines s2)

normalizeNumbers :: String -> String
normalizeNumbers inp =
      TR.subRegex (TR.mkRegex "[0-9]+") inp "N"

normalizeVars :: String -> String
normalizeVars inp =
      TR.subRegex (TR.mkRegex "%[0-9]+") inp "N"
