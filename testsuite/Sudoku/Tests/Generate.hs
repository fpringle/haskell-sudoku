{-
Copyright (c) 2022, Frederick Pringle
All rights reserved.

This source code is licensed under the BSD-style license found in the
LICENSE file in the root directory of this source tree.
-}

module Sudoku.Tests.Generate where

import           System.Random

import           Test.QuickCheck
import           Test.QuickCheck.Instances.Tuple
import           Test.QuickCheck.Monadic

import           Sudoku.Generate
import           Sudoku.Tests.TestUtils
import           Sudoku.Types
import           Sudoku.Validity

propGenSolvedIsSolved :: Pos -> Int -> Bool
propGenSolvedIsSolved pos val = isValid $ generateSolvedFromFirst pos val

propGenRandomSolvedIsRandom :: Property
propGenRandomSolvedIsRandom = monadicIO $ do
  s1 <- run generateSolved
  s2 <- run generateSolved
  return $ counterexample (show s1 ++ " | " ++ show s2) (s1 /= s2)

propGenRandomFromGenIsRepeated :: StdGen -> Property
propGenRandomFromGenIsRepeated g = monadicIO $ do
  s1 <- generateSolvedFromGen g
  s2 <- generateSolvedFromGen g
  return $ s1 === s2

instance Arbitrary StdGen where
  arbitrary = mkStdGen <$> arbitrary

testGenerate :: IO ()
testGenerate = do
  quickCheck $ withMaxSuccess 10 $ forAll (genPos >*< elements [1 .. 9]) $ uncurry propGenSolvedIsSolved
  quickCheck $ once propGenRandomSolvedIsRandom
  quickCheck $ withMaxSuccess 10 propGenRandomFromGenIsRepeated
