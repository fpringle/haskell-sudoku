module Sudoku.Tests.Generate where

import System.Random

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Instances.Tuple

import Sudoku.Defs
import Sudoku.Generate
import Sudoku.Validity
import Sudoku.Tests.TestUtils

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
  quickCheck $ once $ propGenRandomSolvedIsRandom
  quickCheck $ withMaxSuccess 10 propGenRandomFromGenIsRepeated
