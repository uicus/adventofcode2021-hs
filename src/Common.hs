{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Common
     ( day
     , Pr(..)
     , manyNames
     ) where

import System.IO
import Control.Monad

class Printable a where
  prepare :: a -> String

instance Printable Int where
  prepare = (' ':) . (++"\n") . show

instance Printable String where
  prepare = ('\n':)

data Pr = forall a. Printable a => Pr a
instance Printable Pr where
  prepare x = case x of Pr val -> prepare val

readTestcase :: (Handle -> IO a) -> String -> IO a
readTestcase = flip (flip withFile ReadMode)

readTestcases :: (Handle -> IO a) -> [String] -> IO [a]
readTestcases reader = flip forM (readTestcase reader)

solveSinglePair :: String -> a -> Int -> (a -> Pr) -> IO ()
solveSinglePair name testcase index solver = putStr ("Part " ++ show index ++ ", " ++ name ++ ":") >> (putStr $ prepare $ solver $ testcase)

day :: Int -> (Handle -> IO a) -> [String] -> [a -> Pr] -> IO ()
day number reader testsNames solvers = do
  putStrLn $ "######## Day " ++ show number ++ " ########"
  testcases <- readTestcases reader testsNames
  forM_ ((,) <$> zip testsNames testcases <*> zip [1 ..] solvers) (\((name, testcase), (index, solver)) -> solveSinglePair name testcase index solver)
  putStrLn ""

manyNames :: String -> Int -> [String]
manyNames base times = map ((base++) . show) [1 .. times]
