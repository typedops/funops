module Main (main) where

import TypedOps.Checker.Core
import System.Console.ArgParser

main :: IO ()
main = do
  interface <- defaultCommandLineInterface
  runApp interface print
