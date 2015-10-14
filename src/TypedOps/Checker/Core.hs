{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module TypedOps.Checker.Core where

import System.Console.ArgParser
import System.Console.ArgParser.QuickParams
import Control.Applicative
import Data.Char (toLower)

class Env a where
  toEnvName :: Show a => a -> String
  toEnvName = (map toLower) . show

data DefaultEnv =
    Development
  | Integ
  | Staging
  | Production
  deriving (Show, Eq, Enum, Bounded)

instance Env DefaultEnv

data Checker = Checker
  { checkerConfigFile :: String
  } deriving (Show, Eq)

defaultArgsParser :: ParserSpec Checker
defaultArgsParser = Checker
  `parsedBy` optFlag "thresholds.config" "config"

defaultCommandLineInterface :: IO (CmdLnInterface Checker)
defaultCommandLineInterface = mkApp defaultArgsParser

commandLineInterface :: ParserSpec a -> IO (CmdLnInterface a)
commandLineInterface = mkApp


