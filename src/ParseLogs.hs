{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Attoparsec.Char8
import Data.Word

import Control.Applicative ((<$>), (<|>))

type W16 = Word16

data IP = IPv4 Word8 Word8 Word8 Word8
        | IPv6 W16 W16 W16 W16 W16 W16 W16 W16
        deriving (Eq, Show)

data Host = Host String

parseIPv4 :: Parser IP
parseIPv4 = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  d4 <- decimal
  return $ IPv4 d1 d2 d3 d4

parseIPv6 :: Parser IP
parseIPv6 = do
  h1 <- hexadecimal
  char ':'
  h2 <- hexadecimal
  char ':'
  h3 <- hexadecimal
  char ':'
  h4 <- hexadecimal
  char ':'
  h5 <- hexadecimal
  char ':'
  h6 <- hexadecimal
  char ':'
  h7 <- hexadecimal
  char ':'
  h8 <- hexadecimal
  return $ IPv6 h1 h2 h3 h4 h5 h6 h7 h8

parseIP :: Parser IP
parseIP = parseIPv4 <|> parseIPv6

parseHost :: Parser Host
parseHost = Host <$> string

main :: IO ()
main = do
  print $ parseOnly parseIPv4 "131.45.68.123"
  print $ parseOnly parseIPv4 "2001:db8:85a3:0:0:8a2e:370:7334"
  print $ parseOnly parseIPv6 "131.45.68.123"
  print $ parseOnly parseIPv6 "2001:db8:85a3:0:0:8a2e:370:7334"
  print $ parseOnly parseIP "2001:db8:85a3:0:0:8a2e:370:7334"
  print $ parseOnly parseIP "131.45.68.123"

