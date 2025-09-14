{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)
import qualified Data.Aeson.Types as Aeson (Value(..))
import qualified Text.Megaparsec.Char as C
import           Text.Megaparsec ( Parsec, (<|>), some, anySingle, many, choice
                                 , manyTill, lookAhead, try, eof, runParser
                                 )

import BEL

main :: IO ()
main = do
    let envNew :: Env = HM.fromList [("yyyymmdd", Aeson.String "19700101")]
    case renderTemplate envNew "prefix{{yyyymmdd}}suffix" of
        Left res -> putStrLn res
        Right res -> putStrLn res
