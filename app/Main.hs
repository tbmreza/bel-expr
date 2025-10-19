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
                                 , parseTest
                                 )

import BEL

-- ??: rm app/Main.hs if not too much trouble
-- main :: IO ()
-- main = do
--     let envNew :: Env = HM.fromList [("yyyymmdd", Aeson.String "19700101"), ("year", Aeson.String "2025")]
--     case renderAesonTemplate envNew (Aeson.String "{{today}}") of
--         _ -> putStrLn "atas"
--
--     -- case renderTemplate envNew "http://localhost:9999/path{{yyyymmdd}}tail" of
--     case renderTemplate envNew "aa{{today}}bbb" of  -- ok templateP1
--     -- case renderTemplate envNew "{ "start": "{{today}}" }" of
--     -- case renderTemplate envNew "{{year}}betw{{yyyymmdd}}" of
--         Left res -> putStrLn res
--         Right res -> putStrLn res
main :: IO ()
main = do pure ()
    -- -- res <- parseTest litP "hello()"
    -- res <- parseTest templateP "hello(){{today()}}bb"
    -- let envNew :: Env = HM.fromList [("yyyymmdd", Aeson.String "19700101"), ("year", Aeson.String "2025")]
    -- case renderAesonTemplate envNew (Aeson.String "{{today}}") of
    --     _ -> putStrLn "atas"
    --
    -- -- ok templateP1
    -- -- case renderTemplate envNew "aa{{today}}bbb" of
    -- -- case renderTemplate envNew "12 != 12" of
    -- -- case renderTemplate envNew "true" of
    -- case renderTemplate envNew "{{" of
    --     Left id -> putStrLn id
    --     Right res -> putStrLn res
