module BEL.BatteriesMain (ioToday, loremChars)
where

import           System.Random (randomR, mkStdGen)
import Data.Time (Day, fromGregorian)
import Data.Time.Clock
import Data.Time.Format (formatTime, defaultTimeLocale)

pureRandomInt :: Int -> Int -> Int -> Int
pureRandomInt seed minVal maxVal =
    let gen = mkStdGen seed
        (val, _) = randomR (minVal, maxVal) gen
    in val

formatISODate :: Day -> String
formatISODate day = formatTime defaultTimeLocale "%Y-%m-%d" day

exampleDay :: Day
exampleDay = fromGregorian 2025 9 18

isoDate :: String
isoDate = formatISODate exampleDay


ioToday :: IO String
ioToday = do
    t <- getCurrentTime
    -- pure $ formatISODate exampleDay
    pure $ formatISODate (utctDay t)

loremChars :: Int -> String
loremChars n 
  | n <= 0    = ""
  | otherwise = take n $ cycle "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
