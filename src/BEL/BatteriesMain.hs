module BEL.BatteriesMain (ioToday, ioYear, ioDayOfMonth, loremChars)
where

import Data.Time (Day, toGregorian)
import Data.Time.Clock
import Data.Time.Format (formatTime, defaultTimeLocale)

formatISODate :: Day -> String
formatISODate day = formatTime defaultTimeLocale "%Y-%m-%d" day


ioToday :: IO String
ioToday = do
    t <- getCurrentTime
    -- pure $ formatISODate exampleDay
    pure $ formatISODate (utctDay t)

ioYear :: IO String
ioYear = do
    t <- getCurrentTime
    let (y, _, _) = toGregorian (utctDay t)
    pure $ show y

ioDayOfMonth :: IO String
ioDayOfMonth = do
    t <- getCurrentTime
    let (_, _, d) = toGregorian (utctDay t)
    pure $ show d

loremChars :: Int -> String
loremChars n 
  | n <= 0    = ""
  | otherwise = take n $ cycle "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
