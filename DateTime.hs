-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- | Handle standard date and time stuff.
module DateTime (getTimeRFC3339)
where

import Data.Time.Format
import Data.Time.LocalTime
import System.Locale (defaultTimeLocale)

-- | Get current date+time string in a reasonable format.
getTimeRFC3339 :: IO String
getTimeRFC3339 = do
  dateTime <- getZonedTime
  return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%z" dateTime
