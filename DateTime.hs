{-# LANGUAGE CPP #-}
-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- | Gets the current date and time as a 'String'. Currently
-- supports RFC 3339 time format or thereabouts. This module
-- only exists because doing this is kind of a mess currently.
module DateTime (getTimeRFC3339)
where

import Data.Time.Format
import Data.Time.LocalTime
-- https://mail.haskell.org/pipermail/haskell-cafe/2010-December/086833.html
#if ! MIN_VERSION_time(1,5,0)
import System.Locale (defaultTimeLocale)
#endif

-- | Get a current date+time string in a reasonable format.
getTimeRFC3339 :: IO String
getTimeRFC3339 = do
  dateTime <- getZonedTime
  return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%z" dateTime
