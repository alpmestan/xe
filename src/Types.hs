module Types where

import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Format

-- | Currency symbol (EUR, SGD, ...)
type Currency = Text

-- | This program's input. The user must specify
--   the source and target currency symbols
--   (e.g USD to EUR) and an optional date. If no
--   date is specified, the program will get today's
--   exchange rate.
data Arg = Arg
  { src    :: Currency    -- ^ "source" currency
  , target :: Currency    -- ^ "target" currency
  , day    :: (Maybe Day) -- ^ date. TODO: change to a proper date type
  } deriving (Eq, Show)

-- | Parse a date as YYYY-MM-DD into a proper day type
sToDay :: String -> Day
sToDay = readTime defaultTimeLocale "%F"

-- | Format a day as YYYY-MM-DD
dayToS :: Day -> String
dayToS = formatTime defaultTimeLocale "%F"
