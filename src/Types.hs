module Types where

import Data.Text (Text)
import Data.Text.Read (double)
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
  , day    :: (Maybe Day) -- ^ date.
  , amount :: Double      -- ^ amount to convert
  } deriving (Eq, Show)

-- | Parse a date as YYYY-MM-DD into a proper day type
sToDay :: String -> Day
sToDay = readTime defaultTimeLocale "%F"

-- | Format a day as YYYY-MM-DD
dayToS :: Day -> String
dayToS = formatTime defaultTimeLocale "%F"

-- | Try to extract a 'Double' from a 'Text'. Used to parse exchange rates
--   from the HTML pages.
readDouble :: Text -> Maybe Double
readDouble = either (const Nothing) (Just . fst) . double
