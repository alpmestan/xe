module CLI where

import Data.Text (pack)
import Data.Time.Calendar (Day)
import Options.Applicative as O
import Types

-- | Our final option parser
opts :: O.ParserInfo Arg
opts = O.info (parseArg <**> O.helper)
  ( O.fullDesc
 <> O.progDesc "Get exchange rates from xe.com."
 <> O.header "xe - Command-line utility to get xe.com's exchange rates"
  )

parseArg :: O.Parser Arg
parseArg = Arg <$> srcCurrency <*> targetCurrency <*> optional date

srcCurrency :: O.Parser Currency
srcCurrency = fmap pack . strOption $
     O.long "src"
  <> O.short 's'
  <> O.metavar "CUR"
  <> O.help "three letter symbol for the source currency"

targetCurrency :: O.Parser Currency
targetCurrency = fmap pack . strOption $
     O.long "target"
  <> O.short 't'
  <> O.metavar "CUR"
  <> O.help "three letter symbol for the target currency"

date :: O.Parser Day
date = fmap sToDay . strOption $
     O.long "date"
  <> O.short 'd'
  <> O.metavar "YYYY-MM-DD"
  <> O.help (
       concat [ "(Optional) date we want the exchange rate for. "
              , "When no date is specified, the program gets today's "
              , "exchange rate"
              ]
     )
