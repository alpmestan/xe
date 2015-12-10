{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
import CLI
import Control.Lens hiding (elements, children)
import Control.Monad
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Time.Clock
import Network.Wreq
import Numeric
import Text.Taggy.Lens
import System.IO
import System.Exit (exitFailure)
import Types

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Options.Applicative as O

-- | Given an exchange rates listing page for a given date,
--   extract this listing as a pair of (currency symbol, rate)
--   where rate is the 'currency -> sgd' value.
ratesFromPage :: LT.Text -> [(Currency, Double)]
ratesFromPage page =
  page ^.. html
         . allAttributed (ix "id" . only "historicalRateTbl")
         . elements
         . named (only "tbody")
         . elements
         . named (only "tr")
         . children
         . to rate
         . _Just

  where rate row = do
          code <- row ^? ix 0.elements.named (only "a").contents
          val  <- row ^? ix 2.contents.to readDouble._Just
          return (code, val)

-- | Fetch the content of the page at the given url
visit :: String     -- ^ url
      -> IO LT.Text -- ^ page content
visit = fmap toText
      . getWith opt

  where toText = view (responseBody . to (decodeUtf8With lenientDecode))
        opt    = defaults & header "User-Agent" .~ [ua]
        ua     = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.71 Safari/537.36"

baseURL :: String
baseURL = "http://www.xe.com/currencytables/"

urlForArg :: Arg -> String
urlForArg (Arg srcC _ mdate _) =
  baseURL ++ "?from=" ++ T.unpack srcC
          ++ maybe "" (\d -> "&date=" ++ dayToS d) mdate

fatal :: String -> IO ()
fatal err = hPutStrLn stderr err >> exitFailure

main :: IO ()
main = do
  arg   <- O.execParser opts
  rates <- fmap ratesFromPage $ visit (urlForArg arg)

  when (null rates) $ fatal "Invalid source currency or date."

  case lookup (target arg) rates of
    Nothing   -> fatal $ "Couldn't find target currency " ++ T.unpack (target arg)
    Just rate -> formatOutput rate arg >>= putStrLn

formatOutput :: Double -- ^ exchange rate
             -> Arg    -- ^ parameters of the program
             -> IO String
formatOutput rate arg = fmap format getDay

  where getDay = maybe (fmap utctDay getCurrentTime) return (day arg)

        format d = unwords
          [ showMoney (amount arg)
          , T.unpack (src arg)
          , "="
          , showMoney (amount arg * rate)
          , T.unpack (target arg)
          , "(Exchange rate from XE.com on"
          , dayToS d ++ ":"
          , "1"
          , T.unpack (src arg)
          , "="
          , show rate
          , T.unpack (target arg) ++ ")"
          ]

        showMoney a = showFFloat (Just 2) a ""
