{-# LANGUAGE OverloadedStrings #-}

module Project where 

-- Referenced: http://codereview.stackexchange.com/questions/115066/using-wreq-and-lens-libraries-to-query-prosper-for-account-info
--- http://stackoverflow.com/questions/38641568/extracting-a-list-of-values-from-a-list-of-maybes-without-fromjust
-- X-Mashape-Key, 4XcFUg43ADmsh4L7DL6oOK6tGNErp1jzZeRjsn7wu8Nj8bkGIe, limited to 2,500 requests per day

import Control.Exception as E
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens (key)
import Data.Aeson.Lens (_String, key)
import Data.Aeson (Value)
import Data.Char
import Data.List.Split
import Data.Map as Map
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text
import Data.Text (Text)
import Data.Text.Strict.Lens
import Network.HTTP.Client
import Network.Wreq
import System.IO
import System.IO.Error
import Text.Regex (splitRegex, mkRegex)

import Data.Scientific
{--:set -XOverloadedStrings--}

--String Constants
wordsAPIAddress :: Text
wordsAPIAddress = "https://wordsapiv1.p.mashape.com/words/"

-- Creates the URL to consume the Words API
createURL :: Text -> String
createURL target = (wordsAPIAddress <> target) ^. unpacked

-- Converts texts to strings by unpacking
textToString :: Text -> String
textToString t = t ^. unpacked

-- Converts strings to texts by packing
stringToText :: String -> Text
stringToText s = s ^. packed

-- Parse a string by regex into a list of words that are text
parseIntoWords :: String -> [Text]
parseIntoWords s = [(stringToText u) | u <- (splitRegex (mkRegex "[^a-zA-Z+]") s), u /= ""]

-- 
extractNums :: [Value] -> [Double]
extractNums s = [(toRealFloat x) | (Number x) <- s]

-- Gets the syllable for the input from Words API
getSyllables :: Text -> IO (Maybe Value)
getSyllables input = do
  let opts = defaults & header "Accept" .~ ["application/json"] & header "X-Mashape-Key" .~ ["4XcFUg43ADmsh4L7DL6oOK6tGNErp1jzZeRjsn7wu8Nj8bkGIe"]
  resp <- ((getWith opts (createURL (input <> "/syllables"))) >>= asJSON) :: IO (Response Value)
  return(resp ^? Network.Wreq.responseBody . key "syllables" . key "count")

main = toTry `catchIOError` handler

toTry :: IO ()
toTry = forever $ do
    putStr "Enter a line of poetry: "
    input <- getLine
    print (parseIntoWords input)
    result <- sequence (Prelude.map getSyllables (parseIntoWords input))
    print (extractNums (Prelude.concatMap maybeToList result))


handler :: IOError -> IO ()
handler e = putStrLn "Error processing input. Please enter another line."