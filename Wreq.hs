{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- CSC 395 (Fall 2017)
--
-- Live Music Programming: The Sound of Poetry
--
-- Zhi Chen and Erin Gaschott
--
-- References:
-- http://codereview.stackexchange.com/questions/115066/using-wreq-and-lens-libraries-to-query-prosper-for-account-info
-- http://stackoverflow.com/questions/38641568/extracting-a-list-of-values-from-a-list-of-maybes-without-fromjust
--
-- Notes:
-- The application consumes the Words API, which can be accessed by the X-Mashape-Key, 4XcFUg43ADmsh4L7DL6oOK6tGNErp1jzZeRjsn7wu8Nj8bkGIe
-- The application is limited to 2,500 free requests per day. Additional requests will be charged.
--
-- We would like to thank our beloved instructor, Peter Michael-Osera, for assisting us
-- in overcoming many challenges and difficulties.
--------------------------------------------------------------------------------

module Project where 

import Control.Exception as E
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens (key)
import Data.Aeson.Lens (_String, key)
import Data.Aeson (Value)
import Data.ByteString.Char8
import Data.Char
import Data.List.Split
import Data.Map as Map
import Data.Maybe
import Data.Monoid ((<>))
import Data.Scientific
import Data.Text
import Data.Text (Text)
import Data.Text.Strict.Lens
import Network.HTTP.Client
import Network.Wreq
import Prelude hiding (catch)
import System.IO
import System.IO.Error
import Text.Regex (splitRegex, mkRegex)

-- String Constant
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

-- Parses a string by regex into a list of texts
parseIntoWords :: String -> [Text]
parseIntoWords s = [(stringToText u) | u <- (splitRegex (mkRegex "[^a-zA-Z+]") s), u /= ""]

-- Makes a GET request to the Words API for a specific word
getSyllables :: Text -> IO (Maybe Value)
getSyllables input = do
    let opts = defaults & header "Accept" .~ ["application/json"] & header "X-Mashape-Key" .~ ["4XcFUg43ADmsh4L7DL6oOK6tGNErp1jzZeRjsn7wu8Nj8bkGIe"]
    resp <- (getWith opts (createURL (input <> "/syllables"))) `E.catch` handler
    let remaining = (resp ^. Network.Wreq.responseHeader "X-RateLimit-requests-Remaining")
    System.IO.putStr "Free requests remaining: "
    Data.ByteString.Char8.putStrLn remaining
    return (resp ^? Network.Wreq.responseBody . key "syllables" . key "count")
    where
      handler e@(StatusCodeException s _ _)
          | s ^. statusCode == 404 = ioError (userError ((textToString input) ++ " is not defined in Words API."))

-- Gets the syllables from a list of maybe values
extractSyllables :: [(Maybe Value)] -> [Double]
extractSyllables r = [(toRealFloat x) | (Number x) <- (Prelude.concatMap customMaybeToList r)]
    where
      customMaybeToList :: Maybe Value -> [Value]
      customMaybeToList Nothing = [(Number 1)]
      customMaybeToList (Just x) = [x]

-- Main
main :: IO ()
main = do
    System.IO.putStr "Please enter a line of poetry or enter 'Fin' to finish: "
    input <- System.IO.getLine
    if ("Fin" == input)
        then return ()
        else do
            result <- sequence (Prelude.map (\i -> (getSyllables i) `E.catch` handler) (parseIntoWords input))
            print input
            print (extractSyllables result)
            main
    where
        handler :: IOError -> IO (Maybe Value)
        handler _ = return Nothing