{-# LANGUAGE OverloadedStrings #-}

module Project where 

-- Referenced: http://codereview.stackexchange.com/questions/115066/using-wreq-and-lens-libraries-to-query-prosper-for-account-info
-- X-Mashape-Key, 4XcFUg43ADmsh4L7DL6oOK6tGNErp1jzZeRjsn7wu8Nj8bkGIe, limited to 2,500 requests per day
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens (_String, key)
import Data.Text.Strict.Lens
import Data.Map as Map
import Data.Monoid ((<>))
import Data.Aeson (Value)
import Data.Aeson.Lens (key)
import Data.Text (Text)

--String Constants
wordsAPIAddress :: Text
wordsAPIAddress = "https://wordsapiv1.p.mashape.com/words/"

createURL :: Text -> String
createURL target = (wordsAPIAddress <> target) ^. unpacked

getSyllables :: IO (Maybe Value)
getSyllables = do
  let opts = defaults & header "Accept" .~ ["application/json"] & header "X-Mashape-Key" .~ ["4XcFUg43ADmsh4L7DL6oOK6tGNErp1jzZeRjsn7wu8Nj8bkGIe"]
  resp <- ((getWith opts (createURL "success/syllables")) >>= asJSON) :: IO (Response Value)
  return(resp ^? responseBody . key "syllables" . key "count")

main :: IO ()
{--main = do
    (Just n) <- getSyllables
    putStrLn "Hello World!"
    print n--}
main = do
    (Just n) <- getSyllables
    putStrLn "Hello World!"
    print n
