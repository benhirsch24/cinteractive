{-# LANGUAGE OverloadedStrings #-}
import Language

import Data.Conduit as DC
import Data.Conduit.Binary as CB
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid
import Data.Enumerator (run_, enumList, ($$))
import Data.Functor ((<$>))
import Data.ByteString.Char8 (pack)

appJson = "application/json"
textPlain = "text/plain"

main = do
   let port = 3000
   putStrLn $ "Listening on port " ++ show port
   run port app

app :: Request -> ResourceT IO Response
app req =
   case pathInfo req of
      ["parse"] -> requestBody req DC.$$ parse
      x -> do
         ihtml <- CB.sourceFile "index.html" DC.$$ await
         return $ index ihtml

parse = do
   body <- await
   return $ ResponseBuilder status200 [ (hContentType, textPlain) ] $
      case body of
         Nothing -> copyByteString . pack $ "{'text': 'blah'}"
         Just (body) -> copyByteString . pack $ "{'text': 'blah'}"

index html = ResponseBuilder status200 [ ("Content-Type", "text/html") ] $ case html of
   Nothing -> copyByteString "<html><head><title>Whoops!</title></head><body><h1>Whoops! clearly something bad happened.</h1></body></html>"
   (Just html) -> copyByteString html
