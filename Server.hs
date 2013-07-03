{-# LANGUAGE OverloadedStrings #-}
module Main where
import Language
import System.Environment (getEnv)
import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Aeson.Encode (fromValue)
import Data.Conduit as DC
import Data.Conduit.Binary as CB
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Blaze.ByteString.Builder (copyByteString)
import Blaze.ByteString.Builder.Char8 (fromLazyText, fromString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import Data.Enumerator (run_, enumList, ($$))
import Data.Functor ((<$>))
import Data.ByteString.Char8 (pack)
import Control.Arrow ((>>>))
import Data.Text (intercalate, unpack, splitOn)
import Data.Text.Lazy.Builder (toLazyText)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

appJson = "application/json"
textPlain = "text/plain"
textHtml = "text/html"
textCSS = "text/css"
textJS = "text/javascript"

ctHTML = [ (hContentType, textHtml) ]
ctPlain = [ (hContentType, textPlain) ]
ctJson = [ (hContentType, appJson) ]

response200 = ResponseBuilder status200
responseHtml = response200 ctHTML
response400 = ResponseBuilder status400
errorResponse = response400 ctHTML $ copyByteString "<html><head><title>Whoops!</title></head><body><h1>Whoops! clearly something bad happened.</h1></body></html>"

fileResponse ct path = ResponseFile status200 ct path Nothing
htmlFileResponse path = ResponseFile status200 ctHTML path Nothing

resolveContentType "css" = textCSS
resolveContentType "html" = textHtml
resolveContentType "js"  = textJS
resolveContentType _     = textPlain

main = do
   port <- fromIntegral <$> read <$> getEnv "PORT"
   putStrLn $ "Listening on port " ++ show port
   run port app

-- app :: Application
-- TODO: this whole static path thing is clearly vulnerable
app :: Request -> ResourceT IO Response
app req =
   case pathInfo req of
      ["parse"] -> requestBody req DC.$$ parse
      path@("static":rest) -> let filepath    = unpack $ intercalate "/" path
                                  ext         = last . splitOn "." $ last rest
                                  contentType = resolveContentType ext
                              in  return $ fileResponse [(hContentType, contentType)] filepath
      path@("docs":rest) -> let   filepath    = unpack $ intercalate "/" path
                                  ext         = last . splitOn "." $ last rest
                                  contentType = resolveContentType ext
                              in  return $ fileResponse [(hContentType, contentType)] filepath
      _ -> return $ htmlFileResponse "index.html"

parse :: Sink BU.ByteString (ResourceT IO) Response
parse = do
   body <- await
   return $ case body of
      Nothing -> response400 ctPlain $ copyByteString "Error! Nothing received"
      Just body -> let ast = bsToAST body
                   in  case ast of
                           Right ast -> let json = fromLazyText . toLazyText . fromValue . toJSON $ ast
                                        in  response200 ctJson json
                           Left  err -> response400 ctPlain $ fromString $ show err
