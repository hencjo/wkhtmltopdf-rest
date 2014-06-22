{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core
import Snap.Http.Server
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Data.ConfigFile
import Data.Either
import Data.Either.Utils
import Data.Maybe(listToMaybe)
import qualified Data.Text as T
import Data.Text.Encoding(encodeUtf8, decodeUtf8)
import Network.URI (URI, parseURI, scheme)
import System.Environment(getArgs)
import System.Process
import System.IO hiding (readFile)
import Safe(headMay,readMay)
import System.IO.Temp(withSystemTempFile)
import qualified Data.ByteString as ByteString

-- Requires wkhtmltopdf, xvfb to be installed.

-- TODO
-- ============ good enough ======== 
-- [ ] Use PDFCrowds API so that it's easy to switch.
-- [ ] HTTP Status Codes on Errors.
-- [ ] On startup, check that dependencies (wkhtmltopdf, xvfb) are installed.
-- [ ] Nicer error messages when config-file doesn't exist or misses properties.
-- [ ] Nicer error messages when PageSize is not one of the enumerated options. 
-- [ ] Nicer error message when Url is not a URL.
-- [ ] Seems only one xvfb-run can run at a time.

newtype Username = Username T.Text deriving (Eq, Show)
newtype ApiKey = ApiKey T.Text deriving (Eq, Show)
newtype SrcUrl = SrcUrl T.Text deriving (Show)
newtype Port = Port Int deriving (Show)

data PageSize = A4 | Letter deriving (Show, Read)
data Credentials = Credentials Username ApiKey deriving (Show, Eq)

data PdfConfig = PdfConfig {
  credentials :: Credentials,
  port :: Port
} deriving (Show)

data PdfRequest = PdfRequest {
  username :: Username,
  key :: ApiKey,
  src :: SrcUrl,
  pageSize :: PageSize
} deriving (Show)

main :: IO ()
main = do
    a <- getArgs
    let configFile = case a of 
                        f:_ -> f
                        _   -> error "Expected path to config file as argument."
    putStrLn ("Reading configuration from " ++ configFile)
    c <- config $! configFile
    putStrLn (show c)
    httpServe (setPort (port2 c) emptyConfig) (pdfHandler c)  
        where 
            port2 :: PdfConfig -> Int
            port2 pdfConfig = case (port pdfConfig) of 
                                 (Port p) -> p
    
config :: FilePath -> IO PdfConfig
config filePath = do
    val <- readfile emptyCP filePath
    let cp = forceEither val
    return (PdfConfig 
        (Credentials 
            (Username <$> T.pack <$> forceEither $ get cp "DEFAULT" "api.user")
            (ApiKey <$> T.pack <$> forceEither $ get cp "DEFAULT" "api.key")
        )
        (Port (forceEither $ (get cp "DEFAULT" "web.port")::Int)))

missing :: T.Text -> Maybe a -> Either T.Text a
missing p (Just a) = Right a
missing p _        = Left (T.concat ["Missing parameter \"", p,  "\""])

postParam :: Request -> T.Text -> Maybe T.Text
postParam request param = decodeUtf8 <$> (headMay =<< (rqPostParam (encodeUtf8 param) request))

validURI :: T.Text -> Maybe T.Text
validURI url = T.pack <$> show <$> (meow =<< (parseURI (T.unpack url)))
    where
        meow :: URI -> Maybe URI
        meow uri = listToMaybe (filter httpOrHttps [uri])
        httpOrHttps :: URI -> Bool 
        httpOrHttps uri = (scheme == "http" || scheme == "https")
            where 
                scheme = (Network.URI.scheme uri) 

pdfRequest :: Request -> Either [T.Text] PdfRequest
pdfRequest request = case oscar of 
                       (Right pdf)  -> Right pdf
                       _            -> Left errors
    where 
      post     = postParam request
      username = Username <$> (missing "username" $ post "username")
      key      = ApiKey <$> (missing "key" $ post "key")
      src      = SrcUrl <$> (missing "src" $ (validURI =<< (post "src")))
      pageSize = missing "page-size" ((\s -> (readMay s)::(Maybe PageSize)) =<< T.unpack <$> post "page-size")
      errors   = lefts [
        (show <$> username),
        (show <$> key),
        (show <$> src),
        (show <$> pageSize) ]
      oscar    = PdfRequest <$> username <*> key <*> src <*> pageSize

pdfHandler :: PdfConfig -> Snap ()
pdfHandler config = (getsRequest (pdfRequest >=> (auth config))) >>= either (writeText . T.concat) pdfAct

auth :: PdfConfig -> PdfRequest -> Either [T.Text] PdfRequest
auth config req
     | authenticates = (Right req)
     | otherwise     = (Left ["Authorisation failed"])
     where
        authenticates = Credentials (username req) (key req) == (credentials config)

callback :: SrcUrl -> PageSize -> FilePath -> Handle -> IO (ByteString.ByteString)
callback (SrcUrl url) pageSize tempFile tempHandle = do
    hClose tempHandle
    devNull <- openFile "/dev/null" AppendMode
    let commandLine = ["xvfb-run","wkhtmltopdf","--quiet","--disallow-local-file-access","--page-size",(show pageSize),(T.unpack url),tempFile] -- dangerous
    putStrLn (unwords commandLine)
    (_, _, _, pHandle) <- createProcess (proc (head commandLine) (tail commandLine)){ std_err = Inherit } -- dangerous
    waitForProcess $! pHandle
    hClose devNull
    ByteString.readFile tempFile

pdfAct :: PdfRequest -> Snap ()
pdfAct req = do 
    bs <- liftIO (withSystemTempFile "wkhtmltopdf-rest.pdf" (callback (src req) (pageSize req)))
    modifyResponse $ setContentType "application/pdf"
    writeBS bs
