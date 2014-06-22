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
import qualified Data.Text as T
import Data.Text.Encoding(encodeUtf8, decodeUtf8)
import System.Environment(getArgs)
import System.Process
import System.IO hiding (readFile)
import Safe(readMay)
import System.IO.Temp(withSystemTempFile)
import qualified Data.ByteString as ByteString

-- Requires wkhtmltopdf, xvfb to be installed.

-- TODO
-- ==== bad ====
-- [ ] Validate src, it should not be possible to escape bash!
-- ============ good enough ======== 
-- [ ] Use PDFCrowds API so that it's easy to switch.
-- [ ] HTTP Status Codes on Errors.
-- [ ] On startup, check that dependencies (wkhtmltopdf, xvfb) are installed.
-- [ ] Nicer error messages when config-file doesn't exist or misses properties.
-- [ ] Nicer error messages when PageSize is not one of the enumerated options. 
-- [ ] Seems only one xvfb-run can run at a time.

newtype Username = Username T.Text deriving (Eq)
newtype ApiKey = ApiKey T.Text deriving (Eq)
newtype SrcUrl = SrcUrl T.Text deriving (Show)
newtype Port = Port Int deriving (Show)

instance Show Username where
    show (Username a) = show a
instance Show ApiKey where
    show (ApiKey a) = show a

data PageSize = A4 | Letter deriving (Show, Read)
type Credentials = (Username, ApiKey)

data PdfConfig = PdfConfig {
  credentials :: Credentials,
  port :: Port
} deriving (Show)

data PdfRequest = PdfRequest {
  username :: Username,
  key :: ApiKey,
  src :: SrcUrl,
  pageSize :: PageSize
}

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
    return (PdfConfig (
        Username <$> T.pack <$> forceEither $ get cp "DEFAULT" "api.user",
        ApiKey <$> T.pack <$> forceEither $ get cp "DEFAULT" "api.key"
        )
        (Port (forceEither $ (get cp "DEFAULT" "web.port")::Int)))

missing :: Request -> T.Text -> Either T.Text T.Text
missing request param = case (rqPostParam (encodeUtf8 param) request) of 
                          (Just (v:_)) -> Right (decodeUtf8 v)
                          _            -> Left ("Missing parameter \"" `T.append` param `T.append` "\"")

missing2 :: Either T.Text (Maybe a) -> T.Text -> Either T.Text a
missing2 e param = case e of 
                    (Left s     )    -> Left s
                    (Right (Just a)) -> Right a
                    _                -> Left ("Missing parameter \"" `T.append` param `T.append` "\"")

pdfRequest :: Request -> Either [T.Text] PdfRequest
pdfRequest request = case oscar of 
                       (Right pdf)  -> Right pdf
                       _            -> Left errors
    where 
      username = Username <$> (missing request "username")
      key      = ApiKey <$> (missing request "key")
      src      = SrcUrl <$> (missing request "src")
      pageSize = missing2 (fmap (\s -> (readMay s)::(Maybe PageSize)) (T.unpack <$> (missing request "page-size"))) "page-size"
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
        authenticates = (username req, key req) == (credentials config)

callback :: SrcUrl -> PageSize -> FilePath -> Handle -> IO (ByteString.ByteString)
callback (SrcUrl url) pageSize tempFile tempHandle = do
    hClose tempHandle
    devNull <- openFile "/dev/null" AppendMode
    let commandLine = "xvfb-run wkhtmltopdf --page-size " ++ (show pageSize) ++ " " ++ (T.unpack url) ++ " " ++ tempFile -- dangerous
    putStrLn commandLine
    let cl = words commandLine -- dangerous
    (_, _, _, pHandle) <- createProcess (proc (head cl) (tail cl)){ std_err = Inherit } -- dangerous
    waitForProcess $! pHandle
    hClose devNull
    ByteString.readFile tempFile

pdfAct :: PdfRequest -> Snap ()
pdfAct req = do 
    bs <- liftIO (withSystemTempFile "wkhtmltopdf-rest.pdf" (callback (src req) (pageSize req)))
    modifyResponse $ setContentType "application/pdf"
    writeBS bs
