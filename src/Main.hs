{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core
import Snap.Http.Server
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Data.ByteString.Char8(pack, unpack)
import Data.ConfigFile
import Data.Either
import Data.Either.Utils
import System.Environment(getArgs)
import System.Process
import System.IO
import Safe(readMay)
import System.IO.Temp(withSystemTempFile)
import qualified Data.ByteString as B

-- Requires wkhtmltopdf, xvfb to be installed.

-- TODO
-- [ ] Validate src, it should not be possible to escape bash!
-- ============ good enough ======== 
-- [ ] Use PDFCrowds API so that it's easy to switch.
-- [ ] HTTP Status Codes on Errors.
-- [ ] On startup, check that dependencies (wkhtmltopdf, xvfb) are installed.
-- [ ] Nicer error messages when config-file doesn't exist or misses properties.
-- [ ] Nicer error messages when PageSize is not one of the enumerated options. 
-- [ ] Seems only one xvfb-run can run at a time.

-- Test it like this:
-- curl -v http://localhost:8000 -d src="https://www.google.se" -d username='henrik@hencjo.com' -d key='RzNIKegEXLOt44WwRsx3OH5ZPZiMkKLo' -d page-size=A4  > meow.pdf && zathura meow.pdf

newtype Username = Username String deriving (Eq)
newtype ApiKey = ApiKey String deriving (Eq)
newtype SrcUrl = SrcUrl String deriving (Show)

instance Show Username where
    show (Username a) = show a
instance Show ApiKey where
    show (ApiKey a) = show a

data PageSize = A4 | Letter deriving (Show, Read)
type Credentials = (Username, ApiKey)

data PdfConfig = PdfConfig {
  credentials :: Credentials,
  port :: Int
} deriving (Show)

main :: IO ()
main = do
    a <- getArgs
    let configFile = case a of 
                        f:_ -> f
                        _   -> error "Expected path to config file as argument."
    putStrLn ("Reading configuration from " ++ configFile)
    c <- config configFile
    putStrLn (show c)
    httpServe (setPort (port c) emptyConfig) (pdfHandler $! c)  
    
config :: FilePath -> IO PdfConfig
config filePath = do
    val <- readfile emptyCP filePath
    let cp = forceEither val
    let webPort = forceEither $ (get cp "DEFAULT" "web.port")::Int
    let username = Username <$> forceEither $ get cp "DEFAULT" "api.user"
    let apiKey = ApiKey <$> forceEither $ get cp "DEFAULT" "api.key"
    return (PdfConfig (username, apiKey) webPort)

missing :: Request -> String -> Either String String
missing request param = case (rqPostParam (pack param) request) of 
                          (Just (v:_)) -> Right (unpack v)
                          _            -> Left ("Missing parameter \"" ++ param ++ "\"")

missing2 :: Either String (Maybe a) -> String -> Either String a
missing2 e param = case e of 
                    (Left s     )    -> Left s
                    (Right (Just a)) -> Right a
                    otherwise        -> Left ("Missing parameter \"" ++ param ++ "\"")

data PdfRequest = PdfRequest {
  username :: Username,
  key :: ApiKey,
  src :: SrcUrl,
  pageSize :: PageSize
}

pdfRequest :: Request -> Either [String] PdfRequest
pdfRequest request = case oscar of 
                       (Right pdf)  -> Right pdf
                       _            -> Left errors
    where 
      username = Username <$> (missing request "username")
      key      = ApiKey <$> (missing request "key")
      src      = SrcUrl <$> (missing request "src")
      pageSize = missing2 (fmap (\s -> (readMay s)::(Maybe PageSize)) (missing request "page-size") ) "page-size"
      errors   = lefts [
        (show <$> username),
        (show <$> key),
        (show <$> src),
        (show <$> pageSize) ]
      oscar    = PdfRequest <$> username <*> key <*> src <*> pageSize

pdfHandler :: PdfConfig -> Snap ()
pdfHandler config = (getsRequest (pdfRequest >=> (auth config))) >>= either (writeBS . pack . Prelude.concat) pdfAct

auth :: PdfConfig -> PdfRequest -> Either [String] PdfRequest
auth config req
     | authenticates = (Right req)
     | otherwise     = (Left ["Authorisation failed"])
     where
        authenticates = (username req, key req) == (credentials config)

callback :: SrcUrl -> PageSize -> FilePath -> Handle -> IO (B.ByteString)
callback (SrcUrl url) pageSize tempFile tempHandle = do
    hClose tempHandle
    devNull <- openFile "/dev/null" AppendMode
    let commandLine = "xvfb-run wkhtmltopdf --page-size " ++ (show pageSize) ++ " " ++ url ++ " " ++ tempFile
    putStrLn commandLine
    let cl = words commandLine
    (_, _, _, pHandle) <- createProcess (proc (head cl) (tail cl)){ std_err = Inherit } 
    exitCode <- waitForProcess pHandle
    hClose devNull
    B.readFile tempFile

pdfAct :: PdfRequest -> Snap ()
pdfAct req = do 
    modifyResponse $ setContentType "application/pdf"
    bs <- liftIO (withSystemTempFile "wkhtmltopdf-rest.pdf" (callback (src req) (pageSize req)))
    writeBS bs
