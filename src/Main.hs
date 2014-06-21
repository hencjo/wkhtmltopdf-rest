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
import Data.UUID.V4(nextRandom)
import System.Process
import System.IO

-- Requires wkhtmltopdf, xvfb to be installed.

-- TODO
-- [ ] Authenticate against config-file.
-- ============ Pushable ========
-- [ ] Remove temporary files.
-- [ ] Use PDFCrowds API so that it's easy to switch.
-- [ ] HTTP Status Codes on Errors.
-- ============ good enough ======== 
-- [ ] On startup, check that dependencies (wkhtmltopdf, xvfb) are installed.
-- [ ] Validate correct values for pageSize
-- [ ] Implement pageSize
-- [ ] Validate src, it should not be possible to escape bash!

-- Test it like this:
-- curl -v http://localhost:8000 -d src="https://www.google.se" -d username='henrik@hencjo.com' -d key='RzNIKegEXLOt44WwRsx3OH5ZPZiMkKLo' -d page-size=A4  > meow.pdf && zathura meow.pdf

newtype Username = Username String deriving (Show, Eq)
type ApiKey = String
type Credentials = (Username, ApiKey)

data PdfConfig = PdfConfig {
  credentials :: Credentials
} deriving (Show)

main :: IO ()
main = do
    c <- config "pdf.conf"
    let p = pdfHandler c 
    quickHttpServe p 

config :: FilePath -> IO PdfConfig
config filePath = do
--    cp <- readfile emptyCP "pdf.conf"
    return (PdfConfig ((Username "henrik@hencjo.com"),"RzNIKegEXLOt44WwRsx3OH5ZPZiMkKLo"))

missing :: Request -> String -> Either String String
missing request param = case (rqPostParam (pack param) request) of 
                          (Just (v:_)) -> Right (unpack v)
                          _            -> Left ("Missing parameter \"" ++ param ++ "\"")

data PdfRequest = PdfRequest {
  username :: Username,
  key :: ApiKey,
  src :: String,
  pageSize :: String
} deriving (Show)

pdfRequest :: Request -> Either [String] PdfRequest
pdfRequest request = case oscar of 
                       (Right pdf)  -> Right pdf
                       _            -> Left errors
    where 
      username = fmap Username (missing request "username")
      key      = missing request "key"
      src      = missing request "src"
      pageSize = missing request "page-size"
      errors   = lefts [
        (fmap show username), 
        key, src, pageSize]
      oscar    = PdfRequest <$> username <*> key <*> src <*> pageSize

pdfHandler :: PdfConfig -> Snap ()
pdfHandler config = (getsRequest (pdfRequest >=> (auth config))) >>= either (writeBS . pack . Prelude.concat) pdfAct

auth :: PdfConfig -> PdfRequest -> Either [String] PdfRequest
auth config req
     | authenticates = (Right req)
     | otherwise     = (Left ["Authorisation failed"])
     where
        authenticates = (username req, key req) == (credentials config)

pdfAct :: PdfRequest -> Snap ()
pdfAct req = do 
    let url = src req
    modifyResponse $ setContentType "application/pdf"
    file <- liftIO (pdf url)
    sendFile file

pdf :: String -> IO (String)
pdf url = do 
    devNull <- openFile "/dev/null" AppendMode
    randomFilename <- (++".pdf") <$> show <$> nextRandom 
    let commandLine = words ("xvfb-run wkhtmltopdf --page-size A4 " ++ url ++ " " ++ randomFilename)
    putStrLn (show commandLine)
    --(_, _, _, pHandle) <- createProcess (proc (head commandLine) (tail commandLine)){ std_err = (UseHandle devNull)  }
    (_, _, _, pHandle) <- createProcess (proc (head commandLine) (tail commandLine)){ std_err = Inherit } 
    exitCode <- waitForProcess pHandle
    hClose devNull
    return randomFilename

