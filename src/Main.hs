{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core
import Snap.Http.Server
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Data.ByteString.Char8(pack, unpack)
import Data.Either
import Data.UUID.V4(nextRandom)
import System.Process
import System.IO

-- Requires wkhtmltopdf, xvfb to be installed.

-- TODO
-- [ ] Remove temporary files.
-- [ ] Authenticate against config-file.
-- [ ] Use PDFCrowds API so that it's easy to switch.
-- ============ good enough ======== 
-- [ ] On startup, check that dependencies (wkhtmltopdf, xvfb) are installed.
-- [ ] Validate correct values for pageSize
-- [ ] Implement pageSize
-- [ ] Validate src, it should not be possible to escape bash!

-- Test it like this:
-- curl -v http://localhost:8000 -d src="https://www.google.se" -d username=username -d key=key -d page-size=A4 > meow.pdf && zathura meow.pdf 

main :: IO ()
main = quickHttpServe pdfHandler

missing :: Request -> String -> Either String String
missing request param = case (rqPostParam (pack param) request) of 
                          (Just (v:_)) -> Right (unpack v)
                          _            -> Left ("Missing parameter \"" ++ param ++ "\"")

data PdfRequest = PdfRequest {
  username :: String,
  key :: String,
  src :: String,
  pageSize :: String
} deriving (Show)

pdfRequest :: Request -> Either [String] PdfRequest
pdfRequest request = case oscar of 
                       (Right pdf)  -> Right pdf
                       _            -> Left errors
    where 
      username = missing request "username"
      key      = missing request "key"
      src      = missing request "src"
      pageSize = missing request "page-size"
      errors   = lefts [username, key, src, pageSize]
      oscar    = PdfRequest <$> username <*> key <*> src <*> pageSize

pdfHandler :: Snap ()
pdfHandler = (getsRequest (pdfRequest >=> auth)) >>= either (writeBS . pack . Prelude.concat) pdfAct

auth :: PdfRequest -> Either [String] PdfRequest
auth req
    | authenticates = (Right req)
    | otherwise     = (Left ["Authorisation failed"])
    where
        authenticates = (username req, key req) == ("henrik@hencjo.com","RzNIKegEXLOt44WwRsx3OH5ZPZiMkKLo")

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

