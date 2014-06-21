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
import Safe(readMay)

-- Requires wkhtmltopdf, xvfb to be installed.

-- TODO
-- [ ] Authenticate against config-file.
-- ============ Pushable ========
-- [ ] Remove temporary files.
-- [ ] Use PDFCrowds API so that it's easy to switch.
-- [ ] HTTP Status Codes on Errors.
-- ============ good enough ======== 
-- [ ] On startup, check that dependencies (wkhtmltopdf, xvfb) are installed.
-- [ ] Validate src, it should not be possible to escape bash!
-- [ ] Nicer error messages when PageSize is not one of the enumerated options. 

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
    return (PdfConfig ((Username "henrik@hencjo.com"), (ApiKey "RzNIKegEXLOt44WwRsx3OH5ZPZiMkKLo")))

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

pdfAct :: PdfRequest -> Snap ()
pdfAct req = do 
    let url = src req
    modifyResponse $ setContentType "application/pdf"
    file <- liftIO (pdf url (pageSize req))
    sendFile file

pdf :: SrcUrl -> PageSize -> IO (String)
pdf (SrcUrl url) pageSize = do 
    devNull <- openFile "/dev/null" AppendMode
    randomFilename <- (++".pdf") <$> show <$> nextRandom 
    let commandLine = "xvfb-run wkhtmltopdf --page-size " ++ (show pageSize) ++ " " ++ url ++ " " ++ randomFilename
    putStrLn commandLine

    let cl = words commandLine
    --(_, _, _, pHandle) <- createProcess (proc (head commandLine) (tail commandLine)){ std_err = (UseHandle devNull)  }
    (_, _, _, pHandle) <- createProcess (proc (head cl) (tail cl)){ std_err = Inherit } 
    exitCode <- waitForProcess pHandle
    hClose devNull
    return randomFilename

