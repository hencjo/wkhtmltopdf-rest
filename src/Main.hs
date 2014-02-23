{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class

import Snap.Core
import Snap.Http.Server

import Data.ByteString.Char8(unpack, concat, ByteString)

import Data.Either

import System.Process
import System.IO
import Data.UUID.V4(nextRandom)

-- Requires wkhtmltopdf, xvfb to be installed.

-- TODO
-- [ ] Remove temporary files.
-- [ ] Actually do Authentication
-- [ ] Remove or implement width, height.

main :: IO ()
main = quickHttpServe pdfHandler

maybeToEither :: a1 -> Maybe a -> Either a1 a
maybeToEither = flip maybe Right . Left

missing :: Request -> ByteString -> Either ByteString ByteString
missing request param = case values of 
                          (Just (v:_)) -> Right v
                          _            -> Left (Data.ByteString.Char8.concat ("Missing parameter \"" : param : "\"" : []))
    where
      values :: Maybe [ByteString]
      values = rqPostParam param request 

data PdfRequest = PdfRequest {
  username :: String,
  key :: String,
  src :: String
} deriving (Show)

requestify :: Either ByteString ByteString -> Either ByteString ByteString -> Either ByteString ByteString -> PdfRequest
requestify username key src = do
  let username' = unpack (r username)
  let key' = unpack (r key)
  let src' = unpack (r src)
  PdfRequest username' key' src'
    where 
      r :: Either a b -> b
      r (Right x) = x

pdfRequest :: Request -> Either [ByteString] PdfRequest
pdfRequest request
    | errors == [] = Right (requestify username key src)
    | otherwise    = Left errors
    where 
      username = missing request "username"
      key      = missing request "key"
      width    = missing request "width"
      height   = missing request "height"
      src      = missing request "src"
      errors   = lefts [username, key, width, height, src] 


pdfHandler :: Snap ()
pdfHandler = (getsRequest pdfRequest) >>= (either (writeBS . Data.ByteString.Char8.concat) pdfAct)
    
pdfAct :: PdfRequest -> Snap ()
pdfAct req = do 
    let url = src req
    modifyResponse $ setContentType "application/pdf"
    file <- liftIO (pdf url)
    sendFile file

pdf :: String -> IO (String)
pdf url = do 
    devNull <- openFile "/dev/null" AppendMode
    randomUUID <- nextRandom
    let randomFilename = (show randomUUID) ++ ".pdf"
    let commandLine = words ("xvfb-run wkhtmltopdf --page-size A4 " ++ url ++ " " ++ randomFilename)
    putStrLn (show commandLine)
    --(_, _, _, pHandle) <- createProcess (proc (head commandLine) (tail commandLine)){ std_err = (UseHandle devNull)  }
    (_, _, _, pHandle) <- createProcess (proc (head commandLine) (tail commandLine)){ std_err = Inherit } 
    exitCode <- waitForProcess pHandle
    hClose devNull
    return randomFilename

