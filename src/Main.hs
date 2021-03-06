{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core
import Snap.Http.Server
import Control.Monad.IO.Class
import Control.Applicative
import Data.ConfigFile
import Data.Either.Utils
import Data.Maybe(listToMaybe)
import qualified Data.Text as T
import Data.Text.Encoding(encodeUtf8, decodeUtf8)
import Network.URI (URI, parseURI, uriScheme)
import System.Environment(getArgs)
import System.Process
import System.IO hiding (readFile)
import Safe(headMay,readMay)
import System.IO.Temp(withSystemTempFile)
import qualified Data.ByteString as ByteString

import Validation

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

-- Done:
-- -Wall, -Werror
-- =<<, >=> och överdrivna <$>, $ mm förenklade till . $ och >>=
-- PdfHandler -> ResponseHandler med Snap i do-notation + Either som "vad det nu kallas"-notation.
-- seq i config - hjälp?!

newtype Username = Username T.Text deriving (Eq, Show)
newtype ApiKey = ApiKey T.Text deriving (Eq, Show)
newtype SrcUrl = SrcUrl T.Text deriving (Show)
newtype Port = Port Int deriving (Show, Eq)

data PageSize = A4 | Letter deriving (Show, Read)
data Credentials = Credentials Username ApiKey deriving (Show, Eq)

data PdfConfig = PdfConfig {
  configCredentials :: Credentials,
  configPort :: Port
} deriving (Show, Eq)

data PdfRequest = PdfRequest {
  requestUsername :: Username,
  requestKey :: ApiKey,
  requestSrc :: SrcUrl,
  requestPageSize :: PageSize
} deriving (Show)

main :: IO ()
main = do
    a <- getArgs
    let configFile = case a of 
                        f:_ -> f
                        _   -> error "Expected path to config file as argument."
    putStrLn ("Reading configuration from " ++ configFile)
    c <- config configFile
    c == c `seq` httpServe (setPort (port2 c) emptyConfig) (responseHandler c)
        where 
            port2 :: PdfConfig -> Int
            port2 pdfConfig = case (configPort pdfConfig) of
                                 (Port p) -> p
    
config :: FilePath -> IO PdfConfig
config filePath = do
    val <- readfile emptyCP filePath
    let cp = forceEither val
    return (PdfConfig 
        (Credentials 
            (Username . T.pack . forceEither $ get cp "DEFAULT" "api.user")
            (ApiKey . T.pack . forceEither $ get cp "DEFAULT" "api.key")
        )
        (Port (forceEither $ (get cp "DEFAULT" "web.port")::Int)))

m :: String -> Maybe a -> Validation a
m _ (Just a) = Valid a
m p _        = Invalid p

missing :: String -> Maybe a -> Validation a
missing msg = m (concat ["Missing parameter \"", msg, "\""])

postParam :: Request -> T.Text -> Maybe T.Text
postParam request param = decodeUtf8 <$> ((rqPostParam . encodeUtf8) param request >>= headMay)

validURI :: T.Text -> Maybe T.Text
validURI url = T.pack . show <$> ((parseURI . T.unpack) url >>= meow)
    where
        meow :: URI -> Maybe URI
        meow uri = listToMaybe (filter httpOrHttps [uri])
        httpOrHttps :: URI -> Bool 
        httpOrHttps uri = (scheme == "http:" || scheme == "https:")
            where 
                scheme = (Network.URI.uriScheme uri)

pdfRequest :: Request -> AccumulatedValidation PdfRequest
pdfRequest request = (Success PdfRequest) <#> username <#> key <#> src <#> pageSize
  where
      post     = postParam request
      username = Username <$> (missing "username" $ post "username")
      key      = ApiKey <$> (missing "key" $ post "key")
      src      = SrcUrl <$> (missing "src" $ post "src" >>= validURI)
--      pageSize = missing "page-size" (T.unpack <$> post "page-size" >>= (\s -> (readMay s)::(Maybe PageSize)))
      pageSize = missing "page-size" $ do
          res <- post "page-size"
          ps <- (readMay $ T.unpack res) :: Maybe PageSize
          return (ps :: PageSize)

-- I want to be able to write:
--     case ((pdfRequest request) >>= (auth pc)) of
--        Failure messages -> (writeText . T.pack . concat) messages
--        Success request  -> pdfHandler request
--
-- Can this be done automatically for me?
-- Also, I'm noticing that the structure really is:
-- do
--  b <- f a
--  c <- g a
--  h b c a     -- <-- Hang on! Isn't that very Applicative?
--
-- There's not really a dependency between f and g.
-- f and g are pdfRequest and (auth pc)
--
b :: AccumulatedValidation a -> (a -> Either [T.Text] a) -> Either [T.Text] a
(Success a) `b`  f = f a
(Failure xs) `b` _ = Left (map T.pack xs)

responseHandler :: PdfConfig -> Snap ()
responseHandler pc = do
    request <- getRequest
    case ((pdfRequest request) `b` (auth pc)) of
         (Left messages) -> (writeText . T.concat) messages
         (Right r)       -> pdfHandler r

auth :: PdfConfig -> PdfRequest -> Either [T.Text] PdfRequest
auth pc req
     | authenticates = (Right req)
     | otherwise     = (Left ["Authorisation failed"])
     where
        authenticates = Credentials (requestUsername req) (requestKey req) == (configCredentials pc)

callback :: SrcUrl -> PageSize -> FilePath -> Handle -> IO (ByteString.ByteString)
callback (SrcUrl url) pageSize tempFile tempHandle = do
    hClose tempHandle
    devNull <- openFile "/dev/null" AppendMode
    let commandLine = ["xvfb-run","wkhtmltopdf","--quiet","--page-size",(show pageSize),(T.unpack url),tempFile] -- dangerous
    putStrLn (unwords commandLine)
    (_, _, _, pHandle) <- createProcess (proc (head commandLine) (tail commandLine)){ std_err = Inherit } -- dangerous
    waitForProcess pHandle
    hClose devNull
    ByteString.readFile tempFile

pdfHandler :: PdfRequest -> Snap ()
pdfHandler req = do
    bs <- liftIO (withSystemTempFile "wkhtmltopdf-rest.pdf" (callback (requestSrc req) (requestPageSize req)))
    modifyResponse $ setContentType "application/pdf"
    writeBS bs
