{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Config.HMAC
Description :  Configuration of the HMAC verification middleware
-}
module Config.HMAC where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Options.Applicative (Parser, long, short, metavar, help, strOption, some)
import System.Environment (getEnv)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16

type CookieName = Text
type HMACSecret = ByteString

data ConfigHMAC = ConfigHMAC
  { cfgHMACCookies   :: [CookieName]
  , cfgHMACSecretKey :: HMACSecret
  }
  deriving (Eq, Show)

-- | Parse the list of cookie names to verify from the command line. Do not
-- parse the secret key from the command line. That will be passed in via an
-- environment variable.
pConfigHMAC :: HMACSecret -> Parser ConfigHMAC
pConfigHMAC secret = ConfigHMAC
  <$> some (strOption ( long "hmac-cookie"
                        <> short 'c'
                        <> metavar "COOKIE_NAME"
                        <> help "Cookies to HMAC verify"
                      ))
  <*> pure secret

envConfigHMAC :: MonadIO m => m HMACSecret
envConfigHMAC = liftIO $ do
  keyHex <- T.pack <$> getEnv "HMAC_KEY"
  case Base16.decode (T.encodeUtf8 keyHex) of
    Left err ->
      error
        "HMAC: Failed to decode HMAC secret key, is it in Base16 (hex) encoding?"
    Right keyBytes ->
      pure keyBytes
