{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Config.CSRF
Description :  Configuration of the CSRF protection middleware
-}
module Config.CSRF where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Options.Applicative (Parser, long, short, metavar, help, strOption, some)
import System.Environment (getEnv)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16

data ConfigCSRF = ConfigCSRF
  { cfgCSRFCookie :: ByteString
  , cfgCSRFHeader :: ByteString
  }
  deriving (Eq, Show)

pConfigCSRF :: Parser ConfigCSRF
pConfigCSRF = ConfigCSRF
  <$> strOption ( long "csrf-cookie"
                  <> metavar "CSRF_COOKIE"
                  <> help "Cookies containing CSRF token"
                )
  <*> strOption ( long "csrf-header"
                  <> metavar "CSRF_HEADER"
                  <> help "Header containing CSRF token"
                )
