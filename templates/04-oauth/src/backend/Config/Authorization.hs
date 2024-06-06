{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Config.Authorization
Description :  Configuration of the Authorization module
-}
module Config.Authorization where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Options.Applicative (Parser, long, short, metavar, help, strOption, some)
import System.Environment (getEnv)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16

data ConfigAuthorization = ConfigAuthorization
  { cfgAuthorizationCookie :: ByteString
  }
  deriving (Eq, Show)

pConfigAuthorization :: Parser ConfigAuthorization
pConfigAuthorization = ConfigAuthorization
  <$> strOption ( long "auth-cookie"
                  <> metavar "AUTHORIZATION_COOKIE"
                  <> help "Cookies containing Authorization token"
                )
