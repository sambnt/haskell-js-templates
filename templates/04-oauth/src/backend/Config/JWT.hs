{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config.JWT where

import Options.Applicative (Parser, help, metavar, strOption, long)
import Data.Text (Text)

data ConfigJWT = ConfigJWT
  { cfgIssuer :: Text,
    cfgJWKSUri :: Text
  }
  deriving (Eq, Show)

pConfigJWT :: Parser ConfigJWT
pConfigJWT = ConfigJWT
  <$> strOption ( long "jwt-issuer"
                  <> metavar "URL"
                  <> help "URL of JWT issuer"
                )
  <*> strOption ( long "jwks-url"
                  <> metavar "URL"
                  <> help "URL to retrieve JWKS from"
                )
