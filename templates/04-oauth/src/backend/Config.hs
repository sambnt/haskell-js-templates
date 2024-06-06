{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Config
-- Description :  Backend API server configuration.
--
-- Middleware 1: HMAC verify
--   * Given list of cookies
--   * HMAC verify all cookies and pass them on, unwrapped.
-- Middleware 2: CSRF
--   * If unsafe request, look for 'csrf_header', and compare with 'csrf_cookie'.
--     * Only allow request if they are equal.
-- Middleware 3: Cookie -> Authorization Header
--   * Given 'access_cookie_name' cookie, remove cookie from request and place it
--     in the Authorization header.
--
-- The backend is configured with the following options:
--   * 'access_cookie' - Name of cookie containing JWT, to verify HMAC before use.
--   * 'decrypt_key' - Key to use to verify HMAC of cookies, base16 encoded (hex).
--   * 'csrf_cookie' - Name of CSRF cookie.
--   * 'csrf_header' - Header to look for when processing unsafe requests such as
-- POST and DELETE.
--   * 'jwks_url' - URL to use to verify signature of provided JWTs.
--   *
module Config where

import Config.HMAC (ConfigHMAC, envConfigHMAC, pConfigHMAC)
import Options.Applicative (Parser, execParser, fullDesc, header, helper, info, progDesc, (<**>))

data Config = Config
  { cfgHMAC :: ConfigHMAC
  }
  deriving (Eq, Show)

parseConfig :: IO Config
parseConfig = do
  hmacSecret <- envConfigHMAC
  let pFinal :: Parser Config
      pFinal = Config <$> pConfigHMAC hmacSecret
      opts =
        info
          (pFinal <**> helper)
          ( fullDesc
              <> progDesc "Backend API"
              <> header "A backend API for an SPA"
          )
  execParser opts
