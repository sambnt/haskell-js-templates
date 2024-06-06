{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Text (Text)
import Servant.API
import Servant
import Data.Maybe (fromJust)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), simpleCorsResourcePolicy, simpleMethods, simpleHeaders, cors)
import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import URI.ByteString (serializeURIRef')
import Data.Foldable (find)
import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad.IO.Class (liftIO)
import Servant.Server.Generic (genericServeTWithContext)
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as ByteString
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString.Base64 as ByteStringBase64
import qualified Data.Set as Set
import Network.Wai (Request, requestHeaders, requestMethod)
import Servant.Server.Experimental.Auth (AuthServerData, AuthHandler(AuthHandler))
import Servant.Server.Generic (AsServerT)
import Control.Monad (guard, join)
import Control.Arrow ((>>>), second)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow, try, throwM)
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Exception (throw)
import Control.Monad.Identity (runIdentityT)
import Lens.Micro ((.~), (%~))
import Data.Function ((&))
import Web.Cookie (SetCookie, defaultSetCookie, setCookieName, setCookieValue, setCookieHttpOnly, setCookieSameSite, setCookieMaxAge, sameSiteLax, sameSiteStrict, renderSetCookieBS, parseCookies, Cookies)
import qualified Network.OAuth.OAuth2 as OAuth
import qualified Network.OAuth2.Experiment as OAuth
import qualified Network.OAuth2.Experiment.Pkce as OAuth
import qualified Network.OAuth2.Experiment.Grants.AuthorizationCode as OAuth
import qualified Network.OAuth2.Experiment.Types as OAuth
import qualified System.Entropy as Crypto
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Client (Manager, managerModifyRequest)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Crypto.JOSE.JWK as JOSE
import qualified Crypto.JWT as JOSE

import Common

data Database = Database
  { dbCounter :: TVar Int
  , dbJWKSCache :: TVar JOSE.JWKSet
  }

verifyJWT :: (Aeson.FromJSON payload, JOSE.HasClaimsSet payload, MonadIO m) => Database -> JOSE.SignedJWT -> m payload
verifyJWT db signedJWT = liftIO $ do
  jwks <- STM.readTVarIO $ dbJWKSCache db
  result <- runExceptT $ JOSE.verifyJWT (JOSE.defaultJWTValidationSettings (== "https://cognito-idp.ap-southeast-2.amazonaws.com/ap-southeast-2_wmCNNCByv")) jwks signedJWT
  case result of
    (Left (_err :: JOSE.JWTError)) -> do
      -- Get header from signedJWT
      -- check if kid present in JWKS
      -- If not, refresh JWKS
      -- https://cognito-idp.ap-southeast-2.amazonaws.com/ap-southeast-2_wmCNNCByv/.well-known/jwks.json
      verifyJWT db signedJWT
    (Right payload) ->
      pure payload

appToHandler :: App a -> Handler a
appToHandler (App app) = (Handler . ExceptT . try . runIdentityT) app

getCounterHandler :: (MonadThrow m, MonadIO m) => Database -> m Int
getCounterHandler =
  liftIO . STM.readTVarIO . dbCounter

setCounterHandler :: (MonadIO m) => Database -> () -> Int -> m ()
setCounterHandler db _authData =
  liftIO . STM.atomically . STM.writeTVar (dbCounter db)

api :: Database -> Manager -> Api (AsServerT App)
api db manager = Api { getCounter = getCounterHandler db
                     , secured = securedHandlers db
                     }

securedHandlers :: Database -> Maybe () -> SecuredApi (AsServerT App)
securedHandlers db (Just authData) =
  SecuredApi { setCounter = setCounterHandler db authData
             }
securedHandlers _ _ =
  throw err401

mkAuthHandler :: (req -> Handler usr) -> AuthHandler req usr
mkAuthHandler = AuthHandler

authHandler :: Database -> AuthHandler Request (Maybe ())
authHandler db = mkAuthHandler $ \req -> do
  case getCookies req of
    Nothing   -> pure Nothing
    (Just cookies) -> do
      let
        safeMethods =
          [ HTTP.methodGet
          , HTTP.methodTrace
          , HTTP.methodHead
          , HTTP.methodOptions
          ]
        unsafeRequest = requestMethod req `notElem` safeMethods
      -- csrfResult <- when unsafeRequest $ do
      --   (Just csrfCookie) <- pure $ findCookie "example-csrf" cookies
      --   (Just csrfHeader) <- pure $ findHeader "X-example-csrf" (requestHeaders req)
      --   if (decrypt csrfCookie == csrfHeader)
      --   then pure $ Just ()
      --   else pure Nothing
      -- guard (csrfResult == Just ())
      (Just accessTokenCookie) <- pure $ findCookie "example-at" cookies
      case JOSE.decodeCompact $ ByteStringLazy.fromStrict accessTokenCookie of
        (Left (err :: JOSE.Error)) -> pure Nothing
        (Right signedJWT) -> do
          (_payload :: JOSE.ClaimsSet) <- verifyJWT db signedJWT
          pure $ Just ()

getCookies :: Request -> Maybe Cookies
getCookies req =
  parseCookies <$> lookup "Cookie" (requestHeaders req)

findCookie :: ByteString -> Cookies -> Maybe ByteString
findCookie n = fmap fst . find (\(name, _) -> name == n)

-- getToken :: Request -> Maybe AccessToken
-- getToken req = do
--   (scheme, token) <- split <$> lookup "Authorization" (requestHeaders req)
--   guard (scheme == "Bearer")
--   pure $ AccessToken token
--   where
--     split = ByteString.break (== ' ') >>> second (ByteString.drop 1)

-- | A simple, unsafe verification scheme where we succeed if we are able to
-- decode an "AuthUser" from the JWT payload.
-- verifyToken :: AccessToken -> IO (Maybe AuthUser)
-- verifyToken (AccessToken token) = do
--   -- WARNING: None of this is safe. Use "verifyJWS" from the jose package
--   -- in production.
--   let
--     split = ByteString.break (== '.') >>> second (ByteString.drop 1)
--     (hdr, (payload, sig)) =
--       split >>> second split $ token

--   case Aeson.eitherDecodeStrict $ ByteStringBase64.decodeLenient payload of
--     Left err -> error $ err
--     Right r  -> pure $ Just r

main :: IO ()
main = do
  counter <- STM.newTVarIO 0
  jwksCache <- STM.newTVarIO (JOSE.JWKSet [])

  manager <- newTlsManagerWith $ tlsManagerSettings
    -- tlsManagerSettings { managerModifyRequest = (\r -> do
    --                            print r
    --                            pure r
    --                        )
    --                    }

  let
    db = Database counter jwksCache
    ctx =
      authHandler db -- @AuthUser
      :. EmptyContext

    corsPolicy = simpleCorsResourcePolicy {
      -- Allow Credentials from this origin, we can't use *
      corsOrigins = Just (["http://localhost:8000"], True),
      corsMethods = simpleMethods,
      corsRequestHeaders = simpleHeaders <> ["Content-Type", "Authorization"]
    }
  run 8081
    $ logRequestHeaders
    $ cors (const $ Just corsPolicy)
    $ genericServeTWithContext appToHandler (api db manager) ctx

logRequestHeaders :: Application -> Application
logRequestHeaders incoming request outgoing = do
   let headerList = requestHeaders request
   liftIO $ mapM_ print headerList
   incoming request outgoing


-- x :: Key -> Application -> Application
-- x req respond = do
--    -- If POST, take CSRF from request body
--    let headerList = requestHeaders request
--    liftIO $ mapM_ print headerList
--    incoming request outgoing
