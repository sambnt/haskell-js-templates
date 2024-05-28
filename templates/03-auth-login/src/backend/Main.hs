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
import Network.Wai (Request, requestHeaders)
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
import Web.Cookie (SetCookie, defaultSetCookie, setCookieName, setCookieValue, setCookieHttpOnly, setCookieSameSite, setCookieMaxAge, sameSiteLax, renderSetCookieBS, parseCookies)
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

import Common

appToHandler :: App a -> Handler a
appToHandler (App app) = (Handler . ExceptT . try . runIdentityT) app

type instance AuthServerData AuthAccess = Maybe AuthUser

getCounterHandler :: (MonadThrow m, MonadIO m) => Database -> Maybe Text -> m Int
getCounterHandler db mHdr = do
  liftIO $ putStrLn $ show (getAuthCookie . T.encodeUtf8 =<< mHdr)
  liftIO . STM.readTVarIO . dbCounter $ db

setCounterHandler :: (MonadIO m) => Database -> AuthUser -> Int -> m ()
setCounterHandler db _ = liftIO . STM.atomically . STM.writeTVar (dbCounter db)

-- validateAuthHeader :: Database -> Maybe Text -> m Bool
-- validateAuthHeader _   Nothing   = False
-- validateAuthHeader db (Just hdr) =

oauthApp :: Text -> OAuth.AuthorizationCodeApplication
oauthApp nonce =
  OAuth.AuthorizationCodeApplication { OAuth.acName = "bnt-test"
                                     , OAuth.acClientId = "4pjg54s9u61e3eb6emeong1jsk"
                                     , OAuth.acClientSecret = ""
                                     , OAuth.acScope = mempty -- Set.fromList ["openid", "profile", "email"]
                                     , OAuth.acRedirectUri = parseURI' "http://localhost:8081/login"
                                     , OAuth.acAuthorizeState = OAuth.AuthorizeState $ TL.fromStrict nonce
                                     , OAuth.acAuthorizeRequestExtraParams = mempty
                                     , OAuth.acTokenRequestAuthenticationMethod = OAuth.ClientSecretBasic
                                     }
cognitoIdp :: OAuth.Idp Cognito
cognitoIdp = OAuth.Idp { OAuth.idpUserInfoEndpoint = parseURI' "https://bnt-test.auth.ap-southeast-2.amazoncognito.com/oauth2/userInfo"
                       , OAuth.idpAuthorizeEndpoint = parseURI' "https://bnt-test.auth.ap-southeast-2.amazoncognito.com/oauth2/authorize"
                       , OAuth.idpTokenEndpoint = parseURI' "https://bnt-test.auth.ap-southeast-2.amazoncognito.com/oauth2/token"
                       , OAuth.idpDeviceAuthorizationEndpoint = Nothing
                       }

fooIdpApp :: Text -> OAuth.IdpApplication Cognito OAuth.AuthorizationCodeApplication
fooIdpApp nonce = OAuth.IdpApplication { OAuth.idp = cognitoIdp
                                       , OAuth.application = oauthApp nonce
                                       }

getAuthCookie :: ByteString -> Maybe ByteString
getAuthCookie = fmap snd . find (\(name, _) -> name == "X-Auth") . parseCookies

-- loginHandler :: (MonadThrow m, MonadIO m) => Database -> Manager -> Maybe Text -> Maybe Text -> Maybe Text -> m (Headers '[Header "Set-Cookie" SetCookie] OAuth.OAuth2Token)
loginHandler :: (MonadThrow m, MonadIO m) => Database -> Manager -> Maybe Text -> Maybe Text -> Maybe Text -> m OAuth.OAuth2Token
loginHandler db manager mAuthHdr mCode mNonce = do
  -- liftIO $ putStrLn $ show mAuthHdr
  let mAuthCookie = fmap T.decodeUtf8 . getAuthCookie . T.encodeUtf8 =<< mAuthHdr
  -- liftIO $ putStrLn $ "state " <> show mAuthCookie <> " " <> show mCode <> " " <> show mNonce

  st <- liftIO $ STM.readTVarIO (dbAuth db)
  -- liftIO $ print st

  state <- case (mAuthCookie, mNonce) of
    (_, Just nonce) -> do
      x <- liftIO $ getAuthState db (Nonce nonce)
      if x == Nothing then error ("nonce '" <> show nonce <> "' failed") else pure x
    (Just cookie, Nothing) -> do
      x <- liftIO $ getAuthState db (Cookie cookie)
      -- if x == Nothing then error ("cookie '" <> T.unpack cookie <> "' failed") else pure x
      pure x
    (Nothing, Nothing) ->
      pure Nothing

  -- liftIO $ putStrLn $ " got state: " <> show state

  case state of
    Nothing -> do
      -- Generate opaque code
      opaqueCode <- liftIO $ T.decodeUtf8 . ByteStringBase64.encode <$> Crypto.getEntropy 64
      -- Generate nonce
      nonce <- liftIO $ T.decodeUtf8 . ByteStringBase64.encode <$> Crypto.getEntropy 16
      -- Set state to "InFlight"
      let newState = InFlight opaqueCode
      -- liftIO $ print $ "Writing cookie: " <> T.unpack opaqueCode
      -- liftIO $ print $ "Writing nonce: " <> show nonce
      liftIO $ writeAuthState db (Nonce nonce) newState
      -- Generate redirect URL
      let
        authorizeUrl = OAuth.mkAuthorizationRequest (fooIdpApp nonce)
      throwM $ err302 { errHeaders = [("Location", serializeURIRef' authorizeUrl)] }
    Just (InFlight opaqueCode) -> do
      -- liftIO $ putStrLn "in flight!"
      -- case mNonce of
      --   Nothing ->
      --     error "Auth provider didn't provide state"
      --   Just nonce' ->
      --     if nonce /= nonce'
      --     then error "Auth provider state didn't match our state"
      --     else
      case mCode of
        Nothing ->
          error "Auth provider didn't provide code"
        (Just code) -> do
          -- let
            -- tokenInfo = OAuth.AuthorizationCodeTokenRequest
            --   { OAuth.trCode = OAuth.ExchangeToken code
            --   , OAuth.trGrantType = OAuth.GTAuthorizationCode
            --   , OAuth.trRedirectUri = OAuth.RedirectUri $ parseURI' "http://localhost:8081/login"
            --   }
            -- tokenInfo = OAuth.ExchangeToken code

          -- curl -XPOST "https://bnt-test.auth.ap-southeast-2.amazoncognito.com/oauth2/token?grant_type=authorization_code&client_id=4pjg54s9u61e3eb6emeong1jsk&code=faac7f81-b515-44cc-9dd4-9090f769cb7b&redirect_uri=http%3A%2F%2Flocalhost%3A8081%2Flogin" -H "Content-Type: application/x-www-form-urlencoded" -v | jq .

          initialRequest <- HTTP.parseRequest "https://bnt-test.auth.ap-southeast-2.amazoncognito.com/oauth2/token"
          let request = initialRequest { HTTP.method = "POST"
                                       , HTTP.requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
                                       }
                        & HTTP.setQueryString [ ("grant_type", Just "authorization_code")
                                              , ("client_id", Just "4pjg54s9u61e3eb6emeong1jsk")
                                              , ("code", Just $ T.encodeUtf8 code)
                                              , ("redirect_uri", Just "http://localhost:8081/login")
                                              ]
          resp <- liftIO $ HTTP.httpLbs request manager
          case HTTP.statusCode $ HTTP.responseStatus resp of
            200 -> do
              case Aeson.eitherDecode $ HTTP.responseBody resp of
                Left e -> error $ "Error decoding token: " <> show e
                Right (token :: OAuth.OAuth2Token) -> do
                  liftIO $ writeAuthState db (Cookie opaqueCode) (Done token)
                  st <- liftIO $ STM.readTVarIO (dbAuth db)
                  -- liftIO $ print st

                  -- Set "X-Auth" header
                  let
                    authCookie =
                      defaultSetCookie
                        { setCookieName = "X-Auth"
                        , setCookieValue = T.encodeUtf8 opaqueCode
                        , setCookieHttpOnly = True
                        , setCookieSameSite = Just sameSiteLax
                        -- https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie
                        -- Indicates the number of seconds until the cookie expires. A zero or negative number will expire the cookie immediately. If both Expires and Max-Age are set, Max-Age has precedence.
                        , setCookieMaxAge = Nothing -- Just 300 -- seconds
                        }
                  throwM $ err302 { errHeaders = [("Location", "http://localhost:8081/login"), ("Set-Cookie", renderSetCookieBS authCookie)] }
            s     -> error $ "Token response was: " <> show s
    Just (Done token) -> do
      -- liftIO $ print token
      return $ token
      -- throwM $ err302 { errHeaders = [("Location", "http://localhost:8000")] }
      -- Collect code and nonce from request
      -- Compare nonce with inFlightState.nonce
      -- Send code to AWS to retrieve tokens
      -- Set state to "Completed"

api :: Database -> Manager -> Api (AsServerT App)
api db manager = Api { getCounter = getCounterHandler db
                     , login = loginHandler db manager
                     , secured = securedHandlers db
                     }

securedHandlers :: Database -> Maybe AuthUser -> SecuredApi (AsServerT App)
securedHandlers db (Just authUser) =
  SecuredApi { setCounter = setCounterHandler db authUser
             }
securedHandlers _ _ =
  throw err401

newtype AccessToken = AccessToken ByteString

mkAuthHandler :: (req -> Handler usr) -> AuthHandler req usr
mkAuthHandler = AuthHandler

authHandler :: AuthHandler Request (Maybe AuthUser)
authHandler = mkAuthHandler $ \req -> do
  case getToken req of
    Nothing    -> pure Nothing
    Just token -> liftIO $ verifyToken token

getToken :: Request -> Maybe AccessToken
getToken req = do
  (scheme, token) <- split <$> lookup "Authorization" (requestHeaders req)
  guard (scheme == "Bearer")
  pure $ AccessToken token
  where
    split = ByteString.break (== ' ') >>> second (ByteString.drop 1)

-- | A simple, unsafe verification scheme where we succeed if we are able to
-- decode an "AuthUser" from the JWT payload.
verifyToken :: AccessToken -> IO (Maybe AuthUser)
verifyToken (AccessToken token) = do
  -- WARNING: None of this is safe. Use "verifyJWS" from the jose package
  -- in production.
  let
    split = ByteString.break (== '.') >>> second (ByteString.drop 1)
    (hdr, (payload, sig)) =
      split >>> second split $ token

  case Aeson.eitherDecodeStrict $ ByteStringBase64.decodeLenient payload of
    Left err -> error $ err
    Right r  -> pure $ Just r

main :: IO ()
main = do
  counter <- STM.newTVarIO 0
  authMap <- STM.newTVarIO Map.empty

  manager <- newTlsManagerWith $ tlsManagerSettings
    -- tlsManagerSettings { managerModifyRequest = (\r -> do
    --                            print r
    --                            pure r
    --                        )
    --                    }

  let
    db = Database counter authMap
    ctx =
      authHandler -- @AuthUser
      :. EmptyContext

    corsPolicy = simpleCorsResourcePolicy{
      corsOrigins = Nothing,
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
