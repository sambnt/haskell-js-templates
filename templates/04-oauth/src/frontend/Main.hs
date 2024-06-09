-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import           Miso
import           Miso.String
import           Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromMaybe)
import Data.Either.Combinators (mapLeft)
import Data.ByteString (ByteString)
import qualified JavaScript.Web.XMLHttpRequest as XHR
import GHC.Generics (Generic)
import GHCJS.Types (JSVal)
import Language.Javascript.JSaddle ((#), jsg, (!))

-- | JSAddle import
#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets
#endif
import           Control.Monad.IO.Class
import qualified Control.Concurrent as C
import Network.URI (URI)

import Servant.Client.JSaddle
import Servant.Client.Core (AuthenticatedRequest, mkAuthenticatedRequest)
import qualified GHCJS.DOM.XMLHttpRequest          as JS
import Api (AuthAccess, SecuredApi)
import qualified Api as Api
import qualified GHCJS.DOM.Types as DOM

-- | TODO: Load config
oauthBaseUrl = "http://localhost:8082"
apiBaseUrl = "http://localhost:8081"

-- | Type synonym for an application model
data AuthenticationStatus = Init
                          | Authenticated
                          | NotAuthenticated
                          | AuthenticationError MisoString
  deriving (Eq, Show)

data Model = Model { currentURI :: URI
                   , authStatus :: AuthenticationStatus
                   , privateInfo :: Maybe MisoString
                   , counter :: Int
                   }
  deriving (Eq, Show)

-- | Sum type for application events
data Action
  = NoOp
  | CheckAuthentication
  | SetAuthentication (Either MisoString AuthenticateResponse)
  | StartAuthentication
  | DoAuthRedirect (Either MisoString URI)
  | GetPrivateInfo
  | SetPrivateInfo (Either MisoString MisoString)
  | HandleURI URI
  | ChangeURI URI
  | AddOne
  | SubtractOne
  deriving (Show, Eq)

#ifndef ghcjs_HOST_OS
runApp :: JSM () -> IO ()
runApp f = JSaddle.debugOr 8080 (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

foreign import javascript unsafe
  "((x) => { x.withCredentials = true; })"
  js_setWithCredentials :: JS.XMLHttpRequest -> DOM.DOM ()

-- | Entry point for a miso application
main :: IO ()
main = runApp $ do
  let (Just clientBaseUrl) = parseBaseUrl apiBaseUrl
      clientEnv = ClientEnv { baseUrl = clientBaseUrl
                            -- Ensure we set xhr.withCredentials to true, so cookies are sent
                            , fixUpXhr = js_setWithCredentials
                            }

  (Right initialCount) <- flip runClientM clientEnv $ Api.getCounter Api.client
  currentURI <- getCurrentURI

  startApp $
    App { model = Model { currentURI = currentURI
                        , authStatus = Init
                        , privateInfo = Nothing
                        , counter = initialCount
                        }
        , initialAction = CheckAuthentication -- initial action to be executed on application load
        , update = updateModel clientEnv
        , view = viewModel
        , events = defaultEvents
        , subs = [ uriSub HandleURI ]
        , mountPoint = Nothing
        , logLevel = Off
        }

-- | Updates model, optionally introduces side effects
updateModel :: ClientEnv -> Action -> Model -> Effect Action Model
updateModel _ CheckAuthentication m = m <# do
  SetAuthentication <$> getAuthState (currentURI m)
updateModel _ (SetAuthentication result) m =
  case result of
    Left err ->
      (m { authStatus = AuthenticationError err }) <# do
        pure $ ChangeURI ((currentURI m) { uriQuery = ""} )
    Right r  ->
      if not (isLoggedIn r)
      then do
        (m { authStatus = NotAuthenticated }) <# do
          pure $ ChangeURI ((currentURI m) { uriQuery = ""} )
      else
        (m { authStatus = Authenticated }) <# do
          pure $ ChangeURI ((currentURI m) { uriQuery = ""} )
updateModel _ (HandleURI u) m = noEff $ m { currentURI = u }
updateModel _ (ChangeURI u) m = m <# do
  pushURI u
  pure NoOp
updateModel _ StartAuthentication m = m <# do
  (DoAuthRedirect . fmap authorizationRequestUrl) <$> startAuth
updateModel _ (DoAuthRedirect result) m =
  case result of
    Left err -> noEff $ m { authStatus = AuthenticationError err }
    Right u  -> m <# do
      assignURI (toMisoString $ show u)
      pure NoOp
updateModel _ GetPrivateInfo m = m <# do
  SetPrivateInfo <$> getPrivateInfo
updateModel _ (SetPrivateInfo result) m =
  case result of
    Left err -> noEff $ m { privateInfo = Just err }
    Right info -> noEff $ m { privateInfo = Just info }
updateModel clientEnv AddOne m = (m { counter = (counter m + 1) }) <# do
  flip runClientM clientEnv $ Api.setCounter secureBrowserClient $ (counter m + 1)
  pure NoOp
updateModel clientEnv SubtractOne m = (m { counter = (counter m - 1) }) <# do
  flip runClientM clientEnv $ Api.setCounter secureBrowserClient $ (counter m - 1)
  pure NoOp
updateModel _ NoOp m = noEff m

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m =
  case authStatus m of
    Init                    -> div_ [] [ text "You are being authenticated" ]
    Authenticated           ->
      div_ []
        ([ text "You are authenticated"
        , button_ [ onClick GetPrivateInfo ] [ text "Get" ]
        ] <> maybe [] (\i -> [ text i ]) (privateInfo m)
        <> [ button_ [ onClick AddOne ] [ text "+" ]
           , text (ms $ counter m)
           , button_ [ onClick SubtractOne ] [ text "-" ]
           ]
        )
    NotAuthenticated        ->
      div_ []
        [ text "You are NOT authenticated"
        , text $ "The current count is: " <> ms (counter m)
        , button_ [ onClick StartAuthentication ] [ text "Sign In" ]
        ]
    (AuthenticationError e) -> div_ [] [ text $ "There was an error: '" <> e <> "'." ]

-- foreign import javascript unsafe
--   "((x) => { x.withCredentials = true; })"
--   js_setWithCredentials :: JS.XMLHttpRequest -> DOM.DOM ()

getAuthState :: URI -> IO (Either MisoString AuthenticateResponse)
getAuthState pageURI = do
  let
    req = XHR.Request { reqMethod = XHR.POST
                      , reqURI = pack $ oauthBaseUrl <> "/oauth-agent/login/end"
                      , reqLogin = Nothing
                      , reqHeaders = [("content-type", "application/json")]
                      , reqWithCredentials = True
                      , reqData = XHR.StringData $
                          pack $ "{ \"pageUrl\": \"" <> show pageURI <> "\" }"
                      }
  result <- mkXHRRequestJSON req
  case result of
    Left err -> pure $ Left $ "OAuth agent returned error: '" <> err <> "'."
    Right resp -> pure $ Right resp

startAuth :: IO (Either MisoString AuthRedirect)
startAuth = do
  let
    req = XHR.Request { reqMethod = XHR.POST
                      , reqURI = pack $ oauthBaseUrl <> "/oauth-agent/login/start"
                      , reqLogin = Nothing
                      , reqHeaders = []
                      , reqWithCredentials = True
                      , reqData = XHR.NoData
                      }

  result <- mkXHRRequestJSON req
  case result of
    Left err -> pure $ Left $ "OAuth agent returned error: '" <> err <> "'."
    Right resp -> pure $ Right resp

getPrivateInfo :: IO (Either MisoString MisoString)
getPrivateInfo = do
  let
    req = XHR.Request { reqMethod = XHR.GET
                      , reqURI = pack $ apiBaseUrl <> "/"
                      , reqLogin = Nothing
                      , reqHeaders = []
                      , reqWithCredentials = True
                      , reqData = XHR.NoData
                      }

  result <- mkXHRRequest req
  case result of
    Left err -> pure $ Left $ "API returned error: '" <> err <> "'."
    Right resp -> pure $ Right (toMisoString resp)

data AuthRedirect =
  -- TODO: authorization -> authentication
  AuthRedirect { authorizationRequestUrl :: URI
               }
  deriving (Eq, Show, Generic)

instance FromJSON AuthRedirect where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

data AuthenticateResponse
  = AuthenticateResponse { handled :: Bool
                         , isLoggedIn :: Bool
                         , csrf :: Maybe MisoString
                         }
  deriving (Eq, Show, Generic)

instance FromJSON AuthenticateResponse where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

mkXHRRequestJSON :: FromJSON a => XHR.Request -> IO (Either MisoString a)
mkXHRRequestJSON req = do
  result <- mkXHRRequest req
  pure $ case result of
    Left err ->
      Left err
    Right bytes ->
      mapLeft toMisoString $ Aeson.eitherDecodeStrict bytes

mkXHRRequest :: XHR.Request -> IO (Either MisoString ByteString)
mkXHRRequest req = do
  resp <- XHR.xhrByteString req
  let statusCode = XHR.status resp
      -- TODO: 400 Should be treated as auth failure, not error message worthy
      statusFailure = statusCode < 200 || statusCode > 299

  if statusFailure
  then do
    let failureMsg =
          case XHR.contents resp of
            Nothing  -> "no error message was returned."
            Just msg -> "error message was: '" <> toMisoString msg <> "'."
    pure $ Left $
      "XHR request returned response code "
        <> toMisoString statusCode <> " " <> failureMsg
  else
    pure $ Right $ fromMaybe "" $ XHR.contents resp

getLocation :: JSM JSVal
getLocation = jsg ("window" :: String) ! ("location" :: String)

assignURI :: MisoString -> JSM ()
assignURI uri = do
  _ <- getLocation # ("assign" :: String) $ [uri]
  pure ()


addNoAuth :: AuthenticatedRequest AuthAccess
addNoAuth = mkAuthenticatedRequest undefined (const id)

-- | The browser will send the auth cookie, we don't need to
secureBrowserClient :: SecuredApi (AsClientT ClientM)
secureBrowserClient =
  Api.secured Api.client addNoAuth
