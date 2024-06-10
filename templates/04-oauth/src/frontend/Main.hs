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
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Either.Combinators (mapLeft)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import GHCJS.Types (JSVal)
import Language.Javascript.JSaddle ((#), jsg, (!))

-- | JSAddle import

import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets

import           Control.Monad.IO.Class
import qualified Control.Concurrent as C
import Network.URI (URI)

import Servant.Client.JSaddle
import Servant.Client.Core (AuthenticatedRequest, mkAuthenticatedRequest)
import qualified GHCJS.DOM.XMLHttpRequest          as JS
-- import qualified JavaScript.Web.XMLHttpRequest as XHR
import Api (AuthAccess, SecuredApi)
import qualified Api as Api
import qualified Api.OAuth as OAuth
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.XMLHttpRequest as XHR
import Data.Function ((&))

-- | TODO: Load config
oauthBaseUrl = "http://localhost:8082"
apiBaseUrl = "http://localhost:8081"

type CSRF = Maybe Text

-- | Type synonym for an application model
data AuthenticationStatus = Init
                          | Authenticated CSRF
                          | NotAuthenticated
                          | AuthenticationError ClientError
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
  | SetAuthentication (Either ClientError OAuth.AuthState)
  | StartAuthentication
  | DoAuthRedirect (Either ClientError URI)
  | GetPrivateInfo
  | SetPrivateInfo (Either ClientError Aeson.Value)
  | HandleURI URI
  | ChangeURI URI
  | AddOne
  | SubtractOne
  deriving (Show, Eq)

#ifndef ghcjs_HOST_OS
runApp :: JSM () -> IO ()
runApp f = JSaddle.debugOr 8000 (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

-- | Entry point for a miso application
main :: IO ()
main = runApp $ do
  let (Just clientBaseUrl) = parseBaseUrl apiBaseUrl
      clientEnv = ClientEnv { baseUrl = clientBaseUrl
                            -- Ensure we set xhr.withCredentials to true, so
                            -- cookies are sent
                            , fixUpXhr = (`XHR.setWithCredentials` True)
                            }
      (Just oauthClientBaseUrl) = parseBaseUrl oauthBaseUrl
      oauthClientEnv = ClientEnv { baseUrl = oauthClientBaseUrl
                                 -- Ensure we set xhr.withCredentials to true,
                                 -- so cookies are sent
                                 , fixUpXhr = (`XHR.setWithCredentials` True)
                                 }

  (Right initialCount) <- get clientEnv $ Api.getCounter Api.client
  currentURI <- getCurrentURI

  startApp $
    App { model = Model { currentURI = currentURI
                        , authStatus = Init
                        , privateInfo = Nothing
                        , counter = initialCount
                        }
        , initialAction = CheckAuthentication -- initial action to be executed on application load
        , update = updateModel clientEnv oauthClientEnv
        , view = viewModel
        , events = defaultEvents
        , subs = [ uriSub HandleURI ]
        , mountPoint = Nothing
        , logLevel = Off
        }

-- | Updates model, optionally introduces side effects
updateModel :: ClientEnv -> ClientEnv -> Action -> Model -> Effect Action Model
updateModel _ oauthClient CheckAuthentication m = m <# do
  SetAuthentication <$> getAuthState oauthClient (currentURI m)
updateModel _ _ (SetAuthentication result) m =
  case result of
    Left err ->
      (m { authStatus = AuthenticationError err }) <# do
        pure $ ChangeURI ((currentURI m) { uriQuery = ""} )
    Right r  ->
      if not (OAuth.isLoggedIn r)
      then do
        (m { authStatus = NotAuthenticated }) <# do
          pure $ ChangeURI ((currentURI m) { uriQuery = ""} )
      else
        (m { authStatus = Authenticated (OAuth.csrf r) }) <# do
          pure $ ChangeURI ((currentURI m) { uriQuery = ""} )
updateModel _ _ (HandleURI u) m = noEff $ m { currentURI = u }
updateModel _ _ (ChangeURI u) m = m <# do
  pushURI u
  pure NoOp
updateModel _ oauthClient StartAuthentication m = m <# do
  (DoAuthRedirect . fmap OAuth.authorizationRequestUrl) <$> startAuth oauthClient
updateModel _ _ (DoAuthRedirect result) m =
  case result of
    Left err -> noEff $ m { authStatus = AuthenticationError err }
    Right u  -> m <# do
      assignURI (toMisoString $ show u)
      pure NoOp
updateModel _ oauthClient GetPrivateInfo m = m <# do
  SetPrivateInfo <$> getPrivateInfo oauthClient
updateModel _ _ (SetPrivateInfo result) m =
  case result of
    Left err -> noEff $ m { privateInfo = Just (ms $ show err) }
    Right info -> noEff $ m { privateInfo = Just (ms $ show info) }
updateModel clientEnv _ AddOne m =
  case authStatus m of
    Authenticated csrf ->
      (m { counter = counter m + 1 }) <# do
        post clientEnv csrf $ Api.setCounter secureBrowserClient (counter m + 1)
        pure NoOp
    _ -> noEff m
updateModel clientEnv _ SubtractOne m =
  case authStatus m of
    Authenticated csrf ->
      (m { counter = counter m - 1 }) <# do
        post clientEnv csrf $ Api.setCounter secureBrowserClient (counter m - 1)
        pure NoOp
    _ -> noEff m
updateModel _ _ NoOp m = noEff m

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m =
  case authStatus m of
    Init                    -> div_ [] [ text "You are being authenticated" ]
    Authenticated _         ->
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
    (AuthenticationError e) -> div_ [] [ text $ "There was an error: '" <> ms (show e) <> "'." ]

-- foreign import javascript unsafe
--   "((x) => { x.withCredentials = true; })"
--   js_setWithCredentials :: JS.XMLHttpRequest -> DOM.DOM ()

getAuthState :: ClientEnv -> URI -> JSM (Either ClientError OAuth.AuthState)
getAuthState oauthClientEnv pageURI = do
  flip runClientM oauthClientEnv $
    OAuth.loginEnd oauthClient $ OAuth.CallbackInfo $ T.pack $ show pageURI
  -- let
  --   req = XHR.Request { reqMethod = XHR.POST
  --                     , reqURI = pack $ oauthBaseUrl <> "/oauth-agent/login/end"
  --                     , reqLogin = Nothing
  --                     , reqHeaders = [("content-type", "application/json")]
  --                     , reqWithCredentials = True
  --                     , reqData = XHR.StringData $
  --                         pack $ "{ \"pageUrl\": \"" <> show pageURI <> "\" }"
  --                     }
  -- result <- mkXHRRequestJSON req
  -- case result of
  --   Left err -> pure $ Left $ "OAuth agent returned error: '" <> err <> "'."
  --   Right resp -> pure $ Right resp

startAuth :: ClientEnv -> JSM (Either ClientError OAuth.AuthRedirect)
startAuth oauthClientEnv = do
  flip runClientM oauthClientEnv $
    OAuth.loginStart oauthClient
  -- let
  --   req = XHR.Request { reqMethod = XHR.POST
  --                     , reqURI = pack $ oauthBaseUrl <> "/oauth-agent/login/start"
  --                     , reqLogin = Nothing
  --                     , reqHeaders = []
  --                     , reqWithCredentials = True
  --                     , reqData = XHR.NoData
  --                     }

  -- result <- mkXHRRequestJSON req
  -- case result of
  --   Left err -> pure $ Left $ "OAuth agent returned error: '" <> err <> "'."
  --   Right resp -> pure $ Right resp

getPrivateInfo :: ClientEnv -> JSM (Either ClientError Aeson.Value)
getPrivateInfo oauthClientEnv = do
  flip runClientM oauthClientEnv $
    OAuth.userInfo oauthClient
  -- let
  --   req = XHR.Request { reqMethod = XHR.GET
  --                     , reqURI = pack $ apiBaseUrl <> "/"
  --                     , reqLogin = Nothing
  --                     , reqHeaders = []
  --                     , reqWithCredentials = True
  --                     , reqData = XHR.NoData
  --                     }

  -- result <- mkXHRRequest req
  -- case result of
  --   Left err -> pure $ Left $ "API returned error: '" <> err <> "'."
  --   Right resp -> pure $ Right (toMisoString resp)

-- mkXHRRequestJSON :: FromJSON a => XHR.Request -> IO (Either MisoString a)
-- mkXHRRequestJSON req = do
--   result <- mkXHRRequest req
--   pure $ case result of
--     Left err ->
--       Left err
--     Right bytes ->
--       mapLeft toMisoString $ Aeson.eitherDecodeStrict bytes

-- mkXHRRequest :: XHR.Request -> IO (Either MisoString ByteString)
-- mkXHRRequest req = do
--   resp <- XHR.xhrByteString req
--   let statusCode = XHR.status resp
--       -- TODO: 400 Should be treated as auth failure, not error message worthy
--       statusFailure = statusCode < 200 || statusCode > 299

--   if statusFailure
--   then do
--     let failureMsg =
--           case XHR.contents resp of
--             Nothing  -> "no error message was returned."
--             Just msg -> "error message was: '" <> toMisoString msg <> "'."
--     pure $ Left $
--       "XHR request returned response code "
--         <> toMisoString statusCode <> " " <> failureMsg
--   else
--     pure $ Right $ fromMaybe "" $ XHR.contents resp

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

oauthClient :: OAuth.Api (AsClientT ClientM)
oauthClient = OAuth.client

-- -- | Haskell language pragma
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE CPP #-}

-- -- | Haskell module declaration
-- module Main where

-- -- | Miso framework import
-- import           Miso
-- import           Miso.String

-- -- | JSAddle import
-- #ifndef ghcjs_HOST_OS
-- import           Language.Javascript.JSaddle.Warp as JSaddle
-- import qualified Network.Wai.Handler.Warp         as Warp
-- import           Network.WebSockets
-- #endif
-- import           Control.Monad.IO.Class

-- -- | Type synonym for an application model
-- type Model = Int

-- -- | Sum type for application events
-- data Action
--   = AddOne
--   | SubtractOne
--   | NoOp
--   | SayHelloWorld
--   deriving (Show, Eq)

-- #ifndef ghcjs_HOST_OS
-- runApp :: JSM () -> IO ()
-- runApp f = JSaddle.debugOr 8000 (f >> syncPoint) JSaddle.jsaddleApp
-- #else
-- runApp :: IO () -> IO ()
-- runApp app = app
-- #endif

-- -- | Entry point for a miso application
-- main :: IO ()
-- main = runApp $ startApp App {..}
--   where
--     initialAction = SayHelloWorld -- initial action to be executed on application load
--     model  = 0                    -- initial model
--     update = updateModel          -- update function
--     view   = viewModel            -- view function
--     events = defaultEvents        -- default delegated events
--     subs   = []                   -- empty subscription list
--     mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
--     logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

-- -- | Updates model, optionally introduces side effects
-- updateModel :: Action -> Model -> Effect Action Model
-- updateModel AddOne m = noEff (m + 2)
-- updateModel SubtractOne m = noEff (m - 1)
-- updateModel NoOp m = noEff m
-- updateModel SayHelloWorld m = m <# do
--   liftIO (putStrLn "Hello World") >> pure NoOp

-- -- | Constructs a virtual DOM from a model
-- viewModel :: Model -> View Action
-- viewModel x = div_ [] [
--    button_ [ onClick AddOne ] [ text "+" ]
--  , text (ms x)
--  , button_ [ onClick SubtractOne ] [ text "-" ]
--  ]


post :: ClientEnv -> Maybe Text -> ClientM a -> JSM (Either ClientError a)
post env mCsrf x = do
  let clientEnv = env { fixUpXhr = \req -> do
                          -- Ensure we set xhr.withCredentials to true, so
                          -- cookies are sent
                          XHR.setWithCredentials req True
                          case mCsrf of
                            Nothing -> pure ()
                            Just csrf ->
                              XHR.setRequestHeader req ("X-example-csrf" :: Text) csrf
                       }
  runClientM x clientEnv

get :: ClientEnv -> ClientM a -> JSM (Either ClientError a)
get env x = runClientM x env

-- addNoAuth :: AuthenticatedRequest AuthAccess
-- addNoAuth = mkAuthenticatedRequest undefined (const id)

-- -- | The browser will send the auth cookie, we don't need to
-- secureBrowserClient :: SecuredApi (AsClientT ClientM)
-- secureBrowserClient =
--   Api.secured Api.client addNoAuth
