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
import qualified JavaScript.Web.XMLHttpRequest as XHR
import GHC.Generics (Generic)

-- | JSAddle import
#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets
#endif
import           Control.Monad.IO.Class
import qualified Control.Concurrent as C

-- | TODO: Load config
oauthBaseUrl = "http://api.example.com:8082"
apiBaseUrl = "http://api.example.com:8081"

-- | Type synonym for an application model
data Model = Init
           | Authenticated
           | NotAuthenticated
           | AuthenticationError MisoString
  deriving (Eq, Show)

-- | Sum type for application events
data Action
  = NoOp
  | CheckAuthentication
  | SetAuthentication (Either MisoString ())
  deriving (Show, Eq)

#ifndef ghcjs_HOST_OS
runApp :: JSM () -> IO ()
runApp f = JSaddle.debugOr 8080 (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp App {..}
  where
    initialAction = CheckAuthentication -- initial action to be executed on application load
    model  = Init                       -- initial model
    update = updateModel                -- update function
    view   = viewModel                  -- view function
    events = defaultEvents              -- default delegated events
    subs   = []                         -- empty subscription list
    mountPoint = Nothing                -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                      -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel CheckAuthentication m = m <# do
  SetAuthentication <$> getAuthState
updateModel (SetAuthentication result) m =
  case result of
    Left err -> noEff (AuthenticationError err)
    Right () -> noEff Authenticated

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel Init = div_ [] [ text "You are being authenticated" ]
viewModel Authenticated = div_ [] [ text "You are authenticated" ]
viewModel NotAuthenticated = div_ [] [ text "You are NOT authenticated" ]
viewModel (AuthenticationError e) = div_ [] [ text $ "There was an error: '" <> e <> "'." ]

-- foreign import javascript unsafe
--   "((x) => { x.withCredentials = true; })"
--   js_setWithCredentials :: JS.XMLHttpRequest -> DOM.DOM ()

getAuthState :: IO (Either MisoString ())
getAuthState = do
  let
    req = XHR.Request { reqMethod = XHR.POST
                      , reqURI = pack "http://www.example.com:8082/oauth-agent/login/end"
                      , reqLogin = Nothing
                      , reqHeaders = []
                      , reqWithCredentials = True
                      , reqData = XHR.NoData
                      }

  Just resp <- XHR.contents <$> XHR.xhrByteString req
  case Aeson.eitherDecodeStrict resp of
    Left s -> error s
    Right (j :: AuthenticateResponse) -> pure $ Right ()

data AuthenticateResponse
  = AuthenticateResponse { handled :: Bool
                         , isLoggedIn :: Bool
                         , csrf :: Maybe MisoString
                         }
  deriving (Eq, Show, Generic)

instance FromJSON AuthenticateResponse where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions
