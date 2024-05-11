{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Main where

import Miso
import Miso.String
import qualified Miso
import qualified Data.ByteString.Char8 as ByteString
import Servant.Client.Core
import Network.HTTP.Types (hAuthorization)
import qualified Debug.Trace as Debug

import Control.Monad.IO.Class
import Servant.Client.JSaddle

import Common
import qualified Common as API (client)
import qualified Data.Aeson as Aeson

#ifdef IOS
import Language.Javascript.JSaddle.WKWebView as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run
#else
import Language.Javascript.JSaddle.Warp as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run 8080
#endif

-- | Type synonym for an application model
type Model = Int

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = do
  let clientEnv = mkClientEnv BaseUrl
        { baseUrlScheme = Http
        , baseUrlHost = "localhost"
        , baseUrlPort = 8081
        , baseUrlPath = ""
        }

  (Right initialCount) <- flip runClientM clientEnv $ getCounter API.client

  runApp $ do
    let
      model = initialCount
      update = updateModel clientEnv -- update function
    miso $ \_ -> Miso.App {..}
  where
    initialAction = SayHelloWorld  -- initial action to be executed on application load
    view   = viewModel             -- view function
    events = defaultEvents         -- default delegated events
    subs   = []                    -- empty subscription list
    mountPoint = Nothing           -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                 -- Used to copy DOM into VDOM, applies only to `miso` function

-- | Updates model, optionally introduces side effects
updateModel :: ClientEnv -> Action -> Model -> Effect Action Model
updateModel clientEnv AddOne m = (m + 1) <# do
  flip runClientM clientEnv $ setCounter securedClient $ (m + 1)
  pure NoOp
updateModel clientEnv SubtractOne m = (m - 1) <# do
  flip runClientM clientEnv $ setCounter securedClient $ (m - 1)
  pure NoOp
updateModel _ NoOp m = noEff m
updateModel _ SayHelloWorld m = m <# do
  liftIO (putStrLn "Hello World") >> pure NoOp

type instance AuthClientData AuthAccess = ByteString.ByteString

addAuth :: ByteString.ByteString -> AuthenticatedRequest AuthAccess
addAuth = flip mkAuthenticatedRequest (\v -> addHeader hAuthorization ("Bearer " <> ByteString.unpack v))

securedClient :: SecuredApi (AsClientT ClientM)
securedClient =
  -- Hardcoded auth header, we'd probably want to get this from a cookie.
  secured API.client
    (addAuth "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyLCJlbWFpbCI6ImZvb0BiYXIuY29tIiwiZW1haWxfdmVyaWZpZWQiOnRydWV9.pV7R4m7Jo0hvWKVRJsTYrggTuYNZ1H0HP6kTpwagzEE")

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text (ms x)
 , button_ [ onClick SubtractOne ] [ text "-" ]
 ]
