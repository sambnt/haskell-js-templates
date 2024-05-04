{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Main where

import Miso
import Miso.String

import Control.Monad.IO.Class
import Servant.Client.JSaddle
import           JavaScript.Web.XMLHttpRequest

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

  (Right initialCount) <- flip runClientM clientEnv $ getCount API.client

  runApp $ do
    let
      model = initialCount
      update = updateModel clientEnv -- update function
    miso $ \_ -> App {..}
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
  flip runClientM clientEnv $ setCount API.client (m + 1)
  pure NoOp
updateModel clientEnv SubtractOne m = (m - 1) <# do
  flip runClientM clientEnv $ setCount API.client (m - 1)
  pure NoOp
updateModel _ NoOp m = noEff m
updateModel _ SayHelloWorld m = m <# do
  liftIO (putStrLn "Hello World") >> pure NoOp

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
   button_ [ onClick AddOne ] [ text "+" ]
 , text (ms x)
 , button_ [ onClick SubtractOne ] [ text "-" ]
 ]
