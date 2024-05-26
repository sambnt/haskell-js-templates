{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE CPP                 #-}

module Main where

import Network.URI (URI)
import Miso
import Miso.String
import qualified Miso
import Data.Proxy
import Servant.API
import Servant.Links
import Data.Text

import Language.Javascript.JSaddle.Warp as JSaddle

runApp :: JSM () -> IO ()
runApp = JSaddle.run 8080

-- Routes

type Routes
  =    HomeRoute
  :<|> DashboardRoute

type HomeRoute = View Action

type DashboardRoute = "dashboard" :> QueryParam "code" Text :> View Action
-- type DashboardRoute = "dashboard" :> View Action

homeLink :: URI
homeLink = linkURI (safeLink (Proxy @Routes) (Proxy @HomeRoute))

dashboardLink :: Maybe Text -> URI
dashboardLink queryParamCode =
  linkURI (safeLink (Proxy @Routes) (Proxy @DashboardRoute) queryParamCode)

-- View
viewModel :: Model -> View Action
viewModel m = div_ [] [ page m ]

page :: Model -> View Action
page m =
  let
    pages       = homePage :<|> dashboardPage
    currentPage = runRoute (Proxy @Routes) pages currentURI m
  in
    case currentPage of
      Left _  -> notFoundPage
      Right p -> p

homePage :: Model -> View Action
homePage _ = div_ [] [ nav "Home"
                     , button_ [ onClick $ ChangeURI $ dashboardLink (Just "deadbeef") ] [ text "Go to dashboard" ]
                     , a_ [ href_ "https://www.google.com" ] [ text "sign in" ]
                     ]

dashboardPage :: Maybe Text -> Model -> View Action
dashboardPage code _ =
  let
    codeStr =
      case code of
        Nothing -> "No code provided"
        Just x  -> "Code is: " <> x
  in
    div_ [] [ nav ("Dashboard" <> toMisoString codeStr)
            , button_ [ onClick $ ChangeURI homeLink ] [ text "Go to home" ]
            ]

notFoundPage :: View action
notFoundPage = errorPage "Not found"

errorPage :: String -> View action
errorPage err = div_ []
    [ div_ [ class_ "clearfix mb2 white bg-black p1" ] []
    , div_ [ class_ "p2" ] [ text . ms $ err ]
    ]

nav :: MisoString -> View action
nav title = div_
    [ class_ "clearfix mb2 white bg-black"]
    [ div_ [ class_ "left p2" ] [ text title ] ]

-- Model
data Model = Model
  { currentURI :: URI
  } deriving (Eq, Show)

initialModel :: URI -> Model
initialModel uri =
  Model { currentURI = uri
        }

-- Action
data Action
  = HandleURI URI
  | ChangeURI URI
  | NoOp

updateModel :: Action -> Model -> Effect Action Model
updateModel (HandleURI u) m = m { currentURI = u } <# do
  pure NoOp
updateModel (ChangeURI u) m = m <# do
  pushURI u
  pure NoOp
updateModel _ m = noEff m

-- Main
main :: IO ()
main =
  runApp $ do
    currentURI <- getCurrentURI
    startApp $
      App { model = Model currentURI
          , initialAction = NoOp
          , update = updateModel
          , events = defaultEvents
          , subs = [ uriSub HandleURI ]
          , view = viewModel
          , mountPoint = Nothing
          , logLevel = Off
          }
