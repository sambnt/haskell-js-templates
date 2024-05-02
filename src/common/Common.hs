{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Common where

import Servant.API
    ( type (:<|>)(..), JSON, ReqBody, type (:>), Get, Post )
import Data.Proxy ( Proxy(..) )
import qualified Network.URI as Network
import Servant.Links (linkURI)
import Servant.Client.Core (clientIn, HasClient(Client))

type SetCounter = "counter" :> ReqBody '[JSON] Int :> Post '[JSON] ()
type GetCounter = "counter" :> Get '[JSON] Int

type CounterAPI = SetCounter
             :<|> GetCounter

data CounterAPIClient m
  = CounterAPIClient
    { setCount :: !(Client m SetCounter)
    , getCount :: !(Client m GetCounter)
    }

client :: forall m . HasClient m CounterAPI => CounterAPIClient m
client = CounterAPIClient{..}
  where
    setCount :<|> getCount = Proxy @CounterAPI `clientIn` Proxy @m
