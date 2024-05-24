{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.Set as Set
import qualified Network.OAuth2.Experiment as OAuth
import qualified Network.OAuth2.Experiment.Pkce as OAuth
import qualified URI.ByteString as URI

import URI.ByteString (parseURI, laxURIParserOptions, URIRef, Absolute)

import qualified GHCJS.DOM.SubtleCrypto as SubtleCrypto
import qualified GHCJS.DOM.Crypto as Crypto
import qualified GHCJS.DOM.GlobalCrypto as GlobalCrypto
import GHCJS.DOM (globalThisUnchecked)
import GHCJS.DOM.Types (fromJSValUnchecked, toJSVal)

import qualified GHCJS.Buffer as Buffer
import qualified GHCJS.Buffer as Buffer
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.Marshal.Pure as JS
import JavaScript.TypedArray.ArrayBuffer (ArrayBuffer, thaw, freeze)

import qualified Data.ByteString.Base16 as Base16

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

data Cognito = Cognito deriving (Eq, Show)

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

  let
    oauthApp :: OAuth.AuthorizationCodeApplication
    oauthApp =
      OAuth.AuthorizationCodeApplication { OAuth.acName = "bnt-test"
                                         , OAuth.acClientId = "4bbo68nrfgf1vif5jl0gt4hk71"
                                         , OAuth.acClientSecret = ""
                                         , OAuth.acScope = mempty -- Set.fromList ["openid", "profile", "email"]
                                         , OAuth.acRedirectUri = parseURI' "http://localhost:3000/dashboard"
                                         , OAuth.acAuthorizeState = "changeMe" --undefined
                                         , OAuth.acAuthorizeRequestExtraParams = mempty
                                         , OAuth.acTokenRequestAuthenticationMethod = OAuth.ClientSecretBasic
                                         }
    cognitoIdp :: OAuth.Idp Cognito
    cognitoIdp = OAuth.Idp { OAuth.idpUserInfoEndpoint = parseURI' "https://bnt-test.auth.ap-southeast-2.amazoncognito.com/oauth2/userInfo"
                           , OAuth.idpAuthorizeEndpoint = parseURI' "https://bnt-test.auth.ap-southeast-2.amazoncognito.com/oauth2/authorize"
                           , OAuth.idpTokenEndpoint = parseURI' "https://bnt-test.auth.ap-southeast-2.amazoncognito.com/oauth2/token"
                           , OAuth.idpDeviceAuthorizationEndpoint = Nothing
                           }

    fooIdpApp :: OAuth.IdpApplication Cognito OAuth.AuthorizationCodeApplication
    fooIdpApp = OAuth.IdpApplication { OAuth.idp = cognitoIdp
                                     , OAuth.application = oauthApp
                                     }

  (authorizeReq, (OAuth.CodeVerifier codeVerifier)) <- OAuth.mkPkceAuthorizeRequest fooIdpApp

  codeVerifier' <- hashSHA256 $ T.encodeUtf8 codeVerifier

  error $ (show $ URI.serializeURIRef' authorizeReq) <> "|" <> T.unpack codeVerifier <> "|" <> T.unpack (T.decodeUtf8 codeVerifier')

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

foreign import javascript unsafe
  "((x,y,z) => { return z.slice(x,y); })" js_slice_imm :: Int -> Int -> ArrayBuffer -> ArrayBuffer

parseURI' :: ByteString.ByteString -> URIRef Absolute
parseURI' = either (\err -> error $ show err) id . parseURI laxURIParserOptions

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

hashSHA256 :: ByteString.ByteString -> IO ByteString.ByteString
hashSHA256 bs = do
  -- this   <- globalThisUnchecked
  -- crypto <- GlobalCrypto.getCrypto this
  -- subtle <- Crypto.getSubtle crypto
  -- let (buf, off, len) = Buffer.fromByteString bs
  -- arrayBuffer <- thaw $ js_slice_imm off (off + len) $ Buffer.getArrayBuffer buf
  -- hash <- SubtleCrypto.digest subtle ("SHA-256" :: T.Text) (DOM.ArrayBuffer $ JS.pToJSVal arrayBuffer)
  -- Base16.encode . Buffer.toByteString 0 Nothing . Buffer.createFromArrayBuffer <$> freeze (JS.pFromJSVal hash)

  -- TODO: Use bytestring directly
  T.encodeUtf8 . T.pack . DOM.fromJSString <$> digestMessage (DOM.toJSString $ T.unpack $ T.decodeUtf8 bs)

-- async function digestMessage(message) {
--   const msgUint8 = new TextEncoder().encode(message); // encode as (utf-8) Uint8Array
--   const hashBuffer = await window.crypto.subtle.digest("SHA-256", msgUint8); // hash the message
--   const hashArray = Array.from(new Uint8Array(hashBuffer)); // convert buffer to byte array
--   const hashHex = hashArray
--     .map((b) => b.toString(16).padStart(2, "0"))
--     .join(""); // convert bytes to hex string
--   return hashHex;
-- }

-- digestMessage(text2).then((digestHex) => console.log(digestHex));
