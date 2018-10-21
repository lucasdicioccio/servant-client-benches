{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)
import System.TimeIt (timeIt)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Monad (replicateM, when, forever)
import Data.Aeson
import Data.Default.Class (def)
import Data.Either (rights)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Servant
import Servant.Client
import qualified Network.HTTP2 as HTTP2
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Network.HTTP2.Client.Servant
import Network.HTTP2.Client
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS

type API = "hello" :> Capture "who" Text :> Get '[JSON] Int

api :: Proxy API
api = Proxy

handleHello :: Text -> Handler Int
handleHello who = pure $ Text.length who

sayHelloHttp :: Text -> ClientM Int
sayHelloHttp = client  api

sayHelloHttp2 :: Text -> H2ClientM Int
sayHelloHttp2 = h2client api

nIterations :: Int
nIterations = 10000

nTasks :: Int
nTasks = 10

someFunc :: IO ()
someFunc = do
    xs <- getArgs
    q <- newQSem nTasks
    case xs of
        "server":_ -> mainServer
        "http":_   -> mainHttp q
        "https":_  -> mainHttps q
        "http2":_  -> mainHttp2 q
        "http2c":_ -> mainHttp2c q

withQsem q f = do
  waitQSem q
  r <- f
  signalQSem q
  pure r

runTasks q f = timeIt $ mapConcurrently (\n -> withQsem q $ f n) [1..nIterations]

mainServer =
    runTLS tlsOpts warpOpts (serve api (handleHello))
  where
    tlsOpts = (tlsSettings "cert.pem" "key.pem") { onInsecure = AllowInsecure }
    warpOpts = setPort 8080 defaultSettings
mainHttp q  = do
    print "http"
    base <- parseBaseUrl "http://127.0.0.1:8080/"
    mgr <- newManager defaultManagerSettings
    let env = mkClientEnv mgr base
    xs <- runTasks q $ \_ -> runClientM (sayHelloHttp "hello") env
    print $ length $ rights xs
mainHttps q = do
    print "https"
    base <- parseBaseUrl "http://127.0.0.1:8080/"
    mgr <- newTlsManager
    let env = mkClientEnv mgr base
    xs <- runTasks q $ \_ -> runClientM (sayHelloHttp "hello") env
    print $ length $ rights xs
mainHttp2 q = do
    print "http2"
    frameConn <- newHttp2FrameConnection "127.0.0.1" 8080 (Just tlsParams)
    runHttp2Client frameConn 8192 8192 http2Settings defaultGoAwayHandler ignoreFallbackHandler $ \client -> do
        let icfc = _incomingFlowControl client
        _addCredit icfc 10000000
        _ <- forkIO $ forever $ do
            _ <- _updateWindow icfc
            threadDelay 100000
        let env = H2ClientEnv "127.0.0.1" client
        xs <- runTasks q $ \_ -> runH2ClientM (sayHelloHttp2 "hello") env
        print $ length $ rights xs
mainHttp2c q = do
    print "http2c"
    frameConn <- newHttp2FrameConnection "127.0.0.1" 8080 Nothing
    runHttp2Client frameConn 8192 8192 http2Settings defaultGoAwayHandler ignoreFallbackHandler $ \client -> do
        let icfc = _incomingFlowControl client
        _addCredit icfc 10000000
        _ <- forkIO $ forever $ do
            _ <- _updateWindow icfc
            threadDelay 100000
        let env = H2ClientEnv "127.0.0.1" client
        xs <- runTasks q $ \_ -> runH2ClientM (sayHelloHttp2 "hello") env
        print $ length $ rights xs

http2Settings = [
   (HTTP2.SettingsInitialWindowSize, 10485760)
 , (HTTP2.SettingsEnablePush, 0)
 ]

tlsParams = TLS.ClientParams {
      TLS.clientWantSessionResume    = Nothing
    , TLS.clientUseMaxFragmentLength = Nothing
    , TLS.clientServerIdentification = ("127.0.0.1", "8080")
    , TLS.clientUseServerNameIndication = True
    , TLS.clientShared               = def
    , TLS.clientHooks                = def { TLS.onServerCertificate = \_ _ _ _ -> return []
                                           }
    , TLS.clientSupported            = def { TLS.supportedCiphers = TLS.ciphersuite_default }
    , TLS.clientDebug                = def
    }
