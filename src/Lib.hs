{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)
import System.TimeIt (timeIt)
import Control.Exception (throwIO)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Monad (replicateM, when, forever)
import Network.Connection
import Data.Aeson
import Data.Default.Class (def)
import Data.Either (lefts,rights)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as LByteString
import GHC.Generics
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Servant
import Servant.Client
import qualified Network.HTTP2 as HTTP2
import Network.HTTP.Client hiding (Proxy,port)
import Network.HTTP.Client.TLS
import Network.HTTP2.Client.Servant
import Network.HTTP2.Client
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS


import Control.Lens ((.~), (&), (^.), view, _Right)
import Control.DeepSeq (force)
import Data.ProtoLens.Message (defMessage)
import Data.ProtoLens.Service.Types (MethodInput, MethodOutput, Service, HasMethod)
import Network.GRPC.Server (runGrpc, unary, UnaryHandler)
import Network.GRPC.Client (open, Timeout(..), singleRequest, RawReply)
import Network.GRPC.HTTP2.Types (RPC(..))
import Network.GRPC.HTTP2.Encoding (gzip, uncompressed, Encoding(..), Decoding(..))
import qualified Proto.Protos.Example as Example
import qualified Proto.Protos.Example_Fields as Example

type API = "hello" :> Capture "who" Text :> Get '[JSON] Text

api :: Proxy API
api = Proxy

handleHello :: Text -> Handler Text
handleHello who = pure who

sayHelloHttp :: Text -> ClientM Text
sayHelloHttp = client  api

sayHelloHttp2 :: Text -> H2ClientM Text
sayHelloHttp2 = h2client api

grpcHandleHello :: UnaryHandler Example.Example "hello"
grpcHandleHello _ ex = pure $ defMessage & Example.whom .~ (ex ^.Example.whom)

{-# INLINE runGrpcClient #-}
runGrpcClient
  :: (Service s, HasMethod s m, Show (MethodOutput s m))
  => Http2Client
  -> RPC s m
  -> MethodInput s m
  -> (MethodOutput s m -> b)
  -> IO (Either String b)
runGrpcClient client x y f = do
    x <- open client "127.0.0.1:8081" [] (Timeout 10) (Encoding uncompressed) (Decoding uncompressed) (singleRequest x y)
    let !v = case x of
                (Right (Right (_,_,Right r))) -> let ret = force f r in (Right ret)
                _                             -> Left (show x)
    pure v

nIterations :: Int
nIterations = 10000

nTasks :: Int
nTasks = 10

port = 8080
grpcport = port + 1

someFunc :: IO ()
someFunc = do
    xs <- getArgs
    case xs of
        "server":_ -> mainServer
        "grpc-server":_ -> mainGrpcServer
        "http":_   -> mainHttp
        "https":_  -> mainHttps
        "http2":_  -> mainHttp2
        "http2c":_ -> mainHttp2c
        "grpc":_   -> mainGrpc
        "grpcc":_  -> mainGrpcc

{-# INLINE runTasks #-}
runTasks f = timeIt $ do
    fmap mconcat $ mapConcurrently go [1..nTasks]
  where
    go n = replicateM nIterations (f n)

mainServer =
    runTLS tlsOpts warpOpts (serve api (handleHello))
  where
    tlsOpts = (tlsSettings "cert.pem" "key.pem") { onInsecure = AllowInsecure }
    warpOpts = setPort 8080 defaultSettings
mainGrpcServer =
    runGrpc tlsOpts warpOpts [unary (RPC :: RPC Example.Example "hello") grpcHandleHello] [gzip]
  where
    tlsOpts = (tlsSettings "cert.pem" "key.pem") { onInsecure = AllowInsecure }
    warpOpts = setPort 8081 defaultSettings
mainHttp = do
    print "http"
    base <- parseBaseUrl $ "http://127.0.0.1:" <> show port
    mgr <- newManager defaultManagerSettings
    let env = mkClientEnv mgr base
    xs <- runTasks $ \_ -> runClientM (sayHelloHttp "world.json") env
    printResults xs
mainHttps = do
    print "https"
    base <- parseBaseUrl $ "https://127.0.0.1:" <> show port
    let mgrSetts = mkManagerSettings (TLSSettings tlsParams) Nothing
    mgr <- newTlsManagerWith mgrSetts
    let env = mkClientEnv mgr base
    xs <- runTasks $ \_ -> runClientM (sayHelloHttp "world.json") env
    printResults xs
mainHttp2 = do
    print "http2"
    frameConn <- newHttp2FrameConnection "127.0.0.1" port (Just tlsParams)
    runHttp2Client frameConn 8192 8192 http2Settings defaultGoAwayHandler ignoreFallbackHandler $ \client -> do
        _ <- creditClientForever client
        let env = H2ClientEnv "127.0.0.1" client
        xs <- runTasks $ \_ -> runH2ClientM (sayHelloHttp2 "world.json") env
        printResults xs
mainHttp2c = do
    print "http2c"
    frameConn <- newHttp2FrameConnection "127.0.0.1" port Nothing
    runHttp2Client frameConn 8192 8192 http2Settings defaultGoAwayHandler ignoreFallbackHandler $ \client -> do
        _ <- creditClientForever client
        let env = H2ClientEnv "127.0.0.1" client
        xs <- runTasks $ \_ -> runH2ClientM (sayHelloHttp2 "world.json") env
        printResults xs
mainGrpc = do
    print "grpc"
    frameConn <- newHttp2FrameConnection "127.0.0.1" grpcport (Just tlsParams)
    runHttp2Client frameConn 8192 8192 http2Settings defaultGoAwayHandler ignoreFallbackHandler $ \client -> do
        _ <- creditClientForever client
        xs <- runTasks $ \_ -> runGrpcClient client (RPC :: RPC Example.Example "hello") (defMessage & Example.whom .~ "world.json") getWhom
        printResults xs
mainGrpcc = do
    print "grpcc"
    frameConn <- newHttp2FrameConnection "127.0.0.1" grpcport Nothing
    runHttp2Client frameConn 8192 8192 http2Settings defaultGoAwayHandler ignoreFallbackHandler $ \client -> do
        _ <- creditClientForever client
        xs <- runTasks $ \_ -> runGrpcClient client (RPC :: RPC Example.Example "hello") (defMessage & Example.whom .~ "world.json") getWhom
        printResults xs

getWhom :: Example.HelloRsp -> Text
getWhom = view Example.whom

printResults xs = do
    print $ take 1 $ lefts xs
    print $ take 1 $ rights xs
    print $ length $ rights xs

creditClientForever client = do
    let icfc = _incomingFlowControl client
    _addCredit icfc 10000000
    forkIO $ forever $ do
        _ <- _updateWindow icfc
        threadDelay 100000

http2Settings = [
   (HTTP2.SettingsInitialWindowSize, 10485760)
 , (HTTP2.SettingsEnablePush, 0)
 ]

tlsParams = TLS.ClientParams {
      TLS.clientWantSessionResume    = Nothing
    , TLS.clientUseMaxFragmentLength = Nothing
    , TLS.clientServerIdentification = ("127.0.0.1", ByteString.pack $ show port)
    , TLS.clientUseServerNameIndication = True
    , TLS.clientShared               = def
    , TLS.clientHooks                = def { TLS.onServerCertificate = \_ _ _ _ -> return []
                                           }
    , TLS.clientSupported            = def { TLS.supportedCiphers = TLS.ciphersuite_default }
    , TLS.clientDebug                = def
    }
