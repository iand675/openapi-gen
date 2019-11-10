{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Main where
-- Server bits
{-
import Control.Concurrent.STM
import qualified Network.HTTP.Types.Method as M
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.WebSockets
import Network.Wai.Handler.Warp (runEnv)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WS
-}
import Control.Monad
import OpenAPI.Gen.Config
import OpenAPI.Gen.Reader
import OpenAPI.Gen.Language.Haskell

main :: IO ()
main = do
  conf <- loadConfig "config.yaml"
  spec <- loadRoot $ configSpec conf

  forM_ (configHaskell conf) $ \conf -> do
    putStrLn "Generating Haskell bindings..."
    go conf spec






{-
serveIt :: IO ()
serveIt = do
  globalState <- VisualizerState <$> newTVarIO False <*> newTVarIO responseValue
  putStrLn "Initializing state..."

  runEnv 3000 $ webApp globalState

responseValue :: Value
responseValue = object
  [ "input" .= object
    [ "noddle" .= (1 :: Int)
    , "woo" .= True
    ]
  , "output" .= object
    [ "woodle" .= ("woo" :: Text)
    , "noggin" .= Null
    ]
  ]

data VisualizerState = VisualizerState
  { visualizerStateChanged :: TVar Bool
  , visualizerStateValue :: TVar Value
  }


webApp :: VisualizerState -> Application
webApp serverState = \req respond -> case pathInfo req of
  ["rendered", "watch"] -> (websocketsOr
    WS.defaultConnectionOptions
    appWatch
    (\_ respond' -> respond' $ responseLBS status400 [] "Not a WebSocket request")) req respond
  ["rendered"] -> do
    st <- atomically $ readTVar $ visualizerStateValue serverState
    respond $ responseLBS status200 [(hContentType, "application/json")] $ encode st
  where
    appWatch pConn = do
      conn <- WS.acceptRequest pConn
      atomically (readTVar (visualizerStateValue serverState)) >>= WS.sendTextData conn . encodeToLazyText
      forever $ do
        st <- timeout 5000000 $ atomically $ do
          changed <- readTVar $ visualizerStateChanged serverState
          check changed
          readTVar $ visualizerStateValue serverState
        case st of
          Nothing -> do
            putStr "."
            WS.sendPing conn ("" :: Text)
          Just st -> do
            putStr "x"
            WS.sendTextData conn $ encodeToLazyText st

-}
