{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main
  ( main
  ) where

import Control.Monad.IO.Class
import           Data.Proxy               (Proxy (..))
import           Data.Text (Text)
import qualified Network.Wai.Handler.Warp as Warp
import           Servant.API
import           Servant.Server
--import Servant.Client hiding (client, runClientM)
import           Servant.Client.Streaming

import           Servant.Types.SourceT
import  Network.HTTP.Client hiding (Proxy)

type FastAPI
   = "get" :> StreamGet NewlineFraming JSON (SourceT IO String)
   :<|> "health" :> Get '[JSON] Text


getAPI :<|> _ = client api

api :: Proxy FastAPI
api = Proxy

sourcen :: Int -> [a] -> SourceT IO [a]
sourcen n l = fromStepT $ go l
  where
    go [] = Stop
    go xs = Yield (Prelude.take n xs) $ go (Prelude.drop n xs)

server :: Server FastAPI
server = fast :<|> health
  where
    fast = do
      mgr <- liftIO $ newManager defaultManagerSettings
      url <- liftIO $ parseBaseUrl "clickhouseurl"
      liftIO $ withClientM getAPI (mkClientEnv mgr url) (\resp ->
        case resp of
          Left err -> return $ source []
          Right x -> return $ x
        )
      --d <- liftIO $ Prelude.readFile "sampleData.json" -- clickhouse/file
      --return $ sourcen 5 $ Prelude.lines d -- assuming each json value is seperated by new line.
    health = return "up"

app :: Application
app = serve api server

main :: IO ()
main = do
  putStrLn "Starting server at http://localhost:8080"
  Warp.run 8080 app
