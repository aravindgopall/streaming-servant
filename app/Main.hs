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
import           Servant.Types.SourceT

type FastAPI
   = "get" :> StreamGet NewlineFraming JSON (SourceT IO [String])
   :<|> "health" :> Get '[JSON] Text

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
      d <- liftIO $ Prelude.readFile "sampleData.json"
      return $ sourcen 5 $ Prelude.lines d -- assuming each json value is seperated by new line.
    health = return "up"

app :: Application
app = serve api server

main :: IO ()
main = do
  putStrLn "Starting server at http://localhost:8080"
  Warp.run 8080 app
