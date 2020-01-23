{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Web.Slack.Conversations.Test.MockServer
    ( mockServer
    , runMockServer
    ) where

import           Control.Concurrent
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.Aeson               as J
import           Data.Extensible
import           Data.String              (fromString)
import           Network.Wai.Handler.Warp
import           Servant
import qualified Web.Slack.Conversations  as Slack

type RakutenHeader a = a

type API = "conversations.info" :> Get '[JSON] (Slack.Ok (Record '[ "channel" >: Slack.Conversation ]))
      :<|> "conversations.list" :> Get '[JSON] (Slack.Ok Slack.Conversations)

api :: Proxy API
api = Proxy

server :: Server API
server = conversationsInfo
    :<|> conversationsList
  where
    conversationsInfo = returnJsonFile "test/fixture/conversation.json"
    conversationsList = returnJsonFile "test/fixture/conversations.json"

    returnJsonFile path = liftIO (J.eitherDecodeFileStrict path) >>= \case
      Right json -> pure json
      Left err   -> throwError $ err500 { errBody = fromString err }


mockServer :: IO ()
mockServer = run 8000 (serve api server)

runMockServer :: IO () -> IO ()
runMockServer action = do
  _ <- forkIO mockServer
  action
