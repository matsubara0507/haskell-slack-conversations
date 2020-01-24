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
import           Data.Text                (Text)
import           Network.Wai.Handler.Warp
import           Servant
import qualified Web.Slack.Conversations  as Slack

type RakutenHeader a = a

type API = "conversations.archive"    :> Post '[JSON] (Slack.Ok (Record '[ "ok" >: Bool ]))
      :<|> "conversations.close"      :> Post '[JSON] (Slack.Ok Slack.CloseResule)
      :<|> "conversations.create"     :> Post '[JSON] (Slack.Ok (Record '[ "channel" >: Slack.Conversation ]))
      :<|> "conversations.history"    :> Get '[JSON] (Slack.Ok Slack.Messages)
      :<|> "conversations.info"       :> Get '[JSON] (Slack.Ok (Record '[ "channel" >: Slack.Conversation ]))
      :<|> "conversations.invite"     :> Post '[JSON] (Slack.Ok (Record '[ "channel" >: Slack.Conversation ]))
      :<|> "conversations.join"       :> Post '[JSON] (Slack.Ok (Record '[ "channel" >: Slack.Conversation ]))
      :<|> "conversations.kick"       :> Post '[JSON] (Slack.Ok (Record '[ "ok" >: Bool ]))
      :<|> "conversations.leave"      :> Post '[JSON] (Slack.Ok (Record '[ "not_in_channel" >: Maybe Bool ]))
      :<|> "conversations.list"       :> Get '[JSON] (Slack.Ok Slack.Conversations)
      :<|> "conversations.members"    :> Get '[JSON] (Slack.Ok Slack.Members)
      :<|> "conversations.open"       :> Post '[JSON] (Slack.Ok Slack.OpenResult)
      :<|> "conversations.rename"     :> Post '[JSON] (Slack.Ok (Record '[ "channel" >: Slack.Conversation ]))
      :<|> "conversations.replies"    :> Get '[JSON] (Slack.Ok Slack.Replies)
      :<|> "conversations.setPurpose" :> Post '[JSON] (Slack.Ok (Record '[ "purpose" >: Text ]))
      :<|> "conversations.setTopic"   :> Post '[JSON] (Slack.Ok (Record '[ "topic" >: Text ]))
      :<|> "conversations.unarchive"  :> Post '[JSON] (Slack.Ok (Record '[ "ok" >: Bool ]))

api :: Proxy API
api = Proxy

server :: Server API
server = conversationsArchive
    :<|> conversationsClose
    :<|> conversationsCreate
    :<|> conversationsHistory
    :<|> conversationsInfo
    :<|> conversationsInvite
    :<|> conversationsJoin
    :<|> conversationsKick
    :<|> conversationsLeave
    :<|> conversationsList
    :<|> conversationsMembers
    :<|> conversationsOpen
    :<|> conversationsRename
    :<|> conversationsReplies
    :<|> conversationsSetPurpose
    :<|> conversationsSetTopic
    :<|> conversationsUnarchive
  where
    conversationsArchive = returnJsonFile "test/fixture/ok.json"
    conversationsClose = returnJsonFile "test/fixture/ok.json"
    conversationsCreate = returnJsonFile "test/fixture/conversation.json"
    conversationsHistory = returnJsonFile "test/fixture/messages.json"
    conversationsInfo = returnJsonFile "test/fixture/conversation.json"
    conversationsInvite = returnJsonFile "test/fixture/conversation.json"
    conversationsJoin = returnJsonFile "test/fixture/conversation.json"
    conversationsKick = returnJsonFile "test/fixture/ok.json"
    conversationsLeave = returnJsonFile "test/fixture/ok.json"
    conversationsList = returnJsonFile "test/fixture/conversations.json"
    conversationsMembers = returnJsonFile "test/fixture/members.json"
    conversationsOpen = returnJsonFile "test/fixture/conversation.json"
    conversationsRename = returnJsonFile "test/fixture/conversation.json"
    conversationsReplies = returnJsonFile "test/fixture/replies.json"
    conversationsSetPurpose = returnJsonFile "test/fixture/purpose.json"
    conversationsSetTopic = returnJsonFile "test/fixture/topic.json"
    conversationsUnarchive = returnJsonFile "test/fixture/ok.json"

    returnJsonFile path = liftIO (J.eitherDecodeFileStrict path) >>= \case
      Right json -> pure json
      Left err   -> throwError $ err500 { errBody = fromString err }


mockServer :: IO ()
mockServer = run 8000 (serve api server)

runMockServer :: IO () -> IO ()
runMockServer action = do
  _ <- forkIO mockServer
  action
