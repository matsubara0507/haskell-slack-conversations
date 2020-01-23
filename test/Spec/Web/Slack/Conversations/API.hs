module Spec.Web.Slack.Conversations.API
    ( specWith
    ) where


import           Data.Extensible
import           Test.Tasty
import           Test.Tasty.Hspec
import           Web.Slack.Conversations.API         as Conversations
import           Web.Slack.Conversations.Client      (Client)
import           Web.Slack.Conversations.Test.Helper (shouldResponseAs)

specWith :: Client c => c -> IO TestTree
specWith client = testSpec "Web.Slack.Conversations" $ do
  describe "info" $
    it "should return conversation" $
      Conversations.info client "C012AB3CD" vacancy `shouldResponseAs` "test/fixture/conversation.json"
  describe "list" $
    it "should return conversations" $
      Conversations.list client vacancy `shouldResponseAs` "test/fixture/conversations.json"
