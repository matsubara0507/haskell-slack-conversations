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
  describe "archive" $
    it "should return ok" $
      Conversations.archive client "C1234567890" `shouldResponseAs` "test/fixture/ok.json"
  describe "close" $
    it "should return ok" $
      Conversations.close client "C1234567890" `shouldResponseAs` "test/fixture/ok.json"
  describe "create" $
    it "should return conversation" $
      Conversations.create client "test" vacancy `shouldResponseAs` "test/fixture/conversation.json"
  describe "history" $
    it "should return messages" $
      Conversations.history client "C1234567890" vacancy `shouldResponseAs` "test/fixture/messages.json"
  describe "info" $
    it "should return conversation" $
      Conversations.info client "C1234567890" vacancy `shouldResponseAs` "test/fixture/conversation.json"
  describe "invite" $
    it "should return conversation" $
      Conversations.invite client "C1234567890" [] `shouldResponseAs` "test/fixture/conversation.json"
  describe "join" $
    it "should return conversation" $
      Conversations.join client "C1234567890" `shouldResponseAs` "test/fixture/conversation.json"
  describe "kick" $
    it "should return ok" $
      Conversations.kick client "C1234567890" "W1234567890" `shouldResponseAs` "test/fixture/ok.json"
  describe "leave" $
    it "should return ok" $
      Conversations.leave client "C1234567890" `shouldResponseAs` "test/fixture/ok.json"
  describe "list" $
    it "should return conversations" $
      Conversations.list client vacancy `shouldResponseAs` "test/fixture/conversations.json"
  describe "members" $
    it "should return members" $
      Conversations.members client "C1234567890" vacancy `shouldResponseAs` "test/fixture/members.json"
  describe "open" $
    it "should return conversation" $
      Conversations.open client vacancy `shouldResponseAs` "test/fixture/conversation.json"
  describe "rename" $
    it "should return conversation" $
      Conversations.rename client "C1234567890" "test" `shouldResponseAs` "test/fixture/conversation.json"
  describe "replies" $
    it "should return replies" $
      Conversations.replies client "C1234567890" "1234567890.123456" vacancy `shouldResponseAs` "test/fixture/replies.json"
  describe "setPurpose" $
    it "should return purpose" $
      Conversations.setPurpose client "C1234567890" "My More Special Purpose" `shouldResponseAs` "test/fixture/purpose.json"
  describe "setTopic" $
    it "should return topic" $
      Conversations.setTopic client "C1234567890" "Apply topically for best effects" `shouldResponseAs` "test/fixture/topic.json"
  describe "unarchive" $
    it "should return ok" $
      Conversations.unarchive client "C1234567890" `shouldResponseAs` "test/fixture/ok.json"
