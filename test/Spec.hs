import qualified Spec.Web.Slack.Conversations.API
import           Test.Tasty
import           Web.Slack.Conversations.Test.Client
import           Web.Slack.Conversations.Test.MockServer (runMockServer)

main :: IO ()
main = runMockServer $ defaultMain =<< spec

spec :: IO TestTree
spec = testGroup "Web.Slack.Conversations" <$> sequence
  [ Spec.Web.Slack.Conversations.API.specWith client
  ]
  where
    client = TestClient
