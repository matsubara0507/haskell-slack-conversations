module Web.Slack.Conversations.Client
    ( Client (..)
    , SlackApiClient
    , newClient
    ) where

import           Data.ByteString  (ByteString)
import           Network.HTTP.Req (Option, Scheme (..), Url, (/:))
import qualified Network.HTTP.Req as Req

class Client a where
  type ClientScheme a :: Scheme
  baseUrl :: a -> Url (ClientScheme a)
  mkHeader :: a -> Option (ClientScheme a)

type Token = ByteString

newtype SlackApiClient = SlackApiClient Token

instance Client SlackApiClient where
  type ClientScheme SlackApiClient = 'Https
  baseUrl = const (Req.https "slack.com" /: "api")
  mkHeader (SlackApiClient token) = Req.oAuth2Bearer token

newClient :: Token -> SlackApiClient
newClient = SlackApiClient
