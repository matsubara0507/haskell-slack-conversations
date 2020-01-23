module Web.Slack.Conversations.Test.Client
    ( TestClient(..)
    ) where

import           Network.HTTP.Req        (Scheme (Http), http, port)
import           Web.Slack.Conversations (Client (..))

data TestClient = TestClient

instance Client TestClient where
  type ClientScheme TestClient = 'Http
  baseUrl _ = http "localhost"
  mkHeader _ = mconcat [ port 8000 ]
