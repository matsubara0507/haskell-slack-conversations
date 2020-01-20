module Web.Slack.Conversations
    ( module X
    , run
    ) where

import           Control.Monad.IO.Class         (MonadIO)
import           Data.Aeson                     (FromJSON)
import           Data.Text                      (Text)
import           Network.HTTP.Req               (Req, defaultHttpConfig,
                                                 responseBody, runReq)
import           Web.Slack.Conversations.API    as X
import           Web.Slack.Conversations.Client as X
import           Web.Slack.Conversations.Type   as X

run :: (MonadIO m, FromJSON r) => Req (SlackApiResponse r) -> m (Either Text r)
run request = runReq defaultHttpConfig $ do
  result <- request
  pure $ case responseBody result of
    Ok r  -> Right r
    Err e -> Left e
