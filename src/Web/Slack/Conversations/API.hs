module Web.Slack.Conversations.API
    ( SlackApiResponse
    , OptionalParams
    , buildGetApi
    , buildPostApi
    , NextCursor
    , archive
    , CloseResule
    , close
    , CreateParams
    , create
    , Messages
    , HistoryParams
    , history
    , InfoParams
    , info
    , invite
    , join
    , kick
    , leave
    , Conversations
    , ListParams
    , list
    , Members
    , MembersParams
    , members
    , OpenResult
    , OpenParams
    , open
    , rename
    , Replies
    , RepliesParams
    , replies
    , setPurpose
    , setTopic
    , unarchive
    , create'
    , Channels
    , list'
    ) where

import           Data.Aeson                           (FromJSON)
import           Data.Extensible
import           Data.Function                        ((&))
import           Data.Text                            (Text)
import           Lens.Micro                           ((.~))
import           Network.HTTP.Req
import           Web.Slack.Conversations.API.Internal (OptionalParams,
                                                       buildRequestParams,
                                                       toQueryParam')
import           Web.Slack.Conversations.Client
import           Web.Slack.Conversations.Type         as Slack

type SlackApiResponse r = JsonResponse (Slack.Ok r)

buildUrl :: Client c => c -> Text -> Url (ClientScheme c)
buildUrl c path = baseUrl c /: "conversations." <> path

buildGetApi ::
  (MonadHttp m, Client c, FromJSON r)
  => Text                          -- ^ request path
  -> c                             -- ^ client
  -> Option (ClientScheme c)       -- ^ request params
  -> m (SlackApiResponse r)
buildGetApi path c params =
  req GET (buildUrl c path) NoReqBody jsonResponse (mkHeader c <> params)

buildPostApi ::
  (MonadHttp m, Client c, FromJSON r)
  => Text                          -- ^ request path
  -> c                             -- ^ client
  -> Option (ClientScheme c)       -- ^ request params
  -> m (SlackApiResponse r)
buildPostApi path c params =
  req POST (buildUrl c path) NoReqBody jsonResponse (mkHeader c <> params)

type NextCursor = '[ "next_cursor" >: Text ]


-- | API for conversations

archive
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> m (SlackApiResponse (Record '[ "ok" >: Bool ]))
archive client cid = buildPostApi "archive" client ("channel" =: cid)

type CloseResule = Record
  '[ "no_op"          >: Maybe Bool
   , "already_closed" >: Maybe Bool
   ]

close
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> m (SlackApiResponse CloseResule)
close client cid = buildPostApi "close" client ("channel" =: cid)

type CreateParams = OptionalParams
  '[ "is_private" >: Bool
   , "user_ids"   >: [Slack.UserID]
   ]

create
  :: (MonadHttp m, Client c)
  => c
  -> Text -- ^ channel name
  -> CreateParams
  -> m (SlackApiResponse (Record '[ "channel" >: Slack.Conversation ]))
create client cname =
  buildPostApi "create" client . ("name" =: cname <>) . buildRequestParams

type Messages = Record
  '[ "messages"          >: [Slack.Message]
   , "has_more"          >: Bool
   , "pin_count"         >: Int
   , "response_metadata" >: Record NextCursor
   ]

type HistoryParams = OptionalParams
  '[ "cursor"    >: Text
   , "inclusive" >: Bool
   , "latest"    >: Slack.TimeStamp
   , "limit"     >: Int
   , "oldest"    >: Slack.TimeStamp
   ]

history
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> HistoryParams
  -> m (SlackApiResponse Messages)
history client cid =
  buildGetApi "history" client . ("channel" =: cid <>) . buildRequestParams

type InfoParams = OptionalParams
  '[ "include_locale"      >: Bool
   , "include_num_members" >: Bool
   ]

info
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> InfoParams
  -> m (SlackApiResponse (Record '[ "channel" >: Slack.Conversation ]))
info client cid =
  buildGetApi "info" client . ("channel" =: cid <>) . buildRequestParams

invite
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> [Slack.UserID]
  -> m (SlackApiResponse (Record '[ "channel" >: Slack.Conversation ]))
invite client cid uids = buildPostApi "invite" client opts
  where
    opts = "channel" =: cid <> "users" =: toQueryParam' uids

join
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> m (SlackApiResponse (Record '[ "channel" >: Slack.Conversation ]))
join client cid = buildPostApi "join" client ("channel" =: cid)

kick
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> Slack.UserID
  -> m (SlackApiResponse (Record '[ "ok" >: Bool ]))
kick client cid uid =
  buildPostApi "kick" client ("channel" =: cid <> "user" =: uid)

leave
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> m (SlackApiResponse (Record '[ "not_in_channel" >: Maybe Bool ]))
leave client cid = buildPostApi "leave" client ("channel" =: cid)

type Conversations = Record
  '[ "channels"          >: [Slack.Conversation]
   , "response_metadata" >: Record NextCursor
   ]

type ListParams = OptionalParams
  '[ "cursor"           >: Text
   , "limit"            >: Int
   , "exclude_archived" >: Bool
   , "types"            >: [Slack.ChannelType]
   ]

list
  :: (MonadHttp m, Client c)
  => c -> ListParams -> m (SlackApiResponse Conversations)
list client = buildGetApi "list" client . buildRequestParams

type Members = Record
  '[ "members"           >: [Slack.UserID]
   , "response_metadata" >: Record NextCursor
   ]

type MembersParams = OptionalParams
  '[ "cursor" >: Text
   , "limit"  >: Int
   ]

members
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> MembersParams
  -> m (SlackApiResponse Members)
members client cid =
  buildGetApi "members" client . ("channel" =: cid <>) . buildRequestParams

type OpenResult = Record
  '[ "no_op"        >: Maybe Bool
   , "already_open" >: Maybe Bool
   , "channel"      >: Slack.Channel
   ]

type OpenParams = OptionalParams
  '[ "channel"    >: Text
   , "return_im" >: Bool
   , "users"     >: [Slack.UserID]
   ]

open
  :: (MonadHttp m, Client c)
  => c
  -> OpenParams
  -> m (SlackApiResponse OpenResult)
open client = buildPostApi "open" client . buildRequestParams

rename
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> Text
  -> m (SlackApiResponse (Record '[ "channel" >: Slack.Conversation ]))
rename client cid name =
  buildPostApi "rename" client ("channel" =: cid <> "name" =: name)

type Replies = Record
  '[ "messages"          >: [Slack.Message]
   , "has_more"          >: Bool
   , "response_metadata" >: Record NextCursor
   ]

type RepliesParams = HistoryParams

replies
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> Slack.TimeStamp
  -> RepliesParams
  -> m (SlackApiResponse Replies)
replies client cid ts =
  buildGetApi "replies" client .
    ("channel" =: cid <>) . ("ts" =: ts <>) . buildRequestParams

setPurpose
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> Text
  -> m (SlackApiResponse (Record '[ "purpose" >: Text ]))
setPurpose client cid purpose =
  buildPostApi "setPurpose" client ("channel" =: cid <> "purpose" =: purpose)

setTopic
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> Text
  -> m (SlackApiResponse (Record '[ "topic" >: Text ]))
setTopic client cid topic =
  buildPostApi "setTopic" client ("channel" =: cid <> "topic" =: topic)

unarchive
  :: (MonadHttp m, Client c)
  => c
  -> Slack.ChannelID
  -> m (SlackApiResponse (Record '[ "ok" >: Bool ]))
unarchive client cid = buildPostApi "unarchive" client ("channel" =: cid)

-- | API for only public and private channels

create'
  :: (MonadHttp m, Client c)
  => c
  -> Text -- ^ channel name
  -> CreateParams
  -> m (SlackApiResponse (Record '[ "channel" >: Slack.Channel ]))
create' client cname =
  buildPostApi "create" client . ("name" =: cname <>) . buildRequestParams

type Channels = Record
  '[ "channels"          >: [Slack.Channel]
   , "response_metadata" >: Record NextCursor
   ]

list'
  :: (MonadHttp m, Client c)
  => c -> ListParams -> m (SlackApiResponse Channels)
list' client = buildGetApi "list" client . buildRequestParams . onlyChannel

onlyChannel
  :: Lookup xs "types" [Slack.ChannelType]
  => OptionalParams xs -> OptionalParams xs
onlyChannel params =
  hmap liftNullable (hmap unliftNullable params & #types .~ wrap val)
  where
    val = Just [Slack.PublicChannel, Slack.PrivateChannel]

liftNullable :: Wrapper h => Field (Nullable h) x -> Nullable (Field h) x
liftNullable = wrap . unwrap

unliftNullable :: Wrapper h => Nullable (Field h) x -> Field (Nullable h) x
unliftNullable = wrap . unwrap
