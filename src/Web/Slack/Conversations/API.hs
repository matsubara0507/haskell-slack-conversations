module Web.Slack.Conversations.API
    ( SlackApiResponse
    , OptionalParams
    , buildGetApi
    , buildPostApi
    , NextCursor
    , CreateParams
    , create
    , InfoParams
    , info
    , Conversations
    , ListParams
    , list
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
                                                       buildRequestParams)
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

type CreateParams = OptionalParams
  '[ "is_private" >: Bool
   , "user_ids"   >: [UserID]
   ]

create
  :: (MonadHttp m, Client c)
  => c
  -> Text -- ^ channel name
  -> CreateParams
  -> m (SlackApiResponse (Record '[ "channel" >: Conversation ]))
create client cname =
  buildPostApi "create" client . ("name" =: cname <>) . buildRequestParams

type InfoParams = OptionalParams
  '[ "include_locale"      >: Bool
   , "include_num_members" >: Bool
   ]

info
  :: (MonadHttp m, Client c)
  => c
  -> ChannelID
  -> InfoParams
  -> m (SlackApiResponse (Record '[ "channel" >: Conversation ]))
info client cid =
  buildGetApi "info" client . ("channel" =: cid <>) . buildRequestParams

type Conversations = Record
  '[ "channels"          >: [Slack.Conversation]
   , "response_metadata" >: Record NextCursor
   ]

type ListParams = OptionalParams
  '[ "cursor"           >: Text
   , "limit"            >: Int
   , "exclude_archived" >: Bool
   , "types"            >: [ChannelType]
   ]

list ::
  (MonadHttp m, Client c) => c -> ListParams -> m (SlackApiResponse Conversations)
list client = buildGetApi "list" client . buildRequestParams


-- | API for only public and private channels

create'
  :: (MonadHttp m, Client c)
  => c
  -> Text -- ^ channel name
  -> CreateParams
  -> m (SlackApiResponse (Record '[ "channel" >: Channel ]))
create' client cname =
  buildPostApi "create" client . ("name" =: cname <>) . buildRequestParams

type Channels = Record
  '[ "channels"          >: [Slack.Channel]
   , "response_metadata" >: Record NextCursor
   ]

list' ::
  (MonadHttp m, Client c) => c -> ListParams -> m (SlackApiResponse Channels)
list' client = buildGetApi "list" client . buildRequestParams . onlyChannel

onlyChannel ::
  Lookup xs "types" [ChannelType] => OptionalParams xs -> OptionalParams xs
onlyChannel params =
  hmap liftNullable (hmap unliftNullable params & #types .~ wrap val)
  where
    val = Just [Slack.PublicChannel, Slack.PrivateChannel]

liftNullable :: Wrapper h => Field (Nullable h) x -> Nullable (Field h) x
liftNullable = wrap . unwrap

unliftNullable :: Wrapper h => Nullable (Field h) x -> Field (Nullable h) x
unliftNullable = wrap . unwrap
