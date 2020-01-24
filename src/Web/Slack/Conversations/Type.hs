module Web.Slack.Conversations.Type
    ( Ok (..)
    , ChannelID
    , UserID
    , TimeStamp
    , Conversation
    , Channel
    , ChannelTopic
    , ChannelType (..)
    , Message
    , Reply
    ) where

import           Data.Aeson                           (FromJSON (..),
                                                       ToJSON (..), (.:))
import qualified Data.Aeson                           as J
import           Data.Extensible
import qualified Data.HashMap.Strict                  as HM
import           Data.Int                             (Int64)
import           Data.Text                            (Text)
import           Web.Slack.Conversations.API.Internal (ToHttpApiData' (..))

data Ok a = Ok a | Err Text
  deriving (Show, Eq)

instance FromJSON a => FromJSON (Ok a) where
  parseJSON = J.withObject "Result e a" $ \obj -> case HM.lookup "ok" obj of
    Just (J.Bool True)  -> Ok <$> parseJSON (J.Object obj)
    Just (J.Bool False) -> Err <$> obj .: "error"
    _                   -> fail "key `ok:bool` is not found."

instance ToJSON a => ToJSON (Ok a) where
  toJSON (Ok a) = case toJSON a of
    J.Object obj -> J.Object $ HM.insert "ok" (J.Bool True) obj
    value        -> J.Object $ HM.fromList [("ok", J.Bool True), ("value", value)]
  toJSON (Err e)  = J.Object $ HM.fromList [("ok", J.Bool False), ("error", J.String e)]

type ChannelID = Text
type UserID = Text

type TimeStamp = Text

type Conversation = Record
  '[ "id"                    >: ChannelID
   , "name"                  >: Maybe Text
   , "is_channel"            >: Maybe Bool
   , "is_group"              >: Maybe Bool
   , "is_im"                 >: Bool
   , "created"               >: Int64
   , "creator"               >: Maybe UserID
   , "is_archived"           >: Maybe Bool
   , "is_general"            >: Maybe Bool
   , "unlinked"              >: Maybe Int
   , "name_normalized"       >: Maybe Text
   , "is_shared"             >: Maybe Bool
   , "is_ext_shared"         >: Maybe Bool
   , "is_org_shared"         >: Bool
   , "pending_shared"        >: Maybe [Text]
   , "is_pending_ext_shared" >: Maybe Bool
   , "is_member"             >: Maybe Bool
   , "is_private"            >: Maybe Bool
   , "is_mpim"               >: Maybe Bool
   , "topic"                 >: Maybe ChannelTopic
   , "purpose"               >: Maybe ChannelTopic
   , "previous_names"        >: Maybe [Text]
   , "num_members"           >: Maybe Int
   , "user"                  >: Maybe UserID
   , "is_user_deleted"       >: Maybe Bool
   , "priority"              >: Maybe Int
   , "locale"                >: Maybe Text
   ]

type ChannelTopic = Record
  '[ "value"    >: Text
   , "creator"  >: UserID
   , "last_set" >: Int64
   ]

data ChannelType
    = PublicChannel
    | PrivateChannel
    | Mpim
    | Im
    deriving (Show, Eq)

instance ToHttpApiData' ChannelType where
  toQueryParam' PublicChannel  = Just "public_channel"
  toQueryParam' PrivateChannel = Just "private_channel"
  toQueryParam' Mpim           = Just "mpim"
  toQueryParam' Im             = Just "im"

type Channel = Record
  '[ "id"                    >: ChannelID
   , "name"                  >: Text
   , "created"               >: Int64
   , "creator"               >: UserID
   , "is_archived"           >: Bool
   , "is_general"            >: Bool
   , "unlinked"              >: Int
   , "name_normalized"       >: Text
   , "is_shared"             >: Bool
   , "is_ext_shared"         >: Bool
   , "is_org_shared"         >: Bool
   , "pending_shared"        >: [Text]
   , "is_pending_ext_shared" >: Bool
   , "is_member"             >: Bool
   , "is_private"            >: Bool
   , "topic"                 >: ChannelTopic
   , "purpose"               >: ChannelTopic
   , "previous_names"        >: Maybe [Text]
   , "num_members"           >: Maybe Int
   ]

type Message = Record
  '[ "type"           >: Text
   , "user"           >: UserID
   , "text"           >: Text
   , "thread_ts"      >: Maybe TimeStamp
   , "parent_user_id" >: Maybe UserID
   , "reply_count"    >: Maybe Int
   , "replies"        >: Maybe [Reply]
   , "subscribed"     >: Maybe Bool
   , "last_read"      >: Maybe TimeStamp
   , "unread_count"   >: Maybe Int
   , "ts"             >: TimeStamp
   ]

type Reply = Record
  '[ "user" >: UserID
   , "ts"   >: TimeStamp
   ]
