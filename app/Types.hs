{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Control.Exception (throw)
import Control.Monad.Reader
import qualified Data.Aeson as A
import Data.Int
import Data.Text (Text)
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.Client
import Telegram.Bot.API (Token, botBaseUrl)

data YanderePostT f = YanderePost
  { _postId :: C f Int32,
    _postTags :: C f Text,
    _postFileUrl :: C f Text,
    _postSampleUrl :: C f Text,
    _postJpegUrl :: C f Text,
    _postPreviewUrl :: C f Text,
    _postWidth :: C f Int32,
    _postHeight :: C f Int32
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

type YanderePost = YanderePostT Identity

deriving instance Show YanderePost

deriving instance Show (PrimaryKey YanderePostT Identity)

instance A.FromJSON YanderePost where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions
        { A.fieldLabelModifier = A.camelTo2 '_' . drop 5
        }

instance Table YanderePostT where
  data PrimaryKey YanderePostT f = PostId (C f Int32)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = PostId . _postId

data TGMessageT f = TGMessage
  { _messageId :: C f Int64,
    _messageChatId :: C f Int64,
    _messagePost :: PrimaryKey YanderePostT f
  }
  deriving stock (Generic)
  deriving anyclass (Beamable)

type TGMessage = TGMessageT Identity

deriving instance Show TGMessage

deriving instance Show (PrimaryKey TGMessageT Identity)

instance Table TGMessageT where
  data PrimaryKey TGMessageT f = MessageId (C f Int64)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = MessageId . _messageId

data BotDb f = BotDb
  { _posts :: f (TableEntity YanderePostT),
    _messages :: f (TableEntity TGMessageT)
  }
  deriving stock (Generic)
  deriving anyclass (Database be)

data Env = Env
  { envDb :: Connection,
    envTgClientEnv :: ClientEnv,
    envYandereClientEnv :: ClientEnv,
    envChatUsername :: Text,
    envManager :: Manager
  }
  deriving stock (Generic)

newtype EnvM a = EnvM
  { unEnvM :: ReaderT Env IO a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Env,
      MonadFail
    )

liftSqlite :: SqliteM a -> EnvM a
liftSqlite s = EnvM . ReaderT $ \env ->
  runReaderT (runSqliteM s) (\_ -> pure (), envDb env)

liftYandere :: ClientM a -> EnvM a
liftYandere m = EnvM . ReaderT $ \env ->
  runClientM m (envYandereClientEnv env) >>= either throw pure

liftTg :: ClientM a -> EnvM a
liftTg m = EnvM . ReaderT $ \env ->
  runClientM m (envTgClientEnv env) >>= either throw pure

newEnv :: FilePath -> Token -> Text -> IO (Env, IO ())
newEnv dbPath token envChatUsername = do
  envDb <- open dbPath
  envManager <- newTlsManager
  let envTgClientEnv = mkClientEnv envManager $ botBaseUrl token
      envYandereClientEnv = mkClientEnv envManager (BaseUrl Https "yande.re" 443 "")
  pure (Env {..}, close envDb)

runEnv :: Env -> EnvM a -> IO a
runEnv env = flip runReaderT env . unEnvM
