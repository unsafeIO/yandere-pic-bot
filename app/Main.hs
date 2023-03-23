{-# LANGUAGE OverloadedStrings #-}

module Main where

import BotDb
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay, yield)
import Control.Exception (bracket)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Logger
import SendPic
import System.Cron
import System.Environment (getEnv, lookupEnv)
import Telegram.Bot.API (Token (..))
import Types
import Yandere

main :: IO ()
main = do
  token <- T.pack <$> getEnv "TELEGRAM_TOKEN"
  db <- getEnv "DB_PATH"
  chat <- T.pack <$> getEnv "CHAT_USERNAME"
  lastId <- fmap read <$> lookupEnv "LAST_ID"
  tag <- T.pack <$> getEnv "TAG"
  setUpLogger
  bracket (newEnv db (Token token) chat) snd $ \(env, _) -> do
    _ <- execSchedule $ addJob (run env tag lastId) "0 * * * *"
    forever yield

run :: Env -> Text -> Maybe Int32 -> IO ()
run env tag lastId = runEnv env $ do
  logInfo $ "Start running with tag " <> tag <> " and last id " <> T.pack (show lastId)
  maxId <- fmap _postId <$> liftSqlite getPostWithMaxId
  logInfo $ "Max id:" <> T.pack (show maxId)
  posts <- liftYandere $ getYanderePostsBefore tag (lastId <|> maxId)
  logInfo $ "Fetched posts: " <> T.pack (show $ _postId <$> posts)
  liftSqlite $ insertPosts posts
  unsendPosts <- liftSqlite unsendPosts
  logInfo $ "Unsend posts: " <> T.pack (show $ _postId <$> unsendPosts)
  forM_ unsendPosts $ \post -> do
    sendPic post
    liftIO $ threadDelay 3000000
