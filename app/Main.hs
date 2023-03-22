{-# LANGUAGE OverloadedStrings #-}

module Main where

import BotDb
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import SendPic
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
  (env, purge) <- newEnv db (Token token) chat
  runEnv env $ do
    maxId <- liftSqlite getPostWithMaxId
    posts <- liftYandere $ getYanderePostsBefore tag (lastId <|> _postId <$> maxId)
    liftSqlite $ insertPosts posts
    unsendPosts <- liftSqlite unsendPosts
    forM_ unsendPosts $ \post -> do
      sendPic post
      liftIO $ threadDelay 3000000
  purge
