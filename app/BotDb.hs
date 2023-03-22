{-# LANGUAGE OverloadedStrings #-}

module BotDb where

import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Types

botDb :: DatabaseSettings db BotDb
botDb = defaultDbSettings

getPostWithMaxId :: SqliteM (Maybe YanderePost)
getPostWithMaxId =
  runSelectReturningOne $
    select $
      limit_ 1 $
        orderBy_ (desc_ . _postId) $
          all_ (_posts botDb)

insertPosts :: [YanderePost] -> SqliteM ()
insertPosts posts =
  runInsert $
    insertOnConflict
      (_posts botDb)
      (insertValues posts)
      anyConflict
      onConflictDoNothing

insertMessage :: TGMessage -> SqliteM ()
insertMessage msg =
  runInsert $
    insertOnConflict
      (_messages botDb)
      (insertValues [msg])
      anyConflict
      onConflictDoNothing

unsendPosts :: SqliteM [YanderePost]
unsendPosts = do
  all <- runSelectReturningList $ select $ all_ (_posts botDb)
  msg <- runSelectReturningList $ select $ all_ (_messages botDb)
  pure $
    filter
      ( \x ->
          not $
            any
              ( \y ->
                  let unPostId (PostId k) = k
                   in unPostId (_messagePost y) == _postId x
              )
              msg
      )
      all


createTables :: Connection -> IO ()
createTables conn = do
  execute_ conn "CREATE TABLE posts      (          id          INTEGER PRIMARY KEY,          tags        VARCHAR NOT NULL,          file_url    VARCHAR NOT NULL,          sample_url  VARCHAR NOT NULL,          jpeg_url    VARCHAR NOT NULL,          preview_url VARCHAR NOT NULL,          width       INTEGER NOT NULL,          height      INTEGER NOT NULL      )"
  execute_ conn "CREATE TABLE messages      (          id       INTEGER PRIMARY KEY,          chat_id  INTEGER NOT NULL,          post__id INTEGER NOT NULL      )"
