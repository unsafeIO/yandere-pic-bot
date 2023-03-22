{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Yandere where

import Data.Int (Int32)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Servant.API
import Servant.Client
import Types

type GetYanderePosts =
  "post.json"
    :> QueryParam "tags" Text
    :> QueryParam "limit" Int
    :> QueryParam "page" Int
    :> Get '[JSON] [YanderePost]

getYanderePosts :: Maybe Text -> Maybe Int -> Maybe Int -> ClientM [YanderePost]
getYanderePosts = client (Proxy @GetYanderePosts)

getYanderePostsBefore :: Text -> Maybe Int32 -> ClientM [YanderePost]
getYanderePostsBefore tags postId =
  go
    ( \xs ->
        maybe False (`elem` (_postId <$> xs)) postId
    )
    1
    []
  where
    go f n acc =
      if f acc
        then pure acc
        else do
          posts <- getYanderePosts (Just tags) Nothing (Just n)
          if null posts
            then pure acc
            else go f (n + 1) $ acc <> posts
