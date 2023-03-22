{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SendPic where

import BotDb
import Control.Monad.Reader
import Data.ByteString (ByteString, toStrict)
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Client
import Telegram.Bot.API
import Types

downloadPic :: YanderePost -> EnvM ByteString
downloadPic post = do
  manager <- asks envManager
  result <- liftIO $ httpLbs (parseRequest_ $ T.unpack $ _postJpegUrl post) manager
  pure $ toStrict $ responseBody result

sendPic :: YanderePost -> EnvM ()
sendPic post = do
  username <- asks envChatUsername
  bs <- downloadPic post
  let fp :: FilePath = "tmp/azuuru.jpg"
  liftIO $ BS.writeFile fp bs
  let postUrl = "https://yande.re/post/show/" <> T.pack (show $ _postId post)
      tags = T.intercalate " " $ map (("\\#" <>) . escapeTg) $ T.words $ _postTags post
      req =
        SendPhotoRequest
          { sendPhotoChatId = SomeChatUsername username,
            sendPhotoMessageThreadId = Nothing,
            sendPhotoPhoto = MakePhotoFile $ InputFile fp "image/jpeg",
            sendPhotoThumb = Nothing,
            sendPhotoCaption = Just $ tags <> " [src](" <> postUrl <> ")",
            sendPhotoParseMode = Just MarkdownV2,
            sendPhotoCaptionEntities = Nothing,
            sendPhotoHasSpoiler = Nothing,
            sendPhotoDisableNotification = Nothing,
            sendPhotoProtectContent = Nothing,
            sendPhotoReplyToMessageId = Nothing,
            sendPhotoAllowSendingWithoutReply = Nothing,
            sendPhotoReplyMarkup = Nothing
          }
  liftIO $ T.putStrLn $ "Sending " <> postUrl
  result <- liftTg $ sendPhoto req
  if responseOk result
    then do
      let Message {..} = responseResult result
          tgMsg :: TGMessage =
            TGMessage
              { _messageId = fromInteger . coerce $ messageMessageId,
                _messageChatId = fromInteger . coerce . chatId $ messageChat,
                _messagePost = PostId $ _postId post
              }
      liftIO $ T.putStrLn $ "Sent " <> postUrl
      liftSqlite $ insertMessage tgMsg
      liftIO $ T.putStrLn $ "Saved " <> postUrl
    else fail $ show result

escapeTg :: Text -> Text
escapeTg =
  T.replace "(" "\\("
    . T.replace ")" "\\)"
    . T.replace "_" "\\_"
    . T.replace "*" "\\*"
    . T.replace "~" "\\~"
    . T.replace "`" "\\`"
    . T.replace ">" "\\>"
    . T.replace "<" "\\<"
    . T.replace "#" "\\#"
    . T.replace "+" "\\+"
    . T.replace "-" "\\-"
    . T.replace "=" "\\="
    . T.replace "|" "\\|"
    . T.replace "." "\\."
    . T.replace "!" "\\!"
    . T.replace "[" "\\["
    . T.replace "]" "\\]"
    . T.replace "{" "\\{"
    . T.replace "}" "\\}"
