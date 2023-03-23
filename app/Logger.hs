module Logger where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (stderr)
import System.Log.Formatter
import System.Log.Handler
import System.Log.Handler.Simple
import System.Log.Logger

logInfo :: (MonadIO m) => Text -> m ()
logInfo = liftIO . infoM "bot" . T.unpack

setUpLogger :: (MonadIO m) => m ()
setUpLogger = liftIO $ do
  h <- flip setFormatter (simpleLogFormatter "[$time : $loggername : $prio] $msg") <$> streamHandler stderr DEBUG
  updateGlobalLogger rootLoggerName removeHandler
  updateGlobalLogger "bot" (System.Log.Logger.setLevel DEBUG . setHandlers [h])
