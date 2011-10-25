module System.Util where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)

trackPerformance :: MonadIO m => m a -> m (NominalDiffTime, a)
trackPerformance action = do
    start  <- liftIO getCurrentTime
    result <- action
    end    <- liftIO getCurrentTime
    return (diffUTCTime end start, result)
