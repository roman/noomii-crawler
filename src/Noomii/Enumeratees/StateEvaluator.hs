module Noomii.Enumeratees.StateEvaluator where

import Control.Monad.Trans (MonadIO(..))
import Control.Monad.State (MonadState(..))
import System.IO (
    IOMode(WriteMode)
  , openFile
  , hClose
  , hPrint
  )

--------------------
import Data.Enumerator (
    Iteratee(..)
  , Step(..)
  , Stream(..)
  , (>>==)
  , Enumeratee
  , continue
  )

-------------------------------------------------------------------------------
evaluateState :: (MonadIO m, MonadState s m, Show s)
              => Enumeratee a a m b
evaluateState step0 = do
    handle <- liftIO $ openFile "/dev/null" WriteMode
    helper handle step0
  where
    helper handle (Continue consumer) = Iteratee $ do
        get >>= liftIO . hPrint handle
        runIteratee $ continue go
      where
        go stream = consumer stream >>== helper handle
    helper handle step = Iteratee $ do
        liftIO $ hClose handle
        return $ Yield step EOF

