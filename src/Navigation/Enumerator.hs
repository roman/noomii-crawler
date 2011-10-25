module Navigation.Enumerator (
    enumNavigation
  , removeAlreadyVisited
  , debugFrontier
  , debugVisitedSet
  , debugVisitNumbered
  , module Navigation.Types
  ) where

--------------------
-- Standard

import Control.Monad (liftM, zipWithM_)
import Control.Monad.Trans (MonadIO, liftIO)
import System.IO (Handle, hPrint, hPutStrLn)

import qualified Data.Set as Set

--------------------
-- Third Party

import Data.Enumerator hiding (map, mapM, repeat, length)

import qualified Data.Enumerator.List as EL

--------------------
-- Local

import Navigation.Types

-------------------------------------------------------------------------------

enumNavigation :: (Ord a, Monad m)
               => (a -> a -> m Integer)
               -> (a -> m (a, [a]))
               -> a
               -> Enumerator (NavEvent a) m b
enumNavigation costFn actionsFn zero =
    go (Set.singleton (0, zero, Nothing))
       Set.empty
  where
    go frontier0 visited0 step@(Continue consumer) = Iteratee $
      case Set.minView frontier0 of
        Nothing -> return step
        Just ((cost, node, parent), frontier1)
          | Set.member node visited0 -> do

            let event = NavEvent node
                                 parent
                                 cost
                                 True
                                 visited0
                                 frontier1

            runIteratee $ consumer (Chunks [event]) >>==
                          go frontier1 visited0


          | otherwise -> do

            (node1, children0) <- actionsFn node
            childrenCosts <- mapM (((+cost) `liftM`) . costFn node1)
                                  children0
            let children = Set.fromList $ zip3 childrenCosts
                                               children0
                                               (repeat (Just node))
            let frontier = frontier0 `Set.union` children
            let visited  = Set.insert node visited0

            let event = NavEvent node1
                                 parent
                                 cost
                                 False
                                 visited
                                 frontier

            runIteratee $ consumer (Chunks [event]) >>==
                          go frontier visited

    go _ _ step = returnI step

--------------------

removeAlreadyVisited :: Monad m => Enumeratee (NavEvent a) (NavEvent a) m b
removeAlreadyVisited = EL.filter (not . nvAlreadyVisited)

--------------------

debugVisitNumbered :: (Show a, MonadIO m)
                   => Handle
                   -> Enumeratee (NavEvent a) (NavEvent a) m b
debugVisitNumbered handle = helper 0
  where
    visitNavEvent n entry = 
        liftIO . hPutStrLn handle $ show n ++ "| " ++ show entry

    helper acc step@(Continue consumer) = continue go
      where
        go EOF = yield step EOF
        go stream@(Chunks xs) = Iteratee $ do
            zipWithM_ visitNavEvent [acc..] xs
            runIteratee $ consumer stream >>== 
                          helper (acc + length xs)

    helper _ step = yield step EOF


--------------------

debugFrontier :: (Show a, MonadIO m)
              => Handle
              -> Enumeratee (NavEvent a) (NavEvent a) m b
debugFrontier handle = EL.mapM showFrontier
  where
    showFrontier nv = do
      liftIO $ hPutStrLn handle $ 
                "======= frontier [" 
                ++ show (nvVal nv) 
                ++ "]"
      liftIO $ mapM_ (hPrint handle)
                     (Set.toList $ nvFrontier nv)
      return nv

--------------------

debugVisitedSet :: (Show a, MonadIO m)
                => Handle
                -> Enumeratee (NavEvent a) (NavEvent a) m b
debugVisitedSet handle = EL.mapM showVisited
  where
    showVisited nv = do
      liftIO $ hPutStrLn handle $ 
                "======= visited [" 
                ++ show (nvVal nv) 
                ++ "]"
      liftIO $ mapM_ (hPrint handle) 
                     (Set.toList $ nvVisited nv)
      return nv

