{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Noomii.Monad where

import Control.Monad (when)
import Control.Monad.State.Strict (
    MonadState(..)
  , StateT
  , execStateT
  , modify
  )
import Control.Monad.Trans (MonadIO(..))
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..))

import qualified Data.Map as Map
import qualified Data.Sequence as Seq

----------

import Network.HTTP.Types (Status(..))
import Data.Lens.Common (modL)

----------

import Crawler.HTML
import Crawler.Types
import Noomii.Enumeratees
import Noomii.Types

-------------------------------------------------------------------------------

newtype NoomiiMonad m a
  = NoomiiMonad {
    fromNoomiiMonad :: StateT NoomiiState m a
  }
  deriving (Monad, MonadIO, MonadState NoomiiState)

----------

instance Monad m => StatsTrackerMonad (NoomiiMonad m) where
  trackPerformance wp = NoomiiMonad $
      modify $ modL performanceStats
                    (`mappend` perfStat)
    where
      perfPair = (wpPerf wp, wpURL wp)
      perfStat = PerformanceStat (MinPerformance perfPair)
                                 (MaxPerformance perfPair)

----------

instance Monad m => MetaTrackerMonad (NoomiiMonad m) where
  trackRepeatedMetaTags wp = NoomiiMonad $
      case metaText of
        Nothing -> return ()
        Just meta -> do
          modify $ modL metaMap
                        (Map.alter alterFn meta)
    where
      metaText = listToMaybe .
                 take 1 .
                 map (getAttrFromWholeTag "content") .
                 filter (("description" ==) .
                         getAttrFromWholeTag "name") $
                 wholeTags "meta" (wpBody wp)
      alterFn Nothing = Just $ Seq.singleton (wpParentURL wp, wpURL wp)
      alterFn val = ((wpParentURL wp, wpURL wp) Seq.<|) `fmap` val

----------

instance Monad m => TitleTrackerMonad (NoomiiMonad m) where
  trackRepeatedTitleTags wp = NoomiiMonad $
      case titleText of
        Nothing -> return ()
        Just title ->
          modify $ modL titleMap
                        (Map.alter alterFn title)
    where
      titleText = listToMaybe $
                  concatMap getTextFromWholeTag $
                  Prelude.take 1 $
                  wholeTags "title" (wpBody wp)
      alterFn Nothing = Just $ Seq.singleton (wpParentURL wp, wpURL wp)
      alterFn val = ((wpParentURL wp, wpURL wp) Seq.<|) `fmap` val

----------

instance Monad m => ErrorTrackerMonad (NoomiiMonad m) where
  trackErrors wp = NoomiiMonad $
      when failedWebPage $
            modify $ 
            modL errorMap 
                 (Map.alter alterFn (wpParentURL wp))
    where
      failedWebPage = (statusCode (wpStatusCode wp)) /= 200
      alterFn Nothing = Just $ Seq.singleton (wpStatusCode wp, wpURL wp)
      alterFn val = ((wpStatusCode wp, wpURL wp)  Seq.<|) `fmap` val
      

----------

execNoomiiMonad :: Monad m => NoomiiMonad m a -> m NoomiiState
execNoomiiMonad (NoomiiMonad m) = execStateT m mempty


