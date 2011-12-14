{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Noomii.Monad where

import Control.Monad.State.Strict (
    MonadState(..)
  , StateT
  , execStateT
  , modify
  )
import Control.Monad.Trans (MonadIO(..))
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..))

import Data.Sequence as Seq
import qualified Data.Map as Map

----------

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
        Just meta ->
          modify $ modL metaMap
                        (Map.alter alterFn meta)
    where
      metaText = listToMaybe $
                 map (getAttrFromWholeTag "description") $
                 Prelude.take 1 $
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

execNoomiiMonad :: Monad m => NoomiiMonad m a -> m NoomiiState
execNoomiiMonad (NoomiiMonad m) = execStateT m mempty


