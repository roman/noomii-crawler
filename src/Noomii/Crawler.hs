{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Noomii.Crawler where

--------------------
import Prelude hiding ((.))
import Control.Monad (forM_)
import Control.Monad.Trans (MonadIO(..))
import Codec.Compression.GZip (compress)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.ByteString.Lazy (toChunks)
import Data.Map (Map)
import Data.Maybe (isNothing, listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Time.Clock (NominalDiffTime)
import Control.Category ((.))
import Control.Monad.State (StateT, execStateT, modify)
import System.IO (IOMode(WriteMode), Handle, stdout, withFile, hPutStrLn)

import qualified Data.ByteString as BS
import qualified Data.Map as Map

--------------------
import Data.Enumerator (($$), (=$), run_)
import Data.Lens.Common (getL, modL)
import Data.Lens.Template (makeLenses)

import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB
--------------------

import Crawler.Enumerator
import Crawler.HTML
import Crawler.Types
import Navigation.Enumerator
import Noomii.GenerateSitemap
import Noomii.TrackPerformanceStats
import Noomii.TrackRepeatedMeta
import Noomii.TrackRepeatedTitles

-------------------------------------------------------------------------------

newtype MinPerformance
  = MinPerformance {
    fromMinPerformance :: (Maybe NominalDiffTime, String)
  }
  deriving (Show, Eq, Ord)


instance Monoid MinPerformance where
  mempty = MinPerformance (Nothing, "")
  mappend p1@(MinPerformance (t1, _))
          p2@(MinPerformance (t2, _))
    | isNothing t1  = p2
    | isNothing t2 = p1
    | otherwise = min p1 p2

--------------------

newtype MaxPerformance
  = MaxPerformance {
    fromMaxPerformance :: (Maybe NominalDiffTime, String)
  }
  deriving (Show, Eq, Ord)

instance Monoid MaxPerformance where
  mempty = MaxPerformance (Nothing, "")
  mappend = max

--------------------

data PerformanceStat
  = PerformanceStat {
    _minPerformance :: MinPerformance
  , _maxPerformance :: MaxPerformance
  }
  deriving (Show)

instance Monoid PerformanceStat where
  mempty = PerformanceStat mempty mempty
  mappend (PerformanceStat min1 max1)
          (PerformanceStat min2 max2)
      = PerformanceStat (min1 `mappend` min2)
                        (max1 `mappend` max2)

makeLenses [''PerformanceStat]

--------------------

data NoomiiState
  = NoomiiState {
    _titleMap         :: Map ByteString [String]
  , _metaMap          :: Map ByteString [String]
  , _performanceStats :: PerformanceStat
  }
  deriving (Show)

instance Monoid NoomiiState where
  mempty = NoomiiState mempty mempty mempty
  mappend (NoomiiState a1 b1 c1)
          (NoomiiState a2 b2 c2)
      = NoomiiState (a1 `mappend` a2)
                    (b1 `mappend` b2)
                    (c1 `mappend` c2)

makeLenses [''NoomiiState]

--------------------

newtype NoomiiMonad m a
  = NoomiiMonad {
    fromNoomiiMonad :: StateT NoomiiState m a
  }
  deriving (Monad)

----------

instance MonadIO m => MonadIO (NoomiiMonad m) where
  liftIO = NoomiiMonad . liftIO

----------

instance Monad m => StatsTrackerMonad (NoomiiMonad m) where
  trackPerformance wp = NoomiiMonad $
      modify $ modL performanceStats
                    (mappend perfStat)
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
                 concatMap getTextFromWholeTag $
                 take 1 $
                 wholeTags "meta" (wpBody wp)
      alterFn Nothing = Just [wpURL wp]
      alterFn val = ([wpURL wp] ++) `fmap` val

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
                  take 1 $
                  wholeTags "title" (wpBody wp)
      alterFn Nothing = Just [wpURL wp]
      alterFn val = ([wpURL wp] ++) `fmap` val

----------

execNoomiiMonad :: Monad m => NoomiiMonad m a -> m NoomiiState
execNoomiiMonad (NoomiiMonad m) = execStateT m mempty

-------------------------------------------------------------------------------

printStats :: MonadIO m => Handle -> NoomiiState -> m ()
printStats handle state = do
    let (minperf, urlmin) = fromMinPerformance $
                            getL (minPerformance . performanceStats)
                                 state

    let (maxperf, urlmax) = fromMaxPerformance $
                            getL (maxPerformance . performanceStats)
                                 state

    let repeatedTitles = Map.filter ((> 1) . length) $
                         getL titleMap state

    let repeatedMeta = Map.filter ((> 1) . length) $
                       getL metaMap state

    liftIO $ do
      hPutStrLn handle "========="
      hPutStrLn handle "Webpage with min. response time:"
      hPutStrLn handle $ urlmin ++ " (" ++ show minperf ++ ")"
      hPutStrLn handle "---"
      hPutStrLn handle "Webpage with max. response time:"
      hPutStrLn handle $ urlmax ++ " (" ++ show maxperf ++ ")"

      hPutStrLn handle "========="
      hPutStrLn handle "Webpages with repeated title:"
      hPutStrLn handle ""
      forM_ (Map.toList repeatedTitles) $ \(title, urls) -> do
        hPutStrLn handle "Title:"
        hPutStrLn handle (unpack title)
        forM_ urls $ \(url) -> 
          hPutStrLn handle $ "- " ++ url
        hPutStrLn handle "---"

      hPutStrLn handle "========="
      hPutStrLn handle "Webpages with repeated meta:"
      hPutStrLn handle ""
      forM_ (Map.toList repeatedMeta) $ \(meta, urls) -> do
        hPutStrLn handle "Meta:" 
        hPutStrLn handle (unpack meta)
        forM_ urls $ \(url) -> 
          hPutStrLn handle $ "- " ++ url
        hPutStrLn handle "---"

--------------------

crawlNoomii :: MonadIO m => String -> m NoomiiState
crawlNoomii env =
    liftIO $
    withFile "out/sitemap.xml.gz" WriteMode $ \handle ->
      execNoomiiMonad $
        run_ $
          enumCrawler domain regexp     $$
          removeAlreadyVisited          =$
          EL.isolate 500                =$
          debugVisitNumbered stdout     =$
          removeBrokenWebPages          =$
          trackPerformanceStats         =$
          --trackRepeatedMeta             =$
          --trackRepeatedTitles           =$
          generateSitemap               =$
          EL.map compress               =$
          EL.map (BS.concat . toChunks) =$
          EB.iterHandle handle
  where
    regexp = "https?://.*\\.noomii\\.com/.*"
    domain
      | env == "production" = "http://www.noomii.com/"
      | otherwise = "http://" ++ env ++ ".noomii.com/"

