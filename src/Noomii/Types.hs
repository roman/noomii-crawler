{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Noomii.Types where

----------

import Data.ByteString.Char8 (ByteString)
import Data.Maybe (isNothing, fromMaybe)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Monoid (Monoid(..))
import Data.Time.Clock (NominalDiffTime)

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
----------

import Data.Lens.Template (makeLenses)
import Data.Lens.Common (getL)

class Pretty a where
  prettyShow :: a -> String

-------------------------------------------------------------------------------

newtype MinPerformance
  = MinPerformance {
    fromMinPerformance :: (Maybe NominalDiffTime, String)
  }
  deriving (Show, Eq, Ord)

----------

instance Monoid MinPerformance where
  mempty = MinPerformance (Nothing, "")
  mappend p1@(MinPerformance (t1, _))
          p2@(MinPerformance (t2, _))
    | isNothing t1 = p2
    | isNothing t2 = p1
    | otherwise    = min p1 p2

instance Pretty MinPerformance where
  prettyShow (MinPerformance (Nothing, url)) =
      url ++ " (No performance available)"
  prettyShow (MinPerformance (Just perf, url)) =
      url ++ " (" ++ show perf ++ ")"

-------------------------------------------------------------------------------

newtype MaxPerformance
  = MaxPerformance {
    fromMaxPerformance :: (Maybe NominalDiffTime, String)
  }
  deriving (Show, Eq, Ord)

----------

instance Monoid MaxPerformance where
  mempty  = MaxPerformance (Nothing, "")
  mappend = max

instance Pretty MaxPerformance where
  prettyShow (MaxPerformance (Nothing, url)) =
      url ++ " (No performance available)"
  prettyShow (MaxPerformance (Just perf, url)) =
      url ++ " (" ++ show perf ++ ")"

-------------------------------------------------------------------------------

data PerformanceStat
  = PerformanceStat {
    _minPerformance :: MinPerformance
  , _maxPerformance :: MaxPerformance
  }
  deriving (Show)

makeLenses [''PerformanceStat]

----------

instance Monoid PerformanceStat where
  mempty = PerformanceStat mempty mempty
  mappend (PerformanceStat min1 max1)
          (PerformanceStat min2 max2)
      = PerformanceStat (min1 `mappend` min2)
                        (max1 `mappend` max2)


-------------------------------------------------------------------------------

data NoomiiState
  = NoomiiState {
    _titleMap         :: Map ByteString (Seq (String, String))
  , _metaMap          :: Map ByteString (Seq (String, String))
  , _performanceStats :: PerformanceStat
  }
  deriving (Show)

makeLenses [''NoomiiState]

----------

instance Monoid NoomiiState where
  mempty = NoomiiState mempty mempty mempty
  mappend (NoomiiState a1 b1 c1)
          (NoomiiState a2 b2 c2)
      = NoomiiState (a1 `mappend` a2)
                    (b1 `mappend` b2)
                    (c1 `mappend` c2)

--------------------

splitNoTitleUrls :: NoomiiState
                 -> ( Seq (String, String)
                    , Map ByteString (Seq (String, String)) )
splitNoTitleUrls noomiiState =
    fromMaybe (Seq.empty, titles) $ do
      noTitleUrl <- mNoTitleUrl
      return (noTitleUrl, withoutNoTitle)
  where
    titles = getL titleMap noomiiState
    mNoTitleUrl = Map.lookup "" titles
    withoutNoTitle = Map.delete "" titles

--------------------

splitNoMetaUrls :: NoomiiState
                 -> ( Seq (String, String)
                    , Map ByteString (Seq (String, String)) )
splitNoMetaUrls noomiiState =
    fromMaybe (Seq.empty, metaList) $ do
      noMetaUrl <- mNoMetaUrl
      return (noMetaUrl, withoutNoMeta)
  where
    metaList = getL metaMap noomiiState
    mNoMetaUrl = Map.lookup "" metaList
    withoutNoMeta = Map.delete "" metaList

