{-# LANGUAGE TemplateHaskell #-}
module Noomii.Types where

----------

import Data.ByteString.Char8 (ByteString)
import Data.Maybe (isNothing)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Time.Clock (NominalDiffTime)

----------

import Data.Lens.Template (makeLenses)


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
    _titleMap         :: Map ByteString [String]
  , _metaMap          :: Map ByteString [String]
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

