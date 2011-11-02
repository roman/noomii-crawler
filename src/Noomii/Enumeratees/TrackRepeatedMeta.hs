module Noomii.Enumeratees.TrackRepeatedMeta where

----------

import Data.Enumerator

----------

import Crawler.Enumerator
import Crawler.Types
import Navigation.Types

-------------------------------------------------------------------------------

class Monad m => MetaTrackerMonad m where
  trackRepeatedMetaTags :: WebPage -> m ()

-------------------------------------------------------------------------------

trackRepeatedMeta :: MetaTrackerMonad m
                  => Enumeratee (NavEvent CrawlNode)
                                (NavEvent CrawlNode)
                                m
                                b
trackRepeatedMeta step@(Continue consumer) = continue go
  where
    go stream@(Chunks xs) = Iteratee $ do
      mapM_ (fn . nvVal) xs
      runIteratee $ consumer stream >>==
                    trackRepeatedMeta
    go EOF = yield step EOF
    fn (CrawlWebPage wp) = trackRepeatedMetaTags wp
    fn (CrawlLink _) = return ()

trackRepeatedMeta step = yield step EOF

