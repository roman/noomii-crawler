module Noomii.Enumeratees.TrackErrorPages where

--------------------
import Data.Enumerator

--------------------
import Crawler.Enumerator
import Crawler.Types
import Navigation.Types

-------------------------------------------------------------------------------

class Monad m => ErrorTrackerMonad m where
  trackErrors :: WebPage -> m ()

-------------------------------------------------------------------------------

trackErrorStats :: ErrorTrackerMonad m
                => Enumeratee (NavEvent CrawlNode)
                              (NavEvent CrawlNode)
                              m
                              b
trackErrorStats step@(Continue consumer) = continue go
  where
    go stream@(Chunks xs) = Iteratee $ do
      mapM_ (fn . nvVal) xs
      runIteratee $ consumer stream >>==
                    trackErrorStats
    go EOF = yield step EOF
    fn (CrawlWebPage wp) = trackErrors wp
    fn (CrawlLink _) = return ()

trackErrorStats step = yield step EOF


