module Noomii.TrackPerformanceStats where

--------------------
import Data.Enumerator


--------------------
import Crawler.Enumerator
import Crawler.Types
import Navigation.Types

-------------------------------------------------------------------------------

class Monad m => StatsTrackerMonad m where
  trackPerformance :: WebPage -> m ()

-------------------------------------------------------------------------------

trackPerformanceStats :: StatsTrackerMonad m 
                      => Enumeratee (NavEvent CrawlNode) 
                                    (NavEvent CrawlNode) 
                                    m 
                                    b
trackPerformanceStats step@(Continue consumer) = continue go
  where
    go stream@(Chunks xs) = Iteratee $ do 
      mapM_ (fn . nvVal) xs 
      runIteratee $ consumer stream >>== 
                    trackPerformanceStats
    go EOF = yield step EOF
    fn (CrawlWebPage wp) = trackPerformance wp
    fn (CrawlLink _) = return ()

trackPerformanceStats step = yield step EOF

