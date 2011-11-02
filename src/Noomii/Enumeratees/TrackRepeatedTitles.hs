module Noomii.Enumeratees.TrackRepeatedTitles where

--------------------
import Data.Enumerator

--------------------
import Crawler.Enumerator
import Crawler.Types
import Navigation.Types

-------------------------------------------------------------------------------

class Monad m => TitleTrackerMonad m where
  trackRepeatedTitleTags :: WebPage -> m ()

-------------------------------------------------------------------------------

trackRepeatedTitles :: TitleTrackerMonad m
                  => Enumeratee (NavEvent CrawlNode)
                                (NavEvent CrawlNode)
                                m
                                b
trackRepeatedTitles step@(Continue consumer) = continue go
  where
    go stream@(Chunks xs) = Iteratee $ do
      mapM_ (fn . nvVal) xs
      runIteratee $ consumer stream >>==
                    trackRepeatedTitles
    go EOF = yield step EOF
    fn (CrawlWebPage wp) = trackRepeatedTitleTags wp
    fn (CrawlLink _) = return ()

trackRepeatedTitles step = yield step EOF

