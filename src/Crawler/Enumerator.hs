module Crawler.Enumerator where

--------------------
-- Standard

import Control.Monad.Trans (MonadIO(..))
import Data.Maybe (isNothing)
import Network.URI (parseAbsoluteURI)
import Text.Regex.PCRE ((=~))

--------------------
-- Third Party

import Data.Enumerator hiding (filter, map, mapM, length)

import qualified Data.Enumerator.List as EL

--------------------
-- Local

import Crawler.HTTP
import Crawler.Types
import Navigation.Enumerator
import Pretty

-------------------------------------------------------------------------------
-- Types

data CrawlNode
  = CrawlLink String
  | CrawlWebPage WebPage

--------------------
-- Classtype Instances for CrawlNode

instance Pretty CrawlNode where
  prettyDoc (CrawlLink url) = prettyDoc url
  prettyDoc (CrawlWebPage wp) = prettyDoc wp

----------

instance Eq CrawlNode where
  (CrawlLink l0) == (CrawlLink l1) = l0 == l1
  (CrawlWebPage wp0) == (CrawlWebPage wp1) = wp0 == wp1
  (CrawlLink l) == (CrawlWebPage wp) = l == getWebPageUrlString wp
  a == b = b == a

----------

instance Ord CrawlNode where
  compare (CrawlLink s0) (CrawlLink s1) =
      compare s0 s1
  compare (CrawlWebPage wp0) (CrawlWebPage wp1) =
      compare wp0 wp1
  compare (CrawlLink link) (CrawlWebPage wp) =
      compare link (getWebPageUrlString wp)
  compare a b = compare b a

-------------------------------------------------------------------------------

enumCrawler :: MonadIO m
            => String
            -> String
            -> Enumerator (NavEvent CrawlNode) m b
enumCrawler link0 regexp step = Iteratee $
    case parseAbsoluteURI link0 of
      Nothing -> error $ "[error] invalid link: " ++ link0
      Just uri ->
        runIteratee $ enumNavigation (\_ _ -> return 0)
                                     (requestChildren uri)
                                     (CrawlLink link0)
                                     step
  where
    requestChildren _ (CrawlWebPage _) =
      error "[error] invalid state on crawler"
    requestChildren domain (CrawlLink link) = do
      result <- requestWebPage link
      case result of
        Left _ -> return (CrawlLink link, [])
        Right wp ->
          return ( CrawlWebPage wp
                 , map CrawlLink $
                   filter (=~ regexp) $
                   getFollowLinks domain wp)


--------------------

removeBrokenWebPages :: Monad m => Enumeratee (NavEvent CrawlNode)
                                              (NavEvent CrawlNode)
                                              m
                                              b
removeBrokenWebPages = EL.filter (notBroken . nvVal)
  where
    notBroken (CrawlWebPage wp) = isNothing $ wpError wp
    notBroken _ = False

