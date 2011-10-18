module Crawler.Enumerator where

import Control.Monad (mapM_)
import Control.Monad.Trans (MonadIO(..))
import Network.URI (parseAbsoluteURI)
import System.IO (Handle, hPutStrLn)

import qualified Data.Set as Set

--------------------

import Data.Enumerator hiding (map, mapM)

import qualified Data.Enumerator.List as EL

--------------------

import Crawler.HTTP
import Crawler.Types
import Navigation.Enumerator

-------------------------------------------------------------------------------

data CrawlNode
  = CrawlLink String
  | CrawlWebPage WebPage
  deriving (Show)

instance Eq CrawlNode where
  (CrawlLink l0) == (CrawlLink l1) = l0 == l1
  (CrawlWebPage wp0) == (CrawlWebPage wp1) = wp0 == wp1
  (CrawlLink l) == (CrawlWebPage wp) = l == getWebPageUrlString wp
  a == b = b == a

instance Ord CrawlNode where
  compare (CrawlLink s0) (CrawlLink s1) =
      compare s0 s1
  compare (CrawlWebPage wp0) (CrawlWebPage wp1) =
      compare wp0 wp1
  compare (CrawlLink link) (CrawlWebPage wp) =
      compare link (getWebPageUrlString wp)
  compare a b = compare b a

-------------------------------------------------------------------------------

enumCrawler :: MonadIO m => String -> Enumerator (NavEvent CrawlNode) m b
enumCrawler link step = Iteratee $
    case parseAbsoluteURI link of
      Nothing -> error $ "[error] invalid link: " ++ link
      Just uri ->
        runIteratee $ enumNavigation (\_ _ -> return 0)
                                     (requestChildren uri)
                                     (CrawlLink link)
                                     step
  where
    requestChildren domain n@(CrawlWebPage wp) =
      return (n,
              map CrawlLink $ getFollowLinks domain wp)
    requestChildren domain (CrawlLink link) = do
      result <- requestWebPage link
      case result of
        Left e -> return (CrawlLink link, [])
        Right wp ->
          return (CrawlWebPage wp,
                  map CrawlLink $ getFollowLinks domain wp)


debugVisit :: MonadIO m
           => Handle
           -> Enumeratee (NavEvent CrawlNode) (NavEvent CrawlNode) m b
debugVisit handle = EL.mapM showVisit
  where
    showVisit nv = do
      case nvVal nv of
        CrawlWebPage wp ->
            liftIO .
            hPutStrLn handle $
              "\x1B[32m"
              ++ show wp
              ++ "\x1B[0m"
        CrawlLink link ->
            liftIO .
            hPutStrLn handle $
              "\x1B[31m- [Invalid Link] "
              ++ link
              ++ "\x1B[0m"
      return nv
