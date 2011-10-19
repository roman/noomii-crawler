module Crawler.Enumerator where

import Control.Monad (zipWithM_)
import Control.Monad.Trans (MonadIO(..))
import Network.URI (parseAbsoluteURI)
import System.IO (Handle, hPutStrLn, hPutStr)

--------------------

import Data.Enumerator hiding (map, mapM, length)

--------------------

import Crawler.HTTP
import Crawler.Types
import Navigation.Enumerator

-------------------------------------------------------------------------------

data CrawlNode
  = CrawlLink String
  | CrawlWebPage WebPage

instance Show CrawlNode where
  show (CrawlLink url) = url
  show (CrawlWebPage wp) = show wp

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
enumCrawler link0 step = Iteratee $
    case parseAbsoluteURI link0 of
      Nothing -> error $ "[error] invalid link: " ++ link0
      Just uri ->
        runIteratee $ enumNavigation (\_ _ -> return 0)
                                     (requestChildren uri)
                                     (CrawlLink link0)
                                     step
  where
    requestChildren domain (CrawlLink link) = do
      result <- requestWebPage link
      case result of
        Left _ -> return (CrawlLink link, [])
        Right wp ->
          return (CrawlWebPage wp,
                  map CrawlLink $ getFollowLinks domain wp)


debugVisit :: MonadIO m
           => Handle
           -> Enumeratee (NavEvent CrawlNode) (NavEvent CrawlNode) m b
debugVisit handle = _debugVisit 0
  where
    _debugVisit acc step@(Continue consumer) = continue go
      where
        go EOF = yield step EOF
        go stream@(Chunks xs) = Iteratee $ do
          zipWithM_ showVisit [acc..] xs 
          runIteratee $ consumer stream >>==
                        _debugVisit (acc + length xs)
    _debugVisit _ step = yield step EOF
    showVisit i nv = do
      liftIO . hPutStr handle $ show i ++ " "
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
              "\x1B[31m- "
              ++ link
              ++ " (from: "
              ++ show (maybe "root" show $ nvParent nv)
              ++ ")"
              ++ "\x1B[0m"
      return nv

