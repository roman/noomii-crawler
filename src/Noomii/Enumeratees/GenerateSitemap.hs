{-# LANGUAGE OverloadedStrings #-}
module Noomii.Enumeratees.GenerateSitemap (generateSitemap) where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

import qualified Data.ByteString.Lazy.Char8 as BS

--------------------
import Data.Enumerator hiding (map, concatMap)

--------------------
import Crawler.Enumerator
import Crawler.Types
import Navigation.Types

-------------------------------------------------------------------------------

generateSitemap :: MonadIO m => Enumeratee (NavEvent CrawlNode) ByteString m b
generateSitemap (Continue consumer0) = Iteratee $ do
    currentTime <- liftIO getCurrentTime
    let lastUpdated = BS.pack $ formatTime defaultTimeLocale
                                          "%Y-%m-%dT%H:%M:%S+00:00"
                                          currentTime
    runIteratee $ consumer0 (Chunks header) >>==
                  _generateSitemap lastUpdated
  where
    header = [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
             , "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
             ]
generateSitemap step = return step

----------

_generateSitemap :: MonadIO m
                 => ByteString
                 -> Enumeratee (NavEvent CrawlNode) ByteString m b
_generateSitemap lastUpdated (Continue consumer) = continue go
  where
    footer = [ "</urlset>" ]
    go (Chunks xs) =
      let fn = wpToXml lastUpdated . nvVal
      in consumer (Chunks $ map fn xs) >>==
                  _generateSitemap lastUpdated
    go _ =
      consumer (Chunks footer) >>==
      return
_generateSitemap _ step = return step

----------

escapeURIChar :: Char -> String
escapeURIChar c
  | c == '&'  = "&amp;"
  | c == '\'' = "&apos;"
  | c == '"'  = "&quot;"
  | c == '>'  = "&gt;"
  | c == '<'  = "&lt;"
  | otherwise = [c]


wpToXml :: ByteString -> CrawlNode -> ByteString
wpToXml lastmod (CrawlWebPage wp) =
    BS.concat [ "<url>"
              , BS.concat ["<loc>"
                          , BS.pack . concatMap escapeURIChar $ wpURL wp
                          , "</loc>"]
              , BS.concat ["<lastmod>", lastmod, "</lastmod>"]
              , "<changefreq>Weekly</changefreq>"
              , "</url>"
              ]
wpToXml _ (CrawlLink _) = ""
