{-# LANGUAGE OverloadedStrings #-}
module Crawler.HTTP where

--------------------

import Control.Monad.Trans (MonadIO, liftIO)
import Data.ByteString.Char8 ()
import Data.Maybe (mapMaybe)
import Network.URI (parseAbsoluteURI)

import qualified Data.ByteString as BS

--------------------

import Control.DeepSeq
import Data.Enumerator (
    Iteratee(..)
  , Enumeratee
  , Step(..)
  , Iteratee(..)
  , Stream(..)
  , (>>==)
  , (=$)
  , yield
  , continue
  , run_
  )
import Network.HTTP.Enumerator (withManager, parseUrl, httpRedirect)
import Network.HTTP.Types (Status(..), ResponseHeaders)
import Text.HTML.TagSoup (
    Tag
  , parseTags
  , canonicalizeTags
  )
import Text.StringLike (StringLike)

--------------------

import qualified Data.ByteString as BS
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

--------------------

import Crawler.HTML
import Crawler.Types

-------------------------------------------------------------------------------

requestWebPage :: MonadIO m => String -> m (Either String WebPage)
requestWebPage url = liftIO $ withManager $ \manager ->
    case parseAbsoluteURI url of

      Nothing ->
        return . Left $ "invalid url format: " ++ url

      Just uri -> do
        request <- parseUrl url
        (status, headers, body) <- run_ $ httpRedirect request
                                          responseIt
                                          manager

        let tags =  canonicalizeTags $ parseTags body
        let links = mapMaybe (mkLink uri) $
                    wholeTags "a" tags


        return . Right $ WebPage uri
                                 links
                                 tags
                                 status
                                 headers


responseIt :: Monad m
           => Status
           -> ResponseHeaders
           -> Iteratee BS.ByteString
                       m
                       ( Status
                       , ResponseHeaders
                       , BS.ByteString
                       )
responseIt status@(Status code msg) headers
  | 200 <= code && code < 300 = do
      result <- EL.consume
      return (status, headers, BS.concat result)
  | otherwise = return (status, headers, BS.empty)

--toTags :: Monad m => Enumeratee BS.ByteString (Tag BS.ByteString) m b
--toTags step@(Yield {}) = yield step EOF
--toTags step@(Continue consumer) = continue go
--  where
--    go (Chunks bs) = Iteratee $ do
--      let tags = parseTags $ BS.concat bs
--      runIteratee $ consumer (Chunks tags) >>== toTags
--    go EOF = yield step EOF

