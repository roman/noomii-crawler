{-# LANGUAGE OverloadedStrings #-}
module Crawler.HTTP (requestWebPage) where

--------------------
-- Standard

import Control.Exception (tryJust)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.ByteString.Char8 ()
import Data.Maybe (mapMaybe)
import Network.URI (parseAbsoluteURI, nullURI)

import qualified Data.ByteString as BS

--------------------
-- Third Party

import Data.Enumerator (
    Iteratee(..)
  , run_
  )
import Network.HTTP.Enumerator (
    HttpException(..)
  , withManager
  , parseUrl
  , httpRedirect
  )
import Network.HTTP.Types (Status(..), ResponseHeaders, statusBadRequest)
import Text.HTML.TagSoup (
    parseTags
  , canonicalizeTags
  )


import qualified Data.Enumerator.List as EL

--------------------
-- Local

import Crawler.HTML
import Crawler.Types
import System.Util

-------------------------------------------------------------------------------

requestWebPage :: MonadIO m => String -> m (Either String WebPage)
requestWebPage url = liftIO $ withManager $ \manager ->
    case parseAbsoluteURI url of

      Nothing ->
        -- Return an Invalid UrlException
        return . 
        Right $ 
          mkWebPage nullURI
                    url
                    []
                    []
                    statusBadRequest
                    []
                    Nothing
                    (Just $ InvalidUrlException url "invalid URL")

      Just uri -> do
        request <- parseUrl url
        (perf, result) <- trackPerformance $ 
                            tryJust requestErrorHandler . 
                            run_ $ httpRedirect request
                                                responseIt
                                                manager

        case result of
          Left e ->
            -- Return an invalid WebPage
            return . Right $ mkWebPage uri
                                       url
                                       []
                                       []
                                       statusBadRequest
                                       []
                                       (Just perf)
                                       (Just e)

          Right (status, headers, body) -> do
            let tags =  canonicalizeTags $ parseTags body
            let links = mapMaybe (mkLink uri) $
                        wholeTags "a" tags


            -- Return a valid WebPage
            return . Right $ mkWebPage uri
                                       url
                                       links
                                       tags
                                       status
                                       headers
                                       (Just perf)
                                       Nothing


requestErrorHandler :: HttpException -> Maybe HttpException
requestErrorHandler e@(InvalidUrlException {}) = Just e
requestErrorHandler e@TooManyRedirects = Just e 
requestErrorHandler e@(HttpParserException {}) = Just e
requestErrorHandler _ = Nothing

responseIt :: Monad m
           => Status
           -> ResponseHeaders
           -> Iteratee BS.ByteString
                       m
                       ( Status
                       , ResponseHeaders
                       , BS.ByteString
                       )
responseIt status@(Status code _) headers
  | 200 <= code && code < 300 = do
      result <- EL.consume
      return (status, headers, BS.concat result)
  | otherwise = return (status, headers, BS.empty)

