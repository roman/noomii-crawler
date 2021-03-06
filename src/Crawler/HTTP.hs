{-# LANGUAGE OverloadedStrings #-}
module Crawler.HTTP (requestWebPage) where

--------------------
-- Standard

import Control.Exception (Exception(..), try)
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
import Network.HTTP.Types (Status(..), ResponseHeaders, badRequest400)
import Text.HTML.TagSoup (
    parseTags
  , canonicalizeTags
  )


import qualified Data.Enumerator.List as EL

--------------------
-- Local

import Crawler.HTML (wholeTags)
import Crawler.Types (
    WebPageException(..)
  , WebPage
  , mkWebPage
  , mkLink
  , isLinkWithSpecialChar)
import System.Util (trackPerformance)

-------------------------------------------------------------------------------

requestWebPage :: MonadIO m
               => String
               -> String
               -> m (Either String WebPage)
requestWebPage fromUrl url = liftIO $ withManager $ \manager ->
    case parseAbsoluteURI url of

      Nothing ->
        -- Return an Invalid UrlException
        return .
        Right $
          mkWebPage nullURI
                    url
                    fromUrl
                    []
                    []
                    badRequest400
                    []
                    Nothing
                    (Just .
                      toException .
                      HttpError $
                      InvalidUrlException url "invalid URL")

      Just uri -> do
        request <- parseUrl url
        (perf, result) <- trackPerformance $
                            try $
                            run_ $ httpRedirect request
                                                responseIt
                                                manager

        case result of
          Left e ->
            -- Return an invalid WebPage
            return . Right $ mkWebPage uri
                                       url
                                       fromUrl
                                       []
                                       []
                                       badRequest400
                                       []
                                       (Just perf)
                                       (Just .
                                         toException .
                                         HttpError $ e)

          Right (status, headers, body) -> do
            let tags =  canonicalizeTags $ parseTags body
            let links = mapMaybe (mkLink uri) $
                        wholeTags "a" tags
            let buildWp = mkWebPage uri
                                    url
                                    fromUrl
                                    links
                                    tags
                                    status
                                    headers
                                    (Just perf)


            let links' = filter isLinkWithSpecialChar links
            if null links'
              then
                -- Return a valid WebPage
                return .
                  Right $
                  buildWp Nothing
              else do
                return .
                  Right $
                  buildWp (Just $ toException $ SpecialCharactersOnLink links')


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

