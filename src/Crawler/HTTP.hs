{-# LANGUAGE OverloadedStrings #-}
module Crawler.HTTP where

--------------------

import Control.Exception (try)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.ByteString.Char8 ()
import Data.Maybe (mapMaybe)
import Network.URI (parseAbsoluteURI)

import qualified Data.ByteString as BS

--------------------

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
import Network.HTTP.Types (Status(..), ResponseHeaders)
import Text.HTML.TagSoup (
    parseTags
  , canonicalizeTags
  )

--------------------

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
        result <- try $ run_ $ httpRedirect request
                                            responseIt
                                            manager

        case result of
          Left (StatusCodeException code _) ->
            return . Left $ "status code: " ++ show code
          Left (InvalidUrlException {}) ->
            return $ Left "invalid url format"
          Left TooManyRedirects ->
            return $ Left "too many redirects"
          Left (HttpParserException msg) ->
            return . Left $ "HTTP parsing error: " ++ msg

          Right (status, headers, body) -> do
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
responseIt status@(Status code _) headers
  | 200 <= code && code < 300 = do
      result <- EL.consume
      return (status, headers, BS.concat result)
  | otherwise = return (status, headers, BS.empty)

