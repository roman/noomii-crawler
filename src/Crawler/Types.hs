{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Crawler.Types where

--------------------
-- Standard

import Control.Exception (SomeException, Exception(..))
import Data.ByteString.Char8 (ByteString, unpack, isInfixOf)
import Data.Maybe (listToMaybe)
import Data.Time (NominalDiffTime)
import Network.URI (
    URI(..)
  , parseURIReference
  , nonStrictRelativeTo
  , uriToString
  )
import Data.Ord (comparing)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

--------------------
-- Third Party

import Network.HTTP.Enumerator (HttpException(..))
import Network.HTTP.Types (Status(..), ResponseHeaders)
import Text.HTML.TagSoup (Tag(..), isTagOpenName, fromAttrib)

--------------------
-- Local

import Crawler.URI

-------------------------------------------------------------------------------

data WebPage
  = WebPage {
    wpURI        :: URI
  , wpURL        :: String
  , wpLinks      :: [Link]
  , wpBody       :: [Tag ByteString]
  , wpStatusCode :: Status
  , wpHeaders    :: ResponseHeaders
  , wpPerf       :: Maybe NominalDiffTime
  , wpError      :: Maybe SomeException
  }

data Link
  = Link {
    linkURI :: URI
  , linkTag :: WholeTag ByteString
  }

newtype WholeTag s
  = WholeTag {
    fromWholeTag :: [Tag s]
  }
  deriving (Show)

-------------------------------------------------------------------------------

showBadWebPage :: String -> String -> String -> Status -> String
showBadWebPage url msg perf status =
    "\x1B[31;1m- ["
    ++ show (statusCode status)
    ++ ": "
    ++ msg
    ++ "]\x1B[0m "
    ++ url
    ++ "\x1B[33;1m ("
    ++ perf
    ++ ")\x1B[0m"

showGoodWebPage :: String -> String -> Status -> String
showGoodWebPage url perf status =
    "\x1B[32;1m+ ["
    ++ show (statusCode status)
    ++ "]\x1B[0m "
    ++ url
    ++ "\x1B[33;1m ("
    ++ perf
    ++ ")\x1B[0m"

instance Show WebPage where
  show (WebPage _ url _ _ status _ perf0 (Just e))
    = case fromException e of
        Just (InvalidUrlException _ msg) ->
          showBadWebPage url msg perf status
        Just (TooManyRedirects) ->
          showBadWebPage url "too many redirects" perf status
        Just (HttpParserException _) ->
          showBadWebPage url "http parse error" perf status
        Just _ ->
          showBadWebPage url "status code error" perf status
        Nothing ->
          showBadWebPage url "unknown error" perf status
    where
      perf = maybe "0s" show perf0

  show (WebPage _ url _ _ status _ perf0 _) =
      showGoodWebPage url perf status
    where
      perf = maybe "0s" show perf0

instance Eq WebPage where
  wp0 == wp1 =  wpURI wp0 == wpURI wp1

instance Ord WebPage where
  compare = comparing getWebPageUrlString

instance Show Link where
  show (Link uri _) = "Link: " ++ show uri

-------------------------------------------------------------------------------
-- WebPage Functions

mkWebPage :: URI
          -> String
          -> [Link]
          -> [Tag ByteString]
          -> Status
          -> ResponseHeaders
          -> Maybe NominalDiffTime
          -> Maybe SomeException
          -> WebPage
mkWebPage uri url links tags status headers perf Nothing
  | 200 <= statusCode status && statusCode status < 300
    = WebPage uri url links tags status headers perf Nothing
  | otherwise
    = WebPage uri url links tags status headers perf $
        Just (toException $ StatusCodeException (statusCode status)
                                                BL.empty)
mkWebPage uri url links tags status headers perf e
    = WebPage uri url links tags status headers perf e



getWebPageUrlString :: WebPage -> String
getWebPageUrlString = wpURL


getFollowLinks :: URI -> WebPage -> [String]
getFollowLinks domain =
    map (flip (uriToString $ const "") "") .
    filter (isSameDomain domain) .
    map linkURI .
    filter (not . isNoFollow) .
    wpLinks


--------------------
-- Link Functions

mkLink :: URI -> WholeTag ByteString -> Maybe Link
mkLink domain wholeTag = do
    url <- listToMaybe .
           filter (not . BS.null) .
           map (fromAttrib "href") .
           take 1 .
           dropWhile (not . isTagOpenName "a") $
           fromWholeTag wholeTag


    partialUri  <- parseURIReference (unpack url)
    absoluteUri <- nonStrictRelativeTo partialUri domain
    return $ Link absoluteUri
                  wholeTag



isNoFollow :: Link -> Bool
isNoFollow = maybe False (`isInfixOf` "nofollow") .
             listToMaybe .
             filter (not . BS.null) .
             map (fromAttrib "rel") .
             take 1 .
             dropWhile (not . isTagOpenName "a") .
             fromWholeTag .
             linkTag

