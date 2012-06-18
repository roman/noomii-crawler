{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import Data.Typeable (Typeable)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

--------------------
-- Third Party

import Network.HTTP.Enumerator (HttpException(..))
import Network.HTTP.Types (Status(..), ResponseHeaders)
import Text.HTML.TagSoup (Tag(..), isTagOpenName, fromAttrib)

--------------------
-- Local

import Crawler.URI
import Pretty
import qualified Pretty as P

-------------------------------------------------------------------------------

data WebPageException
  = SpecialCharactersOnLink
  deriving (Show, Typeable)

instance Exception WebPageException

data WebPage
  = WebPage {
    wpURI        :: URI
  , wpURL        :: String
  , wpParentURL  :: String
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

showBadWebPage :: String -> String -> String -> String -> Status -> Doc
showBadWebPage url parentUrl msg perf status =
    P.hang (P.text "From: " <+> P.text parentUrl)
           2 $
           P.red (P.brackets $
                  P.text (show $ statusCode status)
                  <> P.colon
                  <+> P.text msg)
           <+> P.text url
           <+> P.yellow (P.parens $
                         P.text perf)

showGoodWebPage :: String -> String -> Status -> Doc
showGoodWebPage url perf status =
    P.green (P.brackets $
             P.text (show $ statusCode status))
    <+> P.text url
    <+> P.yellow (P.parens $
                  P.text perf)

instance Pretty WebPage where
  prettyDoc (WebPage _ url parentUrl _ _ status _ perf0 (Just e))
    = case fromException e of
        Just (InvalidUrlException _ msg) ->
          showBadWebPage url parentUrl msg perf status
        Just (TooManyRedirects) ->
          showBadWebPage url parentUrl "too many redirects" perf status
        Just (HttpParserException _) ->
          showBadWebPage url parentUrl "http parse error" perf status
        Just _ ->
          showBadWebPage url parentUrl "status code error" perf status
        Nothing ->
          showBadWebPage url parentUrl "unknown error" perf status
    where
      perf = maybe "0s" show perf0

  prettyDoc (WebPage _ url _ _ _ status _ perf0 _) =
      showGoodWebPage url perf status
    where
      perf = maybe "0s" show perf0

instance Eq WebPage where
  wp0 == wp1 =  wpURI wp0 == wpURI wp1

instance Ord WebPage where
  compare = comparing getWebPageUrlString

instance Pretty Link where
  prettyDoc (Link uri _) = P.text "Link" <>
                           P.colon <+>
                           P.text (show uri)

-------------------------------------------------------------------------------
-- WebPage Functions

mkWebPage :: URI
          -> String
          -> String
          -> [Link]
          -> [Tag ByteString]
          -> Status
          -> ResponseHeaders
          -> Maybe NominalDiffTime
          -> Maybe SomeException
          -> WebPage
mkWebPage uri url parentUrl links tags status headers perf Nothing
  | 200 <= statusCode status && statusCode status < 300
    = WebPage uri url parentUrl links tags status headers perf Nothing
  | otherwise
    = WebPage uri url parentUrl links tags status headers perf $
        Just (toException $ StatusCodeException (statusCode status)
                                                BL.empty)
mkWebPage uri url parentUrl links tags status headers perf e
    = WebPage uri
              url
              parentUrl
              links
              tags
              status
              headers
              perf
              e

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

--------------------

isNoFollow :: Link -> Bool
isNoFollow = maybe False (`isInfixOf` "nofollow") .
             listToMaybe .
             filter (not . BS.null) .
             map (fromAttrib "rel") .
             take 1 .
             dropWhile (not . isTagOpenName "a") .
             fromWholeTag .
             linkTag

--------------------

isUrlWithSpecialChar :: ByteString -> Bool
isUrlWithSpecialChar = BC.any isSpecialChar
  where
    isSpecialChar :: Char -> Bool
    isSpecialChar c = c `elem` "#' "

isLinkWithSpecialChar :: Link -> Bool
isLinkWithSpecialChar = null .
                        filter (not . isUrlWithSpecialChar) .
                        map (fromAttrib "href") .
                        take 1 .
                        dropWhile (not . isTagOpenName "a") .
                        fromWholeTag .
                        linkTag
