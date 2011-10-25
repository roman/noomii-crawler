{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Crawler.Types where

--------------------
-- Standard

import Data.ByteString.Char8 (ByteString, unpack, isInfixOf)
import Data.Maybe (listToMaybe)
import Data.Time (NominalDiffTime)
import Network.URI (
    URI(..)
  , URIAuth(..)
  , parseURIReference
  , nonStrictRelativeTo
  , uriToString
  )
import Data.Ord (comparing)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

--------------------
-- Third Party

import Control.DeepSeq (NFData(..))
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
  , wpError      :: Maybe HttpException
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
  deriving (Show, NFData)

-------------------------------------------------------------------------------

showBadWebPage :: String -> String -> Status -> String
showBadWebPage url msg status =
    "- "
    ++ "\x1B[31;1m["
    ++ show (statusCode status)
    ++ "]\x1B[0m "
    ++ url
    ++ " ("
    ++ msg
    ++ ")"

showGoodWebPage :: String -> Status -> String
showGoodWebPage url status =
    "+ "
    ++ "\x1B[32;1m["
    ++ show (statusCode status)
    ++ "]\x1B[0m "
    ++ url

instance Show WebPage where
  show (WebPage _ url _ _ status _ _ (Just e))
    = case e of
        InvalidUrlException _ msg -> 
          showBadWebPage url msg status
        TooManyRedirects          -> 
          showBadWebPage url "too many redirects" status
        HttpParserException _     -> 
          showBadWebPage url "http parse error" status

  show (WebPage _ url _ _ status _ _ _) = showGoodWebPage url status

instance Eq WebPage where
  wp0 == wp1 =  wpURI wp0 == wpURI wp1

instance Ord WebPage where
  compare = comparing getWebPageUrlString

instance Show Link where
  show (Link uri _) = "Link: " ++ show uri

instance NFData Link where
  rnf (Link uri tag) = rnf uri `seq` rnf tag

instance NFData URI where
  rnf (URI s a p q f) =
      rnf s `seq`
      rnf a `seq`
      rnf p `seq`
      rnf q `seq`
      rnf f

instance NFData URIAuth where
  rnf (URIAuth ui urn up) =
      rnf ui `seq`
      rnf urn `seq`
      rnf up

instance NFData (Tag s) where
  rnf (TagOpen {}) = ()
  rnf (TagClose {}) = ()
  rnf (TagText {}) = ()
  rnf (TagComment {}) = ()
  rnf (TagWarning {}) = ()
  rnf (TagPosition {}) = ()

-------------------------------------------------------------------------------

mkWebPage :: URI 
          -> String 
          -> [Link] 
          -> [Tag ByteString] 
          -> Status 
          -> ResponseHeaders 
          -> Maybe NominalDiffTime
          -> Maybe HttpException
          -> WebPage
mkWebPage uri url links tags status headers perf Nothing
  | 200 <= statusCode status && statusCode status < 300
    = WebPage uri url links tags status headers perf Nothing
  | otherwise
    = WebPage uri url links tags status headers perf $ 
        Just (StatusCodeException (statusCode status) BL.empty)
mkWebPage uri url links tags status headers perf e
    = WebPage uri url links tags status headers perf e


getWebPageUrlString :: WebPage -> String
getWebPageUrlString wp = uriToString (const "") (wpURI wp) ""

getFollowLinks :: URI -> WebPage -> [String]
getFollowLinks domain =
    map (flip (uriToString $ const "") "") .
    filter (isSameDomain domain) .
    map linkURI .
    filter (not . isNoFollow) .
    wpLinks

--------------------

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

