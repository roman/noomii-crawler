{-# LANGUAGE OverloadedStrings #-}
module Crawler.Types where

--------------------

import Data.ByteString.Char8 (ByteString, unpack, isInfixOf)
import Data.Maybe (listToMaybe, mapMaybe)
import Network.URI (
    URI
  , parseURIReference
  , nonStrictRelativeTo
  , relativeTo
  , uriToString
  )

import qualified Data.ByteString as BS

--------------------

import Network.HTTP.Types (Status(..), ResponseHeaders)
import Text.HTML.TagSoup (Tag, isTagOpenName, fromAttrib)

--------------------

import Crawler.URI


-------------------------------------------------------------------------------

data WebPage
  = WebPage {
    wpURI        :: URI
  , wpLinks      :: [Link]
  , wpBody       :: [Tag ByteString]
  , wpStatusCode :: Status
  , wpHeaders    :: ResponseHeaders
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

instance Show WebPage where
  show (WebPage uri _ _ status _) =
      statusSymbol status
      ++ show uri
    where
      statusSymbol (Status code msg)
        | 200 <= code && code < 300 =
          "+ ["
          ++ show code
          ++ "] "
        | otherwise =
          "- ["
          ++ show code
          ++ ": "
          ++ show msg
          ++ "] "

instance Show Link where
  show (Link uri _) = "Link: " ++ show uri

-------------------------------------------------------------------------------

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

