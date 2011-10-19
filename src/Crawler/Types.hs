{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Crawler.Types where

--------------------

import Data.ByteString.Char8 (ByteString, unpack, isInfixOf)
import Data.Maybe (listToMaybe, mapMaybe)
import Network.URI (
    URI(..)
  , URIAuth(..)
  , parseURIReference
  , nonStrictRelativeTo
  , relativeTo
  , uriToString
  )

import qualified Data.ByteString as BS

--------------------

import Control.DeepSeq (NFData(..))
import Network.HTTP.Types (Status(..), ResponseHeaders)
import Text.HTML.TagSoup (Tag(..), isTagOpenName, fromAttrib)

--------------------

import Crawler.URI


-------------------------------------------------------------------------------

data WebPage
  = WebPage {
    wpURI        :: URI
  , wpLinks      :: [Link]
  --, wpBody       :: [Tag ByteString]
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
  deriving (Show, NFData)

-------------------------------------------------------------------------------

instance Show WebPage where
  show (WebPage uri _ status _) =
  --show (WebPage uri _ _ status _) =
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

instance Eq WebPage where
  wp0 == wp1 =  wpURI wp0 == wpURI wp1

instance Ord WebPage where
  wp0 `compare` wp1 = 
      compare (getWebPageUrlString wp0)
              (getWebPageUrlString wp1)

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
  rnf (TagOpen str xs) = ()
  rnf (TagClose str) = ()
  rnf (TagText str) = ()
  rnf (TagComment str) = ()
  rnf (TagWarning str) = ()
  rnf (TagPosition a b) = ()

-------------------------------------------------------------------------------

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

