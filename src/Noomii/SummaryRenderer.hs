{-# LANGUAGE OverloadedStrings #-}
module Noomii.SummaryRenderer (renderSummary) where

--------------------

import Prelude hiding ((.))
import Control.Category ((.))
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Sequence (Seq)

import qualified Data.Map as Map
import qualified Data.Sequence as Seq

--------------------

import Blaze.ByteString.Builder (toByteString)
import Data.Lens.Common (Lens, getL)
import Data.Text (Text)
import Network.HTTP.Types (Status(..))
import Text.Templating.Heist

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

--------------------

import Noomii.Types

-------------------------------------------------------------------------------

-- Renders with Pretty instance
prettySplice :: (Monad m, Pretty p) => p -> Splice m
prettySplice = textSplice . T.pack . prettyShow

--------------------

perfSplice :: (Pretty p, Monad m)
           => Lens PerformanceStat p
           -> NoomiiState
           -> Splice m
perfSplice lens =
    prettySplice .
    getL (lens . performanceStats)

minPerfSplice, maxPerfSplice ::
  Monad m => NoomiiState -> Splice m
minPerfSplice = perfSplice minPerformance
maxPerfSplice = perfSplice maxPerformance

--------------------

-- Renders a List of Urls
urlListSplice :: Monad m => Seq (String, String) -> Splice m
urlListSplice = mapSplices urlSplice . toList

--------------------

brokenUrlListSplice :: Monad m => Seq (Status, String) -> Splice m
brokenUrlListSplice = mapSplices brokenUrlSplice . toList

--------------------

-- Renders an URL using the url_item partial
urlSplice :: Monad m => (String, String) -> Splice m
urlSplice (parentUrl, url) = do
    let url' = T.pack url
    let parentUrl' = T.pack parentUrl
    result <- callTemplate "url_item" [ ("urlTarget", url')
                                      , ("urlText", url')
                                      , ("fromTarget", parentUrl')
                                      , ("fromText", parentUrl')
                                      ]
    case result of
      Just tags -> return tags
      Nothing -> error "Error rendering url_item"

--------------------

brokenUrlSplice :: Monad m => (Status, String) -> Splice m
brokenUrlSplice (status, url) = do
    result <- callTemplate "broken_url_item"
                           [ ("urlTarget", T.pack url)
                           , ("urlText", T.pack url)
                           , ("urlStatus", T.pack $ show status)]
    case result of
      Just tags -> return tags
      Nothing -> error "Error rendering broken_url_item"

--------------------

-- Renders a list of urls that have the same title
repeatedTitleSplice :: Monad m
                    => ByteString
                    -> Seq (String, String)
                    -> Splice m
repeatedTitleSplice title urls =
    localTS bindSplices' $ do
      result <- callTemplate "pages_with_title" []
      case result of
        Just tags -> return tags
        Nothing -> error "Check the name of the template"
  where
    bindSplices' =
      bindSplices [("pageTitle", textSplice $ T.decodeUtf8 title),
                   ("urlList", urlListSplice urls)]

--------------------

-- Renders a list of urls that have the same meta desc
repeatedMetaSplice :: Monad m
                    => ByteString
                    -> Seq (String, String)
                    -> Splice m
repeatedMetaSplice desc urls =
    localTS bindSplices' $ do
      result <- callTemplate "pages_with_meta" []
      case result of
        Just tags -> return tags
        Nothing -> error "Check the name of the template"
  where
    bindSplices' =
      bindSplices [("pageMetaDescription", textSplice $ T.decodeUtf8 desc),
                   ("urlList", urlListSplice urls)]

--------------------

urlErrorSplice :: Monad m
               => String
               -> Seq (Status, String)
               -> Splice m
urlErrorSplice parentURL urls =
    localTS bindSplices' $ do
      result <- callTemplate "broken_page" []
      case result of
        Just tags -> return tags
        Nothing -> error "Check the name of the template"
  where
    bindSplices' =
      bindSplices [ ("fromUrl", textSplice $ T.pack parentURL)
                  , ("brokenUrlList", brokenUrlListSplice urls)]


--------------------

-- Renders several list of urls that have the same title
repeatedTitleListSplice :: Monad m
                     => [(ByteString, Seq (String, String))]
                     -> Splice m
repeatedTitleListSplice =
    mapSplices (uncurry repeatedTitleSplice)

--------------------

repeatedMetaListSplice :: Monad m
                     => [(ByteString, Seq (String, String))]
                     -> Splice m
repeatedMetaListSplice =
    mapSplices (uncurry repeatedMetaSplice)

--------------------

urlErrorsListSplice :: Monad m
                    => [(String, Seq (Status, String))]
                    -> Splice m
urlErrorsListSplice = mapSplices (uncurry urlErrorSplice)
--------------------

renderSummary :: NoomiiState -> IO Text
renderSummary noomiiState = do
    ets <- loadTemplates "templates" $
           bindSplices splices
                       emptyTemplateState
    let ts = either error id ets
    renderResult <- renderTemplate ts "summary"
    return $ maybe "ERROR: Template summary not found"
                   (T.decodeUtf8 .
                    toByteString .
                    fst)
                   renderResult
  where
    (noTitleUrls, titles0) = splitNoTitleUrls noomiiState
    (noMetaUrls, meta0) = splitNoMetaUrls noomiiState

    meta = Map.toList $
           Map.filter ((> 1) . Seq.length) meta0

    titles = Map.toList $
             Map.filter ((> 1) . Seq.length) titles0

    errors = Map.toList $ getL errorMap noomiiState

    splices = [ ("pagesWithRepeatedTitles", repeatedTitleListSplice titles)
              , ("pagesWithRepeatedMeta", repeatedMetaListSplice meta)
              , ("noTitlePages", urlListSplice noTitleUrls)
              , ("noMetaPages", urlListSplice noMetaUrls)
              , ("minResponseTime", minPerfSplice noomiiState)
              , ("maxResponseTime", maxPerfSplice noomiiState)
              , ("brokenPages", urlErrorsListSplice errors)]

