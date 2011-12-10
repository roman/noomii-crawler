{-# LANGUAGE OverloadedStrings #-}
module Noomii.SummaryRenderer (renderSummary) where

--------------------

import Prelude hiding ((.))
import Control.Arrow ((***))
import Control.Category ((.))

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map

--------------------

import Blaze.ByteString.Builder (toByteString)
import Data.Lens.Common (Lens, getL)
import Data.Text (Text)
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
urlListSplice :: Monad m => [Text] -> Splice m
urlListSplice = mapSplices urlSplice

--------------------

-- Renders an URL using the url_item partial
urlSplice :: Monad m => Text -> Splice m
urlSplice url = do
    result <- callTemplate "url_item" [ ("urlTarget", url)
                                      , ("urlText", url)
                                      ]
    case result of
      Just tags -> return tags
      Nothing -> error "Error rendering url_item"

--------------------

-- Renders a list of urls that have the same title
repeatedTitleSplice :: Monad m
                    => Text
                    -> [Text]
                    -> Splice m
repeatedTitleSplice title urls =
    localTS bindSplices' $ do
      result <- callTemplate "pages_with_title" []
      case result of
        Just tags -> return tags
        Nothing -> error "Check the name of the template"
  where
    bindSplices' = bindSplices [("pageTitle", textSplice title),
                                ("urlList", urlListSplice urls)]

--------------------

-- Renders a list of urls that have the same meta desc
repeatedMetaSplice :: Monad m
                    => Text
                    -> [Text]
                    -> Splice m
repeatedMetaSplice desc urls =
    localTS bindSplices' $ do
      result <- callTemplate "pages_with_meta" []
      case result of
        Just tags -> return tags
        Nothing -> error "Check the name of the template"
  where
    bindSplices' =
      bindSplices [("pageMetaDescription", textSplice desc),
                   ("urlList", urlListSplice urls)]


--------------------

-- Renders several list of urls that have the same title
repeatedTitleListSplice :: Monad m
                     => [(Text, [Text])]
                     -> Splice m
repeatedTitleListSplice =
    mapSplices (uncurry repeatedTitleSplice)

--------------------

repeatedMetaListSplice :: Monad m
                     => [(Text, [Text])]
                     -> Splice m
repeatedMetaListSplice =
    mapSplices (uncurry repeatedMetaSplice)

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
    (noTitleUrls, titles) = splitNoTitleUrls noomiiState
    (noMetaUrls, meta) = splitNoMetaUrls noomiiState

    textNoTitleUrls = map T.pack noTitleUrls
    textNoMetaUrls = map T.pack noMetaUrls

    textMeta = map ((T.pack . B.unpack) *** (map T.pack)) .
               Map.toList $
               Map.filter ((> 1) . length) meta

    textTitles = map ((T.pack . B.unpack) *** (map T.pack)) .
                 Map.toList $
                 Map.filter ((> 1) . length) titles

    splices = [("pagesWithRepeatedTitles",
                repeatedTitleListSplice textTitles),
               ("pagesWithRepeatedMeta",
                repeatedMetaListSplice textMeta),
               ("noTitlePages", urlListSplice textNoTitleUrls),
               ("noMetaPages", urlListSplice textNoMetaUrls),
               ("minResponseTime", minPerfSplice noomiiState),
               ("maxResponseTime", maxPerfSplice noomiiState)]

