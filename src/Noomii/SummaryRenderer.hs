{-# LANGUAGE OverloadedStrings #-}
module Noomii.SummaryRenderer where

import Prelude hiding ((.))
import Control.Arrow ((***))
import Control.Category ((.))
import Data.ByteString (ByteString)
import Data.Map (Map)

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map

--------------------

import Blaze.ByteString.Builder (toByteString)
import Data.Lens.Common (getL)
import Data.Text (Text)
import Text.Templating.Heist

import qualified Data.Text as T

--------------------

import Noomii.Types

-------------------------------------------------------------------------------

urlListSplice :: Monad m => [Text] -> Splice m
urlListSplice = mapSplices urlSplice

--------------------

urlSplice :: Monad m => Text -> Splice m
urlSplice url = do
    result <- callTemplate "url_item" [ ("urlTarget", url)
                                      , ("urlText", url)
                                      ]
    case result of
      Just tmpl -> return tmpl
      Nothing -> error "Error rendering url_item"

--------------------

repeatedTitleSplice :: Monad m
                    => Text
                    -> [Text]
                    -> Splice m
repeatedTitleSplice title urls = 
    localTS bindedTS $ do
      result <- callTemplate "pages_with_title" []
      case result of
        Just tags -> return tags 
        Nothing -> error "Check the title of the template"
  where
    bindedTS = bindSplices [("pageTitle", textSplice title),
                            ("urlList", urlListSplice urls)]

--------------------

repeatedTitlesSplice :: Monad m 
                     => Map ByteString [String] 
                     -> Splice m
repeatedTitlesSplice =
    -- Create a big Splice with the tuples
    -- (Title, [URL])
    mapSplices (uncurry repeatedTitleSplice) .
    -- Transform the Title and Urls to Text
    map ((T.pack . B.unpack) *** (map T.pack)) .
    Map.toList

--------------------

renderSummary :: NoomiiState -> IO ByteString
renderSummary st = do
    ets <- loadTemplates "templates" $
           bindSplices splices
                       emptyTemplateState
    let ts = either error id ets
    b <- renderTemplate ts "summary"
    return $ maybe "wtf" (toByteString . fst) b
  where
    titles = getL titleMap st
    emptyTitleUrls = maybe [] (map T.pack) $ Map.lookup "" titles
    titlesWithoutEmpty = Map.delete "" titles 
    (minperf, urlmin) = fromMinPerformance $ 
                        getL (minPerformance . performanceStats)
                             st
    (maxperf, urlmax) = fromMaxPerformance $ 
                        getL (maxPerformance . performanceStats)
                             st
    splices = [("pagesWithRepeatedTitles", 
                  repeatedTitlesSplice titlesWithoutEmpty),
               ("noTitlePages", urlListSplice emptyTitleUrls),
               ("minResponseTime", 
                 textSplice $ T.pack $ urlmin ++ "(" ++ show minperf ++ ")"),
               ("maxResponseTime", 
                 textSplice $ T.pack $ urlmax ++ "(" ++ show maxperf ++ ")")]
                
  


