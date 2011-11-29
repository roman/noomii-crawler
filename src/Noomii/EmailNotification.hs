{-# LANGUAGE OverloadedStrings #-}
module Noomii.EmailNotification (sendEmailWithStats) where

----------

import Prelude hiding ((.))
import Control.Category ((.))
import Data.ByteString (ByteString)

import qualified Data.Map as Map

----------

import Data.Lens.Common (getL)
import Data.Text.Lazy (Text)
import Network.HaskellNet.Auth
import Network.HaskellNet.SMTP

import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T

----------

import Noomii.Types

-------------------------------------------------------------------------------

buildStatsString :: NoomiiState -> Text
buildStatsString state = 
    T.intercalate "<br/>\n"  
      [ "Webpage with min. response time:"
      , T.pack $ urlmin ++ " (" ++ show minperf ++ ")"
      , "---"
      , "Webpage with max. response time:"
      , T.pack $ urlmax ++ " (" ++ show maxperf ++ ")"
      ]
    where
      (minperf, urlmin) = fromMinPerformance $ 
                          getL (minPerformance . performanceStats)
                               state
      (maxperf, urlmax) = fromMaxPerformance $ 
                          getL (maxPerformance . performanceStats)
                               state
      
buildRepeatedEntries :: String -> [(ByteString, [String])] -> Text
buildRepeatedEntries entryName [] = T.pack $ "No repeated " ++ entryName
buildRepeatedEntries entryName entries = 
    T.intercalate "<br/>\n-----</br>\n" $ map fn entries 
  where
    formatUrl url = T.pack $ "- " ++ url
    fn (entry, urls) = T.intercalate "\n"
                 [ T.pack $ "Webpages with repeated " ++ entryName
                 , T.pack $ entryName ++ ":"
                 , T.fromChunks [T.decodeUtf8 entry]
                 , T.intercalate "\n" $ map formatUrl urls
                 ]


buildRepeatedTitlesString :: NoomiiState -> Text
buildRepeatedTitlesString state = buildRepeatedEntries "Titles" entries
  where
    entries = Map.toList .
              Map.filter ((> 1) . length) $
              getL titleMap state

buildRepeatedMetaString :: NoomiiState -> Text
buildRepeatedMetaString state = buildRepeatedEntries "Meta" entries
  where
    entries = Map.toList .
              Map.filter ((> 1) . length) $
              getL metaMap state

buildCrawlSummary :: NoomiiState -> Text
buildCrawlSummary state = 
    T.intercalate "\n<br/>===========<br/>\n"
      [ ""
      , buildStatsString state
      , buildRepeatedTitlesString state
      , buildRepeatedMetaString state
      , ""
      ]

--------------------

sendEmailWithStats :: String -> String -> String -> NoomiiState -> IO ()
sendEmailWithStats user pass site state = 
    doSMTP site $ \smtp -> do
      T.putStrLn $ T.toStrict $ buildCrawlSummary state
      putStrLn "* Authenticating SMTP"
      sendCommand smtp (AUTH LOGIN user pass) >>= print
      putStrLn "* Sending Email"
      sendMimeMail "admin@noomii.com"
                   "admin@noomii.com"
                   "Crawl Run Successful" 
                   (buildCrawlSummary state)
                   (buildCrawlSummary state)
                   []
                   smtp
      putStrLn "* Email sent!"
    

