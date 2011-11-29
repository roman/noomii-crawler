{-# LANGUAGE OverloadedStrings #-}
module Noomii.EmailNotification (sendEmailWithStats) where

----------

import Network.HaskellNet.Auth
import Network.HaskellNet.SMTP

import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T

----------

import Noomii.SummaryRenderer
import Noomii.Types

sendEmailWithStats :: String
                   -> String
                   -> String
                   -> NoomiiState
                   -> IO ()
sendEmailWithStats user pass site state =
    doSMTP site $ \smtp -> do
      summary <- renderSummary state
      let lazySummary = T.fromChunks [summary]
      T.putStrLn summary
      putStrLn "* Authenticating SMTP"
      sendCommand smtp (AUTH LOGIN user pass) >>= print
      putStrLn "* Sending Email"
      sendMimeMail "admin@noomii.com"
                   "admin@noomii.com"
                   "Crawl Run Successful"
                   lazySummary
                   lazySummary
                   []
                   smtp
      putStrLn "* Email sent!"


