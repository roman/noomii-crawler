{-# LANGUAGE OverloadedStrings #-}
module Noomii.EmailNotification (sendEmailWithStats) where

----------

import System.IO (hSetEncoding, stdout, utf8)
import Network.HaskellNet.Auth (AuthType(LOGIN))
import Network.HaskellNet.SMTP (Command(AUTH), doSMTP, sendCommand, sendMimeMail)

import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T

----------

import Noomii.SummaryRenderer (renderSummary)
import Noomii.Types (NoomiiState)

sendEmailWithStats :: String
                   -> String
                   -> String
                   -> NoomiiState
                   -> IO ()
sendEmailWithStats user pass site state =
    doSMTP site $ \smtp -> do
      putStrLn "Rendering the crawl summary..."
      summary <- renderSummary state
      let lazySummary = T.fromChunks [summary]
      hSetEncoding stdout utf8
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


