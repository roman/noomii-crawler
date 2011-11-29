{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
----------

import Noomii.Crawler (crawlNoomii)
import Noomii.EmailNotification (sendEmailWithStats)
import Noomii.SummaryRenderer

-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
      (user:pass:site:_) -> do
        state <- crawlNoomii "production"
        renderSummary state >>= print
        sendEmailWithStats user pass site state
        putStrLn "Done."
      _ -> putStrLn "ERROR: usage ./crawler user pass smtp server"


