{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (IOMode(WriteMode), withFile)
import Noomii.Crawler (crawlNoomii, printStats)

-------------------------------------------------------------------------------

main :: IO ()
main = do
    state <- crawlNoomii "staging"
    withFile "log/stats.txt" WriteMode $ flip printStats state
    putStrLn "Done."
