{-# LANGUAGE OverloadedStrings #-}
module Main where

--------------------

import System.IO (stdout)

--------------------
-- Third Party

import Data.Enumerator (run_, ($$), (=$))

import qualified Data.Enumerator.List as EL

--------------------
-- 

import Navigation.Enumerator
import Crawler.Enumerator

-------------------------------------------------------------------------------

main :: IO ()
main =
    run_ $ 
      enumCrawler "http://staging.noomii.com/" 
                  "https?://.*\\.noomii\\.com/.*" $$
      removeAlreadyVisited      =$
      EL.isolate 30             =$
      debugVisitNumbered stdout =$
      EL.dropWhile (const True)
