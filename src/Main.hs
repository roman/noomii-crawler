{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (stdout)

--------------------

import Data.Enumerator (run_, ($$), (=$))

import qualified Data.Enumerator.List as EL

--------------------

import Navigation.Enumerator
import Crawler.Enumerator

-------------------------------------------------------------------------------

main :: IO ()
main =
    run_ $ enumCrawler "http://staging.noomii.com/" $$
           removeVisited     =$
           debugVisit stdout =$
           EL.dropWhile (const True)
