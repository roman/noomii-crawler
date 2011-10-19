{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (mapM_)
import Data.Maybe (fromJust)
import Network.URI (parseURI)
import System.IO (IOMode(..), stdout, withFile)

--------------------

import Data.Enumerator (run_, ($$), (=$))

import qualified Data.Enumerator.List as EL

--------------------

import Navigation.Enumerator
import Crawler.Enumerator
import Crawler.HTTP
import Crawler.HTML
import Crawler.Types

-------------------------------------------------------------------------------

main :: IO ()
main =
    run_ $ enumCrawler "http://staging.noomii.com/" $$
           removeVisited     =$
           debugVisit stdout =$
           EL.dropWhile (const True)
