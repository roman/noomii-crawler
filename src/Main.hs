{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (mapM_)
import Data.Maybe (fromJust)
import Network.URI (parseURI)
import System.IO (stdout)

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
main = do
    result <- run_ $ enumCrawler "http://www.noomii.com/" $$
           removeVisited  =$
           EL.isolate 10  =$
           --debugFrontier     =$
           --debugVisitedSet   =$
           debugVisit stdout =$
           EL.consume
    return ()
    --print result
    --let uri = fromJust $ parseURI "http://www.noomii.com"
    --result <- requestWebPage "http://www.noomii.com" 
    --case result of
    --  Left e -> putStrLn e
    --  Right wp -> 
    --    mapM_ print $ getFollowLinks uri wp
      
