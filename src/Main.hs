{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (mapM_)
import Data.Maybe (fromJust)
import Network.URI (parseURI)

import Crawler.HTTP
import Crawler.HTML
import Crawler.Types

main :: IO ()
main = do
  let uri = fromJust $ parseURI "http://www.noomii.com"
  result <- requestWebPage "http://www.noomii.com" 
  case result of
    Left e -> putStrLn e
    Right wp -> 
      mapM_ print $ getFollowLinks uri wp
      
