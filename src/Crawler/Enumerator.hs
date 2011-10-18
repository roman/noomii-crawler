module Crawler.Enumerator where

import Control.Monad.Trans (MonadIO)

--------------------

import Data.Enumerator 

--------------------

import Crawler.HTML
import Crawler.HTTP
import Crawler.Types

-------------------------------------------------------------------------------

--enumCrawler :: MonadIO m 
--            => String
--            -> Enumerator WebPage m b
--enumCrawler url step@(Yield {}) = returnI step 
--enumCrawler url step@(Continue consumer) = Iteratee $ do
--    result <- getWebPage url  
--    case result of
--      Left e -> putStrLn $ "- [error] " ++ e
--      Right wp -> 



