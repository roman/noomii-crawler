module Noomii.Crawler where

----------

import Prelude hiding ((.))
import Codec.Compression.GZip (compress)
import Data.ByteString.Lazy (toChunks)
import Control.Category ((.))
import Control.Monad.Trans (MonadIO(..))
import System.IO (
    IOMode(WriteMode)
  , stdout
  , withFile
  )

import qualified Data.ByteString as BS

----------

import Data.Enumerator (($$), (=$), run_)

import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB

----------

import Crawler.Enumerator
import Navigation.Enumerator
import Noomii.Enumeratees
import Noomii.Monad
import Noomii.Types

-------------------------------------------------------------------------------


crawlNoomii :: MonadIO m => String -> m NoomiiState
crawlNoomii env =
    liftIO $
    withFile "log/sitemap.xml.gz" WriteMode $ \handle ->
      execNoomiiMonad $
        run_ $
          enumCrawler domain regexp     $$
          removeAlreadyVisited          =$
          debugVisitNumbered stdout     =$
          removeBrokenWebPages          =$
          debugFrontier stdout          =$
          trackPerformanceStats         =$
          trackRepeatedMeta             =$
          trackRepeatedTitles           =$
          evaluateState                 =$
          generateSitemap               =$
          EL.map compress               =$
          EL.map (BS.concat . toChunks) =$
          EB.iterHandle handle
  where
    regexp = "https?://.*\\.noomii\\.com/.*"
    domain
      | env == "production" = "http://www.noomii.com/"
      | otherwise = "http://" ++ env ++ ".noomii.com/"

