module Crawler.URI where

import Control.Applicative
import Data.Maybe (fromMaybe)
import Network.URI (URI(..), URIAuth(..))

isSameDomain :: URI -> URI -> Bool
isSameDomain u1 u2 = fromMaybe False $
    (==) <$> (uriRegName `fmap` uriAuthority u1)
         <*> (uriRegName `fmap` uriAuthority u2)

