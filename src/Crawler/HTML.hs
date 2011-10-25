module Crawler.HTML where

--------------------

import Text.StringLike (StringLike)
import Text.HTML.TagSoup (Tag, isTagOpenName, isTagCloseName)

--------------------

import Crawler.Types


-------------------------------------------------------------------------------

wholeTags :: (StringLike s) => s -> [Tag s] -> [WholeTag s]
wholeTags name tags = go False [] tags
  where
    go _ result [] = result
    go False result ts = 
      go True result $ dropWhile (not . isTagOpenName name) ts
    go True result ts =
      let (important, remainder) = break (isTagCloseName name) ts
          wholeTag = WholeTag important
      in go False (wholeTag : result) remainder
