module Crawler.HTML where

import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid(..))

--------------------

import Text.StringLike (StringLike)
import Text.HTML.TagSoup (
    Tag
  , isTagOpenName
  , isTagCloseName
  , fromAttrib
  , maybeTagText
  )

--------------------

import Crawler.Types

-------------------------------------------------------------------------------

getTextFromWholeTag :: StringLike s
                    => WholeTag s
                    -> [s]
getTextFromWholeTag (WholeTag s) = mapMaybe maybeTagText s

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

getAttrFromWholeTag :: (Show s, StringLike s, Monoid s)
                    => s
                    -> WholeTag s
                    -> s
getAttrFromWholeTag attr (WholeTag (s:_)) = fromAttrib attr s
getAttrFromWholeTag _ (WholeTag _) = mempty

