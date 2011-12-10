{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module Pretty (
    Pretty (..)
  , PrettyElem (..)
  , redColor
  , greenColor
  , yellowColor
  , blueColor
  , purpleColor
  , red
  , green
  , yellow
  , blue
  , purple
  , resetColor
  , module Text.PrettyPrint
  ) where

import Text.PrettyPrint
import qualified Text.PrettyPrint as P
import qualified Data.Map as Map
import qualified Data.Set as Set


redColor :: Doc
redColor = P.text "\x1B[31m"

greenColor :: Doc
greenColor = P.text "\x1B[32m"

yellowColor :: Doc
yellowColor = P.text "\x1B[33m"

blueColor :: Doc
blueColor = P.text "\x1B[34m"

purpleColor :: Doc
purpleColor = P.text "\x1B[35m"

resetColor :: Doc
resetColor = P.text "\x1B[0m"

red :: Doc -> Doc
red p = redColor <> p <> resetColor

green :: Doc -> Doc
green p = greenColor <> p <> resetColor

yellow :: Doc -> Doc
yellow p = yellowColor <> p <> resetColor

blue :: Doc -> Doc
blue p = blueColor <> p <> resetColor

purple :: Doc -> Doc
purple p = purpleColor <> p <> resetColor


class Pretty a where
  prettyShow :: a -> String
  prettyShow = render . prettyDoc
  prettyDoc  :: a -> Doc

data PrettyElem
  = forall a. Pretty a => PrettyElem a

instance Pretty PrettyElem where
  prettyDoc (PrettyElem a) = prettyDoc a

instance Pretty Int where
  prettyDoc = P.int

instance Pretty Integer where
  prettyDoc = P.integer

instance Pretty Char where
  prettyDoc = P.quotes . P.char

instance Pretty a => Pretty (Maybe a) where
  prettyDoc (Just a) = prettyDoc a
  prettyDoc Nothing  = P.empty

tupleDoc :: [PrettyElem] -> Doc
tupleDoc = P.parens .
           P.sep .
           P.punctuate P.comma .
           map prettyDoc

instance (Pretty a, Pretty b) => Pretty (a, b) where
  prettyDoc (a, b) = tupleDoc [PrettyElem a, PrettyElem b]

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  prettyDoc (a, b, c) =
    tupleDoc [ PrettyElem a
             , PrettyElem b
             , PrettyElem c
             ]

instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
  prettyDoc (a, b, c, d) =
    tupleDoc [ PrettyElem a
             , PrettyElem b
             , PrettyElem c
             , PrettyElem d
             ]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e) => Pretty (a, b, c, d, e) where
  prettyDoc (a, b, c, d, e) =
    tupleDoc [ PrettyElem a
             , PrettyElem b
             , PrettyElem c
             , PrettyElem d
             , PrettyElem e
             ]

instance ( Pretty a
         , Pretty b
         , Pretty c
         , Pretty d
         , Pretty e
         , Pretty f
         ) => Pretty (a, b, c, d, e, f) where
  prettyDoc (a, b, c, d, e, f) =
    tupleDoc [ PrettyElem a
             , PrettyElem b
             , PrettyElem c
             , PrettyElem d
             , PrettyElem e
             , PrettyElem f
             ]

instance ( Pretty a
         , Pretty b
         , Pretty c
         , Pretty d
         , Pretty e
         , Pretty f
         , Pretty g
         ) => Pretty (a, b, c, d, e, f, g) where
  prettyDoc (a, b, c, d, e, f, g) =
    tupleDoc [ PrettyElem a
             , PrettyElem b
             , PrettyElem c
             , PrettyElem d
             , PrettyElem e
             , PrettyElem f
             , PrettyElem g
             ]

instance ( Pretty a
         , Pretty b
         , Pretty c
         , Pretty d
         , Pretty e
         , Pretty f
         , Pretty g
         , Pretty h
         ) => Pretty (a, b, c, d, e, f, g, h) where
  prettyDoc (a, b, c, d, e, f, g, h) =
    tupleDoc [ PrettyElem a
             , PrettyElem b
             , PrettyElem c
             , PrettyElem d
             , PrettyElem e
             , PrettyElem f
             , PrettyElem g
             , PrettyElem h
             ]

instance Pretty a => Pretty [a] where
  prettyDoc =
      P.brackets .
      P.sep .
      P.punctuate P.comma .
      map prettyDoc

instance Pretty String where
  prettyDoc = P.doubleQuotes . P.text

instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
  prettyDoc =
      P.braces .
      P.sep .
      P.punctuate P.comma .
      map prettyPair .
      Map.toAscList
    where
      prettyPair (k, v) = P.hang (prettyDoc k <> P.char ':')
                                 4
                                 (prettyDoc v)

instance Pretty a => Pretty (Set.Set a) where
  prettyDoc s =
      P.char '#' <>
      (P.braces .
       P.sep .
       P.punctuate P.comma .
       map prettyDoc $
       Set.toList s)

