{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module Pretty (
    Pretty (..)
  , PrettyElem (..)

  , red
  , redBold
  , redForeground
  , redBlink
  , redUnderline

  , green
  , greenBold
  , greenForeground
  , greenBlink
  , greenUnderline

  , yellow
  , yellowBold
  , yellowForeground
  , yellowBlink
  , yellowUnderline

  , blue
  , blueBold
  , blueForeground
  , blueBlink
  , blueUnderline

  , purple
  , purpleBold
  , purpleForeground
  , purpleBlink
  , purpleUnderline

  , module Text.PrettyPrint
  ) where

import Text.PrettyPrint
import qualified Text.PrettyPrint as P
import qualified Data.Map as Map
import qualified Data.Set as Set

--------------------

redColor :: P.Doc
redColor = P.text "\x1B[31"

greenColor :: P.Doc
greenColor = P.text "\x1B[32"

yellowColor :: P.Doc
yellowColor = P.text "\x1B[33"

blueColor :: P.Doc
blueColor = P.text "\x1B[34"

purpleColor :: P.Doc
purpleColor = P.text "\x1B[35"

resetColor :: P.Doc
resetColor = P.text "\x1B[0m"

plainCode :: P.Doc
plainCode = P.text "m"

blinkCode :: P.Doc
blinkCode = P.text ";5m"

foregroundCode :: P.Doc
foregroundCode = P.text ";7m"

boldCode :: P.Doc
boldCode = P.text ";1m"

underlineCode :: P.Doc
underlineCode = P.text ";4m"


red, redBold, redForeground, redBlink, redUnderline :: P.Doc -> P.Doc
red p = redColor <> plainCode <> p <> resetColor
redBold p = redColor <> boldCode <> p <> resetColor
redForeground p = redColor <> foregroundCode <> p <> resetColor
redBlink p = redColor <> blinkCode <> p <> resetColor
redUnderline p = redColor <> underlineCode <> p <> resetColor

green, greenBold, greenForeground, greenBlink, greenUnderline :: P.Doc -> P.Doc
green p = greenColor <> plainCode <> p <> resetColor
greenBold p = greenColor <> boldCode <> p <> resetColor
greenForeground p = greenColor <> foregroundCode <> p <> resetColor
greenBlink p = greenColor <> blinkCode <> p <> resetColor
greenUnderline p = greenColor <> underlineCode <> p <> resetColor

yellow, yellowBold, yellowForeground, yellowBlink, yellowUnderline :: P.Doc -> P.Doc
yellow p = yellowColor <> plainCode <> p <> resetColor
yellowBold p = yellowColor <> boldCode <> p <> resetColor
yellowForeground p = yellowColor <> foregroundCode <> p <> resetColor
yellowBlink p = yellowColor <> blinkCode <> p <> resetColor
yellowUnderline p = yellowColor <> underlineCode <> p <> resetColor

blue, blueBold, blueForeground, blueBlink, blueUnderline :: P.Doc -> P.Doc
blue p = blueColor <> plainCode <> p <> resetColor
blueBold p = blueColor <> boldCode <> p <> resetColor
blueForeground p = blueColor <> foregroundCode <> p <> resetColor
blueBlink p = blueColor <> blinkCode <> p <> resetColor
blueUnderline p = blueColor <> underlineCode <> p <> resetColor

purple, purpleBold, purpleForeground, purpleBlink, purpleUnderline :: P.Doc -> P.Doc
purple p = purpleColor <> plainCode <> p <> resetColor
purpleBold p = purpleColor <> boldCode <> p <> resetColor
purpleForeground p = purpleColor <> foregroundCode <> p <> resetColor
purpleBlink p = purpleColor <> blinkCode <> p <> resetColor
purpleUnderline p = purpleColor <> underlineCode <> p <> resetColor

--------------------

class Pretty a where
  prettyShow :: a -> String
  prettyShow = P.render . prettyDoc

  prettyStyle :: P.Style -> a -> String
  prettyStyle st = P.renderStyle st . prettyDoc

  prettyDoc :: a -> P.Doc

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

tupleDoc :: [PrettyElem] -> P.Doc
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

instance (Pretty a
         , Pretty b
         , Pretty c
         , Pretty d
         , Pretty e) => Pretty (a, b, c, d, e) where
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

