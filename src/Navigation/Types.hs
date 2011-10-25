module Navigation.Types where

import Data.Ord (Ord(..))
import Data.Set (Set)

-------------------------------------------------------------------------------

data NavEvent a
  = NavEvent {
    nvVal            :: a
  , nvParent         :: Maybe a
  , nvCost           :: Integer
  , nvAlreadyVisited :: Bool
  , nvVisited        :: Set a
  , nvFrontier       :: Set (Integer, a, Maybe a)
  }
  deriving (Ord, Eq)

instance Show a => Show (NavEvent a) where
  show = show . nvVal

--------------------

newtype NavNode a 
  = NavNode { fromNavNode :: (Integer, a, Maybe a) }
  deriving (Eq)

instance Ord a => Ord (NavNode a) where
  (NavNode (i0, a0, _)) `compare` (NavNode (i1, a1,_)) = 
      compare (i0, a0) (i1, a1)

