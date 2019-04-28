module Lib where

import qualified Data.Map.Strict as Map
import qualified Data.RangeSet.Map as RSet
import Data.Char 

newtype Ranges = Ranges { getRanges :: Map.Map GeneralCategory (RSet.RSet Char) }
  deriving (Eq, Ord, Show)

instance Semigroup Ranges where
  (Ranges m0) <> (Ranges m1) = Ranges $ Map.unionWith (<>) m0 m1

instance Monoid Ranges where
  mempty = Ranges Map.empty 

singleton :: Char -> Ranges
singleton c = Ranges $ Map.singleton (generalCategory c) (RSet.singleton c)

unicodeCharacterRanges :: Ranges
unicodeCharacterRanges = foldMap singleton [minBound..]

letters :: RSet.RSet Char
letters = RSet.fromList (filter isLetter [minBound..])
