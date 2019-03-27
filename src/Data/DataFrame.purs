module Data.DataFrame where

import Prelude

import Control.Apply (lift2, lift3)
import Data.Compactable (class Compactable, separate)
import Data.DataFrame.Helper (class RecToRow, recToRow)
import Data.Filterable (class Filterable, partitionMap)
import Data.Foldable (maximum, surround)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), catMaybes, fold, head, mapMaybe, partition, take, transpose, uncons, zipWith)
import Data.List (filter, length) as List
import Data.List.Types (List)
import Data.Map (Map, size)
import Data.Map (toUnfoldable) as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid (power)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.String (length)
import Data.String.NonEmpty (NonEmptyString, toString)
import Data.Tuple (Tuple, fst, snd)
import Prim.RowList (class RowToList, kind RowList)
import Type.Row (RLProxy(..))
import Types (Row)

newtype DataFrame a = DataFrame (List a)

derive instance newtypeDF :: Newtype (DataFrame a) _

derive instance genericDF :: Generic (DataFrame a) _

derive instance functorDF :: Functor DataFrame

instance semigroupDF :: Semigroup (DataFrame a) where
  append df1 = wrap <<< on append unwrap df1

instance monoidDF :: Monoid (DataFrame a) where
  mempty = wrap mempty

instance compactbleDF :: Compactable DataFrame where
  compact = over DataFrame catMaybes
  separate = (\{left, right} -> {left: wrap left, right: wrap right}) <<< separate <<< unwrap

instance filterableDF :: Filterable DataFrame where
  filter f = over DataFrame (List.filter f)
  filterMap f = over DataFrame (mapMaybe f)
  partition f = (\{yes, no} -> {yes: wrap yes, no: wrap no}) <<< partition f <<< unwrap
  partitionMap f = (\{left, right} -> {left: wrap left, right: wrap right}) <<< partitionMap f <<< unwrap


instance showRecordDataFrame ::
  ( RowToList a rl
  , RecToRow rl a (Map NonEmptyString String)
  ) => Show (DataFrame (Record a)) where
  show rec = show $ (over DataFrame (map (recToRow (RLProxy :: RLProxy rl))) rec :: DataFrame (Row String))

instance showUntypedDataFrame :: Show (DataFrame (Map NonEmptyString String)) where
  show =
    unwrap >>> lift3 append3 column showSummary showRemainingCount

    where
      getField f =
        take 10
        >>> (<$>) (Map.toUnfoldable)
        >>> transpose
        >>> (<$>) (drawColumn f)
        >>> transpose

      column =
        getField (fst >>> toString)
        >>> getColumns
        >>> lift3
              append3
              identity
              (const "\n")
              (drawHL (flip sub 3) >>> surroundString "+")

      showSummary :: List (Row String) -> String
      showSummary = getField snd
        >>> lift2
              (zipWith (<>))
              ((<$>) (surround "| " >>> surroundString "\n"))
              (map ((map (drawHL (add 1))) >>> (surround "+")))
        >>> fold

      drawColumn :: ((Tuple NonEmptyString String) -> String)  -> List (Tuple NonEmptyString String) -> List String
      drawColumn f xs =
        map
          (f >>> padTo (max (maxLengthOfValues xs) (maybe 0 (fst >>> toString >>> length) (head xs))))
          xs

      maxLengthOfValues :: List(Tuple NonEmptyString String) -> Int
      maxLengthOfValues =
        map (snd >>> length)
        >>> maximum
        >>> fromMaybe 0

      padTo :: Int -> String -> String
      padTo i a =
        let textPad = i - length a
            padVal = textPad / 2
            padL = power " " padVal
            padR = power " " $ textPad - padVal
        in padL <> a <> " " <> padR

      showRemainingCount :: List (Row String) -> String
      showRemainingCount =
        uncons >>>
        maybe mempty
        (\{head, tail} ->
        "\n\n" <> show (List.length tail + 1) <> " rows x " <> show (size head) <> " columns")

      getColumns :: List (List String) -> String
      getColumns (Cons x y) = surround "| " x
      getColumns Nil = ""

      surroundString :: ∀ a. Semigroup a ⇒ a → a → a
      surroundString x = flip (append3 x) x

      drawHL :: (Int → Int) → String → String
      drawHL fn = length >>> fn >>> power "-"


append3 :: ∀ a. Semigroup a ⇒ a → a → a → a
append3 = ((<<<) (<<<) (<>)) >>> ((>>>) (<>))
