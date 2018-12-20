module Data.DataFrame where

import Prelude

import Control.Apply (lift3)
import Data.Compactable (class Compactable, separate)
import Data.DataFrame.Helper (class RecToRow, recToRow)
import Data.Filterable (class Filterable, partitionMap)
import Data.Foldable (intercalate, maximum, surround)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), catMaybes, head, mapMaybe, partition, sortBy, take, transpose)
import Data.List (filter, length) as List
import Data.List.Types (List)
import Data.Map (Map)
import Data.Map (toUnfoldable) as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid (power)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.String (length)
import Data.String.NonEmpty (NonEmptyString, toString)
import Data.Tuple (fst, snd)
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

getColumns :: List (List String) -> String
getColumns (Cons x y) = (surround " | " x) <> "\n"
getColumns Nil = ""

instance showRecordDataFrame ::
  ( RowToList a rl
  , RecToRow rl a (Map NonEmptyString String)
  ) => Show (DataFrame (Record a)) where
  show rec = show $ (over DataFrame (map (recToRow (RLProxy :: RLProxy rl))) rec :: DataFrame (Row String))

instance showUntypedDataFrame :: Show (DataFrame (Map NonEmptyString String)) where
  show =
    unwrap >>>
    lift3 (\title summary remaining -> title <> summary <> remaining) column (take 10 >>> showSummary) showRemainingCount
    where
    getField f = map (Map.toUnfoldable >>> sortBy (compare `on` fst)) >>> transpose >>> map (drawColumn f) >>> transpose
    column = getField (fst >>> toString) >>> getColumns
    getValue = (flip intercalate <*> (head >>> maybe 0 length >>> power "-" >>> append "\n"))
    showSummary = getField snd
      >>> map (surround " | " >>> append "\n")
      >>> getValue
    drawColumn f xs =
      map
        (f >>> padTo (max (maxLengthOfValues xs) (maybe 0 (fst >>> show >>> length) (head xs))))
        xs
    maxLengthOfValues =
      map (snd >>> show >>> length)
      >>> maximum
      >>> fromMaybe 0
    padTo :: Int -> String -> String
    padTo i a =
      let textPad = i - length a
          padVal = textPad / 2
          padL = power " " padVal
          padR = power " " $ textPad - padVal
      in padL <> a <> padR
    showRemainingCount =
      List.length >>>
      (_ - 10) >>>
      ((\l -> if _ then "\n... and " <> show l <> " more" else mempty) <*> (_ > 0))
