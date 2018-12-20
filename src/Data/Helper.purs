module Data.DataFrame.Helper where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.String.NonEmpty (NonEmptyString, nes)
import Prim.Row (class Cons)
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record (get)
import Type.Prelude (class IsSymbol, RLProxy(..), SProxy(..))

class RecToRow (xs :: RowList) (row :: # Type) to | xs -> to where
  recToRow :: RLProxy xs -> {|row} -> to

instance recToRowCons ::
  ( IsSymbol sym
  , Show ty
  , Cons sym (Maybe ty) trash src
  , RecToRow tail src (Map NonEmptyString String)
  ) => RecToRow (RL.Cons sym (Maybe ty) tail) src (Map NonEmptyString String) where
  recToRow _ src =
    Map.insert (nes prop) value $ recToRow tail' src
    where
      focus = get prop src :: Maybe ty
      value = maybe "" show focus
      prop = SProxy :: SProxy sym
      tail' = RLProxy :: RLProxy tail

else instance recToRowNil :: RecToRow RL.Nil src (Map NonEmptyString String) where
  recToRow _ _ = Map.empty
