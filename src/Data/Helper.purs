module Data.DataFrame.Helper where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.String.NonEmpty (class MakeNonEmpty, NonEmptyString, nes)
import Prim.Row (class Cons)
import Prim.RowList (kind RowList)
import Prim.RowList as RL
import Record (get)
import Type.Prelude (class IsSymbol, RLProxy(..), SProxy(..))

class RecToRow (xs :: RowList) (row :: # Type) to | xs -> to where
  recToRow :: RLProxy xs -> {|row} -> to

instance recToRowStringCons ::
  ( IsSymbol sym
  , Cons sym (Maybe String) trash src
  , RecToRow tail src (Map NonEmptyString String)
  ) => RecToRow (RL.Cons sym (Maybe String) tail) src (Map NonEmptyString String) where
  recToRow _ src = mkMap src (SProxy :: SProxy sym) (RLProxy :: RLProxy tail) identity

else instance recToRowOtherCons ::
  ( IsSymbol sym
  , Show ty
  , Cons sym (Maybe ty) trash src
  , RecToRow tail src (Map NonEmptyString String)
  ) => RecToRow (RL.Cons sym (Maybe ty) tail) src (Map NonEmptyString String) where
  recToRow _ src = mkMap src (SProxy :: SProxy sym) (RLProxy :: RLProxy tail) show

else instance recToRowNil :: RecToRow RL.Nil src (Map NonEmptyString String) where
  recToRow _ _ = Map.empty

mkMap :: ∀ trash src ty sym rl
  . IsSymbol sym
  ⇒ Cons sym (Maybe ty) trash src
  ⇒ MakeNonEmpty sym
  ⇒ RecToRow rl src (Map NonEmptyString String)
  ⇒ { | src }
  → SProxy sym
  → RLProxy rl
  → (ty → String)
  → Map NonEmptyString String
mkMap src sym tail f = Map.insert (nes sym) value $ recToRow tail src
  where
        focus = get sym src
        value = maybe "" f focus
