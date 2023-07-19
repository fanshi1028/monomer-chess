{-# LANGUAGE ViewPatterns #-}

module Chess.Board.Util where

import Chess.Board (ChessBoard (ChessBoard), Position (Position))
import Chess.Piece (ChessPiece)
import Control.Monad.Free (Free (..))
import Data.Functor.Base (ListF (..))
import Data.Functor.Foldable (hylo)
import Data.IntMap.Strict (minViewWithKey)

safeSucc, safePred :: (Eq a, Bounded a, Enum a) => a -> Maybe a
safeSucc z = if z == maxBound then Nothing else Just $ succ z
safePred z = if z == minBound then Nothing else Just $ pred z

chessPiecesCoCVAlg :: ChessBoard -> ListF (Position, ChessPiece) (Free (ListF (Position, ChessPiece)) ChessBoard)
chessPiecesCoCVAlg (ChessBoard board) = case minViewWithKey board of
  Nothing -> Nil
  Just (((toEnum -> x), column), restColumns) -> case minViewWithKey column of
    -- TEMP
    Nothing -> Cons (error "y coord should exists") . Pure $ ChessBoard restColumns
    Just (((toEnum -> y), chessPiece), restYCoords) ->
      Cons (Position x y, chessPiece) $ hylo alg' coalg' restYCoords
      where
        coalg' restYCoords' = case minViewWithKey restYCoords' of
          Nothing -> Nil
          Just (((toEnum -> y'), chessPiece'), restYCoords'') -> Cons (Position x y', chessPiece') restYCoords''
        alg' = \case
          Nil -> Pure $ ChessBoard restColumns
          r -> Free r
