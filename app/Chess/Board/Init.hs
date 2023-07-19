{-# LANGUAGE ViewPatterns #-}

module Chess.Board.Init where

import Chess.Board (ChessBoard, Column (..), Position (Position), Row (R1, R2, R7, R8), insertToChessBoard)
import Chess.Move (chessPieceGenerateMovePosition)
import Chess.Piece (ChessPiece (ChessPiece), ChessPieceType (..), Moved (..))
import Data.Functor.Base (ListF (Cons, Nil))
import Data.Functor.Foldable (Recursive (cata))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map.Lazy (Map, insert)
import Player (Player (..))

initPawns :: [(Position, ChessPiece)]
initPawns = do
  x <- [minBound .. maxBound]
  (player, y) <- [(Player1, R2), (Player2, R7)]
  pure $ (Position x y, ChessPiece player (Pawn Unmoved))

initBacklines :: [(Position, ChessPiece)]
initBacklines = do
  (pieceType, x) <- [(Rook Unmoved, A), (Knight, B), (Bishop, C), (Queen, D), (King Unmoved, E), (Bishop, F), (Knight, G), (Rook Unmoved, H)]
  (player, y) <- [(Player1, R1), (Player2, R8)]
  pure $ (Position x y, ChessPiece player pieceType)

initChessBoard' :: ChessBoard -> [(Position, ChessPiece)] -> ChessBoard
initChessBoard' chessBoard = cata $ \case
  Nil -> chessBoard
  Cons (pos, chessPiece) chessBoard' -> insertToChessBoard pos chessPiece chessBoard'

initChessBoard :: ChessBoard
initChessBoard = mempty `initChessBoard'` initBacklines `initChessBoard'` initPawns

initChessBoardMoves :: Map (Position, ChessPiece) (NonEmpty (Position, (Maybe ChessPiece, ChessBoard)))
initChessBoardMoves = cata alg initPawns
  where
    alg = \case
      Nil -> mempty
      Cons (pos, pawn@(ChessPiece Player1 _)) r -> case nonEmpty $ chessPieceGenerateMovePosition initChessBoard pos pawn of
        Nothing -> r
        Just moves -> insert (pos, pawn) moves r
      Cons _ r -> r
