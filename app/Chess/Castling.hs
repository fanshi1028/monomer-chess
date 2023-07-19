module Chess.Castling where

import Chess.Board (ChessBoard, Column (..), Position (Position), Row (R1, R8), deleteFromChessBoard, insertToChessBoard, lookupChessBoard)
import Chess.Check (findPosThreats)
import Chess.Piece (ChessPiece (ChessPiece), ChessPieceType (King, Rook), Moved (..))
import Control.Monad (guard)
import Data.Maybe (maybeToList)
import Player (Player (..))

data Castling = ShortCastling | LongCastling deriving (Eq, Show)

generateCastlingPositions :: ChessBoard -> Player -> [(Castling, Position, ChessBoard)]
generateCastlingPositions chessBoard player = do
  let y = case player of
        Player1 -> R1
        Player2 -> R8
  guard $ Just (ChessPiece player $ King Unmoved) == lookupChessBoard (Position E y) chessBoard
  (rookCol, range, castling, newKingCol, newRookCol) <- [(A, [B .. D], LongCastling, C, D), (H, [F, G], ShortCastling, G, F)]
  guard $ Just (ChessPiece player $ Rook Unmoved) == lookupChessBoard (Position rookCol y) chessBoard
  let pieces = do
        x <- range
        maybeToList $ lookupChessBoard (Position x y) chessBoard
  let threats = do
        x <- range
        findPosThreats (Position x y) chessBoard player
  guard $ pieces == []
  guard $ findPosThreats (Position E y) chessBoard player == []
  guard $ threats == []
  pure $
    ( castling,
      Position newKingCol y,
      insertToChessBoard (Position newRookCol y) (ChessPiece player $ Rook Moved)
        . insertToChessBoard (Position newKingCol y) (ChessPiece player $ King Moved)
        . deleteFromChessBoard (Position rookCol y)
        $ deleteFromChessBoard (Position E y) chessBoard
    )
