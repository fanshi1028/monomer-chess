module Chess.Piece where

import Data.Text (Text)
import Player (Player (..))

data Moved = Unmoved | Moved deriving (Eq, Ord, Show)

data EnPassantable = EnPassantable deriving (Eq, Show)

data ChessPieceType = Knight | Pawn Moved | King !Moved | Queen | Bishop | Rook !Moved deriving (Eq, Ord, Show)

data ChessPiece = ChessPiece !Player !ChessPieceType deriving (Eq, Ord, Show)

moved :: ChessPiece -> ChessPiece
moved (ChessPiece owner chessPieceType) = ChessPiece owner $ case chessPieceType of
  King Unmoved -> King Moved
  Pawn Unmoved -> Pawn Moved
  Rook Unmoved -> Rook Moved
  others -> others

chessPieceToEmoji :: ChessPiece -> Text
chessPieceToEmoji (ChessPiece player chessPieceType) = case player of
  Player1 ->
    case chessPieceType of
      Knight -> "♘"
      Pawn _ -> "♙"
      King _ -> "♔"
      Queen -> "♕"
      Bishop -> "♗"
      Rook _ -> "♖"
  Player2 ->
    case chessPieceType of
      Knight -> "♞"
      Pawn _ -> "♟" -- "♟️"
      King _ -> "♚"
      Queen -> "♛"
      Bishop -> "♝"
      Rook _ -> "♜"
