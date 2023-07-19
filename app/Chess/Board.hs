{-# LANGUAGE ViewPatterns #-}

module Chess.Board where

import Chess.Piece (ChessPiece)
import Control.Category ((>>>))
import Data.IntMap.Strict (IntMap, alter, delete, insert, singleton)
import Data.IntMap.Strict as Map (update, (!?))

data Column = A | B | C | D | E | F | G | H deriving (Eq, Ord, Show, Bounded, Enum)

data Row = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 deriving (Eq, Ord, Show, Bounded, Enum)

-- instance Show Row where
--   show = show . fromEnum

data Position = Position !Column !Row
  deriving (Eq, Ord, Show, Bounded)

newtype ChessBoard = ChessBoard {unChessBoard :: IntMap (IntMap ChessPiece)} deriving (Eq, Semigroup, Monoid)

-- TEMP
instance Show ChessBoard where
  show _ = "CHESSBOARD"

lookupChessBoard :: Position -> ChessBoard -> Maybe ChessPiece
lookupChessBoard (Position x y) (ChessBoard board) = board !? fromEnum x >>= (!? fromEnum y)

deleteFromChessBoard :: Position -> ChessBoard -> ChessBoard
deleteFromChessBoard (Position (fromEnum -> x) (fromEnum -> y)) (ChessBoard board) =
  ChessBoard $
    update
      ( delete y >>> \case
          column
            | null column -> Nothing
            | otherwise -> Just column
      )
      x
      board

insertToChessBoard :: Position -> ChessPiece -> ChessBoard -> ChessBoard
insertToChessBoard (Position (fromEnum -> x) (fromEnum -> y)) chessPiece (ChessBoard board) =
  ChessBoard $
    alter
      ( Just . \case
          Nothing -> singleton y chessPiece
          Just column -> insert y chessPiece column
      )
      x
      board
