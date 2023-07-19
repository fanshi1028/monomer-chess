module Chess.EnPassant where

import Chess.Board (ChessBoard, Column, Position (Position), Row (R4, R5), deleteFromChessBoard, insertToChessBoard, lookupChessBoard)
import Chess.Board.Util (safePred, safeSucc)
import Chess.Piece (ChessPiece (ChessPiece), ChessPieceType (Pawn), Moved (Moved))
import Control.Monad (guard)
import Data.Maybe (catMaybes)
import Player (Player (..))

newtype EnPassant = EnPassant Position deriving (Eq, Show)

generateEnPassantPositions :: Column -> ChessBoard -> Player -> [(EnPassant, Position, ChessBoard)]
generateEnPassantPositions enPassantCol chessBoard player = do
  let (forward, y) = case player of
        Player1 -> (succ, R5)
        Player2 -> (pred, R4)
  x <- catMaybes [safePred enPassantCol, safeSucc enPassantCol]
  let pos = Position x y
      pos' = Position enPassantCol $ forward y
      pawn = ChessPiece player $ Pawn Moved
  guard $ Just pawn == lookupChessBoard pos chessBoard
  pure
    ( EnPassant pos,
      pos',
      insertToChessBoard pos' pawn
        . deleteFromChessBoard (Position enPassantCol y)
        $ deleteFromChessBoard pos chessBoard
    )
