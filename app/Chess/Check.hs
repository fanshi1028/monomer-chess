{-# LANGUAGE ViewPatterns #-}

module Chess.Check where

import Chess.Board (ChessBoard, Column, Position)
import Chess.Board.Util (chessPiecesCoCVAlg)
import Chess.EnPassant (EnPassant (EnPassant), generateEnPassantPositions)
import Chess.Move (chessPieceGenerateAttackPosition, chessPieceGenerateMovePosition)
import Chess.Piece (ChessPiece (ChessPiece), ChessPieceType (King, Pawn), Moved (Moved))
import Control.Comonad (Comonad (extract))
import Data.Functor.Base (ListF (..))
import Data.Functor.Foldable (Recursive (cata), chrono)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map.Lazy (Map, insert, unionWith)
import Player (Player)

findPositionThreatAlg ::
  ChessBoard ->
  Player ->
  Maybe Position ->
  ListF (Position, ChessPiece) (Maybe (Position, [(Position, ChessPiece)])) ->
  Maybe (Position, [(Position, ChessPiece)])
findPositionThreatAlg chessBoard player mPos = \case
  Nil -> (,[]) <$> mPos
  Cons ele@(pos, chessPiece@(ChessPiece owner _)) result
    | owner == player -> result
    | otherwise ->
        let possibleAttacks = chessPieceGenerateAttackPosition chessBoard pos chessPiece
         in case result of
              Just (pos', threats) ->
                if any ((== pos') . fst) possibleAttacks
                  then Just (pos', ele : threats)
                  else result
              Nothing ->
                let findCheckedKingPosAlg = \case
                      Nil -> Nothing
                      Cons (kingPos, Just (ChessPiece ((== player) -> True) (King _))) _ -> Just kingPos
                      Cons _ mKingPos -> mKingPos
                 in (,[ele]) <$> cata findCheckedKingPosAlg possibleAttacks

findThreats :: Maybe Position -> ChessBoard -> Player -> [(Position, ChessPiece)]
findThreats mPos chessBoard player = maybe [] snd $ chrono (findPositionThreatAlg chessBoard player mPos . fmap extract) chessPiecesCoCVAlg chessBoard

findKingThreats :: ChessBoard -> Player -> [(Position, ChessPiece)]
findKingThreats = findThreats Nothing

findPosThreats :: Position -> ChessBoard -> Player -> [(Position, ChessPiece)]
findPosThreats pos = findThreats $ Just pos

validMovesAlg :: Player -> ListF (Position, (Maybe ChessPiece, ChessBoard)) [(Position, (Maybe ChessPiece, ChessBoard))] -> [(Position, (Maybe ChessPiece, ChessBoard))]
validMovesAlg player = \case
  Nil -> []
  Cons move@(_, (_, updatedChessBoard)) r
    | findKingThreats updatedChessBoard player == [] -> move : r
    | otherwise -> r

generateValidNonCastlingMoves :: Player -> ChessBoard -> Maybe Column -> Map (Position, ChessPiece) (NonEmpty (Position, (Maybe ChessPiece, ChessBoard)))
generateValidNonCastlingMoves player chessBoard mCol = unionWith (<>) validEnPassantMoves (chrono (chessPieceWithValidMoveAlg . fmap extract) chessPiecesCoCVAlg chessBoard)
  where
    alg = \case
      Nil -> mempty
      Cons (EnPassant pos, pos', updatedChessBoard) mp
        | findKingThreats updatedChessBoard player == [] ->
            insert (pos, ChessPiece player (Pawn Moved)) (pure (pos', (Nothing, updatedChessBoard))) mp
        | otherwise -> mp
    validEnPassantMoves = case mCol of
      Just col -> cata alg $ generateEnPassantPositions col chessBoard player
      Nothing -> mempty
    chessPieceWithValidMoveAlg = \case
      Nil -> mempty
      Cons chessPieceOnBoard@(pos, chessPiece@(ChessPiece owner _)) r
        | owner == player -> case nonEmpty . cata (validMovesAlg player) $ chessPieceGenerateMovePosition chessBoard pos chessPiece of
            Nothing -> r
            Just validMoves -> insert chessPieceOnBoard validMoves r
        | otherwise -> r
