{-# LANGUAGE ViewPatterns #-}

module Chess.Move where

import Chess.Board (ChessBoard, Position (Position), deleteFromChessBoard, insertToChessBoard, lookupChessBoard)
import Chess.Board.Util (safePred, safeSucc)
import Chess.Piece (ChessPiece (ChessPiece), ChessPieceType (..), Moved (..), moved)
import Control.Monad (guard)
import Data.Functor.Base (ListF (Cons, Nil))
import Data.Functor.Foldable (Corecursive (ana), coelgot)
import Data.Maybe (catMaybes)
import Player (Player (..))

chessPieceGenerateMovePosition' :: Bool -> ChessBoard -> Position -> ChessPiece -> [(Position, Maybe ChessPiece)]
chessPieceGenerateMovePosition' attackOnly chessBoard pos@(Position x y) (ChessPiece player chessPieceType) = case chessPieceType of
  King _ -> do
    let helper z = catMaybes [safePred z, Just z, safeSucc z]
    pos' <- Position <$> helper x <*> helper y
    guard $ pos' /= pos
    checkPositionHelper pos'
  Knight -> do
    let helper1 z = catMaybes [safePred z, safeSucc z]
        helper2 z = catMaybes [safePred z >>= safePred, safeSucc z >>= safeSucc]
    (f, g) <- [(helper1, helper2), (helper2, helper1)]
    Position <$> f x <*> g y >>= checkPositionHelper
  Rook _ -> do
    let helper pos' = case lookupChessBoard pos' chessBoard of
          Nothing -> Cons (pos', Nothing) $ Just pos'
          Just chessPiece@(ChessPiece owner _)
            | player == owner -> Nil
            | otherwise -> Cons (pos', Just chessPiece) Nothing
        rookMoveCoalgY = do
          (bound, next) <- [(maxBound, succ), (minBound, pred)]
          pure $ \case
            Nothing -> Nil
            Just (Position x' y')
              | y' == bound -> Nil
              | otherwise -> helper . Position x' $ next y'
        rookMoveCoalgX = do
          (bound, next) <- [(maxBound, succ), (minBound, pred)]
          pure $ \case
            Nothing -> Nil
            Just (Position x' y')
              | x' == bound -> Nil
              | otherwise -> helper $ Position (next x') y'
    coalg <- rookMoveCoalgY <> rookMoveCoalgX
    ana coalg $ Just pos
  Bishop -> do
    (xBound, xNext) <- [(maxBound, succ), (minBound, pred)]
    (yBound, yNext) <- [(maxBound, succ), (minBound, pred)]
    let coalg Nothing = Nil
        coalg (Just (Position x' y'))
          | x' == xBound || y' == yBound = Nil
          | otherwise =
              let pos' = Position (xNext x') (yNext y')
               in case lookupChessBoard pos' chessBoard of
                    Nothing -> Cons (pos', Nothing) $ Just pos'
                    Just chessPiece@(ChessPiece owner _)
                      | player == owner -> Nil
                      | otherwise -> Cons (pos', Just chessPiece) Nothing
    ana coalg $ Just pos
  Queen -> [Rook Moved, Bishop] >>= chessPieceGenerateMovePosition' attackOnly chessBoard pos . ChessPiece player
  Pawn moved' -> do
    let forward = case player of
          Player1 -> safeSucc
          Player2 -> safePred
    y' <- catMaybes [forward y]
    let attackPositions = do
          x' <- catMaybes [safeSucc x, safePred x]
          let pos' = Position x' y'
          case lookupChessBoard pos' chessBoard of
            Just chessPiece@(ChessPiece ((== player) -> False) _) -> pure (pos', Just chessPiece)
            _ -> []
        forwardStepCoalg Nothing = Nil
        forwardStepCoalg (Just (pos'@(Position x' y''), step :: Int)) = case lookupChessBoard pos' chessBoard of
          Nothing -> Cons pos' $ (,step + 1) . Position x' <$> forward y''
          Just _ -> Nil
        coelgotAlg (_, Nil) = attackPositions
        coelgotAlg (Just (_, step), Cons pos' forwardPositions) = case (moved', step) of
          (Unmoved, 2) -> attackPositions
          (Moved, 1) -> attackPositions
          _ -> (pos', Nothing) : forwardPositions
        coelgotAlg (_, Cons pos' forwardPositions) = (pos', Nothing) : forwardPositions
     in coelgot coelgotAlg forwardStepCoalg . Just $ (Position x y', 0)
  where
    checkPositionHelper pos' = case lookupChessBoard pos' chessBoard of
      Nothing -> pure (pos', Nothing)
      Just chessPiece@(ChessPiece owner _) -> if player == owner then [] else pure $ (pos', Just chessPiece)

chessPieceGenerateMovePosition :: ChessBoard -> Position -> ChessPiece -> [(Position, (Maybe ChessPiece, ChessBoard))]
chessPieceGenerateMovePosition chessBoard pos chessPiece = do
  r@(moveTo, _) <- chessPieceGenerateMovePosition' False chessBoard pos chessPiece
  let updatedChessBoard = insertToChessBoard moveTo (moved chessPiece) $ deleteFromChessBoard pos chessBoard
  pure $ (,updatedChessBoard) <$> r

chessPieceGenerateAttackPosition :: ChessBoard -> Position -> ChessPiece -> [(Position, Maybe ChessPiece)]
chessPieceGenerateAttackPosition = chessPieceGenerateMovePosition' True
