{-# LANGUAGE ViewPatterns #-}

module Chess.Model where

import Chess.Board (ChessBoard, Column, Position (Position), Row (R1, R4, R5, R8), insertToChessBoard, lookupChessBoard)
import Chess.Castling (Castling, generateCastlingPositions)
import Chess.Check (findKingThreats, findPosThreats, generateValidNonCastlingMoves)
import Chess.End (Stalemate (Stalemate), Win (Win))
import Chess.Piece (ChessPiece (ChessPiece), ChessPieceType (King, Pawn), Moved (Moved, Unmoved), chessPieceToEmoji)
import Chess.Promotion (pawnPromotionTargets)
import Control.Applicative ((<|>))
import Control.Lens (makeLenses, (%~), (.~), (^.))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Base (ListF (..))
import Data.Functor.Foldable (Recursive (cata))
import Data.List.NonEmpty as NE (NonEmpty, filter, toList)
import Data.Map.Lazy as Map (Map, null, (!?))
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Text (pack)
import Monomer
import Monomer.Lens (HasA (a), HasH (h), HasW (w))
import Player (Player (..), nextPlayer)

data ChessModel = ChessModel
  { _currentPlayerState :: Either (Either Stalemate Win) (Player, Map (Position, ChessPiece) (NonEmpty (Position, (Maybe ChessPiece, ChessBoard)))),
    _chessBoard :: ChessBoard,
    _pickedChessPiece :: Maybe ((Position, ChessPiece), [(Position, (Maybe ChessPiece, ChessBoard))]),
    _promotingPawnPosition :: Maybe Position,
    _enPassantColumn :: Maybe Column,
    _chessBoardScale :: Double
  }
  deriving (Eq, Show)

makeLenses 'ChessModel

data ChessEvent
  = ClickedSquare Position
  | ChessPieceDragDrop (Position, ChessPiece) Position
  | ChoseChessPiece (Position, ChessPiece)
  | UnchoseChessPiece (Position, ChessPiece)
  | ChessPieceMove (Position, ChessPiece) Position ChessBoard
  | CastlingMove Castling ChessBoard
  | PawnPromotion Position ChessPieceType
  | ChessBoardResize Rect
  | EndTurn
  deriving (Eq, Show)

buildChessUI :: WidgetEnv ChessModel ChessEvent -> ChessModel -> WidgetNode ChessModel ChessEvent
buildChessUI _wenv (ChessModel eCurrentPlayer _chessBoard _pickedChessPiece _promotingPawnPosition _enPassantColumn _chessBoardScale) =
  let buidldChessPieceUI chessPiece = label (chessPieceToEmoji chessPiece) `styleBasic` [textColor black, textSize $ 65 * _chessBoardScale]
   in vstack
        [ hgrid $ do
            x@(fromEnum -> x') <- [minBound .. maxBound :: Column]
            pure . vgrid $ do
              y@(fromEnum -> y') <- [maxBound, pred maxBound .. minBound :: Row]
              let pos = Position x y
                  mChessPiece = lookupChessBoard pos _chessBoard
                  color = a .~ 0.8 $ case (x' + y') `mod` 2 of
                    0 -> green
                    _ -> beige
                  buidldChessPieceDraggableUI chessPiece = draggable (pos, chessPiece) $ buidldChessPieceUI chessPiece
              pure . dropTarget (flip ChessPieceDragDrop pos) $
                box_
                  (($ ClickedSquare pos) <$> [onClickEmpty, onClick])
                  (widgetMaybe mChessPiece buidldChessPieceDraggableUI)
                  `nodeKey` (pack $ show pos)
                  `styleBasic` [ bgColor $ case mChessPiece of
                                   Just (ChessPiece owner (King _)) | (_ : _) <- findPosThreats pos _chessBoard owner -> red
                                   _ ->
                                     let mix a' b = (a' + 3 * b) `div` 4
                                         sumColor (Color r g b _) (Color r' g' b' _) = Color (mix r r') (mix g g') (mix b b') 0.8
                                         colorAlg = \case
                                           Nil -> Nothing
                                           Cons (pos', (mPiece, _)) mColor
                                             | pos' == pos ->
                                                 Just $
                                                   color `sumColor` case mPiece of
                                                     Nothing -> cyan
                                                     Just _ -> orange
                                             | otherwise -> mColor
                                      in fromMaybe color $ _pickedChessPiece >>= \(_, validMoves') -> cata colorAlg validMoves',
                                 border (5 * _chessBoardScale) $ case _pickedChessPiece of
                                   Just (((pos ==) -> True, _), _) -> pink
                                   _ -> transparent
                               ],
          case eCurrentPlayer of
            Right (player, _) ->
              popupD_
                (WidgetValue (isJust _promotingPawnPosition))
                [popupDisableClose, popupOpenAtCursor]
                $ hstack
                  ( pawnPromotionTargets <&> \target -> case _promotingPawnPosition of
                      Just pos -> box_ (($ PawnPromotion pos target) <$> [onClick, onClickEmpty]) . buidldChessPieceUI $ ChessPiece player target
                      Nothing -> spacer `nodeVisible` False
                  )
                  `styleBasic` [bgColor brown]
            Left result ->
              popupD_ (WidgetValue True) [popupDisableClose, alignMiddle, alignCenter, popupAlignToWindow] $
                ( label . pack $ case result of
                    Left r -> show r
                    Right r -> show r
                )
                  `styleBasic` [bgColor brown]
        ]

handleChessEvent :: WidgetEnv ChessModel ChessEvent -> WidgetNode ChessModel ChessEvent -> ChessModel -> ChessEvent -> [EventResponse ChessModel ChessEvent sp ep]
handleChessEvent _wenv _node (ChessModel (Left _) _ _ _ _ _) = const []
handleChessEvent _wenv _node model@(ChessModel (Right (currentPlayer, validMoves)) _chessBoard _pickedChessPiece _promotingPawnPosition _enPassantColumn _) = \case
  ClickedSquare pos -> case _pickedChessPiece of
    Nothing -> case lookupChessBoard pos _chessBoard of
      Just chessPiece@(ChessPiece ((== currentPlayer) -> True) _) -> pure . Event $ ChoseChessPiece (pos, chessPiece)
      _ -> []
    Just (picked@(pickedPos, _), validMoves')
      | pickedPos == pos -> pure . Event $ UnchoseChessPiece picked
      | otherwise ->
          let validMoveChessBoardAlg = \case
                Nil -> Nothing
                Cons (validPos, (_, updatedChessBoard)) r
                  | validPos == pos -> Just updatedChessBoard
                  | otherwise -> r
           in case cata validMoveChessBoardAlg validMoves' of
                Nothing -> pure $
                  case lookupChessBoard pos _chessBoard of
                    Just chessPiece@(ChessPiece ((== currentPlayer) -> True) _) ->
                      Model $ model & pickedChessPiece .~ Just ((pos, chessPiece), maybe [] NE.toList $ validMoves !? (pos, chessPiece))
                    _ -> Event $ UnchoseChessPiece picked
                Just updatedChessBoard -> pure . Event $ ChessPieceMove picked pos updatedChessBoard
  ChessPieceDragDrop dragFrom@(_, chessPiece@(ChessPiece _ chessPieceType)) dropTo -> case chessPiece of
    ChessPiece ((== currentPlayer) -> True) _ ->
      let moves =
            validMoves !? dragFrom >>= \moves' ->
              case NE.filter ((== dropTo) . fst) moves' of
                [] -> Nothing
                (_, (_, updatedChessBoard)) : _ -> pure . Event $ ChessPieceMove dragFrom dropTo updatedChessBoard
          alg = \case
            Nil -> Nothing
            Cons (castling, pos', updatedChessBoard) r
              | pos' == dropTo -> pure . Event $ CastlingMove castling updatedChessBoard
              | otherwise -> r
          castlingMoves = case chessPieceType of
            King Unmoved -> cata alg $ generateCastlingPositions _chessBoard currentPlayer
            _ -> Nothing
       in maybeToList $ moves <|> castlingMoves
    _ -> []
  ChessPieceMove (_, (ChessPiece _ chessPieceType)) moveToPos@(Position x y) updatedChessBoard ->
    let model' =
          model
            & chessBoard .~ updatedChessBoard
            & pickedChessPiece .~ Nothing
            & enPassantColumn .~ Nothing
        simpleCase = [Model model', Event EndTurn]
     in case chessPieceType of
          Pawn Moved ->
            if (currentPlayer == Player1 && y == R8) || (currentPlayer == Player2 && y == R1)
              then [Model $ model' & promotingPawnPosition .~ Just moveToPos]
              else simpleCase
          Pawn Unmoved ->
            if (currentPlayer == Player1 && y == R4) || (currentPlayer == Player2 && y == R5)
              then [Model $ model' & enPassantColumn .~ Just x, Event EndTurn]
              else simpleCase
          _ -> simpleCase
  ChoseChessPiece chessPieceOnBoard@(_, (ChessPiece owner chessPieceType)) ->
    let helper (_, pos, updatedChessBoard) = (pos, (Nothing, updatedChessBoard))
        specialMoves = case chessPieceType of
          King Unmoved -> helper <$> generateCastlingPositions _chessBoard currentPlayer
          _ -> []
     in pure . responseIf (owner == currentPlayer) . Model $
          model & pickedChessPiece .~ Just (chessPieceOnBoard, specialMoves <> maybe [] NE.toList (validMoves !? chessPieceOnBoard))
  UnchoseChessPiece _ -> [Model $ model & pickedChessPiece .~ Nothing]
  ChessBoardResize rect -> [Model $ model & chessBoardScale .~ min (rect ^. w / 800) (rect ^. h / 600)]
  PawnPromotion pos chessPieceType ->
    [ Model $
        model
          & chessBoard %~ insertToChessBoard pos (ChessPiece currentPlayer chessPieceType)
          & promotingPawnPosition .~ Nothing,
      Event EndTurn
    ]
  CastlingMove _ updatedChessBoard -> [Model $ model & chessBoard .~ updatedChessBoard, Event EndTurn]
  EndTurn -> do
    let player' = nextPlayer currentPlayer
        moves = generateValidNonCastlingMoves player' _chessBoard _enPassantColumn
    pure . Model $
      if Map.null moves
        then
          model
            & currentPlayerState
              .~ Left
                ( case findKingThreats _chessBoard player' of
                    [] -> Left $ Stalemate
                    _ : _ -> Right $ Win currentPlayer
                )
        else model & currentPlayerState .~ Right (nextPlayer currentPlayer, moves)
