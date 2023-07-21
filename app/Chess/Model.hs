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
import Data.Functor.Base (ListF (..))
import Data.Functor.Foldable (Recursive (cata))
import Data.List.NonEmpty as NE (NonEmpty, filter, toList)
import Data.Map.Lazy as Map (Map, null, (!?))
import Data.Maybe (fromMaybe, isNothing, maybeToList)
import Data.Text (pack)
import Monomer
import Monomer.Lens (HasA (a), HasH (h), HasW (w))
import Player (Player (..), nextPlayer)

data ChessModel = ChessModel
  { _currentPlayerState :: Either (Either Stalemate Win) (Player, Map (Position, ChessPiece) (NonEmpty (Position, (Maybe ChessPiece, ChessBoard)))),
    _chessBoard :: ChessBoard,
    _pickedChessPiece :: Maybe ((Position, ChessPiece), [(Position, (Maybe ChessPiece, ChessBoard))]),
    _promotingPawn :: Maybe (Position, Maybe ChessPieceType),
    _enPassantColumn :: Maybe Column,
    _chessBoardScale :: Double
  }
  deriving (Eq, Show)

makeLenses 'ChessModel

data ChessEvent
  = -- NOTE: UI-wise
    ClickedSquare Position
  | ChessPieceDragDrop (Position, ChessPiece) Position
  | ChessBoardResize Rect
  | -- NOTE: gameplay-wise
    ChessPieceMove (Either Castling ((Position, ChessPiece), Position)) ChessBoard
  | PawnPromotion Position ChessPieceType
  | EndTurn
  deriving (Eq, Show)

buildChessUI :: WidgetEnv ChessModel ChessEvent -> ChessModel -> WidgetNode ChessModel ChessEvent
buildChessUI _wenv (ChessModel eCurrentPlayer _chessBoard _pickedChessPiece _promotingPawn _enPassantColumn _chessBoardScale) = vstack [chessBoardUI, popups]
  where
    buidldChessPieceUI chessPiece = label (chessPieceToEmoji chessPiece) `styleBasic` [textColor black, textSize $ 65 * _chessBoardScale]
    chessBoardUI = hgrid $ do
      x@(fromEnum -> x') <- [minBound .. maxBound :: Column]
      pure . vgrid $ do
        y@(fromEnum -> y') <- [maxBound, pred maxBound .. minBound :: Row]
        let pos = Position x y
            mChessPiece = lookupChessBoard pos _chessBoard
            -- NOTE: basic chessbaord square color
            color = a .~ 0.8 $ case (x' + y') `mod` 2 of
              0 -> green
              _ -> beige
            buidldChessPieceDraggableUI chessPiece = draggable (pos, chessPiece) $ buidldChessPieceUI chessPiece
        pure . dropTarget (flip ChessPieceDragDrop pos) $
          box_
            ([onClickEmpty, onClick] <*> [ClickedSquare pos])
            (widgetMaybe mChessPiece buidldChessPieceDraggableUI)
            `styleBasic` [ bgColor $ case mChessPiece of
                             -- NOTE: checked King on the square
                             Just (ChessPiece owner (King _)) | (_ : _) <- findPosThreats pos _chessBoard owner -> red
                             _ ->
                               let mix a' b = (a' + 3 * b) `div` 4
                                   sumColor (Color r g b _) (Color r' g' b' _) = Color (mix r r') (mix g g') (mix b b') 0.8
                                   colorAlg = \case
                                     Nil -> Nothing
                                     Cons (pos', (mPiece, _)) mColor
                                       -- NOTE: valid potential move
                                       | pos' == pos ->
                                           Just $
                                             color `sumColor` case mPiece of
                                               Nothing -> cyan
                                               -- NOTE: when threatening other piece
                                               Just _ -> orange
                                       | otherwise -> mColor
                                in fromMaybe color $ _pickedChessPiece >>= \(_, validMoves') -> cata colorAlg validMoves',
                           -- NOTE: show border depends on if it is chosen
                           border (5 * _chessBoardScale) $ case _pickedChessPiece of
                             Just (((pos ==) -> True, _), _) -> pink
                             _ -> transparent
                         ]
    popups = case eCurrentPlayer of
      -- NOTE: for choosing what pawn promotiing to
      Right (player, _) -> widgetMaybe _promotingPawn $ \(pos, mTarget) ->
        popupD_ (WidgetValue (isNothing mTarget)) [popupDisableClose, popupOpenAtCursor] $
          hstack
            ( do
                target <- pawnPromotionTargets
                pure . box_ ([onClick, onClickEmpty] <*> [PawnPromotion pos target]) . buidldChessPieceUI $ ChessPiece player target
            )
            `styleBasic` [bgColor brown]
      -- NOTE: for displaying end game result
      Left result ->
        popupD_ (WidgetValue True) [popupDisableClose, alignMiddle, alignCenter, popupAlignToWindow] $
          (label . pack $ either show show result)
            `styleBasic` [bgColor brown]

handleChessEvent :: WidgetEnv ChessModel ChessEvent -> WidgetNode ChessModel ChessEvent -> ChessModel -> ChessEvent -> [EventResponse ChessModel ChessEvent sp ep]
handleChessEvent _wenv _node (ChessModel (Left _) _ _ _ _ _) = const [] -- NOTE: Game ended.
handleChessEvent _wenv _node model@(ChessModel (Right (currentPlayer, validMoves)) _chessBoard _pickedChessPiece _promotingPawnPosition _enPassantColumn _) = \case
  ClickedSquare pos -> case _pickedChessPiece of
    Just (picked@(pickedPos, _), validMoves')
      -- NOTE: clicked on the chosen chess piece, unchoose it.
      | pickedPos == pos -> unchoseChessPiece
      | otherwise ->
          let validMoveChessBoardAlg = \case
                Nil -> Nothing
                Cons (validPos, (_, updatedChessBoard)) r
                  | validPos == pos -> Just updatedChessBoard
                  | otherwise -> r
           in case cata validMoveChessBoardAlg validMoves' of
                -- NOTE: no valid move to the clicked square for the chosen piece
                Nothing -> case lookupChessBoard pos _chessBoard of
                  Just chessPiece@(ChessPiece ((== currentPlayer) -> True) _) -> pickTheChessPiece chessPiece
                  _ -> unchoseChessPiece
                -- NOTE: valid move!
                Just updatedChessBoard -> pure . Event $ ChessPieceMove (Right (picked, pos)) updatedChessBoard
    Nothing -> case lookupChessBoard pos _chessBoard of -- NOTE: no chess piece is chosen yet.
      Just chessPiece@(ChessPiece ((== currentPlayer) -> True) _) -> pickTheChessPiece chessPiece
      _ -> []
    where
      pickTheChessPiece chessPiece@(ChessPiece _ chessPieceType) =
        let moves = maybe [] NE.toList (validMoves !? (pos, chessPiece))
            castlingMoves = case chessPieceType of
              King Unmoved -> do
                (_, pos', updatedChessBoard) <- generateCastlingPositions _chessBoard currentPlayer
                pure (pos', (Nothing, updatedChessBoard))
              _ -> []
         in [Model $ model & pickedChessPiece .~ Just ((pos, chessPiece), castlingMoves <> moves)]
      unchoseChessPiece = [Model $ model & pickedChessPiece .~ Nothing]
  ChessPieceDragDrop dragFrom@(_, chessPiece@(ChessPiece _ chessPieceType)) dropTo -> case chessPiece of
    ChessPiece ((== currentPlayer) -> True) _ ->
      let mMatchingValidMoves =
            NE.filter ((== dropTo) . fst) <$> validMoves !? dragFrom >>= \case
              [] -> Nothing
              -- NOTE: valid move matched!
              (_, (_, updatedChessBoard)) : _ -> pure . Event $ ChessPieceMove (Right (dragFrom, dropTo)) updatedChessBoard
          alg = \case
            Nil -> Nothing
            Cons (castling, pos', updatedChessBoard) r
              -- NOTE: valid castling move matched!
              | pos' == dropTo -> pure . Event $ ChessPieceMove (Left castling) updatedChessBoard
              | otherwise -> r
          mCastlingMoves = case chessPieceType of
            King Unmoved -> cata alg $ generateCastlingPositions _chessBoard currentPlayer
            _ -> Nothing
       in maybeToList $ mMatchingValidMoves <|> mCastlingMoves
    -- NOTE: can't dragdrop other player's chess piece!
    _ -> []
  ChessPieceMove move updatedChessBoard ->
    let model' =
          model
            & chessBoard .~ updatedChessBoard
            & pickedChessPiece .~ Nothing
            & enPassantColumn .~ Nothing
        simplyMove = [Model $ model', Event EndTurn] -- NOTE: simply move and end turn.
     in case move of
          Left _ -> simplyMove
          Right ((_, (ChessPiece _ chessPieceType)), moveToPos@(Position x y)) ->
            let forPromote = [Model $ model' & promotingPawn .~ Just (moveToPos, Nothing)]
                forEnPassant = [Model $ model' & enPassantColumn .~ Just x, Event EndTurn]
             in case (chessPieceType, currentPlayer, y) of
                  -- NOTE: pawn promotion!!!
                  (Pawn Moved, Player1, R8) -> forPromote
                  (Pawn Moved, Player2, R1) -> forPromote
                  -- NOTE: pawn first moved for two squares forward, need to consider en passant for next player(if any).
                  (Pawn Unmoved, Player1, R4) -> forEnPassant
                  (Pawn Unmoved, Player2, R5) -> forEnPassant
                  _ -> simplyMove
  ChessBoardResize rect -> [Model $ model & chessBoardScale .~ min (rect ^. w / 800) (rect ^. h / 600)]
  PawnPromotion pos chessPieceType ->
    [ Model $ model & promotingPawn .~ Just (pos, Just chessPieceType), -- NOTE: TEMP FIXME close the popup overlay before remove it (by updating the model)
      Model $
        model
          & chessBoard %~ insertToChessBoard pos (ChessPiece currentPlayer chessPieceType)
          & promotingPawn .~ Nothing,
      Event EndTurn
    ]
  EndTurn -> pure . Model $ do
    let player' = nextPlayer currentPlayer
        moves = generateValidNonCastlingMoves player' _chessBoard _enPassantColumn
    model
      & currentPlayerState
        .~ ( if Map.null moves
               then -- NOTE: no valid move for the opponent
               Left $ case findKingThreats _chessBoard player' of
                 [] -> Left $ Stalemate
                 _ : _ -> Right $ Win currentPlayer
               else Right (nextPlayer currentPlayer, moves)
           )
