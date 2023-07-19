module Main where

import Chess.Board.Init (initChessBoard, initChessBoardMoves)
import Chess.Model (ChessEvent (ChessBoardResize), ChessModel (ChessModel), buildChessUI, handleChessEvent)
import Monomer (appFontDef, appResizeEvent, appWindowTitle, startApp)
import Player (Player (Player1))

main :: IO ()
main = do
  startApp
    (ChessModel (Right (Player1, initChessBoardMoves)) initChessBoard Nothing Nothing Nothing 1)
    handleChessEvent
    buildChessUI
    [ appWindowTitle "Chess",
      -- appFontDef "Regular" "/System/Library/Fonts/Apple Symbols.ttf"
      appFontDef "Regular" "/System/Library/Fonts/Supplemental/Arial Unicode.ttf",
      appResizeEvent ChessBoardResize
    ]
