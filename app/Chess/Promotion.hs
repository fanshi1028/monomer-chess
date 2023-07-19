module Chess.Promotion where

import Chess.Piece (ChessPieceType (Bishop, Knight, Queen, Rook), Moved (Moved))

pawnPromotionTargets :: [ChessPieceType]
pawnPromotionTargets = [Queen, Rook Moved, Bishop, Knight]
