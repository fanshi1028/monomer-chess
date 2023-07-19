module Chess.End where

import Player (Player)

data Stalemate = Stalemate deriving (Eq, Show)

newtype Win = Win Player deriving (Eq, Show)
