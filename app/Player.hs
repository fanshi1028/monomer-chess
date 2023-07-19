module Player where

data Player = Player1 | Player2 deriving (Eq, Ord, Show)

nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1
