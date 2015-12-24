{-# LANGUAGE LambdaCase #-}

{- Connect4 Game 
    by 
     Oleg Batashov
     Nina Antonova
     Irina Prikhodko
-}

import Data.Maybe

{- Game Field -}

{- Field globals -}
gameFieldWidth  = 7
gameFieldHeight = 6

{- Field cell -}
data FieldCell = EmptyField | PlayerField | AiField
    deriving (Show, Eq)

type Field = [[FieldCell]]

{- Turn -}
data Turn = PlayerTurn | AiTurn

{- Game State -}
data GameState = GameState Field Turn

{- Moves -}
type Move = Int -- field column

possibleMoves :: GameState -> [Move]
possibleMoves gs@(GameState [cols] _) 
  | isNothing $ winner gs = map fst $ filter (\case  
                                                 (_,EmptyField) -> True
                                                  _          -> False)
                                    $ zip [0..gameFieldWidth] cols
  | otherwise = []
  

winner :: GameState -> Maybe FieldCell
winner = undefined

