{- Connect4 Game 
    by 
     Oleg Batashov
     Nina Antonova
     Irina Prikhodko
-}

import Data.Maybe
import Data.List

{- Game Field -}

{- Field globals -}
gameFieldWidth  = 7
gameFieldHeight = 6

{- Field cell -}
data FieldCell = EmptyCell | PlayerCell | AiCell
    deriving (Show, Eq)
	
data Winner = AI | Player | Draw
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
  | isNothing $ winner gs = map fst $ filter (\(_, x) -> case x of
                                                  EmptyCell    -> True
                                                  _             -> False)
                                    $ zip [0..gameFieldWidth] cols
  | otherwise = []
  
makeMove :: GameState -> Maybe Move -> GameState
makeMove gs Nothing = gs
makeMove gs@(GameState field currentTurn) (Just moveCol)
    | notEmptyCount == gameFieldHeight = gs
    | not $ elem moveCol $ possibleMoves gs = gs
    | otherwise = GameState (pre ++ newCol ++ post) (nextTurn currentTurn)
    where
        nextTurn PlayerTurn = AiTurn
        nextTurn AiTurn = PlayerTurn
        col = field !! moveCol
        notEmptyElems = filter (\x -> case x of 
                                        EmptyCell -> False
                                        _          -> True) col
        notEmptyCount = length notEmptyElems
        pre = take moveCol field
        post = reverse $ take (gameFieldWidth - moveCol - 1) $ reverse field
        newCol = [(replicate (gameFieldHeight - notEmptyCount - 1) EmptyCell)]

winner :: GameState -> Maybe Winner
winner (GameState field _) 
    | horisMin <= -4 = Just AI
    | vertMin <= -4 = Just AI
    | leftUpRightDownMin <= -4 = Just AI
    | leftDownRightUpMin <= -4 = Just AI
    | horisMax >= 4 = Just Player
    | vertMax >= 4 = Just Player
    | leftUpRightDownMax >= 4 = Just Player
    | leftDownRightUpMax >= 4 = Just Player
    | allNotEmpty field = Just Draw
    | otherwise = Nothing
    where
	    
        (horisMin, horisMax) = minMaxFind $ concatMap (map msum) $ map group $ transpose field
        (vertMin, vertMax) = minMaxFind $ concatMap (map msum) $ map group field
        (leftDownRightUpMin, leftDownRightUpMax) = minMaxFind $ concatMap (map msum) $ map group $ transpose $ skew field
        (leftUpRightDownMin, leftUpRightDownMax) =  minMaxFind $ concatMap (map msum) $ map group $ transpose $ skew $ reverse field 

allNotEmpty :: Field -> Bool
allNotEmpty = undefined

msum :: [FieldCell] -> Int
msum = undefined

minMaxFind :: (Ord a, Eq a)=>[a]->(a,a)
minMaxFind = undefined

skew :: Field -> Field
skew = undefined