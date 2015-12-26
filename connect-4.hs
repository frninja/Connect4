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
data FieldCell = EmptyCell | PlayerCell | AiCell | ArrowCell
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
allNotEmpty f = all (lineAllNotEmpty) f

lineAllNotEmpty :: [FieldCell] -> Bool
lineAllNotEmpty [] = True
lineAllNotEmpty (EmptyCell:fcs) = False
lineAllNotEmpty (_:fcs) = True && (lineAllNotEmpty fcs)

msum :: [FieldCell]->Int
msum [] = 0
msum (EmptyCell:fcs) = msum fcs
msum (PlayerCell:fcs) = 1+(msum fcs)
msum (AiCell:fcs) = (msum fcs)-1 

minMaxFind :: (Ord a, Eq a)=>[a]->(a,a)
minMaxFind xs = (minimum xs, maximum xs)

skew :: Field -> Field
skew fcs = _skew fcs 0 ((length $ head fcs)-1)

_skew :: Field -> Int -> Int -> Field
_skew [] _ _ = []
_skew (fc:fcs) leftgaps rightgaps = ((replicate leftgaps EmptyCell) ++ fc ++ (replicate rightgaps EmptyCell)) : _skew fcs (leftgaps+1) (rightgaps-1)
    
eval :: GameState -> Int
eval gs@(GameState field _) = _eval field $ winner gs    
    
_eval :: Field -> Maybe Winner -> Int
_eval field (Nothing) = sum ( zipWith (*) foundation (map msum $ field))
    where  
        foundation = [1..((gameFieldWidth+1) `div` 2)] ++ (reverse [1..(gameFieldWidth `div` 2)])
_eval _ (Just Draw) = 0    
_eval _ (Just _) = (maxBound::Int) `div` 2

--(depth, alpha, beta, move, failFast)
type ABSearchData = (Int, Int, Int, Move, Bool)

alphaBetaSearch :: GameState -> Int -> Maybe Move
alphaBetaSearch gs depth
  | length posMoves == 0 = Nothing
  | depth <= 0 = Just $ head posMoves
  | isNothing $ winner gs = Just move
  | otherwise = Nothing
  where
    posMoves            = possibleMoves gs
    bestMove            = if depth <= 0 then head posMoves else fromJust $ alphaBetaSearch gs $ depth - 1
    moves               = bestMove : (delete bestMove posMoves)
    initABSDataVal      = (depth, (minBound::Int) `div` 4, (maxBound::Int) `div` 4, head moves, False)
    foldABSData         = zip moves $ replicate (length moves) gs
    (_, _, _, move, _)  = foldl alphaBetaSearchUpdate initABSDataVal foldABSData
    
alphaBetaSearchUpdate :: ABSearchData -> (Move, GameState) -> ABSearchData
alphaBetaSearchUpdate absdata@(_, _, _, _, True) _ = absdata
alphaBetaSearchUpdate absdata@(depth, alpha, beta, bestMove, False) (move, gs)
    | score >= beta = (depth, alpha, beta, move, True)
    | score > alpha = (depth, score, beta, move, False)
    | otherwise = absdata
    where
        score = -(alphaBetaSearchCalcScore (makeMove gs (Just move)) (-beta) (-alpha) (depth-1))
        
alphaBetaSearchCalcScore :: GameState -> Int -> Int -> Int -> Int
alphaBetaSearchCalcScore gs@(GameState _ turn) _ _ 0 = (eval gs) * case turn of
                                                                     AiTurn     -> -1
                                                                     PlayerTurn -> 1
alphaBetaSearchCalcScore gs alpha beta depth = val
    where
      posMoves          = possibleMoves gs
      initVal           = (depth, alpha, beta, alpha, False)
      toFold            = zip posMoves $ replicate (length posMoves) gs
      (_, _, _, val, _) = foldl alphaBetaSearchCalcScoreUpdate initVal toFold
        
--(depth, alpha, beta, returnVal, failFast)
type AlphaBetaSearchCalcScoreData = (Int, Int, Int, Int, Bool)
alphaBetaSearchCalcScoreUpdate :: AlphaBetaSearchCalcScoreData -> (Move, GameState) -> AlphaBetaSearchCalcScoreData
alphaBetaSearchCalcScoreUpdate abscsdata@(_, _, _, _, True) _ = abscsdata
alphaBetaSearchCalcScoreUpdate abscsdata@(depth, alpha, beta, returnVal, False) (move, gs)
    | score >= beta = (depth, alpha, beta, score, True)
    | score > alpha = (depth, score, beta, score, False)
    | otherwise = abscsdata
    where
        score = -(alphaBetaSearchCalcScore (makeMove gs (Just move)) (-beta) (-alpha) (depth-1))