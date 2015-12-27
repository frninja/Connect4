import Data.Map
import Data.Set
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad
import Graphics.Gloss.Game (bmp)
import Data.Maybe
import Data.List 
import System.Random

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
  deriving (Show, Eq)

{- Game State -}
data GameState = GameState Field Turn
  deriving (Show, Eq)

{- Moves -}
type Move = Int -- field column

possibleMoves :: GameState -> [Move]
possibleMoves gs@(GameState field _) 
  | isNothing $ winner gs = Data.List.map fst $ Data.List.filter (\(_, x) -> EmptyCell `elem` x)
                                    $ zip [0..gameFieldWidth - 1] field
  | otherwise = []
  
makeMove :: GameState -> Maybe Move -> GameState
makeMove gs Nothing = gs
makeMove gs@(GameState field currentTurn) (Just moveCol)
    | notEmptyCount == gameFieldHeight = gs
    | not $ elem moveCol $ possibleMoves gs = gs
    | otherwise = GameState (pre++newCol++post) (nextTurn currentTurn) --GameState updatedField (nextTurn currentTurn)
    where
        nextTurn PlayerTurn = AiTurn
        nextTurn AiTurn = PlayerTurn
        cellTurn PlayerTurn = PlayerCell
        cellTurn AiTurn = AiCell
        fieldUI = field2fieldUI field
        
        updatedField = fieldUI2field $ Data.Map.insert (moveCol, freeRowNum) (cellTurn currentTurn) fieldUI
        freeRowNum = fst $ head $ dropWhile cond (zip [0..gameFieldHeight - 1] (field !! moveCol))
          where
            cond (_, EmptyCell)  = False
            cond (_, PlayerCell) = True
            cond (_, AiCell)     = True
        
        col = field !! moveCol
        notEmptyElems = _notEmptyElems   col
        notEmptyCount = length notEmptyElems
        pre = take moveCol field
        post = reverse $ take (gameFieldWidth - moveCol - 1) $ reverse field
        --newCol = [(replicate (gameFieldHeight - notEmptyCount - 1) EmptyCell) ++ [cellTurn currentTurn] ++ notEmptyElems]
        newCol = [ notEmptyElems ++ [cellTurn currentTurn] ++ (replicate (gameFieldHeight - notEmptyCount - 1) EmptyCell)] 

_notEmptyElems col = Data.List.filter (\x -> case x of 
                                        EmptyCell -> False
                                        _          -> True) col
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
	    
        (horisMin, horisMax) = minMaxFind $ concatMap (Data.List.map mysum) $ Data.List.map group $ transpose field
        (vertMin, vertMax) = minMaxFind $ concatMap (Data.List.map mysum) $ Data.List.map group field
        (leftDownRightUpMin, leftDownRightUpMax) = minMaxFind $ concatMap (Data.List.map mysum) $ Data.List.map group $ transpose $ skew field
        (leftUpRightDownMin, leftUpRightDownMax) =  minMaxFind $ concatMap (Data.List.map mysum) $ Data.List.map group $ transpose $ skew $ reverse field 

allNotEmpty :: Field -> Bool
allNotEmpty f = all (lineAllNotEmpty) f

lineAllNotEmpty :: [FieldCell] -> Bool
lineAllNotEmpty [] = True
lineAllNotEmpty (EmptyCell:fcs) = False
lineAllNotEmpty (_:fcs) = True && (lineAllNotEmpty fcs)

mysum :: [FieldCell]->Int
mysum [] = 0
mysum (EmptyCell:fcs) = mysum fcs
mysum (PlayerCell:fcs) = 1+(mysum fcs)
mysum (AiCell:fcs) = (mysum fcs)-1 

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
_eval field (Nothing) = sum ( zipWith (*) foundation (Data.List.map mysum $ field))
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
    moves               = bestMove : (Data.List.delete bestMove posMoves)
    initABSDataVal      = (depth, (minBound::Int) `div` 4, (maxBound::Int) `div` 4, head moves, False)
    foldABSData         = zip moves $ replicate (length moves) gs
    (_, _, _, move, _)  = Data.List.foldl alphaBetaSearchUpdate initABSDataVal foldABSData
    
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
      (_, _, _, val, _) = Data.List.foldl alphaBetaSearchCalcScoreUpdate initVal toFold
        
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

main :: IO ()
main = do
   gen <- getStdGen
   startGame gen
 
fieldSize@(fieldWidth, fieldHeight) = (7,7) :: (Int, Int)
mineCount = 0 :: Int
 
createField :: FieldUI
createField = Data.Map.fromList(arrowLine ++ initBoard)

arrowLine :: [(Cell, FieldCell)] 
arrowLine = [((x, gameFieldHeight),ArrowCell) | x<-[0..gameFieldWidth-1]]

initBoard :: [(Cell, FieldCell)]
initBoard = [((x, y), EmptyCell) | x<-[0..gameFieldWidth-1], y<-[0..gameFieldHeight-1]]
 
type FieldUI = Map Cell FieldCell
type Cell = (Int, Int)
 
data GameStateUI = GS
    { field    :: FieldUI
    , current_turn :: Turn
    }
   deriving (Show, Eq) 
    
-- Converter for GameState
gsUI2gs :: GameStateUI -> GameState
gsUI2gs (GS fieldUI turn) = GameState (fieldUI2field fieldUI) turn


gs2gsUI :: GameState -> GameStateUI
gs2gsUI (GameState field turn) = GS (field2fieldUI field) turn


-- Converter for Field
fieldUI2field :: FieldUI -> Field
fieldUI2field fieldUI = 
  [ [fromJust $ Data.Map.lookup (x, y) fieldUI | y <- [0..gameFieldHeight - 1] ] | x <- [0..gameFieldWidth -1]]

field2fieldUI :: Field -> FieldUI
field2fieldUI field = Data.Map.fromList $ (Data.Map.toList workField) ++ arrowLine
  where
    workField = snd $ Data.List.foldl rowsFold (0, Data.Map.empty) field
    rowsFold (rowNum, acc) cols = (rowNum + 1, snd $ Data.List.foldl inserter (0, acc) cols)
      where
        inserter (colNum, acc) col = (colNum + 1, Data.Map.insert (rowNum, colNum) col acc)


-- Нужно инициализировать все, кроме стрелок(Nothing) на EmptyCell
startGame :: StdGen -> IO ()
startGame gen = play (InWindow "Connect4" (550,600) (200,0)) white 30 (initState gen) renderer handler updater
windowSize = both (* (round cellSize)) fieldSize
cellSize = 80 :: Float
 
initState gen = GS createField PlayerTurn
 
both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b) 
 
updater _ = id
 
cellToScreen = both ((* cellSize) . fromIntegral)

--Здесь: нажатие на верхние стрелки :
--		 проверка соответствующего столбца, поиск незанятой самой нижней ячейки и ее заполнение
-- 		 далее вызов функции, которая ставит ход Ai (rendere)
handler (EventKey (MouseButton LeftButton) Down _ mouse) gs@GS
    { field = field,
       current_turn = current_turn
    } 
    | snd coord == gameFieldHeight = --case Data.Map.lookup coord field of
        let ngs = (makeMove (gsUI2gs gs) (Just $ fst coord)) in gs2gsUI $ makeMove ngs (alphaBetaSearch ngs 2)
	    --Nothing -> gs { 
        --  field = Data.Map.insert coord PlayerCell field, current_turn = AiTurn }
        --Just _ -> gs { 
        --  field =  Data.Map.insert coord AiCell field, current_turn = AiTurn }
        where coord = screenToCell mouse
handler _ gs = gs
screenToCell = both (round . (/ cellSize)) . invertViewPort viewPort 

-- Сейчас только вывод по Map соответствующих картинок
-- добавить ход вызов хода компа после рендеринга
renderer GS { field = field } = applyViewPortToPicture viewPort $ pictures $ cells ++ grid where
    grid = [uncurry translate (cellToScreen (x, y)) $ color black $ rectangleWire cellSize cellSize | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1]]
    cells = [uncurry translate (cellToScreen (x, y)) $ drawCell x y | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1]]
    drawCell x y = case Data.Map.lookup (x, y) field of
        Just ArrowCell         -> pictures [ color red $ rectangleSolid cellSize cellSize
                                    , scale 0.4 0.4 $ color black $ text "->" --  bmp "arrow.bmp"
                                    ]
        Just EmptyCell      -> pictures [ color red $ rectangleSolid cellSize cellSize
                                    , scale 0.4 0.4 $ color red $ text "+" --bmp "empty.bmp"
                                    ]
        Just PlayerCell      -> pictures [ color yellow $ rectangleSolid cellSize cellSize
                                    ,  scale 0.4 0.4 $ color green $ text "P" --bmp "white.bmp"
                                    ]
        Just AiCell       -> pictures [ color yellow $ rectangleSolid cellSize cellSize
                                    ,  scale 0.4 0.4 $ color blue $ text "A" --bmp "black.bmp"
                                    ]
    --label = translate (-5) (-5) . scale 0.15 0.15 . color black . text
 
viewPort = ViewPort (both (negate . (/ 2) . (subtract cellSize)) $ cellToScreen fieldSize) 0 1
