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

{- Game State -}
data GameState = GameState Field Turn

{- Moves -}
type Move = Int -- field column

possibleMoves :: GameState -> [Move]
possibleMoves gs@(GameState [cols] _) 
  | isNothing $ winner gs = Data.List.map fst $ Data.List.filter (\(_, x) -> case x of
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
        notEmptyElems = Data.List.filter (\x -> case x of 
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
    , score_human :: Int
    , score_ai :: Int
    , current_turn :: Turn
    }

-- Нужно инициализировать все, кроме стрелок(Nothing) на EmptyCell
startGame :: StdGen -> IO ()
startGame gen = play (InWindow "Connect4" (550,600) (200,0)) white 30 (initState gen) renderer handler updater
windowSize = both (* (round cellSize)) fieldSize
cellSize = 80 :: Float
 
initState gen = GS createField 0 0  PlayerTurn
 
both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b) 
 
updater _ = id
 
cellToScreen = both ((* cellSize) . fromIntegral)

--Здесь: нажатие на верхние стрелки :
--		 проверка соответствующего столбца, поиск незанятой самой нижней ячейки и ее заполнение
-- 		 далее вызов функции, которая ставит ход Ai (rendere)
handler (EventKey (MouseButton LeftButton) Down _ mouse) gs@GS
    { field = field
    } 
    | fst coord >= gameFieldWidth -1 && snd coord >= gameFieldHeight - 1 = case Data.Map.lookup coord field of
        Nothing -> gs { 
          field = Data.Map.insert coord PlayerCell field 
          , score_human = 0
          , score_ai = 0
          , current_turn = AiTurn }
        Just _ -> gs { 
          field =  Data.Map.insert coord AiCell field
          , score_human = 0
          , score_ai = 0
          , current_turn = AiTurn }
          --_ -> gs
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
                                    , scale 0.4 0.4 $ bmp "arrow.bmp"
                                    ]
        Just EmptyCell      -> pictures [ color red $ rectangleSolid cellSize cellSize
                                    , scale 0.4 0.4 $ bmp "empty.bmp"
                                    ]
        Just PlayerCell      -> pictures [ color yellow $ rectangleSolid cellSize cellSize
                                    ,  scale 0.4 0.4 $ bmp "white.bmp"
                                    ]
        Just AiCell       -> pictures [ color yellow $ rectangleSolid cellSize cellSize
                                    ,  scale 0.4 0.4 $ bmp "black.bmp"
                                    ]
    label = translate (-5) (-5) . scale 0.15 0.15 . color black . text
 
viewPort = ViewPort (both (negate . (/ 2) . (subtract cellSize)) $ cellToScreen fieldSize) 0 1
