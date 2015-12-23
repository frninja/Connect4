{- Connect4 Game 
    by 
     Oleg Batashov
     Nina Antonova
     Irina Prikhodko
-}

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
