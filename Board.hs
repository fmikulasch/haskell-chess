module Board where

import Data.Char (toUpper, isUpper, toLower, isDigit)
import Data.List (elemIndex, intersperse)
import Data.Tuple (swap)

-- |Basic types and datas
type State = (Board, Move)
type Move  = Board -> Board
type Board = [[Tile]]

type Tile  = Maybe Piece
type Position = (Int,Int)

data Piece = Piece PColor PType deriving (Show, Eq)
data PColor = White | Black deriving (Show, Eq)
data PType = Pawn | Rook | Bishop | Knight | King | Queen deriving (Show, Eq)


-- |Board definition
initBoardStr :: String
initBoardStr = unlines ["rnbkqbnr"
                       ,"pppppppp"
                       ,"        "
                       ,"        "
                       ,"        "
                       ,"        "
                       ,"PPPPPPPP"
                       ,"RNBKQBNR"]

initBoard :: Board
initBoard = readBoard initBoardStr

tileNumbers :: [Position]
tileNumbers = [(x,y) | x <- [0..7], y <- [0..7]]

oponentColor :: PColor -> PColor
oponentColor Black = White
oponentColor White = Black

typeMap :: [(Char,PType)]
typeMap = [('P',Pawn)
          ,('R',Rook)
          ,('B',Bishop)
          ,('N',Knight)
          ,('K',King)
          ,('Q',Queen)]

emptyFieldChar :: Char
emptyFieldChar = ' '


-- |Functions to interact with board
updateBoard :: Position -> Position -> Board -> Board
updateBoard from to board = deleteTile from $ insertTile tile to board
    where tile = getTile board from

insertTile :: Tile -> Position -> Board -> Board
insertTile tile (x,y) board = take y board ++ [newLine] ++ drop (y + 1) board
    where line    = board !! y
          newLine = take x line ++ [tile] ++ drop (x + 1) line

deleteTile :: Position -> Board -> Board
deleteTile = insertTile Nothing

getTile :: Board -> Position -> Tile
getTile b pos@(x,y) = if isOnBoard pos
                        then (b !! y) !! x
                        else Nothing



-- |Functions for testing tiles
isOponent :: PColor -> Board -> Position -> Bool
isOponent pColor board pos = case tile of
        Nothing                 -> False
        (Just (Piece tColor _)) -> pColor /= tColor
    where tile = getTile board pos

isSameColor :: Tile -> Tile -> Bool
isSameColor (Just (Piece c1 _)) (Just (Piece c2 _)) = c1 == c2
isSameColor _ _                                     = False

isPlayerPiece :: Board -> PColor -> Position -> Bool
isPlayerPiece board pc pos = isSameColor (Just (Piece pc Pawn)) (getTile board pos)

isOnBoard :: Position -> Bool
isOnBoard = (flip elem) tileNumbers



-- |Show and read functions
readTile :: Char -> Tile
readTile = readPiece

showTile :: Tile -> Char
showTile = maybe emptyFieldChar showPiece

readBoard :: String -> Board
readBoard = map readLine . lines
    where readLine = map readTile

showBoard :: Board -> String
showBoard = unlines . map showLine
    where showLine = map showTile

prettyPrintBoard :: Board -> String
prettyPrintBoard board =
    unlines $ map (intersperse '|')
                  (intersperse linebreak (board'' ++ ["-"++['A'..'H']]))
    where board'  = lines $ showBoard board
          board'' = zipWith (++) numbers board'
          linebreak = "---------"
          numbers = map show [1..8]

readPiece :: Char -> Maybe Piece
readPiece c = fmap (Piece color) (lookup (toUpper c) typeMap)
    where color = if isUpper c then White else Black

showPiece :: Piece -> Char
showPiece (Piece pcolor ptype) = colorCase c
    where c         = maybe emptyFieldChar id typeChar
          typeChar  = lookup ptype (map swap typeMap)
          colorCase = if pcolor == White
                        then toLower
                        else id

readInputPosition :: String -> Maybe Position
readInputPosition (x:y:[]) = case pos of
                                (Just a, Just b) -> Just (a,b)
                                _                -> Nothing
    where pos = (xCord x, if isDigit y then yCord y else Nothing)
          xCord x = elemIndex (toUpper x) ['A'..'H']
          yCord y = elemIndex (read [y] :: Int) [1..8]
readInputPosition _        = Nothing

showInputPosition :: Position -> String
showInputPosition (x,y) = (['A'..] !! x) : show y
