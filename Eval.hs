module Eval where

import Utils
import Board
import Data.Char (toUpper)

kingDown :: State -> Bool
kingDown = (foldr hasKing False) . captured
    where hasKing p b = b || isKing p
          isKing      = isSameType King

-- |Calculates the Numerical value of the board
boardValue :: Board -> Int
boardValue board = white - black
    where (white,black) = analyseBoard board

analyseBoard :: Board -> (Int, Int)
analyseBoard = foldl addValue (0,0) . concat
    where addValue val = (positionPlus val) . evalTile

evalTile :: Tile -> (Int, Int)
evalTile Nothing = (0,0)
evalTile (Just (Piece pc pt)) = if pc == White
                                    then (val,0)
                                    else (0,val)
    where val = maybe 0 id (lookup pt valueMap)

valueMap :: [(PType, Int)]
valueMap = [(Pawn,1)
           ,(Knight,3)
           ,(Bishop,3)
           ,(Rook,5)
           ,(Queen,9)
           ,(King,1000)]
