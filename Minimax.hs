module Minimax where

import Board
import Eval
import Moves
import Utils

-- |Datatype Gametree contains possible moves
data Gametree = Gametree {state :: State, tree :: [Gametree]}


-- |Defines how many moves are calculated
searchdepth :: Int
searchdepth = 3

getNextMove :: PColor -> State -> Move
getNextMove color = snd . (findBestMove color) . (genGameTree searchdepth color)


{-
   |Creates Gametree with possible movements
    (State contains a board and the move to reach the board
-}
genGameTree :: Int -> PColor -> State -> Gametree
genGameTree 0 _ state = Gametree state []
genGameTree n color state@(board,_) = Gametree state newTrees
    where positions = filter (isPlayerPiece board color) tileNumbers
          newStates = concat $ map (genStates board) positions
          newTrees  = map (genGameTree (n - 1) (oponentColor color)) newStates

genStates :: Board -> Position -> [State]
genStates board pos = map createState endPositions
    where endPositions     = reachablePositions board pos
          createState pos2 = (move pos2 board, move pos2)
          move             = updateBoard pos

-- |Chooses a move depending on a color and a gametree using minimax
findBestMove :: PColor -> Gametree -> (Int, Move)
findBestMove color (Gametree state gs) = extremum values
    where moveValueTuple g@(Gametree (_,move) _) = (evalGametree color g, move)
          values   = map moveValueTuple gs
          extremum = if color == White then tupleMaximum else tupleMinimum

-- |Minimax evaluation of a gametree results in the total value of the tree
evalGametree :: PColor -> Gametree -> Int
evalGametree _     (Gametree (board,_) []) = boardValue board
evalGametree color (Gametree (board,_) gs) =
    boardValue board + comp (map (evalGametree (oponentColor color)) gs)
    where comp = if color == White then minimum else maximum -- Takes the minimum of oponent and maximum of own moves
