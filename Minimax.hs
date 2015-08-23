module Minimax where

import Board
import Eval
import Moves
import Utils

-- |Datatype Gametree contains possible moves
data Gametree = Gametree {treeState :: TreeState, tree :: [Gametree]}
type TreeState = (Board, Move)

-- |Defines how many moves are calculated
searchdepth :: Int
searchdepth = 3

getNextMove :: State -> Move
getNextMove state = getNextMove' color (board, move)
    where board = stateBoard state
          color = turnOf state
          move  = lastMove state

getNextMove' :: PColor -> (Board, Move) -> Move
getNextMove' color = (findBestMove color) . (genGameTree searchdepth color)

{-
   |Creates Gametree with possible movements 
    (State contains a board and the move to reach the board
-}

genGameTree :: Int -> PColor -> TreeState -> Gametree
genGameTree 0 _     s@(_,_) = Gametree s []
genGameTree n color s@(b,_) = Gametree s newTrees
    where positions = filter (isPlayerPiece b color) tileNumbers
          newStates = concat $ map (genBoards b) positions
          newTrees  = map (genGameTree (n - 1) (oponentColor color)) newStates

genBoards :: Board -> Position -> [TreeState]
genBoards board from = map createState endPositions
    where endPositions   = reachablePositions board from
          createState to = (move to, (from,to))
          move to        = updateBoard from to board

-- |Chooses the median of all best moves (so the pieces in the middle move first)
findBestMove :: PColor -> Gametree -> Move
findBestMove color gt = snd (moves !! mid)
    where moves = findBestMoves color gt
          mid   = (length moves) `div` 2

-- |Computes best moves depending on a color and a gametree using minimax
findBestMoves :: PColor -> Gametree -> [(Int, Move)]
findBestMoves color (Gametree state gs) = extrema values
    where moveValueTuple g@(Gametree (_, move) _) = (evalGametree color g, move)
          values  = map moveValueTuple gs
          extrema = if color == White then tupleMaxima else tupleMinima

-- |Minimax evaluation of a gametree results in the total value of the tree
evalGametree :: PColor -> Gametree -> Int
evalGametree _     (Gametree (board,_) []) = boardValue board
evalGametree color (Gametree (board,_) gs) = boardValue board + comp (map (evalGametree (oponentColor color)) gs)
    where comp = if color == White then minimum else maximum -- Takes the minimum of oponent and maximum of own moves

