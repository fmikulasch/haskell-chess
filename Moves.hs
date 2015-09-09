module Moves (isValidMove, reachablePositions) where

import Board
import Utils

-- |Determines whether a move is executable or not
isValidMove :: Position -> Position -> Board -> PColor -> Bool
isValidMove from to board pColor =  to `elem` (reachablePositions board from)
                                 && not (isOponent pColor board from)


-- |Creates a List of all reachable Positions from a position on a board
reachablePositions :: Board -> Position -> [Position]
reachablePositions board pos@(x,y) = filter (\p -> isOnBoard p && differentColor p)
                                            positions
    where tile           = getTile board pos
          differentColor = not . (isSameColor tile) . (getTile board)
          positions      = reachablePositions' board tile pos

reachablePositions' :: Board -> Tile -> Position -> [Position]
-- If Black Pawn or White Pawn stands on baseline he con move two fields
reachablePositions' board (Just (Piece Black Pawn)) pos@(x,1) =
    takeWhile (not . (isOponent Black board)) [(x,2),(x,3)]
    ++ filter (isOponent Black board) (map (positionPlus pos) [(1,1),(-1,1)])
reachablePositions' board (Just (Piece White Pawn)) pos@(x,6) =
    takeWhile (not . (isOponent White board)) [(x,5),(x,4)]
    ++ filter (isOponent White board) (map (positionPlus pos) [(1,-1),(-1,-1)])
-- Normal movement
reachablePositions' board tile@(Just (Piece pc pt)) pos@(x,y) = case pt of
    Rook   -> iterateMovement board pos (movement tile)
    Queen  -> iterateMovement board pos (movement tile)
    Bishop -> iterateMovement board pos (movement tile)
    Pawn   -> filter (isOponent pc board)
                     (map (positionPlus (positionPlus pos (head (movement tile))))
                          [(1,0),(-1,0)]) -- Can only capture diagonally
           ++ filter (not . (isOponent pc board))
                     (map (positionPlus pos) (movement tile)) -- Can only move straight
    _      -> map (positionPlus pos) (movement tile)
reachablePositions' _     Nothing                    _        = []

-- For Pieces that can move to infinity
iterateMovement :: Board -> Position -> [Position] -> [Position]
iterateMovement _ _ [] = []
iterateMovement board pos (m:ms) = takeWhileInclusive movable (drop 1 (iterate (positionPlus m) pos))
                                ++ iterateMovement board pos ms
    where movable p = getTile board p == Nothing && isOnBoard p

diagonal, straight :: [Position]
diagonal = [(-1,-1),(-1,1),(1,-1),(1,1)]
straight = [(-1,0),(1,0),(0,-1),(0,1)]

movement :: Tile -> [Position]
movement (Just (Piece White Pawn)) = [(0,-1)]
movement (Just (Piece Black Pawn)) = [(0,1)]
movement (Just (Piece _   Knight)) = [(a,b) | a <- q, b <- q, (a + b) `mod` 2 /= 0]
    where q = [-2..2]
movement (Just (Piece _     Rook)) = straight
movement (Just (Piece _   Bishop)) = diagonal
movement (Just (Piece _    Queen)) = diagonal ++ straight
movement (Just (Piece _     King)) = [(x,y) | x <- [-1,0,1], y <- [-1,0,1]
                                            , x /= 0 || y /= 0]
movement Nothing = []
