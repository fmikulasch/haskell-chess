module Game (game) where

import Board
import Moves
import Eval
import Minimax
import System.IO (hFlush, stdout)

type Game = [State]

-- |Main game loop
game :: State -> IO()
game state = do
    let board = stateBoard state
        color = turnOf state
    putStrLn ""
    putStrLn $ prettyPrint state
    if ((abs . boardValue) board) >= 1000
        then winner board
        else if color == White
                then playerMove state
                else computerMove state

-- |Announces the winner and ends the game.
winner :: Board -> IO ()
winner board = do
    putStrLn (color ++ " wins!")
    return ()
        where color = if boardValue board > 0
                        then "White"
                        else "Black"

-- |Calculates the ai move (Black Player)
computerMove :: State -> IO ()
computerMove state = do
    let board = stateBoard state
        move@(from, to) = getNextMove state
        move'           = updateBoard from to
    game State 
         { stateBoard= move' board
         , lastMove =  move
         , turnOf = oponentColor (turnOf state)
         , captured = updateCaptured to board (captured state)
         , whiteCanCastle = True
         , blackCanCastle = True}
    return ()

-- |Asks for the players move
playerMove :: State -> IO ()
playerMove state = do
    let board = stateBoard state
        color = turnOf state
    move@(from, to) <- getMove
    if isValidMove from to board color
        then do let move' = updateBoard from to
                game State 
                     { stateBoard= move' board
                     , lastMove =  move
                     , turnOf = oponentColor (turnOf state)
                     , captured = updateCaptured to board (captured state)
                     , whiteCanCastle = True
                     , blackCanCastle = True}
                return ()
        else do putStrLn "The move you chose is not possible."
                playerMove state
                return ()

getMove :: IO Move
getMove = do
    putStr "From: "
    hFlush stdout
    from <- getLine
    putStr "To: "
    hFlush stdout
    to <- getLine
    let from' = readInputPosition from
        to'   = readInputPosition to
    if from' == Nothing || to' == Nothing
        then do putStrLn "Please enter a valid move."
                getMove
        else do let (Just f) = from'
                    (Just t) = to'
                return (f,t)

updateCaptured :: Position -> Board -> [Piece] -> [Piece]
updateCaptured pos board cs = update (getTile board pos) cs
    where update Nothing cs = cs
          update (Just t)  cs = t:cs
