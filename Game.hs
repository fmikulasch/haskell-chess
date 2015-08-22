import Board
import Moves
import Eval
import Minimax
import System.IO (hFlush, stdout)


-- |Starts the game with an empty State and White playing
main = do
    game (initBoard, id) White
    return ()

type Game = [State]

-- |Main game loop
game :: State -> PColor -> IO()
game s@(board, lastMove) color = do
    putStrLn ""
    putStrLn $ prettyPrintBoard board
    if ((abs . boardValue) board) >= 1000
        then do winner board
                return ()
        else if color == White
                then do playerMove s color
                        return()
                else do computerMove s color
                        return()

-- |Announces the winner and ends the game.
winner :: Board -> IO ()
winner board = do
    putStrLn (color ++ " wins!")
    return ()
        where color = if boardValue board > 0
                        then "White"
                        else "Black"

-- |Calculates the ai move (Black Player)
computerMove :: State -> PColor -> IO ()
computerMove s@(board, _) color = do
    let move = getNextMove color s
    game (move board, move) (oponentColor color)
    return ()

-- |Asks for the player move
playerMove :: State -> PColor -> IO ()
playerMove s@(board, _) color= do
    (from, to) <- getMove
    let from' = readInputPosition from
        to'   = readInputPosition to
    if from' == Nothing || to' == Nothing
        then do putStrLn "Please enter a valid move."
                game s color
                return ()
        else do
            let (Just f) = from'
                (Just t) = to'
            if isValidMove f t board color
                then do let move = updateBoard f t
                        game (move board, move) (oponentColor color)
                        return ()
                else do putStrLn "The move you chose is not possible."
                        game s color
                        return ()

getMove :: IO (String, String)
getMove = do
    putStr "From: "
    hFlush stdout
    from <- getLine
    putStr "To: "
    hFlush stdout
    to <- getLine
    return (from,to)
