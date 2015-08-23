import Game
import Board

-- |Starts the game with an empty State and White playing
main = game State { stateBoard = initBoard 
                  , turnOf = White
                  , lastMove = ((0,0),(0,0))
                  , captured = []
                  , whiteCanCastle = True
                  , blackCanCastle = True}

