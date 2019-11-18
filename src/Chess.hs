{-# LANGUAGE TemplateHaskell #-}
module Chess where 
import Lens.Micro.TH (makeLenses) 
import Lens.Micro
import Linear.V2 (V2(..), _x, _y)
import Data.Maybe (fromMaybe)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show)
data Coord = Coord Int Int deriving (Show)
data Player = White | Black deriving (Show)

data Piece = Piece
    { _piece :: PieceType
    , _coord :: Coord
    , _owner :: Player 
    } deriving (Show)

data Game = Game
    { _pieces :: [Piece]
    , _turn :: Player
    } deriving (Show)

$(makeLenses ''Piece)
$(makeLenses ''Game)

g = Game [Piece Pawn (Coord 2 2) White] White

inBoard :: Coord -> Bool
inBoard c@(Coord x y) = x <= 8 && x >= 1 && y <= 8 && y >= 1

threatens :: Game -> Piece -> [Coord]
threatens g (Piece Pawn (Coord x y) White) = filter inBoard [Coord (x+1) (y+1), Coord (x-1) (y+1)]
threatens g (Piece Pawn (Coord x y) Black) = filter inBoard [Coord (x+1) (y-1), Coord (x-1) (y-1)]
