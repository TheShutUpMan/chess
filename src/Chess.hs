{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Chess where 
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (%~))
import Control.Monad (join)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Vector ((!), (!?), (//), Vector)
import qualified Data.Vector as V
import Data.Sequence (Seq((:<|)), (><), ViewL(..))
import qualified Data.Sequence as S
import qualified Data.Text as T

{- Chess datatype definitions -}

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)
data Player = White | Black deriving (Show, Eq)
data MoveType = Castle | Capture | Passant | Normal | PawnDouble deriving (Show, Eq, Ord)
type Index = Int 

data Move = Move
    { _from     :: Index
    , _to       :: Index
    , _movetype :: MoveType
    } deriving (Eq, Show, Ord)
    
data Piece = Piece
    { _piece :: PieceType
    , _colour :: Player}
    | Empty
    deriving (Eq)

newtype Board = Board {getBoard :: Vector (Maybe Piece)}

data Game = Game
    { _board :: Board
    , _turn :: Player
    }

$(makeLenses ''Move)
$(makeLenses ''Piece)
$(makeLenses ''Game)

{- Utilities for operating on the board -}

(#>) :: Game -> Move -> Game -- Do move, assumed (pseudo)legal
g@Game {_turn = t} #> (Move from to movetype) = flipTurn newGame
    where newGame = g & board %~ case movetype of
                Castle -> castle from to
                Passant -> passant t from to
                _ -> movePiece from to

(#!) :: Game -> Index -> Maybe Piece -- Retrieve a piece
g #! ix = join $ (!?ix) $ getBoard $ _board g


movePiece :: Index -> Index -> Board -> Board
movePiece from to (Board b) = Board $ b // [(from, Just Empty), (to, b ! from)]

passant :: Player -> Index -> Index -> Board -> Board
passant t from to (Board b) =
    let capSquare = case t of
                      White -> to + 12
                      _     -> to - 12
     in Board $ b // [(from, Just Empty),
                      (to, Just $ Piece Pawn t),
                      (capSquare, Just Empty)]

castle :: Index -> Index -> Board -> Board
castle from to (Board b) =
    let (rookFrom, rookTo) = if from < to
                                then (to + 1, to - 1) -- Kingside
                                else (to - 2, to + 1) -- Queenside
     in Board $ b // [(from, Just Empty),
                      (to, b ! from),
                      (rookFrom, Just Empty),
                      (rookTo, b ! rookFrom)]

flipTurn :: Game -> Game
flipTurn g = g & turn %~ nextTurn
    where nextTurn t = if t == White then Black else White 

{- Data declarations -}

instance Show Piece where 
    show Empty = "." 
    show (Piece Pawn Black)   = "♙"
    show (Piece Knight Black) = "♘"
    show (Piece Bishop Black) = "♗"
    show (Piece Rook Black)  = "♖"
    show (Piece Queen Black)  = "♕"
    show (Piece King Black)   = "♔"

    show (Piece Pawn White)   = "♟"
    show (Piece Knight White) = "♞"
    show (Piece Bishop White) = "♝"
    show (Piece Rook White)  = "♜"
    show (Piece Queen White)  = "♛"
    show (Piece King White)   = "♚"

instance Show Board where
    show = T.unpack . formatBoard . T.pack . concatMap show . catMaybes . V.toList . getBoard
        where
            addCols = zipWith T.cons "87654321" . T.chunksOf 8
            addRows = flip T.append "\n  a b c d e f g h"
            formatBoard = addRows . T.intercalate "\n" . fmap (T.intersperse ' ') . addCols

startBoard :: Board
startBoard = Board $ V.fromList [
    Nothing, Nothing,
    Just $ Piece Rook Black, Just $ Piece Knight Black, Just $ Piece Bishop Black, Just $ Piece Queen Black,
    Just $ Piece King  Black, Just $ Piece Bishop Black, Just $ Piece Knight Black, Just $ Piece Rook Black,
    Nothing, Nothing, Nothing, Nothing,
    Just $ Piece Pawn Black, Just $ Piece Pawn Black, Just $ Piece Pawn Black, Just $ Piece Pawn Black,
    Just $ Piece Pawn Black, Just $ Piece Pawn Black, Just $ Piece Pawn Black, Just $ Piece Pawn Black,
    Nothing, Nothing, Nothing, Nothing,
    Just Empty, Just Empty, Just Empty, Just Empty, Just Empty, Just Empty, Just Empty, Just Empty,
    Nothing, Nothing, Nothing, Nothing,
    Just Empty, Just Empty, Just Empty, Just Empty, Just Empty, Just Empty, Just Empty, Just Empty,
    Nothing, Nothing, Nothing, Nothing,
    Just Empty, Just Empty, Just Empty, Just Empty, Just Empty, Just Empty, Just Empty, Just Empty,
    Nothing, Nothing, Nothing, Nothing,
    Just Empty, Just Empty, Just Empty, Just Empty, Just Empty, Just Empty, Just Empty, Just Empty, 
    Nothing, Nothing, Nothing, Nothing,
    Just $ Piece Pawn White, Just $ Piece Pawn White, Just $ Piece Pawn White, Just $ Piece Pawn White,
    Just $ Piece Pawn White, Just $ Piece Pawn White, Just $ Piece Pawn White, Just $ Piece Pawn White, 
    Nothing, Nothing, Nothing, Nothing,
    Just $ Piece Rook White, Just $ Piece Knight White, Just $ Piece Bishop White, Just $ Piece Queen White,
    Just $ Piece King White, Just $ Piece Bishop White, Just $ Piece Knight White, Just $ Piece Rook White,
    Nothing, Nothing, 
    Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing,
    Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing
    ]

startGame :: Game
startGame = Game startBoard White
