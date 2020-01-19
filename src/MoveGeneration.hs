{-# LANGUAGE ViewPatterns #-}
module MoveGeneration
    where 

import Chess
import Data.Sequence (Seq((:<|)), ViewL(..), (><))
import qualified Data.Sequence as S
import Data.Vector ((!), (!?))
import qualified Data.Vector as V
import Data.Maybe (fromMaybe, catMaybes)
import Control.Monad (join)


data GamePlay = GamePlay
    { _game     :: Game
    , _lastMove :: Move
    , _kings    :: (Index, Index)
    , _pins     :: [(Index, Index)]
    , _check    :: Bool
    }

getAllMoves :: GamePlay -> Seq Move
getAllMoves gp =
    foldr (\x y -> fromMaybe S.Empty (pieceMoves g x) >< y)
                       S.Empty (S.fromList [2..93])

pieceMoves :: GamePlay -> Index -> Maybe (Seq Move) -- pseudo-legal generator
pieceMoves g ix = 
    let pieceCol = b!ix
     in case pieceCol of
          Just (Piece pieceT col) -> 
              if (_colour <$> pieceCol) /= Just turn
                then Nothing
                else Just $ fmap (uncurry $ Move ix) $ case pieceT of
                    Pawn -> handlePawn g ix 
                    Knight -> moveKnight g ix
                    Bishop -> castBishop g ix
                    Rook -> castRook g ix
                    Queen -> castQueen g ix
                    King -> moveKing g ix
          _ -> Nothing

(#!>) :: GamePlay -> Move -> Maybe GamePlay -- Legality check a move then do it
(#!>) g m = let g' = g #> m
                flipG = flipTurn g'
             in if checkLegal g m
                   then Nothing
                   else Just g'

checkLegal :: GamePlay -> Move -> Bool
checkLegal g m =
    let g' = g #> m
        flipG = flipTurn g
     in not $ inCheck flipG (getKingIx flipG)

inCheck :: Game -> Index -> Bool
inCheck g@Game {_board=(Board b), _turn=turn} kingIx = not $ null $
    filterMoves [Knight] (moveKnight g kingIx) ><
    filterMoves [King] (moveKing g kingIx) ><
    filterMoves [Queen, Bishop] (castBishop g kingIx) ><
    filterMoves [Queen, Rook] (castRook g kingIx) ><
    filterMoves [Pawn] (handlePawn g kingIx)
        where
            filterMoves pieces = S.filter (\(ix, mtyp) -> mtyp == Capture &&
                fmap _piece (join $ b!?ix) `elem` map Just pieces)

validateMoves :: Game -> Seq Index -> Seq (Index, MoveType)
validateMoves _ S.Empty = S.Empty
validateMoves g@Game {_board=(Board b), _turn=turn} (S.viewl -> x :< xs) =
    let square = b !? x
        move = case join square of
                 Nothing -> Nothing
                 Just Empty -> Just (x, Normal)
                 Just (Piece _ col) -> if col == turn
                                          then Nothing
                                          else Just (x, Capture)
     in case move of
          Nothing -> validateMoves g xs
          Just m -> m :<| validateMoves g xs 

castOffset :: Game -> Index -> (Index -> Index) -> Seq (Index, MoveType)
castOffset g@Game {_board=(Board b), _turn = turn} ix fn =
    let
        nextix = fn ix
        square = join $ b !? nextix
     in case square of
          Nothing -> S.Empty
          Just Empty -> (nextix, Normal) :<| castOffset g nextix fn
          Just a -> if fmap _colour square == Just turn 
                       then S.Empty
                       else S.singleton (nextix, Capture)

bishopMoves = [(+13), (+11),  subtract 13, subtract 11]

castBishop :: Game -> Index -> Seq (Index, MoveType)
castBishop g@Game {_board=b, _turn=turn} ix = foldr ((><) . castOffset g ix) S.Empty
                                      (S.fromList bishopMoves)

rookMoves = [(+1), (+12),  subtract 1, subtract 12]

castRook :: Game -> Index -> Seq (Index, MoveType)
castRook g@Game {_board=b, _turn=turn} ix = foldr ((><) . castOffset g ix) S.Empty
                                    (S.fromList rookMoves)

castQueen :: Game -> Index -> Seq (Index, MoveType)
castQueen g ix = castRook g ix >< castBishop g ix

knightMoves = S.fromList [-25, -23, -14, -10, 10, 14, 23, 25] 

moveKnight :: Game -> Index -> Seq (Index, MoveType)
moveKnight g ix = validateMoves g $ fmap (+ix) knightMoves

kingMoves = S.fromList [-13, -12, -11, -1, 1, 11, 12, 13]

moveKing :: Game -> Index -> Seq (Index, MoveType)
moveKing g ix = validateMoves g $ fmap (+ix) kingMoves

handlePawn :: Game -> Index -> Seq (Index, MoveType)
handlePawn Game {_board=(Board b), _turn=turn} ix = S.fromList $ catMaybes $
    if turn == White
    then [checkEnemy (ix-11), checkEmpty (ix-12), checkEnemy (ix-13)] ++
        [checkDouble (ix-24) (ix-12)| ix `div` 12 == 6]
    else [checkEnemy (ix+11), checkEmpty (ix+12), checkEnemy (ix+13)] ++ 
        [checkDouble (ix+24) (ix+12) | ix `div` 12 == 1]
        where
            checkEnemy ix' = if maybe True (notEnemy turn) (join (b !? ix'))
                             then Nothing else Just (ix', Capture)
            checkEmpty ix' = if join (b !? ix') == Just Empty
                                  then Just (ix', Normal) else Nothing
            checkDouble ix' prev = if join (b !? ix') == Just Empty && join (b !? prev) == Just Empty
                                  then Just (ix', PawnDouble) else Nothing

notEnemy :: Player -> Piece -> Bool
notEnemy col p = p == Empty || col == _colour p

isThreatened :: Game -> Index -> (Index -> Index) -> (PieceType -> Bool) -> Bool
isThreatened g@Game {_board=(Board b), _turn=turn} kingIx fn isPiece =
    let nextix = fn kingIx
        square = join $ b !? nextix
    in case square of
        Nothing -> False
        Just Empty -> isThreatened g nextix fn isPiece
        Just s -> _colour s /= turn && (isPiece . _piece) s

