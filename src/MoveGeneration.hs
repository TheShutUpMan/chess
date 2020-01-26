{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
module MoveGeneration (legalMoves, startGamePlay)
    where 

import Chess
import Data.Sequence (Seq((:<|)), ViewL(..), (><))
import qualified Data.Sequence as S
import Data.Vector ((!?))
import qualified Data.Vector as V
import Data.Maybe (fromMaybe, catMaybes)
import Data.Foldable (find)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Control.Monad (join)
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (%~))


data GamePlay = GamePlay
    { _game       :: Game
    , _lastMove   :: Maybe Move    -- Needed for passant
    , _kings      :: (Index, Index)-- Index of king
    , _attackedBy :: IntMap IntSet -- Set of all pieces attacking a square
    , _defendedBy :: IntMap IntSet -- Set of all allied pieces attacking a square
    , _pins       :: IntMap (Maybe Index) -- Can only be pinned by one piece
    }

$(makeLenses ''GamePlay)

getPin :: GamePlay -> Index -> Maybe Index
getPin gp ix = _pins gp IntMap.! ix

(#!>) :: GamePlay -> Move -> GamePlay -- assumes move is legal
gp #!> m =
    let g = _game gp
        p = g #! _from m
     in case _piece <$> p of
          Just Pawn -> undefined

getAllMoves :: GamePlay -> Seq Move
getAllMoves gp =
    let attacks = _attackedBy gp IntMap.! fst (_kings gp)
        in if attacks == IntSet.empty
              then foldr (\x y -> fromMaybe S.Empty (legalMoves gp x) >< y)
                         S.Empty (S.fromList [2..93])
              else checkMoveGen gp attacks

checkMoveGen :: GamePlay -> IntSet -> Seq Move
checkMoveGen gp attacks = -- assumes king is in check
    if IntSet.size attacks == 2 -- double check
       then toMove k <$> moveKing gp k
       else undefined
           where k = fst $ _kings gp
                       
toMove :: Index -> (Index, MoveType) -> Move
toMove from (to, mtyp) = Move from to mtyp

-- generate legal moves when out of check 
legalMoves :: GamePlay -> Index -> Maybe (Seq Move)
legalMoves gp ix = 
    let
        g = _game gp
        piece = g#!ix
        turn = _turn g
     in case piece of
          Just (Piece pType col) -> 
              if (_colour <$> piece) /= Just turn
                then Nothing
                else Just $ toMove ix <$> case pType of
                    Pawn -> legalPawn gp ix 
                    Knight -> legalKnight gp ix
                    Bishop -> legalBishop gp ix
                    Rook -> legalRook gp ix
                    Queen -> legalQueen gp ix
                    King -> legalKing gp ix
          _ -> Nothing

pseudoMoves :: Game -> Index -> Maybe (Seq Move)
pseudoMoves g ix =
    let
        piece = g#!ix
        turn = _turn g
     in case piece of
          Just (Piece pType col) ->
              if (_colour <$> piece) /= Just turn
                 then Nothing
                 else Just $ toMove ix <$> case pType of
                     Pawn -> pawnCaptures gp ix 
                     Knight -> pseudoKnight gp ix
                     Bishop -> pseudoBishop gp ix
                     Rook -> pseudoRook gp ix
                     Queen -> pseudoQueen gp ix
                     King -> pseudoKing gp ix
          _ -> Nothing



annotateMoves :: Game -> Seq Index -> Seq (Index, MoveType)
annotateMoves _ S.Empty = S.Empty
annotateMoves g (S.viewl -> x :< xs) =
    let
        square = g #! x
        move = case square of
                 Nothing -> Nothing
                 Just Empty -> Just (x, Normal)
                 Just (Piece _ col) -> if col == _turn g
                                          then Nothing
                                          else Just (x, Capture)
     in case move of
          Nothing -> annotateMoves g xs
          Just m -> m :<| annotateMoves g xs 

castPiece :: Game -> Index -> (Index -> Index) -> Seq (Index, MoveType)
castPiece g@Game {_board=(Board b), _turn = turn} ix fn =
    let
        nextix = fn ix
        square = join $ b !? nextix
     in case square of
          Nothing -> S.Empty
          Just Empty -> (nextix, Normal) :<| castPiece g nextix fn
          Just a -> if fmap _colour square == Just turn 
                       then S.Empty
                       else S.singleton (nextix, Capture)

bishopMoves = S.fromList [(+13), (+11),  subtract 13, subtract 11]

pseudoBishop :: Game -> Index -> Seq (Index, MoveType)
pseudoBishop g ix = foldr ((><) . castPiece g ix) S.Empty bishopMoves

legalBishop :: GamePlay -> Index -> Seq (Index, MoveType)
legalBishop gp ix =
    let g = _game gp
     in case getPin gp ix of
          Nothing -> pseudoBishop g ix
          Just p -> 
              let rows = (p `mod` 12 - ix `mod` 12) 
                  cols = (p `div` 12 - ix `div` 12)
               in if
                     | rows == cols ->
                        castPiece g ix (+13) >< castPiece g ix (subtract 13)
                     | rows == (- cols) ->
                        castPiece g ix (+11) >< castPiece g ix (subtract 11)
                     | otherwise -> S.Empty

rookMoves = S.fromList [(+1), (+12),  subtract 1, subtract 12]

pseudoRook :: Game -> Index -> Seq (Index, MoveType)
pseudoRook g ix = foldr ((><) . castPiece g ix) S.Empty rookMoves

legalRook :: GamePlay -> Index -> Seq (Index, MoveType)
legalRook gp ix =
    let g = _game gp
     in case getPin gp ix of
          Nothing -> pseudoRook g ix
          Just p -> if
               | p `mod` 12 == ix `mod` 12 ->
                   castPiece g ix (+12) >< castPiece g ix (subtract 12)
               | p `div` 12 == ix `div` 12 ->
                   castPiece g ix (+1) >< castPiece g ix (subtract 1)
               | otherwise -> S.Empty

pseudoQueen :: Game -> Index -> Seq (Index, MoveType)
pseudoQueen g ix = pseudoRook g ix >< pseudoBishop g ix

legalQueen :: GamePlay -> Index -> Seq (Index, MoveType)
legalQueen gp ix = case getPin gp ix of
                    Nothing -> pseudoQueen (_game gp) ix
                    Just p -> legalRook gp ix >< legalBishop gp ix

knightMoves = S.fromList [-25, -23, -14, -10, 10, 14, 23, 25] 

pseudoKnight :: Game -> Index -> Seq (Index, MoveType)
pseudoKnight g ix = annotateMoves g $ fmap (+ix) knightMoves

legalKnight :: GamePlay -> Index -> Seq (Index, MoveType)
legalKnight gp ix = case getPin gp ix of
                      Nothing -> pseudoKnight (_game gp) ix
                      _ -> S.Empty -- Cannot do anything when pinned

kingMoves = S.fromList [-13, -12, -11, -1, 1, 11, 12, 13]

pseudoKing :: Game -> Index -> Seq (Index, MoveType)
pseudoKing g ix = annotateMoves g $ fmap (+ix) kingMoves

legalKing :: GamePlay -> Index -> Seq (Index, MoveType)
legalKing gp ix = S.filter isNotAttacked moves
    where
        moves = pseudoKing (_game gp) ix
        isNotAttacked (ix, _) = 
            let attacks = _attackedBy gp IntMap.! ix
             in attacks == IntSet.empty

pawnCaptures :: Game -> Index -> Seq (Index, MoveType)
pawnCaptures Game {_turn = t} ix = S.fromList $
    fmap (, Capture) $
        case t of
          White -> [ix-11, ix-13]
          Black -> [ix+11, ix+13]

legalPawn :: GamePlay -> Index -> Seq (Index, MoveType)
legalPawn gp ix = S.fromList $ catMaybes $
    if _turn g == White
       then
         case getPin gp ix of
            Nothing -> [checkEnemy (ix-11),
                        checkEmpty (ix-12),
                        checkEnemy (ix-13)] ++
                       [checkDouble (ix-24) (ix-12)| ix `div` 12 == 6]
            Just p -> if
                 | p `mod` 12 == ix `mod` 12 -> -- Pinned by rook or queen
                     checkEmpty (ix - 12) :
                     [checkDouble (ix-24) (ix-12) | ix `div` 12 == 6]
                 | p == ix - 11 || p == ix - 13 -> -- Can capture
                     [checkEnemy p]
                 | otherwise -> [] 
       else
         case getPin gp ix of
           Nothing -> [checkEnemy (ix+11),
                       checkEmpty (ix+12),
                       checkEnemy (ix+13)] ++
                      [checkDouble (ix+24) (ix+12)| ix `div` 12 == 6]
           Just p -> if
                 | p `mod` 12 == ix `mod` 12 -> -- Pinned by rook or queen
                     checkEmpty (ix+12) :
                     [checkDouble (ix+24) (ix+12) | ix `div` 12 == 6]
                 | p == ix + 11 || p == ix + 13 -> -- Can capture
                     [checkEnemy p]
                 | otherwise -> [] 
        where
            g = _game gp
            checkEnemy ix' = if maybe True (notEnemy $ _turn g) (g#!ix')
                                then case _lastMove gp of
                                       Just (Move _ to PawnDouble) ->
                                           if ix' `mod` 12 == to `mod` 12
                                              then Just (ix', Passant)
                                              else Nothing 
                                       _ -> Nothing
                                else Just (ix', Capture)
            checkEmpty ix' = if g#!ix' == Just Empty
                                  then Just (ix', Normal) else Nothing
            checkDouble ix' prev = if g#!ix' == Just Empty && g#!prev == Just Empty
                                  then Just (ix', PawnDouble) else Nothing
            notEnemy turn piece = piece == Empty || _colour piece == turn


findKing :: Game -> Index
findKing Game{_board = b, _turn = t} = fromMaybe undefined $ 
    V.findIndex (\x -> x == Just (Piece King t)) (getBoard b)

genAttacks :: Game -> IntMap IntSet
genAttacks Game {_board = b, _turn = t} =
    let
        enemyAttacks = foldr (\x y -> fromMaybe S.Empty (pseudoMoves gp x) >< y)
                         S.Empty (S.fromList [2..93])
     in undefined

genPins :: Game -> IntMap IntSet -> IntMap (Maybe Index)
genPins Game {_board = b, _turn = t} attacks = undefined

toGamePlay :: Game -> GamePlay
toGamePlay g =
    let attacks = genAttacks g
     in GamePlay
            { _game = g
            , _lastMove = Nothing
            , _kings = (findKing g, findKing (flipTurn g))
            , _attackedBy = genAttacks g
            , _defendedBy = genAttacks (flipTurn g)
            , _pins = genPins g attacks
            }

startGamePlay = toGamePlay startGame
