{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
module MoveGeneration
    ( getAllMoves
    , startGP
    , checkmate
    , (#!>)
    , GamePlay(..)
    ) where

import Chess
import Data.Sequence (Seq((:<|)), ViewL(..), (><))
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Bifunctor (second)
import Data.Maybe (fromMaybe, catMaybes)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Control.Monad (join)
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (%~))

type Attacks = IntMap IntSet
type Pins    = IntMap Index

data GamePlay = GamePlay
    { _game       :: Game
    , _lastMove   :: Maybe Move    -- Needed for passant
    , _kings      :: (Index, Index)-- Index of king
    , _attackedBy :: Attacks       -- Set of all enemy pieces attacking a square,
                                   -- Not storing empty sets
    , _defendedBy :: Attacks       -- Set of all allied pieces attacking a square
    , _pins       :: Pins          -- Can only be pinned by one piece
    , _enemyPins  :: Pins          -- Pins on enemy king
    }

$(makeLenses ''GamePlay)

-- Do legal move - legality is not verified
(#!>) :: GamePlay -> Move -> GamePlay
gp@(GamePlay g _ kings attacks defenses pins enemyPins) #!> m =
     GamePlay newG (Just m) (enemyKing, ourKing) attacks' defenses' pins' enemyPins'
    where 
        newG = g #> m
        (enemyKing, ourKing) = kings
        Just p = g #! _from m
        attacks'  = genAttacks newG
        defenses' = genAttacks (flipTurn newG)
        pins' = genPins newG enemyKing attacks'
        enemyPins' = genPins (flipTurn newG) ourKing defenses'


getPin :: GamePlay -> Index -> Maybe Index
getPin gp ix = _pins gp IntMap.!? ix

getAttacks :: GamePlay -> Index -> IntSet
getAttacks gp ix = fromMaybe IntSet.empty $ _attackedBy gp IntMap.!? ix

getAllMoves :: GamePlay -> Seq Move
getAllMoves gp =
    let attacks = getAttacks gp $ fst (_kings gp)
        in if attacks == IntSet.empty
              then foldr (\x y -> fromMaybe S.Empty (legalMoves gp x) >< y)
                         S.Empty (S.fromList [2..93])
              else --checkMoveGen gp attacks
                foldr (\x y -> fromMaybe S.Empty (legalMoves gp x) >< y)
                         S.Empty (S.fromList [2..93])

checkmate :: GamePlay -> Bool
checkmate = (== S.empty) . getAllMoves

-- Generate moves when king is in check
-- This is not verified
checkMoveGen :: GamePlay -> IntSet -> Seq Move
checkMoveGen gp attacks = 
    if IntSet.size attacks == 2 -- double check
       then toMove k <$> moveKing gp k
       else undefined
           where k = fst $ _kings gp
                 moveKing gp k = undefined

toMove :: Index -> (Index, MoveType) -> Move
toMove from (to, mtyp) = Move from to mtyp

-- generate legal moves when out of check
legalMoves :: GamePlay -> Index -> Maybe (Seq Move)
legalMoves gp@GamePlay{_game=g} ix =
    let
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

-- Generate all pseudo-legal moves, which may put the king in check
-- but are otherwise valid
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
                     Pawn -> pawnCaptures g ix
                     Knight -> pseudoKnight g ix
                     Bishop -> pseudoBishop g ix
                     Rook -> pseudoRook g ix
                     Queen -> pseudoQueen g ix
                     King -> pseudoKing g ix
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
        square = join $ b V.!? nextix
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
            let attacks = getAttacks gp ix
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

-- Generate an attack table from scratch
-- Not efficient, incrementally update attack table instead if possible
genAttacks :: Game -> Attacks
genAttacks g =
    let enemyAttacks = foldr (\x y -> fromMaybe S.Empty (pseudoMoves g x) >< y)
                             S.Empty (S.fromList [2..93])
     in foldr (\(Move from to _) y ->
                IntMap.insertWith IntSet.union to (IntSet.singleton from) y)
              IntMap.empty enemyAttacks

-- Incremental update of attack table after a single move
updateAttacks :: Move -> Piece -> GamePlay -> Attacks
updateAttacks
    m@(Move from to mtyp)
    Piece {_piece = p}
    gp@GamePlay {_game = g} =
    let fromAttacks = pseudoMoves g from
        toAttacks = pseudoMoves (g #> m) to
     in undefined

-- Generate pins on the king from scratch
genPins :: Game -> Index -> Attacks -> Pins
genPins g kingIx attacks =
    let
        b = getBoard (_board g)
        -- Get (allied) pieces that would be captured by a given piece
        getCaptures f = S.filter (\(_,m) -> m == Capture)
                                 (f (flipTurn g) kingIx)
        -- Pieces that can be pinned by rook cast
        rookCasts = fst <$> getCaptures pseudoRook
        -- Pieces that are pinning the king by rook cast
        rookQueen = (\(a,s) ->
                        (a, IntSet.filter
                            (\ix -> sameRowCol a ix && case b V.! ix of
                                     Just (Piece Queen _) -> True
                                     Just (Piece Rook _) -> True
                                     _ -> False) s))
                        . (\x -> (x, fetchAttacks attacks x)) <$> rookCasts
        -- Pieces that can be pinned by bishop cast
        bishopCasts = fst <$> getCaptures pseudoBishop
        -- Pieces that are pinning the king by bishop cast
        bishopQueen = (\(a, s) ->
                          (a, IntSet.filter
                                (\ix -> sameDiagonal a ix && case b V.! ix of
                                      Just (Piece Queen _) -> True
                                      Just (Piece Bishop _) -> True
                                      _ -> False) s))
                              . (\x -> (x, fetchAttacks attacks x)) <$> bishopCasts
     in IntMap.fromList $
         -- Convert (Index, IntSet) to [(Index, Index)]
         concatMap (\(a,s) -> (a,) <$> IntSet.toList s) bishopQueen ++
         concatMap (\(a,s) -> (a,) <$> IntSet.toList s) rookQueen
    where
        fetchAttacks attacks ix = IntMap.findWithDefault IntSet.empty ix attacks
        sameRowCol a b = a `mod` 12 == b `mod` 12 || a `div` 12 == b `div` 12
        sameDiagonal a b =
              let rows = (a `mod` 12 - b `mod` 12)
                  cols = (a `div` 12 - b `div` 12)
               in abs rows == abs cols

updatePins :: Move -> GamePlay -> Pins
updatePins m@(Move from to _) GamePlay {_game = g, _attackedBy=a} =
    case g #! from of
      Just (Piece King _) -> genPins (g #> m) from a
      Just (Piece a _) -> undefined
      _ -> undefined


toGamePlay :: Game -> GamePlay
toGamePlay g =
    let
     attacks = genAttacks g
     flipG = flipTurn g
     in GamePlay
            { _game = g
            , _lastMove = Nothing
            , _kings = (findKing g, findKing flipG)
            , _attackedBy = genAttacks g
            , _defendedBy = genAttacks flipG
            , _pins = genPins g (findKing g) attacks
            , _enemyPins = genPins flipG (findKing flipG) attacks
            }

startGP = toGamePlay startGame
