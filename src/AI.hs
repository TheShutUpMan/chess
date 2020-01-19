module AI (
    minimaxMove,
    alphaBetaMove,
    alphaBetaValue,
    randomMove,
    Minimax(..)
) where 
import Chess
import MoveGeneration
import qualified Data.Sequence as S
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)
import System.Random

evalGame :: Game -> Int
evalGame Game{_board=b, _turn=turn} = foldr ((+) . pieceValue turn) 0 $ getBoard b

pieceValue :: Player -> Maybe Piece -> Int
pieceValue turn piece =
    case piece of
      Nothing -> 0
      Just Empty -> 0
      Just (Piece typ pcol) ->
          (if turn == pcol then 1 else - 1) *
          case typ of
               King -> 0
               Queen -> 9
               Rook -> 5
               Knight -> 3
               Bishop -> 3
               Pawn -> 1

data Minimax = Min | Max deriving (Eq, Show)

getAIMove :: (Game -> Int) -> Game -> Move
getAIMove evalFun g =
    let moves = getAllMoves g
        successors = (g #>) <$> moves
     in snd $ maximum $ S.zip (fmap evalFun successors) moves 

minimaxMove :: Int -> Game -> Move
minimaxMove depth = getAIMove $ minimaxValue Max depth
     
minimaxValue :: Minimax -> Int -> Game -> Int
minimaxValue turn depth g =
    if simpleCheckmate g
    then case turn of
             Max -> minBound
             Min -> maxBound
    else case depth of
      0 -> (if turn == Max then -1 else 1) * evalGame g
      n -> let states = (g #>) <$> getAllMoves g
            in case turn of
                 Max -> maximum $ fmap (minimaxValue Min (n-1)) states
                 Min -> minimum $ fmap (minimaxValue Max (n-1)) states

alphaBetaMove :: Int -> Game -> Move
alphaBetaMove depth = getAIMove $ alphaBetaValue Max minBound maxBound depth

alphaBetaValue :: Minimax -> Int -> Int -> Int -> Game -> Int
alphaBetaValue turn alpha beta depth g =
    if simpleCheckmate g
    then case turn of
             Max -> minBound
             Min -> maxBound
    else case depth of
      0 -> case turn of
             Max -> evalGame (flipTurn g)
             Min -> evalGame g
      n -> let moves = getAllMoves g
            in case turn of
                 Max -> either snd snd $ foldM evalMax (alpha, minBound) moves
                 Min -> either snd snd $ foldM evalMin (beta, maxBound) moves
             where
                 evalMax (a, val) move = 
                     let g' = g #> move
                         val' = max val (alphaBetaValue Min a beta (depth - 1) g')
                         alpha' = max a val'
                      in if alpha' >= beta
                           then Left (alpha', val')
                           else Right (alpha', val')
                 evalMin (b, val) move =
                     let g' = g #> move
                         val' = min val (alphaBetaValue Max alpha b (depth - 1) g')
                         beta' = min val' b
                      in if alpha >= beta'
                           then Left (beta', val')
                           else Right (beta', val')

randomMove :: RandomGen g => g -> Game -> (Move, g)
randomMove gen g = (moves `S.index` ix, gen')
    where 
        moves = getAllMoves g
        (ix, gen') = randomR (0, length moves - 1) gen

randomGame :: RandomGen g => g -> Game -> (Bool, g)
randomGame gen g = undefined

mctsMove :: RandomGen g => g -> Int -> Game -> Move
mctsMove gen iterations g = undefined  
