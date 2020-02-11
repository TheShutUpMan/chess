{-# LANGUAGE ViewPatterns #-}
module CommandLine where
import AI
import Chess
import MoveGeneration
import Control.Monad.IO.Class (liftIO)
import Data.Char (ord, chr)
import qualified Data.Sequence as S
import Data.Void
import qualified Data.Text as T
import Data.Text (Text, pack)
import Text.Megaparsec hiding (State)
import System.Console.Haskeline
import System.Random

commandLine :: IO GamePlay
commandLine = runInputT defaultSettings (iterateM_ runCommand startGP)
    where iterateM_ f = let g x = f x >>= g in g

runCommand :: GamePlay -> InputT IO GamePlay
runCommand gp@GamePlay{_game = g} = do
    liftIO $ print (_board g)
    inp <- getInputLine "% "
    -- gen <- liftIO getStdGen
    case inp of
      Nothing -> return gp 
      Just "p" -> outputStrLn
        (show $ getAllMoves gp)  >> return gp 
      Just "m" -> 
          let move = alphaBetaMove 4 gp
           in return $ gp #!> move
      Just cmd -> do
          let move = case parse parseAN "" cmd of
                        Left _ -> Nothing
                        Right x -> Just x
          case move >>= toMove gp of
               Nothing -> outputStrLn "Invalid/Illegal move" >> return gp
               Just m ->
                   let g' = gp #!> m 
                    in return g'

type Parser = Parsec Void String

parseAN :: Parser ((Char, Char), (Char, Char))
parseAN = do
    c1 <- satisfy (\x -> let o = ord x in 97 <= o && o <= 104)
    r1 <- satisfy (\x -> let o = ord x in 49 <= o && o <= 56)
    c2 <- satisfy (\x -> let o = ord x in 97 <= o && o <= 104)
    r2 <- satisfy (\x -> let o = ord x in 49 <= o && o <= 56)
    return ((c1, r1), (c2, r2))

toMove :: GamePlay -> ((Char, Char), (Char, Char)) -> Maybe Move
toMove gp ((fromc, fromr), (toc, tor)) =
    let fcol = ord fromc - 97
        frow = (abs (ord fromr - 56) * 12) + 2
        tcol = ord toc - 97
        trow = (abs (ord tor - 56) * 12) + 2
     in matchMove gp (fcol+frow) (tcol+trow)

toMoveStr :: Move -> String
toMoveStr (Move from to _) =
    let frow = chr $ (from `mod` 12) + 95
        fcol = chr $ ((from - 2) `div` 12) + 53
        trow = chr $ (to `mod` 12) + 95
        tcol = chr $ ((to - 2) `div` 12) + 53
    in [fcol, frow, tcol, trow]

matchMove :: GamePlay -> Index -> Index -> Maybe Move
matchMove g f t =
    let moves = getAllMoves g
    in case S.filter (\(Move from to _) -> f == from && t == to) moves of
        S.Empty -> Nothing
        (S.viewl -> m S.:< _) -> Just m 
