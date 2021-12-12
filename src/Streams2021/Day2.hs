module Streams2021.Day2 where

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as F
import System.IO (openFile, IOMode (ReadMode))
import qualified Streamly.Internal.Unicode.Stream as U
import Prelude hiding (head)
import Text.Megaparsec
import Data.Void
import Data.Foldable (asum)
import Text.Megaparsec.Char (digitChar, space, string)
import Control.Monad (void)

type Parser = Parsec Void String

data Coordinate = Coordinate { x :: Int, y :: Int }

instance Show Coordinate where
  show Coordinate{..} = "x = " <> show x <> ", y = " <> show y

data Aim = Aim { coord :: Coordinate, aim :: Int } deriving Show

data Instruction = Forward Int | Up Int | Down Int

solve :: IO ()
solve = do
  input <- openFile "src/Streams2021/Inputs/day2.txt" ReadMode

  (coordinates, aim) <- S.fold (F.catMaybes $ F.tee instructionToCoords instructionToAim)
    $ S.map parseInstructions
    . U.lines F.toList
    . U.decodeUtf8Arrays
    . S.unfold FH.readChunks
    $ input

  putStrLn $ "Coordinates: " <> show coordinates
  putStrLn $ "Solution1: " <> show (x coordinates * y coordinates)

  putStrLn $ "Aim: " <> show aim
  putStrLn $ "Solution2: " <> show (x (coord aim) * y (coord aim))

instructionToCoords :: Monad m => F.Fold m Instruction Coordinate
instructionToCoords =
  F.foldl' f (Coordinate 0 0)
      where
        f Coordinate{..} (Forward i) = Coordinate { x = x + i, y }
        f Coordinate{..} (Down i)    = Coordinate { x, y = y + i }
        f Coordinate{..} (Up i)      = Coordinate { x, y = max 0 $ y - i }

instructionToAim :: Monad m => F.Fold m Instruction Aim
instructionToAim =
  F.foldl' f (Aim (Coordinate 0 0) 0)
      where
        f Aim{ coord = Coordinate{..}, aim } (Forward i) =
          Aim { coord = Coordinate { x = x + i, y = max 0 $ y + aim * i}, aim }
        f Aim{..} (Down i) = Aim { coord, aim = aim + i }
        f Aim{..} (Up i) = Aim { coord, aim = aim - i }

parseInstructions :: String -> Maybe Instruction
parseInstructions = parseMaybe instructionParser

instructionParser :: Parser Instruction
instructionParser = do
  direction <- asum [ forward, up, down ]
  void space
  vector <- Text.Megaparsec.many digitChar
  pure . direction . read $ vector
  where
    forward :: Parser (Int -> Instruction)
    forward = string "forward" >> pure Forward

    up :: Parser (Int -> Instruction)
    up = string "up" >> pure Up

    down :: Parser (Int -> Instruction)
    down = string "down" >> pure Down
