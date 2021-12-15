module Streams2021.Day3 where

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as F
import System.IO (openFile, IOMode (ReadMode))
import qualified Streamly.Internal.Unicode.Stream as U
import Prelude hiding (head)
import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char (char)
import Data.Function
import Data.Foldable (foldl')
import Data.Bifunctor

type Parser = Parsec Void String

data Bit = I | O deriving (Eq, Show)

data PositionCount = PositionCount { i :: Int, o :: Int } deriving Show

data Tree a = Branch { ibranch :: (Int, Tree a), obranch :: (Int, Tree a)} | Leaf
  deriving Show

solve :: IO ()
solve = do
  input <- openFile "src/Streams2021/Inputs/day3.txt" ReadMode

  (bitCount, bitTree) <- S.fold (F.tee countBits foldBitTree & F.catMaybes)
      $ S.map parseBits
      . U.lines F.toList
      . U.decodeUtf8Arrays
      . S.unfold FH.readChunks
      $ input

  calculateSolution1 bitCount
  calculateSolution2 bitTree
    where
      calculateSolution1 :: [PositionCount] -> IO ()
      calculateSolution1 bitCount =
        let g = toDecimal $ gamma <$> bitCount
            e = toDecimal $ epsilon <$> bitCount
            powerConsumption = g * e
         in putStrLn $ "Solution1: " <> show powerConsumption

      calculateSolution2 :: Tree Bit -> IO ()
      calculateSolution2 bitTree = do
        let leastCommonBit = toDecimal $ leastCommonBitFromTree bitTree
            greatestCommonBit = toDecimal $ mostCommonBitFromTree bitTree
         in putStrLn $ "Solution2: " <> show (leastCommonBit * greatestCommonBit)

gamma :: PositionCount -> Bit
gamma PositionCount{..}
  | i > o = I
  | otherwise = O

epsilon :: PositionCount -> Bit
epsilon PositionCount{..}
  | i > o = O
  | otherwise = I

countBits :: Monad m => F.Fold m [Bit] [PositionCount]
countBits = F.foldl' (zipWith addBitCount) (repeat $ PositionCount 0 0)

addBitCount :: PositionCount -> Bit -> PositionCount
addBitCount pc I = pc { i = i pc + 1 }
addBitCount pc O = pc { o = o pc + 1 }

parseBits :: String -> Maybe [Bit]
parseBits = parseMaybe $ many bit
  where
    bit :: Parser Bit
    bit = (char '1' >> pure I) <|> (char '0' >> pure O)

insertBits :: Tree Bit -> [Bit] -> Tree Bit
insertBits tree [] = tree
insertBits Leaf (x:xs) =
  if x == I
     then Branch { ibranch = (1, insertBits Leaf xs), obranch = (0, Leaf) }
     else Branch { ibranch = (0, Leaf), obranch = (1, insertBits Leaf xs) }
insertBits (Branch i o) (x:xs) =
  if x == I
     then Branch { ibranch = bimap (+1) (`insertBits` xs) i, obranch = o }
     else Branch { ibranch = i, obranch = bimap (+1) (`insertBits` xs) o }

foldBitTree :: Monad m => F.Fold m [Bit] (Tree Bit)
foldBitTree = F.foldl' insertBits Leaf

mostCommonBitFromTree :: Tree Bit -> [Bit]
mostCommonBitFromTree Leaf = []
mostCommonBitFromTree (Branch i o) =
  if fst i >= fst o
     then I : mostCommonBitFromTree (snd i)
     else O : mostCommonBitFromTree (snd o)

leastCommonBitFromTree :: Tree Bit -> [Bit]
leastCommonBitFromTree Leaf = []
leastCommonBitFromTree (Branch i o) =
  if (fst i < fst o && fst i /= 0) || fst o == 0
     then I : leastCommonBitFromTree (snd i)
     else O : leastCommonBitFromTree (snd o)

toDecimal :: [Bit] -> Int
toDecimal = foldl' (\y x -> bitVal x + 2 * y) 0

bitVal :: Bit -> Int
bitVal = \case I -> 1; O -> 0
