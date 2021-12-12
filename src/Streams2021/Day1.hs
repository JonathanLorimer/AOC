module Streams2021.Day1 where

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as F
import System.IO (openFile, IOMode (ReadMode))
import qualified Streamly.Internal.Unicode.Stream as U
import Prelude hiding (head)
import Data.Monoid
import Control.Applicative
import Data.Bifunctor

solve :: IO ()
solve = do
  input <- openFile "src/Streams2021/Inputs/day1.txt" ReadMode

  mHeadAndStream <- S.uncons . S.map (read @Int) . U.lines F.toList . U.decodeUtf8Arrays . S.unfold FH.readChunks $ input

  case mHeadAndStream of
    Nothing -> print "No input from src/Streams/2021/Inputs/day1.txt"
    Just (head, stream) -> do

      let fold = F.catMaybes countIncreases

      (sol1, sol2) <- fmap (bimap getSum getSum)
                    $ S.fold (F.unzip fold fold)
                    $ S.zipWith (,) (orderPrev head stream) (orderWindows head stream)

      putStrLn $ "Solution 1: " <> show sol1
      putStrLn $ "Solution 2: " <> show sol2


countIncreases :: Monad m => F.Fold m Ordering (Sum Int)
countIncreases = F.foldMap \case GT -> Sum 1; _ -> mempty

orderPrev :: Monad m => Int -> S.SerialT m Int -> S.SerialT m (Maybe Ordering)
orderPrev head = S.map snd . S.scanl' (\(prev, _) curr -> (curr, Just $ curr `compare` prev)) (head, Nothing)


data Window =
  Window
    { initialValue :: Sum Int
    , incompleteWindow :: Maybe (Sum Int)
    , completeWindow :: Maybe (Sum Int)
    , ordering :: Maybe Ordering
    }

orderWindows :: Monad m => Int -> S.SerialT m Int -> S.SerialT m (Maybe Ordering)
orderWindows head = S.map ordering . S.scanl' scanFunction start
  where
    start = Window { initialValue = Sum head, incompleteWindow = Nothing, completeWindow = Nothing, ordering = Nothing}
    scanFunction :: Window -> Int -> Window
    scanFunction Window {..} n =
      let initial = Sum n
          incw = initial <> initialValue
          compw = mappend incw <$> incompleteWindow
          ord = liftA2 compare compw completeWindow
      in
        Window
          { initialValue = initial
          , incompleteWindow = Just incw
          , completeWindow = compw
          , ordering = ord
          }
