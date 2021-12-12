module Streams2021.Day1 where

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as F
import System.IO (openFile, IOMode (ReadMode), Handle)
import Control.Monad.IO.Class (MonadIO)
import qualified Streamly.Internal.Unicode.Stream as U
import Prelude hiding (head)
import Data.Monoid
import Control.Applicative

solve1 :: IO ()
solve1 = do
  input <- openFile "src/Streams2021/Inputs/day1.txt" ReadMode
  print =<< foldInput orderPrev input

solve2 :: IO ()
solve2 = do
  input <- openFile "src/Streams2021/Inputs/day1.txt" ReadMode
  print =<< foldInput orderWindows input

foldInput ::
  MonadIO m =>
  (Int -> S.SerialT m Int -> S.SerialT m (Maybe Ordering)) ->
  Handle ->
  m (Sum Int)
foldInput mkOrderings h = do
  mHeadAndTail <- S.uncons . S.map (read @Int) . U.lines F.toList . U.decodeUtf8Arrays . S.unfold FH.readChunks $ h
  case mHeadAndTail of
    Nothing -> pure mempty
    Just (head, stream) ->
            S.fold (F.catMaybes countIncreases)
          . mkOrderings head
          $ stream

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
