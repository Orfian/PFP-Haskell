import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array
import Data.STRef

quickSort :: Array Int Int -> Array Int Int
quickSort arr = runArrayST $ do
    stArr <- thaw arr -- Convert immutable array to mutable STArray
    quickSortST stArr 0 (n - 1)
    return stArr
  where
    n = snd (bounds arr) + 1  -- Array length

-- Core quicksort algorithm for mutable STArray
quickSortST :: STArray s Int Int -> Int -> Int -> ST s ()
quickSortST arr low high
  | low < high = do
      pivotIndex <- partition arr low high
      quickSortST arr low (pivotIndex - 1)
      quickSortST arr (pivotIndex + 1) high
  | otherwise = return ()

-- Partition function for quicksort
partition :: STArray s Int Int -> Int -> Int -> ST s Int
partition arr low high = do
    pivot <- readArray arr high
    i <- newSTRef (low - 1)
    forM_ [low .. high - 1] $ \j -> do
      val <- readArray arr j
      when (val <= pivot) $ do
        modifySTRef i (+1)
        i' <- readSTRef i
        swap arr i' j
    i' <- readSTRef i
    swap arr (i' + 1) high
    return (i' + 1)

-- Helper function to swap elements in the array
swap :: STArray s Int Int -> Int -> Int -> ST s ()
swap arr i j = do
    temp <- readArray arr i
    val <- readArray arr j
    writeArray arr i val
    writeArray arr j temp

-- Helper function to run ST and return an immutable Array
runArrayST :: (forall s. ST s (STArray s Int Int)) -> Array Int Int
runArrayST st = runST (st >>= freeze)
