import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Mutable as BM
import Control.DeepSeq
import System.CPUTime
import Text.Printf

-- ex1:
--do
--    mv <- M.new 0
--    U.unsafeFreeze mv

-- M.unsafeRead do bounds checking, which is not great for performance
-- then we got those differences

-- ghci> out <- M.unsafeNew 4 :: IO (M.IOVector Int)
-- ghci> M.write out 0 1
-- ghci> M.read out 0
-- 1
-- 
-- ghci> M.write out 10 1
-- *** Exception: index out of bounds (10,4)
-- CallStack (from HasCallStack):
--   error, called at src/Data/Vector/Internal/Check.hs:103:12 in vector-0.13.1.0-E4LTjcK991m18cbKZhUjhX:Data.Vector.Internal.Check
--   checkError, called at src/Data/Vector/Internal/Check.hs:109:17 in vector-0.13.1.0-E4LTjcK991m18cbKZhUjhX:Data.Vector.Internal.Check
--   check, called at src/Data/Vector/Internal/Check.hs:122:5 in vector-0.13.1.0-E4LTjcK991m18cbKZhUjhX:Data.Vector.Internal.Check
--   checkIndex, called at src/Data/Vector/Generic/Mutable.hs:671:15 in vector-0.13.1.0-E4LTjcK991m18cbKZhUjhX:Data.Vector.Generic.Mutable
--   write, called at src/Data/Vector/Unboxed/Mutable.hs:330:9 in vector-0.13.1.0-E4LTjcK991m18cbKZhUjhX:Data.Vector.Unboxed.Mutable
-- ghci> M.unsafeWrite out 10 1
-- 
-- AND
-- 
-- ghci> M.read out 10
-- *** Exception: index out of bounds (10,4)
-- CallStack (from HasCallStack):
--   error, called at src/Data/Vector/Internal/Check.hs:103:12 in vector-0.13.1.0-E4LTjcK991m18cbKZhUjhX:Data.Vector.Internal.Check
--   checkError, called at src/Data/Vector/Internal/Check.hs:109:17 in vector-0.13.1.0-E4LTjcK991m18cbKZhUjhX:Data.Vector.Internal.Check
--   check, called at src/Data/Vector/Internal/Check.hs:122:5 in vector-0.13.1.0-E4LTjcK991m18cbKZhUjhX:Data.Vector.Internal.Check
--   checkIndex, called at src/Data/Vector/Generic/Mutable.hs:647:12 in vector-0.13.1.0-E4LTjcK991m18cbKZhUjhX:Data.Vector.Generic.Mutable
--   read, called at src/Data/Vector/Unboxed/Mutable.hs:308:8 in vector-0.13.1.0-E4LTjcK991m18cbKZhUjhX:Data.Vector.Unboxed.Mutable
-- ghci> M.unsafeRead out 10
-- 1

-- ex2:

-- ghci> :t out
-- out :: M.IOVector Int
-- ghci> U.freeze out >>= \v -> print $ v U.! 0
-- 1
-- OR without bounds checking
-- ghci> U.freeze out >>= \v -> print $ U.unsafeIndex v 0
-- 1

-- ex3:

-- ghci> :t out
-- out :: M.IOVector Int
-- ghci> U.freeze out >>= \v -> pure $ U.foldl' (\acc x -> acc * x) 1 v
-- -6311956490426118400

-- Because the function after >>= must return an IO action, it's what `pure` does
-- Also,

-- U.foldl is lazy in the accumulator.
-- 
-- U.foldl' is strict in the accumulator.

-- That ' is pronounced “prime”.
-- 
-- Difference
-- 
-- Lazy version:
-- 
-- U.foldl (\acc x -> acc * x) 1 v
-- 
-- can build a chain like:
-- 
-- (((1 * x1) * x2) * x3) * x4
-- 
-- as unevaluated thunks before finally computing it.
-- 
-- Strict version:
-- 
-- U.foldl' (\acc x -> acc * x) 1 v
-- 
-- forces the accumulator at every step:
-- 
-- acc = 1
-- acc = acc * x1
-- acc = acc * x2
-- acc = acc * x3
-- ...
-- 
-- So for numeric folds, prefer:
-- 
-- U.foldl' (*) 1 v
-- 
-- or simply:
-- 
-- U.product v

totalLength :: U.Vector Int -> Maybe Int
totalLength lengths =
    U.foldM' step 1 lengths
  where
    step acc x
      | x == 0 = Just 0
      | acc > maxBound `div` x = Nothing -- prevents overflow
      | otherwise = Just (acc * x)

cartesianIndices :: U.Vector Int -> U.Vector Int
cartesianIndices lengths = runST $ do
    let !ndim = U.length lengths

    case totalLength lengths of
      Nothing -> error "cartesianIndices: total size overflow"
      Just 0  -> U.unsafeFreeze =<< M.new 0 -- creates the actin of creating a mutable vector of length 0 (M.new 0) and this called action (with =<<) is passed to U.unsafeFreeze that creates an immutable vector like ex1 WITHOUT copying it -> in-place function
      Just total -> do
          out <- M.unsafeNew (total * ndim) -- creates a mutable vector of size total * ndim, that is the output vector, and same thing the action is M.unsafeNew (total * ndim) and executed toward out with `out <-`
          cur <- M.replicate ndim 0 -- same thing, but thatis the index vector, same thing

          let loop row
                | row >= total = pure () -- because loop has type loop :: Int -> ST s () because every branch must return an ST s (), wht ? - because the otherwise branch is performing  M.unsafeWrite cur dim 0, which is "execute a write action. It mutates the vector and returns no useful value, so its result type is ST s ()."

                | otherwise = do
                    let !rowIdx = row * ndim -- '!' forces to directly evaluate rowIdx and not store the temporary 'row * ndim' computation in a more larger data structure than just Int until the 'rowIdx + i'

                    forM_ [0 .. ndim - 1] $ \i -> do
                        v <- M.unsafeRead cur i
                        M.unsafeWrite out (rowIdx + i) v

                    let carry dim
                          | dim < 0 = pure ()
                          | otherwise = do
                              v <- M.unsafeRead cur dim
                              let limit = U.unsafeIndex lengths dim -- 'U.unsafeIndex' just the random access  operator for immutable vector, see ex2

                              if v == limit - 1
                              then do
                                M.unsafeWrite cur dim 0
                                carry (dim - 1)
                              else do
                                M.unsafeWrite cur dim (v + 1)

                    carry (ndim - 1)
                    loop (row + 1)

          loop 0
          U.unsafeFreeze out

makeVector :: U.Vector Int -> BM.MVector (M.Vector Int) -> BM.MVector (MVector Int) -> BM.MVector (MVector Int)
makeVector rtn ids = 

benchmark :: U.Vector Int -> Int -> BM.Vector Int -> U.Vector Int
benchmark lengths iter inpt = go iter inpt 
  where
    go 0 inpt inpt = 0
    go n inpt inpt =
        let !r = cartesianIndices n lengths
            rtn = BM.new (U.product lengths) :: ST s (BM.MVector s (M.Vector s Int))
            !cur_rtn = makeVector r inpt rtn
        in go (n - 1) (cur_rtn:rtn)

main :: IO ()
main = do
    let inpt1 = U.fromList [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
        inpt2 = U.fromList [10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
        inpt3 = U.fromList [20, 21, 22, 23, 24, 25, 26, 27, 28, 29]
        inpt4 = U.fromList [30, 31, 32, 33, 34, 35, 36, 37, 38, 39]
        inpt5 = U.fromList [40, 41, 42, 43, 44, 45, 46, 47, 48, 49] 

        inpt = BM.new 5 :: ST s (BM.MVector s (M.Vector s Int))

        BM.write inpt  0 inpt1
        BM.write inpt  1 inpt2
        BM.write inpt  2 inpt3
        BM.write inpt  3 inpt4
        BM.write inpt  4 inpt5

        lengths = U.fromList [10, 10, 10, 10, 10]
        iter = 100000

    start <- getCPUTime

    let !result = benchmark lengths iter inpt

    result `deepseq` pure ()

    end <- getCPUTime

    let elapsedNs :: Double
        elapsedNs = fromIntegral (end - start) / 1000.0 --convert an Integral (end - start) to a Num which is more general and can be divided like a Double

        nsPerCall = elapsedNs / fromIntegral iter

    printf "result:      %d\n" result
    printf "iterations:  %d\n" iter
    printf "elapsed ns:  %.0f\n" elapsedNs
    printf "ns / call:   %.2f\n" nsPerCall



