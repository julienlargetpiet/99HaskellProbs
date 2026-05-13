{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector as B
import Control.DeepSeq
import System.CPUTime
import Text.Printf

data Matrix a = Matrix
  { nRows :: !Int
  , nCols :: !Int
  , values :: !(U.Vector a)
  }

unsafeIndex2D :: U.Unbox a => Matrix a -> Int -> Int -> a
unsafeIndex2D mat row col =
  U.unsafeIndex (values mat) idx
  where
    idx = row * nCols mat + col

totalLength :: U.Vector Int -> Maybe Int
totalLength lengths =
    U.foldM' step 1 lengths
  where
    step acc x
      | x == 0 = Just 0
      | acc > maxBound `div` x = Nothing 
      | otherwise = Just (acc * x)

cartesianIndices :: U.Vector Int -> U.Vector Int
cartesianIndices lengths = runST $ do
    let !ndim = U.length lengths

    case totalLength lengths of
      Nothing -> error "cartesianIndices: total size overflow"
      Just 0  -> U.unsafeFreeze =<< M.new 0 
      Just total -> do
          out <- M.unsafeNew (total * ndim) 
          cur <- M.replicate ndim 0 
          let loop row
                | row >= total = pure () 
                | otherwise = do
                    let !rowIdx = row * ndim 
                    forM_ [0 .. ndim - 1] $ \i -> do
                        v <- M.unsafeRead cur i
                        M.unsafeWrite out (rowIdx + i) v

                    let carry dim
                          | dim < 0 = pure ()
                          | otherwise = do
                              v <- M.unsafeRead cur dim
                              let limit = U.unsafeIndex lengths dim 

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

makeMatrix :: Int -> U.Vector Int -> B.Vector (U.Vector Int) -> Matrix Int
makeMatrix !salt indices dataVec = runST $ do
    let !ndim = B.length dataVec
        !rows = U.length indices `quot` ndim
        !total = rows * ndim

    out <- M.unsafeNew total

    let loop !row
          | row >= rows = pure ()
          | otherwise = do
              let !rowIdx = row * ndim

              let inner !j
                    | j >= ndim = pure ()
                    | otherwise = do
                        let !idx = U.unsafeIndex indices (rowIdx + j)
                            !src = B.unsafeIndex dataVec j
                            !val = U.unsafeIndex src idx + salt

                        M.unsafeWrite out (rowIdx + j) val
                        inner (j + 1)

              inner 0
              loop (row + 1)

    loop 0

    frozen <- U.unsafeFreeze out

    pure Matrix
      { nRows = rows
      , nCols = ndim
      , values = frozen
      }

benchmark :: U.Vector Int -> Int -> B.Vector (U.Vector Int) -> Int
benchmark lengths iter inpt = go iter
  where
    go 1 =
        let !indices = cartesianIndices lengths
            !r = makeMatrix 1 indices inpt
        in values r `deepseq` U.length (values r)

    go n =
        let !indices = cartesianIndices lengths
            !r = makeMatrix n indices inpt
        in values r `deepseq` go (n - 1)

main :: IO ()
main = do
    let inpt1 = U.fromList [0,  1,  2,  3,  4] --, 5, 6, 7, 8, 9]
        inpt2 = U.fromList [10, 11, 12, 13, 14] --15, 16, 17, 18, 19]
        inpt3 = U.fromList [20, 21, 22, 23, 24] --25, 26, 27, 28, 29]
        inpt4 = U.fromList [30, 31, 32, 33, 34] --35, 36, 37, 38, 39]
        inpt5 = U.fromList [40, 41, 42, 43, 44] --45, 46, 47, 48, 49] 

        inpt =
                B.fromList
                  [ inpt1
                  , inpt2
                  , inpt3
                  , inpt4
                  , inpt5
                  ]

        lengths = U.fromList $ replicate 5 5

        iter = 100000

    start <- getCPUTime

    let !result = benchmark lengths iter inpt

    result `deepseq` pure ()

    end <- getCPUTime

    let elapsedNs :: Double
        elapsedNs = fromIntegral (end - start) / 1000.0 

        nsPerCall = elapsedNs / fromIntegral iter

    printf "result:      %d\n" result
    printf "iterations:  %d\n" iter
    printf "elapsed ns:  %.0f\n" elapsedNs
    printf "ns / call:   %.2f\n" nsPerCall



