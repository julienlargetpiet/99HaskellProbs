import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Mutable as BM
import qualified Data.Vector as B
import Control.DeepSeq
import System.CPUTime
import Text.Printf

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

makeVectorRows :: Int -> U.Vector Int -> B.Vector (U.Vector Int) -> B.Vector (U.Vector Int)
makeVectorRows !salt indices dataVec = runST $ do
    let !ndim = B.length dataVec
        !rows = U.length indices `quot` ndim

    out <- BM.unsafeNew rows

    let loop !row
          | row >= rows = pure ()
          | otherwise = do
              let !rowIdx = row * ndim

              rowVec <- M.unsafeNew ndim

              let inner !j
                    | j >= ndim = pure ()
                    | otherwise = do
                        let !idx = U.unsafeIndex indices (rowIdx + j)
                            !src = B.unsafeIndex dataVec j
                            -- !src = dataVec B.! j
                            !val = U.unsafeIndex src idx + salt

                        M.unsafeWrite rowVec j val
                        inner (j + 1)

              inner 0

              frozenRow <- U.unsafeFreeze rowVec
              BM.unsafeWrite out row frozenRow

              loop (row + 1)

    loop 0
    B.unsafeFreeze out

benchmark :: U.Vector Int -> Int -> B.Vector (U.Vector Int) -> Int
benchmark lengths iter inpt = go iter
    where
        go 1 =
            let !indices = cartesianIndices lengths
                !r = makeVectorRows 1 indices inpt
            in r `deepseq` B.length r  
        go n =
            let !indices = cartesianIndices lengths
                !r = makeVectorRows n indices inpt
            in r `deepseq` go (n - 1)

main :: IO ()
main = do
    let inpt1 = U.fromList [0,  1,  2,  3,  4]                    
        inpt2 = U.fromList [10, 11, 12, 13, 14]                      
        inpt3 = U.fromList [20, 21, 22, 23, 24]                      
        inpt4 = U.fromList [30, 31, 32, 33, 34]                      
        inpt5 = U.fromList [40, 41, 42, 43, 44]                       

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



