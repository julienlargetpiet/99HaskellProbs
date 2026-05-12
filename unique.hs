import qualified Data.Set as Set

uniqueOrd :: Ord a => [a] -> [a]
uniqueOrd xss = go Set.empty xss
  where
    go _ [] = []
    go seen (x:xs)
      | x `Set.member` seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs
