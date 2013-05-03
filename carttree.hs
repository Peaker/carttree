{-# OPTIONS -O2 #-}
-- Transliteration of: http://tomerfiliba.com/blog/Cartesian-Tree-Product/
-- Run with: ghc carttree.hs && ./carttree
import Control.Applicative
import Control.Monad.State
import qualified Data.List as List

sortNub = map head . List.group . List.sort

data Op = Or | And deriving (Eq, Ord, Show)
data Tree = Pred String | Combine Op Tree Tree deriving (Eq, Ord, Show)

cartesianTreeProduct (Pred x) = [Pred x]
cartesianTreeProduct (Combine op lhs rhs) = do
  l <- nubCartesianTreeProduct lhs
  r <- nubCartesianTreeProduct rhs
  case op of
    Or -> [l, r]
    _ -> [Combine op l r]

nubCartesianTreeProduct = sortNub . cartesianTreeProduct

x `andP` y = Combine And x y
x `orP` y = Combine Or x y

expr = ((Pred "x=5" `orP` Pred "y=6") `andP` ((Pred "z=7" `orP` Pred "w=8") `orP` Pred "q=9"))
       `andP` Pred "r=10"

mkExp = (`evalState` (0 :: Int)) . loop
  where
    next = get <* modify (+1)
    loop 0 = (\c -> Pred $ "x" ++ show c) <$> next
    loop n =
      let op | even n = And
             | otherwise = Or
      in Combine op <$> loop (n-1) <*> loop (n-1)

main = do
  mapM_ print $ cartesianTreeProduct expr
-- ((x=5 & q=9) & r=10)
-- ((x=5 & w=8) & r=10)
-- ((x=5 & z=7) & r=10)
-- ((y=6 & q=9) & r=10)
-- ((y=6 & w=8) & r=10)
-- ((y=6 & z=7) & r=10)
  print $ mkExp 3
-- (((x0 | x1) & (x2 | x3)) | ((x4 | x5) & (x6 | x7)))
  forM_ [1..6] $ \i -> do
    let variants = nubCartesianTreeProduct $ mkExp i
    putStrLn $ unwords [show i, show (length variants)]
-- 1 2
-- 2 4
-- 3 8
-- 4 64
-- 5 128
-- 6 16384
