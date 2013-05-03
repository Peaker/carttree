{-# OPTIONS -Wall -O2 #-}
-- Transliteration of: http://tomerfiliba.com/blog/Cartesian-Tree-Product/
-- Run with: ghc carttree.hs && ./carttree
import Control.Applicative
import Control.Monad.State
import qualified Data.List as List

sortNub :: Ord a => [a] -> [a]
sortNub = map head . List.group . List.sort

data Op = Or | And deriving (Eq, Ord)
instance Show Op where
  show Or = "|"
  show And = "&"

data Tree = Pred String | Combine Op Tree Tree deriving (Eq, Ord)
instance Show Tree where
  show (Pred x) = x
  show (Combine op x y) = concat ["(", show x, " ", show op, " ", show y, ")"]

cartesianTreeProduct :: Tree -> [Tree]
cartesianTreeProduct (Pred x) = [Pred x]
cartesianTreeProduct (Combine op lhs rhs) = do
  l <- nubCartesianTreeProduct lhs
  r <- nubCartesianTreeProduct rhs
  case op of
    Or -> [l, r]
    _ -> [Combine op l r]

nubCartesianTreeProduct :: Tree -> [Tree]
nubCartesianTreeProduct = sortNub . cartesianTreeProduct

p :: String -> Tree
p = Pred

orP, andP :: Tree -> Tree -> Tree
x `andP` y = Combine And x y
x `orP` y = Combine Or x y

expr :: Tree
expr = ((p "x=5" `orP` p "y=6") `andP` ((p "z=7" `orP` p "w=8") `orP` p "q=9"))
       `andP` p "r=10"

mkExp :: Int -> Tree
mkExp = (`evalState` (0 :: Int)) . loop
  where
    next = get <* modify (+1)
    loop 0 = (\c -> Pred $ "x" ++ show c) <$> next
    loop n =
      let op | even n = And
             | otherwise = Or
      in Combine op <$> loop (n-1) <*> loop (n-1)

main :: IO ()
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
