module Test.MySolutions where

import Prelude

import Control.MonadZero (guard)
import Data.Array (cons, filter, foldl, head, length, null, tail, (..))
import Data.Maybe (fromMaybe)
import Data.Path (Path(..))
import Test.Examples (allFiles, factors)

-- Note to reader: Add your solutions to this file
isEven :: Int -> Boolean
isEven x = mod x 2 == 0

countEven :: Array Int -> Int
countEven arr =
  if null arr
    then 0
    else calc (fromMaybe 1 $ head arr) + (countEven $ fromMaybe [] $ tail arr)
  where
    calc x = if isEven x then 1 else 0

squared :: Array Number -> Array Number
squared = map (\n -> n * n)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (\n -> n >= 0.0)

infix 4 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite arr = (\n -> n >= 0.0) <$?> arr

isPrime :: Int -> Boolean
isPrime 0 = false
isPrime 1 = false
isPrime n = (length $ factors n) == 1 

-- pure를 return으로 표기하면 명령형 코드와 유사해진다.
-- 실제로 haskell에서는 return으로 표기한다.
return :: forall f a. Applicative f => a -> f a
return = pure

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct arr1 arr2 = do
  x <- arr1
  y <- arr2
  return [x, y]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ (a * a + b * b) == (c * c)
  return [a, b, c]

factorize :: Int -> Array Int
factorize n = go n 2 []
  where
    go :: Int -> Int -> Array Int -> Array Int
    go n p acc =
      if n == p then cons p acc
      else if (mod n p) == 0 then go (n/p) (p+1) (cons p acc)
      else go n (p+1) acc

allTrue :: Array Boolean -> Boolean
allTrue = foldl (\r b -> r && b) true

fibTailRec :: Int -> Int
fibTailRec n = go 0 0 1
  where
    go x n1 n2 =
      if x == n
        then n2
        else go (x + 1) n2 (n1 + n2)

reverse :: forall a. Array a -> Array a
reverse = foldl (flip cons) []

onlyFiles :: Path -> Array Path
onlyFiles root = filter isFile $ allFiles root
  where
    isFile (File _ _) = true
    isFile (Directory _ _) = false
