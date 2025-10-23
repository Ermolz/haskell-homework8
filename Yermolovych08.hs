{-# OPTIONS_GHC -Wall #-}
module Yermolovych08 where

import Data.List (sort)

data Rose a = Node a (Forest a) deriving (Show, Eq)
type Forest a = [Rose a]

data Bin a = Tip | BNode a (Bin a) (Bin a) deriving (Show, Eq)

-- Задача 1 -----------------------------------------
dfsForest :: Forest a -> [a]
dfsForest = concatMap (\(Node v children) -> v : dfsForest children)

-- Задача 2 -----------------------------------------
bfsForest :: Forest a -> [a]
bfsForest [] = []
bfsForest forest = map (\(Node v _) -> v) forest ++ bfsForest (concatMap (\(Node _ children) -> children) forest)

-- Задача 3 -----------------------------------------
toBin :: Forest a -> Bin a
toBin [] = Tip
toBin (Node v children : siblings) = BNode v (toBin children) (toBin siblings)

-- Задача 4 -----------------------------------------
fromBin :: Bin a -> Forest a
fromBin Tip = []
fromBin (BNode v l r) = Node v (fromBin l) : fromBin r

inOrder :: Bin a -> [a]
inOrder Tip = []
inOrder (BNode v l r) = inOrder l ++ [v] ++ inOrder r


isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- Задача 5 -----------------------------------------
isSearch :: (Ord a) => Bin a -> Bool
isSearch = isSorted . inOrder

-- Задача 6 -----------------------------------------
elemSearch ::(Ord a) => Bin a -> a -> Bool
elemSearch Tip _ = False
elemSearch (BNode v l r) x
  | x == v    = True
  | x < v     = elemSearch l x
  | otherwise = elemSearch r x

-- Задача 7 ------------------------------------------
insSearch :: (Ord a) => Bin a -> a -> Bin a
insSearch Tip x = BNode x Tip Tip
insSearch (BNode v l r) x
  | x <= v    = BNode v (insSearch l x) r
  | otherwise = BNode v l (insSearch r x)

findMin :: Bin a -> a
findMin (BNode v Tip _) = v
findMin (BNode _ l _) = findMin l
findMin Tip = error "Cannot find minimum in an empty tree"

-- Задача 8 ------------------------------------------
delSearch :: (Ord a) => Bin a -> a -> Bin a
delSearch Tip _ = Tip
delSearch (BNode v l r) x
  | x < v     = BNode v (delSearch l x) r
  | x > v     = BNode v l (delSearch r x)
  | otherwise = -- x == v
      case (l, r) of
        (Tip, _) -> r
        (_, Tip) -> l
        (_, _)   -> let successor = findMin r
                    in BNode successor l (delSearch r successor)

-- Задача 9 -----------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList = inOrder . foldr (flip insSearch) Tip

-- Provided test data
otx :: Forest Int
otx = [ Node 1 [Node 2 [],
                 Node 3 [Node 10 []] ] ,
        Node 4 [Node 5 [Node 8 []],
                Node 6 [Node 9 []],
                Node 7 []]
      ]

bt, bin1, bin2, bin3 :: Bin Int
bt = BNode 1(BNode 2 Tip
                       (BNode 3 (BNode 10 Tip Tip)
                                Tip)
            )
            (BNode 4  (BNode 5 (BNode 8  Tip Tip)
                               (BNode 6  (BNode 9 Tip Tip)
                                         (BNode 7 Tip Tip)
                               )
                      )
             Tip
            )

bin1 = BNode 7 (BNode 3 Tip Tip)
               (BNode 8 Tip Tip)

bin2 = BNode 7 (BNode 3 Tip (BNode 8 Tip Tip))
               Tip

bin3 = BNode 7 (BNode 3 Tip (BNode 5 Tip Tip))
               (BNode 8 Tip Tip)
