-- This module contains defines a Tree structure and its methods.
--
-- @author  Hung Do
-- @date    04/03/2024
-- @file    Tree.hs

module Tree
  ( Tree (Leaf, Node),
    getClass,
    testTree,
    leafTree,
  )
where

data Tree ix th cl
  = Leaf cl
  | Node ix th (Tree ix th cl) (Tree ix th cl)
  deriving (Eq)

-- | Defining Show function to match the Tree format in the text file.
instance Show (Tree Int Float String) where
  show (Leaf cl) = "Leaf " ++ cl
  show (Node ix th l r) =
    "Node: "
      ++ show ix
      ++ ", "
      ++ show th
      ++ recShow 1 l
      ++ recShow 1 r
    where
      recShow n (Leaf cl) =
        "\n"
          ++ replicate (2 * n) ' '
          ++ "Leaf: "
          ++ cl
      recShow n (Node _ix _th _l _r) =
        "\n"
          ++ replicate (2 * n) ' '
          ++ "Node: "
          ++ show _ix
          ++ ", "
          ++ show _th
          ++ recShow (n + 1) _l
          ++ recShow (n + 1) _r

-- | Calculating a class based on the given list of values.
getClass :: [Float] -> Tree Int Float String -> String
getClass _ (Leaf c) = c
getClass [] Node {} = error "No leaf found"
getClass (x : xs) (Node _ th l r)
  | x < th = getClass xs l
  | otherwise = getClass xs r

testTree :: Tree Int Float String
testTree = Node 0 5.5 (Leaf "Class1") (Node 1 3.0 (Leaf "Class2") (Leaf "Class3"))

leafTree :: Tree Int Float String
leafTree = Leaf "Hello"
