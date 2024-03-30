-- This module contains defines a Tree structure and its methods.
--
-- This source code serves as the submission to the first assignment
-- of class FLP at FIT, BUT 2023/24
--
-- @author  Hung Do
-- @date    04/03/2024
-- @file    Tree.hs
{-# LANGUAGE FlexibleInstances #-}

module Tree
  ( Tree (Leaf, Node),
    getClass,
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
getClass x (Node ix th l r)
  | x !! ix <= th = getClass x l
  | otherwise = getClass x r
