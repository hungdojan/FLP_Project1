-- This module contains function and utilities used to solve the second task of
-- the assignment.
--
-- @author  Hung Do
-- @date    04/06/2024
-- @file    TaskTwo.hs
module TaskTwo where

import           Control.Monad (forM, unless)
import           Data.Maybe    (fromJust, isNothing)
import           Parser        (split, strip)
import           Tree

type SingleData = ([Float], String)

type ClassPercentage = (String, Float)

type FeatureData = (Int, (Float, [SingleData], [SingleData]))

-- | Extract data from the given `String`.
extractDataTaskTwo :: String -> IO SingleData
extractDataTaskTwo str = do
  let splitData = map (strip (== ' ')) $ split str ','
      -- class is the last string in the list
      className = head $ take 1 $ reverse splitData
      -- thresholds are converted to `Float`
      thresholds = map (\x -> (read x :: Float)) $ init splitData
  return (thresholds, className)

-- | Validate feature list length (all the `SingleData` contains the same
-- number of features).
validateFeatureList :: [SingleData] -> Bool
validateFeatureList [] = True
validateFeatureList sd = length (toSet getFeatures) == 1
  where
    getFeatures = map (length . fst) sd

-- | Remove duplicate data from the list.
toSet :: (Ord a) => [a] -> [a]
toSet [] = []
toSet (x : xs)
  | x `elem` xs = toSet xs
  | otherwise = x : toSet xs

-- | Check if given `SingleData` has any features left.
isEmptyFeatures :: SingleData -> Bool
isEmptyFeatures ([], _) = True
isEmptyFeatures _       = False

-- | Return a list of features for the given feature index in `SingleData`.
-- Takes three arguments:
--    `[SingleData]` - The whole dataset.
--    `Int`          - Chosen index of the feature.
--    `Int`          - Total number of features.
getFeaturesAt :: [SingleData] -> Int -> Int -> [Float]
getFeaturesAt sd ix len
  | ix >= len = []
  | otherwise = map ((!! ix) . fst) sd

-- | Get list of class names within the dataset.
getClassNames :: [SingleData] -> [String]
getClassNames [] = []
getClassNames xs = toSet $ map snd xs

-- | Get percentage of occurrence of each class in the given dataset.
-- Returns a pair of `String` and `Float` where:
--    `String` - Class name.
--    `Float`  - Percentage of occurrence (< 1.0).
getClassPercentage :: [SingleData] -> IO [ClassPercentage]
getClassPercentage [] = return []
getClassPercentage sd@(h : _)
  | isEmptyFeatures h = return []
  | otherwise = do
      let _totalCount = length sd
      forM (getClassNames sd) $ \x -> do
        -- get number of occurrence of the class `x`
        let _count = length [head v | (v, cl) <- sd, cl == x]
        -- calculates with the formula (count / total)
        return (x, fromIntegral _count / fromIntegral _totalCount)

-- | Calculate Gini value for the given dataset.
-- The Gini formula is: 1 - sum([x**2 for x in `percentage`])
-- where percentage is the percentage of occurrence of the given class
-- in the dataset.
calculateGiniScore :: [ClassPercentage] -> Float
calculateGiniScore lcp = 1 - foldr ((+) . (** 2) . snd) 0 lcp

-- | Calculate weighted sum of Gini indices for the chosen datasets.
-- Takes two data sets. Returns their Gini index value.
giniForDataset :: [SingleData] -> [SingleData] -> IO Float
giniForDataset xs ys = do
  let xsSizeF :: Float = fromIntegral $ length xs
      ysSizeF :: Float = fromIntegral $ length ys
      totalSizeF :: Float = xsSizeF + ysSizeF
  xPerc <- getClassPercentage xs
  yPerc <- getClassPercentage ys
  -- calculates the weighted sum of all classes for the selected datasets
  -- the formula: sum([giniScore(dataset) * weight(dataset)
  --                   for dataset in datasets])
  return $
    ( calculateGiniScore xPerc * xsSizeF
        + calculateGiniScore yPerc * ysSizeF
    )
      / totalSizeF

-- | Search and calculate the smallest Gini index value for the given
-- feature index.
-- Takes 3 arguments:
--    `[SingleData]` - The whole data set.
--    `Int`          - Index of the feature.
--    `Int`          - Total number of features.
-- Returns a pair of `Float` and `FeatureData` where
--    `Float`       - Gini index value of the found dataset separation.
--    `FeatureData` - Feature data for the found dataset separation.
giniForFeature :: [SingleData] -> Int -> Int -> IO (Float, FeatureData)
giniForFeature sds ix len = do
  let features :: [Float] = toSet $ getFeaturesAt sds ix len
  ginis <- forM features $ \threshold -> do
    let less = [y | y <- sds, (head . fst) y <= threshold]
        greater = [y | y <- sds, (head . fst) y > threshold]
    giniValue <- giniForDataset less greater
    return (giniValue, (threshold, less, greater))
  let minGini = minGiniInpurity ginis
  return (fst minGini, (ix, snd minGini))

-- | Find min Gini index value.
minGiniInpurity :: (Ord a) => [(a, b)] -> (a, b)
minGiniInpurity [] = error "Nothing found"
minGiniInpurity [x] = x
minGiniInpurity (x : xs) = _getMin x xs
  where
    _getMin _x [] = _x
    _getMin (_x, pair1) ((v, pair2) : ys)
      | _x <= v = _getMin (_x, pair1) ys
      | otherwise = _getMin (v, pair2) ys

-- | Train a model on the given dataset.
trainOnDataSet :: [SingleData] -> Int -> IO (Maybe (Tree Int Float String))
trainOnDataSet [] _ = return Nothing
trainOnDataSet sd len
  | length (getClassNames sd) == 1 = return $ Just $ Leaf $ head $ getClassNames sd
  | otherwise = do
      featureIndices <- forM [0 .. (len - 1)] $ \x -> giniForFeature sd x len
      let (_, smallestGini) = minGiniInpurity featureIndices
          (ix, (th, l, r)) = smallestGini
      leftChild <- trainOnDataSet l len
      rightChild <- trainOnDataSet r len
      if isNothing leftChild || isNothing rightChild
        then return Nothing
        else return $ Just $ Node ix th (fromJust leftChild) (fromJust rightChild)

-- | Task 2 logic.
taskTwo :: FilePath -> IO ()
taskTwo fInput = do
  fileContent <- readFile fInput
  ds <- forM (lines fileContent) extractDataTaskTwo
  unless (validateFeatureList ds) (error "Features not aligned")
  tree <- trainOnDataSet ds ((length . fst . head) ds)
  print $ fromJust tree
