-- This module contains function and utilities used to solve the second task of
-- the assignment.
--
-- This source code serves as the submission to the first assignment
-- of class FLP at FIT, BUT 2023/24
--
-- @author  Hung Do
-- @date    04/06/2024
-- @file    TaskTwo.hs
module TaskTwo where

import           Control.Arrow ((&&&))
import           Control.Monad (unless)
import           Data.List     (group, sort, sortBy)
import           Data.Maybe    (fromJust, isNothing)
import           Parser        (split, strip)
import           Tree

type SingleData = ([Float], String)

type ClassPercentage = (String, Float)

type TreeWrapper = (Int, Float, [SingleData], [SingleData])

-- | Extract data from the given `String`.
extractDataSet :: String -> SingleData
extractDataSet str = _toSingleData _splitData
  where
    _splitData = map (strip (== ' ')) $ split str ','
    _toSingleData [] = ([], "")
    _toSingleData lst = (_features, _class)
      where
        _features = map (read :: String -> Float) (init lst)
        _class = last lst

-- | Validate feature list length (all the `SingleData` contains the same
-- number of features).
validateFeatureList :: [SingleData] -> Bool
validateFeatureList [] = True
validateFeatureList sd = length _featureLenSet == 1 && head _featureLenSet > 0
  where
    _featureLenSet = toSet $ map (length . fst) sd

-- | Remove duplicate data from the list.
toSet :: (Ord a) => [a] -> [a]
toSet [] = []
toSet (x : xs)
  | x `elem` xs = toSet xs
  | otherwise = x : toSet xs

-- | Calculates a threshold value as an average of neighbor thresholds.
-- Args:
--    `[Float]` - List of thresholds.
-- Returns:
--    `[Float]` - New range of thresholds.
calculateMiddleThreshold :: [Float] -> [Float]
calculateMiddleThreshold [] = []
calculateMiddleThreshold [a] = [a]
calculateMiddleThreshold (x : y : xs) = ((x + y) / 2) : calculateMiddleThreshold (y : xs)

-- | Return a list of middle thresholds for the given feature index in `SingleData`.
-- Takes three arguments:
--    `[SingleData]` - The whole dataset.
--    `Int`          - Chosen index of the feature.
getMidThresholdAt :: [SingleData] -> Int -> [Float]
getMidThresholdAt sd ix = calculateMiddleThreshold $ sort $ map ((!! ix) . fst) sd

-- | Return a list of thresholds for the given feature index in `SingleData`.
-- Takes three arguments:
--    `[SingleData]` - The whole dataset.
--    `Int`          - Chosen index of the feature.
getThresholdAt :: [SingleData] -> Int -> [Float]
getThresholdAt sd ix = map ((!! ix) . fst) sd

-- | Get list of class names within the dataset.
getClassNames :: [SingleData] -> [String]
getClassNames [] = []
getClassNames xs = toSet $ map snd xs

-- | Get a percentage occurrence for each class.
getOccurrence :: Int -> [SingleData] -> [ClassPercentage]
getOccurrence len dx = map (\(x, y) -> (x, fromIntegral y / fromIntegral len)) getCounts
  where
    _popClassName (_, cn) = cn
    getCounts = map (head &&& length) $ group . sort $ map _popClassName dx

-- | Calculate Gini value for the given dataset.
-- The Gini formula is: 1 - sum([x**2 for x in `percentage`])
-- where percentage is the percentage of occurrence of the given class
-- in the dataset.
calculateGiniScore :: [ClassPercentage] -> Float
calculateGiniScore lofCP = 1 - foldr ((+) . (** 2) . snd) 0 lofCP

-- | Split sorted dataset based on the given feature index.
-- The list (dataset) is split into two sublists where the first sublist contains
-- `SingleData` where its feature value is less than the given feature threshold.
-- The rest of data will be moved into the second sublist.
-- Args:
--    `[SingleData]` - Given dataset.
--    `Int` - Feature index.
--    `Float` - Feature threshold.
-- Returns:
--    `[[SingleData]]` - A list that contains 2 sublists.
splitSortedDataSet :: [SingleData] -> Int -> Float -> [[SingleData]]
splitSortedDataSet [] _ _ = [[], []]
splitSortedDataSet xs ix th = helpFunc [] xs
  where
    helpFunc _l [] = [_l, []]
    helpFunc _l _ll@(_x : _xs)
      | ((!! ix) . fst) _x <= th = helpFunc (_x : _l) _xs
      | otherwise = [_l, _ll]

-- | Sort the dataset on the given feature index.
sortByFeature :: [SingleData] -> Int -> [SingleData]
sortByFeature sd ix = sortBy cmpFnc sd
  where
    cmpFnc (f, _) (s, _) = compare (f !! ix) (s !! ix)

-- | Calculate Gini index value for the given dataset split.
-- Args:
--    `Int` - Feature index.
--    `Float` - Feature threshold.
--    `[[SingleData]]` - Split dataset.
-- Returns:
--    `(Float, TreeWrapper)` - A pair where:
--        `Float` - Calculated Gini index value.
--        `TreeWrapper` - Data format ready to create a Tree structure.
_giniForSplit :: Int -> Float -> [[SingleData]] -> (Float, TreeWrapper)
_giniForSplit _ _ [] =  (20, (0, 0, [], []))
_giniForSplit _ _ [_] = (20, (0, 0, [], []))
_giniForSplit ix th dss@(l : r : _) = (_calculateGini, (ix, th, l, r))
  where
    _calculateGini = sum (map _calculateWeightedSum dss) / fromIntegral (sum (map length dss))
    _calculateWeightedSum d =
      let len = length d
       in (fromIntegral len * (calculateGiniScore . getOccurrence len) d)

-- | Calculate Gini index value for the chosen feature threshold in the dataset.
-- First it does the dataset split and then it calculates the value.
-- Args:
--    `[SingleData]` - Given dataset.
--    `Int` - Feature index.
--    `Float` - Feature threshold.
-- Returns:
--    `(Float, TreeWrapper)` - A pair where:
--        `Float` - Calculated Gini index value.
--        `TreeWrapper` - Data format ready to create a Tree structure.
_giniForThreshold :: [SingleData] -> Int -> Float -> (Float, TreeWrapper)
_giniForThreshold ds ix th = _giniForSplit ix th $ splitSortedDataSet (sortByFeature ds ix) ix th

-- | Find the split with the smallest Gini index value in the chosen feature index.
_minGiniForFeature :: [SingleData] -> Int -> (Float, TreeWrapper)
_minGiniForFeature [] _ = (20, (0, 0, [], []))
_minGiniForFeature ds ix = minFirst $ map (_giniForThreshold ds ix) _features
  where
    _features = toSet $ getMidThresholdAt ds ix

-- | Find min Gini index value. Expecting the first element of the tuple to be
-- the Gini index value.
minFirst :: (Ord a) => [(a, b)] -> (a, b)
minFirst [] = error "Nothing found"
minFirst [x] = x
minFirst (x : xs) = _getMin x xs
  where
    _getMin _x [] = _x
    _getMin (_x, pair1) ((v, pair2) : ys)
      | _x <= v = _getMin (_x, pair1) ys
      | otherwise = _getMin (v, pair2) ys

-- | Check if there is only one class left in the given dataset.
isSingleClass :: [SingleData] -> Bool
isSingleClass [] = True
isSingleClass (x : xs) = isSameClass xs (snd x)
  where
    isSameClass [] _ = True
    isSameClass (_x : _xs) lbl
      | snd _x /= lbl = False
      | otherwise = isSameClass _xs lbl

-- | Train and construct a decision tree using Gini index.
-- Args:
--    `[SingleData]` - Given dataset.
--    `Int` - Number of features.
train :: [SingleData] -> Int -> Maybe (Tree Int Float String)
train [] _ = Nothing
train sd fLen
  | isSingleClass sd = Just $ Leaf $ (snd . head) sd
  | isNothing leftTree || isNothing rightTree = Nothing
  | otherwise = Just $ Node ix th (fromJust leftTree) (fromJust rightTree)
  where
    (_, (ix, th, l, r)) = minFirst $ map (_minGiniForFeature sd) [0 .. (fLen - 1)]
    leftTree = train l fLen
    rightTree = train r fLen

-- | Task 2 logic.
taskTwo :: FilePath -> IO ()
taskTwo fInput = do
  fileContent <- readFile fInput
  -- extract data and ignore empty lines
  let ds = map extractDataSet $ filter (/= "") $ lines fileContent
  unless (validateFeatureList ds) (error "Features not aligned")
  case train ds ((length . fst . head) ds) of
    Nothing  -> print "Error while training"
    (Just t) -> print t
