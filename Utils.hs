module Utils where

-- |takeWhile but including the first element that not meets the requirements
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

-- |Adds two Positions
positionPlus :: (Int, Int) -> (Int, Int) -> (Int, Int)
positionPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- |Returns a Bool with only the first values compared
tupleEqual, tupleMaximum, tupleMinimum :: (Eq a, Ord a) => (a,b) -> (a,c) -> Bool
tupleEqual   = tupleCompare (==)
tupleMaximum = tupleCompare (>)
tupleMinimum = tupleCompare (<)

tupleCompare :: (a -> a -> Bool) -> (a,b) -> (a,c) -> Bool
tupleCompare c (v1,_) (v2,_) = c v1 v2

-- |Returns a list of tuples with the biggest first value
tupleMinima, tupleMaxima :: [(Int,a)] -> [(Int,a)]
tupleMaxima = tupleExtrema tupleMaximum
tupleMinima = tupleExtrema tupleMinimum

tupleExtrema :: (Eq a, Ord a) => ((a,b) -> (a,b) -> Bool) -> [(a,b)] -> [(a,b)]
tupleExtrema c = foldr accumTuples []
    where accumTuples t [] = [t]
          accumTuples a acc@(t:_)
            | tupleEqual a t = a:acc
            | c a t          = [a]
            | otherwise      = acc
