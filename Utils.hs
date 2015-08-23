module Utils where

-- |takeWhile but including the first element that not meets the requirements
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

-- |Adds two Positions
positionPlus :: (Int, Int) -> (Int, Int) -> (Int, Int)
positionPlus (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- |Returns the tuple with the biggest first value
tupleMaximum, tupleMinimum :: [(Int,a)] -> (Int, a)
tupleMaximum = tupleCompare (>)
tupleMinimum = tupleCompare (<)

tupleCompare :: (a -> a -> Bool) -> [(a,b)] -> (a,b)
tupleCompare c = foldr1 (\a@(v1,_) b@(v2,_) -> if c v1 v2 then a else b)

-- |Returns a list of tuples with the biggest first value
tupleMinima, tupleMaxima :: [(Int,a)] -> [(Int,a)]
tupleMaxima = tupleExtrema (>)
tupleMinima = tupleExtrema (<)

tupleExtrema :: (Eq a) => (a -> a -> Bool) -> [(a,b)] -> [(a,b)]
tupleExtrema c = foldr accumTuples []
    where accumTuples t [] = [t]
          accumTuples a@(v1,_) ts@((v2,_):_)
            | v1 == v2  = a:ts
            | c v1 v2   = [a]
            | otherwise = ts
