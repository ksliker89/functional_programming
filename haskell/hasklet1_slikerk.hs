--produce a list of differences between consecutive elements in the given list of integers.
-- >>> diff[4,2,7,3,6,5]
-- [2,-5,4,-3,1]


diff :: [Int] -> [Int]
-- diff list = map head list - drop 1 list  

diff (x:y:ys) = x - y : diff (y:xs)




-- produce a list where sequences of repeated elements in the given list are replace by just one of the repeated element.
-- >>> compress [1,1,1,2,3,3,3,1,2,2]
-- [1,2,3,1,2]


compress :: [Int] -> [Int]
compress [] = []
compress list
	| head list /= head (drop 1 list) = head list
	| otherwise = new list                                     
		where
		new list = head list : compress (drop 1 list)

-- compress (x:xs) is a pattern matching from x to x's
compress (x:xs) = case xs of
		    []     -> [x]
		    (y:ys) -> if x == y 
		    		then compress xs 
		    		else x : compress xs

-- this statement is the same as writing two base cases compress [] = [] AND compress [x] = [x]
compress xs = xs
