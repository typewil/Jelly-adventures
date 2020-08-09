
type Tuple = (Int,Int)

sumar :: Int -> Int -> Int
sumar a b = a+b


addTuple :: [(Int,Int)] -> Tuple -> [Tuple]
addTuple list tuple = list ++ [tuple]
