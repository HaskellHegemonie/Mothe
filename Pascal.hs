module Pascal where
import Data.Array
import Data.Function

pascal = scanl (\b _ -> zipWith (+) b (0 : b) <> [1]) [1] . enumFromTo 1

poscol :: Int -> Array Int [Int]
poscol x =  orr
  where
    ot x = zipWith (+) (orr ! x) (0 : orr ! x) <> [1]
    orr = listArray (0, x) $ [[1]] <> map ot [0..x]
    
