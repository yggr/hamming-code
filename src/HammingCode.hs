import Data.Char
import Data.List (transpose)

type Bit = Int

-- binary to integer
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

-- integer to binary
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 $ bits ++ repeat 0

encode :: String -> [Bit]
encode = concat . map (concat . gx . make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int . (\(x,y) -> x ++ y) . backTo8 . take 8 . checkParity) . chop14

-- Matrix multiplication
matrixMult :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMult xs ys = [map (sum . zipWith (*) r) $ transpose ys | r <- xs]

-- generate a new code including parity bits using G matrix
gx :: [Bit] -> [[Bit]]
gx bits = map (map (`mod` 2)) $ matrixMult g (makeMatrix8 bits)

-- new code generator matrix, G
g :: [[Bit]]
g = [ [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, 0],
      [0, 0, 0, 1],
      [1, 0, 1, 1],
      [1, 1, 0, 1],
      [0, 1, 1, 1] ]

-- Parity check matrix, P
p :: [[Bit]]
p = [ [1, 0, 1, 1, 1, 0, 0],
      [1, 1, 0, 1, 0, 1, 0],
      [0, 1, 1, 1, 0, 0, 1] ]

-- check parity bits by taking in GX
checkParity :: [Bit] -> [Bit]
checkParity [] = []
checkParity bits | checkErrorMatrix bits          == [[0,0],[0,0],[0,0]] = bits
                 | otherwise                      = [0,0,0,0,0,0] --just for testing
                   where
                     checkErrorMatrix bs = map (map (`mod` 2)) $ matrixMult p (makeC bs)

--fixBits :: [Bit] -> [Bit]
--fixBits bits = 

-- cut 8 bits into two lists
chop4 :: [Bit] -> [[Bit]]
chop4 []   = []
chop4 bits = take 4 bits : chop4 (drop 4 bits)

-- chop into lists of 14 for decoding
chop14 :: [Bit] -> [[Bit]]
chop14 []   = []
chop14 bits = take 14 bits : chop14 (drop 14 bits)

-- make 8 bit 4x2 matrix. X, to be multiplied with G
makeMatrix8 :: [Bit] -> [[Bit]]
makeMatrix8 = transpose . chop4

--make 7x2 matrix, C
makeC :: [Bit] -> [[Bit]]
makeC [] = []
makeC [_] = []
makeC (x:y:zs) = [x,y] : makeC zs

-- order the 8 bits correctly after matrix transformations
backTo8 :: [Bit] -> ([Bit],[Bit])
backTo8 [] = ([],[])
backTo8 [x] = ([x],[])
backTo8 (x:y:xs) = (x:xp, y:yp) where (xp, yp) = backTo8 xs

