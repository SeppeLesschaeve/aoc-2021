import Data.List
import Data.Bifunctor
import Data.Maybe

data Packet = Packet Int Packetdata
data Packetdata = Lit Int | Op Packetop [Packet]
type Packetop = [Int] -> Int

type Bits = [Bool]
type ParseResult a = (a, Bits)
type Parse a = Bits -> ParseResult a

hexToBinary :: String -> Bits
hexToBinary = concatMap hexCharToBinary

hexCharToBinary :: Char -> Bits
hexCharToBinary digit = map (== '1') $ case digit of {
	'0' -> "0000"; '4' -> "0100"; '8' -> "1000"; 'C' -> "1100";
	'1' -> "0001"; '5' -> "0101"; '9' -> "1001"; 'D' -> "1101";
	'2' -> "0010"; '6' -> "0110"; 'A' -> "1010"; 'E' -> "1110";
	'3' -> "0011"; '7' -> "0111"; 'B' -> "1011"; 'F' -> "1111"
}

binaryToInt :: Bits -> Int
binaryToInt = foldl' (\n bit -> n*2 + fromEnum bit) 0

chop :: Int -> [a] -> ([a], [a])
chop n list = (take n list, drop n list)

first2 :: [a] -> (a, a)
first2 (a:b:_) = (a, b)

parseHelper :: Bits -> Maybe (ParseResult Packet)
parseHelper (v1:v2:v3: t1:t2:t3: bits)  = Just . first (Packet version) $ parseData typeID bits
 where 
   version = binaryToInt [v1,v2,v3]
   typeID  = binaryToInt [t1,t2,t3]
parseHelper _ = Nothing


parseData :: Int -> Parse Packetdata
parseData 4 = first (Lit . binaryToInt) . parsePartialData
parseData n = first (Op $ n2op n) . uncurry parseSubPackets . fromJust . uncons

parsePartialData :: Parse Bits
parsePartialData (cont:bits) = first (bitgroup ++) $ if cont then parsePartialData bits' else ([], bits')
  where (bitgroup, bits') = chop 4 bits
	

parseSubPackets :: Bool -> Parse [Packet]
parseSubPackets False = first parse . uncurry chop . first binaryToInt . chop 15
parseSubPackets True = uncurry parseN . first binaryToInt . chop 11

parse :: Bits -> [Packet]
parse bits = case parseHelper bits of
	Just (packet, bits') -> packet:(parse bits')
	Nothing -> []

parseN :: Int -> Parse [Packet]
parseN 0 bits = ([], bits)
parseN n bits = case parseHelper bits of
	Just (packet, bits') ->
		let (packets, bits'') = parseN (n-1) bits'
		in (packet:packets, bits'')
	Nothing -> ([], bits)

n2op :: Int -> Packetop
n2op n 
  | n == 0 = sum
  | n == 1 = product
  | n == 2 = minimum
  | n == 3 = maximum
  | n == 5 = fromEnum . uncurry (>) . first2
  | n == 6 = fromEnum . uncurry (<) . first2
  | n == 7 = fromEnum . uncurry (==) . first2

eval :: Packet -> Int
eval (Packet _ packetdata) = case packetdata of
	Lit n -> n
	Op op packets -> op $ map eval packets

parseInput :: String -> Packet
parseInput = fst . fromJust . parseHelper . hexToBinary

main = do
	input <- parseInput <$> readFile "input16.txt"
	print $ eval input
