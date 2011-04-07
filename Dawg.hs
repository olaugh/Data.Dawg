import Data.Array.Unboxed
import Data.Binary
import qualified Data.ByteString as B
import Data.ByteString.Internal
import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as L
import Debug.Trace (trace)
import Foreign

alphabet :: String
alphabet = ' ':['A'..'Z']

alphabetArr :: UArray Int Char
alphabetArr = listArray (0,(length alphabet)-1) alphabet

readDawg :: String -> IO (UArray Int Word32)
readDawg file = L.readFile file >>= return . decode

dawgWords :: UArray Int Word32 -> [String]
dawgWords dawg = dawgWords' dawg "" 0 False

dawgWords' :: UArray Int Word32 -> String -> Word32 -> Bool -> [String]
dawgWords' dawg prefix i term = prefix' ++ (concat words)
  where prefix' = if term then [prefix] else []
        !words = zipWith3 (dawgWords' dawg) prefixes indices terms
        i' = fromIntegral i
        childBits = dawg ! i'
        !letters = map (alphabetArr!) $ oneBits childBits
        !prefixes = map (\c -> prefix++[c]) letters
        !children = map (dawg!) [i'+1..]
        !indices = map getIndex children
        !terms = map (`testBit` termBitPos) children

--{-# NOINLINE peekAt #-}
peekAt :: Ptr Word32 -> Int -> Word32
peekAt ptr i =
--  trace ("peekAt " ++ show ptr ++ " " ++ show i ++ ": " ++ show result) $
  result where result = unsafePerformIO $ peekElemOff ptr i

ptrDawgWords :: Ptr Word32 -> [String]
ptrDawgWords dawg = ptrDawgWords' dawg "" 0 False

ptrDawgWords' :: Ptr Word32 -> String -> Word32 -> Bool -> [String]
ptrDawgWords' dawg prefix i term =
    --trace ((show dawg) ++ " " ++ show i ++ " " ++ show term) $
    prefix' ++ (concat words)
  where prefix' = if term then [prefix] else []
        !words = zipWith3 (ptrDawgWords' dawg) prefixes indices terms
        i' = fromIntegral i
        childBits = dawg `peekAt` i'
        !letters = map (alphabetArr!) $ oneBits childBits
        !prefixes = map (\c -> prefix++[c]) letters
        !children = map (dawg `peekAt`) [i'+1..]
        !indices = map getIndex children
        !terms = map (`testBit` termBitPos) children

termBitPos :: Int
termBitPos = 31

indexMask :: Word32
indexMask = complement $ bit termBitPos

getIndex :: Word32 -> Word32
getIndex child = child .&. indexMask

oneBits :: Word32 -> [Int]
oneBits w = filter (testBit w) [1..26]

flipEndian :: [Word8] -> [Word8]
flipEndian (a:b:c:d:xs) = (d:c:b:a:(flipEndian xs))
flipEndian _ = []

main :: IO ()
main = do
  dawgBS <- B.readFile "csw.dawg"
  let (PS fp _ _) = dawgBS
  let ptr = unsafeForeignPtrToPtr $ ((castForeignPtr fp)::(ForeignPtr Word32))
  mapM_ putStrLn $ ptrDawgWords ptr
  touchForeignPtr fp

