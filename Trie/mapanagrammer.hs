import Data.Array
import Data.Binary
import Data.Bits
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace (trace)

data Node = Node { nodeLetter     :: Maybe Char
                 , nodePrefix     :: String
                 , nodeTerminates :: Bool
                 } deriving (Eq, Show)

data Trie a b = Trie a (Map b (Trie a b)) deriving (Eq, Show)

triePrefix :: Trie Node a -> String
triePrefix (Trie (Node _ prefix _) _) = prefix

push :: String -> Trie Node Char -> Trie Node Char
push = push' ""

push' :: String -> String -> Trie Node Char -> Trie Node Char
push' prefix (c:suff) (Trie node children) =
  Trie node (Map.insert c child' children)
    where child' = case Map.lookup c children of
                     Nothing    -> push' prefix' suff newChild
                     Just child -> push' prefix' suff child
          prefix' = prefix ++ [c]
          newChild = Trie node' Map.empty
          node' = Node (Just c) prefix' False
push' _ _ (Trie (Node letter prefix _) children) =
  Trie node' children
    where node' = Node letter prefix True

flatten :: Trie a b -> [Trie a b]
flatten trie = trie:childTries
  where childTries = concatMap flatten $ Map.elems children
        Trie _ children = trie

indexTries :: [Trie Node a] -> [(Word32,Trie Node a)]
indexTries tries = indexTries' tries 0

indexTries' :: [Trie Node a] -> Word32 -> [(Word32,Trie Node a)]
indexTries' (trie:tries) n = -- trace ("indexTries' " ++ prefix ++ " " ++ (show n)) $
  result
    where result = (n,trie):(indexTries' tries n') 
          n' = n+1 + fromIntegral (Map.size children)
          (Trie (Node _ prefix _) children) = trie
indexTries' [] _ = []

isWord :: Trie Node Char -> String -> Bool
isWord (Trie node children) (c:suff) = case Map.lookup c children of 
                                         Nothing -> False
                                         Just child -> isWord child suff
isWord (Trie (Node _ _ terminates) _) [] = terminates
        
makeTrie :: [String] -> Trie Node Char
makeTrie words = trie
  where rootNode = Node Nothing "" False
        rootTrie = Trie rootNode Map.empty
        !trie = foldl (flip push) rootTrie words

trieArray :: Map String Word32 -> [Trie Node Char] -> Array Int Word32
trieArray indices nodes = listArray bounds word32s
  where bounds = (0,(fromIntegral size)-1)
        size = length word32s
        word32s = concatMap (trieWord32s indices) nodes

writeTrie :: String -> Trie Node Char -> IO ()
writeTrie file trie = do
  let indices = indexTries $ flatten trie
  let makeAssoc (i,t) = (triePrefix t,i)
  let reverseIndices = Map.fromList $ map makeAssoc indices
  let nodes = map snd indices
  let trieLazyBS = encode $ trieArray reverseIndices nodes
  L.writeFile "csw.dawg" trieLazyBS

alphabet :: String
alphabet = ' ':['A'..'Z']

alphabetMap :: Map Char Int
alphabetMap = Map.fromList $ zip alphabet [0..]

alphaLookup :: Char -> Int
alphaLookup c = fromJust $ Map.lookup (toUpper c) alphabetMap

childrenBits :: [Char] -> Word32
childrenBits children = foldr (.|.) 0 $ map childBit children
  where childBit child = shiftL 1 $ alphaLookup child

trieWord32s :: Map String Word32 -> Trie Node Char -> [Word32]
trieWord32s indices (Trie (Node letter prefix terminates) children) =
  childBits ++ childIndices'
  where childBits = [(childrenBits $ Map.keys children)]
        childIndices' = map (\x -> x) childIndices
        childIndices = map lookupIndex $ Map.elems children
        lookupIndex child = case Map.lookup (triePrefix child) indices of
                              Nothing -> error $ "couldn't find " ++ (triePrefix child)
                              Just c -> c

main :: IO ()
main = do 
  wordFile <- readFile "csw.txt"
  let !words = lines wordFile
  let !trie = makeTrie words
  writeTrie "csw.dawg" trie
  putStrLn $ "isWord trie CATSUPS: " ++ (show $ isWord trie "CATSUPS")
  putStrLn $ "isWord trie CATSUPSS: " ++ (show $ isWord trie "CATSUPSS")