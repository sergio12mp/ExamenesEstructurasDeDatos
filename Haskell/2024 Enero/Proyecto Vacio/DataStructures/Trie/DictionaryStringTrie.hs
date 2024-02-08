-------------------------------------------------------------------------------
-- Student's name:
-- Student's group:
-- Identity number (DNI if Spanish/passport if Erasmus):
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Trie.DictionaryStringTrie(
    Trie()
  , empty
  , isEmpty
  , size
  , search
  , insert
  , strings
  , fromList
  , pretty
  , sampleTrie, sampleTrie1, sampleTrie2, sampleTrie3, sampleTrie4
  -- sizeValue, toTrie, childOf, update
  ) where

import qualified Control.DeepSeq as Deep
import Data.Maybe
import qualified DataStructures.Dictionary.AVLDictionary as D

data Trie a = Empty | Node (Maybe a) (D.Dictionary Char (Trie a)) deriving Show

-------------------------------------------------------------------------------
-- DO NOT WRITE ANY CODE ABOVE ------------------------------------------------
-------------------------------------------------------------------------------

-- | = Exercise a - empty
empty :: Trie a
empty = Empty

-- | = Exercise b - isEmpty
isEmpty :: Trie a -> Bool
isEmpty = undefined

-- | = Exercise c - sizeValue
sizeValue :: Maybe a -> Int
sizeValue = undefined

-- | = Exercise d - size
size :: Trie a -> Int
size = undefined

-- | = Exercise e - toTrie
toTrie :: Maybe (Trie a) -> Trie a
toTrie = undefined

-- | = Exercise f - childOf
childOf :: Char -> Trie a -> Trie a
childOf = undefined

-- | = Exercise g - search
search :: String -> Trie a -> Maybe a
search = undefined

-- | = Exercise h - update
update :: Trie a -> Char -> Trie a -> Trie a
update = undefined

-- | = Exercise i - insert
insert :: String -> a -> Trie a -> Trie a
insert = undefined

-------------------------------------------------------------------------------
-- ONLY FOR PART TIME STUDENTS ------------------------------------------------
-------------------------------------------------------------------------------

-- | = Exercise e1 - strings
strings :: Trie a -> [String]
strings t = undefined

-- | = Exercise e2 - fromList
fromList :: [String] -> Trie Int
fromList xs = undefined

-------------------------------------------------------------------------------
-- DO NOT WRITE ANY CODE BELOW ------------------------------------------------
-------------------------------------------------------------------------------

pretty :: (Show a) => Trie a -> IO ()
pretty t = putStrLn (showsTrie t "")

showsTrie :: (Show a) => Trie a -> ShowS
showsTrie Empty       = shows "Empty"
showsTrie (Node mb d) = showString "Node " . showValue mb . showChar '\n' . aux 1 d
  where
    aux n d =
      foldr (.) id [ showString (replicate (6*n) ' ')
                      . showChar c
                      . showString " -> "
                      . showString "Node "
                      . showValue mb
                      . showChar '\n'
                      . aux (n+1) d'
                    | (c, Node mb d') <- D.keysValues d
                    ]

    showValue mb = maybe (shows mb) (const (showChar '(' . shows mb . showChar ')')) mb

instance (Eq a) => Eq (Trie a) where
  Empty     == Empty       = True
  Node mb d == Node mb' d' = mb == mb' && d == d'
  _         == _           = False

instance (Deep.NFData a) => Deep.NFData (Trie a) where
  rnf Empty       = ()
  rnf (Node mb d) = mb `Deep.deepseq` rnfDict d
    where
      rnfDict = D.foldKeysValues (\k v d -> k `Deep.deepseq` v `Deep.deepseq` v `Deep.deepseq` d) ()


sampleTrie :: Trie Integer
sampleTrie = n0
   -- bat -> 0  be -> 1  bed -> 2  cat -> 3  to -> 4  toe -> 5
   where
      children = foldr (uncurry D.insert) D.empty
      n0 = Node Nothing $ children [('b', n1), ('c', n6), ('t', n9)]
      n1 = Node Nothing $ children [('a', n2), ('e', n4)]
      n2 = Node Nothing $ children [('t', n3)]
      n3 = Node (Just 0) $ children []
      n4 = Node (Just 1) $ children [('d', n5)]
      n5 = Node (Just 2) $ children []
      n6 = Node Nothing $ children [('a', n7)]
      n7 = Node Nothing $ children [('t', n8)]
      n8 = Node (Just 3) $ children []
      n9 = Node Nothing $ children [('o', n10)]
      n10 = Node (Just 4) $ children [('e', n11)]
      n11 = Node (Just 5) $ children []

sampleTrie1 :: Trie Integer
sampleTrie1 = n0
   -- a -> 3  b -> 2  c -> 1
   where
      children = foldr (uncurry D.insert) D.empty
      n0 = Node Nothing $ children [('a', n1), ('b', n2), ('c', n3)]
      n1 = Node (Just 3) $ children []
      n2 = Node (Just 2) $ children []
      n3 = Node (Just 1) $ children []

sampleTrie2 :: Trie Integer
sampleTrie2 = n0
   -- a -> 1  ab -> 2  abc -> 3  abd -> 4  acdef -> 5
   where
      children = foldr (uncurry D.insert) D.empty
      n0 = Node Nothing $ children [('a', n1)]
      n1 = Node (Just 1) $ children [('b', n2), ('c', n5)]
      n2 = Node (Just 2) $ children [('c', n3), ('d', n4)]
      n3 = Node (Just 3) $ children []
      n4 = Node (Just 4) $ children []
      n5 = Node Nothing $ children [('d', n6)]
      n6 = Node Nothing $ children [('e', n7)]
      n7 = Node Nothing $ children [('f', n8)]
      n8 = Node (Just 5) $ children []

sampleTrie3 :: Trie Integer
sampleTrie3 = n0
   -- abcd -> 1
   where
      children = foldr (uncurry D.insert) D.empty
      n0 = Node Nothing $ children [('a', n1)]
      n1 = Node Nothing $ children [('b', n2)]
      n2 = Node Nothing $ children [('c', n3)]
      n3 = Node Nothing $ children [('d', n4)]
      n4 = Node (Just 1) $ children []

sampleTrie4 :: Trie Integer
sampleTrie4 = n0
   -- abcd -> 1  def -> 2
   where
      children = foldr (uncurry D.insert) D.empty
      n0 = Node Nothing $ children [('a', n1), ('d', n5)]
      n1 = Node Nothing $ children [('b', n2)]
      n2 = Node Nothing $ children [('c', n3)]
      n3 = Node Nothing $ children [('d', n4)]
      n4 = Node (Just 1) $ children []
      n5 = Node Nothing $ children [('e', n6)]
      n6 = Node Nothing $ children [('f', n7)]
      n7 = Node (Just 2) $ children []
