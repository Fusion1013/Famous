import Prelude hiding (lookup)

data Table key q p = Empty
                   | Node (Table key q p) (q) (Table key q p)
                   | Person (key, p)
                   --Invariant: Yes-Left, No-Right
                   deriving Show

empty :: Table key q p
empty = Empty

lookup _ Empty = Nothing
lookup (x:xs) (Node tl q tr) | x == 0          = lookup xs tl
                             | otherwise       = lookup xs tr
lookup key (Person (key', p))                  = Just p

insert key q p Empty                          = Person (key, p)
insert (x:xs) q p (Node tl q' tr) | x == 0    = Node (insert xs q p tl) q' tr
                                  | otherwise = Node tl q' (insert xs q p tr)
insert key q p (Person (key', p'))            = Node (Person (key'++[0], p)) (q) (Person (key'++[1], p'))

toList :: Table key q p -> []