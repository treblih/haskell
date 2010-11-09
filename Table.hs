module Table (
  Table,
  new,
  find,
  update
) where

new :: Eq key => [(key, val)] -> Table key val
find :: Eq key => key -> Table key val -> val
update :: Eq key => (key, val) -> Table key val -> Table key val

{- function implementation
newtype Table key val = Table (key -> val)

instance Show (Table key val) where
  showsPrec _ _ str = showString "<<A Table>>" str

new assoc = foldr update (Table (\_ -> error "item not found in table")) assoc

find i (Table f) = f i

update (key, val) (Table f) = Table g
  where g key'
          | key == key' = val
          | otherwise = f key'
end of function implementatioin -}


{- list implementation
newtype Table key val = Table [(key, val)] deriving Show

new t = Table t

find _ (Table []) = error "not in table"
find key' (Table ((key, val) : xs)) 
  | key' == key = val
  | otherwise = find key' (Table xs)

update a (Table t) = Table (update' a t [])
  where update' a [] acc = acc ++ [a]
        update' p@(key, val) (q@(x, _) : xs) acc
	  | key == x = acc ++ [p] ++ xs
	  | otherwise = update' p xs (acc ++ [q])
end of list implementatioin -}

import Array

newtype Table key val = Table (Array key val) deriving Show

new :: Ix key => [(key, val)] -> Table key val
find :: Ix key => key -> Table key val -> val
update :: Ix key => (key, val) -> Table key val -> Table key val

new t = Table (array (min, max) t)
  where
  min = minimum indices
  max = maximum indices
  indices = map fst t

find key (Table t) = t ! key

update q@(key, val) (Table t) = Table (t // [q])



import BinTree

newtype Table key val = Table (BinTree (key, val)) deriving Show

new [] = Table empty
new ((i, v) : xs) = Table (insert x )
