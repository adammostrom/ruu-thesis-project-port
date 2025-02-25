module CustomMap where 



newtype CustomMap k v = CustomMap {getMap :: [(k, v)]} deriving (Show, Eq)

-- Insert a key-value pair into the custom map.
insert :: (Eq k) => k -> v -> CustomMap k v -> CustomMap k v
insert k v (CustomMap xs) = CustomMap ((k, v) : filter ((/= k) . fst) xs)

-- Lookup a value by key in the custom map.
lookupC :: (Eq k) => k -> CustomMap k v -> Maybe v
lookupC k (CustomMap xs) = fmap snd (find ((== k) . fst) xs)

-- Convert a list of key-value pairs to a custom map.
fromList :: (Eq k) => [(k, v)] -> CustomMap k v
fromList = CustomMap

-- Convert a custom map back to a list of key-value pairs.
toList :: CustomMap k v -> [(k, v)]
toList (CustomMap xs) = xs

-- Find a key-value pair in the list.
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x : xs)
  | p x = Just x
  | otherwise = find p xs

-- Custom Map version of `Map.findWithDefault`.
findWithDefault :: (Eq k) => v -> k -> CustomMap k v -> v
findWithDefault def k m = case lookupC k m of
  Nothing -> def
  Just v -> v