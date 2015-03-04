{-# LANGUAGE MultiParamTypeClasses, GADTs #-}
module Data.IEnumerable where

import Interface.Class

data IEnumerator t = IEnumerator
                   { current :: t
                   , moveNext :: Maybe (IEnumerator t)
                   , reset :: IEnumerator t
                   } deriving Show

data IEnumerable t = IEnumerable
                   { getEnumerator :: IEnumerator t
                   } deriving Show

instance Functor IEnumerator where
    fmap f ie = IEnumerator
              { current = f . current $ ie
              , moveNext = fmap (fmap f) . moveNext $ ie
              , reset = fmap f . reset $ ie
              }

instance Functor IEnumerable where
    fmap f ie = IEnumerable { getEnumerator = fmap f . getEnumerator $ ie }

data ListEnumerator t = ListEnumerator
                      { leCurrent :: [t]
                      , leStart :: [t]
                      } deriving Show

leMoveNext :: ListEnumerator t -> Maybe (ListEnumerator t)
leMoveNext le = case leCurrent le of
    (_ : x : xs) -> Just $ le { leCurrent = x : xs }
    _            -> Nothing

leReset :: ListEnumerator t -> ListEnumerator t
leReset le = le { leCurrent = leStart le }

instance a ~ b => Interface (ListEnumerator a) (IEnumerator b) where
    i le = IEnumerator
         { current = head . leCurrent $ le
         , moveNext = fmap i . leMoveNext $ le
         , reset = i . leReset $ le
         }

instance a ~ b => Interface [a] (IEnumerable b) where
    i l = IEnumerable . i $ ListEnumerator l l

toList :: IEnumerable t -> [t]
toList ie = toList' . Just . getEnumerator $ ie
    where toList' (Just ie') = current ie' : toList' (moveNext ie')
          toList' Nothing    = []

asList :: ([a] -> [b]) -> IEnumerable a -> IEnumerable b
asList f = i . f . toList

aggregate :: a -> (a -> t -> a) -> (a -> b) -> IEnumerable t -> b
aggregate i acc t ie = t . foldl acc i . toList $ ie

select :: (t -> Int -> b) -> IEnumerable t -> IEnumerable b
select f = asList $ map (uncurry f) . (`zip` [0..])

all :: (t -> Bool) -> IEnumerable t -> Bool
all p ie = ie --> select (\x _ -> p x)
              --> aggregate True (&&) id

where' :: (t -> Bool) -> IEnumerable t -> IEnumerable t
where' p = asList $ filter p

test = i ["hello", "world", "how", "are", "you"] --> where' (\s -> length s > 3)
                                                 --> select (\s i -> s ++ "!")
                                                 --> toList
