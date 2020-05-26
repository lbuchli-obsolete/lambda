module Util where

---------------------------------------------------------
--                         Heap                        --
---------------------------------------------------------

type Heap a = (Int, [Int], Map Int a)
type Addr   = Int
  
hInitial   :: Heap a
hAlloc     :: Heap a -> a -> (Heap a, Addr)
hUpdate    :: Heap a -> Addr -> a -> Heap a
hFree      :: Heap a -> Addr -> Heap a
hLookup    :: Heap a -> Addr -> a
hAddresses :: Heap a -> [Addr]
hSize      :: Heap a -> Int
hNull      :: Addr
hIsNull    :: Addr -> Bool

hInitial = (0, [1..], [])
hAlloc (size, next:free, cts) node = ((size+1, free, (next, node):cts), next)
hAlloc _ _ = error "None of the infinite addresses are left (╯°□°)╯ ┻━┻"
hUpdate (size, free, cts) addr node = (size, free, (addr, node) : filter (\x -> fst x /= addr) cts)
hFree (size, free, cts) addr = (size-1, addr:free, filter (\x -> fst x /= addr) cts)
hLookup (_, _, cts) = mLookup cts
hAddresses (_, _, cts) = [addr | (addr, _) <- cts]
hSize (size, _, _) = size
hNull = 0
hIsNull a = a == 0

-------------------------------------------------------------------
--                              Map                              --
-------------------------------------------------------------------

type Map a b = [(a, b)]

mLookup :: Eq a => Show a => Map a b -> a -> b
mLookup ((k, v):bs) k' | k == k' = v
                       | k /= k' = mLookup bs k'
mLookup m k'                     = error ("Can't find key " ++ show k' ++ " in map " ++ show (mDomain m))

mDomain :: Map a b -> [a]
mDomain mMap = [key | (key, _) <- mMap]

mRange :: Map a b -> [b]
mRange mMap = [val | (_, val) <- mMap]

mEmpty :: Map a b
mEmpty = []

  
----------------------------------------------------------------------
--                              Result                              --
----------------------------------------------------------------------

data Result a = Success a | Error (IO ())

instance Functor Result where
  fmap f (Success a) = Success (f a)
  fmap _ (Error msg) = Error msg

instance Applicative Result where
  pure x = Success x
  (<*>) (Success f) (Success a) = Success (f a)
  (<*>) (Error msg) _           = Error msg
  (<*>) _           (Error msg) = Error msg

instance Monad Result where
  (>>=) (Success a) f = f a
  (>>=) (Error msg) _ = Error msg

-- TODO very ugly. Is there a way around 'IO ()' error messages that doesn't
-- look ugly?
printResult :: Show a => Result a -> IO ()
printResult (Error msg) = do
  putStr "ERROR: "
  msg
printResult (Success s) = print s
