module Compiler where

import Language
import Util

compile :: Prog -> Result State
compile prog = Trace "Compiling..." $  Success (initial_stack, initial_dump, initial_heap, globals, statsInitial)
  where
    (initial_heap, globals) = buildInitialHeap prog
    initial_stack = [mLookup globals "main"]
    initial_dump = ()

buildInitialHeap :: Prog -> (SHeap, SGlobals)
buildInitialHeap (Prog scs) = foldl allocateSc (hInitial, mEmpty) scs

allocateSc :: (SHeap, SGlobals) -> Def -> (SHeap, SGlobals)
allocateSc (heap, globals) (Def (name, body)) = (heap', (name, addr) : globals)
  where
    (heap', addr) = hAlloc heap (NSuperComb name body)
