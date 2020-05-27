module Interpreter where

import Language
import Util


interpret :: State -> Result Int
interpret state = Trace "Interpreting..." $ Trace ("Globals: " ++ show globals) $ eval state
  where
    (_, _, _, globals, _) = state

eval :: State -> Result Int
eval state | isFinal state = Success 42
eval state                 = Trace ("### STEP " ++ show (statsGetSteps stats + 1) ++ " ###")
                           $ Trace ("Stack: " ++ show stack)
                           $ Trace ("<Heap size=" ++ show h_size ++ ">")
                           $ trace_h_map h_map
                           $ step state >>= inc_stat_steps >>= eval
  where
    (stack, _, (h_size, _, h_map), _, stats) = state
    trace_h_map ((addr, node):xs) next = Trace ("  " ++ show addr ++ ": " ++ show node) (trace_h_map xs next)
    trace_h_map [] next = Trace "</Heap>" next
    inc_stat_steps (s, d, h, g, st) = Success (s, d, h, g, statsIncSteps st)

isFinal :: State -> Bool
isFinal ([def], _, heap, _, _) = is_def def
  where
    is_def addr = is_rdef $ hLookup heap addr
    is_rdef (Success (NDef _)) = True
    is_rdef (Trace _ inner)    = is_rdef inner
    is_rdef _                  = False
isFinal _ = False

step :: State -> Result State
step state = hLookup heap (head stack) >>= \node -> Trace ("Origin: " ++ showAddr (head stack)) (stepNode node) 
  where
    (stack, _, heap, _, _) = state
    stepNode (NAp a b)         = Trace ("Target: Application " ++ show a ++ " " ++ show b) $ stepAp state a b
    stepNode (NSuperComb body) = Trace ("Target: SuperComb "   ++ show body) $ stepSc state body
    stepNode (NInd addr)       = Trace ("Target: Indirection " ++ show addr) $ stepInd state addr
    stepNode (NDef name)       = Trace ("Target: Definition " ++ name) $ stepDef state name
    
stepAp :: State -> Addr -> Addr -> Result State
stepAp (stack, dump, heap, globals, stats) a _ = Success (a : stack, dump, heap, globals, stats)

stepSc :: State -> Expr -> Result State
stepSc (stack, dump, heap, globals, stats) body =
  instantiateAndUpdate body (head stack) heap globals >>= \new_heap ->
  Success (stack, dump, new_heap, globals, stats)

stepInd :: State -> Addr -> Result State
stepInd ([], _, _, _, _) _ = Error $ putStr "Indirection with empty stack"
stepInd (_:stack, dump, heap, globals, stats) addr = Success (addr:stack, dump, heap, globals, stats)

stepDef :: State -> String -> Result State
stepDef (_:ap:stack, dump, heap, globals, stats) _ =
  hLookup heap ap >>= \(NAp _ b) -> Success (b:stack, dump, heap, globals, stats)
stepDef _ _ = Error $ putStr "No further def reduce"

instantiateAndUpdate
  :: Expr         -- supercombinator body
  -> Addr         -- address to update
  -> SHeap        -- heap before instantiation
  -> SGlobals     -- environment
  -> Result SHeap -- heap after instantiation

instantiateAndUpdate (EAp (EDef name body) b) upd_addr heap env =
  instantiate (EDef name body) heap env >>= \(heapA, addrA) ->
  instantiate b heapA ((name, addrA) : env) >>= \(heapB, addrB) ->
  Success $ hUpdate heapB upd_addr (NAp addrA addrB)
  
instantiateAndUpdate (EAp a b) upd_addr heap env =
  instantiate a heap  env >>= \(heapA, addrA) ->
  instantiate b heapA env >>= \(heapB, addrB) ->
  Success $ hUpdate heapB upd_addr (NAp addrA addrB)

instantiateAndUpdate (EVar name) upd_addr heap env =
  mLookup env name >>= \var ->
  Success $ hUpdate heap upd_addr (NInd var)
  
instantiateAndUpdate (EDef name body) upd_addr heap env =
  instantiate (EDef name body) heap env >>= \(new_heap, addr) ->
  Success $ hUpdate new_heap upd_addr (NInd addr)

instantiate
  :: Expr                 -- supercombinator body
  -> SHeap                -- heap before instantiation
  -> SGlobals             -- environment
  -> Result (SHeap, Addr) -- heap and instantiation root address

instantiate (EAp (EDef name body) b) heap env =
  instantiate (EDef name body) heap env >>= \(heapA, addrA) ->
  instantiate b heapA ((name, addrA) : env) >>= \(heapB, addrB) ->
  Success $ hAlloc heapB (NAp addrA addrB)
  
instantiate (EAp a b) heap env =
  instantiate a heap  env >>= \(heapA, addrA) ->
  instantiate b heapA env >>= \(heapB, addrB) ->
  Success $ hAlloc heapB (NAp addrA addrB)
  
instantiate (EVar name) heap env =
  mLookup env name >>= \v -> Success (heap, v)

instantiate (EDef name body) heap env = instantiate body new_heap ((name, addr) : env)
  where
    (new_heap, addr) = hAlloc heap (NDef name)
