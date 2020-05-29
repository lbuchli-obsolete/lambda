{-# LANGUAGE LambdaCase #-}
module Interpreter where

import Language
import Util


interpret :: State -> Result String
interpret state = Trace "Interpreting..." $ Trace ("Globals: " ++ show globals) $ eval state
  where
    (_, _, _, globals, _) = state

eval :: State -> Result String
eval state | isFinal state = Trace "### FINAL STATE ###" $ traceState state $ Success ""
eval state = traceState state $ step state >>= inc_stat_steps >>= eval
  where
    inc_stat_steps (s, d, h, g, st) = Success (s, d, h, g, statsIncSteps st)

traceState :: State -> Result a -> Result a
traceState state nextOp
  = Trace ("### STEP " ++ show (statsGetSteps stats + 1) ++ " ###")
  $ Trace ("Stack: " ++ show stack)
  $ Trace ("<Heap size=" ++ show h_size ++ ">")
  $ trace_h_map h_map nextOp
  where
    (stack, _, (h_size, _, h_map), _, stats) = state
    trace_h_map ((addr, node):xs) next = Trace ("  " ++ show addr ++ ": " ++ show node) (trace_h_map xs next)
    trace_h_map [] next = Trace "</Heap>" next
    
isFinal :: State -> Bool
isFinal ([def], _, heap, _, _) = is_def def
  where
    is_def addr = is_rdef $ hLookup heap addr
    is_rdef (Success (NDef _ _)) = True
    is_rdef (Trace _ inner)      = is_rdef inner
    is_rdef _                    = False
isFinal _ = False

step :: State -> Result State
step state = hLookup heap (head stack) >>= \node ->
  Trace ("Origin: " ++ showAddr (head stack)) (stepNode node) 
  where
    (stack, _, heap, _, _) = state
    stepNode (NAp a b)         = Trace ("Target: Application " ++ showAddr a ++ " " ++ showAddr b)
                                 $ stepAp state a b
    stepNode (NSuperComb body) = Trace ("Target: SuperComb "   ++ show body) $ stepSc state body
    stepNode (NInd addr)       = Trace ("Target: Indirection " ++ show addr) $ stepInd state addr
    stepNode (NDef name body)  = Trace ("Target: Definition " ++ name ++ " => " ++ show body)
                                 $ stepDef state name body

stepAp :: State -> Addr -> Addr -> Result State
stepAp state a b =
  hLookup heap a >>= \case
    (NDef name body) -> instantiateAndUpdate body ap heap ((name, b) : globals) >>= \new_heap ->
      Trace "Ap with (sc)-ndef" $ Success (ap:stack, dump, new_heap, globals, stats)
    _                -> Trace "Normal Ap" $ Success (a:ap:stack, dump, heap, globals, stats)
  where
    (ap:stack, dump, heap, globals, stats) = state
  
stepSc :: State -> Expr -> Result State
stepSc (stack, dump, heap, globals, stats) body =
  instantiateAndUpdate body (head stack) heap globals >>= \new_heap ->
  Success (stack, dump, new_heap, globals, stats)

stepInd :: State -> Addr -> Result State
stepInd ([], _, _, _, _) _ = Error $ putStr "Indirection with empty stack"
stepInd (_:stack, dump, heap, globals, stats) addr = Success (addr:stack, dump, heap, globals, stats)

stepDef :: State -> String -> Expr -> Result State
stepDef (_:stack, dump, heap, globals, stats) _ _ = Success (stack, dump, heap, globals, stats)
stepDef _                                     _ _ = Error $ putStr "Definition with empty stack"
{-
stepDef (_:ap:stack, dump, heap, globals, stats) name body =
  hLookup heap ap >>= \(NAp _ b) ->
  instantiate body heap ((name, b) : globals) >>= \(new_heap, addr) ->
  Success (addr:stack, dump, new_heap, globals, stats)
-}
--stepDef _ _ _ = Error $ putStr "[stepDef] This should not be a possible state. Perhaps end state?"

instantiateAndUpdate
  :: Expr         -- supercombinator body
  -> Addr         -- address to update
  -> SHeap        -- heap before instantiation
  -> SGlobals     -- environment
  -> Result SHeap -- heap after instantiation

instantiateAndUpdate (EAp (EDef name body) b) upd_addr heap env =
  instantiate b heap env >>= \(heapB, addrB) ->
  instantiateAndUpdate body upd_addr heapB ((name, addrB) : env)

instantiateAndUpdate (EAp a b) upd_addr heap env =
  instantiate a heap  env >>= \(heapA, addrA) ->
  instantiate b heapA env >>= \(heapB, addrB) ->
  Success $ hUpdate heapB upd_addr (NAp addrA addrB)

instantiateAndUpdate (EVar name) upd_addr heap env =
  mLookup env name >>= \var ->
  Success $ hUpdate heap upd_addr (NInd var)
  
instantiateAndUpdate (EDef name body) upd_addr heap _ =
  Success $ hUpdate heap upd_addr (NDef name body)

instantiate
  :: Expr                 -- supercombinator body
  -> SHeap                -- heap before instantiation
  -> SGlobals             -- environment
  -> Result (SHeap, Addr) -- heap and instantiation root address

instantiate (EAp (EDef name body) b) heap env =
  instantiate b heap env >>= \(heapB, addrB) ->
  instantiate body heapB ((name, addrB) : env)
  
instantiate (EAp a b) heap env =
  instantiate a heap  env >>= \(heapA, addrA) ->
  instantiate b heapA env >>= \(heapB, addrB) ->
  Success $ hAlloc heapB (NAp addrA addrB)
  
instantiate (EVar name) heap env =
  mLookup env name >>= \v -> Success (heap, v)

instantiate (EDef name body) heap _ =
  Success $ hAlloc heap (NDef name body)
