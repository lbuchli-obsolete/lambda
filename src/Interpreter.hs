module Interpreter where

import Language
import Util


interpret :: State -> Result Int
interpret state = Trace "Interpreting..." $ Trace ("Globals: " ++ show globals) $ eval state
  where
    (_, _, _, globals, _) = state

eval :: State -> Result Int
eval state | isFinal state = Success 42
eval state                 = Trace ("STEP " ++ show (statsGetSteps stats))
                           $ Trace ("Stack: " ++ show stack)
                           $ Trace ("<Heap size=" ++ show h_size ++ ">")
                           $ trace_h_map h_map
                           $ step state >>= eval
  where
    (stack, _, (h_size, _, h_map), _, stats) = state
    trace_h_map ((addr, node):xs) next = Trace ("  " ++ show addr ++ ": " ++ show node) (trace_h_map xs next)
    trace_h_map [] next = Trace "</Heap>" next

-- TODO what criteria should end interpretation and how should the output format look like?
isFinal :: State -> Bool
isFinal (_, _, _, _, 100) = True
isFinal _ = False

step :: State -> Result State
step = undefined
