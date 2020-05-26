module Language where

import Util

------------------------------------------------------------------
--                              AST                             --
------------------------------------------------------------------

newtype Prog = Prog [Def]
  deriving Show
newtype Def = Def (Symbol, Expr)
  deriving Show
data Expr = EDef Symbol Expr
          | EAp Expr Expr
          | EVar Symbol
  deriving Show
type Symbol = String


-----------------------------------------------------------------
--                  Interpreter state / IR                     --
-----------------------------------------------------------------

data Node = NAp Addr Addr
          | NSuperComb Symbol Expr
          | NInd Addr
  deriving Show

type State = (SStack, SDump, SHeap, SGlobals, SStats)
type SStack = [Addr]
type SDump  = () -- Not used yet
type SHeap  = Heap Node
type SGlobals = Map Symbol Addr
type SStats = Int

statsInitial :: SStats
statsInitial = 0
statsIncSteps :: SStats -> SStats
statsIncSteps = (+1)
statsGetSteps :: SStats -> Int
statsGetSteps = id

