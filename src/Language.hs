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
type Symbol = String

instance Show Expr where
  show (EDef name body) = "λ" ++ name ++ "." ++ show body
  show (EAp a b) = "(" ++ show a ++ " " ++ show b ++ ")"
  show (EVar name) = name

-----------------------------------------------------------------
--                  Interpreter state / IR                     --
-----------------------------------------------------------------

data Node = NAp Addr Addr
          | NSuperComb Expr
          | NInd Addr
          | NDef Symbol Expr

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

showAddr :: Addr -> String
showAddr addr = "#" ++ show addr

instance Show Node where
  show (NAp a b) = "(" ++ showAddr a ++ " " ++ showAddr b ++ ")"
  show (NSuperComb body) = "SC " ++ show body
  show (NInd addr) = showAddr addr
  show (NDef name body) = "Definition " ++ name ++ " => " ++ show body
