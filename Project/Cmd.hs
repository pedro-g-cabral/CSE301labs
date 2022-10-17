module Cmd where

import Expr

data Cmd = Eval LExp | Let Var LExp | Noop | Quit
  deriving (Show,Read)
