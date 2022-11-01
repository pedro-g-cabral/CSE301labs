import Cmd

-- Pretty Printing
import Parser
import PrettyExpr

import Data.List
import Data.Maybe
import Eval
import Expr
import Subst
import System.IO

-- an "environment" is a list of variable names paired with their definitions as lambda-expressions
type Env = [(Var, LExp)]

-- undefinedVar determines whether an expression has any free variable that is not defined by the environment
undefinedVar :: Env -> LExp -> Maybe Var
undefinedVar env t = find (\y -> isNothing (lookup y env)) (free t)

-- the top-level read-eval-print-loop
repl :: IO ()
repl = go [] -- start the interpreter in an empty environment
  where
    go :: Env -> IO ()
    go env = do
      putStr "> " -- print the prompt
      hFlush stdout -- flush standard output
      line <- getLine -- get a line of input
      let cmd = read line -- parse the input as a command
      case cmd of
        Eval t ->
          -- execute an eval command
          -- the expression to be evaluated cannot have any free
          -- variables that are not defined in the environment
          case undefinedVar env t of
            Just y -> putStrLn ("Variable not in scope: " ++ y) >> go env
            Nothing -> do
              -- substitute for all variables defined in the environment,
              -- in order from left to right
              let t' = foldl (\t (x, u) -> subst (u, x) t) t env
              -- normalize the resulting term
              let u = normalize t'
              -- print the result
              print u
              -- continue the REPL
              go env
        Let x t ->
          -- execute a let command
          case undefinedVar env t of
            Just y -> putStrLn ("Variable not in scope: " ++ y) >> go env
            Nothing -> do
              -- continue the REPL in an environment extended with x=t
              go ((x, t) : env)
        Noop -> go env -- execute a no-op command, by doing nothing and continuing the REPL
        Quit -> do
          -- execute a quit command, by terminating the REPL
          putStrLn "Goodbye."
          return ()


-- repl with pretty syntax
replPretty :: IO ()
replPretty = go []                     -- start the interpreter in an empty environment
  where
    go :: Env -> IO ()
    go env = do
      putStr "> "                -- print the prompt
      hFlush stdout              -- flush standard output
      line <- getLine            -- get a line of input
      let cmdPreprocess = (runParser parseCmd) line        -- parse the input as a command
      case cmdPreprocess of
        Nothing -> putStrLn("Parse Error") >> go env
        otherwise -> let cmd = fst (fromJust cmdPreprocess) in 
          case cmd of               
              Eval t ->              -- execute an eval command
                -- the expression to be evaluated cannot have any free
                -- variables that are not defined in the environment
                case undefinedVar env t of
                  Just y -> putStrLn ("Variable not in scope: " ++ y) >> go env
                  Nothing -> do
                    -- substitute for all variables defined in the environment,
                    -- in order from left to right
                    let t' = foldl (\t (x,u) -> subst (u,x) t) t env
                    -- normalize the resulting term
                    let u = normalize t'
                    -- print the result
                    printLExp u
                    -- continue the REPL
                    go env
                    
              Let x t ->             -- execute a let command
                case undefinedVar env t of
                  Just y -> putStrLn ("Variable not in scope: " ++ y) >> go env
                  Nothing -> do
                    -- continue the REPL in an environment extended with x=t
                    go ((x,t):env)
                  
              Noop -> go env         -- execute a no-op command, by doing nothing and continuing the REPL

              Quit -> do             -- execute a quit command, by terminating the REPL
                putStrLn "Goodbye."
                return ()


main = replPretty

-- Helper functions (TODO: Add a helper.hs)
fromJust :: Maybe a -> a
fromJust (Just a) = a
