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

-- For keyboard parsing
import System.IO (stdin, hSetEcho, hSetBuffering, BufferMode( NoBuffering ), hReady, hFlush)
import UI.NCurses -- cabal install --lib ncurses

-- an "environment" is a list of variable names paired with their definitions as lambda-expressions
type Env = [(Var, LExp)]

-- undefinedVar determines whether an expression has any free variable that is not defined by the environment
undefinedVar :: Env -> LExp -> Maybe Var
undefinedVar env t = find (\y -> isNothing (lookup y env)) (free t)

replPrettyCurses :: IO ()
replPrettyCurses = runCurses $ do 
  w <- defaultWindow
  go [] w 
  where 
    go :: Env -> Window -> Curses ()
    go env w = do
      dirtyLine <- prettyGetLine w    -- get a line of input
      let line = removeInitialWhiteSpaces dirtyLine;

      cursorNewLine w

      let cmdPreprocess = (runParser parseCmd) line        -- parse the input as a command
      case cmdPreprocess of
        Nothing -> prettyPrint w ("Parse Error") >> go env w
        otherwise -> let cmd = fst (fromJust cmdPreprocess) in 
          case cmd of               
              Eval t ->              -- execute an eval command
                -- the expression to be evaluated cannot have any free
                -- variables that are not defined in the environment
                case undefinedVar env t of
                  Just y -> prettyPrint w ("Variable not in scope: " ++ y) >> go env w
                  Nothing -> do
                    -- substitute for all variables defined in the environment,
                    -- in order from left to right
                    let t' = foldl (\t (x,u) -> subst (u,x) t) t env

                    -- FOR DEBUGGING --
                    -- printLExp t'

                    -- normalize the resulting term
                    let u = normalize t'
                    -- print the result
                    prettyPrint w (prettyLExp u)

                    -- continue the REPL
                    go env w
                    
              Let x t ->             -- execute a let command
                case undefinedVar env t of
                  Just y -> prettyPrint w ("Variable not in scope: " ++ y) >> go env w
                  Nothing -> do
                    -- continue the REPL in an environment extended with x=t
                    go ((x,t):env) w
                  
              Noop -> go env w        -- execute a no-op command, by doing nothing and continuing the REPL

              Quit -> do             -- execute a quit command, by terminating the REPL
                prettyPrint w "Goodbye."
                return ()

data WindowState = WindowState { 
  lineSize :: Integer,
  textLeft :: String,
  textRight :: String,
  cursorX :: Integer,
  cursorY :: Integer,
  done :: Bool
  } deriving (Show)

-- prettyGetLine() get an input from the command line
prettyGetLine :: Window -> Curses (String)
prettyGetLine w = do
  -- Init new line
  stateW <- defaultStateInput w
  renderInputLine w stateW
  loop w stateW 
    where
      loop :: Window -> WindowState -> Curses String
      loop w stateW = do
        ev <- getEvent w Nothing
        stateW <- eventHandler stateW ev
        renderInputLine w stateW
        if (done stateW) 
          then return ((reverse $ textLeft stateW) ++ (textRight stateW))
          else loop w stateW

eventHandler :: WindowState -> Maybe Event -> Curses (WindowState)
eventHandler stateW ev = do
  case ev of 
    Just (EventCharacter '\n') -> do
      return (handleEnter stateW)
    Just (EventCharacter k) -> do
      return (handleCharacter stateW k)
    Just (EventSpecialKey KeyLeftArrow) -> do
      return (handleLeftArrow stateW)
    Just (EventSpecialKey KeyRightArrow) -> do
      return (handleRightArrow stateW)
    Just (EventSpecialKey KeyBackspace) -> do
      return (handleBackspace stateW)
    otherwise -> return stateW

----- HANDLERS -----
--------------------

handleCharacter :: WindowState -> Char -> WindowState
handleCharacter stateW k = 
  WindowState {
    lineSize = (lineSize stateW) + 1,
    textLeft = (k:(textLeft stateW)),
    textRight = textRight stateW,
    cursorX = cursorX stateW,
    cursorY = (cursorY stateW) + 1,
    done = done stateW
  }

handleLeftArrow :: WindowState -> WindowState
handleLeftArrow stateW = 
  if null (textLeft stateW) 
    then stateW 
    else 
      WindowState {
        lineSize = lineSize stateW,
        textLeft = (tail (textLeft stateW)),
        textRight = ((head (textLeft stateW)) : (textRight stateW)),
        cursorX = cursorX stateW,
        cursorY = (cursorY stateW) - 1,
        done = done stateW
      }

handleRightArrow :: WindowState -> WindowState
handleRightArrow stateW = 
  if null (textRight stateW) 
    then stateW 
    else 
      WindowState {
        lineSize = lineSize stateW,
        textLeft = ((head (textRight stateW)) : (textLeft stateW)),
        textRight = (tail (textRight stateW)),
        cursorX = cursorX stateW,
        cursorY = (cursorY stateW) + 1,
        done = done stateW
      }

handleBackspace :: WindowState -> WindowState
handleBackspace stateW = 
  if null (textLeft stateW) 
    then stateW 
    else 
      WindowState {
        lineSize = (lineSize stateW) - 1,
        textLeft = (tail (textLeft stateW)),
        textRight = textRight stateW,
        cursorX = cursorX stateW,
        cursorY = (cursorY stateW) - 1,
        done = done stateW
      }

handleEnter :: WindowState -> WindowState
handleEnter stateW = 
  WindowState {
    lineSize = lineSize stateW,
    textLeft = textLeft stateW,
    textRight = textRight stateW,
    cursorX = cursorX stateW,
    cursorY = cursorY stateW,
    done = True
  }

handleString :: WindowState -> [Char] -> WindowState
handleString stateW [] = stateW
handleString stateW (c:cs) = handleString (handleCharacter stateW c) cs

updateInputLine :: WindowState -> Update ()
updateInputLine stateW = do 
  moveCursor (cursorX stateW) 0
  drawString "> "
  drawString $ take (2 + (fromIntegral $ lineSize stateW)) $ repeat ' '
  moveCursor (cursorX stateW) 2
  drawString (reverse (textLeft stateW))
  drawString (textRight stateW)
  moveCursor (cursorX stateW) (cursorY stateW)

updateOutputLine :: WindowState -> Update ()
updateOutputLine stateW = do
  moveCursor (cursorX stateW) 0
  drawString $ take (fromIntegral $ lineSize stateW) $ repeat ' '
  moveCursor (cursorX stateW) 0
  drawString (reverse (textLeft stateW))
  drawString (textRight stateW)
  moveCursor (cursorX stateW) (cursorY stateW)

defaultStateInput :: Window -> Curses (WindowState)
defaultStateInput w = do 
  (cX, _) <- getCursor w
  return WindowState {lineSize = 0, textLeft = "", textRight = "", cursorX = cX, cursorY = 2, done = False};

defaultStateOutput :: Window -> Curses (WindowState)
defaultStateOutput w = do
  (cX, _) <- getCursor w
  return WindowState {lineSize = 0, textLeft = "", textRight = "", cursorX = cX, cursorY = 0, done = False};

renderInputLine :: Window -> WindowState -> Curses ()
renderInputLine w stateW = do
  updateWindow w $ do
    updateInputLine stateW
  render

renderOutputLine :: Window -> WindowState -> Curses ()
renderOutputLine w stateW = do
  updateWindow w $ do
    updateOutputLine stateW
  render

cursorNewLine :: Window -> Curses ()
cursorNewLine w = do
  (cX, _) <- getCursor w
  updateWindow w $ do
    moveCursor (cX+1) 0
  render

prettyPrint :: Window -> String -> Curses ()
prettyPrint w s = do
  stateW <- defaultStateOutput w
  let newStateW = handleString stateW s;
  renderOutputLine w newStateW
  cursorNewLine w

removeInitialWhiteSpaces :: String -> String
removeInitialWhiteSpaces (' ': cs) = removeInitialWhiteSpaces cs
removeInitialWhiteSpaces s = s

----- MAIN -----
----------------

main :: IO ()
main = replPrettyCurses

