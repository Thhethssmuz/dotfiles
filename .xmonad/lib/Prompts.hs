module Prompts (prompt, calc, bash, pass) where

import Control.Monad
import Data.Char (isSpace)
import Data.List
import Data.IORef
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Map as M

import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.Environment
import System.FilePath.Posix

import XMonad
import XMonad.Prompt
import XMonad.Core
import XMonad.Util.Run

-------------------------------------------------------------------------------
-- Calc
-------------------------------------------------------------------------------

data Calc = Calc State

instance XPrompt Calc where
  showXPrompt        (Calc state) = "calc> "
  commandToComplete  (Calc state) = id
  completionFunction (Calc state) = calcCompletion state
  modeAction (Calc state) _ result =
    spawn $ "echo -n '" ++ result ++ "' | xclip -selection c"

calcCompletion state ""   = do
  io . writeIORef (tabCompletionFunction state) $ calcTabCompletion
  io . writeIORef (currentCompletions state) $ []
  return []
calcCompletion state line = do
  completions <- fmap ((:[]) . trim) $ runProcessWithInput "calc" [line] ""
  io . writeIORef (tabCompletionFunction state) $ calcTabCompletion
  io . writeIORef (currentCompletions state) $ completions
  return $ completions ++ [""] -- somehow does not work without the dummy element

calcTabCompletion :: [String] -> XP ()
calcTabCompletion xs = case xs of
  []  -> return ()
  [x] -> setInput x >> endOfLine
  x:xs -> setInput x >> endOfLine

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-------------------------------------------------------------------------------
-- Bash
-------------------------------------------------------------------------------

data Bash = Bash State

instance XPrompt Bash where
  showXPrompt        (Bash state) = "Run: "
  commandToComplete  (Bash state) = id
  completionFunction (Bash state) = bashCompletion state
  modeAction (Bash state) query _ = spawn query

exec :: String -> IO [String]
exec script = fmap lines . runProcessWithInput "bash" ["-c", script] $ ""

completeSingle :: String -> IO [String]
completeSingle "" = return []
completeSingle x  = if '/' `elem` x then exec path else exec cmd
  where cmd  = "compgen -A command \"" ++ unescape x ++ "\" | sort -u"
        path = "compgen -A file \"" ++ unescape x ++ "\" | " ++
               "while IFS=$'\n' read -r line; do " ++
               "printf \"%q\" \"$line\"; " ++
               "[ -d \"$line\" ] && echo \"/\" || echo; " ++
               "done | sort -u"

completeMultiple :: String -> [String] -> IO [String]
completeMultiple x xs = do
  e <- doesFileExist $ "/usr/share/bash-completion/completions/" ++ x
  if e
    then exec script
    else completeSingle . last $ xs
  where line   = intercalate " " (x:xs)
        script = intercalate ";"
               [ "source \"/usr/share/bash-completion/bash_completion\""
               , "source \"/usr/share/bash-completion/completions/" ++ x ++ "\""
               , "a=($(complete -p " ++ x ++ "))"
               , "_completion_loader \"${a[-1]}\""
               , "COMP_WORDS=(" ++ line ++ ")"
               , "COMP_LINE='" ++ line ++ "'"
               , "COMP_POINT=" ++ show (length line)
               , "COMP_CWORD=" ++ show (length xs)
               , "${a[-2]} 2>/dev/null"
               , "(IFS=$'\\n'; echo \"${COMPREPLY[*]}\") | sort -u"
               ]

bashCompletion state ""   = do
  io . writeIORef (tabCompletionFunction state) $ bashTabCompletion
  io . writeIORef (currentCompletions state) $ []
  return []
bashCompletion state line = do
  completions <- case words' line of
    []     -> return []
    [x]    -> completeSingle x
    (x:xs) -> completeMultiple x xs
  io . writeIORef (tabCompletionFunction state) $ bashTabCompletion
  io . writeIORef (currentCompletions state) $ completions
  return completions

unescape :: String -> String
unescape []          = []
unescape ('\\':x:xs) = x : unescape xs
unescape (x:xs)      = x : unescape xs

-- escape aware variant of words
words' :: String -> [String]
words' = foldr f [""]
  where f x (y:ys) = case (x,y,ys) of
                       ('\\', "", y:ys) -> ("\\ "++y):ys
                       (' ',  _ , _   ) -> []:y:ys
                       _                -> (x:y):ys

bashSetInput :: String -> XP ()
bashSetInput x = do
  ys <- fmap words' getInput
  if length ys <= 1
    then setInput x
    else setInput (intercalate " " $ init ys ++ [x])
  endOfLine

bashTabCompletion :: [String] -> XP ()
bashTabCompletion xs = case xs of
  []  -> return ()
  [x] -> bashSetInput x
  xs  -> bashSetInput . foldl1 (\x y -> map fst . takeWhile (\(x,y) -> x == y) $ zip x y) $ xs

-------------------------------------------------------------------------------
-- Pass
-------------------------------------------------------------------------------

data Pass = Pass State

instance XPrompt Pass where
  showXPrompt        (Pass state) = "Pass: "
  commandToComplete  (Pass state) = id
  completionFunction (Pass state) = passCompletion state
  modeAction (Pass state) query _ = spawn $ "pass show -c " ++ query
    --let args  | query `isInfixOf` result = "show -c " ++ result
    --          | otherwise "generate -c " ++ query ++ " 32"
    --in  spawn $ "pass " ++ args

passCompletion state ""   = do
  io . writeIORef (tabCompletionFunction state) $ passTabCompletion
  io . writeIORef (currentCompletions state) $ []
  return []
passCompletion state line = do
  completions <- fmap (filter (isInfixOf line)) $ getPasswords
  io . writeIORef (tabCompletionFunction state) $ passTabCompletion
  io . writeIORef (currentCompletions state) $ completions
  return completions

getPasswordDir :: IO FilePath
getPasswordDir = do
  envDir <- lookupEnv "PASSWORD_STORE_DIR"
  home   <- getEnv "HOME"
  return $ fromMaybe (home </> ".password-store") envDir

getFiles :: FilePath -> IO [String]
getFiles dir = do
  names <- getDirectoryContents dir
  let properNames = filter ((/=) "." . take 1) names
  paths <- forM properNames $ \name -> do
    let path = dir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getFiles path
      else return [path]
  return (concat paths)

getPasswords :: IO [String]
getPasswords = do
  password_dir <- getPasswordDir
  files        <- getFiles password_dir
  return $ map ((makeRelative password_dir) . dropExtension) files

passTabCompletion :: [String] -> XP ()
passTabCompletion xs = case xs of
  []  -> return ()
  [x] -> setInput x >> endOfLine
  xs  -> do
         input <- getInput
         setInput . foldl1 (\x y -> map fst . takeWhile (\(x,y) -> x == y) $ zip x y)
                  . map (head . dropWhile (not . isPrefixOf input) . tails)
                  $ xs
         endOfLine

-------------------------------------------------------------------------------
-- Tab completion
-------------------------------------------------------------------------------

data State = State
  { currentCompletions    :: IORef [String]
  , tabCompletionFunction :: IORef ([String] -> XP ())
  }

initState :: IO State
initState = do
  xs <- io $ newIORef []
  f  <- io $ newIORef (\_ -> return ())
  return $ State { currentCompletions = xs, tabCompletionFunction = f }

tabComplete :: State -> XP ()
tabComplete state = do
  xs <- io . readIORef $ currentCompletions state
  f  <- io . readIORef $ tabCompletionFunction state
  f xs

extConf conf state history = conf
  { historyFilter       = deleteAllDuplicates
  , completionKey       = xK_F23
  , showCompletionOnTab = True
  , promptKeymap        = M.fromList
    [ ((0,                      xK_Tab      ), tabComplete state)
    , ((0,                      xK_Up       ), historyUpMatching history)
    , ((0,                      xK_Down     ), historyDownMatching history)
    ] <+> promptKeymap conf
  }

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

calc = XPT . Calc
bash = XPT . Bash
pass = XPT . Pass

prompt :: XPConfig -> [State -> XPType] -> X ()
prompt conf xs = do
  s <- io initState
  mkXPromptWithModes (map (\x -> x s) xs) . (extConf conf s) =<< initMatches
