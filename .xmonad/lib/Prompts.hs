module Prompts (prompt, prompt', calc, bash, pass, defi, enno, noen) where

import Control.Monad
import Data.Char (isSpace)
import Data.List
import Data.IORef
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Map as M

import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents, getHomeDirectory)
import System.Environment
import System.FilePath.Posix
import System.IO (appendFile)

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
  completionFunction (Calc state) = mkCompelFunc state calcTabCompletion calcCompletion
  modeAction (Calc state) _ result =
    spawn $ "echo -n '" ++ result ++ "' | xclip -selection c"

calcCompletion line = fmap ((:[""]) . trim) $ runProcessWithInput "calc" [line] ""

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

inContext :: String -> String
inContext script = prefix ++ script
  where prefix  = concatMap (\x -> "source " ++ x ++ ";") sources
        sources = ["~/.aliases", "~/.functions"]

instance XPrompt Bash where
  showXPrompt        (Bash state) = "Run: "
  commandToComplete  (Bash state) = id
  completionFunction (Bash state) = mkCompelFunc state bashTabCompletion bashCompletion
  modeAction (Bash state) query _ = spawn $ inContext query

exec :: String -> IO [String]
exec script = fmap lines . runProcessWithInput "bash" ["-c", inContext script] $ ""

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

bashCompletion line = case words' line of
  []     -> return []
  [x]    -> completeSingle x
  (x:xs) -> completeMultiple x xs

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
  completionFunction (Pass state) = mkCompelFunc state passTabCompletion passCompletion
  modeAction (Pass state) query _ = spawn $ "pass show -c " ++ query
    --let args  | query `isInfixOf` result = "show -c " ++ result
    --          | otherwise "generate -c " ++ query ++ " 32"
    --in  spawn $ "pass " ++ args

passCompletion line = fmap (filter (isInfixOf line)) getPasswords

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
-- Defi
-------------------------------------------------------------------------------

data Defi = Defi State

instance XPrompt Defi where
  showXPrompt        (Defi state) = "Def: "
  commandToComplete  (Defi state) = id
  completionFunction (Defi state) = mkCompelFunc' state (\_ -> return ()) defiReturnComplete (defiCompletion "en_GB")
  modeAction (Defi state) query _ = return ()

defiCompletion :: String -> String -> IO [String]
defiCompletion _    ""   = return []
defiCompletion lang line = do
  let w = last . words $ line
  xs <- fmap words
      . runProcessWithInput "bash" ["-c", "hunspell -d " ++ lang ++ " | egrep '&|#' | sed 's/#.*$/-/g' | sed 's/&.*: //g' | sed 's/, /\\n/g'"]
      $ w
  return $ case xs of
    []    -> [w]
    ["-"] -> []
    xs    -> xs

defiReturnComplete :: String -> IO [String]
defiReturnComplete line = do
  h <- getHomeDirectory
  d <- runProcessWithInput "node" [h++"/.xmonad/lib/lang.js", "78", "dictionaryapi", line] $ ""
  return . f . lines $ d
  where f xs | length xs == 0 = ["no definitions found"]
             | otherwise      = xs ++ replicate 10 ""

-------------------------------------------------------------------------------
-- Trans
-------------------------------------------------------------------------------

data Enno = Enno State

instance XPrompt Enno where
  showXPrompt        (Enno state) = "en->no: "
  commandToComplete  (Enno state) = id
  completionFunction (Enno state) = mkCompelFunc' state (\_ -> return ()) (transReturnComplete "eng" "nob") (defiCompletion "en_GB")
  modeAction (Enno state) query _ = return ()

data Noen = Noen State

instance XPrompt Noen where
  showXPrompt        (Noen state) = "no->en: "
  commandToComplete  (Noen state) = id
  completionFunction (Noen state) = mkCompelFunc' state (\_ -> return ()) (transReturnComplete "nob" "eng") (defiCompletion "nb_NO")
  modeAction (Noen state) query _ = return ()

transReturnComplete :: String -> String -> String -> IO [String]
transReturnComplete from to line = do
  h <- getHomeDirectory
  d <- runProcessWithInput "node" [h++"/.xmonad/lib/lang.js", "78", "glosbe", from, to, line] $ ""
  return . f . lines $ d
  where f xs | length xs == 0 = ["no translation found"]
             | otherwise      = xs ++ replicate 10 ""

-------------------------------------------------------------------------------
-- Tab completion
-------------------------------------------------------------------------------

data State = State
  { currentCompletions    :: IORef [String]
  , tabCompletionFunction :: IORef ([String] -> XP ())
  , getTabCompletions     :: IORef (String -> IO [String])
  , returnComplete        :: IORef (String -> IO [String])
  }

initState :: IO State
initState = do
  xs <- newIORef []
  ot <- newIORef (\_ -> return ())
  gc <- newIORef (\_ -> return [])
  rc <- newIORef (\_ -> return [])
  return $ State
         { currentCompletions    = xs
         , tabCompletionFunction = ot
         , getTabCompletions     = gc
         , returnComplete        = rc
         }

mkCompelFunc :: State -> ([String] -> XP ()) -> (String -> IO [String]) -> String -> IO [String]
mkCompelFunc state onTab getCompletions line = do
  xs <- if null line then return [] else getCompletions line
  writeIORef (tabCompletionFunction state) onTab
  writeIORef (currentCompletions state) xs
  return xs

tabComplete :: State -> XP ()
tabComplete state = do
  xs <- io . readIORef $ currentCompletions state
  f  <- io . readIORef $ tabCompletionFunction state
  f xs

mkCompelFunc' :: State -> ([String] -> XP ()) -> (String -> IO [String]) -> (String -> IO [String]) -> String -> IO [String]
mkCompelFunc' state onTab onReturn getCompletions line = do
  xs <- readIORef (currentCompletions state)
  writeIORef (tabCompletionFunction state) onTab
  writeIORef (returnComplete state) onReturn
  writeIORef (getTabCompletions state) getCompletions
  return xs

tabComplete' :: State -> XP ()
tabComplete' state = do
  gc <- io . readIORef $ getTabCompletions state
  xs <- io . gc =<< getInput
  io . writeIORef (currentCompletions state) $ xs
  ot <- io . readIORef $ tabCompletionFunction state
  ot xs

returnComplete' :: State -> XP ()
returnComplete' state = do
  rc <- io . readIORef $ returnComplete state
  xs <- io . rc =<< getInput
  io . writeIORef (currentCompletions state) $ xs

extConf conf state history = conf
  { historyFilter       = deleteAllDuplicates
  , completionKey       = xK_F23
  , promptKeymap        = M.fromList
    [ ((0,                      xK_Tab      ), tabComplete state)
    , ((0,                      xK_Up       ), historyUpMatching history)
    , ((0,                      xK_Down     ), historyDownMatching history)
    ] <+> promptKeymap conf
  }

extConf' conf state history = conf
  { historyFilter       = deleteAllDuplicates
  , completionKey       = xK_F23
  , promptKeymap        = M.fromList
    [ ((0,                      xK_Tab      ), tabComplete' state)
    , ((0,                      xK_Up       ), historyUpMatching history)
    , ((0,                      xK_Down     ), historyDownMatching history)
    , ((0,                      xK_Return   ), returnComplete' state)
    ] <+> promptKeymap conf
  }

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

calc = XPT . Calc
bash = XPT . Bash
pass = XPT . Pass
defi = XPT . Defi
enno = XPT . Enno
noen = XPT . Noen

prompt :: XPConfig -> [State -> XPType] -> X ()
prompt conf xs = do
  s <- io initState
  mkXPromptWithModes (map (\x -> x s) xs) . (extConf conf s) =<< initMatches

prompt' :: XPConfig -> [State -> XPType] -> X ()
prompt' conf xs = do
  s <- io initState
  mkXPromptWithModes (map (\x -> x s) xs) . (extConf' conf s) =<< initMatches
