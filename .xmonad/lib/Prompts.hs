module Prompts (prompt, calc, bash, pass) where

import Control.Monad
import Data.Char (isSpace)
import Data.List
import Data.Maybe (fromMaybe, catMaybes)

import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.Environment
import System.FilePath.Posix

import XMonad.Core
import XMonad.Prompt
import XMonad.Util.Run


trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-------------------------------------------------------------------------------
-- Calc
-------------------------------------------------------------------------------

data Calc = Calc

instance XPrompt Calc where
  showXPrompt        Calc = "calc> "
  commandToComplete  Calc = id
  completionFunction Calc = \s ->
    if   length s == 0
    then return []
    else fmap ((:[]) . trim) $ runProcessWithInput "calc" [s] ""

  modeAction Calc _ result =
    spawn $ "echo -n '" ++ result ++ "' | xclip -selection c"

-------------------------------------------------------------------------------
-- Bash
-------------------------------------------------------------------------------

data Bash = Bash

instance XPrompt Bash where
  showXPrompt        Bash = "Run: "
  commandToComplete  Bash = id
  completionFunction Bash = bashCompletion
  modeAction Bash query _ = spawn query

completeSingle :: String -> IO [String]
completeSingle "" = return []
completeSingle x  =
  let cmdType = if '/' `elem` x then "file" else "command"
  in  fmap lines . runProcessWithInput "bash" ["-c", "compgen -A " ++ cmdType ++ " " ++ x ++ "|sort -u"] $ ""

completeMultiple :: String -> [String] -> IO [String]
completeMultiple x xs = do
  exists <- doesFileExist $ "/usr/share/bash-completion/completions/" ++ x
  if   exists
  then fmap lines . runProcessWithInput "bash" ["-c", script] $ ""
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
               , "(IFS=$'\\n'; echo \"${COMPREPLY[*]}\")|sort -u"
               ]

bashCompletion ""   = return []
bashCompletion line = case words' line of
  []     -> return []
  [x]    -> completeSingle x
  (x:xs) -> completeMultiple x xs

-- escape aware variant of words
words' :: String -> [String]
words' = foldr f [""]
  where f x (y:ys) = case (x,y,ys) of
                       ('\\', "", y:ys) -> ("\\ "++y):ys
                       (' ',  _ , _   ) -> []:y:ys
                       _                -> (x:y):ys

-------------------------------------------------------------------------------
-- Pass
-------------------------------------------------------------------------------

data Pass = Pass

instance XPrompt Pass where
  showXPrompt        Pass = "Pass: "
  commandToComplete  Pass = id
  completionFunction Pass = \s -> do
    passwords <- getPasswords
    return . filter (\x -> s `isInfixOf` x) $ passwords

  modeAction Pass query result =
    if query `isInfixOf` result
      then spawn $ "pass show -c " ++ result
      else return ()

    --let args  | query `isInfixOf` result = "show -c " ++ result
    --          | otherwise "generate -c " ++ query ++ " 32"
    --in  spawn $ "pass " ++ args


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

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

calc = XPT Calc
bash = XPT Bash
pass = XPT Pass

prompt = mkXPromptWithModes
