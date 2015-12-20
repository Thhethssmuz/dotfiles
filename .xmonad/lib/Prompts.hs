module Prompts (masterPrompt) where

import Control.Monad
import Data.Char (isSpace)
import Data.List
import Data.Maybe (fromMaybe, catMaybes)

import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.Environment
import System.FilePath.Posix

import XMonad.Core
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run

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
    else fmap (map (f . f) . lines) $ runProcessWithInput "calc" [s] ""
    where f = reverse . dropWhile isSpace

  modeAction Calc _ result =
    spawn $ "echo -n '" ++ result ++ "' | xclip -selection c"

-------------------------------------------------------------------------------
-- Bash
-------------------------------------------------------------------------------

data Bash = Bash

instance XPrompt Bash where
  showXPrompt        Bash = "Run: "
  commandToComplete  Bash = reverse . takeWhile (`notElem` "\n;|<>") . reverse
  completionFunction Bash = bashCompletion
  modeAction Bash query result =
    let cmd = case words query of
                []  -> ""
                [x] -> if null result then query else result
                xs  -> if null . last $ xs then query else intercalate " " $ init xs ++ [result]
    in  spawn cmd

completeFile :: String -> IO [String]
completeFile x = do
  fmap lines . runProcessWithInput "bash" ["-c", script] $ ""
  where script = "compgen -A file " ++ x

completeCMD :: String -> IO [String]
completeCMD x = do
  fmap lines . runProcessWithInput "bash" ["-c", script] $ ""
  where script = "compgen -A command " ++ x

completeSingle :: String -> IO [String]
completeSingle x = if or . map (`elem` "./") $ x
                   then completeFile x
                   else completeCMD x

completeMultiple :: String -> [String] -> IO [String]
completeMultiple x xs = do
  exists <- doesFileExist $ "/usr/share/bash-completion/completions/" ++ x
  if   exists
  then fmap lines . runProcessWithInput "bash" ["-c", script] $ ""
  else case xs of
         []     -> error "this should not happen?"
         [""]   -> completeFile ""
         [y]    -> completeSingle y
         (y:ys) -> completeMultiple y ys

  where line   = intercalate " " (x:xs)
        script = intercalate "\n"
               [ "DIR=/usr/share/bash-completion"
               , ". \"$DIR/bash_completion\""
               , "[ -e \"$DIR/completions/" ++ x ++ "\" ] && . \"$DIR/completions/" ++ x ++ "\""
               , "a=($(complete -p " ++ x ++ "))"
               , "_completion_loader \"${a[-1]}\""
               , "COMP_WORDS=(" ++ line ++ ")"
               , "COMP_LINE='" ++ line ++ "'"
               , "COMP_POINT=" ++ show (length line)
               , "COMP_CWORD=" ++ show (length xs)
               , "${a[-2]} 2>/dev/null"
               , "(IFS=$'\\n'; echo \"${COMPREPLY[*]}\")"
               ]

bashCompletion ""   = return []
bashCompletion line =
  let ws  = words line
      ws' = if isSpace (last line) then ws ++ [""] else ws
  in  case ws' of
    []     -> return []
    [""]   -> completeFile ""
    [x]    -> completeSingle x
    (x:xs) -> completeMultiple x xs

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
    let args  | query `isInfixOf` result = "show -c " ++ result
              | otherwise = "generate -c " ++ query ++ " 32"
    in  spawn $ "pass " ++ args


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

modes :: [XPMode]
modes =
  [ XPT Calc
  , XPT Pass
  , XPT Bash
  ]

masterPrompt config = do
  mkXPromptWithModes modes config
