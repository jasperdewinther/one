-- |This module is used for reading a file.

module Reader where
import Tools
import System.IO.UTF8
import Prelude hiding (readFile, writeFile)

-- |Read a given file and return the filecontent as an IO String.
-- If the given file does not exist the return value is an Error.
readOneFile :: String -> MaybeError (IO String)
readOneFile filename | isValidFilename filename = NotError $ readFile filename
                     | otherwise = Error "invalid filename, note that all source files have to use the following naming scheme: \"p.one\" where p is always a single character"

-- |Remove whitespaces, split the string on newlines and check if the first line contains a questionmark.
-- The first line must be a questionmark otherwise there are operations outside functions.
-- Error is returned if the first line does not contain a questionmark.
cleanInput :: String -> MaybeError [String]
cleanInput s = checkFirstLine (removeEmptyLines (splitString (removeWhitespace s) '\n'))

-- |Check if the first character of the first line is a questionmark, otherwise return Error.
checkFirstLine :: [String] -> MaybeError [String]
checkFirstLine s | head (head s) == '?' = NotError s
                 | otherwise = Error "there are instructions outside functions, the instruction on the first line should always be a function declaration"

-- |Check if a given string is a valid filename, this is only the case when the filename consists of one character and the .one extension.
isValidFilename :: String -> Bool
isValidFilename [_,'.','o','n','e'] = True
isValidFilename s | any (\x -> x=='\\' || x=='/') s = isValidFilename $ last $ _splitOn s (\x -> x=='\\' || x=='/') "" ""
                  | otherwise = False

-- |remove tabs, spaces or carriage return characters
removeWhitespace :: String -> String
removeWhitespace s = conditionalRemove s (\c -> c==' ' || c=='\t' || c == '\r')

-- |remove empty lines, or lines that start with #, from a list of strings
removeEmptyLines :: [String] -> [String]
removeEmptyLines s = conditionalRemove s (\x -> x=="" || head x == '#')

