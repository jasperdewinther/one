module Reader where
import Tools
import System.IO.UTF8
import Prelude hiding (readFile, writeFile)

-- |Read a given file and return the filecontent as an IO String.
-- If the given file does not exist the return value is an Error.
readOneFile :: String -> MaybeError (IO String)
readOneFile filename = do
    if (isValidFilename filename)
        then NotError $ readFile filename
        else Error "invalid filename"

-- |Remove whitespaces, split the string on newlines and check if the first line contains a questionmark.
-- The first line must be a questionmark otherwise there are operations outside functions.
-- Error is returned if the first line does not contain a questionmark.
cleanInput :: String -> MaybeError [String]
cleanInput s = checkFirstLine (removeEmptyLines (splitString (removeWhitespace s) '\n'))

-- |Check if the first character of the first line is a questionmark, otherwise return Error.
checkFirstLine :: [String] -> MaybeError [String]
checkFirstLine s = if isQuestionmark (head (head s))
                    then NotError s
                    else Error "there are instructions outside functions, the instruction on the first line should always be a function declaration"


isValidFilename :: String -> Bool
isValidFilename [_,'.','o','n','e'] = True
isValidFilename s | any (\x -> x=='\\' || x=='/') s = isValidFilename $ head $ reverse $ _splitOn s (\x -> x=='\\' || x=='/') "" ""
                  | otherwise = False

isInvalidWhitespace :: Char -> Bool
isInvalidWhitespace c = c==' ' || c=='\t' || c == '\r'

removeWhitespace :: String -> String
removeWhitespace s = conditionalRemove s isInvalidWhitespace

removeEmptyLines :: [String] -> [String]
removeEmptyLines s = conditionalRemove s (\x -> x=="" || (head x) == '#')

