module Reader where
import Tools
import System.IO.UTF8
import Prelude hiding (readFile, writeFile)

readOneFile :: String -> MaybeError (IO String)
readOneFile filename = do
    if (isValidFilename filename)
        then NotError $ readFile filename
        else Error "invalid filename"

cleanInput :: String -> MaybeError [String]
cleanInput s = checkFirstLine (removeEmptyLines (splitString (removeWhitespace s) '\n'))

checkFirstLine :: [String] -> MaybeError [String]
checkFirstLine s = if isQuestionmark (head (head s))
                    then NotError s
                    else Error "there are instructions outside functions, the instruction on the first line should always be a function declaration"


isValidFilename :: String -> Bool
isValidFilename [_,'.','o','n','e'] = True
isValidFilename _ = False

isInvalidWhitespace :: Char -> Bool
isInvalidWhitespace c = c==' ' || c=='\t' || c == '\r'

isEmptyString :: String -> Bool
isEmptyString s = s == ""

removeWhitespace :: String -> String
removeWhitespace s = conditionalRemove s isInvalidWhitespace

removeEmptyLines :: [String] -> [String]
removeEmptyLines s = conditionalRemove s isEmptyString

