module Reader where
import Tools


readOneFile :: String -> IO String
readOneFile filename = do
    if (isValidFilename filename)
        then readFile filename
        else error "invalid filename"

cleanInput :: String -> [String]
cleanInput s = (removeEmptyLines (splitString (removeWhitespace s) '\n'))

isValidFilename :: String -> Bool
isValidFilename [_,'.','o','n','e'] = True
isValidFilename _ = False

isInvalidWhitespace :: Char -> Bool
isInvalidWhitespace c = c==' ' || c=='\t'

isEmptyString :: String -> Bool
isEmptyString s = s == ""

removeWhitespace :: String -> String
removeWhitespace s = conditionalRemove s isInvalidWhitespace

removeEmptyLines :: [String] -> [String]
removeEmptyLines s = conditionalRemove s isEmptyString

