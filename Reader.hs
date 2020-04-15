module Reader where

readOneFile :: String -> Maybe (IO String) 
readOneFile filename = do
    if (isValidFilename filename)
        then Just $ readFile filename
        else Nothing


isValidFilename :: String -> Bool
isValidFilename ([_,'.','o','n','e']) = True
isValidFilename _ = False