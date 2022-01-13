{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Interface where
    import Prelude
    import Essentials hiding (readInput)
    import Types
    import qualified Data.Text as T


    -- run :: pathStr -> IO()
    -- run currentPath = do
    --         cmd <- getline
    --         if (cmd == "")
    --             then run currentPath 
    --         else if cmd == "exit"
    --             then return()
    --           --  else do

    processInput :: String -> PathStr -> SystemElement -> IO (PathStr, SystemElement)
    processInput inputArr currentPath currentRoot = runCmd (head $ words inputArr) (tail $ words inputArr) currentPath currentRoot

    runCmd :: String -> [String] -> PathStr -> SystemElement -> IO (PathStr, SystemElement)
    runCmd "pwd" _ currentPath currentRoot = printWorkingDirectory currentPath currentRoot
    runCmd "cd" args currentPath  currentRoot = changeDirCommand args currentPath currentRoot
    runCmd "ls" args currentPath  currentRoot = listContentsCommand args currentPath currentRoot
    --runCmd "cat" args currentPath currentRoot = concatenateFilesCommand args currentPath currentRoot

    printWorkingDirectory :: PathStr -> SystemElement -> IO (PathStr, SystemElement)
    printWorkingDirectory currentPath currentRoot = do
                            putStrLn currentPath
                            return (currentPath, currentRoot)

    changeDirCommand :: [String] -> PathStr -> SystemElement -> IO (PathStr, SystemElement)
    changeDirCommand [".."] currentPath currentRoot = return (convertPathToStr (getParentPath (convertPathToArr currentPath)), currentRoot)
    changeDirCommand [path] currentPath currentRoot = if changeDirectory path currentPath currentRoot /= dummy
                                            then return (getFullPath currentPath path, currentRoot)
                                            else do
                                                putStrLn "The system cannot find the path specified"
                                                return (currentPath, currentRoot)
    changeDirCommand _ currentPath currentRoot = do
                                            putStrLn "Too many arguments"
                                            return (currentPath, currentRoot)

--cd scheme/../haskell/../scheme/../haskell е еквивалентно на cd haskell

    listContentsCommand :: [String] -> PathStr -> SystemElement -> IO (PathStr, SystemElement)
    listContentsCommand [] currentPath currentRoot = do
                        putStrLn $ getContent $ goToPath currentPath currentRoot
                        return (currentPath, currentRoot)
    listContentsCommand [path] currentPath currentRoot = do
        putStrLn $ getContent $ goToPath(getFullPath currentPath path) currentRoot
        return (currentPath, currentRoot)

    concatenateFilesCommand :: [String] -> PathStr -> SystemElement -> IO (PathStr, SystemElement)
    -- "Folder1 Folder1 Folder1 Folder1 Folder1 "
    concatenateFilesCommand [files] currentPath currentRoot 
        | null outputFile =  do putStrLn $ concatMap (\x -> getContent(goToPath (getFullPath currentPath x) currentRoot)) inputFilePaths
                                return (currentPath, currentRoot)
        | otherwise = return (currentPath, concatenateFiles inputFilePaths (head outputFile) currentPath currentRoot)
                                      where filesList = words files
                                            inputFilePaths = takeWhile (/= ">") filesList
                                            outputFile = tail $ dropWhile (/= ">") filesList --head is ">"

    readInput :: [Char] -> String -> IO()
    readInput input "." = print input
    readInput input currentLine = do currInput <- getLine
                                     readInput (input ++ currentLine) currInput
        