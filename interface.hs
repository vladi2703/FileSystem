{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Interface where
    import Prelude
    import Essentials hiding (readInput)
    import Types
    import qualified Data.Text as T


    run :: PathStr -> SystemElement -> IO ()
    run currentPath currentRoot = do
            putStr (currentPath ++ "> ")
            cmd <- getLine
            if cmd == ""
                then run currentPath currentRoot
            else if cmd == "exit"
                then return()
            else do
                (newPath, newRoot) <- processInput cmd currentPath currentRoot
                run newPath newRoot


    processInput :: String -> PathStr -> SystemElement -> IO (PathStr, SystemElement)
    processInput inputArr currentPath currentRoot = runCmd (head $ words inputArr) (tail $ words inputArr) currentPath currentRoot

    runCmd :: String -> [String] -> PathStr -> SystemElement -> IO (PathStr, SystemElement)
    runCmd "pwd" _ currentPath currentRoot = printWorkingDirectory currentPath currentRoot
    runCmd "cd" args currentPath  currentRoot = changeDirCommand args currentPath currentRoot
    runCmd "ls" args currentPath  currentRoot = listContentsCommand args currentPath currentRoot
    runCmd "cat" args currentPath currentRoot = concatenateFilesCommand args currentPath currentRoot
    runCmd "rm" args currentPath currentRoot = removeFileCommand args currentPath currentRoot
    runCmd "cp" args currentPath currentRoot = copyFileCommand args currentPath currentRoot
    runCmd "mv" args currentPath currentRoot = moveFileCommand args currentPath currentRoot
    
    runCmd "reset" args currentPath currentRoot = return(currentPath, root)
    
    runCmd "getRoot" args currentPath currentRoot = do print currentRoot
                                                       return(currentPath, currentRoot)
    runCmd "print" args currentPath currentRoot = do putStrLn (concat args)
                                                     return(currentPath, currentRoot)

    runCmd  _   _    currentPath currentRoot = do putStrLn "command not found"
                                                  return(currentPath, currentRoot)

    printWorkingDirectory :: PathStr -> SystemElement -> IO (PathStr, SystemElement)
    printWorkingDirectory currentPath currentRoot = do
                            putStrLn currentPath
                            return (currentPath, currentRoot)

    changeDirCommand :: [String] -> PathStr -> SystemElement -> IO (PathStr, SystemElement)
    changeDirCommand [path] currentPath currentRoot = changeDirCommandRec (convertPathToArr path) (currentPath, currentRoot)
    changeDirCommand _ currentPath currentRoot = do
                                                putStrLn "Invalid arguments"
                                                return (currentPath, currentRoot)


    changeDirCommandRec :: [String] -> (PathStr, SystemElement) -> IO (PathStr, SystemElement)
    changeDirCommandRec [] (currentPath, currentRoot) = return (currentPath, currentRoot)
    changeDirCommandRec (".." : rest)  (currentPath, currentRoot) = changeDirCommandRec rest (convertPathToStr (getParentPath (convertPathToArr currentPath)), currentRoot)
    changeDirCommandRec (next : rest) (currentPath, currentRoot) = if isDir $ changeDirectory next currentPath currentRoot
                                            then changeDirCommandRec rest (getFullPath currentPath next, currentRoot)
                                            else do
                                                putStrLn "The system cannot find the path specified"
                                                return (currentPath, currentRoot)

--  cd Folder1/../Folder2/../Folder1/../Folder2 е еквивалентно на cd Folder2
    listContentsCommand :: [String] -> PathStr -> SystemElement -> IO (PathStr, SystemElement)
    listContentsCommand [] currentPath currentRoot = do
                        putStrLn $ getContent $ goToPath currentPath currentRoot
                        return (currentPath, currentRoot)
    listContentsCommand [path] currentPath currentRoot = do
        if pathElement == dummy 
            then do putStrLn "Invalid path"
                    return (currentPath, currentRoot) 
        else do putStrLn $ getContent $ pathElement
                return (currentPath, currentRoot)
            where pathElement =  goToPath(getFullPath currentPath path) currentRoot

    concatenateFilesCommand :: [String] -> PathStr -> SystemElement -> IO (PathStr, SystemElement)
    concatenateFilesCommand args currentPath currentRoot
        | null outputFile = if (all (/= dummy) (map (\x -> (goToPath (getFullPath currentPath x) currentRoot)) inputFilePaths)) then
                                do putStrLn $ concatMap (\x -> getContent(goToPath (getFullPath currentPath x) currentRoot)) inputFilePaths
                                   return (currentPath, currentRoot)
                            else do putStrLn "Invalid path"
                                    return (currentPath, currentRoot)
        | null inputFilePaths = do output <- readInput "" ""
                                   return (currentPath, addFile fileName output filePath (goToPath currentPath currentRoot))
        | otherwise = return (currentPath, concatenateFiles inputFilePaths (head outputFile) currentPath currentRoot)
                                      where filesList = args
                                            inputFilePaths = takeWhile (/= ">") filesList
                                            outputFile = if null $ dropWhile (/= ">") filesList
                                                            then []
                                                            else tail $ dropWhile (/= ">") filesList--head is ">"
                                            -- FOR SECOND CASE 
                                            outputFilePathArr = convertPathToArr (head outputFile)
                                            fileName = last outputFilePathArr
                                            filePath = init outputFilePathArr


    removeFileCommand :: [String] -> PathStr -> SystemElement -> IO (PathStr, SystemElement)
    removeFileCommand [] currentPath currentRoot = return(currentPath, currentRoot)
    removeFileCommand [file] currentPath currentRoot = return(currentPath, removeFile pathToRemoveFrom currentDir)
        where pathToRemoveFrom = convertPathToArr(getFullPath currentPath file)
              currentDir = goToPath currentPath currentRoot
    removeFileCommand (file:files) currentPath currentRoot = do (currPath, newRoot) <- removeFileCommand files currentPath (removeFile pathToRemoveFrom currentDir)
                                                                return(currentPath, newRoot)
        where pathToRemoveFrom = convertPathToArr(getFullPath currentPath file)
              currentDir = goToPath currentPath currentRoot

    copyFileCommand :: [String] -> PathStr -> SystemElement -> IO (PathStr, SystemElement)
    copyFileCommand [] currentPath currentRoot = do putStrLn "Unsuccessful operation" 
                                                    return(currentPath, currentRoot)
    copyFileCommand [_] currentPath currentRoot = do putStrLn "Unsuccessful operation" 
                                                     return(currentPath, currentRoot)
    copyFileCommand [destination, source] currentPath currentRoot = return(currentPath, copyFile destination source currentDir currentRoot)
        where currentDir = goToPath currentPath currentRoot
    
    moveFileCommand :: [String] -> PathStr -> SystemElement -> IO (PathStr, SystemElement)
    moveFileCommand [] currentPath currentRoot = do putStrLn "Unsuccessful operation" 
                                                    return(currentPath, currentRoot)
    moveFileCommand [_] currentPath currentRoot = do putStrLn "Unsuccessful operation" 
                                                     return(currentPath, currentRoot)
    moveFileCommand [destination, source] currentPath currentRoot = return(currentPath, moveFile destination source currentDir currentRoot)
        where currentDir = goToPath currentPath currentRoot
    
        
    
    readInput :: String -> String -> IO String
    readInput input "." = return input
    readInput input currentLine = do currInput <- getLine
                                     readInput (input ++ currentLine) currInput


    main = run "/" root