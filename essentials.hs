{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Essentials where
    import Prelude
    import Data.List
    import Types
    import qualified Data.Text as T
    import qualified Data.Maybe

    contentFile1 :: Content
    contentFile1 = "Content of file1"

    contentFile2 :: Content
    contentFile2 = "Content of file2"

    contentFile3 :: Content
    contentFile3 = "Content of file3"

    contentFile4 :: Content
    contentFile4 = "Content of file4"

    --https://paste.pics/FSY8D hand-drawn sketch 
    root :: SystemElement
    root = Directory {directoryName = "/", directoryContent = [
                Directory "Folder1" [
                    Directory "Folder1.1" [], File {fileName = "File1", fileContent = contentFile1}
                ],
                File {fileName =  "File2", fileContent = contentFile2},
                Directory "Folder2" [
                    File{fileName = "File2.1", fileContent = contentFile3}, File{fileName = "File2.2", fileContent = contentFile4}
                ]
    ]}
-- don't want to use Just and Nothing at this point
    dummy :: SystemElement
    dummy = File {fileName = "dummy", fileContent = "dummy" }
--Path converters
    convertPathToStr :: PathArr -> PathStr
    convertPathToStr ["/"] = "/"
    convertPathToStr pathArray =  "/" ++ intercalate "/" pathArray ----convertPathToArr ignores the /

    --not enough time to get comfortable using text and invesitgate how it behaves in comparison with String
    convertPathToArr :: PathStr -> PathArr
    convertPathToArr "/" = ["/"]
    convertPathToArr path = map T.unpack $ T.splitOn (T.pack "/") (T.pack pathClear)
        where pathClear | head path == '/' = tail path
                        | otherwise = path
--Getters
    getName :: SystemElement -> FileName
    getName(File name _) = name
    getName(Directory name _) = name

    getContent :: SystemElement -> Content
    getContent (Directory _ contents) = unwords (map getName contents)
    getContent (File _ content) = content

    getSubDirs :: SystemElement -> [SystemElement]
    getSubDirs (Directory _ contents) = contents
    getSubDirs (File _ _) = []

    isDir :: SystemElement -> Bool
    isDir (Directory _ _) = True
    isDir (File _ _) = False

    getDirByName :: DirName -> SystemElement -> SystemElement
    getDirByName toSearch currentDir
        | toSearch `notElem` map getName (getSubDirs currentDir) = dummy
        | otherwise = rightDir
            where rightDir = head $ filter ((==toSearch) . getName) (getSubDirs currentDir)
    --assume names are unique

    getFullPath :: PathStr -> PathStr -> PathStr
    getFullPath currentPath filePath
        | "/" `isPrefixOf` filePath = filePath
        | currentPath == "/" = "/" ++ filePath
        | otherwise = currentPath ++ "/" ++ filePath

    -- !FULL PATH
    getDir :: PathArr ->SystemElement ->SystemElement
    getDir [] currentDir = currentDir
    getDir (elder:children) currentDir = if nextDir /= dummy then getDir children nextDir else dummy
        where nextDir = getDirByName elder currentDir

    getParentPath :: PathArr -> PathArr
    getParentPath ["/"] = ["/"]
    getParentPath [path] = ["/"] --if only one dir -> return root
    getParentPath path = init path

    goToPath :: PathStr -> SystemElement -> SystemElement
    goToPath "/" currentRoot = currentRoot
    goToPath pathStr currentRoot= getDir pathArr currentRoot
        where pathArr = convertPathToArr pathStr

    -- Current path is FULL Path
    changeDirectory :: PathStr -> PathStr -> SystemElement -> SystemElement
    changeDirectory pathToGo currentPath currentRoot = goToPath (getFullPath currentPath pathToGo) currentRoot

    -- ! FULL PATH  required 
    -- when path to add to is empty -> then we are in the desired directory and we have to create the file
    -- we create the file when to the content of current Directory we create and add the file we want
    addFile :: FileName -> Content -> [DirName] -> SystemElement -> SystemElement
    addFile fileName fileContent pathToAddTo currentDir@(File _ _) = currentDir
    addFile fileName fileContent [] currentDir@(Directory dirName dirContent)
             = currentDir{directoryContent = dirContent ++ [File fileName fileContent]}
    -- in order to get to the desired directory we check if the next folder in the path is part of the contents of the current folder
    -- if the current folder is parrent of the desired one we continue the reccursion and we add the rest of the content of the current Folder
    -- if we don't find the desired folder in the current -> then we return the current folder in order to stop searching because an invalid path was given
    addFile fileName fileContent pathToAddTo currentDir@(Directory dirName dirContent)
        | newCurr == dummy = currentDir --invalid pathToAddTo
        | newCurr /= dummy = currentDir{directoryContent = dirContentNoNewCurr ++ [addFile fileName fileContent (tail pathToAddTo) newCurr]}
            where toSearch = head pathToAddTo
                  newCurr = getDirByName toSearch currentDir
                  dirContentNoNewCurr = filter (/=newCurr) dirContent
--if this file exists it gets overwritten

    concatenateFiles :: [PathStr] -> [Char] -> PathStr -> SystemElement -> SystemElement
    concatenateFiles filePaths outputFile currentPath currentRoot
            = addFile fileName output filePath (goToPath currentPath currentRoot)
        where output = concatMap (\x -> getContent(goToPath (getFullPath currentPath x) currentRoot)) filePaths
              outputFilePathArr = convertPathToArr outputFile
              fileName = last outputFilePathArr
              filePath = init outputFilePathArr

    copyTree :: SystemElement -> SystemElement
    copyTree tree@(File _ _) = tree
    copyTree tree@(Directory name contents) = Directory name (map copyTree contents)

-- when rm called -> root = rm PATH_ARR root
--          where PATH_ARR = convertPathToArr getFullPath currentPath
    removeFile :: PathArr -> SystemElement -> SystemElement
    removeFile _ currentDir@(File _ _ ) = currentDir --invalid current dir, cannot be File
    removeFile [name] currentDir@(Directory _ dirContent) = currentDir{directoryContent = filter ((/= name) . getName) dirContent}
    removeFile pathToRemoveFrom currentDir@(Directory dirName dirContent)
        | newCurr == dummy = currentDir --invalid Path
        | newCurr /= dummy = currentDir{directoryContent = dirContentNoNewCurr ++ [removeFile (tail pathToRemoveFrom) newCurr]}
            where toSearch = head $ init pathToRemoveFrom
                  newCurr = getDirByName toSearch currentDir
                  dirContentNoNewCurr = filter (/=newCurr) dirContent

    copyFile :: PathStr -> PathStr -> SystemElement -> SystemElement -> SystemElement
    copyFile destination source currentDir root 
        | isDir (goToPath destination root) = addFile (getName toCopy) (getContent toCopy) destinationArr currentDir
        | otherwise = currentDir
            where toCopy = goToPath source root
                  destinationArr = convertPathToArr destination
    
    moveFile :: PathStr -> PathStr -> SystemElement -> SystemElement -> SystemElement
    moveFile destination source currentDir root 
        | isDir (goToPath destination root) = removeFile sourceArr (copyFile destination source currentDir root)
        | otherwise = currentDir
            where sourceArr = convertPathToArr source