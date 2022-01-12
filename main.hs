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


--"Folder1/Folder 1.1"

--https://paste.pics/FFN8V MAY EXPIRE 05.01
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

currentDir :: SystemElement
currentDir = root

file112 :: SystemElement
file112 = File {fileName =  "File112", fileContent = "Content of file 112"}

getName :: SystemElement -> FileName
getName(File name _) = name
getName(Directory name _) = name

getContent :: SystemElement -> Content
-- !! LS should unwords getContent 
getContent (Directory _ contents) = unwords (map getName contents)
getContent (File _ content) = content

-- getSystemElement :: PathArr -> SystemElement
-- getSystemElement [] currentElement = currentElement
-- getSystemElement fullPath currentElement = 

-- getSubDirsMaybe :: SystemElement -> Maybe [SystemElement]
-- getSubDirsMaybe (Directory _ contents) = Just contents
-- getSubDirsMaybe (File _ content) = Nothing --file has no subdirs

getSubDirs :: SystemElement -> [SystemElement]
getSubDirs (Directory _ contents) = contents
getSubDirs (File _ content) = [] --file has no subdirs


getFullPath currentPath filePath
    | "/" `isPrefixOf` filePath = filePath
    | currentPath == "/" = "/" ++ filePath
    | otherwise = currentPath ++ "/" ++ filePath


--gets path, returnt path without the last elem 
--"/folder1/folder2/folder3" -> "/folder1/folder2/"
getParent :: PathArr -> PathArr
getParent ["/"] = ["/"]
getParent [path] = ["/"] --if only one dir -> return root
getParent path = init path

getParentStr :: PathStr -> PathStr
getParentStr pathStr = convertPathToStr $ getParent $ convertPathToArr pathStr

convertPathToStr :: PathArr -> PathStr
convertPathToStr pathArray =  intercalate "/" pathArray

--not enough time to get comfortable using text and invesitgate how it behaves in comparison with String
convertPathToArr :: PathStr -> PathArr
convertPathToArr "/" = ["/"]
convertPathToArr path = map T.unpack $ T.splitOn (T.pack "/") (T.pack pathClear)
    where pathClear | head path == '/' = tail path
                    | otherwise = path
--if there is no #where then "/head/tail" -> ["", "head", "tail"]
--https://newbedev.com/how-to-split-a-string-in-haskell if forbidden Data.List
-- corner case is ..

--isValidPath currentPathArr toCheck 
--isValidFullPath :: [String] -> Bool

member :: (Eq a) => a -> [a] -> Bool
member x [] = False
member x (y:ys) | x == y    = True
                | otherwise = member x ys

memberOfDir :: [SystemElement] -> DirName -> Maybe SystemElement
memberOfDir [] dirName = Nothing
memberOfDir contentOfDir dirName
    | getName (head contentOfDir) == dirName && (getSubDirs (head contentOfDir) /= []) = Just (head contentOfDir)
    | otherwise = memberOfDir (tail contentOfDir) dirName

isSubDir :: PathArr -> PathStr-> Bool
isSubDir dirToCheck potentialParent = last (getParent dirToCheck) == potentialParent
--last elem is the deepest folder 

-- check if current dir should be root 
-- !current dir should be FULL 
isValidPath :: PathStr -> PathStr -> Bool
isValidPath [] currentDir = False
isValidPath pathStr currentDir = case getDir pathArr currentDirSysElem of
                                    Just dir -> True
                                    Nothing  -> False
    where currentDirSysElem = Data.Maybe.fromMaybe (Directory "Dummy" []) (getDir (convertPathToArr currentDir) root)
          pathArr = convertPathToArr pathStr

--when we return dummy with empty subfolders list #getDir pathArr Dummy# 
--will return Nothing which is already a case 

getDir :: PathArr -> SystemElement -> Maybe SystemElement
getDir ["/"] _ = Just root
getDir [] currentDir = Just currentDir
getDir (dirToCheck: rest) currentDir =
 case nextDir of
    Just dir -> getDir rest dir
    Nothing  -> Nothing
    where nextDir = memberOfDir (getSubDirs currentDir) dirToCheck

--  goToPath :: [String] -> SystemElement
--  goToPath (el:elements) =   

-- main filePath = do
--     input <- getline
--     if input == "x"
--     then return ()   
--     else do
--     runInput input filePath --returns the new path

runInput :: String -> PathStr -> IO PathStr
runInput inputArr currentPath = runCmd (head $ words inputArr) (tail $ words inputArr) currentPath

runCmd :: String -> [String] -> PathStr -> IO PathStr
runCmd "pwd" _ currentPath = printWorkingDirectory currentPath
--runCmd "cd" args currentPath = changeDirectory args currentPath


printWorkingDirectory :: PathStr -> IO PathStr
printWorkingDirectory currentPath = do
    putStrLn currentPath
    return currentPath

-- changeDirectory :: PathStr -> PathStr -> IO ()
-- changeDirectory ".." currentPath = getParent currentPath
-- changeDirectory path currentPath
--     | pathList == [] return currentPath 
--     |  
--       where pathList = convertPathToArr path

main = putStrLn $ getContent root
