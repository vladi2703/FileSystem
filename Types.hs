module Types where

    type Content = String
    type FileName = String
    type DirName = String
    type PathStr = String
    type PathArr = [PathStr]

    data SystemElement = File {fileName::FileName,
                            fileContent::Content}
                        | Directory {directoryName::DirName,
                                    directoryContent::[SystemElement]} deriving (Eq, Show)
