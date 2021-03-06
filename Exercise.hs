{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
module Exercise where
import Data.List (intercalate, sort, sortOn)



-- Exercise set 2.
--
-- 30% of the exercises are intended to be rather challenging, and
-- will allow you to get a mark above 69%, in conjunction with the
-- other available exercises, so as to get a 1st class mark. To get
-- II.2, you will need to do enough hard exercises, in addition to the
-- medium and easy ones. 
--
--      >= 70% 1st
--      >= 60% II.1
--      >= 50% II.2
--      >= 40% III
--      <= 39% fail
--
-- 
-- Do as many unassessed exercises as you can, as they should make the
-- assessed exercises easier.
--
-- You are allowed to use the functions available in the standard
-- prelude (loaded by default by ghc and ghci). You should not need to
-- use other Haskell libraries from the Haskell platform available in
-- the lab, but you are allowed to use them if you wish. However, in
-- your final submission, you should not use any IO facilities, as
-- this won't compile with the marking script.

-- This exercise set revolves around a directory tree on a computer,
-- and some Unix-like functions to manipulate them.
--
-- This exercise doesn't involve reading or writing actual files from
-- disk. Instead, we represent them internally in Haskell using a
-- "data" definition, explained below.
--
-- We have these data types:
--
--     - Entry: Can be either a file or a directory (with sub-Entries)
--     - EntryName: exactly the same as a string; 
--                  represents a directory or file name
--     - Path: exactly the same as a list of strings
--     - FileProp: file properties 


data Entry = File EntryName FileProp
           | Dir EntryName [Entry]
  deriving (Show, Eq, Read)


-- The name of a file
type EntryName = String

-- A sequence of file name components.
--
-- A path is a list of strings used to navigate down to a subdirectory
-- of a given directory. We start from an Entry, and we end up with a
-- sub-Entry, provided the path is valid. See the example in the "cd"
-- exercise below.
type Path = [String]

-- FileProp describes some attributes of a file.
--
-- The components mean size, content, and creation time,
-- in that order.
data FileProp = FP Int String Int
  deriving (Show, Eq, Read)


-- Exercise, easy. Create a FileProp that describes a file with size 3,
-- content "abc", and time 4.

exampleFP :: FileProp
exampleFP = FP 3 "abc" 4

{-

Entries describe directories and files in the file system. For instance, the
following entry describes an empty directory:

    Dir "somedirname" []

The following entry describes a file in the filesystem:

    File "somefilename" (FP 4 "xyz" 3)

The following entry describes a more complicated directory.

    Dir "uni" [File "marks.txt" (FP 1036 "..." 2014),
               Dir "examSheets" [],
               File "address.txt" (FP 65 "..." 2010)
              ]

-}

-- Exercise, easy. Create three entries that correspond to the following trees:
--
-- 1.   todo.txt, size 723, time 2015, content "do fp exercises"
--
-- 2.   empty-directory
--      |
--
-- 3.   hard drive
--      |
--      |-- WINDOWS
--      |   |
--      |   |-- cmd.exe, size 1024, time 1995, content ""
--      |   |
--      |   |-- explorer.exe, size 2048, time 1995, content ""
--      |
--      |-- Documents
--      |   |
--      |   |-- User1
--      |   |   |
--      |   |   |-- recipe.doc, size 723, time 2000, content ""
--      |   |
--      |   |-- User2
--      |   |   |
--
-- You must pay attention to the order of entries in a directory.
--
-- There is a dash in the directory name of exampleEntry2.


exampleEntry1 :: Entry
exampleEntry1 = File "todo.txt" (FP 723 "do fp exercises" 2015)
exampleEntry2 :: Entry
exampleEntry2 = Dir "empty-directory" []
exampleEntry3 :: Entry
exampleEntry3 = Dir "hard drive" [Dir "WINDOWS" [File "cmd.exe" (FP 1024 "" 1995), File "explorer.exe" (FP 2048 "" 1995)],
                                  Dir "Documents" [Dir "User1" [File "recipe.doc" (FP 723 "" 2000)], 
                                                   Dir "User2" []] ]
exampleEntry4 = Dir "hard drive" [Dir "WINDOWS" [File "xmd.exe" (FP 1024 "" 1995), File "explorer.exe" (FP 2048 "" 1995)],
                                  Dir "Documents" [Dir "User2" [File "recipe.doc" (FP 723 "" 2000)], 
                                                   Dir "User1" []] ]

-- Exercise, unassessed. You're given a directory as a value of type
-- Entry. In this directory there is a subdirectory with name n. Find
-- (and return) this subdirectory.

cd1 :: Entry -> String -> Maybe Entry
--cd1 (File _ _)  n = Nothing 
cd1 ( Dir name [] ) n                                                    = Nothing
cd1 ( Dir name directory1@( (File _ _ ):xs) ) n                          = cd1 (Dir name xs) n
cd1 ( Dir name directory1@( (directory2@(Dir entryName _) ):xs) ) n  
                                                        | directory1==[] = Nothing
                                                        | entryName==n   = Just directory2
                                                        | entryName/=n   = cd1 (Dir name xs) n
                                                        | otherwise      = Nothing



-- Exercise, easy. As before, but you need to navigate not one but
-- possibly many steps down; consecutive directory names are given in
-- the list of strings.
-- 
-- Example: Given the entry in the following drawing
-- 
--     root
--     |
--     |-- dir1
--     |   |
--     |   |-- dir1a
--     |   |   |
--     |   |   |-- dir1a1
--     |   |   |
--     |   |   |-- dir1a2
--     |   |
--     |   |-- dir1b
--     |
--     |-- dir2
--     |   |
--     |   |-- dir1a
--     |   |
--     |   |-- dir1b
--     |
--     |-- file3
-- 
-- and the path ["dir1", "dir1a"], you need to return the Entry that
-- contains dir1a1 and dir1a2.
-- 
-- If there is no such entry, return Nothing. If there is such an entry,
-- return Just that entry.
-- 
-- You can assume that there will be at most one entry with the given path.

cd :: Entry -> Path -> Maybe Entry 
cd root []                              = Just root
cd root [""]                            = Just root
cd root [x]                             = cd2 root x 
cd root (x:xs)   | cd1 root x ==Nothing = Nothing
                 | otherwise            = cd ( maybe2Entry (cd1 root x) ) xs

cd2 :: Entry -> String -> Maybe Entry
cd2 file@(File name _)  n                                           | n==name        = Just file
                                                                    | otherwise      = Nothing
cd2 ( Dir name [] ) n                                                    = Nothing
cd2 ( Dir name ( file@(File fileName _ ):xs) ) n                    | fileName == n  = Just file
                                                                    | otherwise      = cd2 (Dir name xs) n
cd2 ( Dir name directory1@( (directory2@(Dir entryName _) ):xs) ) n  
                                                                    | directory1==[] = Nothing
                                                                    | entryName==n   = Just directory2
                                                                    | entryName/=n   = cd2 (Dir name xs) n
                                                                    | otherwise      = Nothing


maybe2Entry :: Maybe Entry -> Entry
maybe2Entry x = case x of Just x -> x
-- Exercise, medium. Split a string representing a path into its
-- components. The components are separated by (forward) slashes.
-- Hint: the prelude functions for lists will be helpful here, but you
-- are not required to use them.
--
-- Examples:
--
--     explode "abc/de/fghi" = ["abc", "de", "fghi"]
--     explode "abc//fghi" = ["abc", "", "fghi"]
--
-- It is a matter of convention whether we prefer
--     explode "" = [] 
-- or
--     explode "" = [""]
-- Choose your own convention. Both will be considered to be correct.

--use splitAt()
--locate the forward slash then split
explode :: String -> Path
explode s = explode' s (elemIndex' '/' s 0)

explode' s index | index<0   = [s]
                 | index==((length s)-1) = take (index) s : [""]
                 | otherwise = take (index) s : explode (drop (index+1) s)  

elemIndex' char string index| index > ((length string) -1) = -1
                            | char == (!!) string index    = index
                            | otherwise                    = elemIndex' char string (index+1)

-- Exercise, easy. The "inverse" of explode: combine components with
-- slashes to a single String.
--
-- For every string s, you must have
--
--    implode (explode s) = s
--
-- You may want to use the functions "concat", "intersperse", and/or
-- "intercalate"; the latter two are from package Data.List.

implode :: Path -> String
implode path = intercalate "/" path   

-- Exercise, easy. Given an Entry representing a directory, print out
-- a directory listing in a format similar to "ls -l" on Unix.
--
-- The required format is as in the following example:
--
--     size: 420 time: 5 filename1
--     size: 5040 time: 200 other.txt
--     size: 30 time: 36 filename2
--
-- You need to separate every line with a newline ('\n') character,
-- and also put a newline at the end.
--
-- Keep the files in their order in the given Entry.
--
-- You do not need to convert units, just print the numbers.

lsL :: Entry -> String
lsL (Dir _ [])        = ""
lsL (Dir name1 (File fileName (FP size _ time): xs ) )      = "size: " ++ show size ++ " time: "++ show time ++  " " ++ fileName ++ "\n" ++ lsL (Dir name1 xs)
lsL (Dir name1 ( (Dir dirName _):xs ) )                     = dirName ++ "\n" ++ lsL (Dir name1 xs) 

-- Exercise, medium. List all the files in a directory tree. Sample
-- output:how
--
--    root
--    |
--    |-- dir1
--    |   |
--    |   |-- dir1a
--    |   |   |
--    |   |   |-- dir1a1
--    |   |   |
--    |   |   |-- somefile
--    |   |   |
--    |   |   |-- dir1a2
--    |   |
--    |   |-- dir1b
--    |
--    |-- file2
--    |
--    |-- dir3
--    |   |
--    |   |-- dir3a
--    |   |
--    |   |-- dir3b
--
--
-- You can assume that the entry represents a directory.
--
-- Use the newline convention as given above.

lsTree ::Entry ->String
lsTree directory@(Dir name _) = name ++ (lsTree' directory 0)

lsTree' ::Entry -> Int -> String
lsTree' (Dir name []) n                                    = ""
lsTree' (Dir name ( directory@( Dir dirName ys ):xs ) ) n  = "\n|" ++pipes n ++ "\n|" ++ pipes n ++ "-- " ++ dirName ++ lsTree' directory (n+1) ++ lsTree' (Dir name xs) n
lsTree' (Dir name ( (File fileName (FP _ _ _) ) :xs ) ) n  = "\n|" ++pipes n ++ "\n|" ++ pipes n ++ "-- " ++ fileName ++ lsTree' (Dir name xs) n 


pipes :: Int -> String
pipes 0 = ""
pipes n = "   |" ++ (pipes $ n-1)

-- Exercise, challenge. Make a list of all the files and directories
-- recursively in a tree (similar to "find ." in linux). If the
-- argument fullPath is False, every entry in the returned list will
-- have only the bare directory or file name. If fullPath is True,
-- every entry is the path towards that entry,
-- e.g. "root/subdir1/subdir1a/file".
--
-- The root must be the first list item. The output will be in the
-- same order as for lsTree.
--
-- For example, if d is this directory from an earlier exercise:
--
--      hard drive
--      |
--      |-- WINDOWS
--      |   |
--      |   |-- cmd.exe, size 1024, time 1995, content ""
--      |   |
--      |   |-- explorer.exe, size 2048, time 1995, content ""
--      |
--      |-- Documents
--      |   |
--      |   |-- User1
--      |   |   |
--      |   |   |-- recipe.doc, size 723, time 2000
--      |   |
--      |   |-- User2
--      |   |   |
--
-- then we have

--       listAll False d =
--                ["hard drive", "WINDOWS", "cmd.exe", "explorer.exe"
--                ,"Documents", "User1", "recipe.doc", "User2"]
-- and
--      listAll True d = 
--                ["hard drive"
--                ,"hard drive/WINDOWS"
--                ,"hard drive/WINDOWS/cmd.exe"
--                ,"hard drive/WINDOWS/explorer.exe"
--                ,"hard drive/Documents"
--                ,"hard drive/Documents/User1"
--                ,"hard drive/Documents/User1/recipe.doc",
--                ,"hard drive/Documents/User2"]

listAll :: Bool -> Entry -> [String]
--listAll that adds the directory name to the beginning if it is the first time it is opened
listAll fullPath (Dir name []) = [name]
listAll fullPath (File fileName _) = [fileName]
--the below case is to tell the difference between an empty directory, when the name should be placed once,
--and a directory that we have finished searching through, when the name should now be placed to avoid duplications.
listAll fullPath (Dir name ( directory@( Dir dirName [] ): xs ) )| fullPath     = name : (listAll fullPath (Dir (name ++ "/" ++ dirName) []) ) ++ (listAll' fullPath (Dir name xs))
                                                                 | not fullPath = name : (listAll fullPath directory) ++ (listAll' fullPath (Dir name xs))
listAll fullPath (Dir name ( directory@( Dir dirName ents): xs)) | fullPath     = name : (listAll fullPath (Dir (name ++ "/" ++ dirName) ents) ) ++ (listAll' fullPath (Dir name xs))
                                                                 | not fullPath = name : (listAll fullPath directory) ++ (listAll' fullPath (Dir name xs))
listAll fullPath (Dir name ( (File fileName (FP _ _ _) ): xs ) ) | fullPath     = name : (name ++ "/" ++ fileName) : (listAll' fullPath (Dir name xs))
                                                                 | not fullPath = name :  fileName : (listAll' fullPath (Dir name xs))

--listAll that does not add the directory name to the beginning to avoid duplicating names
listAll' _ (Dir name []) = []
listAll' fullPath (Dir name ( directory@( Dir dirName ents ): xs ) )| fullPath     = (listAll fullPath (Dir (name ++ "/" ++ dirName) ents)) ++ listAll' fullPath (Dir name xs) 
                                                                    | not fullPath = (listAll fullPath directory) ++ listAll' fullPath (Dir name xs) 
listAll' fullPath (Dir name ( (File fileName (FP _ _ _) ): xs ) )   | fullPath     = (name ++ "/" ++ fileName) : listAll' fullPath (Dir name xs)
                                                                    | not fullPath = fileName : listAll' fullPath (Dir name xs)

-- Exercise, hard. 
--
-- Given a tree, insert a given subtree in a certain position.
--
-- It does not matter how the inserted subtree is ordered with respect
-- to the other items in the directory where it is inserted. That is,
--
--     cp (Dir "root" [Dir "subdir1" [Dir "subdir1a" []]]) (["subdir1"], Dir "subdir1b" [])
--
-- may return either
--
--     Dir "root" [Dir "subdir1" [Dir "subdir1a" [], Dir "subdir1b" []]]
--
-- or
--
--     Dir "root" [Dir "subdir1" [Dir "subdir1b" [], Dir "subdir1a" []]] .
--
-- (This function is similar-ish to the Unix 'cp' utility.)

cp :: Entry -> (Path, Entry) -> Entry
cp (Dir name entries) ([], subtree)                                                              = Dir name (subtree:entries)
cp (Dir name ( ( Dir dirName ents ): xs ) ) ([path],subtree)            | dirName == path  = Dir name ( ( Dir dirName (subtree:ents) ): xs )
                                                                        | otherwise        = Dir name ( [Dir dirName ents] ++ cp' (Dir name xs) ([path],subtree)  )
cp (Dir name ( ( Dir dirName ents ): xs ) ) ( (path:subpath),subtree )  | dirName == path  = Dir name ( ( cp ( Dir dirName ents ) (subpath, subtree) ) : xs )
                                                                        | otherwise        = Dir name ([Dir dirName ents] ++ cp' (Dir name xs) ( (path:subpath), subtree )  ) 
cp (Dir name ( ( File fileName fps ): xs ) ) ([path], subtree)          | fileName == path = error "Path invalid, cannot copy into a file"
                                                                        | otherwise        = Dir name ([File fileName fps] ++ cp' (Dir name xs) ([path], subtree)  )
cp (Dir name ( ( File fileName fps ): xs ) ) ((path:subpath), subtree)  | fileName == path = error "Path invalid, cannot cd into a file"
                                                                        | otherwise        = Dir name ([File fileName fps] ++ cp' (Dir name xs) ( (path:subpath), subtree )  )         
cp' :: Entry -> (Path, Entry) -> [Entry]
cp' (Dir name []) (path, _)                                                                = []
cp' (Dir name entries) ([], subtree)                                                       = [Dir name (subtree:entries) ]
cp' (Dir name ( ( Dir dirName ents ): xs ) ) ([path],subtree)           | dirName == path  = ( Dir dirName (subtree:ents) ) :xs
                                                                        | otherwise        = [Dir dirName ents] ++ cp' (Dir name xs) ([path],subtree) 
cp' (Dir name ( ( Dir dirName ents ): xs ) ) ( (path:subpath),subtree ) | dirName == path  = ( cp ( Dir dirName ents ) (subpath, subtree) )  : xs 
                                                                        | otherwise        = [(Dir dirName ents )] ++ cp' (Dir name xs) ( (path:subpath), subtree)
cp' (Dir name ( ( File fileName fps ): xs ) ) ([path],subtree)          | fileName == path = (File fileName fps):xs
                                                                        | otherwise        = [File fileName fps] ++ cp' (Dir name xs) ([path], subtree) 
cp' (Dir name ( ( File fileName fps ): xs ) ) ( (path:subpath),subtree )| fileName == path = (File fileName fps) : xs
                                                                        | otherwise        = [File fileName fps] ++ cp' (Dir name xs) ( (path:subpath), subtree)    

-- Exercise, medium. Given a tree and a path, remove the file or
-- directory at that path.
--
-- You can assume that there is a file or directory at that path. If
-- there are multiple files or directories with that path, you need to
-- remove all of them.
--
-- (In that the case, the tree would not be "valid" according to isValid.)

rm :: Entry -> Path -> Entry
rm (Dir name entries) []                                                        = Dir name entries
rm (Dir name ( ( Dir dirName ents ): xs ) ) [path]           | dirName == path  = if xs == [] then Dir name xs
                                                                                  else  Dir name (rm' (Dir name xs) [path])
                                                             | otherwise        = Dir name (Dir dirName ents : rm' (Dir name xs) [path]  )
rm (Dir name ( ( Dir dirName ents ): xs ) ) (path:subpath)   | dirName == path  = Dir name ( ( rm ( Dir dirName ents ) subpath ) : xs )
                                                             | otherwise        = Dir name ((Dir dirName ents ) : rm' (Dir name xs) (path:subpath))
rm (Dir name ( ( File fileName fps ): xs ) ) [path]          | fileName == path = if xs == [] then Dir name xs
                                                                                  else  Dir name (rm' (Dir name xs) [path])
                                                             | otherwise        = Dir name (File fileName fps : rm' (Dir name xs) [path]  )
rm (Dir name ( ( File fileName fps ): xs ) ) (path:subpath)  | fileName == path = error "Path doesn't exist in the directory"
                                                             | otherwise        = Dir name (File fileName fps : rm' (Dir name xs) (path:subpath))         


rm' :: Entry -> Path -> [Entry]
rm' (Dir name entries) []                                                      = [Dir name entries]
rm' (Dir name []) [path]                                                       = []                          
rm' (Dir name ( ( Dir dirName ents ): xs ) ) [path]         | dirName == path  = if xs == [] then []
                                                                                 else (rm' (Dir name xs) [path]) 
                                                            | otherwise        = [Dir dirName ents] ++ rm' (Dir name xs) [path] 
rm' (Dir name ( ( Dir dirName ents ): xs ) ) (path:subpath) | dirName == path  =  (rm ( Dir dirName ents ) subpath)  : xs 
                                                            | otherwise        = [(Dir dirName ents )] ++ rm' (Dir name xs) (path:subpath)
rm' (Dir name ( ( File fileName fps ): xs ) ) [path]        | fileName == path = if xs == [] then []
                                                                                  else (rm' (Dir name xs) [path] ) 
                                                            | otherwise        = [File fileName fps] ++ rm' (Dir name xs) [path]  
rm' (Dir name ( ( File fileName fps ): xs ) ) (path:subpath)| fileName == path = error "Path doesn't exist in the directory"
                                                            | otherwise        = [File fileName fps] ++ rm' (Dir name xs) (path:subpath)                                                

-- Exercise, harder. Return a tree with all the same entries, but so
-- that the entries of each (sub)directory are in sorted order.
--
-- You may use the function `sort` from the Prelude.
--
-- If there are multiple entries with the same name in a directory,
-- you may choose any order.

sortTree :: Entry -> Entry
sortTree (Dir name []) = Dir name []
sortTree file@(File _ _) = file
sortTree (Dir name entries) = Dir name ( sortEntries (map sortTree entries) )

--sorts a list of entries by, extracting the names of all the entries, sorting the names, and returning the files and directories with those names.
sortEntries :: [Entry] -> [Entry]
sortEntries entries = sortOn getName entries 

getName :: Entry -> EntryName
getName (File fileName _) = fileName
getName (Dir name _)      = name

-- Exercise, unassessed. Change all letters to upper case in a string.
--
-- For instance,
--
--     upcaseStr "!someString123" = "!SOMESTRING123"
--
-- Hint: look at the definition of the String type in the Prelude, and think
-- about functions related to that type.
--
-- You may use the function upcaseChar, defined below.

upcaseStr :: String -> String
upcaseStr s = [upcaseChar x|x<-s]

upcaseChar :: Char -> Char
upcaseChar c =
    if ('a' <= c && c <= 'z')
    then toEnum (fromEnum c + fromEnum 'A' - fromEnum 'a')
    else c

-- Exercise, harder. Change all the file names (as above) and their
-- properties, similar to the above exercise.
--
-- From the type of modifyEntries, you can see what the input of
-- fileMap must be.

modifyEntries :: Entry -> ((EntryName, FileProp) -> (EntryName, FileProp)) -> Entry
modifyEntries (File fileName fps) fileMap                       = File (fst $ alteredFile) (snd $ alteredFile) where alteredFile = fileMap (fileName, fps)
modifyEntries directory@(Dir _ []) _                            = directory
modifyEntries (Dir name ( ( Dir dirName ents ): xs ) )  fileMap = Dir name ( (modifyEntries (Dir dirName ents) fileMap) : modifyEntries' (Dir name xs) fileMap )  
modifyEntries (Dir name ( ( File fileName fps ): xs ) ) fileMap = Dir name ( (File (fst $ fileMap (fileName, fps) ) (snd $ fileMap (fileName, fps) ) ) : modifyEntries' (Dir name xs) fileMap )

modifyEntries' :: Entry -> ((EntryName, FileProp) -> (EntryName, FileProp)) -> [Entry]
modifyEntries' (Dir _ []) _                                      = []
modifyEntries' (Dir name ( ( Dir dirName ents ): xs ) )  fileMap = (modifyEntries (Dir dirName ents) fileMap) : modifyEntries' (Dir name xs) fileMap  
modifyEntries' (Dir name ( ( File fileName fps ): xs ) ) fileMap = (File (fst $ fileMap (fileName, fps) ) (snd $ fileMap (fileName, fps) ) ) : modifyEntries' (Dir name xs) fileMap 

fileMap :: (EntryName, FileProp) -> (EntryName, FileProp)
fileMap (name, fileProp) = (upcaseStr name, fileProp)



-- Exercise, unassessed. Create a "Fibonacci tree".
--
-- The Fibonacci tree for n=3 looks like this:
--
--     dir3
--     |   
--     |-- dir2
--     |   |
--     |   |-- dir1
--     |   |   |
--     |   |   |-- file, size 0, time 0, content 0
--     |   |   
--     |   |
--     |   |-- file, size 0, time 0, content 0
--     |
--     |-- dir1
--     |   |
--     |   |-- file, size 0, time 0, content 0
--
--
--
-- The Fibonacci tree (fibCreate 0) is a file with name "file", size and time
-- 0, and content "". For n >= 1, fibCreate n is a directory named "dir{n}",
-- containing precisely fibCreate (n-1) and fibCreate (n-2). Exception:
-- fibCreate 1 contains only fibCreate 0, and not fibCreate (-1).
--
-- (We just made up the concept of Fibonacci trees, it is not a real thing.)

fibCreate :: Int -> Entry
fibCreate maxLevel = undefined

-- Exercise, unassessed. Make the following infinite tree:
--
--     all
--     |
--     |-- file, size 0, time 0, content 0
--     |
--     |-- dir1
--     |   |
--     |   |-- file, size 0, time 0, content 0
--     |
--     |-- dir2
--     |   |
--     |   |-- dir1
--     |   |   |
--     |   |   |-- file, size 0, time 0, content 0
--     |
--     |-- dir3
--     |   |
--     |   |-- dir2
--     |   |   |
--     |   |   |-- dir1
--     |   |   |   |
--     |   |   |   |-- file, size 0, time 0, content 0
--     |   |
--     |   |-- dir1
--     |   |   |
--     |   |   |-- file, size 0, time 0, content 0
--     |
--     |-- dir4
--     |   |
--     |   (and so on)
--     |
--     | ...
--
--
-- It is to be expected that computations such as (size fibEntry) will
-- not return a result and loop for ever (or until we run out of
-- memory). But you can still e.g. "cd" into such a tree.

fibEntry :: Entry
fibEntry = undefined

-- Exercise, unassessed. Remove from a tree all files that are larger
-- than a certain size. You should not remove any directories.
--
-- Files that are exactly that size should be kept in.

findSmallerThan :: Entry -> Int -> Entry
findSmallerThan root maxSize = case root of
  Dir name entries -> Dir name entries''
    where 
      pred (File name (FP size content time) ) = size < maxSize
      pred _ =  True
      entries' = filter pred entries
      entries'' = map mapper entries'
      mapper entry = findSmallerThan entry maxSize
  _ -> root 


{-find :: Entry -> ((EntryName, FileProp) -> Bool) -> Entry
find root predicate = case root of
  file@(File fileName fps)-> predicate (fileName, fps)   
  Dir name entries -> Dir name entries''
    where 
      pred (File fileName fps) = predicate (fileName, fps)
      pred _ =  True
      entries' = filter pred entries
      entries'' = map mapper entries'
      mapper entry = if entry ==File then find entry predicate-}


-- Exercise, challenge. Remove from a tree all files that do not
-- satisfy a given predicate. You should not remove any directories.

{-find :: Entry -> ((EntryName, FileProp) -> Bool) -> Entry
find (Dir name ( (File fileName fps):xs ) ) predicate | predicate (fileName, fps) = File fileName fps
                                   | otherwise                 = -}
  

find :: Entry -> ((EntryName, FileProp) -> Bool) -> Entry
find (Dir name []) _ = Dir name []
find (Dir name ( directory@(Dir dirName entries) : xs) ) predicate = Dir name ( (find directory predicate) : find' (Dir name xs) predicate )
find (Dir name ( file@(File fileName fps) : xs) ) predicate | predicate (fileName, fps) = Dir name (file: find' (Dir name xs) predicate)
                                                            | otherwise                 = Dir name (find' (Dir name xs) predicate)

find' :: Entry -> ((EntryName, FileProp) -> Bool) -> [Entry]
find' (Dir name []) _ = []
find' (Dir name ( directory@(Dir dirName entries) : xs) ) predicate = ( (find directory predicate) : (find' (Dir name xs) predicate) )
find' (Dir name ( file@(File fileName fps) : xs) ) predicate | predicate (fileName, fps) = (file:find' (Dir name xs) predicate)
                                                             | otherwise                 = find' (Dir name xs) predicate
-- Exercise, unassessed. Given a maximum file size, a file name and its file
-- properties, return whether the file is at most that size.
--
-- (This function gets a lot of information that it doesn't need; the
-- extra arguments are so that you can easily use `findSmallerThanPred
-- maxSize` as the predicate argument to `find`, in the next
-- exercise.)

findSmallerThanPred :: Int -> ((EntryName, FileProp) -> Bool)
findSmallerThanPred maxSize (filename, props) = undefined

-- Exercise, unassessed. Same as findSmallerThan, but implement it again
-- using `find` and `findSmallerThanPred`.

--use filter

findSmallerThan2 :: Entry -> Int -> Entry
findSmallerThan2 root maxSize = case root of
  Dir name entries -> Dir name entries''
    where 
      pred (File name (FP size content time) ) = size < maxSize
      pred _ =  True
      entries' = filter pred entries
      entries'' = map mapper entries'
      mapper entry = findSmallerThan2 entry maxSize
  _ -> root 

-- Exercise, challenge, assessed.
--
-- List all directory and file names in the current directory in a
-- table. The table can be at most termWidth cells wide. You need to
-- use as few rows as possible, while separating columns with 2
-- spaces.
--
-- (This is similar to the Unix utility 'ls'.)
-- 
-- For instance, for terminal width 80, you might have the following
-- output:
--
--     a  d  g  j                zbcdefghijklmnc  zbcdefghijklmnf  zbcdefghijklmni
--     b  e  h  zbcdefghijklmna  zbcdefghijklmnd  zbcdefghijklmng
--     c  f  i  zbcdefghijklmnb  zbcdefghijklmne  zbcdefghijklmnh
--
--
-- The ordering is alphabetical by column, and the columns should be
-- indented as above.  You can assume that the longest directory/file
-- name is at most as long as the terminal is wide.
--
-- The first argument is the terminal width:

ls :: Int -> Entry -> String
ls termWidth root = undefined

-- End of exercise set 2.

