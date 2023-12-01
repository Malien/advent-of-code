{-# LANGUAGE NamedFieldPuns #-}
import           Data.List  (isPrefixOf)
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (fromJust)

main = readFile "day7.txt" >>= print . process

test =
  "$ cd /\n\
  \$ ls\n\
  \dir a\n\
  \14848514 b.txt\n\
  \8504156 c.dat\n\
  \dir d\n\
  \$ cd a\n\
  \$ ls\n\
  \dir e\n\
  \29116 f\n\
  \2557 g\n\
  \62596 h.lst\n\
  \$ cd e\n\
  \$ ls\n\
  \584 i\n\
  \$ cd ..\n\
  \$ cd ..\n\
  \$ cd d\n\
  \$ ls\n\
  \4060174 j\n\
  \8033020 d.log\n\
  \5626152 d.ext\n\
  \7214296 k"

process = 
    sum 
  . filter (<100_000) 
  . map dirSize 
  . allRoots 
  . populate 
  . parseCommands
  . tail 
  . lines

data File = File String Int deriving Show

data FsItem = FsFile File | Dir String 

data Command 
  = CdUp 
  | Cd String 
  | Ls [FsItem] 

data Fs = Fs { 
  files :: [File], 
  dirs :: Map String (Maybe Fs)
  }

parseCommands [] = []
parseCommands ("$ ls"   :rest) = Ls (map parseFs output) : parseCommands left
  where (output, left) = break (isPrefixOf "$ ") rest
parseCommands ("$ cd ..":rest) = CdUp : parseCommands rest
parseCommands (cmd:rest) | "$ cd " `isPrefixOf` cmd = Cd (drop 5 cmd) : parseCommands rest

parseFs line = case first of
    "dir" -> Dir name
    size  -> FsFile $ File name (read size)
  where [first, name] = words line

populate = snd . foldl populate' ([], emptyFs)
  where populate' (path, fs) (Cd dir)   = (path ++ [dir], fs                               )
        populate' (path, fs) CdUp       = (init path    , fs                               )
        populate' (path, fs) (Ls items) = (path         , insertIn path (contents items) fs)

contents = foldr contents' emptyFs
  where contents' (FsFile file) (Fs { files, dirs }) = Fs { files = file : files, dirs }
        contents' (Dir name)    (Fs { files, dirs }) = Fs { files, dirs = Map.insert name Nothing dirs }

emptyFs = Fs { files = [], dirs = Map.empty }

insertIn :: [String] -> Fs -> Fs -> Fs
insertIn [] item _ = item
insertIn (x:xs) item (Fs { dirs, files }) = Fs {
  files,
  dirs = Map.update (Just . Just . insertIn xs item . fromJust) x dirs
  }

allRoots fs@Fs { dirs } = fs : (concatMap (allRoots . fromJust) . Map.elems $ dirs)

dirSize (Fs { files, dirs }) = sum (map fileSize files) + sum (map (dirSize . fromJust) $ Map.elems dirs)

fileSize (File _ size) = size

splitOnceWhere f [] acc = (acc, [])
splitOnceWhere f (x:xs) acc | f x = (acc, x:xs)
                            | otherwise = splitOnceWhere f xs (x:acc)

