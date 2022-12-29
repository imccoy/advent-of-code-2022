{-# LANGUAGE LambdaCase #-}
module Day7 (day7) where
import Part (Part (Part1, Part2))

import Data.Maybe (fromMaybe)
import Data.List (partition)

data TerminalCommand = TerminalCommandCd String | TerminalCommandLs [LsContent]
  deriving (Show)
data LsContent = LsContentDir String | LsContentFile String Int
  deriving (Show)

data FsTree = FsTree String FsTreeEntry
  deriving (Show)
data FsTreeEntry = FsTreeDir [FsTree] | FsTreeFile Int
  deriving (Show)

fsTreeName (FsTree name _) = name

type Parsed = [TerminalCommand]

parseInput :: String -> Parsed
parseInput = parseCommands . lines

parseCommands (head:rest) | head == "$ ls" = let (ls, rest') = parseLsCommand rest
                                              in ls:(parseCommands rest')
                          | otherwise      = (parseCdCommand head):(parseCommands rest)
parseCommands [] = []

parseCdCommand ('$':' ':'c':'d':' ':dirname) = TerminalCommandCd dirname

parseLsCommand lines = let go contents (rest@(('$':_):_)) = (contents, rest)
                           go contents [] = (contents, [])
                           go contents (here:rest) = let (first,second) = span (/= ' ') here
                                                         filename = drop 1 second
                                                         entry = if first == "dir"
                                                                   then LsContentDir filename
                                                                   else LsContentFile filename (read first)
                                                      in go (entry:contents) rest
                           (contents, rest) = go [] lines
                        in (TerminalCommandLs contents, rest)

buildTree :: [TerminalCommand] -> FsTree
buildTree = go [(FsTree "/" (FsTreeDir []))]
  where go :: [FsTree] -> [TerminalCommand] -> FsTree
        go cwd (command:commands) = let cwd' = runCommand cwd command
                                     in go cwd' commands
        go [tree@(FsTree "/" _)] [] = tree
        go cwd [] = go cwd [TerminalCommandCd "/"]

        runCommand :: [FsTree] -> TerminalCommand -> [FsTree]
        runCommand tree@[FsTree "/" _] (TerminalCommandCd "/") = tree
        runCommand cwd (TerminalCommandCd "/")
          = runCommand (runCommand cwd (TerminalCommandCd "..")) (TerminalCommandCd "/")

        runCommand (innerTree:(FsTree outerDirName (FsTreeDir outerDir)):cwd) (TerminalCommandCd "..")
          = (FsTree outerDirName $ FsTreeDir $ addTreeEntry innerTree outerDir (\_ new -> new)):cwd
        runCommand (cwd@((FsTree _ (FsTreeDir dir)):_)) (TerminalCommandCd filename)
          = let (newDir, _) = findTreeEntry filename dir
             in (fromMaybe (FsTree filename $ FsTreeDir []) newDir):cwd

        runCommand ((FsTree dirName (FsTreeDir dir)):cwd) (TerminalCommandLs contents)
          = (FsTree dirName $ FsTreeDir (foldr (\lsContent dir' -> addLsEntry lsContent dir') dir contents)):cwd

        findTreeEntry :: String -> [FsTree] -> (Maybe FsTree, [FsTree])
        findTreeEntry name [] = (Nothing, [])
        findTreeEntry name (entry@(FsTree name' _):dir)
          | name == name' = (Just entry, dir)
          | otherwise     = let (found, dir') = findTreeEntry name dir
                             in (found, entry:dir')
        addTreeEntry new@(FsTree newName existing) dir f = let (existing, dir') = findTreeEntry newName dir
                                                            in (f existing new):dir'
        addLsEntry :: LsContent -> [FsTree] -> [FsTree]
        addLsEntry lsContent dir = addTreeEntry (case lsContent of
                                                   LsContentDir name -> FsTree name $ FsTreeDir []
                                                   LsContentFile name size -> FsTree name $ FsTreeFile size)
                                                dir
                                                (\existing new -> fromMaybe new existing)

printTree :: FsTree -> IO ()
printTree = go 0
  where go indent (FsTree name (FsTreeDir children)) = do putStrLn $ replicate (indent*2) ' ' ++ name ++ "(dir)"
                                                          mapM_ (go (indent+1)) children
        go indent (FsTree name (FsTreeFile size)) = do putStrLn $ replicate (indent*2) ' ' ++ name ++ "(file)"

findSizes :: FsTree -> (Int, [Int])
findSizes (FsTree _ (FsTreeDir children)) = let childSizes = findSizes <$> children
                                                mySize = sum (fst <$> childSizes)
                                             in (mySize, mySize:(concat (snd <$> childSizes)))
findSizes (FsTree _ (FsTreeFile size)) = (size, [])

part1 :: Parsed -> IO ()
part1 input = do printTree . buildTree $ input
                 let sizes = findSizes . buildTree $ input
                 putStrLn $ show sizes
                 putStrLn . show . sum . filter (<= 100000) . snd $ sizes
                 

part2 :: Parsed -> IO ()
part2 input = do let (totalUsedSpace,dirSizes) = findSizes . buildTree $ input
                 let fsSize = 70000000
                 let spaceRequired = 30000000
                 let currentlyUnused = fsSize - totalUsedSpace
                 let freeSpaceRequired = spaceRequired - currentlyUnused
                 putStrLn $ show currentlyUnused
                 putStrLn $ show freeSpaceRequired
                 putStrLn . show . minimum . filter (>= freeSpaceRequired) $ dirSizes

day7 part args = do let filename = case args of
                                      [] -> "inputs/day7"
                                      [f] -> f
                    inputs <- parseInput <$> readFile filename
                    case part of
                      Part1 -> part1 inputs
                      Part2 -> part2 inputs
