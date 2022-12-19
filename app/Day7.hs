{-# LANGUAGE LambdaCase #-}
module Day7 (day7) where
import Part (Part (Part1, Part2))

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
buildTree = go [] (FsTree "/" (FsTreeDir []))
  where go :: [String] -> FsTree -> [TerminalCommand] -> FsTree
        go cwd tree (command:commands) = let (cwd', tree') = runCommand cwd tree command
                                          in go cwd' tree' commands
        go cwd tree [] = tree
        runCommand :: [String] -> FsTree -> TerminalCommand -> ([String], FsTree)
        runCommand cwd tree (TerminalCommandCd "/") = ([], tree)
        runCommand cwd tree (TerminalCommandCd "..") = (tail cwd, tree)
        runCommand cwd tree (TerminalCommandCd filename) = (filename:cwd, tree)
        runCommand cwd tree (TerminalCommandLs contents) = (cwd, foldr (flip (addContents (reverse cwd))) tree contents)
        addContents :: [String] -> FsTree -> LsContent -> FsTree
        addContents [] tree content = addContent tree content
        addContents (p:ps) (FsTree here (FsTreeDir children)) content = let (theseOnes, otherOnes) = partition ((== p) . fsTreeName) children
                                                                            insertInto = case theseOnes of
                                                                                           [thisOne] -> thisOne
                                                                                           [] -> FsTree p (FsTreeDir [])
                                                                         in FsTree here (FsTreeDir ((addContents ps insertInto content):otherOnes))
        addContent (FsTree here (FsTreeDir currentChildren)) content = let newEntry = case content of
                                                                                        LsContentFile filename size -> FsTree filename (FsTreeFile size)
                                                                                        LsContentDir filename -> FsTree filename (FsTreeDir [])
                                                                        in FsTree here (FsTreeDir $ addChild newEntry currentChildren)
        addChild newEntry@(FsTree newEntryName _) currentChildren | elem newEntryName (fsTreeName <$> currentChildren) = currentChildren
                                                                  | otherwise                                          = newEntry:currentChildren


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
