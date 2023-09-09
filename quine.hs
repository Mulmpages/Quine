module Main where

import System.Process (readProcess, callCommand)
import System.FilePath (dropExtension, addExtension)
import Data.List (isInfixOf, elemIndex, findIndices, intercalate)
import System.Environment (getArgs)

main :: IO ()
main = do
    args0 <- getArgs
    let args = proc0 args0
    if null args0
        then do
            putStr x0
            putStr $ "\"" ++ concatMap lit0 x0 ++ "\""
        else do

        if null args
            then info
            else case args !! 0 of
                "check" -> do
                    res <- checkQuine (args !! 1)
                    case res of
                        True -> putStrLn "Oh Baby, a Quine!"
                        _    -> putStrLn "Oh no, not quite Quine :("
                "make" -> do
                    _ <- makeQuine (args !! 1) (args !! 2)
                    putStrLn "Quintessence: Quite queerly quined!"
                _ -> do info

info :: IO ()
info = do
    putStrLn ""
    putStrLn "Wrong arguments. Argument format: <command> <arg1> [<arg2>]."
    putStrLn "Commands are \"make FileIn FileOut\" and \"check FileIn\"."
    putStrLn "Works on Haskell Source Files. The File needs to have this structure:"
    putStrLn ""
    putStrLn "> ..."
    putStrLn "> main = do"
    putStrLn ">      args <- getArgs"
    putStrLn "> ..."
    putStrLn ""

toX :: String -> String
toX inn = "\"" ++ concatMap lit0 inn ++ "\""

fileToX :: FilePath -> IO String
fileToX path = do
    inn <- readFile path
    let innStripped = init (init inn)
    return $ toX innStripped

isFree :: String -> String -> Bool
isFree name str = not $ name `isInfixOf` str

findFreeNameLoop :: Int -> String -> String -> String
findFreeNameLoop n pre str = 
    let name = pre ++ show n in
        if isFree name str
            then name
            else findFreeNameLoop (n + 1) pre str

findFreeName :: String -> String -> String
findFreeName = findFreeNameLoop 0

findSubList :: Eq a => [a] -> [a] -> Int
findSubList = f 0
    where
        f :: Eq a => Int -> [a] -> [a] -> Int
        f n toFind xs =
            if toFind == take (length toFind) (drop n xs)
                then n
                else f (n + 1) toFind xs

--Findet das erste Vorkommen von "args <- getArgs"
findBeginning :: String -> Int
findBeginning = findSubList "args <- getArgs"

indent :: [String] -> [String]
indent [] = []
indent ([]:ss) = [] : indent ss
indent (s:ss) = 
    if head s `elem` [' ', '\t', '\n']
        then ("        " ++ s) : indent ss
        else s:ss

getIndentedPost :: String -> String
getIndentedPost str = unlines $ indent (lines str)

makeNewStr :: String -> String -> String -> String -> String -> String -> String
makeNewStr nameX nameArgs nameProc nameLit pre post =
       pre ++ nameArgs ++ " <- getArgs\n"
    ++ "    let args = " ++ nameProc ++ " " ++ nameArgs ++ "\n"
    ++ "    if null " ++ nameArgs ++ "\n"
    ++ "        then do\n"
    ++ "            putStr " ++ nameX ++ "\n"
    ++ "            putStr $ \"\\\"\" ++ concatMap " ++ nameLit ++ " " ++ nameX ++ " ++ \"\\\"\"\n"
    ++ "        else do\n"
    ++ post ++ "\n"
    ++ "\n"
    ++ nameLit ++ " :: Char -> String\n"
    ++ nameLit ++ " '\\a' = \"\\\\a\"\n"
    ++ nameLit ++ " '\\b' = \"\\\\b\"\n"
    ++ nameLit ++ " '\\f' = \"\\\\f\"\n"
    ++ nameLit ++ " '\\n' = \"\\\\n\\\\\\n\\\\\"\n"
    ++ nameLit ++ " '\\r' = \"\\\\r\"\n"
    ++ nameLit ++ " '\\t' = \"\\\\t\"\n"
    ++ nameLit ++ " '\\v' = \"\\\\v\"\n"
    ++ nameLit ++ " '\\\"' = \"\\\\\\\"\"\n"
    ++ nameLit ++ " '\\'' = \"\\\\\\'\"\n"
    ++ nameLit ++ " '\\\\' = \"\\\\\\\\\"\n"
    ++ nameLit ++ " c = [c]\n"
    ++ "\n"
    ++ nameProc ++ " :: [String] -> [String]\n"
    ++ nameProc ++ " [] = []\n"
    ++ nameProc ++ " [\"@\"] = []\n"
    ++ nameProc ++ " (s:ss) =\n"
    ++ "    if head s == '@'\n"
    ++ "        then tail s : ss\n"
    ++ "        else s : ss\n"
    ++ "\n"
    ++ nameX ++ " :: String\n"
    ++ nameX ++ " = "

-- Funktioniert nur, wenn das Programm folgendermaen beginnt:
-- ...
-- main = do
--     args <- getArgs
--     ...
makeQuine :: FilePath -> FilePath -> IO String
makeQuine pathIn pathOut = do
    str <- readFile pathIn
    let nameX = findFreeName "x" str
    let nameArgs = findFreeName "args" str
    let nameProc = findFreeName "proc" str
    let nameLit = findFreeName "lit" str
    let (pre, middleAndPost) = splitAt (findBeginning str) str
    let (middle, post) = splitAt 15 middleAndPost
    let newStr = makeNewStr nameX nameArgs nameProc nameLit pre (getIndentedPost post)
    let finalStr = newStr ++ toX newStr
    writeFile pathOut finalStr
    return finalStr

compile :: FilePath -> IO ()
compile path = callCommand ("ghc " ++ path)

run :: FilePath -> IO String
run path = readProcess path [] []

checkQuine :: FilePath -> IO Bool
checkQuine path = do
    source <- readFile path
    compile path
    let pathEXE = addExtension (dropExtension path) ".exe"
    output <- run pathEXE
    return (source == output)


lit0 :: Char -> String
lit0 '\a' = "\\a"
lit0 '\b' = "\\b"
lit0 '\f' = "\\f"
lit0 '\n' = "\\n\\\n\\"
lit0 '\r' = "\\r"
lit0 '\t' = "\\t"
lit0 '\v' = "\\v"
lit0 '\"' = "\\\""
lit0 '\'' = "\\\'"
lit0 '\\' = "\\\\"
lit0 c = [c]

proc0 :: [String] -> [String]
proc0 [] = []
proc0 ["@"] = []
proc0 (s:ss) =
    if head s == '@'
        then tail s : ss
        else s : ss

x0 :: String
x0 = "module Main where\n\
\\n\
\import System.Process (readProcess, callCommand)\n\
\import System.FilePath (dropExtension, addExtension)\n\
\import Data.List (isInfixOf, elemIndex, findIndices, intercalate)\n\
\import System.Environment (getArgs)\n\
\\n\
\main :: IO ()\n\
\main = do\n\
\    args0 <- getArgs\n\
\    let args = proc0 args0\n\
\    if null args0\n\
\        then do\n\
\            putStr x0\n\
\            putStr $ \"\\\"\" ++ concatMap lit0 x0 ++ \"\\\"\"\n\
\        else do\n\
\\n\
\        if null args\n\
\            then info\n\
\            else case args !! 0 of\n\
\                \"check\" -> do\n\
\                    res <- checkQuine (args !! 1)\n\
\                    case res of\n\
\                        True -> putStrLn \"Oh Baby, a Quine!\"\n\
\                        _    -> putStrLn \"Oh no, not quite Quine :(\"\n\
\                \"make\" -> do\n\
\                    _ <- makeQuine (args !! 1) (args !! 2)\n\
\                    putStrLn \"Quintessence: Quite queerly quined!\"\n\
\                _ -> do info\n\
\\n\
\info :: IO ()\n\
\info = do\n\
\    putStrLn \"\"\n\
\    putStrLn \"Wrong arguments. Argument format: <command> <arg1> [<arg2>].\"\n\
\    putStrLn \"Commands are \\\"make FileIn FileOut\\\" and \\\"check FileIn\\\".\"\n\
\    putStrLn \"Works on Haskell Source Files. The File needs to have this structure:\"\n\
\    putStrLn \"\"\n\
\    putStrLn \"> ...\"\n\
\    putStrLn \"> main = do\"\n\
\    putStrLn \">      args <- getArgs\"\n\
\    putStrLn \"> ...\"\n\
\    putStrLn \"\"\n\
\\n\
\toX :: String -> String\n\
\toX inn = \"\\\"\" ++ concatMap lit0 inn ++ \"\\\"\"\n\
\\n\
\fileToX :: FilePath -> IO String\n\
\fileToX path = do\n\
\    inn <- readFile path\n\
\    let innStripped = init (init inn)\n\
\    return $ toX innStripped\n\
\\n\
\isFree :: String -> String -> Bool\n\
\isFree name str = not $ name `isInfixOf` str\n\
\\n\
\findFreeNameLoop :: Int -> String -> String -> String\n\
\findFreeNameLoop n pre str = \n\
\    let name = pre ++ show n in\n\
\        if isFree name str\n\
\            then name\n\
\            else findFreeNameLoop (n + 1) pre str\n\
\\n\
\findFreeName :: String -> String -> String\n\
\findFreeName = findFreeNameLoop 0\n\
\\n\
\findSubList :: Eq a => [a] -> [a] -> Int\n\
\findSubList = f 0\n\
\    where\n\
\        f :: Eq a => Int -> [a] -> [a] -> Int\n\
\        f n toFind xs =\n\
\            if toFind == take (length toFind) (drop n xs)\n\
\                then n\n\
\                else f (n + 1) toFind xs\n\
\\n\
\--Findet das erste Vorkommen von \"args <- getArgs\"\n\
\findBeginning :: String -> Int\n\
\findBeginning = findSubList \"args <- getArgs\"\n\
\\n\
\indent :: [String] -> [String]\n\
\indent [] = []\n\
\indent ([]:ss) = [] : indent ss\n\
\indent (s:ss) = \n\
\    if head s `elem` [\' \', \'\\t\', \'\\n\']\n\
\        then (\"        \" ++ s) : indent ss\n\
\        else s:ss\n\
\\n\
\getIndentedPost :: String -> String\n\
\getIndentedPost str = unlines $ indent (lines str)\n\
\\n\
\makeNewStr :: String -> String -> String -> String -> String -> String -> String\n\
\makeNewStr nameX nameArgs nameProc nameLit pre post =\n\
\       pre ++ nameArgs ++ \" <- getArgs\\n\"\n\
\    ++ \"    let args = \" ++ nameProc ++ \" \" ++ nameArgs ++ \"\\n\"\n\
\    ++ \"    if null \" ++ nameArgs ++ \"\\n\"\n\
\    ++ \"        then do\\n\"\n\
\    ++ \"            putStr \" ++ nameX ++ \"\\n\"\n\
\    ++ \"            putStr $ \\\"\\\\\\\"\\\" ++ concatMap \" ++ nameLit ++ \" \" ++ nameX ++ \" ++ \\\"\\\\\\\"\\\"\\n\"\n\
\    ++ \"        else do\\n\"\n\
\    ++ post ++ \"\\n\"\n\
\    ++ \"\\n\"\n\
\    ++ nameLit ++ \" :: Char -> String\\n\"\n\
\    ++ nameLit ++ \" \'\\\\a\' = \\\"\\\\\\\\a\\\"\\n\"\n\
\    ++ nameLit ++ \" \'\\\\b\' = \\\"\\\\\\\\b\\\"\\n\"\n\
\    ++ nameLit ++ \" \'\\\\f\' = \\\"\\\\\\\\f\\\"\\n\"\n\
\    ++ nameLit ++ \" \'\\\\n\' = \\\"\\\\\\\\n\\\\\\\\\\\\n\\\\\\\\\\\"\\n\"\n\
\    ++ nameLit ++ \" \'\\\\r\' = \\\"\\\\\\\\r\\\"\\n\"\n\
\    ++ nameLit ++ \" \'\\\\t\' = \\\"\\\\\\\\t\\\"\\n\"\n\
\    ++ nameLit ++ \" \'\\\\v\' = \\\"\\\\\\\\v\\\"\\n\"\n\
\    ++ nameLit ++ \" \'\\\\\\\"\' = \\\"\\\\\\\\\\\\\\\"\\\"\\n\"\n\
\    ++ nameLit ++ \" \'\\\\\'\' = \\\"\\\\\\\\\\\\\'\\\"\\n\"\n\
\    ++ nameLit ++ \" \'\\\\\\\\\' = \\\"\\\\\\\\\\\\\\\\\\\"\\n\"\n\
\    ++ nameLit ++ \" c = [c]\\n\"\n\
\    ++ \"\\n\"\n\
\    ++ nameProc ++ \" :: [String] -> [String]\\n\"\n\
\    ++ nameProc ++ \" [] = []\\n\"\n\
\    ++ nameProc ++ \" [\\\"@\\\"] = []\\n\"\n\
\    ++ nameProc ++ \" (s:ss) =\\n\"\n\
\    ++ \"    if head s == \'@\'\\n\"\n\
\    ++ \"        then tail s : ss\\n\"\n\
\    ++ \"        else s : ss\\n\"\n\
\    ++ \"\\n\"\n\
\    ++ nameX ++ \" :: String\\n\"\n\
\    ++ nameX ++ \" = \"\n\
\\n\
\-- Funktioniert nur, wenn das Programm folgendermaen beginnt:\n\
\-- ...\n\
\-- main = do\n\
\--     args <- getArgs\n\
\--     ...\n\
\makeQuine :: FilePath -> FilePath -> IO String\n\
\makeQuine pathIn pathOut = do\n\
\    str <- readFile pathIn\n\
\    let nameX = findFreeName \"x\" str\n\
\    let nameArgs = findFreeName \"args\" str\n\
\    let nameProc = findFreeName \"proc\" str\n\
\    let nameLit = findFreeName \"lit\" str\n\
\    let (pre, middleAndPost) = splitAt (findBeginning str) str\n\
\    let (middle, post) = splitAt 15 middleAndPost\n\
\    let newStr = makeNewStr nameX nameArgs nameProc nameLit pre (getIndentedPost post)\n\
\    let finalStr = newStr ++ toX newStr\n\
\    writeFile pathOut finalStr\n\
\    return finalStr\n\
\\n\
\compile :: FilePath -> IO ()\n\
\compile path = callCommand (\"ghc \" ++ path)\n\
\\n\
\run :: FilePath -> IO String\n\
\run path = readProcess path [] []\n\
\\n\
\checkQuine :: FilePath -> IO Bool\n\
\checkQuine path = do\n\
\    source <- readFile path\n\
\    compile path\n\
\    let pathEXE = addExtension (dropExtension path) \".exe\"\n\
\    output <- run pathEXE\n\
\    return (source == output)\n\
\\n\
\\n\
\lit0 :: Char -> String\n\
\lit0 \'\\a\' = \"\\\\a\"\n\
\lit0 \'\\b\' = \"\\\\b\"\n\
\lit0 \'\\f\' = \"\\\\f\"\n\
\lit0 \'\\n\' = \"\\\\n\\\\\\n\\\\\"\n\
\lit0 \'\\r\' = \"\\\\r\"\n\
\lit0 \'\\t\' = \"\\\\t\"\n\
\lit0 \'\\v\' = \"\\\\v\"\n\
\lit0 \'\\\"\' = \"\\\\\\\"\"\n\
\lit0 \'\\\'\' = \"\\\\\\\'\"\n\
\lit0 \'\\\\\' = \"\\\\\\\\\"\n\
\lit0 c = [c]\n\
\\n\
\proc0 :: [String] -> [String]\n\
\proc0 [] = []\n\
\proc0 [\"@\"] = []\n\
\proc0 (s:ss) =\n\
\    if head s == \'@\'\n\
\        then tail s : ss\n\
\        else s : ss\n\
\\n\
\x0 :: String\n\
\x0 = "