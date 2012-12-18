import Data.HashTable
import Data.Maybe
import Data.Char (toLower)
import Data.List (sort, sortBy, nub)
import Data.Function (on)
import Control.Monad (filterM)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

insertWord :: HashTable String [String] -> String -> IO ()
insertWord h v = do
    let key = sort v
    q <- Data.HashTable.lookup h key
    case q of
        Nothing     -> insert h key [v]
        Just vals   -> do update h key (v:vals)
                          return ()

getWords :: HashTable String [String] -> String -> IO [String]
getWords h k = do
    q <- Data.HashTable.lookup h (sort k)
    case q of
        Nothing     -> return []
        Just vals   -> return vals

powerSet :: [a] -> [[a]]
powerSet = filterM (const [True, False])

baseScore :: String -> Int
baseScore [] = 0
baseScore (x:xs) = baseScore xs + 
    if (any (== x) ['q', 'z']) then 10 
    else if (any (== x) ['j', 'x']) then 8
    else if (x == 'k') then 5
    else if (any (== x) ['f', 'h', 'v', 'w', 'y']) then 4
    else if (any (== x) ['b', 'c', 'm', 'p']) then 3
    else if (any (== x) ['d', 'g']) then 2
    else if (any (== x) ['e', 'a', 'i', 'o', 'n', 'r', 't', 'l', 's', 'u']) then 1
    else 0

readDict :: IO String
readDict = do
    f <- doesFileExist "/usr/dict/words"
    if f then readFile "/usr/dict/words"
    else do
        f' <- doesFileExist "/usr/share/dict/words"
        if f' then readFile "/usr/share/dict/words"
        else error "Dictionary file not found. Re-run this program with the path to a dictionary file as its first parameter."

solveWords :: HashTable String [String] -> IO ()
solveWords h = do
    letters <- getLine
    possibleWords <- mapM (getWords h) (powerSet (map toLower letters))
    putStrLn (show (reverse ((sortBy (compare `on` baseScore) (nub (concat possibleWords))))))
    solveWords h

main :: IO ()
main = do
    hashTable <- new (==) hashString
    args <- getArgs
    words <- (if (length args) > 0 then readFile (args!!0) else readDict)
    putStrLn "Loading the dictionary... This may take a couple seconds."
    mapM_ (insertWord hashTable) (lines (map toLower words))
    putStrLn "Dictionary loaded! Enter a combination of letters to solve for:"
    solveWords hashTable
