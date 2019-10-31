import Prelude hiding (lookup)
import Data.List hiding (insert, lookup)
import System.IO.Error
import Control.Exception
import Data.Either

data QA key p = Empty
                 | Node (QA key p) (key, p) (QA key p)
                 --Invariant: Yes-Left-0, No-Right-1
                 deriving (Show, Read)

empty :: QA key p
empty = Empty

-- Inserts a new (key, p) at position key in the tree
insert :: [Int] -> String -> String -> QA [Int] String -> QA [Int] String
insert key q p t = insert' key key q p t

insert' key' key q p (Node Empty (k',p') Empty) 
    = Node (Node Empty (k'++[0],p) Empty) (key, q) (Node Empty (k'++[1],p') Empty)

insert' (x:xs) key q p (Node tl (k',p') tr) | x == 0    = Node (insert' xs key q p tl) (k',p') tr
                                            | otherwise = Node tl (k',p') (insert' xs key q p tr)

-- Writes t to file
toFile t = writeFile "famous.qa" (show t)

-- Asks the question q and returns the answer string
question :: String -> IO String
question q = do
    putStr (q ++ " ")
    ans <- getLine
    return ans

-- Asks the question q, then returns True or False depending on the answer
-- Repeats the question if no correct answer is given
yesNoQuestion :: String -> IO Bool
yesNoQuestion q = do
    s <- question q
    if s == "yes"
        then do
            return True
        else if s == "no"
            then do return False
        else do
            putStrLn("Let me repeat the question:")
            yesNoQuestion q

-- Returns the computer's guess
play :: QA [Int] String -> IO (QA [Int] String)
play (Node Empty (key, p) Empty) = do
    let t1 = Node Empty (key, p) Empty
    ans <- yesNoQuestion ("My guess: Is it " ++ p ++ "?")
    if ans
        then do
            putStrLn("Yay! I won!")
            return t1
        else do
            newPerson <- question "Just curious: Who was your famous person?"
            newQuestion <- question ("Give me a question for which the answer for " 
                                    ++ newPerson
                                    ++ " is \"yes\" and the answer for "
                                    ++ p
                                    ++ " is \"no\".")
            return (insert key newQuestion newPerson t1)
            

-- Plays the game
play (Node tl (key, p) tr) = do
    b <- yesNoQuestion p
    if b
        then do
            left <- play tl
            return (Node left (key, p) tr)
        else do
            right <- play tr
            return (Node tl (key, p) right)

-- Runs the game
main :: IO()
main = do
    -- Checks if the file exists
    r <- tryIOError (readFile "famous.qa")
    case r of
        (Right contents) -> do
            -- Reads the file and plays the game
            let s = read contents :: QA [Int] String
            newQA <- play s
            toFile newQA
            putStrLn "Game Over"
        (Left error) -> do
            -- Playing the base case
            putStrLn "Could not find file\nPlaying Base Case"
            let s = baseCase
            newQA <- play s
            toFile newQA
            putStrLn "Game Over"

baseCase = Node Empty ([],"Marie Curie") Empty
