module Main (main) where

import Lib
import Data.Char
import System.Random
import System.Environment
import Control.Exception
import Words

type Name = String
type Nouns = IO [String]
type Adjectives = IO [String]
type Input = String 

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs [] = putStrLn "You didn't give a format!\nTry \"AANDD\""
handleArgs (x:xs) = do
    res <- sequence [ dsl c (pure nouns) (pure adjectives) | c <- x ]
    putStrLn $ concat res

dsl :: Char -> Nouns -> Adjectives -> IO String
dsl 'A' _ adjs     = getRandomEntry adjs
dsl 'N' nouns _    = getRandomEntry nouns
dsl 'D' _  _    = do
    i <- getRandomIndex 10
    pure (show i :: String)
dsl _ _ _    = error "Unknown"

getRandomIndex :: Int -> IO Int
getRandomIndex size = getStdRandom (randomR (0,size-1))
 
capitalise :: String -> String
capitalise s = case intoUppercase $ head s of
                        Just c -> [c] ++ tail s
                        _      -> s

intoUppercase :: Char -> Maybe Char
intoUppercase c 
    | asNumber > 122 || asNumber < 97 = Nothing
    | otherwise                       = Just $ chr $ (asNumber) - 32
    where
        asNumber = ord c

getRandomEntry :: IO [String] -> IO String
getRandomEntry xs = do
    elements <- xs
    index <- getRandomIndex (length elements)
    pure $ capitalise $ elements !! index
