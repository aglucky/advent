{-
Advent of Code Day 1
Adam Gluck
-}

import System.IO ( hGetContents, openFile, IOMode(ReadMode) )
import Data.Maybe (fromMaybe)
import Data.Char (toLower)

readInputFile :: FilePath -> IO String
readInputFile filePath = do
    handle <- openFile filePath ReadMode
    hGetContents handle

extractNumbers :: String -> Int
extractNumbers input = read [head digits, last digits]
  where digits = filter (`elem` ['0'..'9']) input

solveFirstPart :: String -> Int
solveFirstPart = sum . map extractNumbers . lines

getNumOrLetter :: String -> Char
getNumOrLetter input =
    let numMap = [("one", '1'), ("two", '2'), ("three", '3'), ("four", '4'), ("five", '5'), 
                  ("six", '6'), ("seven", '7'), ("eight", '8'), ("nine", '9'), ("zero", '0')]
        findMatch [] = Nothing
        findMatch ((str, num):xs) =
            if map toLower (take (length str) input) == str
            then Just num
            else findMatch xs
    in fromMaybe (head input) (findMatch numMap)

convertNumbersInString :: String -> String
convertNumbersInString "" = ""
convertNumbersInString word = getNumOrLetter word : convertNumbersInString (tail word)

solveNextPart :: String -> Int
solveNextPart = sum . map (extractNumbers . convertNumbersInString) . lines

main :: IO ()
main = do
    contents <- readInputFile "input.txt"
    putStrLn $ "First Part Solution: " ++ show (solveFirstPart contents)
    putStrLn $ "Second Part Solution: " ++ show (solveNextPart contents)
