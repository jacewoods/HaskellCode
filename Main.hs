{-
    CS 360 lab 4
    Emmet syntax HTML generator.
    Starting code
    Jace Woods
-}

module Main
(
main
) where

import System.IO (hFlush, stdout)
import Data.Char (ord, digitToInt)
import Data.List (elemIndex)


process :: String -> String
process text
    | any (\x -> x == '*') text == True = multipleLines text
    | any (\x -> x == '.') text == True && checkSymbols (fst (splitter '.' text) ) == False 
      && checkSymbols (tail $ snd $ splitter '.' text ) == False && elemIndex '.' text /= Just 0 && (tail $ snd $ splitter '.' text) /= "" =  
      "<" ++ fst (splitter '.' text) ++ " class=\"" ++ tail (snd (splitter '.' text)) ++ "\"></" ++ fst (splitter '.' text) ++ ">"
    | any (\x -> x == '#') text == True && checkSymbols (fst (splitter '#' text) ) == False 
      && checkSymbols (tail $ snd $ splitter '#' text ) == False && elemIndex '#' text /= Just 0 && (tail $ snd $ splitter '#' text) /= "" = 
      "<" ++ fst (splitter '#' text) ++ " id=\"" ++ tail (snd (splitter '#' text)) ++ "\"></" ++ fst (splitter '#' text) ++ ">" 
    | any (\x -> x == '>') text == True && checkSymbols (fst (splitter '>' text) ) == False
      && checkSymbols (tail $ snd $ splitter '>' text ) == False && elemIndex '>' text /= Just 0 && (tail $ snd $ splitter '>' text) /= "" =
      "<" ++ fst (splitter '>' text) ++ "><" ++ tail (snd (splitter '>' text)) ++ "></" ++ tail (snd (splitter '>' text)) ++ "></" ++ fst (splitter '>' text) ++ ">"
    | checkSymbols text == False = ("<" ++ text ++ "></" ++ text ++ ">")
    | otherwise = "ERROR: Not Valid Type"

--This function works with Question 2 HTML and numbers 0 - 9
multipleLines :: String -> String
multipleLines text
    | checkSymbols (fst (splitter '*' text) ) == False && checkSymbols (tail $ snd $ splitter '*' text ) == False && elemIndex '*' text /= Just 0 
      && (tail $ snd $ splitter '*' text) /= "" && all (\x -> ord x > 47 && ord x < 58) (tail $ snd $ splitter '*' text) == True = 
      multiplier ("<" ++ fst (splitter '*' text) ++ "></" ++ fst (splitter '*' text) ++ ">" ) (digitToInt (last (snd (splitter '*' text)) ) )
    | otherwise = "ERROR: Not Valid Type"

checkSymbols :: Foldable t => t Char -> Bool
checkSymbols = any (\x -> ord x < 48 || ord x > 122 || (ord x > 57 && ord x < 65) || (ord x > 90 && ord x < 97))

indexLoc :: Eq a => a -> [a] -> Int
indexLoc x xs = maybe 0 (+0) (elemIndex x xs)

splitter x xs = splitAt (indexLoc x xs) xs

multiplier :: String -> Int -> String
multiplier string n = concat (replicate n (string ++ "\n") )


{-
    The main entry point function.  Interactively expand Emmet syntax
    abbreviations and generate HTML skeleton code.  Prints HTML to standard
    output.

    Enter an empty line to quit.
-}
main :: IO ()
main = do
  putStrLn "Type Emmet abbreviations and we'll generate HTML for you"
  putStrLn "  -- to quit, hit return on an empty line"
  -- invoke a recursive main to continue to prompt the user until they wish to quit
  mainR

-- Main interactive function
mainR :: IO ()
mainR = do
  putStr "\nemmet: "
  hFlush stdout   -- line buffering prevents the prompt from printing without the newline, so this sends it
  oneLine <- getLine
  if null oneLine
    then do
      putStrLn "Exiting ..."
      return ()
    else do
      putStrLn $ process oneLine
      mainR