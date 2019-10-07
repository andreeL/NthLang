module Main where

import Control.Monad
import System.Random.MWC as MWC
type Dictionary = [(String, String)]

getDictionary :: IO Dictionary
getDictionary = do
    fileContent <- readFile "data/words.txt"
    pure (read fileContent)

listWords :: IO ()
listWords = do
    dictionary <- getDictionary
    print dictionary

getWord gen dictionary nthWord = do
    index <- uniformR (0 :: Int, (min nthWord (length dictionary)) - 1) gen
    pure $ dictionary !! index

testNextWord gen dictionary nthWord = do
    wordEntry <- getWord gen dictionary nthWord
    putStr $ "What is " ++ (fst wordEntry) ++ "? "
    answer <- getLine
    if (answer == ":q")
        then pure ()
        else do
            if (answer == snd wordEntry)
                then pure ()
                else do
                    print $ "Wrong! " ++ (fst wordEntry) ++ " = " ++ (snd wordEntry)
                    print "------"
            testNextWord gen dictionary nthWord

testWords :: Int -> IO ()
testWords nthWord = do
    dictionary <- getDictionary
    gen <- MWC.create
    testNextWord gen dictionary nthWord

main :: IO ()
main = testWords 10