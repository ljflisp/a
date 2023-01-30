import System.Environment
import Data.List

--------------------------------
--        Utilities           --
--------------------------------

-- Get first index of char in str
indexOf str char = getEl [ y | (x, y) <- zip str [0..], x == char ] where
    getEl arr
        | arr == [] = -1
        | otherwise = head arr

-- Split string str into list with delimiter char
split str char = splitFrom (indexOf str char) where
    splitFrom indx
        | indx == -1 = [str]
        | otherwise = parse $ (splitAt indx str) where
            parse tupple = [fst tupple] ++ split (removeFirst $ snd tupple) char where
                removeFirst "" = ""
                removeFirst (a:b) = b


--------------------------------
--         Functions          --
--------------------------------

-- Get letter in alphabet with most occurances in arr 
getMaxLetter arr alphabet = fst $ getMax [(letter, number) | (letter, number) <- (zip alphabet [getNumWithLetter arr letter | letter <- alphabet])] where
    getNumWithLetter arr letter = sum [1 | word <- arr, (indexOf word letter) /= -1]
    getMax arr = maximum ('a', 0) arr where
        maximum original [] = original
        maximum original (first:rest)
            | (snd original) > (snd first) = maximum original rest
            | otherwise = maximum first rest

-- Get best starting word
getMaxWord arr alphabet = getWord arr alphabet 0 where
    getWord arr alpha number
        | number >= 5 = head arr
        | sum [1 | _ <- arr] == 1 = head arr
        | alpha == [] = head arr
        | otherwise = parseNext (getMaxLetter arr alpha) alpha where
            parseNext maxLetter alpha = getWord [letter | letter <- arr, (indexOf letter maxLetter) /= -1] [letter | letter <- alpha, letter /= maxLetter] (number + 1)

-- Parse new array given a response
parseArrayTotal (typeName:rest) pos arr letter others = parse_ typeName letter pos arr others where
    parse_ typeName letter pos arr others
        | typeName == 'g' = parseGreen letter pos arr
        | typeName == 'y' = parseYellow letter pos arr
        | otherwise = parseNone letter pos arr others where
            parseYellow letter pos arr = parseNotAt letter pos (parseIncludes letter arr) where
                parseIncludes letter arr = [x | x <- arr, (indexOf x letter) /= -1]
                parseNotAt letter pos arr = [x | x <- arr, x !! pos /= letter]
            parseGreen letter pos arr = parseAt letter pos arr where
                parseAt letter pos arr = [x | x <- arr, x !! pos == letter]
            parseNone letter pos arr item = parseNotNth letter item (parseNotAt letter pos arr) where
                parseNotAt letter pos arr = [x | x <- arr, x !! pos /= letter]
                parseNotNth letter number arr = [x | x <- arr, getCount x letter <= number] where
                    getCount str char = sum [1 | x <- str, x == char]



--------------------------------
--            IO              --
--------------------------------

-- Iterate to get responses and get next word
parseIter word arr alpha = do
    putStrLn $ "Word: " ++ word
    putStr $ (show $ word !! 0) ++ ": "
    s0 <- getLine
    putStr $ (show $ word !! 1) ++ ": "
    s1 <- getLine
    putStr $ (show $ word !! 2) ++ ": "
    s2 <- getLine
    putStr $ (show $ word !! 3) ++ ": "
    s3 <- getLine
    putStr $ (show $ word !! 4) ++ ": "
    s4 <- getLine
    let all = zip [s0, s1, s2, s3, s4] word
    let arr0 = parseArrayTotal s0 0 arr (word !! 0) (sum [1 | (t, l) <- all, l == (word !! 0), (head t) /= 'n'])
    let arr1 = parseArrayTotal s1 1 arr0 (word !! 1) (sum [1 | (t, l) <- all, l == (word !! 1), (head t) /= 'n'])
    let arr2 = parseArrayTotal s2 2 arr1 (word !! 2) (sum [1 | (t, l) <- all, l == (word !! 2), (head t) /= 'n'])
    let arr3 = parseArrayTotal s3 3 arr2 (word !! 3) (sum [1 | (t, l) <- all, l == (word !! 3), (head t) /= 'n'])
    let arr4 = parseArrayTotal s4 4 arr3 (word !! 4) (sum [1 | (t, l) <- all, l == (word !! 4), (head t) /= 'n'])
    let word0 = getMaxWord arr4 alpha
    putStrLn ""
    if sum [1 | (t, l) <- all, (head t) == 'g'] == 5 then return () else parseIter word0 arr4 alpha

-- Main function
main = do
    putStrLn "Welcome!"
    putStrLn "\nThis is an AI to solve your Wordle challenges.\n\n\n"
    s <- readFile "small.txt"
    let arr = split s ' '
    let alpha = "abcdefghijklmnopqrstuvwxyz"
    let word = getMaxWord arr alpha
    parseIter word arr alpha
