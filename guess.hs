-- guess a number

import System.Random

numbersToGuess = 5
attemptsPerNumber = 3

main = do
    rng <- newStdGen
    let randomNums = randomRs (1, 10) rng :: [Int]
    results <- mapM guessOneNumber $ take numbersToGuess randomNums
    putStrLn $ "\nGAME OVER! You won " ++
        (show $ countTrues results) ++ " times out of " ++
        (show $ length results)

guessOneNumber goal = do
    putStrLn "\nGuess a number between 1 and 10"
    res <- doGuesses goal attemptsPerNumber
    return res

doGuesses goal 0 = do
    putStrLn $ "You failed! The number was " ++ (show goal)
    return False
doGuesses goal attempt = do
    putStrLn $ (show attempt) ++ " attempts left. Guess a number: "
    res <- doOneGuess goal
    if res
        then return True
        else doGuesses goal (attempt-1)

doOneGuess goal = do
    v <- getLine
    case read v of
        v | v < goal -> do { putStrLn "Too low!"; return False }
          | v > goal -> do { putStrLn "Too high!"; return False }
        otherwise    -> do { putStrLn "Correct!"; return True }

countTrues list = foldl (\acc b -> if b then acc+1 else acc) 0 list
