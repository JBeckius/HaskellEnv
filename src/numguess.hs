import System.Random
import Control.Monad(when)
import Text.Read

main = do
    putStrLn "Give me five integers and I'll tell you which one is largest"
    rs <- sequence $ replicate 5 getNum
    print $ "The largest number you gave me was " ++ show  (maximum rs)
    -- gen <- getStdGen
    -- askForNumber gen

getNum :: IO Int
getNum = do
    line <- getLine
    let numM = readMaybe line :: Maybe Int
    print numM
    case numM of
        Just a -> return a
        Nothing -> do
            putStrLn "That thing you gave me was not an integer. Try again."
            getNum

-- askForNumber :: StdGen -> IO ()
-- askForNumber gen = do
--     let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
--     putStrLn "Which number in the range from 1 to 10 am I thinking of?"
--     numberString <- getLine
--     when (not $ null numberString) $ do
--         let number = read numberString

--         if randNumber == number
--             then putStrLn "You are correct!"
--             else putStrLn $ "Sorry, it was " ++ show randNumber
--         askForNumber newGen

-- putStr :: String -> IO ()
-- putStr [] = return ()
-- putStr (x:xs) = do
--     putChar x
--     putStr xs