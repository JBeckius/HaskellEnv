import System.IO
import System.Directory
import Data.List
import Control.Exception

main = do
    contents <- readFile "todo.txt" -- read todo and bint its contents to "contents"
    let todoTasks = lines contents -- split the contents into a list of strings
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line)
                                    [0..] todoTasks -- create function for displaying numbered tasks
    
    putStrLn "These are your TO-DO items: "
    mapM_ putStrLn numberedTasks -- prints each task to a new line
    putStrLn "Which one do you want to delete?"
    numberString <- getLine -- ask the user for input on which task to delete
    let number = read numberString -- convert the user string input into a number
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks -- create a new task list after deleting the requested task
    bracketOnError (openTempFile "." "temp") -- open temporary file and setup error handing
        (\(tempName, tempHandle) -> do -- if error occurs:
            hClose tempHandle -- close the tempfile
            removeFile tempName -- remove the tempfile
        )
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems -- Add the new todo list lines to the temp file
            hClose tempHandle -- close the temp file
            removeFile "todo.txt" -- get rid of the old todo list file
            renameFile tempName "todo.txt" -- rename the temp file to replace the old todo.txt
        )
