
import Interpreter (interpret)
import State (State(..), get, put)
import Parser (Parser(..), statement, statements)

import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO (hFlush, stdout)




main :: IO ()
main = do
  clearScreen
  setCursorPosition 0 0
  putStrLn "==================================="
  putStrLn "|         Lambda Calculus         |"
  putStrLn "==================================="
  putStrLn ""
  putStrLn helpMenu
  loop []
  where
    loop env = do
      putStr "lambda-calc> "
      hFlush stdout
      s <- getLine
      case s of
        ""       -> loop env
        (':':cs) -> do 
          parseCommand env cs
        _        -> do
          let (output, env') = run (convert s) env
          putStrLn output
          loop env'

    convert s = do
      env <- get
      let ctx = map (\(x,_) -> x) env
      case parse (statement ctx) s of
        Nothing      -> return $ "Error: Parse failed on '" ++ s ++ "'"
        Just ([], j) -> interpret j
        Just (s, _) -> return $ "Error: Parse failed at '" ++ take 10 s ++ "...'"

    parseCommand env "clear"  = do
      clearScreen
      setCursorPosition 0 0
      loop env
    parseCommand env "bound"  = do
      traverse putStrLn (map (\(x,_) -> x) (reverse env))
      loop env
    parseCommand env "unbind" = loop []
    parseCommand env "exit"   = return ()
    parseCommand env "help"   = do
      putStr helpMenu
      loop env
    parseCommand env s        = do
      case words s of
        ["interpret", filename] -> do
          contents <- lines <$> readFile filename
          putStrLn $ "Loading '" ++ filename ++ "'"
          let consumeFile env []     = loop env
              consumeFile env ("":ls) = consumeFile env ls
              consumeFile env (l:ls) = do
                let (output, env') = run (convert l) env
                putStrLn output
                consumeFile env' ls
          consumeFile env contents
        
        ["load", filename] -> do
          contents <- readFile filename
          case parse (statements (map (\(x,_) -> x) env)) contents of
            Nothing       -> do
              putStrLn $ "Failed to parse file '" ++ filename ++ "'"
              loop env
            
            Just ([], js) -> do 
              let (output, env') = run (mapM interpret js) env
              mapM putStrLn output
              loop env'

            Just (s, _)   -> do
              putStrLn $ "Error: Parse failed at '" ++ take 10 s ++ "...'"
              loop env

        _                  -> do
          putStrLn $ "Error: Unrecognized command '" ++ s ++ "'"
          loop env
        

    helpMenu = "Console Commands\n \
               \\t:clear\t\tclears the screen\n \
               \\t:bound\t\tdisplays bound names\n \
               \\t:unbind\t\tremoves all bound names\n \
               \\t:exit\t\texits the interactive\n \
               \\t:help\t\tdisplays this menu\n \
               \\t:interpret <fn>\tinterprets a file into the current\n \
               \\t               \tenvironment. does not work if the\n \
               \\t               \tstatements span multiple lines.\n \
               \\t:load <fn>\tloads a file into the interpreter\n \
               \\n \
               \Statements\n \
               \\tdefine <identifier> := <term>\n \
               \\teval   <term>\n \
               \\tstep   <count>|? <term>\n"