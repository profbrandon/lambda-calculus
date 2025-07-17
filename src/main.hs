
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
  loop (False, [])
  where
    loop (b, env) = do
      putStr "lambda-calc> "
      hFlush stdout
      s <- getLine
      case s of
        ""       -> loop (b, env)
        (':':cs) -> do 
          parseCommand (b, env) cs
        _        -> do
          let (output, (b', env')) = run (convert s) (b, env)
          putStrLn output
          loop (b', env')

    convert s = do
      (b, env) <- get
      let ctx = map (\(x,_,_) -> x) env
      case parse (statement b ctx) s of
        Nothing      -> return $ "Error: Parse failed on '" ++ s ++ "'"
        Just ([], j) -> interpret j
        Just (s, _) -> return $ "Error: Parse failed at '" ++ take 10 s ++ "...'"

    parseCommand (b, env) "clear"  = do
      clearScreen
      setCursorPosition 0 0
      loop (b, env)
    parseCommand (b, env) "bound"  = do
      traverse putStrLn (map (\(x,_,_) -> x) (reverse env))
      loop (b, env)
    parseCommand (b, env) "unbind" = loop (b, [])
    parseCommand (b, env) "exit"   = return ()
    parseCommand (b, env) "help"   = do
      putStr helpMenu
      loop (b, env)
    parseCommand (b, env) s        = do
      case words s of
        ["interpret", filename] -> do
          contents <- lines <$> readFile filename
          putStrLn $ "Loading '" ++ filename ++ "'"
          let consumeFile (b, env) []     = loop (b, env)
              consumeFile (b, env) ("":ls) = consumeFile (b, env) ls
              consumeFile (b, env) (l:ls) = do
                let (output, (b', env')) = run (convert l) (b, env)
                putStrLn output
                consumeFile (b', env') ls
          consumeFile (b, env) contents
        
        ["load", filename] -> do
          contents <- readFile filename
          case parse statements contents of
            Nothing       -> do
              putStrLn $ "Failed to parse file '" ++ filename ++ "'"
              loop (b, env)
            
            Just ([], js) -> do 
              let (output, (b', env')) = run (mapM interpret js) (b, env)
              mapM putStrLn output
              loop (b', env')

            Just (s, _)   -> do
              putStrLn $ "Error: Parse failed at '" ++ take 10 s ++ "...'"
              loop (b, env)

        _                  -> do
          putStrLn $ "Error: Unrecognized command '" ++ s ++ "'"
          loop (b, env)
        

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
               \\t(# UNTYPED #)\n \
               \\t(# TYPED #)\n \
               \\tdefine <identifier> := <term>\n \
               \\ttypeof <term>\n \
               \\teval   <term>\n"