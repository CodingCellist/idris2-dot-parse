module Graphics.DOT

import Graphics.DOT.Lexer
import Graphics.DOT.Parser

-- TODO: REMOVE ONCE READY
import System.File
import Text.Lexer.Core

lexTest : String -> IO ()
lexTest fp =
  do (Right contents) <- readFile fp
        | Left err => putStrLn $ "FILE ERROR: " ++ show err
     let (tokList, (line, col, rem)) = lex contents
     putStrLn $ "Reached " ++ (show line) ++ ":" ++ (show col)
     putStrLn $ "Remainder: " ++ rem
     putStrLn "----------------\n-- TOKEN LIST --\n----------------\n"
     putStrLn $ show tokList

