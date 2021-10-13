module Graphics.DOT

import Graphics.DOT.AST
import Graphics.DOT.Lexer
import Graphics.DOT.Parser
import Data.List1

-- TODO: REMOVE ONCE READY
import System.File
import Text.Lexer.Core
import Text.Parser.Core

lexTest : String -> IO ()
lexTest fp =
  do (Right contents) <- readFile fp
        | Left err => putStrLn $ "FILE ERROR: " ++ show err
     let (tokList, (line, col, rem)) = lex contents
     putStrLn $ "Reached " ++ (show line) ++ ":" ++ (show col)
     putStrLn $ "Remainder: " ++ rem
     putStrLn "----------------\n-- TOKEN LIST --\n----------------\n"
     putStrLn $ show tokList

parseTest : String -> IO ()
parseTest fp =
  do (Right contents) <- readFile fp
       | Left err => putStrLn $ "FILE ERROR: " ++ show err
     let (tokData, _) = lex contents
     let pRes = parse tokData
     putStrLn $
        the String $
            case pRes of
                 Left errs =>
                     "PARSER ERROR: " -- ++ (show errs) -- ++ "\n\t" ++ show ts
                 Right (ast, rem) =>
                     show ast ++ "\n\tREM: " ++ show rem
     Right (ast, rem) <- pure $ parse tokData
        | Left errs => do putStrLn $ "PARSER ERROR: " -- ++ (show errs)
                          -- putStrLn $ "\t" ++ show ts
     putStrLn $ "Remainder: " ++ show rem
--     putStrLn "----------------\n-- AST --\n----------------\n"
--     putStrLn $ show ast

