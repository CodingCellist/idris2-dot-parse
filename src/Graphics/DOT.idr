module Graphics.DOT

import Data.List
import Data.List1

import public Graphics.DOT.AST
import Graphics.DOT.Lexer
import Graphics.DOT.Parser

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

||| The types of errors that can occur when processing a DOT/.gv file, combined
||| with the respective error message.
public export
data DOTError : Type where
   ||| An error occurred when trying to read the file.
   FError : (errMsg : String) -> DOTError
   ||| Something's wrong with the structure of the DOT in the file.
   ParseError : (errMsg : String) -> DOTError

public export
Show DOTError where
   show (FError errMsg) = "FILE ERR: " ++ errMsg
   show (ParseError errMsg) = "PARSE ERR: " ++ errMsg

||| Given a file name, open it and lex and parse the DOT in it.
|||
||| @ fname the file name to read
export
readDOTFile : HasIO io => (fname : String) -> io (Either DOTError DOT)
readDOTFile fname =
   do (Right contents) <- readFile fname
         | Left err => pure $ Left $ FError (show err)
      let (tokData, _) = lex contents
      Right (ast, rem) <- pure $ parse tokData
         | Left _ => pure $ Left $ ParseError "Couldn't parse token data."
      if (not . isNil) rem
         then pure $ Left $ ParseError ("Couldn't parse entire token stream.\nRemainder: " ++ show rem)
         else pure $ Right ast

