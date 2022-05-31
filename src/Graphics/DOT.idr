module Graphics.DOT

import Data.List
import Data.List1

import Graphics.DOT.Lexer

import public Graphics.DOT.AST
import public Graphics.DOT.Utils
import public Graphics.DOT.Parser
import public Graphics.DOT.Interfaces

-- TODO: REMOVE ONCE READY
import System.File
import Text.Lexer.Core
import Text.Parser.Core

----------
-- UTIL --
----------

||| The types of errors that can occur when processing a DOT/.gv file, combined
||| with the respective error message.
public export
data DOTError : Type where
   ||| An error occurred when trying to read the file.
   FError : (errMsg : String) -> DOTError
   ||| Something's wrong with the structure of the DOT in the file.
   ParseError : (errMsg : String) -> DOTError

export
Show DOTError where
   show (FError errMsg) = "DOTERROR from file: " ++ errMsg
   show (ParseError errMsg) = "DOTERROR when parsing: " ++ errMsg


||| Given a file name, open it and lex and parse the DOT in it, returning the
||| `Graph` node which is the root of the AST.
|||
||| @ fname the file name to read
export
readDOTFile : HasIO io => (fname : String) -> io (Either DOTError Graph)
readDOTFile fname =
  do (Right contents) <- readFile fname
        | Left err => pure $ (Left . FError) $ show err
     let (tokData, _) = lex contents
     Right (ast, rem) <- pure $ parse tokData
        | Left pErrs => pure $ (Left . ParseError) $ show (map show pErrs)
     if (not . isNil) rem
        then pure $ (Left . ParseError) $ "Non-empty token remainder:\n\t\{show rem}"
        else pure $ Right ast


-----------
-- TESTS --
-----------

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
                 Left errs => show $ map show errs
                 Right (ast, rem) =>
                     show ast ++ "\n\tREM: " ++ show rem
     Right (ast, rem) <- pure $ parse tokData
        | Left errs => do putStrLn (show $ map show errs)
     putStrLn $ "Remainder: " ++ show rem

