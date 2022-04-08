module Graphics.DOT

import Data.List
import Data.List1

import Graphics.DOT.Lexer

import public Graphics.DOT.AST
import public Graphics.DOT.Parser

import public Graphics.DOT.ASTv2
import public Graphics.DOT.Parser2
import public Graphics.DOT.Interfaces

-- TODO: REMOVE ONCE READY
import System.File
import Text.Lexer.Core
import Text.Parser.Core

public export
Show a => Show (ParsingError a) where
  show (Error s Nothing) = "PARSING ERROR: " ++ s
  show (Error s (Just (MkBounds startLine startCol endLine endCol))) =
    "PARSING ERROR: "
    ++ s
    ++ " @ L\{show startLine}:\{show startCol}-L\{show endLine}:\{show endCol}"

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
     let pRes = Parser2.parse tokData
     putStrLn $
        the String $
            case pRes of
                 Left errs => show $ map show errs
                 Right (ast, rem) =>
                     show ast ++ "\n\tREM: " ++ show rem
     Right (ast, rem) <- pure $ Parser2.parse tokData
        | Left errs => do putStrLn (show $ map show errs)
     putStrLn $ "Remainder: " ++ show rem

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
   show (FError errMsg) = "DOTERROR from file: " ++ errMsg
   show (ParseError errMsg) = "DOTERROR when parsing: " ++ errMsg

||| Given a file name, open it and lex and parse the DOT in it, returning the
||| deprecated AST `DOT`.
|||
||| @ fname the file name to read
export
readDOTFile : HasIO io => (fname : String) -> io (Either DOTError DOT)
readDOTFile fname =
  do (Right contents) <- readFile fname
        | Left err => pure $ Left $ FError (show err)
     let (tokData, _) = lex contents
     Right (ast, rem) <- pure $ Parser.parse tokData
        | Left _ => pure $ Left $ ParseError "Couldn't parse token data."
     if (not . isNil) rem
        then pure $ Left $ ParseError ("Couldn't parse entire token stream.\nRemainder: " ++ show rem)
        else pure $ Right ast

||| Given a file name, open it and lex and parse the DOT in it, returning the
||| new AST `Graph` (and its internals).
|||
||| @ fname the file name to read
export
readDOTFileV2 : HasIO io => (fname : String) -> io (Either DOTError Graph)
readDOTFileV2 fname =
  do (Right contents) <- readFile fname
        | Left err => pure $ (Left . FError) $ show err
     let (tokData, _) = lex contents
     Right (ast, rem) <- pure $ Parser2.parse tokData
        | Left pErrs => pure $ (Left . ParseError) $ show (map show pErrs)
     if (not . isNil) rem
        then pure $ (Left . ParseError) $ "Non-empty token remainder:\n\t\{show rem}"
        else pure $ Right ast

