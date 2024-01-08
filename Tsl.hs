module Tsl where
import Interpreter_2
import Grammar_2
import Lexer_2
import TypeChecker
import Control.Exception
import System.IO
import System.Environment
--import ParseTile

main :: IO ()
main = catch main' noParse

collectReadFromProg :: ProgramData -> [(String,String)]
collectReadFromProg (Prog c) = collectRead c

collectRead :: CodeData -> [(String,String)]
collectRead (CodeBlock ((Read (Var id) (StringExpr file)))) = [(id,(file ++ ".tl"))]
--collectRead e | isTerminated e = []
collectRead (CodeBlock (While e code)) = collectRead code
collectRead (CodeBlock (If e code1 code2)) = collectRead code1 ++ (collectRead code2)
--collectRead (CodeBlock statement) = collectRead statement
collectRead (CodeBlocks statement code) = collectRead (CodeBlock statement) ++ (collectRead code)
collectRead _ = []

convertAll :: [(String,String)] -> Environment -> IO Environment
convertAll [] _ = return []
convertAll ((id,file):[]) env = do input <- readFile file
                                   let inputTile = stringToTile (lines input)
                                   let x = (id,(TileExpr inputTile))
                                   return (x:[])
convertAll ((id,file):all) env =
  do input <- readFile file
     let inputTile = stringToTile (lines input)
     let x = (id,(TileExpr inputTile))
     xs <- convertAll all env
     return (x : xs)

stringToTile :: [String] -> [[Cell]]
stringToTile [] = []
stringToTile (row:tile) | row == "\n" = stringToTile tile
                        | otherwise = stringToCells row : stringToTile tile

stringToCells :: String -> [Cell]
stringToCells [] = []
stringToCells (c:cs) | c == '0' = C0 : stringToCells cs
                     | c == '1' = C1 : stringToCells cs
                     | c == '-' = C2 : stringToCells cs


main' = do  (filename : _) <- getArgs
            sourceText  <- readFile filename
            --putStrLn ("Parsing : " ++ sourceText)
            let parsedProg = parseTL (alexScanTokens sourceText)
            --putStrLn ("Parsed as " ++ (show parsedProg) ++ "\n")
            --putStrLn ("Type Checking : " ++ (show parsedProg) ++ "\n")
            let typedProg = typePatternProg [] parsedProg
            let readStatements = collectReadFromProg parsedProg
            environment <- convertAll readStatements []
            let result = interpret parsedProg environment
            putStrLn (result)

 {-
getResult :: StatementData -> String
getResult (Return (IntLiteral i)) = show i
getResult (Return (BoolLiteral b)) = show b
getResult (Return (TileExpr t)) = tileToString t
-}


noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()