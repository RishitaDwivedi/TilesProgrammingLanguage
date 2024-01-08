module Interpreter_2 where
import Grammar_2
import Lexer_2
import Data.List



type Kontinuation = [ FrameExpr ]

type State = (ExprData,Environment,Kontinuation)



lookupBinding :: IdentifierData -> Environment -> (ExprData,Environment)
lookupBinding id [] = error []
lookupBinding (Var id) ((id2,e):env) | id == id2 = (e,env)
                                           | otherwise = lookupBinding (Var id) env


update :: Environment -> IdentifierData -> ExprData -> Environment
update env (Var id) e = (id,e) : env

isTerminated :: ExprData -> Bool
isTerminated (CellLiteral _) = True
isTerminated (BoolLiteral _) = True
isTerminated (IntLiteral _) = True
isTerminated (TileExpr _) = True
isTerminated (BlockExpr _) = True
isTerminated (Pair e1 e2) = isTerminated e1 && isTerminated e2
isTerminated (TileLsExpr _) = True
isTerminated (ReturnExpr e) = isTerminated e
isTerminated (Comma e1 e2) = isTerminated e1 && isTerminated e2
isTerminated (List e) = isTerminated e
isTerminated _ = False


toString :: ExprData -> String
toString (IntLiteral i) = show i
toString (CellLiteral C2) = " "
toString (CellLiteral C1) = "C1"
toString (CellLiteral C0) = "C0"
toString (BoolLiteral True) = show True
toString (BoolLiteral False) = show False
toString (StringExpr str) = str
toString (TileExpr tile) = tileToString tile
toString (Pair e1 e2) = "(" ++ toString e1 ++ "," ++ toString e2 ++ ")"
toString (BlockExpr block) = tileToString (blockToTile block)



tileToString :: Tile -> String
tileToString [] = []
tileToString (row : []) = rowToString row
tileToString (row:tile) = rowToString row ++ "\n" ++ tileToString tile

rowToString :: [Cell] -> String
rowToString [] = []
rowToString (C0 : row) = "0" ++ rowToString row
rowToString (C1 : row) = "1" ++ rowToString row
rowToString (C2 : row) = "0" ++ rowToString row

blockToTile :: [[[[Cell]]]] -> [[Cell]]
blockToTile [] = []
blockToTile [[],[]] = []
blockToTile (row:block) | length row > 0 = concatTileRow row ++ blockToTile block
                        | otherwise = []

concatTileRow :: [[[Cell]]] -> [[Cell]]

concatTileRow row | length (head row) >= 2 = concat (concat (map (take 1) row)) : concatTileRow (map (drop 1) row)
                  | length (head row) == 1 =  concat (concat (map (take 1) row)) : []
                  | otherwise = []
-- fucntion main will call
interpret :: ProgramData -> Environment -> String
interpret prog env = evalReturn (evaluation prog env)

evalReturn :: (ExprData,Environment) -> String
evalReturn ((ReturnExpr e),env) = toString e

evalReturn (_,e) = error "no return statement"

evaluation :: ProgramData -> Environment -> (ExprData,Environment)
evaluation (Prog c) env = evaluationCode c env



-- environment needs to be passed between code

evaluationCode :: CodeData -> Environment -> (ExprData,Environment)
evaluationCode (CodeBlock (Return e)) env | isTerminated e = ((ReturnExpr e),env)
evaluationCode (CodeBlock (Return e)) env = ((ReturnExpr (evaluationExpr e env)),env)
evaluationCode (CodeBlock statement) env = (evaluationStatement statement env)

evaluationCode (CodeBlocks statement code) env = evaluationCode code (snd (evaluationStatement statement env) ++ env)


evaluationStatement :: StatementData -> Environment -> (ExprData,Environment)
evaluationStatement (If e1 code1 code2) env | evaluationExpr e1 env == (BoolLiteral True) = evaluationCode code1 env
                                            | otherwise = evaluationCode code2 env
evaluationStatement (JustIf e1 code) env | evaluationExpr e1 env == (BoolLiteral True) = (evaluationCode code env)
                                         | otherwise = (e1,env)
evaluationStatement (Assign t (Var id) e) env | isTerminated e = (e, (id,e) : env)
                                              | otherwise = (e',(id,e'):env)
                                                where e' = evaluationExpr e env
evaluationStatement (Reassign (Var id) e) env | lookupBinding (Var id) env == (e,[]) = error "no type"
                                              | otherwise = (e , ((id,e) : env))
evaluationStatement (Return e) env | isTerminated e = ((ReturnExpr e),env)
                                   | otherwise = ((ReturnExpr (evaluationExpr e env)),env)

evaluationStatement (Read e1 e2) env = ((Id e1),env)

evaluationStatement (While e code) env | evaluationExpr e env == (BoolLiteral True) && evaluationExpr e env' == (BoolLiteral True)
                                              = evaluationStatement (While e code) env'
                                        -- if e true for current and next iteration the loop
                                       | evaluationExpr e env == (BoolLiteral True) = (code',env')
                                        -- if e only true for current iteration then return evaluated code
                                       | otherwise = (e,env)
                                       -- if not true then return unaltered env
                                          where (code',env') = evaluationCode code env



evaluationExpr :: ExprData -> Environment -> ExprData
evaluationExpr e env = eval (e,env,[])

eval :: State -> ExprData
eval (e,env,k) | e == e' && isTerminated e' && null k = e'
               | otherwise = eval (e',env',k')
                  where (e',env',k') = evalStep (e,env,k)

data FrameExpr = HReturn
                | HCons ExprData Environment
                | ConsH ExprData
                | HLT ExprData Environment
                | LTH ExprData
                | HGT ExprData Environment
                | GTH ExprData
                | HEq ExprData Environment
                | EqH ExprData
                | HNeg --
                | HAnd ExprData Environment --
                | AndH ExprData --
                | HOr ExprData Environment --
                | OrH ExprData --
                | HExp ExprData Environment --
                | ExpH ExprData --
                | HMul ExprData Environment
                | MulH ExprData --
                | HDiv ExprData Environment
                | DivH ExprData
                | HMod ExprData Environment
                | ModH ExprData
                | HAdd ExprData Environment
                | AddH ExprData
                | HSub ExprData Environment
                | SubH ExprData
                | HRefX --
                | HRefY --
                | HRefYB
                | HRefXB
                | HRot --
                | HRepX ExprData Environment --n
                | RepXH ExprData --tile
                | HRepXB ExprData Environment -- n
                | RepXBH ExprData -- block
                | HRepY ExprData Environment -- n
                | RepYH ExprData --tile
                | HRepYB ExprData Environment -- n
                | RepYBH ExprData --block
                | HAddU ExprData Environment
                | AddUH ExprData
                | HAddUB ExprData Environment
                | AddUBH ExprData
                | HAddR ExprData Environment
                | AddRH ExprData
                | HAddRB ExprData Environment
                | AddRBH ExprData
                | HPair ExprData Environment
                | PairH ExprData
                | HFst
                | HSnd
                | HScale ExprData Environment
                | ScaleH ExprData
                | HSubtile ExprData ExprData Environment
                | SubHtile ExprData Environment ExprData Environment
                | SubtileH ExprData ExprData
                | HBlank
                | HIndex ExprData Environment
                | IndexH ExprData
                | HTileN
                | HTileConj ExprData Environment
                | TileConjH ExprData
                | HTileUnion ExprData Environment
                | TileUnionH ExprData
                | HList
                | HComma ExprData Environment
                | CommaH ExprData
                | HLength
                | HFormat
                | HBrack



evalStep :: State -> State
evalStep ((Id id),env,k) = (e',env',k)
    where (e',env') = lookupBinding id env

evalStep (v,env,[]) | isTerminated v = (v,env,[])

evalStep ((BrackExpr e),env,k) = (e,env, HBrack:k)
evalStep (e,env,(HBrack:k)) = evalStep (e,env,k)



evalStep ((Cons e1 e2),env,k) = (e1,env,(HCons e2 env):k)
evalStep (e,env,(HCons e2 env2):k) | isTerminated e = (e2,env2,(ConsH e):k)
                                   | otherwise = error []
evalStep ((List list),env,(ConsH e):k) = ((List (Comma e list)),[],k)

-- eval for return
-- need to maintain its what is being return by the program
evalStep ((ReturnExpr e),env,k) = (e,env,(HReturn:k))
evalStep (e,env,HReturn:k) | isTerminated e = ((ReturnExpr e),[],k)

-- less than
evalStep ((LsThan e1 e2),env,k) = (e1,env,(HLT e2 env):k)
evalStep ((IntLiteral i),env1,(HLT e env2):k) = (e,env2,(LTH (IntLiteral i)):k)
evalStep ((IntLiteral j),env,(LTH (IntLiteral i)):k) | i < j = ((BoolLiteral True),[],k)
                                                     | otherwise = ((BoolLiteral False),[],k)
--eval rules for GT

evalStep ((GrThan e1 e2),env,k) = (e1,env,(HGT e2 env):k)
evalStep ((IntLiteral i),env1,(HGT e env2):k) = (e,env2,(GTH (IntLiteral i)):k)
evalStep ((IntLiteral j),env,(GTH (IntLiteral i)):k) | i > j = ((BoolLiteral True),[],k)
                                                     | otherwise = ((BoolLiteral False),[],k)

--equivalence

evalStep ((Equal e1 e2),env,k) = (e1,env,(HEq e2 env):k)
--evalStep ((Equal e1 e2),env,k | isTerminated e1 && isTerminated e2

evalStep ((CellLiteral i),env1,(HEq e env2):k) = (e,env2,(EqH (CellLiteral i)):k)
evalStep ((CellLiteral j),env,(EqH (CellLiteral i)):k) | i == j = ((BoolLiteral True),[],k)
                                                | otherwise = ((BoolLiteral False),[],k)

evalStep ((BoolLiteral i),env1,(HEq e env2):k) = (e,env2,(EqH (BoolLiteral i)):k)
evalStep ((BoolLiteral j),env,(EqH (BoolLiteral i)):k) | i == j = ((BoolLiteral True),[],k)
                                                | otherwise = ((BoolLiteral False),[],k)

evalStep ((IntLiteral i),env1,(HEq e env2):k) = (e,env2,(EqH (IntLiteral i)):k)
evalStep ((IntLiteral j),env,(EqH (IntLiteral i)):k) | i == j = ((BoolLiteral True),[],k)
                                                | otherwise = ((BoolLiteral False),[],k)

evalStep ((TileExpr i),env1,(HEq e env2):k) = (e,env2,(EqH (TileExpr i)):k)
evalStep ((TileExpr j),env,(EqH (TileExpr i)):k) | i == j = ((BoolLiteral True),[],k)
                                                | otherwise = ((BoolLiteral False),[],k)

evalStep ((BlockExpr i),env1,(HEq e env2):k) = (e,env2,(EqH (BlockExpr i)):k)
evalStep ((BlockExpr j),env,(EqH (BlockExpr i)):k) | i == j = ((BoolLiteral True),[],k)
                                                | otherwise = ((BoolLiteral False),[],k)

--evalStep (e1,env,(EqH e2):k)  | isTerminated e1 && isTerminated e2 = error

--negation
evalStep ((Negtn e),env,k) = (e,env,HNeg : k)
evalStep ((BoolLiteral True),env,HNeg:k) = ((BoolLiteral False),[],k)
evalStep ((BoolLiteral False),env,HNeg:k) = ((BoolLiteral True),[],k)
evalStep ((CellLiteral C0),env,HNeg:k) = ((CellLiteral C1),[],k)
evalStep ((CellLiteral C1),env,HNeg:k) = ((CellLiteral C0),[],k)

--and
evalStep ((Conj e1 e2),env,k) = (e1,env,(HAnd e2 env) : k)

evalStep ((BoolLiteral True),env1,(HAnd e env2) : k) = (e,env2,(AndH (BoolLiteral True)):k)
evalStep ((BoolLiteral False),env1,(HAnd e env2) : k) = ((BoolLiteral False),[],k)
evalStep ((BoolLiteral False),env1,(AndH e) : k) = ((BoolLiteral False),[],k)
evalStep ((BoolLiteral True),env,(AndH (BoolLiteral True)) : k) = ((BoolLiteral True),[],k)

evalStep ((CellLiteral C1),env1,(HAnd e env2) : k) = (e,env2,(AndH (CellLiteral C1)):k)
evalStep ((CellLiteral C0),env1,(HAnd e env2) : k) = ((CellLiteral C0),[],k)
evalStep ((CellLiteral C0),env1,(AndH e) : k) = ((CellLiteral C0),[],k)
evalStep ((CellLiteral C1),env,(AndH (CellLiteral C1)) : k) = ((CellLiteral C1),[],k)


--or
evalStep ((Union e1 e2),env,k) = (e1,env,(HOr e2 env) : k)
evalStep ((BoolLiteral True),env1,(HOr e env2) : k) = ((BoolLiteral True),[],k)
evalStep ((BoolLiteral False),env1,(HOr e env2) : k) = (e,env2,(OrH (BoolLiteral False)) : k)
evalStep ((BoolLiteral True),env,(OrH e) : k) = ((BoolLiteral True),[],k)

evalStep ((CellLiteral C1),env1,(HOr e env2) : k) = ((CellLiteral C1),[],k)
evalStep ((CellLiteral C0),env1,(HOr e env2) : k) = (e,env2,(OrH (CellLiteral C0)) : k)
evalStep ((CellLiteral C1),env,(OrH e) : k) = ((CellLiteral C1),[],k)

--exponetial i ^ j
evalStep ((Exp e1 e2),env,k) = (e1,env,(HExp e2 env) : k)
evalStep ((IntLiteral i),env1,(HExp e env2):k) = (e,env2,(ExpH (IntLiteral i)):k)
evalStep ((IntLiteral j),env,(ExpH (IntLiteral i)):k) = ((IntLiteral (i^j)),[],k)
--multiply
evalStep ((Mult e1 e2),env,k) = (e1,env,(HMul e2 env):k)
evalStep ((IntLiteral i),env1,(HMul e env2):k) = (e,env2,(MulH (IntLiteral i)):k)
evalStep ((IntLiteral j),env,(MulH (IntLiteral i)):k) = ((IntLiteral (i * j)),[],k)
--divide
evalStep ((Div e1 e2),env,k) = (e1,env,(HDiv e2 env):k)
evalStep ((IntLiteral i),env1,(HDiv e env2):k) = (e,env2,(DivH (IntLiteral i)):k)
evalStep ((IntLiteral j),env,(DivH (IntLiteral i)):k) = ((IntLiteral (i `div` j)),[],k)
--add
evalStep ((Add e1 e2),env,k) = (e1,env,(HAdd e2 env):k)
evalStep ((IntLiteral i),env1,(HAdd e env2):k) = (e,env2,(AddH (IntLiteral i)):k)
evalStep ((IntLiteral j),env,(AddH (IntLiteral i)):k) = ((IntLiteral (i + j)),[],k)
--subtract
evalStep ((Sub e1 e2),env,k) = (e1,env,(HSub e2 env):k)
evalStep ((IntLiteral i),env1,(HSub e env2):k) = (e,env2,(SubH (IntLiteral i)):k)
evalStep ((IntLiteral j),env,(SubH (IntLiteral i)):k) = ((IntLiteral (i - j)),[],k)
--modulus
evalStep ((Mod e1 e2),env,k) = (e1,env,(HMod e2 env):k)
evalStep ((IntLiteral i),env1,(HMod e env2):k) = (e,env2,(ModH (IntLiteral i)):k)
evalStep ((IntLiteral j),env,(ModH (IntLiteral i)):k) = ((IntLiteral (i `mod` j)),[],k)

-- pair
--evalStep ((Pair e1 e2),env,k) = (e1,env,(HPair e2 env):k)
--evalStep (e1,env1,(HPair e2 env2):k) | isTerminated e1 = (e2,env2,(PairH e1):k)
--evalStep (e2,env,(PairH e1):k) | isTerminated e2 = ((Pair e1 e2),[],k)

--fst
evalStep ((Fst e),env,k) = (e,env,HFst : k)
evalStep ((Pair e1 e2),env,HFst :k) | isTerminated e1 && isTerminated e2 = (e1,[],k)

--snd
evalStep ((Snd e),env,k) = (e,env,HSnd : k)
evalStep ((Pair e1 e2),env,HSnd :k) | isTerminated e1 && isTerminated e2 = (e2,[],k)



--tile functions

evalStep ((EmptyBlock),env,k) = ((BlockExpr evalEmptyBlock),[],k)

-- reflectX
evalStep ((RefX e,env,k)) = (e,env,(HRefX:k))
evalStep ((TileExpr t),env,HRefX : k) = ((TileExpr (evalRefX t)),[],k)

evalStep ((RefXB e,env,k)) = (e,env,(HRefXB:k))
evalStep ((BlockExpr b),env,HRefXB : k) = ((BlockExpr (evalRefXB b)),[],k)
-- reflectY
evalStep ((RefY e),env,k) = (e,env,HRefY : k)
evalStep ((TileExpr t),env,HRefY:k) = ((TileExpr (evalRefY t)),[],k)

evalStep ((RefYB e),env,k) = (e,env,HRefYB : k)
evalStep ((BlockExpr b),env,HRefYB:k) = ((BlockExpr (evalRefYB b )),[],k)

-- rotate
evalStep ((Rotate e),env,k) = (e,env,HRot : k)
evalStep ((TileExpr t),env1,HRot  : k) = ((TileExpr (evalRotate t)),[],k)
-- repeatX
evalStep ((RepXB e1 e2),env,k) = (e1,env,(HRepXB e2 env) : k)
evalStep ((BlockExpr b),env1,(HRepXB e env2):k) = (e,env2,(RepXBH (BlockExpr b)): k)
evalStep ((IntLiteral n),env,(RepXBH (BlockExpr b)):k) = ((BlockExpr (evalRepXB b n)),[],k)

evalStep ((RepX e1 e2),env,k) = (e1,env,(HRepX e2 env) : k)
evalStep ((TileExpr t),env1,(HRepX e env2):k) = (e,env2,(RepXH (TileExpr t)): k)
evalStep ((IntLiteral n),env,(RepXH (TileExpr t)):k) = ((TileExpr (evalRepX t n)),[],k)
-- repeatY
evalStep ((RepYB e1 e2),env,k) = (e1,env,(HRepYB e2 env) : k)
evalStep ((BlockExpr b),env1,(HRepYB e env2):k) = (e,env2,(RepYBH (BlockExpr b)): k)
evalStep ((IntLiteral n),env,(RepYBH (BlockExpr b)):k) = ((BlockExpr (evalRepYB b n)),[],k)

evalStep ((RepY e1 e2),env,k) = (e1,env,(HRepY e2 env) : k)
evalStep ((TileExpr t),env1,(HRepY e env2):k) = (e,env2,(RepYH (TileExpr t)): k)
evalStep ((IntLiteral n),env,(RepYH (TileExpr t)):k) = ((TileExpr (evalRepY t n)),[],k)
-- add under
evalStep ((AddU e1 e2),env,k) = (e1,env,(HAddU e2 env):k)
evalStep ((BlockExpr b),env1,(HAddU e env2):k) = (e,env2,(AddUH (BlockExpr b)):k)
evalStep ((List list),env,(AddUH (BlockExpr b)):k) = ((BlockExpr (evalAddU b (unwrapList (List list) env))),[],k)

evalStep ((AddUB e1 e2),env,k) = (e1,env,(HAddUB e2 env) : k)
evalStep ((BlockExpr b),env1,(HAddUB e env2):k) = (e,env2,(AddUBH (BlockExpr b)):k)
evalStep ((BlockExpr tiles),env,(AddUBH (BlockExpr b)):k) = ((BlockExpr (evalAddUB b tiles)),[],k)

-- add right
evalStep ((AddR e1 e2),env,k) = (e1,env,(HAddR e2 env):k)
evalStep ((BlockExpr b),env1,(HAddR e env2):k) = (e,env2,(AddRH (BlockExpr b)):k)
--evalStep (EmptyBlock,env1,(HAddR e env2):k) = (e,env2,(AddRH (BlockExpr evalEmptyBlock)):k)
--evalStep ((List (Id id)),env,(AddRH (BlockExpr b)):k) = ((BlockExpr (evalAddR b (unwrapList (Id id) env))),[],k)
evalStep ((List list),env,(AddRH (BlockExpr b)):k) = ((BlockExpr (evalAddR b (unwrapList (List list) env))),[],k)

evalStep ((AddRB e1 e2),env,k) = (e1,env,(HAddRB e2 env) : k)
evalStep ((BlockExpr b),env1,(HAddRB e env2):k) = (e,env2,(AddRBH (BlockExpr b)):k)
evalStep ((BlockExpr tiles),env,(AddRBH (BlockExpr b)):k) = ((BlockExpr (evalAddRB b tiles)),[],k)

-- scaling

evalStep ((Scale e1 e2),env,k) = (e1,env,(HScale e2 env):k)
evalStep ((TileExpr tile),env1,(HScale e2 env2):k) = (e2,env2,(ScaleH (TileExpr tile)):k)
evalStep ((BlockExpr b),env1,(HScale e2 env2):k) = (e2,env2,(ScaleH (BlockExpr b)):k)
evalStep ((IntLiteral n),env,(ScaleH (TileExpr tile)):k) = ((TileExpr (evalScale tile n)),[],k)
evalStep ((IntLiteral n),env,(ScaleH (BlockExpr b)):k) = ((BlockExpr (evalScaleBlock b n)),[],k)

--subtiling
evalStep ((Subtiling e1 e2 e3),env,k) = (e1,env,(HSubtile e2 e3 env):k)
evalStep ((TileExpr tile),env1,(HSubtile e2 e3 env2):k) = (e2,env2,(SubHtile (TileExpr tile) env1 e3 env2):k)
evalStep ((Pair (IntLiteral i) (IntLiteral j)),env2,(SubHtile (TileExpr tile) env1 e3 env3):k)
  = (e3,env3,(SubtileH (TileExpr tile) (Pair (IntLiteral i) (IntLiteral j))):k)
evalStep ((Pair e1 e2),env2,(SubHtile (TileExpr tile) env1 e3 env3):k)
  = (e3,env3,(SubtileH (TileExpr tile) (Pair (evaluationExpr e1 env2) (evaluationExpr e2 env2))):k)
evalStep ((IntLiteral n),env,(SubtileH (TileExpr tile) (Pair (IntLiteral i) (IntLiteral j))):k) = ((TileExpr (evalSubtile tile (i,j) n)),[],k)


-- to eval

-- getIndex
evalStep ((GetIndex e1 e2),env,k) = (e1,env,(HIndex e2 env):k)
evalStep ((TileExpr t),env1,(HIndex e2 env2):k) = (e2,env2,(IndexH (TileExpr t)):k)
evalStep ((Pair (IntLiteral i) (IntLiteral j)),env,(IndexH (TileExpr t)):k)
    = ((CellLiteral (getAtIndex t (i,j))),[],k)

-- pair
evalStep ((Pair e1 e2),env,k) = (e1,env,(HPair e2 env):k)
evalStep (e1,env1,(HPair e2 env2):k) | isTerminated e1 = (e2,env2,(PairH e1):k)
evalStep (e2,env,(PairH e1):k) | isTerminated e2 = ((Pair e1 e2),[],k)

-- tile negation,conjunction,union
evalStep ((TileNegate e),env,k) = (e,env,HTileN:k)
evalStep ((TileExpr tile),env,HTileN :k) = ((TileExpr (negateTile tile)),[],k)

evalStep ((TileConj e1 e2),env,k) = (e1,env,(HTileConj e2 env) :k)
evalStep ((TileExpr tile1),env1,(HTileConj e2 env2) :k) = (e2,env2,(TileConjH (TileExpr tile1)):k)
evalStep ((TileExpr tile2),env,((TileConjH (TileExpr tile1)):k))= ((TileExpr (conjTile tile1 tile2)),[],k)

evalStep ((TileUnion e1 e2),env,k) = (e1,env,(HTileUnion e2 env):k)
evalStep ((TileExpr tile),env1,(HTileUnion e2 env2):k) = (e2,env2,(TileUnionH (TileExpr tile)):k)
evalStep ((TileExpr tile2),env,(TileUnionH (TileExpr tile1)):k) = ((TileExpr (unionTile tile1 tile2)),[],k)

--makeblank

evalStep ((MakeBlank e),env,k) = (e,env,HBlank:k)
evalStep ((IntLiteral n),env,HBlank:k) = ((TileExpr (makeBlankTile n n)),[],k)


--reformat blocks to tile

evalStep ((Format e),env,k) = (e,env,HFormat:k)
evalStep ((BlockExpr block),env,HFormat:k) = ((BlockExpr (tileToBlock (blockToTile block))),[],k)

evalStep ((Comma e1 e2),env,k) = (e1,env,(HComma e2 env):k)
evalStep (e1,env1,(HComma e2 env2):k) | isTerminated e1 = (e2,env2,(CommaH e1):k)
evalStep (e2,env,(CommaH e1):k) | isTerminated e2 = ((Comma e1 e2),[],k)

evalStep ((List e),env,k) | isTerminated e == False = (e,env,HList:k)
evalStep (e,env,HList:k) | isTerminated e = ((List e),[],k)

evalStep ((Length e),env,k) = (e,env,HLength:k)
evalStep ((TileExpr tile),env,HLength:k) = ((IntLiteral (evalLength tile)),env,k)
evalStep ((BlockExpr b),env,HLength:k) = ((IntLiteral (evalLength b)),env,k)



-- helper functions

evalLength :: [a] -> Int
evalLength list = length list

makeBlankTile :: Int -> Int -> Tile
makeBlankTile 0 _ = []
makeBlankTile _ 0 = []
makeBlankTile size count = makeBlankRow size : makeBlankTile size (count - 1)

makeBlankRow :: Int -> [Cell]
makeBlankRow 0 = []
makeBlankRow n = C2 : makeBlankRow (n-1)

unionTile :: Tile -> Tile -> Tile
unionTile [] [] = []
unionTile [] tile = tile
unionTile tile [] = tile
unionTile (row1:tile1) (row2:tile2) = unionRow row1 row2 : unionTile tile1 tile2

unionRow :: [Cell] -> [Cell] -> [Cell]
unionRow [] [] = []
unionRow [] row = row
unionRow row [] = row
unionRow (c1:row1) (c2:row2) | c1 == C1 || c2 == C1 = C1 : unionRow row1 row2
                             | otherwise = C0 : unionRow row1 row2


negateTile :: Tile -> Tile
negateTile [] = []
negateTile (row:tile) = negateRow row : negateTile tile

negateRow :: [Cell] -> [Cell]
negateRow [] = []
negateRow (cell:row) = negateCell cell : negateRow row

negateCell :: Cell -> Cell
negateCell C0 = C1
negateCell C1 = C0

conjTile :: Tile -> Tile -> Tile
conjTile [] _ = []
conjTile (row1:tile1) (row2:tile2) = conjRow row1 row2 : conjTile tile1 tile2

conjRow :: [Cell] -> [Cell]-> [Cell]
conjRow [] _  = []
conjRow (c1:r1) (c2:r2) = conj c1 c2 : conjRow r1 r2

conj :: Cell -> Cell -> Cell
conj c1 c2 | c1 == c2 = c1
           | otherwise = C0

evalSubtile :: Tile -> (Int,Int) -> Int -> Tile
evalSubtile [] _ _ = []
evalSubtile _ _ 0 = []
evalSubtile tile (0,0) n | n == length tile = tile
                         | otherwise = evalSubtileRow tile (0,0) (0,0) n
evalSubtile tile (i,j) n = evalSubtileRow tile (0,0) (i,j) n

evalSubtileCol :: [Cell] -> (Int,Int) -> (Int,Int) -> Int -> [Cell]
evalSubtileCol [] _ _ _ = []
evalSubtileCol (cell: row) (cI,cJ) (i,j) n | cI < i = evalSubtileCol row ((cI + 1), cJ) (i,j) n
                                           | cI >= (i + n) = []
                                           | otherwise = cell : evalSubtileCol row ((cI + 1), cJ) (i,j) n
evalSubtileRow :: [[Cell]] -> (Int,Int) -> (Int,Int) -> Int -> [[Cell]]
evalSubtileRow [] _ _ _ = []
evalSubtileRow (row:tile) (cI,cJ) (i,j) n | cJ < j = evalSubtileRow tile (cI,(cJ + 1)) (i, j) n
                                          | cJ >= (j + n) = []
                                          | otherwise = evalSubtileCol row (0,0) (i,j) n : evalSubtileRow tile (cI,(cJ + 1)) (i, j) n

 {-
evalSubtileCol :: Tile -> (Int,Int) -> Int -> Int -> Tile
evalSubtileCol tile (i,j) n count | count < n = evalSubtileRow tile (i,j) n 0 : evalSubtileCol tile (i,(j+1)) n (count + 1)
                                  | otherwise = []


evalSubtileRow :: Tile -> (Int,Int) -> Int -> Int -> [Cell]
evalSubtileRow tile (i,j) n count | count < n = getAtIndex tile (i,j) : evalSubtileRow tile ((i+1),j) n (count + 1)
                                  | otherwise = []

getAtIndex :: Tile -> (Int,Int) -> Cell
getAtIndex [] _ = error "emptyTile"
getAtIndex tile index = getAtIndexHelper tile index (0,0)

getAtIndexHelper :: Tile -> (Int,Int) -> (Int,Int) -> Cell
getAtIndexHelper [] _ _ = error "index not in tile"
getAtIndexHelper (row:tile) (i,j) (cI,cJ) | cJ < j = getAtIndexHelper tile (i,j) (cI,(cJ + 1))
                                          | cJ == j = traverseRow row i cI

traverseRow :: [Cell] -> Int -> Int -> Cell
traverseRow [] _ _ = error "index not in tile"
traverseRow (cell:row) i cI | cI < i = traverseRow row i (cI + 1)
                            | cI == i = cell
-}

{-
evalSubtile :: Tile -> (Int,Int) -> Int -> Tile
evalSubtile [] _ _ = []
evalSubtile _ _ 0 = []
evalSubtile tile index size | index == (0,0) && size >= (length tile) = tile
                            | size > (length tile) = evalSubtile tile index (size - (size - (length tile) - (fst index)))
                            | otherwise = splitEvery size (evalSubtileHelper tile index (0,0) size)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list | n > 0 = (take n list) :  (splitEvery n (drop n list))
                  | otherwise = error []


evalSubtileHelper :: [[Cell]] -> (Int,Int) -> (Int,Int) -> Int -> [Cell]
evalSubtileHelper tile (i,j) (countI,countJ) size
  | (j + countJ) > (length tile) = []
  | (j + countJ) >= (size + j) = []
  | (i + countI) > (length tile) || (i + countI) >= (size + i)
    = evalSubtileHelper tile (i,j) (0,(countJ + 1)) size -- end of row jump to new col
  | (i + countI) < (i + size)
    = getAtIndex tile (i + countI,j) : (evalSubtileHelper tile (i,j) ((countI + 1), countJ) size)

-}
getAtIndex :: Tile -> (Int,Int) -> Cell
getAtIndex [] (i,j) = error []
getAtIndex tile (i,j) | i < 0 || i > (length tile - 1) || j < 0 || j > (length tile -1 ) = error []
                      | otherwise = getAtIndexHelper tile (i,j) (0,0)

getAtIndexHelper :: Tile -> (Int,Int) -> (Int,Int) -> Cell
getAtIndexHelper tile (i,j) (countI,countJ) = traverseCol (traverseRow tile j countJ) i countI

traverseCol :: [Cell] -> Int -> Int -> Cell
traverseCol (cell:row) i counter | i == counter = cell
                                  | otherwise = traverseCol row i (counter + 1)

traverseRow :: [[Cell]] -> Int -> Int -> [Cell]
traverseRow (row:tile) i counter | i == counter = row
                                | otherwise = traverseRow tile i (counter + 1)
evalScaleBlock :: Block -> Int -> Block
--evalScaleBlock b n = tileToBlock(blockToTile (evalScaleBlockHelper b n))
evalScaleBlock b n = evalScaleBlockHelper b n

evalScaleBlockHelper :: Block -> Int -> Block
evalScaleBlockHelper [] _ = []
evalScaleBlockHelper (row:block) n = evalScaleTileRow row n : evalScaleBlock block n

evalScaleTileRow :: [[[Cell]]] -> Int -> [[[Cell]]]
evalScaleTileRow [] _ = []
evalScaleTileRow (tile:row) n = evalScale tile n : evalScaleTileRow row n

evalScale :: Tile -> Int -> Tile
evalScale [] _ = []
evalScale tile 0 = tile
evalScale tile n = scaleLength (scaleWidth tile n) n

scaleLength :: [[Cell]] -> Int -> [[Cell]]
scaleLength [] _ = []
scaleLength (row:rows) n = repeatRowVertical row n ++ (scaleLength rows n)

repeatRowVertical :: [Cell] -> Int -> [[Cell]]
repeatRowVertical [] _ = []
repeatRowVertical row 0 = []
repeatRowVertical row n = row : (repeatRowVertical row (n-1))

scaleWidth :: [[Cell]] -> Int -> [[Cell]]
scaleWidth [] _ = []
scaleWidth (row:rows) n = scaleRow row n : (scaleWidth rows n)

scaleRow :: [Cell] -> Int -> [Cell]
scaleRow [] _ = []
scaleRow row 0 = []
--scaleRow row 1 = []
scaleRow (r:row) n = (repeatCell r n) ++ (scaleRow row n)

repeatCell :: Cell -> Int -> [Cell]
--repeatCell [] _ = []
repeatCell cell 1 = [cell]
repeatCell cell n = cell : (repeatCell cell (n-1))

unwrapList :: ExprData -> Environment -> [ExprData]
unwrapList (List (Comma e1 e2)) env = unwrapList e1 env ++ unwrapList e2 env
unwrapList (Comma e1 e2) env = unwrapList e1 env ++ unwrapList e2 env
--unwrapList (List e) env | isTerminated e = [e]
unwrapList (List e) env = unwrapList e env
unwrapList e env | isTerminated e = [e]
                 | otherwise = [(evaluationExpr e env)]

evalAddU :: Block -> [ExprData] -> Block
evalAddU block1 block2 = evalAddUHelper block1 block2
--evalAddU block1 block2 = tileToBlock (blockToTile (evalAddUHelper block1 block2))

evalAddUHelper :: Block -> [ExprData] -> Block
evalAddUHelper [] [] = []
evalAddUHelper b [] = b
--evalAddUHelper [] tiles = [(unwrapTileExprList tiles)]
evalAddUHelper [[]] tiles = [(unwrapTileExprList tiles)]
evalAddUHelper block tileList = block ++ [(unwrapTileExprList tileList)]

unwrapTileExprList :: [ExprData] -> [Tile]
unwrapTileExprList [] = []
unwrapTileExprList ((TileExpr tile):list) = tile : unwrapTileExprList list
--unwrapTileExprList ((RefY (TileExpr tile)):list) = tile : unwrapTileExprList list
unwrapTileExprList (e:list) = unwrapTileExprList list

evalAddUB :: Block -> Block -> Block
evalAddUB block1 block2 = evalAddUBHelper block1 block2
--evalAddUB block1 block2 = tileToBlock (blockToTile (evalAddUBHelper block1 block2))

evalAddUBHelper :: Block -> Block -> Block
evalAddUBHelper [] [] = []
evalAddUBHelper b [] = b
evalAddUBHelper [] b = b
evalAddUBHelper block block2 =  block ++ block2

evalAddR :: [[Tile]] -> [ExprData] -> Block
evalAddR block1 block2 = evalAddRHelper block1 block2
--evalAddR block1 block2 = tileToBlock (blockToTile (evalAddRHelper block1 block2))

evalAddRHelper :: [[Tile]] -> [ExprData] -> Block
evalAddRHelper [] [] = []
evalAddRHelper block [] = block
evalAddRHelper [[]] tiles = map (: []) (unwrapTileExprList tiles)
evalAddRHelper (row:block) ((TileExpr tile) : tiles) = (row ++ [tile]) : evalAddRHelper block tiles


tileToBlock :: [[Cell]] -> Block
tileToBlock [] = []
tileToBlock tile = [[tile]]

evalAddRB :: Block -> Block -> Block
evalAddRB block1 block2 = evalAddRBHelper block1 block2
--evalAddRB block1 block2 = tileToBlock (blockToTile (evalAddRBHelper block1 block2))

evalAddRBHelper :: Block -> Block -> Block
evalAddRBHelper [] [] = []
evalAddRBHelper block [] = block
evalAddRBHelper [] block = block
evalAddRBHelper (row:block) (row2:block2) = (row ++ row2) : evalAddRBHelper block block2

evalRefXLine :: [a] -> [a] -> [a]
evalRefXLine [] acc = acc
evalRefXLine (c:cs) acc = evalRefXLine cs (c:acc)

evalRefXB :: Block -> Block
evalRefXB [] = []
evalRefXB (row:block) = eXB row : evalRefXB block

eXB :: [Tile] -> [Tile]
eXB [] = []
eXB (tile:row) = evalRefX tile : eXB row

evalRefX :: [[a]] -> [[a]]
evalRefX [] = []
evalRefX xs = reverse xs
--evalRefX xs = reverse (evalRefXHelper xs)


evalRefXHelper :: [[a]] -> [[a]]
evalRefXHelper [] = []
evalRefXHelper (cs:css) = reverse cs : (evalRefXHelper css)
--evalRefXHelper (cs:css) = (evalRefXLine cs []) : (evalRefXHelper css)

evalRefYB :: Block -> Block
evalRefYB block = evalRefYBHelper block
--evalRefYB block = tileToBlock (blockToTile (evalRefYBHelper block))

evalRefY :: [[a]]  -> [[a]]
evalRefY []  = []
evalRefY css  = map reverse css

evalRefYBHelper :: Block -> Block
evalRefYBHelper [] = []
evalRefYBHelper (row:block) = e row : evalRefYBHelper block

e :: [Tile] -> [Tile]
e [] = []
e (tile:row) = evalRefY tile : e row
 {-
evalRefYHelper :: [a]  -> [a]
evalRefYHelper [] = []
evalRefYHelper (cs:css)) = reverse cs : evalRefYHelper
-}
evalRotate :: Tile -> Tile
evalRotate [] = []
evalRotate t = transpose (reverse t)

evalRepXB :: Block -> Int -> Block
--evalRepXB block n = tileToBlock (blockToTile (evalRepX block n))
evalRepXB block n = evalRepX block n

evalRepX :: [[a]] -> Int -> [[a]]
evalRepX b n =  (evalRepXHelper b n)

evalRepXHelper :: [[a]] -> Int -> [[a]]
evalRepXHelper [] _ = []
evalRepXHelper t 1 = t
evalRepXHelper t 2 = concatSelf t
evalRepXHelper t n = concatX t (evalRepXHelper t (n-1))

evalRX :: [[a]] -> Int -> [[a]]
evalRX [] _ = []
evalRX _ 0 = []
evalRX as n = as ++ evalRX as (n-1)


concatX :: [[a]] -> [[a]] -> [[a]]
concatX [] [] = []
concatX as [] = as
concatX [] as = as
concatX (a:as) (b:bs) = (a ++ b) : concatX as bs

concatSelf :: [[a]] -> [[a]]
concatSelf [] = []
concatSelf (a : as) = (a ++ a) : (concatSelf as)

evalRepYB :: Block -> Int -> Block
evalRepYB block n = evalRepY block n
--evalRepYB block n = tileToBlock (blockToTile (evalRepY block n))

evalRepY :: [a] -> Int -> [a]
evalRepY [] _ = []
evalRepY t 1 = t
evalRepY t n = t ++ (evalRepY t (n-1))

evalEmptyBlock :: Block
evalEmptyBlock = [[]]