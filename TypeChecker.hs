module TypeChecker where     
import Grammar_2


type TypeEnv = [(String , TLTypeData)]

getBind :: String -> TypeEnv -> TLTypeData
getBind str [] = error "Cannot bind variable"
getBind str ((x , t) : typeEnv) | str == x = t 
                                | otherwise = getBind str typeEnv 

addBind :: String -> TLTypeData -> TypeEnv -> TypeEnv
addBind str t typeEnv = (str , t) : typeEnv 

typePatternProg :: TypeEnv -> ProgramData -> (TLTypeData,TypeEnv)
typePatternProg env (Prog c) = typePatternCode env c

typePatternCode :: TypeEnv -> CodeData -> (TLTypeData, TypeEnv)
typePatternCode env (CodeBlock b) = typePatternStatement env b
typePatternCode env (CodeBlocks statement b) = typePatternCode env' b
                                               where (tlType,env') = typePatternStatement env statement

typePatternStatement :: TypeEnv -> StatementData -> (TLTypeData, TypeEnv)
typePatternStatement env (If e1 code1 code2) | (typePatternCode env code1) == (typePatternCode env code2) && (typePattern env e1 == TLBool) = typePatternCode env code1
                                             | otherwise = error "type error in if statement"

typePatternStatement env (JustIf e1 code) | typePattern env e1 == TLBool = typePatternCode env code
                                          | otherwise = error "type error in if statement"
typePatternStatement env (Assign t (Var id) e) | (t == typePattern env e) = (t, ((id, t) : env))
                                               | otherwise = error "type error"
typePatternStatement env (Reassign (Var id) e) | getBind id env == typePattern env e = (typePattern env e, (id,typePattern env e) : env)
                                               | otherwise = error "inconsistent types in reassignment"
typePatternStatement env (Return e) = (typePattern env e, env)
typePatternStatement env (Read (Var id) e2) = (TLTile, (id,TLTile) : env)
typePatternStatement env (While e code) | typePattern env e == TLBool = typePatternCode env code
                                        | otherwise = error "While needs a bool"




typePattern :: TypeEnv -> ExprData -> TLTypeData
typePattern env (TileExpr t) = TLTile
typePattern env (BlockExpr b) = TLBlock
typePattern env (StringExpr e1) = TLString 
typePattern env (IntLiteral e1) = TLInt
typePattern env (BoolLiteral e1) = TLBool
typePattern env (CellLiteral e1) = TLCell



typePattern env (GrThan e1 e2) | (TLInt , TLInt) == (typePattern env e1 , typePattern env e2) = TLBool 
typePattern env (LsThan e1 e2) | (TLInt , TLInt) == (typePattern env e1 , typePattern env e2) = TLBool 

typePattern env (Equal e1 e2)  | t1 == t2 = TLBool
  where (t1 , t2) = ( typePattern env e1 , typePattern env e2)

typePattern env (Negtn e1) | TLBool == (typePattern env e1) = TLBool 
typePattern env (Conj e1 e2) | (TLBool , TLBool) == (typePattern env e1 , typePattern env e2) = TLBool
typePattern env (Union e1 e2) | (TLBool , TLBool) == (typePattern env e1 , typePattern env e2) = TLBool

typePattern env (Mult e1 e2) | (TLInt, TLInt) == (typePattern env e1 , typePattern env e2) = TLInt
typePattern env (Div e1 e2) | (TLInt, TLInt) == (typePattern env e1 , typePattern env e2) = TLInt
typePattern env (Mod e1 e2) | (TLInt, TLInt) == (typePattern env e1 , typePattern env e2) = TLInt
typePattern env (Add e1 e2) | (TLInt, TLInt) == (typePattern env e1 , typePattern env e2) = TLInt
typePattern env (Sub e1 e2) | (TLInt, TLInt) == (typePattern env e1 , typePattern env e2) = TLInt

typePattern env (RefX e1) | (TLTile) == (typePattern env e1) = TLTile 
typePattern env (RefXB e1) | (TLBlock) == (typePattern env e1) = TLBlock 
typePattern env (RefY e1) | (TLTile) == (typePattern env e1) = TLTile 
typePattern env (RefYB e1) | (TLBlock) == (typePattern env e1) = TLBlock 

typePattern env (Rotate e1) | TLTile == (typePattern env e1) = TLTile 

typePattern env (RepX e1 e2) | (TLTile , TLInt) == (typePattern env e1 , typePattern env e2) = TLTile 
typePattern env (RepXB e1 e2) | (TLBlock , TLInt) == (typePattern env e1 , typePattern env e2) = TLBlock 
typePattern env (RepY e1 e2) | (TLTile , TLInt) == (typePattern env e1 , typePattern env e2) = TLTile 
typePattern env (RepYB e1 e2) | (TLBlock , TLInt) == (typePattern env e1 , typePattern env e2) = TLBlock 
 

typePattern env (AddUB e1 e2) | (TLBlock , TLBlock) == (typePattern env e1 , typePattern env e2) = TLBlock 
typePattern env (AddRB e1 e2) | (TLBlock , TLBlock) == (typePattern env e1 , typePattern env e2) = TLBlock 

typePattern env (Scale e1 e2) | (TLTile , TLInt) == (typePattern env e1 , typePattern env e2) = TLTile 


typePattern env (TileConj e1 e2) | (TLTile , TLTile) == (typePattern env e1 , typePattern env e2) = TLTile 
typePattern env (TileNegate e1) | TLTile == (typePattern env e1) = TLTile 

typePattern env (Length e1) | TLInt == (typePattern env e1) = TLInt

typePattern typeEnv _ = error "Type Error"

printRes :: TLTypeData -> String 
printRes TLTile = "Type Tile"
printRes TLInt = "Type Int"
printRes TLBool = "Type Bool"
printRes TLBlock = "Type Block"
printRed TLString = "Type String"


