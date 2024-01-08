{
    module Lexer_2 where
}

%wrapper "posn"
$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
$white+      ;
    "--".*       ;
    \[           { \p s -> TokenLeftSqBrack p}
    \]           { \p s -> TokenRightSqBrack p}
    while        { \p s -> TokenWhile p}
    if           { \p s -> TokenIf p}
    then         { \p s -> TokenThen p}
    else         { \p s -> TokenElse p}
    \=           { \p s -> TokenEqAssign p}
    \=\=         { \p s -> TokenEqCheck p}
    \+           { \p s -> TokenAdd p}
    \-           { \p s -> TokenSubtract p}
    \\           { \p s -> TokenDivide p}
    \*           { \p s -> TokenMultiply p}
    \%           { \p s -> TokenModulus p}
    \^           { \p s -> TokenExp p}
    \<           { \p s -> TokenLessThan p}
    \>           { \p s -> TokenGreaterThan p}
    \&\&         { \p s -> TokenConjunction p}
    \|\|         { \p s -> TokenUnion p}
    true         { \p s -> TokenTrue p}
    false        { \p s -> TokenFalse p}
    \~           { \p s -> TokenNegation p}
    \;           { \p s -> TokenSemiColon p}
    \:           { \p s -> TokenColon p}
    \(           { \p s -> TokenLeftPara p}
    \)           { \p s -> TokenRightPara p}
    \{           { \p s -> TokenLeftCurlyBr p}
    \}           { \p s -> TokenRightCurlyBr p}
    \"           { \p s -> TokenQuote p}
    \,           { \p s -> TokenComma p}
    return       { \p s -> TokenReturn p }
    Tile         { \p s -> TokenTileType p}
    Block        { \p s -> TokenBlockType p}
    Integer      { \p s -> TokenIntType p}
    Bool         { \p s -> TokenBoolType p}
    Cell         { \p s -> TokenCellType p}
    String       { \p s -> TokenStringType p}
    addUnder     { \p s -> TokenAddU p}
    addRight     { \p s -> TokenAddR p}
    addUnderBlock     { \p s -> TokenAddUB p}
    addRightBlock     { \p s -> TokenAddRB p}
    repeatX      { \p s -> TokenRepeatX p}
    repeatY      { \p s -> TokenRepeatY p}
    repeatXBlock      { \p s -> TokenRepeatXBlock p}
    repeatYBlock      { \p s -> TokenRepeatYBlock p}
    reflectYBlock   { \p s -> TokenReflectYBlock p}
    reflectXBlock   { \p s -> TokenReflectXBlock p}
    reflectX     { \p s -> TokenReflectX p}
    reflectY     { \p s -> TokenReflectY p}
    rotate       { \p s -> TokenRotate p}
    scale        { \p s -> TokenScaling p}
    subtiling    { \p s -> TokenSubtiling p}
    readTileFile { \p s -> TokenReadTF p }
    tileNegate   { \p s -> TokenTileNegate p}
    tileUnion    { \p s -> TokenTileUnion p}
    tileConj     { \p s -> TokenTileConj p}
    getIndex     { \p s -> TokenGetIndex p}
    makeBlank    { \p s -> TokenMakeBlank p}
    reformat     { \p s -> TokenReformat p}
    read         { \p s -> TokenRead p}
    fst          { \p s -> TokenFst p}
    snd          { \p s -> TokenSnd p}
    emptyBlock   { \p s -> TokenEmptyBlock p}
    length       { \p s -> TokenLength p}



    $digit+                         { \p s -> TokenInt p (read s) }
    [A-Z] $alpha*                   { \p s -> TokenType p s }
    c0                              { \p s -> TokenC0 p}
    c1                              { \p s -> TokenC1 p}
    c2                              { \p s -> TokenC2 p}
    $alpha [$alpha $digit \_ \’]*   { \p s -> TokenVar p s}




--tile         { \p s -> TokenTile p}
--block        { \p s -> TokenBlock p}
{
data Token =
    TokenType AlexPosn String |
    TokenLeftSqBrack AlexPosn |
    TokenRightSqBrack AlexPosn |
    TokenIf AlexPosn |
    TokenWhile AlexPosn |
    TokenThen AlexPosn |
    TokenElse AlexPosn |
    TokenEqAssign AlexPosn |
    TokenEqCheck AlexPosn |
    TokenAdd AlexPosn |
    TokenSubtract AlexPosn |
    TokenMultiply AlexPosn |
    TokenDivide AlexPosn|
    TokenModulus AlexPosn |
    TokenExp AlexPosn |
    TokenLessThan AlexPosn |
    TokenGreaterThan AlexPosn |
    TokenConjunction AlexPosn |
    TokenUnion AlexPosn |
    TokenTrue AlexPosn |
    TokenFalse AlexPosn |
    TokenNegation AlexPosn |
    TokenSemiColon AlexPosn |
    TokenColon AlexPosn |
    TokenLeftPara AlexPosn |
    TokenRightPara AlexPosn |
    TokenLeftCurlyBr AlexPosn |
    TokenRightCurlyBr AlexPosn |
    TokenQuote AlexPosn |
    TokenComma AlexPosn |
    TokenReturn AlexPosn |
    TokenInt AlexPosn Int |
    TokenC0 AlexPosn |
    TokenC1 AlexPosn |
    TokenC2 AlexPosn |
    TokenSubtiling AlexPosn |
    TokenReflectX AlexPosn |
    TokenReflectY AlexPosn |
    TokenRotate AlexPosn |
    TokenScaling AlexPosn |
    TokenTileType AlexPosn |
    TokenBlockType AlexPosn |
    TokenBoolType AlexPosn |
    TokenStringType AlexPosn |
    TokenIntType AlexPosn |
    TokenCellType AlexPosn |
    TokenAddU AlexPosn |
    TokenAddR AlexPosn |
    TokenAddUB AlexPosn |
    TokenAddRB AlexPosn |
    TokenRepeatX AlexPosn |
    TokenRepeatY AlexPosn |
    TokenRepeatXBlock AlexPosn |
    TokenRepeatYBlock AlexPosn |
    TokenReadTF AlexPosn |
    TokenTileNegate AlexPosn |
    TokenTileConj AlexPosn |
    TokenTileUnion AlexPosn |
    TokenGetIndex AlexPosn |
    TokenMakeBlank AlexPosn |
    TokenRead AlexPosn |
    TokenFst AlexPosn |
    TokenSnd AlexPosn |
    TokenReflectYBlock AlexPosn |
    TokenReflectXBlock AlexPosn |
    TokenEmptyBlock AlexPosn |
    TokenLength AlexPosn |
    TokenReformat AlexPosn |
    TokenVar AlexPosn String
    deriving (Eq , Show)


    {- TokenTileType AlexPosn |
    TokenIntegerType AlexPosn |
    TokenStringType AlexPosn |
    TokenBoolType AlexPosn |
    TokenCellType AlexPosn |
    TokenLineType AlexPosn |
    TokenPatternType AlexPosn |
    TokenBlockPatternType AlexPosn |
    TokenRowType AlexPosn |
    TokenGridType AlexPosn |
    -}
    --TokenDot AlexPosn |

tokenPosn :: Token -> String
tokenPosn (TokenLeftSqBrack (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRightSqBrack (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenThen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEqAssign (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEqCheck (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAdd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSubtract (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDivide (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMultiply (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenModulus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenExp (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLessThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGreaterThan (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenConjunction (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenUnion (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTrue (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFalse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNegation (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenColon (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSemiColon (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLeftPara (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRightPara (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLeftCurlyBr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRightCurlyBr (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenQuote (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComma (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReturn (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt (AlexPn a l c ) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenC0 (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenC1 (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenC2 (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReflectX (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReflectY (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReflectXBlock (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReflectYBlock (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRotate (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenScaling (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSubtiling (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTileType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBlockType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIntType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenStringType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBoolType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCellType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAddU (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAddR (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAddUB (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAddRB (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRepeatX (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRepeatY (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRepeatXBlock (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRepeatYBlock (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReadTF (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTileConj (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTileNegate (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTileUnion (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGetIndex (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMakeBlank (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFst (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRead (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEmptyBlock (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReformat (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLength (AlexPn a l c)) = show(l) ++ ":" ++ show(c)


--tokenPosn (TokenTileType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
--tokenPosn (TokenIntegerType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
--tokenPosn (TokenStringType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
--tokenPosn (TokenBoolType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
--tokenPosn (TokenCellType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
--tokenPosn (TokenLineType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
--tokenPosn (TokenPatternType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
--tokenPosn (TokenRowType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
--tokenPosn (TokenGridType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
--tokenPosn (TokenDot (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
--tokenPosn (TokenRefectX (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
--tokenPosn (TokenReflectY (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
--tokenPosn (TokenRotate (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
--tokenPosn (TokenScaling (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
--tokenPosn (TokenSubtiling (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
}
