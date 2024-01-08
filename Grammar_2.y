{
module Grammar_2 where
import Lexer_2

-- this is stuff that tells the parse how to translate the tokens into stuff the interpreter uses
-- left is what the lexer has read and given to parser
-- right is how the parser constructs the data to be then used by the interpreter

-- to write something as a program surround it in {}

}

%name parseTL
%tokentype { Token }
%error { parseError}
%token
        Tile 		 {TokenTileType _}
        Block        {TokenBlockType _}
        Integer      {TokenIntType _ }
        Bool         {TokenBoolType _}
        Cell         {TokenCellType _}
        String       {TokenStringType _}
        '[' 		 { TokenLeftSqBrack _ }
        ']' 		 { TokenRightSqBrack _ }
        while 		 { TokenWhile _ }
        if 			 { TokenIf _ }
        then         { TokenThen _}
        else 		 { TokenElse _ }
        '=' 		 { TokenEqAssign _ }
        '==' 		 { TokenEqCheck _ }
        '+' 		 { TokenAdd _ }
        '-' 		 { TokenSubtract _ }
        '/' 		 { TokenDivide _ }
        '*' 		 { TokenMultiply _ }
        '%' 		 { TokenModulus _ }
        '^' 		 { TokenExp _ }
        '<' 		 { TokenLessThan _ }
        '>' 		 { TokenGreaterThan _  }
        '&&'    	 { TokenConjunction _  }
        '||'         { TokenUnion _ }
        true 		 { TokenTrue _ }
        false 		 { TokenFalse _ }
        '~' 		 { TokenNegation _ }
        ';' 		 { TokenSemiColon _ }
        ':'          { TokenColon _ }
        ',' 		 { TokenComma _ }
        '(' 		 { TokenLeftPara _ }
        ')' 		 { TokenRightPara _ }
        '{' 		 { TokenLeftCurlyBr _ }
        '}' 		 { TokenRightCurlyBr _ }
        '"' 		 { TokenQuote _ }
        c0 	         { TokenC0 _ }
        c1           { TokenC1 _}
        c2           { TokenC2 _}
        return 		 { TokenReturn _ }
        reflectX 	 { TokenReflectX _}
        reflectY 	 { TokenReflectY _}
        reflectXBlock 	 { TokenReflectXBlock _}
        reflectYBlock 	 { TokenReflectYBlock _}
        rotate 		 { TokenRotate _}
        repeatX 	 { TokenRepeatX _}
        repeatY 	 { TokenRepeatY _}
        repeatXBlock 	 { TokenRepeatXBlock _}
        repeatYBlock 	 { TokenRepeatYBlock _}
        length       { TokenLength _}
        addUnder     { TokenAddU _}
        addRight     { TokenAddR _}
        addUnderBlock     { TokenAddUB _}
        addRightBlock     { TokenAddRB _}
        scale 		 { TokenScaling _}
        subtiling 	 { TokenSubtiling _}
        readTileFile { TokenReadTF _}
        getIndex     { TokenGetIndex _}
        tileConj     { TokenTileConj _}
        tileNegate   { TokenTileNegate _}
        tileUnion    { TokenTileUnion _}
        makeBlank    { TokenMakeBlank _}
        fst          { TokenFst _}
        snd          { TokenSnd _}
        read         { TokenRead _}
        int 		 { TokenInt _ $$ }
        var 		 { TokenVar _ $$ }
        type 		 { TokenType _ $$}
        emptyBlock   { TokenEmptyBlock _}
        reformat     { TokenReformat p}

 %right else
 %left '&&'
 %left '||'
 %left '=='
 %nonassoc ','
 %left '[' '('
 %nonassoc '<' '>' '='
 %left '+' '-'
 %right '*' '/' '%'
 %right '~'
 %right '^'
 %left ','

 %%

Program : '{' Code '}'                     {Prog $2}

Code : Statement Code                      {CodeBlocks $1 $2}
     | Statement                           {CodeBlock $1}

Statement : if Expr then Code else Code ';'   {If $2 $4 $6}
       | if Expr then Code ';'              {JustIf $2 $4}
       | read Identifier Expr           {Read $2 $3}
       | return Expr                          {Return $2}
       | TLType Identifier '=' Expr           {Assign $1 $2 $4}
       | Identifier '=' Expr                  {Reassign $1 $3}
       | while Expr then '{' Code '}' ';'            {While $2 $5}

Identifier : var                           {Var $1}

Expr : int                                  {IntLiteral $1}
  | c0                                   {CellLiteral C0}
  | c1                                   {CellLiteral C1}
  | c2                                   {CellLiteral C2}
  | true                                 {BoolLiteral True}
  | false                                {BoolLiteral False}
  | Identifier                           {Id $1}
  | '"' var '"'                         {StringExpr $2}
  | '(' Expr ',' Expr ')'               {Pair $2 $4}
  | fst Expr                             {Fst $2 }
  | snd Expr                             {Snd $2 }
  | '[' Expr ']'                         {List $2}
  | Expr ':' Expr                        {Cons $1 $3}
  | Expr '>' Expr 					     {GrThan $1 $3}
  | Expr '<' Expr 					     {LsThan $1 $3}
  | Expr '==' Expr 				 	     {Equal  $1 $3}
  | '~' Expr 						     {Negtn $2}
  | Expr '&&' Expr 					     {Conj $1 $3}
  | Expr '||' Expr 					     {Union $1 $3}
  | Expr '^' '(' Expr ')' 				 {Exp $1 $4}
  | Expr '*' Expr 					     {Mult $1 $3}
  | Expr '/' Expr 					     {Div $1 $3}
  | Expr '%' Expr 					     {Mod $1 $3}
  | Expr '+' Expr 					     {Add $1 $3}
  | Expr '-' Expr 					     {Sub $1 $3}
  | reflectX Expr                        {RefX $2}
  | reflectXBlock Expr                        {RefXB $2}
  | reflectY Expr                        {RefY $2}
  | reflectYBlock Expr                        {RefYB $2}
  | rotate Expr                          {Rotate $2}
  | repeatX Expr Expr                    {RepX $2 $3}
  | repeatXBlock Expr Expr                    {RepXB $2 $3}
  | repeatY Expr Expr                    {RepY $2 $3}
  | repeatYBlock Expr Expr                    {RepYB $2 $3}
  | addUnder Expr Expr             {AddU $2 $3}
  | addRight Expr Expr             {AddR $2 $3}
  | addRightBlock Expr Expr                   {AddRB $2 $3}
  | addUnderBlock Expr Expr                   {AddUB $2 $3}
  | addUnderBlock Expr Expr                   {AddUB $2 $3}
  | scale Expr Expr                      {Scale $2 $3}
  | getIndex Expr Expr                   {GetIndex $2 $3}
  | tileConj Expr Expr                   {TileConj $2 $3}
  | tileNegate Expr                      {TileNegate $2}
  | tileUnion Expr Expr                      {TileUnion $2 $3}
  | makeBlank Expr                       {MakeBlank $2}
  | subtiling Expr Expr Expr             {Subtiling $2 $3 $4}
  | emptyBlock                           {EmptyBlock}
  | reformat Expr                            {Format $2}
  | length Expr                              {Length $2}
  |'(' Expr ')' 		  %shift 				 {BrackExpr $2}
  | Expr ',' Expr         %shift          {Comma $1 $3}

TLType : Bool                            {TLBool}
    | Integer                            {TLInt}
    | Cell                               {TLCell}
    | Tile                               {TLTile}
    | Block                              {TLBlock}
    | String                             {TLString}

{

 -- this is stuff that the interpreter will read during evaluation
 -- stuff with Data is a type
 -- stuff without is the constructor

parseError :: [Token] -> a
parseError [] = error "Unknown parse error"
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t) ++ "\n" ++ show t)

data ProgramData = Prog CodeData
        deriving (Show, Eq)

data CodeData = CodeBlock StatementData
    | CodeBlocks StatementData CodeData
    deriving (Show, Eq)

data StatementData = If ExprData CodeData CodeData
    | JustIf ExprData CodeData
    | Read IdentifierData ExprData
    | Assign TLTypeData IdentifierData ExprData
    | Reassign IdentifierData ExprData
    | While ExprData CodeData
    | Return ExprData
        deriving (Show, Eq)

data ExprData = BrackExpr ExprData
    | TileExpr Tile
    | CellLiteral Cell
    | IntLiteral Int
    | BoolLiteral Bool
    | StringExpr String
    | List ExprData
    | Cons ExprData ExprData
    | BlockExpr Block
    | Pair ExprData ExprData
    | Fst ExprData
    | Snd ExprData
    | Comma ExprData ExprData
    | TileLsExpr [Tile]
    | ReturnExpr ExprData
    | GrThan ExprData ExprData
    | LsThan ExprData ExprData
    | Equal ExprData ExprData
    | Negtn ExprData
    | Conj ExprData ExprData
    | Union ExprData ExprData
    | Exp ExprData ExprData
    | Mult ExprData ExprData
    | Div ExprData ExprData
    | Mod ExprData ExprData
    | Add ExprData ExprData
    | Sub ExprData ExprData
    | RefX ExprData
    | RefY ExprData
    | RefYB ExprData
    | RefXB ExprData
    | Rotate ExprData
    | RepX ExprData ExprData
    | RepXB ExprData ExprData
    | RepY ExprData ExprData
    | RepYB ExprData ExprData
    | AddU ExprData ExprData
    | AddUB ExprData ExprData
    | AddR ExprData ExprData
    | AddRB ExprData ExprData
    | Scale ExprData ExprData
    | Subtiling ExprData ExprData ExprData
    | GetIndex ExprData ExprData
    | TileConj ExprData ExprData
    | TileNegate ExprData
    | TileUnion ExprData ExprData
    | MakeBlank ExprData
    | Format ExprData
    | Id IdentifierData
    | Length ExprData
    | EmptyBlock
        deriving (Show, Eq)

data TLTypeData = TLCell | TLInt | TLTile | TLBlock | TLBool | TLString
    deriving (Show, Eq)

data Cell = C0 | C1 | C2
    deriving (Show, Eq)

type Tile = [[Cell]]
type Block = [[Tile]]
data IdentifierData = Var String
        deriving (Show, Eq)

type Environment = [ (String,ExprData) ]


}