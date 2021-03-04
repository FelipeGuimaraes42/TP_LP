%%

%name PlcParser

%pos int

%term VAR
    | PLUS | MINUS | MULTI | DIV | EQ | NEG | DIF
    | AND | NOT | BLT | BLE | 
    | SEMIC | DBCOL | COMMA
    | LPAR | RPAR | LBKT | RBKT | LBRC | RBRC
    | NAME of string | CINT

%right SEMIC DBCOL
%left AND EQ DIF BLT BLE PLUS MINUS MULTI DIV LBKT

%nonterm Prog of expr | Expr of expr | Decl of expr (?) | AtomExpr of expr |
Const of Expr | AppExpr of expr (?) | 

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
    | : Decl SEMIC Prog

Decl : VAR NAME EQ Expr (Let (NAME, Expr))
    | :
    | :

Expr : AtomExpr (AtomExpr)
    | : AppExpr (AppExpr)
    | :
    | :
    | : NOT Expr (Prim1("not", Expr)) (?)
    | : NEG Expr (Prim1("~", Expr)) (?)
    | : 
    | :
    | :
    | :  
    | : Expr AND Expr (Prim2("andalso", Expr1, Expr2))
    | : Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | : Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | : Expr MULTI Expr (Prim2("*", Expr1, Expr2))
    | : Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | : Expr EQ Expr (Prim2("=", Expr1, Expr2))
    | : Expr DIF Expr (Prim2("!=", Expr1, Expr2)) (?)
    | : Expr BLT Expr (Prim2("<", Expr1, Expr2))
    | : Expr BLE Expr (Prim2("<=", Expr1, Expr2))
    | : Expr DBCOL Expr (Prim2("::", Expr1, Expr2))
    | : Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
    | : 

AtomExpr : Const (Const)
    | : NAME (Var(NAME))
    | : LBRC Prog RBRC
    | : LPAR Expr RPAR
    | : 
    | : 

AppExpr : AtomExpr AtomExpr
    | : AppExpr AtomExpr

Const : Expr COMMA Expr
    | : 
    | : LPAR RPAR
    | : 