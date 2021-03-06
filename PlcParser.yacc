%%

%name PlcParser

%pos int

%term VAR
    | PLUS | MINUS | MULTI | DIV | EQ | NEG | DIF
    | TRUE | FALSE | AND | NOT | BLT | BLE
    | SEMIC | COLON | DBCOL | COMMA
    | LPAR | RPAR | LBKT | RBKT | LBRC | RBRC
    | NAME of string | INTEGER of int | BOOLEAN of bool
    | NIL of unit
    | FUN | REC
    | IF | THEN | ELSE | MATCH | WITH
    | HD | TL | ISE | PRINT
    | FN | END | ARROW | DBARROW
    | UNDSCR | PIPE
    | EOF

%right SEMIC ARROW DBCOL
%left AND EQ DIF BLT BLE PLUS MINUS MULTI DIV LBKT

%nonterm Prog of expr | Decl of expr (?) | Expr of expr | AtomExpr of expr
    | AppExpr of expr | Const of plcType (?) |  Comps of expr | Matchexpr of expr
    | Condexpr of expr | Args of plcType (?) | Params of plcType 
    | TypedVar of plcVal | Type of AtomType | AtomType of plcType Type of plcType

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
    | Decl SEMIC Prog

Decl : VAR NAME EQ Expr (Let (NAME, Expr))
    | FUN NAME Args EQ Expr (?)
    | FUN REC NAME Args COLON Type EQ Expr ((?)primeira aparicao de type e args?)

Expr : AtomExpr (AtomExpr)
    | AppExpr (AppExpr)
    | IF Expr THEN Expr ELSE Expr (?)
    | MATCH Expr WITH Matchexpr ((?)primeira aparicao de Matchexpr?)
    | NOT Expr (Prim1("not", Expr)) (?)
    | NEG Expr (Prim1("~", Expr)) (?)
    | HD Expr (?)
    | TL Expr (?)
    | ISE Expr (?)
    | PRINT Expr (?)
    | Expr AND Expr (Prim2("andalso", Expr1, Expr2))
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr MULTI Expr (Prim2("*", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | Expr EQ Expr (Prim2("=", Expr1, Expr2))
    | Expr DIF Expr (Prim2("!=", Expr1, Expr2)) (?)
    | Expr BLT Expr (Prim2("<", Expr1, Expr2))
    | Expr BLE Expr (Prim2("<=", Expr1, Expr2))
    | Expr DBCOL Expr (Prim2("::", Expr1, Expr2))
    | Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
    | Expr LBKT <nat>(?) RBKT (? pq no exemplo do video ele colocou cint(?))

AtomExpr : Const (Const)
    | NAME (Var(NAME))
    | LBRC Prog RBRC
    | LPAR Expr RPAR
    | LPAR Comps RPAR Comps(Comps) (?) preciso passar os parenteses?
    | FN Args DBARROW Expr END (?)

AppExpr : AtomExpr AtomExpr
    | AppExpr AtomExpr

Const : TRUE | FALSE ou BOOLEAN (?)
    | <nat> (?)
    | LPAR RPAR
    | LPAR Type LBKT RBKT RPAR Type(Type []) (?) preciso dos brackets?

Comps : Expr COMMA Expr
    | expr COMMA Comps

Matchexpr : END ([])
    | PIPE Condexpr ARROW Expr Matchexpr (?)

Condexpr : Expr (?)
    | UNDSCR (?)

Args : LPAR RPAR
    | LPAR Params RPAR Params(Params) (?) preciso dos parenteses?

Params : TypedVar(TypedVar)
    | TypedVar COMMA Params

TypedVar : Type NAME (Var(NAME))

Type : AtomType (AtomType())
    | LPAR Types RPAR
    | LBKT Types RBKT
    | Type ARROW Type (?)

AtomType : NIL (plcType(NIL)) (?)
    | BOOLEAN (plcType(BOOLEAN))
    | INT (plcType(INTEGER))
    | LPAR Type RPAR

Types : Type COMMA Type
    | Type COMMA Types