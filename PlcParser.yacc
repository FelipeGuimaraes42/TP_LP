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

%nonterm Prog of expr | Decl of expr | Expr of expr | AtomExpr of expr
    | AppExpr of expr | Const of expr |  Comps of expr | MatchExpr of expr
    | CondExpr of expr | Args of plcType (?) | Params of plcType 
    | TypedVar of plcType (?) | Type of plcType | AtomType of plcType | Type of plcType

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
    | Decl SEMIC Prog ()

Decl : VAR NAME EQ Expr (Let (NAME, Expr))
    | FUN NAME Args EQ Expr ()
    | FUN REC NAME Args COLON Type EQ Expr ()

Expr : AtomExpr (AtomExpr)
    | AppExpr (AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
    | NOT Expr (Prim1("not", Expr))
    | NEG Expr (Prim1("~", Expr))
    | HD Expr ()
    | TL Expr ()
    | ISE Expr ()
    | PRINT Expr ()
    | Expr AND Expr (Prim2("andalso", Expr1, Expr2))
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr MULTI Expr (Prim2("*", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | Expr EQ Expr (Prim2("=", Expr1, Expr2))
    | Expr DIF Expr (Prim2("!=", Expr1, Expr2))
    | Expr BLT Expr (Prim2("<", Expr1, Expr2))
    | Expr BLE Expr (Prim2("<=", Expr1, Expr2))
    | Expr DBCOL Expr (Prim2("::", Expr1, Expr2))
    | Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
    | Expr LBKT INTEGER RBKT (conI(INTEGER))

AtomExpr : Const (Const)
    | NAME (Var(NAME))
    | LBRC Prog RBRC (Prog)
    | LPAR Expr RPAR (Expr)
    | LPAR Comps RPAR (Comps)
    | FN Args DBARROW Expr END (?)

AppExpr : AtomExpr AtomExpr (?)
    | AppExpr AtomExpr (?)

Const : BOOLEAN (conB(BOOLEAN))
    | INTEGER (conI(INTEGER))
    | LPAR RPAR () (? preciso de algo?)
    | LPAR Type LBKT RBKT RPAR (Type [])

Comps : Expr COMMA Expr (Expr, Expr) (?)
    | expr COMMA Comps (Expr, Comps) (?)

MatchExpr : END ([])
    | PIPE CondExpr ARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr))

CondExpr : Expr (Expr)
    | UNDSCR (_)

Args : LPAR RPAR
    | LPAR Params RPAR Params(Params) (?) preciso dos parenteses?

Params : TypedVar(TypedVar)
    | TypedVar COMMA Params

TypedVar : Type NAME (Type(Var(NAME))) (?)

Type : AtomType (AtomType)
    | LPAR Types RPAR ()
    | LBKT Types RBKT()
    | Type ARROW Type ()

AtomType : NIL (plcType(NIL)) ou () (?)
    | BOOLEAN (plcType(BOOLEAN))
    | INT (plcType(INTEGER))
    | LPAR Type RPAR (Type) (? mais parenteses?)

Types : Type COMMA Type (Type, Type) (?)
    | Type COMMA Types (Type, Types) (?)