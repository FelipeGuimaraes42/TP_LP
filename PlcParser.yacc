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
    | AppExpr of expr | Const of expr |  Comps of expr * expr
    | MatchExpr of expr | CondExpr of expr | Args of (plcType * string) list
    | Params of (plcType * string) list | TypedVar of (plcType * string)
    | Type of plcType | AtomType of plcType | Types of plcType list
    | RetType of plcType

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
    | Decl (Decl)

Decl : VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
    | FUN NAME Args EQ Expr (LetNAME, makeAnon(Params, Expr), Prog) (?)
    | FUN REC NAME Args RetType EQ Expr (makeFun(NAME, Args, RetType, Expr, Prog)) (?)

Expr : AtomExpr (AtomExpr)
    | AppExpr (AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
    | NOT Expr (Prim1("not", Expr))
    | NEG Expr (Prim1("~", Expr))
    | HD Expr (Prim1("hd", Expr))
    | TL Expr (Prim1("tl", Expr))
    | ISE Expr (Prim1("ise", Expr))
    | PRINT Expr (Prim1("print", Expr))
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
    | FN Args DBARROW Expr END (Anon(Type, Args, Expr))

AppExpr : AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
    | AppExpr AtomExpr (Call(AppExpr, AtomExpr))

Const : BOOLEAN (conB(BOOLEAN))
    | INTEGER (conI(INTEGER))
    | LPAR RPAR (ListT [])
    | LPAR Type LBKT RBKT RPAR (SeqT [])

Comps : Expr COMMA Expr (Expr1, Expr2)
    | expr COMMA Comps (Expr, Comps)

MatchExpr : END ([])
    | PIPE CondExpr ARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr))

CondExpr : Expr (Some(Expr))
    | UNDSCR (None)

Args : LPAR RPAR (ListT [])
    | LPAR Params RPAR (Params)

Params : TypedVar (TypedVar)
    | TypedVar COMMA Params (TypedVar, Params)

TypedVar : Type NAME (Type, Var(NAME))

Type : AtomType (AtomType)
    | LPAR Types RPAR (ListT(Type))
    | LBKT Types RBKT (SeqT(Type))
    | Type ARROW Type (FunT(Type1, Type2))

AtomType : NIL (ListT(NIL))
    | BOOLEAN (plcType(BOOLEAN))
    | INT (plcType(INTEGER))
    | LPAR Type RPAR ((Type))

Types : Type COMMA Type (Types)
    | Type COMMA Types (Types)

RetType : COLON Type (Type)