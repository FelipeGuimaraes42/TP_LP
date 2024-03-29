%%

%name PlcParser

%pos int

%term VAR
    | PLUS | MINUS | MULTI | DIV | EQ | DIF
    | AND | NOT | BLT | BLE
    | SEMIC | COLON | DBCOL | COMMA
    | LPAR | RPAR | LBKT | RBKT | LBRC | RBRC
    | NAME of string | INTEGER of int | BOOLEAN of bool
    | NIL | FUN | REC | BOOL | INT
    | IF | THEN | ELSE | MATCH | WITH
    | HD | TL | ISE | PRINT
    | FN | END | ARROW | DBARROW
    | UNDSCR | PIPE
    | EOF

%right SEMIC ARROW
%nonassoc IF
%left ELSE
%left AND 
%left EQ DIF
%left BLT BLE
%right DBCOL
%left PLUS MINUS
%left MULTI DIV
%nonassoc NOT HD TL ISE PRINT NAME
%left LBKT

%nonterm Prog of expr | Decl of expr | Expr of expr | AtomExpr of expr
    | AppExpr of expr | Const of expr |  Comps of expr list
    | MatchExpr of (expr option * expr) list | CondExpr of expr option
    | Args of (plcType * string) list | Params of (plcType * string) list
    | TypedVar of (plcType * string) | Type of plcType | AtomType of plcType
    | Types of plcType list | RetType of plcType

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
    | Decl (Decl)

Decl : VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
    | FUN NAME Args EQ Expr SEMIC Prog (Let(NAME, makeAnon(Args, Expr), Prog))
    | FUN REC NAME Args RetType EQ Expr SEMIC Prog (makeFun(NAME, Args, RetType, Expr, Prog))

Expr : AtomExpr (AtomExpr)
    | AppExpr (AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
    | NOT Expr (Prim1("!", Expr))
    | MINUS Expr (Prim1("-", Expr))
    | HD Expr (Prim1("hd", Expr))
    | TL Expr (Prim1("tl", Expr))
    | ISE Expr (Prim1("ise", Expr))
    | PRINT Expr (Prim1("print", Expr))
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
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
    | Expr LBKT INTEGER RBKT (Item(INTEGER, Expr))

AtomExpr : Const (Const)
    | NAME (Var(NAME))
    | LBRC Prog RBRC (Prog)
    | LPAR Expr RPAR (Expr)
    | LPAR Comps RPAR (List(Comps))
    | FN Args DBARROW Expr END (makeAnon(Args, Expr))

AppExpr : AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
    | AppExpr AtomExpr (Call(AppExpr, AtomExpr))

Const : BOOLEAN (ConB(BOOLEAN))
    | INTEGER (ConI(INTEGER))
    | LPAR RPAR (List([]))
    | LPAR Type LBKT RBKT RPAR (ESeq(Type))

Comps : Expr COMMA Expr (Expr1::Expr2::[])
    | Expr COMMA Comps (Expr::Comps)

MatchExpr : END ([])
    | PIPE CondExpr ARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr : Expr (SOME(Expr))
    | UNDSCR (NONE)

Args : LPAR RPAR ([])
    | LPAR Params RPAR (Params)

Params : TypedVar (TypedVar::[])
    | TypedVar COMMA Params (TypedVar::Params)

TypedVar : Type NAME (Type, NAME)

Type : AtomType (AtomType)
    | LPAR Types RPAR (ListT(Types))
    | LBKT Type RBKT (SeqT(Type))
    | Type ARROW Type (FunT(Type1, Type2))

AtomType : NIL (ListT[])
    | BOOL (BoolT)
    | INT (IntT)
    | LPAR Type RPAR (Type)

Types : Type COMMA Type (Type1::Type2::[])
    | Type COMMA Types (Type::Types)

RetType : COLON Type (Type)