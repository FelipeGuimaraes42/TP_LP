%%

%name PlcParser

%pos int

%term VAR
    | PLUS | MINUS | MULTI | DIV | EQ
    | AND | DIF | BLT | BLE | 
    | SEMIC | DBCOL | 
    | LPAR | RPAR | LBKT | RBKT
    | NAME of string | CINT

%right SEMIC DBCOL
%left AND EQ DIF BLT BLE PLUS MINUS MULTI DIV LBKT

%nonterm Prog of expr | Expr of expr | 

%eop EOF

%noshift EOF

%start Prog

%%
