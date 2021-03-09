(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

fun keyword (s, lpos, rpos) =
    case s of 
        "Bool" => BOOLEAN(lpos, rpos)
        | "else" => ELSE(lpos, rpos)
        | "false" => BOOLEAN(lpos, rpos)
        | "fn" => FN(lpos, rpos)
        | "fun" => FUN(lpos, rpos)
        | "hd" => HD(lpos, rpos)
        | "if" => IF(lpos, rpos)
        | "Int" => INTEGER(lpos, rpos)
        | "ise" => ISE(lpos, rpos)
        | "match" => MATCH(lpos, rpos)
        | "Nil" => NIL(lpos, rpos)
        | "print" => PRINT(lpos, rpos)
        | "rec" => REC(lpos, rpos)
        | "then" => THEN(lpos, rpos)
        | "tl" => TL(lpos, rpos)
        | "true" => BOOLEAN(lpos, rpos)
        | "var"   => VAR(lpos, rpos)
        | "with" => WITH(lpos, rpos)
        | _ => NAME(s, lpos, rpos)

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
alpha= [A-Za-z];
digit= [0-9];
whitespace= [\ \t];
identifier= [a-zA-Z_0-9]*;

%%

\n => (lineNumber := !lineNumber + 1; lex());
{whitespace}+ => (lex());
{digit}+ => ();
{identifier} => (keyword(yytext, yypos, yypos));
"+" => (PLUS(yypos, yypos));
"-" => (MINUS(yypos, yypos));
"*" => (MULTI(yypos, yypos));
"/" => (DIV(yypos, yypos));
"=" => (EQ(yypos, yypos));
"!=" => (DIF(yypos, yypos));
"(" => (LPAR(yypos, yypos));
")" => (RPAR(yypos, yypos));
"[" => (LBKT(yypos, yypos));
"]" => (RBKT(yypos, yypos));
"{" => (LBRC(yypos, yypos));
"}" => (RBRC(yypos, yypos));
"&&" => (AND(yypos, yypos));
"<" => (BLT(yypos, yypos));
"<=" => (BLE(yypos, yypos));
";" => (SEMIC(yypos, yypos));
":" => (COLON(yypos, yypos));
"::" => (DBCOL(yypos, yypos));
"," => (COMMA(yypos, yypos));
"~" => (NEG(yypos, yypos));
"!" => (NOT(yypos, yypos));
"->" => (ARROW(yypos, yypos));
"=>" => (DBARROW(yypos, yypos));
"_" => (UNDSCR(yypos, yypos));
"|" => (PIPE(yypos, yypos));
. => (error("\n***Lexer error: bad character ***\n");
    raise Fail("Lexer error: bad character"^yytext));