(* PlcInterp *)

(* 
    Interpreta os programas escritos em PLC 

    Deve implementar função :

    eval : expr -> plcValue env -> plcValue

    // Dada uma expressão E bem tipada e um ambiente de valores para as variáveis livres
    // de E (podendo não haver nenhum ambiente), retorna o valor de E nesse ambiente.
    //
    // . Erros de interpretação devem gerar exceções, conforme definido em PlcInterp.sml
    // . Expressões E que computam infinitamente ( i.e. fun rec f(Int x):Int = f(x - 1); f(0). )
    //   devem fazer com que eval "se perca", nunca produzindo um valor de retorno.
*)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

(*
  fun evalConI (ConI i) = i; // Avalia uma expressão que representa um inteiro em PLC e retorna esse inteiro
*)

(* (p:plcType env) : plcValue = *)

(* Copiei as definições para poder testar sem necessidade de rodar script *)

exception SymbolNotFound;

type 'a env = (string * 'a) list;

fun lookup [] id = raise SymbolNotFound
  | lookup ((k:string, v)::t) id = if k = id then v else lookup t id;

datatype plcType =
   IntT
  | BoolT
  | FunT of plcType * plcType
  | ListT of plcType list
  | SeqT of plcType;
  
datatype expr =
   ConI of int
  | ConB of bool
  | ESeq of plcType
  | Var of string
  | Let of string * expr * expr
  | Letrec of string * plcType * string * plcType * expr * expr
  | Prim1 of string * expr
  | Prim2 of string * expr * expr
  | If of expr * expr * expr
  | Match of expr * (expr option * expr) list
  | Call of expr * expr
  | List of expr list
  | Item of int * expr
  | Anon of plcType * string * expr;

datatype plcVal =
   BoolV of bool
  | IntV of int
  | ListV of plcVal list
  | SeqV of plcVal list
  | Clos of string * string * expr * plcVal env;

(* ============================================================================= *)

fun eval (e:expr) =
  case e of
    (ConI i) => IntV i
  | (ConB b) => BoolV b;

eval (ConI 5);
eval (ConB true);

fun eval (e:expr) (p:plcType env) : plcVal =
  case e of
      (ConI i) => IntV i
    | (ConB b) => BoolV b
    | (Var  x) => eval (lookup p x) p (* dunno, precisa retornar plcValue *)
    | (Prim2(f:string, e1:expr, e2:expr)) =>
      (*
        Error: case object and rules do not agree [tycon mismatch]
        rule domain: expr * plcType env
        object: plcType * 'Z
      *)
      let
        val v1 = (eval e1 p); (* Por algum motivo nas aulas o professor coloca ; após uma primeira avaliação usando env *)
        val v2 = (eval e2 p)
      in
        case f of
          ("+") => IntV (v1 + v2)
        | ("-") => IntV (v1 - v2)
        | ("*") => IntV (v1 * v2)
        | ("/") => IntV (v1 div v2)
        | _ =>  raise ValueNotFoundInMatch
      end;