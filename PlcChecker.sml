(* PlcChecker *)

(*
    Módulo responsável pela checagem de tipos.

    Deve implementar função :

    teval : expr -> plcType env -> plcType

    // Dada uma expressão qualquer E em sintaxe abstrata e um ambiente de tipos para
    // as variáveis livres em E (podendo ou não haver tal ambiente), produz :
    //
    // . O tipo de E dentro ambiente informado, caso E seja bem tipada
    // . Uma exceção, dentre as definidas abaixo, caso E tenha erro de tipagem.
*)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

fun isTypeEq BoolT = true
  | isTypeEq IntT = true
  | isTypeEq (SeqT (exp)) = isTypeEq exp
  | isTypeEq (ListT []) = true
  | isTypeEq (ListT (hd :: [])) = isTypeEq hd
  | isTypeEq (ListT (hd :: tl)) =
      if isTypeEq then isTypeEq (ListT tl) else false
  | isTypeEq _ = false;

fun isTypeSq (SeqT t:plcType) = true
  | isTypeSq _ false;

fun isFunc (FunT (s,t)) = t
  | isFunc _ raise NotFunc;

fun teval (e:expr) (p:plcType env) : plcType =
  case e of
    (*
        x := function or variable name
        n := numerals
        e, e1, e2 := PLC Expressions
        s, t, ti  := PLC Types
        p := type environment
        ρ[x -> t] := enviroment that maps variable x to type t . 

        Expression 'e' is well-typed and has type t iff we can conclude that
        type(e,p) = t (the type of expression e in enviroment p is t)
        according to the rules defined in the documentation
    *)
      (Var x) => lookup p x  (* 1 : type(x, ρ) = ρ(x) *)
    | (ConI  _ ) => IntT     (* 2 : type(n, ρ) = Int   *)
    | (BoolT _ ) => BoolT    (* 3 and 4 : type(true|false, p) = Bool *)
    | (List [])  => ListT [] (* 5 : type( () , p ) = Nil *)
    | (List l)   =>          (* 6 : type((e1, ..., en), ρ) = (t1, ..., tn) se n > 1 e type(ei, ρ) = ti para todo i = 1, . . . , n*)
        let
          val list = map (fn t => teval t p) l 
        in
          ListT list
        end
    | (ESeq (SeqT t)) => SeqT t (* 7 : type((t []), ρ) = t se t é um tipo sequência *)
    | (Eseq _) => raise EmptySeq
    | (Let ( (x:string) , (e1:expr), (e2:expr) )) => (* 8 : type(var x = e1 ; e2, ρ) = t2 se 
                                                            type(e1, ρ) = t1 e type(e2, ρ[x 7→ t1]) = t2 
                                                            para algum tipo t1                            *)
        let
          val t = teval e1 p
        in
          teval e2 ( (x,t) :: p)
        end
    | (* 9 : ... dunno ... *)
      Anon (s:plcType, x:string , e:expr) => (* 10 : type(fn (s x) => e end, ρ) = s -> t se type(e, ρ[x 7→ s]) = t *)
        let
          val t = teval e ( (x,s) :: p )
        in
          FunT (s,t)
        end
    | Call (e2,e1) => (* 11 : type(e2(e1), ρ) = t2 se type(e2, ρ) = t1 -> t2 e type(e1, ρ) = t1 para algum tipo t1 *)
      let
        val t1 = teval e1 p
        val t2 = isFunc (teval e2 p)
      in
        if teval e2 p = FunT (t1,t2) then t2 else raise CallTypeMisM
      end
    | If (e,e1,e2) => (* 12 : type(if e then e1 else e2, ρ) = t se type(e, ρ) = Bool e type(e1, ρ) = type(e2, ρ) = t *)
      let
        val t0 = teval e  p
        val t1 = teval e1 p
        val t2 = teval e2 p
      in
        if t0 <> BoolT then raise IfCondNotBool else if t1 = t2 then t1 else raise DiffBrTypes
      end