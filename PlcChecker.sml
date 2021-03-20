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

fun isPlcType BoolT = true
  | isPlcType IntT = true
  | isPlcType (SeqT (exp)) = isPlcType exp
  | isPlcType (ListT []) = true
  | isPlcType (ListT (hd :: [])) = isPlcType hd
  | isPlcType (ListT (hd :: tl)) =
      if isPlcType hd then isPlcType (ListT tl) else false
  | isPlcType _ = false;

fun isPlcSeq (SeqT t:plcType) = true
  | isPlcSeq _ = false;

fun isFunc (FunT (s,t)) = t
  | isFunc _ = raise NotFunc;

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
    | (ConB _ ) => BoolT    (* 3 and 4 : type(true|false, p) = Bool *)
    | (List [])  => ListT [] (* 5 : type( () , p ) = Nil *)
    | (List l)   =>          (* 6 : type((e1, ..., en), ρ) = (t1, ..., tn) se n > 1 e type(ei, ρ) = ti para todo i = 1, . . . , n*)
        let
          val list = map (fn t => teval t p) l 
        in
          ListT list
        end
    | (ESeq (SeqT t)) => SeqT t (* 7 : type((t []), ρ) = t se t é um tipo sequência *)
    | (ESeq _) => raise EmptySeq
    | (Let ( (x:string) , (e1:expr), (e2:expr) )) => (* 8 : type(var x = e1 ; e2, ρ) = t2 se 
                                                            type(e1, ρ) = t1 e type(e2, ρ[x 7→ t1]) = t2 
                                                            para algum tipo t1                            *)
        let
          val t = teval e1 p
        in
          teval e2 ( (x,t) :: p)
        end
    | Letrec(f, t0, x, t1, e0, e1) => (* 9 : type(fun rec f (t x) : t1 = e1 ; e2, ρ) = t2
            se type(e1, ρ[f 7→ t -> t1][x 7→ t]) = t1 e type(e2, ρ[f 7→ t -> t1]) = t2*)
        let
            val recP = (f, FunT(t0, t1))
            val argP = (x, t0)
            val e0Type = teval e0 (recP :: argP :: p)
            val e1Type = teval e1 (recP :: p)
        in
            if e0Type = t1 then e1Type else raise WrongRetType
        end
    | Anon (s:plcType, x:string , e:expr) => (* 10 : type(fn (s x) => e end, ρ) = s -> t se type(e, ρ[x 7→ s]) = t *)
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
    | If (e0,e1,e2) => (* 12 : type(if e then e1 else e2, ρ) = t se type(e, ρ) = Bool e type(e1, ρ) = type(e2, ρ) = t *)
        let
            val t0 = teval e0 p
            val t1 = teval e1 p
            val t2 = teval e2 p
        in
            if t0 <> BoolT then raise IfCondNotBool else if t1 = t2 then t1 else raise DiffBrTypes
        end
    | Match(e, l) => (*13: type(match e with | e1 -> r1 | ...| en -> rn, ρ) = t se
                          (a) type(e, ρ) = type(ei , ρ), para cada ei diferente de `__', e
                          (b) type(r1, ρ) = . . . = type(rn, ρ) = t*)
        let
            fun verifyOptions [] p = raise NoMatchResults
                | verifyOptions((NONE, r)::[]) p =
                    teval r p
                | verifyOptions((SOME ex, r)::[]) p = 
                    if teval ex p = teval e p then teval r p else raise MatchCondTypesDiff
                | verifyOptions((NONE, r)::(exi, ri)::tl) p = 
                    if teval r p = teval ri p then verifyOptions((exi, ri)::tl) p else raise MatchResTypeDiff
                | verifyOptions((SOME ex, r)::(exi, ri)::tl) p =
                    if teval ex p <> teval e p then raise MatchCondTypesDiff
                    else if teval r p = teval ri p then verifyOptions((exi, ri)::tl) p
                    else raise MatchResTypeDiff
        in
            verifyOptions l p
        end
    | Prim1("!", e) => (*14: type(!e, ρ) = Bool se type(e, ρ) = Bool*)
        if teval e p = BoolT then BoolT else raise UnknownType
    | Prim1("-", e) => (*15: type(-e, ρ) = Int se type(e, ρ) = Int*)
        if teval e p = IntT then IntT else raise UnknownType
    | Prim1("hd", e) => (*16: type(hd(e), ρ) = t se type(e, ρ) = [t]*)
        let
            fun seqChecker(SeqT s) = s
                | seqChecker _ = raise UnknownType
            val t = teval e p
        in
            if isPlcSeq t then seqChecker t else raise UnknownType
        end
    | Prim1("tl", e) => (*17: type(tl(e), ρ) = [t] se type(e, ρ) = [t]*)
        let
            val t= teval e p
        in
            if isPlcSeq t then t else raise UnknownType
        end
    | Prim1("ise", e) => (*18: type(ise(e), ρ) = Bool se type(e, ρ) = [t] para algum tipo t*)
        let
            val t= teval e p
        in
            if isPlcSeq t then BoolT else raise UnknownType
        end
    | Prim1("print", e) => (*19: type(print(e), ρ) = Nil se type(e, ρ) = t para algum tipo t*)
        let
            val t= teval e p
        in
            ListT []
        end
    | Prim2("&&", e0, e1) => (*20: type(e1, ρ) = type(e2, ρ) = Bool*)
        let 
            val t0 = teval e0 p
            val t1 = teval e1 p
        in
            if t0 = BoolT andalso t1 = BoolT then BoolT else raise UnknownType
        end
    | Prim2("::", e0, e1) => (*21: type(e1, ρ) = t e type(e2, ρ) = [t]*)
        let
            fun seqChecker(SeqT s) = s
                | seqChecker _ = raise UnknownType
            val t0 = teval e0 p
            val t1 = teval e1 p
        in
            if t0 = (seqChecker t1) then t1 else raise NotEqTypes
        end
    | Prim2("+", e0, e1) => (*22a: op ∈ {+, -, *, /} e type(e1, ρ) = type(e2, ρ) = Int*)
        let
            val t0 = teval e0 p
            val t1 = teval e1 p
        in
            if t0 = IntT andalso t1= IntT then IntT else raise UnknownType
        end
    | Prim2("-", e0, e1) => (*22b: op ∈ {+, -, *, /} e type(e1, ρ) = type(e2, ρ) = Int*)
        let
            val t0 = teval e0 p
            val t1 = teval e1 p
        in
            if t0 = IntT andalso t1= IntT then IntT else raise UnknownType
        end
    | Prim2("*", e0, e1) => (*22c: op ∈ {+, -, *, /} e type(e1, ρ) = type(e2, ρ) = Int*)
        let
            val t0 = teval e0 p
            val t1 = teval e1 p
        in
            if t0 = IntT andalso t1= IntT then IntT else raise UnknownType
        end
    | Prim2("/", e0, e1) => (*22d: op ∈ {+, -, *, /} e type(e1, ρ) = type(e2, ρ) = Int*)
        let
            val t0 = teval e0 p
            val t1 = teval e1 p
        in
            if t0 = IntT andalso t1= IntT then IntT else raise UnknownType
        end
    | Prim2("<", e0, e1) => (*23a: op ∈ {<, <=} e type(e1, ρ) = type(e2, ρ) = Int*)
        let
            val t0 = teval e0 p
            val t1 = teval e1 p
        in
            if t0 = IntT andalso t1 = IntT then BoolT else raise UnknownType
        end
    | Prim2("<=", e0, e1) => (*23b: op ∈ {<, <=} e type(e1, ρ) = type(e2, ρ) = Int*)
        let
            val t0 = teval e0 p
            val t1 = teval e1 p
        in
            if t0 = IntT andalso t1 = IntT then BoolT else raise UnknownType
        end
    | Prim2("=", e0, e1) => (*24a: op ∈ {=, !=} e type(e1, ρ) = type(e2, ρ) = t para algum tipo de igualdade t*)
        let 
            val t0 = teval e0 p
            val t1 = teval e1 p
        in
            if t0 = t1 then BoolT
            else if t0 <> t1 then raise NotEqTypes
            else raise UnknownType
        end
    | Prim2("!=", e0, e1) => (*24b: op ∈ {=, !=} e type(e1, ρ) = type(e2, ρ) = t para algum tipo de igualdade t*)
        let 
            val t0 = teval e0 p
            val t1 = teval e1 p
        in
            if t0 <> t1 then BoolT
            else if t0 = t1 then raise NotEqTypes
            else raise UnknownType
        end
    (*25: type(e [i], ρ) = ti se type(e, ρ) = (t1, ..., tn) para algum n > 1 e tipos t1, . . . ,
          tn, e i ∈ {1, . . . , n}*)
    | Item(i, List[]) => raise ListOutOfRange
    | Item(0, List(hd::tl)) => teval hd p
    | Item(i, List(hd::tl)) => teval (Item(i-1, (List tl))) p
    | Item(_, _) => raise OpNonList
    | Prim2(";", e0, e1) => (*26: type(e1, ρ) = t1 para algum tipo t e type(e2, ρ) = t2*)
        let
            val t0 = teval e0 p
            val t1 = teval e1 p
        in
            t1
        end
    | _ => raise UnknownType
