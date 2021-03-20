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

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

(*
  x := function or variable name
  n := numerals
  e, e1, e2, e3 := PLC Expressions
  s, t, ti  := PLC Types
  p := type environment
  p' := new type enviroment
  ρ[x -> t] := enviroment that maps variable x to type t . 

  Expression 'e' is well-typed and has type t iff we can conclude that
  type(e,p) = t (the type of expression e in enviroment p is t)
  according to the rules defined in the documentation
*)

fun eval (Var x) (p:plcVal env) =             (* 1 : type(x, ρ) = ρ(x) *)
      let in 
        lookup p x
        handle SymbolNotFound => raise SymbolNotFound 
      end
  | eval (ConI i) _ = IntV i                  (* 2 : type(n, ρ) = Int   *)
  | eval (ConB b) _ = BoolV b                 (* 3 and 4 : type(true|false, p) = Bool *)
  | eval (List []) (p:plcVal env) = ListV []  (* 5 : type( () , p ) = Nil *)
  | eval (List l ) (p:plcVal env) =           (* 6 : type((e1, ..., en), ρ) = (t1, ..., tn) se n > 1 e type(ei, ρ) = ti para todo i = 1, . . . , n*)
    let
      fun aux (x::[]) = eval x p :: []
        | aux (x::y) = eval x p :: aux y
        | aux _ = raise Impossible;
    in
      ListV (aux l)
    end
  | eval (ESeq e) _ = SeqV []                 (* 7 : type((t []), ρ) = t se t é um tipo sequência *)
  | eval (Let (f, e1, e2)) (p:plcVal env) =
    (*
      8 : type(var x = e1 ; e2, ρ) = t2 se 
      type(e1, ρ) = t1 e type(e2, ρ[x 7→ t1]) = t2 
      para algum tipo t1
    *)
    let
      val p' = (f, eval e1 p) :: p
    in
      eval e2 p'
    end
  | eval (Letrec (f, argTyp, arg, funTyp, e1, e2)) (p:plcVal env) = 
    (* 
      9 : type(fun rec f (t x) : t1 = e1 ; e2, ρ) = t2
      se type(e1, ρ[f 7→ t -> t1][x 7→ t]) = t1 e type(e2, ρ[f 7→ t -> t1]) = t2
    *)
    let
      val p' = (f, Clos(f, arg, e1, p)) :: p
    in
      eval e2 p'
    end
  | eval (Anon (typ, arg, exp)) (p:plcVal env) = Clos ("", arg, exp, p) (* 10 : type(fn (s x) => e end, ρ) = s -> t se type(e, ρ[x 7→ s]) = t *)
  | eval (Call (e1, e2)) (p:plcVal env) =                             (* 11 : type(e2(e1), ρ) = t2 se type(e2, ρ) = t1 -> t2 e type(e1, ρ) = t1 para algum tipo t1 *)
    let
      fun mountArguments (List (x::[])) = [eval x p]
        | mountArguments (List (x::xs)) = [eval x p] @ mountArguments (List xs)
        | mountArguments (exp) = [eval exp p]
      val p' = [("$list", ListV (mountArguments e2))] @ p
      val f = eval e1 p
    in
      case f of
          Clos(name, var, exp, cEnv) =>
            let
              val ev = eval e2 p'
              val fEnv = (var, ev)::(name, f)::cEnv
            in
              eval exp fEnv
            end
        | _ => raise NotAFunc
    end
  | eval (If (e1, e2, e3)) (p:plcVal env) = (* 12 : type(if e then e1 else e2, ρ) = t se type(e, ρ) = Bool e type(e1, ρ) = type(e2, ρ) = t *)
    let in
      case eval e1 p of 
          BoolV true => eval e2 p
        | BoolV false => eval e3 p
        | _ => raise Impossible
    end
  | eval (Match (e1, matchList)) (p:plcVal env) =
    (*
      13: type(match e with | e1 -> r1 | ...| en -> rn, ρ) = t se
      (a) type(e, ρ) = type(ei , ρ), para cada ei diferente de `__', e
      (b) type(r1, ρ) = . . . = type(rn, ρ) = t
    *)
    let
      val evalMatchVar = eval e1 p 
      (* Try matches will return the "cond -> expr" for which cond matches e1 *)
      fun tryMatches (matchVar, x::[]) p =
          let in
            case x of
                (SOME e2, e3) => if matchVar = eval e2 p then e3 else raise ValueNotFoundInMatch
              | (NONE, e3) => e3
          end
        | tryMatches (matchVar, x::xs) p =  let in
            case x of
                (SOME e2, e3) => if matchVar = eval e2 p then e3 else tryMatches (matchVar, xs) p
              | (NONE, e3) => raise Impossible
          end
        | tryMatches (matchVar, _ ) p = raise Impossible
    in
      eval (tryMatches (evalMatchVar, matchList) p) p
    end
  | eval (Prim1 (oper, exp)) (p:plcVal env) = (* 14 ao 19 *)
    let
      val v = eval exp p
    in
      case v of
          BoolV b =>
          let in
            case oper of
                "!" => BoolV (not b) (*14: type(!e, ρ) = Bool se type(e, ρ) = Bool*)
              | "print" => 
                let 
                  val v = BoolV b
                  val ignore = print(val2string(v) ^ "\n")
                in
                  ListV []
                end
              | _ => raise Impossible
          end
        | IntV i => 
          let in
            case oper of
                "-" => IntV (~ i) (*15: type(-e, ρ) = Int se type(e, ρ) = Int*)
              | "print" => 
                let 
                  val v = IntV i
                  val ignore = print(val2string(v) ^ "\n")
                in
                  ListV []
                end
              | _ => raise Impossible
          end
        | SeqV s =>
          let in
            case oper of
                "hd" => let in let in hd s end handle Empty => raise HDEmptySeq end        (*16: type(hd(e), ρ) = t se type(e, ρ) = [t]*)
              | "tl" => let in let in SeqV (tl s) end handle Empty => raise TLEmptySeq end (*17: type(tl(e), ρ) = [t] se type(e, ρ) = [t]*)
              | "ise" =>                                                                   (*18: type(ise(e), ρ) = Bool se type(e, ρ) = [t] para algum tipo t*)
                let in
                  case s of
                      [] => BoolV true
                    | _ => BoolV false
                end
              | "print" => 
                let 
                  val ignore = print(list2string(val2string, s) ^ "\n")
                in
                  ListV []
                end
              | _ => raise Impossible
          end
        | ListV l =>
          let in
            case oper of
                "print" =>  (*19: type(print(e), ρ) = Nil se type(e, ρ) = t para algum tipo t*)
                let 
                  val ignore = print(list2string(val2string, l) ^ "\n")
                in
                  ListV []
                end
              | _ => raise Impossible
          end
        | _ => raise Impossible
    end
  | eval (Prim2 (oper, e1, e2)) (p:plcVal env) = (* 20,21,22,23,24,26 *)
    if oper = ";" then
      let
        val ignore = eval e1 p
      in
        eval e2 p
      end
    else
      let
        val v1 = eval e1 p
        val v2 = eval e2 p
      in
        case (v1, v2) of
            (BoolV b1, BoolV b2) => 
            let in
              case oper of
                  "&&" => BoolV (b1 andalso b2) 
                | "=" => BoolV (b1 = b2)        
                | "!=" => BoolV (b1 <> b2)      
                | _ => raise Impossible
            end
          | (IntV i1, IntV i2) => 
            let in
              case oper of
                  "+" => IntV (i1 + i2)   
                | "-" => IntV (i1 - i2)
                | "*" => IntV (i1 * i2)
                | "/" => IntV (i1 div i2)
                | "<" => BoolV (i1 < i2) 
                | "<=" => BoolV (i1 <= i2)
                | "=" => BoolV (i1 = i2)
                | "!=" => BoolV (i1 <> i2)
                | _ => raise Impossible
            end
          | (IntV i1, SeqV s2) => 
            let in
              case oper of
                  "::" => SeqV (IntV i1 :: s2)
                | _ => raise Impossible
            end
          | (BoolV b1, SeqV s2) => 
            let in
              case oper of
                  "::" => SeqV (BoolV b1 :: s2)
                | _ => raise Impossible
            end
          | (ListV l1, SeqV s2) => 
            let in
              case oper of
                  "::" => SeqV (ListV l1 :: s2)
                | _ => raise Impossible
            end
          | _ => raise Impossible
      end
  | eval (Item (index, exp)) (p:plcVal env) =
    (*
      25: type(e [i], ρ) = ti se type(e, ρ) = (t1, ..., tn) 
      para algum n > 1 e tipos t1, . . . , tn, e i ∈ {1, . . . , n}
    *)
    let
      fun getElementI (index, []) = raise Impossible
        | getElementI (index, (x::[])) = if index = 1 then x else raise Impossible
        | getElementI (index, (x::xs)) = if index = 1 then x else getElementI (index - 1, xs)
      val value = eval exp p
    in
      case value of
          ListV l => getElementI (index, l)
        | SeqV s => getElementI (index, s)
        | _ => raise Impossible
    end
  ;