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

fun test (e:expr) (p:plcVal env) : plcVal =
  case e of
      (Var x) => lookup p x
    | (ConI i) => IntV i
    | (ConB b) => BoolV b
    | (List []) => ListV []
    | (List l) =>
      let
        fun aux (x::[]) = eval x p :: []
          | aux (x::y) = eval x p :: aux y
          | aux _ = raise Impossible;
      in
        ListV (aux l)
      end
    | (ESeq e) => SeqV []
    | (Let (f,e1,e2)) =>
      let
        val p' = (f, eval e1 p) :: p
      in
        eval e2 p'
      end
    | (Letrec (f, argT, arg, fT, e1, e2)) =>
      let
        val p' = (f, Clos(f, arg, e1, p)) :: p
      in
        eval e2 p'
      end
    | (Anon (tipo, arg, e)) => Clos ("", arg, e, p)
    | (Call (e1,e2)) =>
      let
        fun aux (List (x::[])) = [eval x p]
          | aux (List (x::y )) = [eval x p] @ aux (List y)
          | aux (e) = [eval e p]
        val p' = [("$list", ListV (aux e2))] @ p
        val f = eval e1 p
      in
        case f of
          Clos(nome, var, e, cp) =>
            let
              val x = eval e2 p'
              val y = (var, x) :: (nome, f) :: cp
            in
              eval e y
            end
          | _ => raise NotAFunc
      end
    | (If (e,thenB,elseB)) =>
      let
        val whichB = eval e p
      in
        case whichB of
            BoolV true => eval thenB p
          | BoolV false => eval elseB p
          | _ => raise Impossible
      end
    | (Match (e1,mList)) =>
      let
        val evaluation = eval e1 p
        fun aux (var, x::[]) p =
            let
            in
              case x of
                (SOME e2,e3) => if var = eval e2 p then e3 else raise ValueNotFoundInMatch
                | (NONE, e3) => e3
            end
          | aux (var, x::y) p =
            let
            in
              case x of
                (SOME e2,e3) => if var = eval e2 p then e3 else aux (var,y) p
                | (None, e3) => raise Impossible
            end
          | aux (var, _) p = raise Impossible
      in
        eval (aux (evaluation, mList) p) p
      end
    | (Prim1 (opp, e)) =>
      let
        val v = eval e p
      in
      case v of
          BoolV b =>
          let
          in
            case opp of
                "!" => BoolV (not b)
              | "print" => 
                let 
                  val bool = BoolV b
                  val prin = print(val2string(bool)^"\n")
                in
                  ListV []
                end
              | _ => raise Impossible
          end
        | IntV i => 
          let in
            case opp of
                "-" => IntV (~ i)
              | "print" => 
                let 
                  val integer = IntV i
                  val prin = print(val2string(integer)^"\n")
                in
                  ListV []
                end
              | _ => raise Impossible
          end
        | SeqV s =>
          let in
            case opp of
                "hd" => let
                        in
                          let in hd s end 
                          handle Empty => raise HDEmptySeq 
                        end        
              | "tl" => let
                        in 
                          let in SeqV (tl s) end 
                          handle Empty => raise TLEmptySeq 
                        end 
              | "ise" =>                                                                   
                let 
                in
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
          let
          in
            case opp of
                "print" =>
                let 
                  val ignore = print(list2string(val2string, l) ^ "\n")
                in
                  ListV []
                end
              | _ => raise Impossible
          end
        | _ => raise Impossible
    end
  | (Prim2 (opp, e1, e2)) =>
    if opp = ";" then
      let
        val prin = eval e1 p
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
              case opp of
                  "&&" => BoolV (b1 andalso b2) 
                | "=" => BoolV  (b1 = b2)        
                | "!=" => BoolV (b1 <> b2)      
                | _ => raise Impossible
            end
          | (IntV i1, IntV i2) => 
            let in
              case opp of
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
              case opp of
                  "::" => SeqV (IntV i1 :: s2)
                | _ => raise Impossible
            end
          | (BoolV b1, SeqV s2) => 
            let in
              case opp of
                  "::" => SeqV (BoolV b1 :: s2)
                | _ => raise Impossible
            end
          | (ListV l1, SeqV s2) => 
            let in
              case opp of
                  "::" => SeqV (ListV l1 :: s2)
                | _ => raise Impossible
            end
          | _ => raise Impossible
      end
  | (Item (idx, e)) =>
    let
      fun aux (idx, []) = raise Impossible
        | aux (idx, (x::[])) = if idx = 1 then x else raise Impossible
        | aux (idx, (x::y)) = if idx = 1 then x else aux (idx - 1, y)
      val value = eval e p
    in
      case value of
          ListV l => aux (idx, l)
        | SeqV s => aux (idx, s)
        | _ => raise Impossible
    end
  ;