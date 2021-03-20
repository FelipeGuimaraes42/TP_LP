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

fun eval (e:expr) (p:plcType env) : plcVal =
  case (e,p) of
      ((Var x)   , p) => lookup(p,x)
    | ((ConI i)  , _) => IntV i
    | ((ConB b)  , _) => BoolV b
    | ((List []) , _) => ListV []
    | ((List l)  , p) =>
      let
        val list = map ( (fn x => eval(x, p)) , l)
      in
        ListV list
      end;