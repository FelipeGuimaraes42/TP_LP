(* Plc interpreter main file *)

(*
    run : expr -> string

    // Toma uma expressão E em sintaxe abstrata, faz sua checagem de tipos com teval,
    // a avalia com eval e produz :
    //
    // . String contendo tanto o valor quanto o tipo de E em sintaxe concreta
    // . Exceções geradas em teval e eval devem ser tratadas ( só printar mensagem de erro contextualizada )

    Usando a função run justo com fromString ou fromFile é possível testar a implementação
    do verificador de tipos e do interpretador.
*)

fun run (e:expr) =
    let
        val tipo = teval e
        val valor = eval e
    in
        handle 
            (* Exceções de teval (tipagem incorreta) *)
              EmptySeq => "Do Stuff"
            | UnknownType => " Problem Descrition ... "
            | NotEqTypes => " Problem Descrition ... "
            | WrongRetType => " Problem Descrition ... "
            | DiffBrTypes => " Problem Descrition ... "
            | IfCondNotBool => " Problem Descrition ... "
            | NoMatchResults => " Problem Descrition ... "
            | MatchResTypeDiff => " Problem Descrition ... "
            | MatchCondTypesDiff => " Problem Descrition ... "
            | CallTypeMisM => " Problem Descrition ... "
            | NotFunc => " Problem Descrition ... "
            | ListOutOfRange => " Problem Descrition ... "
            | OpNonList => " Problem Descrition ... "
            (* Exceções de eval (erros de interpretação) *)
            | Impossible => " Problem Descrition ... "
            | HDEmptySeq => " Problem Descrition ... "
            | TLEmptySeq => " Problem Descrition ... "
            | ValueNotFoundInMatch => " Problem Descrition ... "
            | NotAFunc => " Problem Descrition ..."

        (* 
            Receber o valor fornecido por 'eval e' e o tipo fornecido por 'teval e'
            Transformá-los numa string, juntar ambos noutra string e ser feliz =)
        *)

    end;