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

(*
    PROBLEMA : Como run_teval e run_eval tratam a exceção imprimindo uma string, isso faz
               com, por exemplo, a função teval tenha que retornar uma string. Porém, como :
               
               teval : expr -> plcType env -> plcType

               Essa forma de tratar os erros causará um tycon_mismatch
*)

fun run_teval (e:expr) =
    teval e
    handle
        EmptySeq => " Problem Descrition ... "
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
    ;

fun run_eval (e:expr) =
    eval e
    handle
        Impossible => " Problem Descrition ... "
        | HDEmptySeq => " Problem Descrition ... "
        | TLEmptySeq => " Problem Descrition ... "
        | ValueNotFoundInMatch => " Problem Descrition ... "
        | NotAFunc => " Problem Descrition ... "
    ;

    

