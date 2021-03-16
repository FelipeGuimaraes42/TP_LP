(* Plc interpreter main file *)

(*
    run : expr -> string

    // Toma uma expressão E em sintaxe abstrata, faz sua checagem de tipos com teval,
    // a avalia com eval e produz :
    //
    // . String contendo tanto o valor quanto o tipo de E em sintaxe concreta
    // . Exceções geradas em teval e eval devem ser tratadas ( só printar mensagem de erro contextualizada)

    Usando a função run justo com fromString ou fromFile é possível testar a implementação
    do verificador de tipos e do interpretador.
*)