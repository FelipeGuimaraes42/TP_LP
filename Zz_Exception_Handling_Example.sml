(* Exemplo de como tratar múltiplas exceções : *)

exception OMG;
exception BBQ;
exception MEGATRON;

fun ex (x:int) = 
    if x = 0 then raise OMG
    else if x = 1 then raise BBQ
    else if x = 2 then raise MEGATRON
    else "All green";

fun teste num = 
    ex num
    handle
        OMG => "OMG exception generated"
        | BBQ => "BBQ Exception generated"
        | MEGATRON => "Your life is FORFEIT human!"
    ;

teste 2;