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

fun teval (e:expr) = 
    if ... then raise EmptySeq 
    else if ... then raise UnknownType
    else if ... then raise NotEqTypes
    else if ... then raise WrongRetType
    else if ... then raise DiffBrTypes
    else if ... then raise IfCondNotBool
    else if ... then raise NoMatchResults
    else if ... then raise MatchResTypeDiff
    else if ... then raise MatchCondTypesDiff
    else if ... then raise CallTypeMisM
    else if ... then raise NotFunc
    else if ... then raise ListOutOfRange
    else if ... then raise OpNonList
    else
        (* Implementação da função *)