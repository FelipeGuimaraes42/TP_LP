(* Plc interpreter main file *)

(* Infrastructure to run the Plc Front-End *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcChecker.sml";
use "PlcInterp.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

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

open PlcFrontEnd;


fun run (e:expr) =
    let
        val type = type2String(teval e [])
        val val = val2String(eval e [])
    in
        (* 
            Receber o valor fornecido por 'eval e' e o tipo fornecido por 'teval e'
            Transformá-los numa string, juntar ambos noutra string e ser feliz =)
        *)
        val ^" : "^type
    end
        handle 
            (* Exceções de teval (tipagem incorreta) *)
              EmptySeq => "A sequência de entrada não contém nenhum elemento"
            | UnknownType => "É usada nas situações onde nenhuma das específicas se encaixa"
            | NotEqTypes => "Se os tipos usados numa comparação são diferentes"
            | WrongRetType => "O tipo de retorno da função não condiz com o corpo da mesma"
            | DiffBrTypes => "Os tipos da expressões dos possíveis caminhos de um If divergem"
            | IfCondNotBool => "A condição do if não é booleana"
            | NoMatchResults => "Não há resultados para a expressão match"
            | MatchResTypeDiff => "O tipo de algum dos casos em match difere dos demais"
            | MatchCondTypesDiff => "O tipo das opções de match difere do tipo da expressão passada para Match"
            | CallTypeMisM => "Você está passando pra uma chamada de função um tipo diferente do qual ela suporta"
            | NotFunc => "Você está tentando chamar algo que não é uma função"
            | ListOutOfRange => "Tentativa de acessar um elemento fora dos limites da lista"
            | OpNonList => "Tentativa de acessar um elemento em uma expressão que não é uma lista"
            (* Exceções de eval (erros de interpretação) *)
            | Impossible => " Problem Descrition ... "
            | HDEmptySeq => " Problem Descrition ... "
            | TLEmptySeq => " Problem Descrition ... "
            | ValueNotFoundInMatch => " Problem Descrition ... "
            | NotAFunc => " Problem Descrition ..."
            (*Exceções de env (erros de ambiente)*)
            | SymbolNotFound => " Problem Description ... "
