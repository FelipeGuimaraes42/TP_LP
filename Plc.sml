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
  Comandos para rodar :

C:\"Program Files (x86)"\SMLNJ\bin\ml-lex.bat PlcLexer.lex
C:\"Program Files (x86)"\SMLNJ\bin\ml-yacc.bat PlcParser.yacc

*)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";
use "PlcInterp.sml";
use "PlcChecker.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fun run (e:expr) =
    let
      val tipo = type2string(teval e [])
      val valor = val2string(eval e [])
    in
      valor ^ " : " ^ tipo
    end
    handle
      (* Exceções de teval (tipagem incorreta) *)
        EmptySeq => " EmptySeq : A sequência de entrada não contém nenhum elemento"
      | UnknownType => " UnknownTyp: É usada nas situações onde nenhuma das específicas se encaixa"
      | NotEqTypes => " NotEqTypes : Se os tipos usados numa comparação são diferentes"
      | WrongRetType => " WrongRetType : O tipo de retorno da função não condiz com o corpo da mesma"
      | DiffBrTypes => " DiffBrTypes : Os tipos da expressões dos possíveis caminhos de um If divergem"
      | IfCondNotBool => " IfCondNotBool : A condição do if não é booleana"
      | NoMatchResults => " NoMatchResults : Não há resultados para a expressão match"
      | MatchResTypeDiff => " MatchResTypeDiff : O tipo de algum dos casos em match difere dos demais"
      | MatchCondTypesDiff => " MatchCondTypesDiff : O tipo das opções de match difere do tipo da expressão passada para Match"
      | CallTypeMisM => " CallTypeMisM : Você está passando pra uma chamada de função um tipo diferente do qual ela suporta"
      | NotFunc => " NotFunc: Você está tentando chamar algo que não é uma função"
      | ListOutOfRange => " ListOutOfRange : Tentativa de acessar um elemento fora dos limites da lista"
      | OpNonList => " OpNonList : Tentativa de acessar um elemento em uma expressão que não é uma lista"
      (* Exceções de environ *)
      | SymbolNotFound => " SymbolNotFound : Um simbolo nao foi definido ou nao pode ser encontrado "
      (* Exceções de eval (erros de interpretação) *)
      | Impossible => " Impossible : Este erro nao deveria acontecer "
      | HDEmptySeq => " HDEmptySeq : Nao e possivel acessar o header de uma sequencia vazia "
      | TLEmptySeq => " TLEmptySeq : Não e possivel acessar a calda de uma sequencia vazia "
      | ValueNotFoundInMatch => " ValueNotFoundInMatch : A operaçao de match foi incapaz de combinar com o padrao passado "
      | NotAFunc => " NotAFunc : Nao e permitido tratar tipos nao funcionais como funçoes "