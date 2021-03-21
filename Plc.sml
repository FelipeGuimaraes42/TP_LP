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
        EmptySeq => " EmptySeq : A sequencia de entrada nao contem nenhum elemento"
      | UnknownType => " UnknownTyp: Tipo desconhecido"
      | NotEqTypes => " NotEqTypes : Os tipos da comparacao nao sao iguais"
      | WrongRetType => " WrongRetType : O tipo de retorno da funcao nao condiz com o corpo da mesma"
      | DiffBrTypes => " DiffBrTypes : Os tipos da expressoes dos possiveis caminhos de um If divergem"
      | IfCondNotBool => " IfCondNotBool : A condicao do if nao e booleana"
      | NoMatchResults => " NoMatchResults : Nao ha resultados para a expressao match"
      | MatchResTypeDiff => " MatchResTypeDiff : O tipo de algum dos casos em match difere dos demais"
      | MatchCondTypesDiff => " MatchCondTypesDiff : O tipo das opcoes de match difere do tipo da expressao passada para Match"
      | CallTypeMisM => " CallTypeMisM : Voce esta passando pra uma chamada de funcao um tipo diferente do qual ela suporta"
      | NotFunc => " NotFunc: Voce esta tentando chamar algo que nao e uma funcao"
      | ListOutOfRange => " ListOutOfRange : Tentativa de acessar um elemento fora dos limites da lista"
      | OpNonList => " OpNonList : Tentativa de acessar um elemento em uma expressao que nao e uma lista"
      (* Exceções de environ *)
      | SymbolNotFound => " SymbolNotFound : Um simbolo nao foi definido ou nao pode ser encontrado "
      (* Exceções de eval (erros de interpretação) *)
      | Impossible => " Impossible : Este erro nao deveria acontecer "
      | HDEmptySeq => " HDEmptySeq : Nao e possivel acessar o header de uma sequencia vazia "
      | TLEmptySeq => " TLEmptySeq : Nao e possivel acessar a cauda de uma sequencia vazia "
      | ValueNotFoundInMatch => " ValueNotFoundInMatch : A operacao de match foi incapaz de combinar com o padrao passado "
      | NotAFunc => " NotAFunc : Nao e permitido tratar tipos nao funcionais como funcoes "