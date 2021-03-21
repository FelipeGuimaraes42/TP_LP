print("\n\nTESTANDO : SymbolNotFound | ");
run (fromString "x");

print("\n\nTESTANDO : Impossible | ");
let in val2string(eval (fromString "x = true") [("x", IntV 1)]) end
handle Impossible => "Impossible : Este erro nao deveria acontecer";

print("\n\nTESTANDO : HDEmptySeq | ");
run (fromString "hd ([Int] [])");

print("\n\nTESTANDO : TLEmptySeq | ");
run (fromString "tl ([Int] [])");

print("\n\nTESTANDO : ValueNotFoundInMatch | ");
let in val2string(eval (fromString "match x with | true -> 1 end") [("x", BoolV false)]) end
handle ValueNotFoundInMatch => "ValueNotFoundInMatch : A operacao de match foi incapaz de combinar com o padrao passado";

print("\n\nTESTANDO : NotAFunc | ");
run (fromString "var x = true; x(false)");