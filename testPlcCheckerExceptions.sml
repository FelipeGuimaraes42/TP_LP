print("\n\nTESTANDO: EmptySeq");
run (fromString "(Bool [])");

print("\n\nTESTANDO: UnknownType");
run (fromString "false::false::true");

print("\n\nTESTANDO: NotEqTypes");
run(fromString "if false=1 then 1 else 0");

print("\n\nTESTANDO: WrongRetType");
run (fromString "fun rec f(Bool a, Bool b):Bool = if a!= 1 then a else b; f(false, true)");

print("\n\nTESTANDO: EmptySeq");
run (fromString "(Bool [])");

print("\n\nTESTANDO: EmptySeq");
run (fromString "(Bool [])");

print("\n\nTESTANDO: EmptySeq");
run (fromString "(Bool [])");

print("\n\nTESTANDO: EmptySeq");
run (fromString "(Bool [])");

print("\n\nTESTANDO: EmptySeq");
run (fromString "(Bool [])");

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