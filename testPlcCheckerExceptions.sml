print("\n\nTESTANDO: EmptySeq");
run (fromString "(Bool [])");

print("\n\nTESTANDO: UnknownType");
run (fromString "false::false::true");

print("\n\nTESTANDO: NotEqTypes");
run(fromString "if false=1 then 1 else 0");

print("\n\nTESTANDO: WrongRetType");
run (fromString "fun rec f1(Int x):Bool = x + 1; f1(12)");

print("\n\nTESTANDO: DiffBrTypes");
run(fromString "if 1= 1 then 1 else false");

print("\n\nTESTANDO: IfCondNotBool");
run (fromString "if 5 then 5 else 1");

print("\n\nTESTANDO: NoMatchResults");
run (fromString "match 1 with end");

print("\n\nTESTANDO: MatchResTypeDiff");
run (fromString "var x = 5; match x with | 0 -> 1 | _ -> false end");

print("\n\nTESTANDO: MatchCondTypesDiff");
run (fromString "var x = 5; match x with | 0 -> 1 | false -> 0 |_ -> 2 end");

print("\n\nTESTANDO: CallTypeMisM");
run (fromString "fun f(Bool a) = true; f(1)");

print("\n\nTESTANDO: NotFunc");
run (fromString "var f = true; f(true)");

print("\n\nTESTANDO: ListOutOfRange");
run (fromString "(2, false)[3]");

print("\n\nTESTANDO: OpNonList");
run (fromString "var x = 5; x[1]");
