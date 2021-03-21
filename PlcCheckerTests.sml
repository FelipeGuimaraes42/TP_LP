print("\n\TESTANDO: Casos Corretamente Tipados")

run (fromString "42");
run (fromString "false");
run (fromString "()");
run (fromString "(42, true)");
run (fromString "(42, false) [1]");
run (fromString "0::4::2::([Int] [])");
run (fromString "false::false::false::([Bool] [])");
run (fromString "var x = 40; x + 2");
run (fromString "var x = 40; var y = 2; x + y");
run (fromString "var x = 42; match x with | 0 -> 1 | _ -> -1 end");

teval (fromString "var x = 40; x + y") [("y", IntT)];