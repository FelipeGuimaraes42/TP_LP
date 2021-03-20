use "Plc.sml";

teval (fromString "42") [];
teval (fromString "false") [];
teval (fromString "()") [];
teval (fromString "(42, true)") [];
teval (fromString "(42, false) [1]");
teval (fromString "0::4::2::([Int] [])") [];
teval (fromString "false::false::false::([Bool] [])") [];
teval (fromString "var x = 40; x + 2") [];
teval (fromString "var x = 40; x + y") [("y", IntT)];
teval (fromString "match x with | 0 -> 1 | _ -> -1 end") [("x", IntT)];