(*
  x := function or variable name
  n := numerals
  e, e1, e2, e3 := PLC Expressions
  s, t, ti  := PLC Types
  p := type environment
  p' := new type enviroment
  p[x -> t] := enviroment that maps variable x to type t . 

  Expression 'e' is well-typed and has type t iff we can conclude that
  type(e,p) = t (the type of expression e in enviroment p is t)
  according to the rules defined in the documentation
*)

print("\n\nCASE 1 : type(x, p) = p(x) | ");
eval (fromString "x") [("x", IntV 15)];
run (fromString "var x = 15 ; x");

print("\n\nCASE 2 : type(n, p) = Int | ");
run (fromString "15");

print("\n\nCASE 3 and 4 : type(true|false, p) = Bool | ");
run (fromString "false");

print("\n\nCASE 5 : type( () , p ) = Nil | ");
run (fromString "()");

print("\n\nCASE 6 : type((e1, ..., en), p) = (t1, ..., tn) se n > 1 e type(ei, p) = ti para todo i = 1, ..., n | ");
run (fromString "(1,2,3,4,5,false,true)");

print("\n\nCASE 7 : type((t []), p) = t se t é um tipo sequência | ");
run (fromString "([Bool] [])");

print("\n\nCASE 8 : type(var x = e1 ; e2, p) = t2 se type(e1, p) = t1 e type(e2, p[x 7 -> t1]) = t2 para algum tipo t1 | ");
run (fromString "var x = 9; x + 1");

print("\n\nCASE 9 :  type(fun rec f (t x) : t1 = e1 ; e2, p) = t2 se type(e1, p[f 7 -> t -> t1][x 7 -> t]) = t1 e type(e2, p[f 7 -> t -> t1]) = t2*) | ");
run (fromString "fun rec f1(Int x):Int = x + 1; f1(12)");

print("\n\nCASE 10 : type(fn (s x) => e end, p) = s -> t se type(e, p[x 7 -> s]) = t | ");
run (fromString "fn (Int x) => -x end");

print("\n\nCASE 11 : type(e2(e1), p) = t2 se type(e2, p) = t1 -> t2 e type(e1, p) = t1 para algum tipo t1 | ");
run (fromString "fun sum5 (Int x) = x + 5; sum5(15)");

print("\n\nCASE 12 : type(if e then e1 else e2, p) = t se type(e, p) = Bool e type(e1, p) = type(e2, p) = t | ");
run (fromString "if 999 < 0 then true else false");

print("\n\nCASE 13 : type(match e with | e1 -> r1 | ...| en -> rn, p) = t se (a) type(e, p) = type(ei , p), para cada ei diferente de `__', e (b) type(r1, p) = . . . = type(rn, p) = t | ");
run (fromString "var x = true; match x with | true -> 999 end");

print("\n\nCASE 14 : type(!e, p) = Bool se type(e, p) = Bool | ");
run (fromString "!false");

print("\n\nCASE 15 : type(-e, p) = Int se type(e, p) = Int | ");
run (fromString "-(-(-5))");

print("\n\nCASE 16 : type(hd(e), p) = t se type(e, p) = [t] | ");
run (fromString "hd (1::2::3::4::([Int] []))");

print("\n\nCASE 17 : type(tl(e), p) = [t] se type(e, p) = [t] | ");
run (fromString "tl (1::2::3::4::([Int] []))");

print("\n\nCASE 18 : type(ise(e), p) = Bool se type(e, p) = [t] para algum tipo t | ");
run (fromString "ise (([Int] []))");

print("\n\nCASE 19 : type(print(e), p) = Nil se type(e, p) = t para algum tipo t | ");
run (fromString "print (15::20::25::(([Int] [])))");

print("\n\nCASE 20 : type(e1, p) = type(e2, p) = Bool | ");
run (fromString "true && false");

print("\n\nCASE 21 : type(e1, p) = t e type(e2, p) = [t] | ");
run (fromString "false::false::(3<5)::(false=false)::([Bool] [])");

print("\n\nCASE 22 : op E {+, -, *, /} e type(e1, p) = type(e2, p) = Int | ");
run (fromString "var x = 15; ((x+7-2) * 10) / 100");

print("\n\nCASE 23 : op E {<, <=} e type(e1, p) = type(e2, p) = Int | ");
run (fromString "( (5<3) , (999 <= 191919) , (if 200 < 5000 then 0 else 1) )");

print("\n\nCASE 24 :  op E {=, !=} e type(e1, p) = type(e2, p) = t para algum tipo de igualdade t | ");
run (fromString "( (5!=3) , (1 = 1) , (true=false) , (false != true) )");

print("\n\nCASE 25 : type(e [i], p) = ti se type(e, p) = (t1, ..., tn) para algum n > 1 e tipos t1, . . . , tn, e i E {1, . . . , n} | ");
run (fromString "(6, false)[1]");

print("\n\nCASE 26 : type(e1, p) = t1 para algum tipo t e type(e2, p) = t2 | ");
run (fromString "var x = 5; x * x");