use "Plc.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

print("\n\nTESTES - exceptions in PlcInterp :  ");
use "test_PlcInterp_exceptions.sml";

print("\n *===========================================================* \n  ");

print("\n\nTESTANDO COMPUTACAO INFINITA - interrompa com ctrl c  ");
run ( fromString "fun rec f(Int x):Int = f(x - 1); f(0)" );
