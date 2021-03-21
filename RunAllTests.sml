use "Plc.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

print("\n\nTestando casos de acordo com regras de tipagem :  ");
use "testPlcCases.sml";

print("\n *===========================================================* \n  ");

print("\n\nTESTES - Exceptions em PlcInterp.sml :  ");
use "testPlcInterpExceptions.sml";

print("\n *===========================================================* \n  ");

print("\n\nTESTANDO COMPUTACAO INFINITA - interrompa com ctrl c  ");
run ( fromString "fun rec f(Int x):Int = f(x - 1); f(0)" );
