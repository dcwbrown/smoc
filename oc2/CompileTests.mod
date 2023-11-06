MODULE CompileTests;  IMPORT SYSTEM;

CONST SomeString = "Some string.";

TYPE
  r1 = POINTER TO r1desc;
  r1desc* = RECORD next: r1 END;

  r2 = POINTER TO r2desc;
  r2desc* = RECORD (r1desc) n2: r2 END;

  r3 = POINTER TO r3desc;
  r3desc* = RECORD (r2desc) n3: r3 END;

VAR  b1, b2, b3: BOOLEAN;  i: INTEGER;

BEGIN
(*
  IF    b1 THEN
    i := 1
  ELSIF b2 THEN
    i := 2
  ELSIF b3 THEN
    i := 3
  ELSE
    i := 0
  END;
*)

  IF b1 OR b2 OR b3 THEN
    i := 1
  END;

  i := 99
END CompileTests.


(*
CONST
  c = "a";
  s = "Hello";

TYPE 
  A = ARRAY 123 OF INTEGER;
  R = RECORD x: INTEGER END;

VAR 
  a,b: A;
  r:   R;

PROCEDURE bottom(a: ARRAY OF INTEGER);
BEGIN
END bottom;

PROCEDURE middle(a: ARRAY OF INTEGER);
BEGIN
  bottom(a)
END middle;

PROCEDURE check(a: ARRAY OF INTEGER);
VAR i: INTEGER;
BEGIN
  i := a[100];
  i := ABS(i);
  i := ABS(-1);
  IF ODD(i) THEN END;
END check;

PROCEDURE top;
BEGIN
  middle(a)
END top;

BEGIN
  SYSTEM.COPY(10, 20, 4);
END CompileTests.
*)
