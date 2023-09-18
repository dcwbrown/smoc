MODULE CompileTests;  IMPORT SYSTEM;

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
