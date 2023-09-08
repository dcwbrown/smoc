MODULE CompileTests;

CONST
  s = "Hello";

TYPE 
  A = ARRAY 123 OF INTEGER;
  R = RECORD x: INTEGER END;

VAR 
  a: A;
  r: R;

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
  i := a[100]
END check;

PROCEDURE top;
BEGIN
  middle(a)
END top;

END CompileTests.
