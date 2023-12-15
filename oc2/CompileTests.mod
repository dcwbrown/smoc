MODULE CompileTests;  IMPORT SYSTEM;

CONST title = "Compilation Tests";

TYPE
  mbproc = PROCEDURE#(hwnd, text, caption, type: INTEGER);

VAR
  x:  mbproc;
  go: PROCEDURE(x,y: INTEGER);

PROCEDURE a(x,y: INTEGER); BEGIN END a;

PROCEDURE b(p: PROCEDURE(x,y: INTEGER)); BEGIN p(123,321) END b;

PROCEDURE c(m: mbproc);
BEGIN
  m(0, SYSTEM.ADR("win abi by param"), SYSTEM.ADR(title), 0);
END c;

BEGIN
  go := a;
  b(a);
  x(0, SYSTEM.ADR("Hello"), SYSTEM.ADR(title), 0);
  go := a;
  b(a);
  c(x);
END CompileTests.







(* -----------------------------------------------------------------------------

CONST SomeString = "Some string.";

TYPE
  r1 = POINTER TO r1desc;
  r1desc* = RECORD next: r1 END;

  r2 = POINTER TO r2desc;
  r2desc* = RECORD (r1desc) n2: r2 END;

  r3 = POINTER TO r3desc;
  r3desc* = RECORD (r2desc) n3: r3 END;

VAR
  b1, b2, b3: BOOLEAN;
  i: INTEGER;
  p: PROCEDURE(i: INTEGER);

PROCEDURE fred(i: INTEGER);
VAR j: INTEGER;
BEGIN
  b1 := TRUE;
  j := i;
END fred;

PROCEDURE bert(i: INTEGER): INTEGER;
BEGIN RETURN i + 1 END bert;

BEGIN
  IF    b1 THEN
    i := 1
  ELSIF b2 THEN
    i := 2
  ELSIF b3 THEN
    i := 3
  ELSE
    i := 0
  END;

  fred(5);

  IF b1 OR b2 OR b3 THEN
    i := 1
  END;

  i := 99;

  i := bert(2);
  i := 5 + 7 + (bert(4) + (bert(3) - 6) + 2);

  p := fred;
  p(5);

END CompileTests.


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
