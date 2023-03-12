MODULE test;  (*$CONSOLE*) (*$TRACE*)

IMPORT Out;

TYPE
  intrec = RECORD
             i: INTEGER
           END;
VAR
  i: INTEGER;
  j: POINTER TO intrec;
  s: ARRAY 10 OF CHAR;
  c: CHAR;

BEGIN
  Out.String("Hello teapots.");  Out.Ln;

  s := 'Hello';
  i := 20;
  (* c := s[20];*)
  c := s[i];

  i := 0;
  i := 5 DIV i;

  j := NIL;
  i := j.i;

  ASSERT(FALSE);

END test.
