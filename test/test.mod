MODULE test;  (*$CONSOLE*)

IMPORT Dumper;

VAR
  c: CHAR8;
  s: ARRAY 10 OF CHAR8;

BEGIN
  c := `Q`;
  Dumper.DumpVar("c", c);

  s := `Hello`;
  Dumper.DumpVar("s", s);

  s[1] := `a`;
  Dumper.DumpVar("s", s);

END test.
