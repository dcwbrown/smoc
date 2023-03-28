MODULE test;  (*$CONSOLE*)

IMPORT Dumper, w := Write16, Write8;

VAR
  c: CHAR8;
  s: ARRAY 10 OF CHAR8;
  t: ARRAY 10 OF CHAR8;

BEGIN
  w.sl("Hello teapots.");
  Write8.sl(`Hello 8 teapots.`);

  c := `Q`;
  Dumper.DumpVar("c", c);

  s := `Hello`;
  Dumper.DumpVar("s", s);

  s[1] := `a`;
  Dumper.DumpVar("s", s);

  t := `Hello`;
  Dumper.DumpVar("t", t);

END test.
