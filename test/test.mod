MODULE test;  (*$CONSOLE*)

IMPORT Dumper, w := Write8;

VAR
  c: CHAR8;
  s: ARRAY 10 OF CHAR8;
  t: ARRAY 10 OF CHAR8;

BEGIN
  w.sl(`Hello teapots.`);
  w.s(`w.i(12345):      `); w.i(12345);      w.l;
  w.s(`w.h(12345):      `); w.h(12345);      w.l;
  w.s(`w.hn(12345, 12): `); w.hn(12345, 12); w.l;
  w.s(`w.hs(-25):       `); w.hs(-25);       w.l;
  w.s(`w.c( X ):        `); w.c(`X`);        w.l;

  c := CHR8(113);
  w.s(`c: '`);  w.c(c);  w.sl(`'.`);

  c := `Q`;
  w.s(`c: '`);  w.c(c);  w.sl(`'.`);
  Dumper.DumpVar("c", c);

  s := `Hello`;
  w.s(`s: '`);  w.s(s);  w.sl(`'.`);
  Dumper.DumpVar("s", s);

  s[1] := `a`;
  w.s(`s: '`);  w.s(s);  w.sl(`'.`);
  Dumper.DumpVar("s", s);

  t := `Hello`;
  w.s(`t: '`);  w.s(t);  w.sl(`'.`);
  Dumper.DumpVar("t", t);

  IF s = t THEN w.sl(`s=t.`) ELSE w.sl(`s#t.`) END;
  IF s < t THEN w.sl(`s<t.`) ELSE w.sl(`s¬<t.`) END;
  IF s > t THEN w.sl(`s>t.`) ELSE w.sl(`s¬>t.`) END;
  s[1] := `e`;
  IF s = t THEN w.sl(`s=t.`) ELSE w.sl(`s#t.`) END;

  s := $$40 41
         42 43 00$;
  w.s(`s: '`);  w.s(s);  w.sl(`'.`);

END test.
