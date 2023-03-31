MODULE test;  (*$CONSOLE*)

IMPORT Dumper, w := Write8;

CONST
  cs8 = `8 bit string`;

VAR
  c: CHAR8;
  i: INTEGER;
  s: ARRAY 10 OF CHAR8;
  t: ARRAY 10 OF CHAR8;
  u: ARRAY 10 OF CHAR16;
  v: ARRAY 10 OF CHAR16;
  b: BOOLEAN;

BEGIN
  w.sl(`Hello teapots.`);

  ASSERT(TRUE);
  b := u # v;
  ASSERT(TRUE);
  b := s # t;
  ASSERT(TRUE);

  w.s(`w.i(12345):      `); w.i(12345);      w.l;
  w.s(`w.h(12345):      `); w.h(12345);      w.l;
  w.s(`w.hn(12345, 12): `); w.hn(12345, 12); w.l;
  w.s(`w.hs(-25):       `); w.hs(-25);       w.l;
  w.s(`w.c( X ):        `); w.c(`X`);        w.l;

  w.s(`cs8:            '`); w.s(cs8);  w.sl(`'.`);

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
  w.sl(`Set s[1] := 'e'.`);
  s[1] := `e`;
  IF s = t THEN w.sl(`s=t.`) ELSE w.sl(`s#t.`) END;
  IF s < t THEN w.sl(`s<t.`) ELSE w.sl(`s¬<t.`) END;
  IF s > t THEN w.sl(`s>t.`) ELSE w.sl(`s¬>t.`) END;

  s := $$40 41
         42 43 00$;
  w.s(`s: '`);  w.s(s);  w.sl(`'.`);
  s[2] := 50Y;
  w.s(`s: '`);  w.s(s);  w.sl(`'.`);

  ASSERT(s[2] # 1Y);

  IF FALSE THEN i := 50;  s[i] := ` ` END;
  IF FALSE THEN ASSERT(FALSE)         END;
END test.
