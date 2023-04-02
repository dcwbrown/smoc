MODULE test;  (*$CONSOLE*)

IMPORT SYSTEM, Dumper, w := Writer;

CONST
  cs8 = "8 bit string";

VAR
  c: CHAR;
  i: INTEGER;
  s: ARRAY 10 OF CHAR;
  t: ARRAY 10 OF CHAR;
  b: BOOLEAN;

PROCEDURE assignvarstring(VAR s: ARRAY OF CHAR);
BEGIN
  s := "splurgle"
END assignvarstring;

BEGIN
  w.sl("Hello teapots.");

  w.s("w.i(12345):      "); w.i(12345);      w.l;
  w.s("w.h(12345):      "); w.h(12345);      w.l;
  w.s("w.hn(12345, 12): "); w.hn(12345, 12); w.l;
  w.s("w.hs(-25):       "); w.hs(-25);       w.l;
  w.s("w.c( X ):        "); w.c("X");        w.l;

  w.s("cs8:            '"); w.s(cs8);  w.sl("'.");

  c := CHR(113);
  w.s("c: '");  w.c(c);  w.sl("'.");

  c := "Q";
  w.s("c: '");  w.c(c);  w.sl("'.");
  Dumper.DumpVar("c", c);

  s := "Hello";
  w.s("s: '");  w.s(s);  w.sl("'.");
  Dumper.DumpVar("s", s);

  s[1] := "a";
  w.s("s: '");  w.s(s);  w.sl("'.");
  Dumper.DumpVar("s", s);

  t := "Hello";
  w.s("t: '");  w.s(t);  w.sl("'.");
  Dumper.DumpVar("t", t);

  IF s = t THEN w.sl("s=t.") ELSE w.sl("s#t.") END;
  IF s < t THEN w.sl("s<t.") ELSE w.sl("s¬<t.") END;
  IF s > t THEN w.sl("s>t.") ELSE w.sl("s¬>t.") END;
  w.sl("Set s[1] := 'e'.");
  s[1] := "e";
  IF s = t THEN w.sl("s=t.") ELSE w.sl("s#t.") END;
  IF s < t THEN w.sl("s<t.") ELSE w.sl("s¬<t.") END;
  IF s > t THEN w.sl("s>t.") ELSE w.sl("s¬>t.") END;

  s := $ 40 41
         42 43 00 $;
  w.s("s: '");  w.s(s);  w.sl("'.");
  s[2] := 50X;
  w.s("s: '");  w.s(s);  w.sl("'.");

  ASSERT(s[2] # 1X);

  assignvarstring(s);
  w.s("s: '");  w.s(s);  w.sl("'.");

  IF FALSE THEN i := 50;  s[i] := " " END;
  IF FALSE THEN ASSERT(FALSE)         END;
END test.
