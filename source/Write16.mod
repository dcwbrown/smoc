MODULE Write16;  (* Character output convenience functions *)

IMPORT Out;

VAR ii: INTEGER;  (* Workaround for bad PE when no globals. *)

PROCEDURE l*();                  BEGIN Out.Ln        END l;
PROCEDURE c*(c: CHAR);           BEGIN Out.Char(c)   END c;
PROCEDURE s*(s: ARRAY OF CHAR);  BEGIN Out.String(s) END s;
PROCEDURE sl*(t: ARRAY OF CHAR); BEGIN s(t); l       END sl;
PROCEDURE b*(i: INTEGER);        BEGIN WHILE i > 0 DO c(' ');  DEC(i) END END b;

PROCEDURE h1*(i: INTEGER);    BEGIN IF i<10 THEN c(CHR(i + 48)) ELSE c(CHR(i + 87)) END END h1;
PROCEDURE hn*(i, n: INTEGER); BEGIN IF n>1 THEN hn(i DIV 16, n-1) END;  h1(i MOD 16)    END hn;

PROCEDURE h*(n: INTEGER);
BEGIN
  IF (n < 0) OR (n > 15) THEN h((n DIV 16) MOD 1000000000000000H) END;
  h1(n MOD 16);
END h;

PROCEDURE hs*(n: INTEGER); BEGIN IF n < 0 THEN c("-");  n := -n END;  h(n) END hs;

PROCEDURE i*(n: INTEGER);
BEGIN
  IF n < 0 THEN c('-'); n := -n END;
  IF n > 9 THEN i(n DIV 10) END;
  c(CHR(n MOD 10 + 48))
END i;


END Write16.
