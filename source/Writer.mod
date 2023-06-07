MODULE Writer;  (* Character output convenience functions *)

IMPORT SYSTEM, K := Kernel;

CONST crlf = $ 0D 0A $;

PROCEDURE write(bytes: ARRAY OF BYTE);
BEGIN K.WriteLog(SYSTEM.ADR(bytes), LEN(bytes)) END write;

PROCEDURE writebyte(b: BYTE);
BEGIN K.WriteLog(SYSTEM.ADR(b), 1) END writebyte;

PROCEDURE writesz(bytes: ARRAY OF BYTE);
VAR len: INTEGER;
BEGIN
  len := 0;  WHILE (len < LEN(bytes)) & (bytes[len] # 0) DO INC(len) END;
  K.WriteLog(SYSTEM.ADR(bytes), len)
END writesz;

PROCEDURE l*();                  BEGIN write(crlf) END l;
PROCEDURE c*(c: CHAR);           BEGIN write(c)    END c;
PROCEDURE s*(s: ARRAY OF CHAR);  BEGIN writesz(s)  END s;
PROCEDURE sl*(t: ARRAY OF CHAR); BEGIN s(t); l     END sl;

PROCEDURE b*(n: INTEGER);
BEGIN WHILE n > 0 DO writebyte(ORD(" ")); DEC(n) END END b;

PROCEDURE sn*(s: ARRAY OF CHAR; n: INTEGER);
VAR l, w: INTEGER;
BEGIN l := 0;
  IF n < 0 THEN w := -n ELSE w := n END;
  WHILE (l < LEN(s)) & (l < w) & (s[l] # 0X) DO INC(l) END;
  IF n < 0 THEN
    b(w-l);  K.WriteLog(SYSTEM.ADR(s), l)
  ELSE
    K.WriteLog(SYSTEM.ADR(s), l);  b(w-l)
  END
END sn;

PROCEDURE h1*(i: INTEGER);
BEGIN IF i<10 THEN writebyte(i + 48) ELSE writebyte(i + 55) END END h1;

PROCEDURE hn*(i, n: INTEGER);
BEGIN
  IF n>1 THEN hn(i DIV 16, n-1) END;
  h1(i MOD 16)
END hn;

PROCEDURE h*(i: INTEGER);
BEGIN
  IF (i < 0) OR (i > 15) THEN h((i DIV 16) MOD 1000000000000000H) END;
  h1(i MOD 16);
END h;

PROCEDURE hs*(i: INTEGER);
BEGIN IF i < 0 THEN writebyte(ORD("-"));  i := -i END;  h(i) END hs;

PROCEDURE i*(j: INTEGER);
BEGIN
  IF j < 0 THEN writebyte(ORD("-")); j := -j END;
  IF j > 9 THEN i(j DIV 10) END;
  writebyte(j MOD 10 + 48)
END i;

PROCEDURE ini*(i, n: INTEGER);
BEGIN
  IF n > 1 THEN ini(i DIV 10, n-1) END;
  IF (i = 0) THEN c(" ") ELSE writebyte(i MOD 10 + 48) END
END ini;

PROCEDURE in*(i, n: INTEGER);
BEGIN
  IF i = 0 THEN b(n-1); c("0")
  ELSE
    IF i < 0 THEN writebyte(ORD("-"));  i := -i;  DEC(n) END;
    ini(i, n)
  END
END in;

PROCEDURE DumpMem*(indent, adr, start, len: INTEGER);
VAR
  rowstart, i, dumplimit: INTEGER;
  byte:  BYTE;
  bytes: ARRAY 16 OF INTEGER;
BEGIN
  rowstart  := (       start       DIV 16) * 16;
  dumplimit := ((start + len + 15) DIV 16) * 16;
  WHILE rowstart < dumplimit DO
    b(indent); hn(rowstart, 12); s("  ");

    (* Load a row of bytes *)
    FOR i := 0 TO 15 DO
      IF (rowstart+i >= start) & (rowstart+i < start+len) THEN
        SYSTEM.GET(rowstart-start+adr+i, byte);  bytes[i] := byte
      ELSE
        bytes[i] := -1
      END
    END;

    (* One row of hex Dump *)
    FOR i := 0 TO 15 DO
      IF i MOD 8 = 0 THEN c(" ") END;
      IF bytes[i] >= 0 THEN hn(bytes[i], 2);  c(" ") ELSE s("   ") END;
    END;
    s("  ");

    (* One row of character Dump *)
    FOR i := 0 TO 15 DO
      IF bytes[i] >= 0 THEN
        IF (bytes[i] < 32) OR (bytes[i] >= 127) THEN
          c(".")
        ELSE
          c(CHR(bytes[i]))
        END
      ELSE
        c(" ")
      END
    END;

    l;
    INC(rowstart, 16);
  END
END DumpMem;

BEGIN END Writer.
