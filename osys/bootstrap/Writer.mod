MODULE Writer;  (* Character output convenience functions *)

IMPORT SYSTEM, K := Kernel;

VAR
  crlf:     ARRAY 2 OF CHAR;
  skipline: BOOLEAN;

PROCEDURE write(bytes: ARRAY OF BYTE);
BEGIN IF ~skipline THEN K.WriteLog(SYSTEM.ADR(bytes), LEN(bytes)) END END write;

PROCEDURE writebyte(b: BYTE);
BEGIN IF ~skipline THEN K.WriteLog(SYSTEM.ADR(b), 1) END END writebyte;

PROCEDURE writesz(bytes: ARRAY OF BYTE);
VAR len: INTEGER;
BEGIN IF ~skipline THEN
  len := 0;  WHILE (len < LEN(bytes)) & (bytes[len] # 0) DO INC(len) END;
  K.WriteLog(SYSTEM.ADR(bytes), len)
END END writesz;

PROCEDURE SkipLine*; BEGIN skipline := TRUE END SkipLine;

PROCEDURE l*();
BEGIN
  IF skipline THEN skipline := FALSE ELSE write(crlf) END
END l;

PROCEDURE c*(c: CHAR);           BEGIN write(c)    END c;
PROCEDURE s*(s: ARRAY OF CHAR);  BEGIN writesz(s)  END s;
PROCEDURE sl*(t: ARRAY OF CHAR); BEGIN s(t); l     END sl;

PROCEDURE b*(n: INTEGER);
BEGIN WHILE n > 0 DO writebyte(ORD(" ")); DEC(n) END END b;

PROCEDURE sn*(s: ARRAY OF CHAR; n: INTEGER);  (* n<0 spaces to left, >0 spaces to right *)
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

PROCEDURE zn*(s: ARRAY OF CHAR; n: INTEGER);  (* n<0 zeroes to left, >0 spaces to right *)
VAR l: INTEGER;
BEGIN
  IF n >= 0 THEN sn(s, n) ELSE
    l := 0;  n := -n;
    WHILE (l < LEN(s)) & (l < n) & (s[l] # 0X) DO INC(l) END;
    WHILE n > l DO writebyte(ORD("0"));  DEC(n) END;
    K.WriteLog(SYSTEM.ADR(s), l)
  END
END zn;

PROCEDURE h1*(i: INTEGER);
BEGIN IF i<10 THEN writebyte(i + 48) ELSE writebyte(i + 55) END END h1;

PROCEDURE h*(i: INTEGER);
VAR num: ARRAY 20 OF CHAR;
BEGIN K.IntToHex(i, num);  s(num) END h;

PROCEDURE hn*(i, n: INTEGER);
VAR num: ARRAY 20 OF CHAR;
BEGIN K.IntToHex(i, num);  zn(num, n) END hn;

PROCEDURE hs*(i: INTEGER);
BEGIN IF i < 0 THEN writebyte(ORD("-"));  i := -i END;  h(i) END hs;

PROCEDURE i*(j: INTEGER);
VAR num: ARRAY 24 OF CHAR;
BEGIN K.IntToDecimal(j, num);  s(num) END i;

PROCEDURE in*(i, n: INTEGER);
VAR num: ARRAY 24 OF CHAR;
BEGIN K.IntToDecimal(i, num);  sn(num, n) END in;

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

BEGIN
  crlf := $0D 0A$;
  skipline := FALSE;
END Writer.
