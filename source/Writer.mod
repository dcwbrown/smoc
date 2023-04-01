MODULE Writer;  (* Character output convenience functions *)

IMPORT SYSTEM, Rtl;

VAR
  GetStdHandle:       PROCEDURE(nStdHandle: SYSTEM.CARD32): INTEGER;
  SetConsoleOutputCP: PROCEDURE(codepage: INTEGER): INTEGER;

  WriteFile: PROCEDURE(
               hFile, lpBuffer, nNumberOfBytesToWrite,
               lpNumberOfBytesWritten, lpOverlapped: INTEGER
             ): SYSTEM.CARD32;

  hOut: INTEGER;
  crlf: ARRAY 2 OF BYTE;

PROCEDURE writebuf(adr, len: INTEGER);
VAR written, result: INTEGER;
BEGIN result := WriteFile(hOut, adr, len, SYSTEM.ADR(written), 0) END writebuf;


PROCEDURE write(VAR bytes: ARRAY OF BYTE);
BEGIN writebuf(SYSTEM.ADR(bytes), LEN(bytes)) END write;

PROCEDURE writebyte(b: BYTE); BEGIN write(b) END writebyte;

PROCEDURE writesz(bytes: ARRAY OF BYTE);
VAR len: INTEGER;
BEGIN
  len := 0;  WHILE (len < LEN(bytes)) & (bytes[len] # 0) DO INC(len) END;
  writebuf(SYSTEM.ADR(bytes), len)
END writesz;

PROCEDURE l*();                   BEGIN write(crlf) END l;
PROCEDURE c*(c: CHAR8);           BEGIN write(c)    END c;
PROCEDURE s*(s: ARRAY OF CHAR8);  BEGIN writesz(s)  END s;
PROCEDURE sl*(t: ARRAY OF CHAR8); BEGIN s(t); l     END sl;
PROCEDURE b*(n: INTEGER);         BEGIN WHILE n > 0 DO writebyte(ORD(` `)); DEC(n) END END b;

PROCEDURE h1*(i: INTEGER);  BEGIN IF i<10 THEN writebyte(i + 48) ELSE writebyte(i + 87) END END h1;

PROCEDURE hn*(i, n: INTEGER); BEGIN IF n>1 THEN hn(i DIV 16, n-1) END;  h1(i MOD 16) END hn;

PROCEDURE h*(i: INTEGER);
BEGIN
  IF (i < 0) OR (i > 15) THEN h((i DIV 16) MOD 1000000000000000H) END;
  h1(i MOD 16);
END h;

PROCEDURE hs*(i: INTEGER); BEGIN IF i < 0 THEN writebyte(ORD(`-`));  i := -i END;  h(i) END hs;

PROCEDURE i*(j: INTEGER);
BEGIN
  IF j < 0 THEN writebyte(ORD(`-`)); j := -j END;
  IF j > 9 THEN i(j DIV 10) END;
  writebyte(j MOD 10 + 48)
END i;

PROCEDURE ini*(i, n: INTEGER);
BEGIN
  IF n > 1 THEN ini(i DIV 10, n-1) END;
  IF (i = 0) THEN c(` `) ELSE writebyte(i MOD 10 + 48) END
END ini;

PROCEDURE in*(i, n: INTEGER);
BEGIN
  IF i = 0 THEN b(n-1); c(`0`)
  ELSE
    IF i < 0 THEN writebyte(ORD(`-`));  i := -i;  DEC(n) END;
    ini(i, n)
  END
END in;

PROCEDURE init;
CONST
  STD_OUTPUT_HANDLE = -11;
  UTF8              = 65001;
VAR result: INTEGER;
BEGIN
  SYSTEM.GetProcAddress(GetStdHandle,       Rtl.HKernel, SYSTEM.ADR(`GetStdHandle`));       ASSERT(GetStdHandle       # NIL);
  SYSTEM.GetProcAddress(SetConsoleOutputCP, Rtl.HKernel, SYSTEM.ADR(`SetConsoleOutputCP`)); ASSERT(SetConsoleOutputCP # NIL);
  SYSTEM.GetProcAddress(WriteFile,          Rtl.HKernel, SYSTEM.ADR(`WriteFile`));          ASSERT(WriteFile          # NIL);

  hOut    := GetStdHandle(STD_OUTPUT_HANDLE);
  result  := SetConsoleOutputCP(UTF8);
  crlf[0] := 13;  crlf[1] := 10;
END init;

BEGIN init
END Writer.
