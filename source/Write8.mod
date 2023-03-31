MODULE Write8;  (* Character output convenience functions *)

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
PROCEDURE b*(i: INTEGER);         BEGIN WHILE i > 0 DO writebyte(ORD(' '));  DEC(i) END END b;

PROCEDURE h1*(i: INTEGER);  BEGIN IF i<10 THEN writebyte(i + 48) ELSE writebyte(i + 87) END END h1;

PROCEDURE hn*(i, n: INTEGER); BEGIN IF n>1 THEN hn(i DIV 16, n-1) END;  h1(i MOD 16) END hn;

PROCEDURE h*(n: INTEGER);
BEGIN
  IF (n < 0) OR (n > 15) THEN h((n DIV 16) MOD 1000000000000000H) END;
  h1(n MOD 16);
END h;

PROCEDURE hs*(n: INTEGER); BEGIN IF n < 0 THEN writebyte(ORD('-'));  n := -n END;  h(n) END hs;

PROCEDURE i*(n: INTEGER);
BEGIN
  IF n < 0 THEN writebyte(ORD('-')); n := -n END;
  IF n > 9 THEN i(n DIV 10) END;
  writebyte(n MOD 10 + 48)
END i;

PROCEDURE init;
CONST
  STD_OUTPUT_HANDLE = -11;
  UTF8              = 65001;
VAR result: INTEGER;
BEGIN
  SYSTEM.GetProcAddress(GetStdHandle,       Rtl.HKernel, SYSTEM.ADR(`GetStdHandle`));       ASSERT(GetStdHandle       # NIL);
  SYSTEM.GetProcAddress(SetConsoleOutputCP, Rtl.HKernel, SYSTEM.ADR(`SetConsoleOutputCP`)); ASSERT(SetConsoleOutputCP # NIL);
  SYSTEM.GetProcAddress(WriteFile,          Rtl.HKernel, SYSTEM.ADR(`WriteFile`));          ASSERT(WriteFile          # NIL);

  hOut := GetStdHandle(STD_OUTPUT_HANDLE);
  IF SetConsoleOutputCP # NIL THEN result := SetConsoleOutputCP(UTF8) END;
  crlf[0] := 13;  crlf[1] := 10;
END init;

BEGIN init
END Write8.
