MODULE Winload;  IMPORT SYSTEM;

CONST title = "Compilation Tests";

VAR
  (* The first 3 64 bit values in Winload global VAR space are preloaded by
     link instructions in the EXE file *)
  LoadLibraryA*:      PROCEDURE#(libname: INTEGER): INTEGER;
  GetProcAddress*:    PROCEDURE#(hmodule, procname: INTEGER): INTEGER;
  MessageBoxA*:       PROCEDURE#(hwnd, text, caption, type: INTEGER);

PROCEDURE msg*(message: ARRAY OF CHAR);
BEGIN
  MessageBoxA(0, SYSTEM.ADR(message), SYSTEM.ADR("Winload"), 0)
END msg;

PROCEDURE assert(expectation: BOOLEAN);
BEGIN IF ~expectation THEN msg("Assertion failure.") END
END assert;

PROCEDURE IntToHex*(n: INTEGER; VAR s: ARRAY OF CHAR);
VAR d, i, j: INTEGER;  ch: CHAR;
BEGIN
  i := 0;  j := 0;
  REPEAT
    d := n MOD 16;  n := n DIV 16 MOD 1000000000000000H;
    IF d <= 9 THEN s[j] := CHR(d + 48) ELSE s[j] := CHR(d + 55) END;
    INC(j)
  UNTIL n = 0;
  s[j] := 0X;  DEC(j);
  WHILE i < j DO ch:=s[i]; s[i]:=s[j]; s[j]:=ch; INC(i); DEC(j) END;
END IntToHex;

PROCEDURE msghex(n: INTEGER);
VAR s: ARRAY 32 OF CHAR;
BEGIN IntToHex(n, s);  msg(s) END msghex;

PROCEDURE msgsp;
VAR i: INTEGER;
BEGIN msghex(SYSTEM.ADR(i)+16) END msgsp;

PROCEDURE GetProc*(dll: INTEGER; name: ARRAY OF CHAR; VAR proc: ARRAY OF BYTE);
VAR adr: INTEGER;
BEGIN
  adr := GetProcAddress(dll, SYSTEM.ADR(name));
  SYSTEM.PUT(SYSTEM.ADR(proc), adr)
END GetProc;

PROCEDURE BOOTSTRAP;
VAR
  Kernel:             INTEGER;
  GetStdHandle:       PROCEDURE#(nStdHandle: SYSTEM.INT32): INTEGER;
  SetConsoleOutputCP: PROCEDURE#(codepage: INTEGER) (* : INTEGER *);
  WriteFile:          PROCEDURE#(hFile, lpBuffer, nNumberOfBytesToWrite,
                                 lpNumberOfBytesWritten, lpOverlapped: INTEGER
                      ): SYSTEM.CARD32;
  StdOut:             INTEGER;
  result:             INTEGER;
  written:            INTEGER;
  crlf:               ARRAY 2 OF CHAR;
BEGIN
  Kernel := LoadLibraryA(SYSTEM.ADR("kernel32")); assert(Kernel # 0);
  GetProc(Kernel, "WriteFile",          WriteFile);          assert(WriteFile          # NIL);
  GetProc(Kernel, "GetStdHandle",       GetStdHandle);       assert(GetStdHandle       # NIL);
  GetProc(Kernel, "SetConsoleOutputCP", SetConsoleOutputCP); assert(SetConsoleOutputCP # NIL);
  StdOut := GetStdHandle(-11);  (* -11:   StdOutputHandle *)
  SetConsoleOutputCP(65001);    (* 65001: UTF8            *)
  result := WriteFile(StdOut, SYSTEM.ADR("Hello."), 6, SYSTEM.ADR(written), 0);
  crlf := $0D 0A$;
  result := WriteFile(StdOut, SYSTEM.ADR(crlf), 2, SYSTEM.ADR(written), 0);
END BOOTSTRAP;


BEGIN
  BOOTSTRAP;
END Winload.
