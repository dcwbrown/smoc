MODULE Winload;  IMPORT SYSTEM;

CONST title = "Compilation Tests";

VAR
  (* The first 3 64 bit values in Winload global VAR space are reloaded by
     directions in EXE file *)
  LoadLibraryA:       PROCEDURE#(libname: INTEGER): INTEGER;
  GetProcAddress:     PROCEDURE#(hmodule, procname: INTEGER): INTEGER;
  MessageBoxA*:       PROCEDURE#(hwnd, text, caption, type: INTEGER);
  Kernel:             INTEGER;
  GetStdHandle:       PROCEDURE#(nStdHandle: SYSTEM.CARD32): INTEGER;
  SetConsoleOutputCP: PROCEDURE#(codepage: INTEGER) (* : INTEGER *);
  WriteFile:          PROCEDURE#(hFile, lpBuffer, nNumberOfBytesToWrite,
                                 lpNumberOfBytesWritten, lpOverlapped: INTEGER
                      ): SYSTEM.CARD32;
  StdOut:             INTEGER;


PROCEDURE msg*(message: ARRAY OF CHAR);
BEGIN
  MessageBoxA(0, SYSTEM.ADR(message), SYSTEM.ADR("Winload"), 0)
END msg;

PROCEDURE assert(expectation: BOOLEAN);
BEGIN IF ~expectation THEN msg("Assertion failure.") END
END assert;

PROCEDURE GetProc*(dll: INTEGER; name: ARRAY OF CHAR; VAR proc: ARRAY OF BYTE);
VAR adr: INTEGER;
BEGIN
  (*msg("GetProc entry");*)
  adr := GetProcAddress(dll, SYSTEM.ADR(name));
  (*IF adr = 0 THEN msg("Got 0") ELSE msg("Got nonzero") END;*)
  SYSTEM.PUT(SYSTEM.ADR(proc), adr);
  (*msg("GetProc exit");*)
END GetProc;

PROCEDURE WriteConsoleBytes(adr, len: INTEGER);
VAR written, result: INTEGER;
BEGIN
  (*msg("WriteConsoleBytes entry.");*)
  result := WriteFile(StdOut, adr, len, SYSTEM.ADR(written), 0);
  (*msg("WriteConsoleBytes complete.");*)
END WriteConsoleBytes;

PROCEDURE ConsoleNewline;
VAR c: CHAR;
BEGIN
  c := 0DX;  WriteConsoleBytes(SYSTEM.ADR(c), 1);
  c := 0AX;  WriteConsoleBytes(SYSTEM.ADR(c), 1);
END ConsoleNewline;


BEGIN
  Kernel := LoadLibraryA(SYSTEM.ADR("kernel32"));
  IF Kernel = 0 THEN
    msg("Couldn't load kernel.")
  ELSE
    msg("Kernel loaded.")
  END;

  GetProc(Kernel, "WriteFile",          WriteFile);          assert(WriteFile          # NIL);
  GetProc(Kernel, "GetStdHandle",       GetStdHandle);       assert(GetStdHandle       # NIL);
  GetProc(Kernel, "SetConsoleOutputCP", SetConsoleOutputCP); assert(SetConsoleOutputCP # NIL);

  StdOut := GetStdHandle(-11);  (* StdOutputHandle = -11 *)
  SetConsoleOutputCP(65001);    (* UTF8 = 65001 *)

  WriteConsoleBytes(SYSTEM.ADR("Hello."), 6);
  ConsoleNewline;

  msg("Winload complete.")

END Winload.
