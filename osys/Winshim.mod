MODULE Winshim;  IMPORT SYSTEM;

TYPE
  CodeHeaderPtr = POINTER TO CodeHeader;
  CodeHeader* = RECORD
    length*:   SYSTEM.CARD32;  (* File length *)
    initcode*: SYSTEM.CARD32;
    pointers*: SYSTEM.CARD32;
    commands*: SYSTEM.CARD32;
    exports*:  SYSTEM.CARD32;
    imports*:  SYSTEM.CARD32;  (* VARs start here following import resolution *)
    varsize*:  SYSTEM.CARD32;
    key*:      INTEGER;
  END;


VAR
  (* WinPE.mod builds the executable with the following Winshim variables pre-loaded *)
  Header: CodeHeaderPtr;

  (* Pre-loaded Kernel32 imports *)
  LoadLibraryA:                   PROCEDURE-(libname: INTEGER): INTEGER;
  GetProcAddress:                 PROCEDURE-(hmodule, procname: INTEGER): INTEGER;
  VirtualAlloc:                   PROCEDURE-(address, size, type, protection: INTEGER): INTEGER;
  ExitProcess:                    PROCEDURE-(exitcode: INTEGER);
  GetStdHandle:                   PROCEDURE-(nStdHandle: SYSTEM.INT32): INTEGER;
  SetConsoleOutputCP:             PROCEDURE-(codepage: INTEGER) (* : INTEGER *);
  WriteFile:                      PROCEDURE-(hFile, lpBuffer, nNumberOfBytesToWrite,
                                             lpNumberOfBytesWritten, lpOverlapped: INTEGER
                                            ): SYSTEM.CARD32;
  AddVectoredExceptionHandler:    PROCEDURE-(first, filter: INTEGER);
  GetCommandLineW:                PROCEDURE-(): INTEGER;
  GetSystemTimePreciseAsFileTime: PROCEDURE-(tickAdr: INTEGER): INTEGER;
  GetModuleFileNameW:             PROCEDURE-(hModule, lpFilename, nSize: INTEGER): INTEGER;
  GetCurrentDirectoryW:           PROCEDURE-(nsize, pbuffer: INTEGER): INTEGER;

  (* Pre-loaded User32 imports *)
  MessageBoxA:        PROCEDURE-(hWnd, lpText, lpCaption, uType: INTEGER)(*: INTEGER*);
  MessageBoxW:        PROCEDURE-(hWnd, lpText, lpCaption, uType: INTEGER)(*: INTEGER*);

  (* Pre-loaded Shell32 imports *)
  CommandLineToArgvW: PROCEDURE-(lpCmdLine, pNumArgs: INTEGER): INTEGER;

  (* End of pre-loaded variables *)

  Stdout:    INTEGER;
  crlf*:     ARRAY 3 OF CHAR;
  Log:       PROCEDURE(s: ARRAY OF BYTE);
  OberonAdr: INTEGER;   (* Address of first module (Winshim.mod) *)
  LoadAdr:   INTEGER;   (* Where to load next module *)


PROCEDURE NoLog(s: ARRAY OF BYTE); BEGIN END NoLog;

PROCEDURE assert(expectation: BOOLEAN);
BEGIN
  IF ~expectation THEN Log("Assertion failure."); Log(crlf); ExitProcess(99) END
END assert;

(* ---------------------------------------------------------------------------- *)

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


PROCEDURE Strlen(s: ARRAY OF BYTE): INTEGER;
VAR i: INTEGER;
BEGIN i := 0;  WHILE (i < LEN(s)) & (s[i] # 0) DO INC(i) END
RETURN i END Strlen;

(* ---------------------------------------------------------------------------- *)

PROCEDURE WriteStdout(s: ARRAY OF BYTE);
VAR written, result: INTEGER;
BEGIN
  result := WriteFile(Stdout, SYSTEM.ADR(s), Strlen(s), SYSTEM.ADR(written), 0);
END WriteStdout;

(* ---------------------------------------------------------------------------- *)

PROCEDURE ws*(s: ARRAY OF CHAR); BEGIN Log(s) END ws;

PROCEDURE wc*(c: CHAR); BEGIN Log(c) END wc;

PROCEDURE wl*; BEGIN Log(crlf) END wl;

PROCEDURE wsl*(s: ARRAY OF CHAR); BEGIN Log(s);  Log(crlf) END wsl;

PROCEDURE wh*(n: INTEGER);
VAR hex: ARRAY 32 OF CHAR;
BEGIN IntToHex(n, hex);  Log(hex) END wh;

(* ---------------------------------------------------------------------------- *)

PROCEDURE WriteModuleHeader(adr: INTEGER);
VAR hdr: CodeHeaderPtr;  ch: CHAR;
BEGIN
  hdr := SYSTEM.VAL(CodeHeaderPtr, adr);
  INC(adr, SYSTEM.SIZE(CodeHeader));
  SYSTEM.GET(adr, ch);
  WHILE ch # 0X DO wc(ch);  INC(adr);  SYSTEM.GET(adr, ch) END;
  ws(" header at ");  wh(SYSTEM.VAL(INTEGER, hdr));  wsl("H:");
  ws("  length:   ");  wh(hdr.length);    wsl("H.");
  ws("  initcode: ");  wh(hdr.initcode);  wsl("H.");
  ws("  pointers: ");  wh(hdr.pointers);  wsl("H.");
  ws("  commands: ");  wh(hdr.commands);  wsl("H.");
  ws("  exports:  ");  wh(hdr.exports);   wsl("H.");
  ws("  imports:  ");  wh(hdr.imports);   wsl("H.");
  ws("  varsize:  ");  wh(hdr.varsize);   wsl("H.");
  ws("  key:      ");  wh(hdr.key);       wsl("H.");
END WriteModuleHeader;

PROCEDURE WriteExports(modadr: INTEGER);
VAR hdr: CodeHeaderPtr; adr: INTEGER; export: SYSTEM.CARD32;
BEGIN
  ws("Export table at "); wh(modadr); wsl("H.");
  adr := modadr;
  SYSTEM.GET(adr, export);  INC(adr, 4);
  WHILE export # 0FFFFFFFFH DO
    ws("  export "); wh(export); wsl("H.");
    SYSTEM.GET(adr, export);  INC(adr, 4);
  END
END WriteExports;

(* ---------------------------------------------------------------------------- *)

PROCEDURE GetString(adr: INTEGER; VAR s: ARRAY OF CHAR; VAR len: INTEGER);
VAR i: INTEGER;
BEGIN i := 0;
  (*ws("GetString -> '");*)
  REPEAT SYSTEM.GET(adr, s[i]); INC(adr); INC(i) UNTIL s[i-1] = 0X;
  len := i;
  (*ws(s); wsl("'.")*)
END GetString;

(* ---------------------------------------------------------------------------- *)

PROCEDURE ExportedAddress(modhdr: CodeHeaderPtr; index: INTEGER): INTEGER;
VAR exportoffset: SYSTEM.CARD32;
BEGIN
  SYSTEM.GET(SYSTEM.VAL(INTEGER, modhdr) + modhdr.exports + index * 4, exportoffset);
RETURN SYSTEM.VAL(INTEGER, modhdr) + exportoffset END ExportedAddress;

PROCEDURE FindModule(name: ARRAY OF CHAR; key: INTEGER): INTEGER;
VAR
  hdr:     CodeHeaderPtr;
  modadr:  INTEGER;
  modname: ARRAY 32 OF CHAR;
  len:     INTEGER;
BEGIN
  (*ws("Findmodule "); ws(name); wsl(".");*)
  modadr := OberonAdr;
  hdr := SYSTEM.VAL(CodeHeaderPtr, modadr);
  GetString(modadr + SYSTEM.SIZE(CodeHeader), modname, len);
  WHILE (hdr.length # 0) & (modname # name) DO
    modadr := (modadr + hdr.imports + hdr.varsize + 15) DIV 16 * 16;
    hdr := SYSTEM.VAL(CodeHeaderPtr, modadr);
    IF hdr.length > 0 THEN
      GetString(modadr + SYSTEM.SIZE(CodeHeader), modname, len)
    END
  END;
  assert(hdr.length # 0);
  (*ws("Requested key "); wh(hdr.key); ws("H, found key "); wh(hdr.key); wsl("H.");*)
RETURN modadr END FindModule;


PROCEDURE LoadModule(modadr: INTEGER);  (* Load module whose code image is at modadr *)
VAR
  adr:         INTEGER;
  loadedsize:  INTEGER;
  hdr:         CodeHeaderPtr;
  impmod:      ARRAY 32 OF CHAR;
  modules:     ARRAY 64 OF INTEGER; (* Import from up to 64 modules *)
  i, len:      INTEGER;
  key:         INTEGER;
  importcount: SYSTEM.CARD32;
  offset:      SYSTEM.CARD32;
  disp:        SYSTEM.INT32;
  modno:       SYSTEM.CARD16;
  impno:       SYSTEM.CARD16;
  impmodadr:   INTEGER;  (* Module being imported from base address *)
  expadr:      INTEGER;  (* Address relative to imported module of an export *)
  modulebody:  PROCEDURE;
BEGIN
  (*
  ws("Loading module image from address "); wh(modadr); wsl("H.");
  WriteModuleHeader(modadr);
  ws("Loading to "); wh(LoadAdr); wsl("H.");
  *)

  hdr := SYSTEM.VAL(CodeHeaderPtr, modadr);
  SYSTEM.COPY(modadr, LoadAdr, hdr.imports);  (* Copy up to but excluding import table *)
  loadedsize := hdr.imports + hdr.varsize;

  (*ws("Loaded size "); wh(loadedsize); wsl("H.");*)

  (* Build list of imported module header addresses *)
  adr := modadr + hdr.imports;
  GetString(adr, impmod, len);  INC(adr, len);  (* Ignore key *)
  SYSTEM.GET(adr, key);  INC(adr, 8);
  i := 0;
  WHILE impmod[0] # 0X DO
    modules[i] := FindModule(impmod, key);
    INC(i);
    GetString(adr, impmod, len);  INC(adr, len);
    SYSTEM.GET(adr, key);  INC(adr, 8);
  END;
  modules[i] := 0;

  adr := (adr + 15) DIV 16 * 16;
  SYSTEM.GET(adr, importcount);  INC(adr, 4);
  i := 0;
  WHILE i < importcount DO
    SYSTEM.GET(adr, offset); INC(adr, 4);
    SYSTEM.GET(adr, impno);  INC(adr, 2);
    SYSTEM.GET(adr, modno);  INC(adr, 2);
    (*
    ws("Import from module "); wh(modno);
    ws("H, impno "); wh(impno);
    ws("H, to offset "); wh(offset); wsl("H.");
    *)
    assert(modno > 0);  (* TODO modno 0 is boot fn *)
    impmodadr := modules[modno-1];
    expadr := ExportedAddress(SYSTEM.VAL(CodeHeaderPtr, impmodadr), impno-1);
    (*
    ws("expadr  "); wh(expadr); wsl("H.");
    ws("LoadAdr "); wh(LoadAdr); wsl("H.");
    ws("offset  "); wh(offset); wsl("H.");
    *)
    SYSTEM.GET(LoadAdr + offset, disp);
    (*ws("disp    -"); wh(-disp); wsl("H.");*)
    disp := expadr + disp - LoadAdr;
    (*ws("disp'   -"); wh(-disp); wsl("H.");*)
    SYSTEM.PUT(LoadAdr + offset, disp);
    INC(i)
  END;

  (* Run loaded modules body *)
  SYSTEM.PUT(SYSTEM.ADR(modulebody), LoadAdr + hdr.initcode);
  modulebody;

  INC(LoadAdr, loadedsize);

  (*wsl("LoadModule complete.")*)
END LoadModule;

(* ---------------------------------------------------------------------------- *)

PROCEDURE IncPC(increment: INTEGER);  (* Update return address by increment *)
VAR pc: INTEGER;
BEGIN
  SYSTEM.GET(SYSTEM.ADR(pc) + 8, pc);
  SYSTEM.PUT(SYSTEM.ADR(pc) + 8, pc + increment);
END IncPC;

PROCEDURE GetPC(): INTEGER;
VAR pc: INTEGER;
BEGIN SYSTEM.GET(SYSTEM.ADR(pc) + 8, pc);
RETURN pc END GetPC;

PROCEDURE PrepareOberonMachine;
CONST
  MEMRESERVE           = 2000H;
  MEMCOMMIT            = 1000H;
  PAGEEXECUTEREADWRITE = 40H;
VAR
  reserveadr:   INTEGER;
  moduleadr:    INTEGER;
  modulesize:   INTEGER;        (* loaded length including global vars *)
  modulelength: SYSTEM.CARD32;  (* image length from code file         *)
  bootsize:     INTEGER;
  res:          INTEGER;
  hdr:          CodeHeaderPtr;
BEGIN
  (* Reserve 2GB memory for the Oberon machine *)
  reserveadr := VirtualAlloc(0, 80000000H, MEMRESERVE, PAGEEXECUTEREADWRITE);
  (*ws("Reserved 2GB mem at ");  wh(reserveadr);  wsl("H.");*)

  (* Determine loaded size of all modules *)
  bootsize   := Header.imports + Header.varsize;
  (*ws("bootsize "); wh(bootsize); wsl("H.");*)
  modulesize := bootsize;
  (*ws("modulesize "); wh(modulesize); wsl("H.");*)
  moduleadr  := (SYSTEM.VAL(INTEGER, Header) + bootsize + 15) DIV 16 * 16;  (* Address of first module for Oberon machine *)
  (*
  ws("Looking for modules to load starting at "); wh(moduleadr); wsl("H.");
  WriteModuleHeader(moduleadr);
  *)
  hdr        := SYSTEM.VAL(CodeHeaderPtr, moduleadr);
  (*ws("Potential first module length "); wh(hdr.length); wsl("H.");*)
  WHILE hdr.length > 0 DO
    (*
    ws("Module at "); wh(moduleadr); ws("H, length "); wh(hdr.length); wsl("H.");
    WriteModuleHeader(moduleadr);
    *)
    INC(modulesize, hdr.imports + hdr.varsize);
    INC(moduleadr, hdr.length);
    hdr := SYSTEM.VAL(CodeHeaderPtr, moduleadr);
  END;

  (* Commit enough for the modules being loaded. *)
  OberonAdr := VirtualAlloc(reserveadr, modulesize, MEMCOMMIT, PAGEEXECUTEREADWRITE);
  (*ws("Committed ");  wh(modulesize);  ws("H bytes at ");  wh(OberonAdr);  wsl("H.");*)
END PrepareOberonMachine;

PROCEDURE LoadRemainingModules;
VAR moduleadr: INTEGER;  modulelength: SYSTEM.CARD32;
BEGIN
  (* Load and link remaining code modules from EXE file image *)
  moduleadr := SYSTEM.VAL(INTEGER, Header) + Header.imports + Header.varsize;  (* Address of first module for Oberon machine *)
  moduleadr := (moduleadr + 15) DIV 16 * 16;
  (*
  ws("Load remaining modules starting from "); wh(moduleadr); wsl("H.");
  wsl("First remaining module header:");
  WriteModuleHeader(moduleadr);
  *)
  SYSTEM.GET(moduleadr, modulelength);
  WHILE modulelength # 0 DO
    LoadModule(moduleadr);
    (*ws("  module length "); wh(modulelength); wsl("H.");*)
    moduleadr := (moduleadr + modulelength + 15) DIV 16 * 16;
    SYSTEM.GET(moduleadr, modulelength);
  END;
  (*wsl("LoadRemainingModules complete.")*)
END LoadRemainingModules;

(* ---------------------------------------------------------------------------- *)

BEGIN
  Log := NoLog;

  (* Initialise console output *)
  Stdout := GetStdHandle(-11);  (* -11:   StdOutputHandle *)
  SetConsoleOutputCP(65001);    (* 65001: UTF8            *)
  crlf := $0D 0A 00$;

  Log := WriteStdout;

  wsl("Winshim starting.");
  (*
  ws("Stdout handle "); wh(Stdout);            wsl("H.");
  ws("NoLog at ");      wh(SYSTEM.ADR(NoLog)); wsl("H.");
  ws("Log at ");        wh(SYSTEM.ADR(Log));   wsl("H.");
  WriteModuleHeader(SYSTEM.VAL(INTEGER, Header));
  *)

  PrepareOberonMachine;

  (* Copy boot module into newly committed memory and switch PC to the new code. *)
  SYSTEM.COPY(SYSTEM.VAL(INTEGER, Header), OberonAdr, Header.imports + Header.varsize);
  (*ws("Transferring PC from original load at ");  wh(GetPC());  wsl("H.");*)
  IncPC(OberonAdr - SYSTEM.VAL(INTEGER, Header));  (* Transfer to copied code *)
  (*ws("Transferred PC to code copied to Oberon memory at ");  wh(GetPC());  wsl("H.");*)

  (*
  WriteExports(OberonAdr + Header.exports);
  ws("First code module at "); wh(SYSTEM.VAL(INTEGER, Header) + Header.imports + Header.varsize); wsl("H.");
  ws("Oberon loaded boot module ends at "); wh(OberonAdr + Header.imports + Header.varsize); wsl("H.");
  *)

  LoadAdr := (OberonAdr + Header.imports + Header.varsize + 15) DIV 16 * 16;

  (* ws("crlf at "); wh(SYSTEM.ADR(crlf)); wsl("H.");*)

  LoadRemainingModules;

  (*MessageBoxA(0, SYSTEM.ADR("Complete."), SYSTEM.ADR("Winshim"), 0);*)

  wsl("Winshim complete.");
  ExitProcess(0);
END Winshim.
