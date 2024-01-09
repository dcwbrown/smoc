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
  LoadLibraryA*:            PROCEDURE-(libname: INTEGER): INTEGER;
  GetProcAddress*:          PROCEDURE-(hmodule, procname: INTEGER): INTEGER;
  VirtualAlloc*:            PROCEDURE-(address, size, type, protection: INTEGER): INTEGER;
  ExitProcess*:             PROCEDURE-(exitcode: INTEGER);
  GetStdHandle*:            PROCEDURE-(nStdHandle: SYSTEM.INT32): INTEGER;
  SetConsoleOutputCP*:      PROCEDURE-(codepage: INTEGER) (* : INTEGER *);
  GetCommandLineW*:         PROCEDURE-(): INTEGER;
  GetModuleFileNameW*:      PROCEDURE-(hModule, lpFilename, nSize: INTEGER): INTEGER;
  GetCurrentDirectoryW*:    PROCEDURE-(nsize, pbuffer: INTEGER): INTEGER;
  GetFileAttributesW*:      PROCEDURE-(lpFileName: INTEGER): INTEGER;
  DeleteFileW*:             PROCEDURE-(lpFilename: INTEGER): INTEGER;
  CloseHandle*:             PROCEDURE-(hObject: INTEGER): INTEGER;
  FlushFileBuffers*:        PROCEDURE-(hFile: INTEGER): INTEGER;
  SetEndOfFile*:            PROCEDURE-(hFile: INTEGER): INTEGER;
  GetFileSizeEx*:           PROCEDURE-(hFile, lpFileSize: INTEGER): INTEGER;
  GetCurrentProcessId*:     PROCEDURE-(): INTEGER;
  MoveFileExW*:             PROCEDURE-(lpExistingFileName, lpNewFileName, dwFlags: INTEGER): INTEGER;
  CreateFileW*:             PROCEDURE-(lpFileName, dwDesiredAccess, dwShareMode,
                                       lpSecurityAttributes, dwCreationDisposition,
                                       dwFlagsAndAttributes, hTemplateFile: INTEGER): INTEGER;
  ReadFile*:                PROCEDURE-(hFile, lpBuffer, nNumberOfBytesToRead,
                                       lpNumberOfBytesRead, lpOverlapped: INTEGER): INTEGER;
  WriteFile*:               PROCEDURE-(hFile, lpBuffer, nNumberOfBytesToWrite,
                                       lpNumberOfBytesWritten, lpOverlapped: INTEGER): INTEGER;
  SetFilePointerEx*:        PROCEDURE-(hFile, liDistanceToMove,
                                       lpNewFilePointer, dwMoveMethod: INTEGER): INTEGER;
  GetEnvironmentVariableW*: PROCEDURE-(lpName, lpBuffer, nSize: INTEGER): INTEGER;
  GetFileAttributesExW*:    PROCEDURE-(lpName, fInfoLevelId, lpFileInformation: INTEGER): INTEGER;
                            (* fInfoLevelId Must be 0 (GetFileExInfoStandard) *)
  GetLastError*:            PROCEDURE-(): INTEGER;

  AddVectoredExceptionHandler:    PROCEDURE-(first, filter: INTEGER);
  GetSystemTimePreciseAsFileTime: PROCEDURE-(tickAdr: INTEGER): INTEGER;

  (* Pre-loaded User32 imports *)
  MessageBoxA:        PROCEDURE-(hWnd, lpText, lpCaption, uType: INTEGER)(*: INTEGER*);
  MessageBoxW:        PROCEDURE-(hWnd, lpText, lpCaption, uType: INTEGER): INTEGER;

  (* Pre-loaded Shell32 imports *)
  CommandLineToArgvW: PROCEDURE-(lpCmdLine, pNumArgs: INTEGER): INTEGER;

  (* End of pre-loaded variables *)

  Stdout:    INTEGER;
  crlf*:     ARRAY 3 OF CHAR;
  Log:       PROCEDURE(s: ARRAY OF BYTE);
  OberonAdr: INTEGER;   (* Address of first module (Winshim.mod) *)
  LoadAdr:   INTEGER;   (* Where to load next module *)
  HWnd:      INTEGER;   (* Set if a window has been created *)


PROCEDURE NoLog(s: ARRAY OF BYTE); BEGIN END NoLog;

PROCEDURE assert(expectation: BOOLEAN);
BEGIN
  IF ~expectation THEN Log("Assertion failure."); Log(crlf); ExitProcess(99) END
END assert;

(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* ------------ Unicode Transformation Formats UTF-8 and UTF-16 ------------- *)
(* -------------------------------------------------------------------------- *)

(* UTF-8:                                                                                           *)
(* -------------- codepoint --------------    ----------------------- bytes ----------------------- *)
(* 0000 0000 0000 0000 0000 0000 0zzz zzzz    0zzzzzzz                                              *)
(* 0000 0000 0000 0000 0000 0yyy yyzz zzzz    110yyyyy 10zzzzzz                                     *)
(* 0000 0000 0000 0000 xxxx yyyy yyzz zzzz    1110xxxx 10yyyyyy 10zzzzzz                            *)
(* 0000 0000 000w wwxx xxxx yyyy yyzz zzzz    11110www 10xxxxxx 10yyyyyy 10zzzzzz                   *)
(* The below are beyond the range of valid Unicode codepoints                                       *)
(* 0000 00vv wwww wwxx xxxx yyyy yyzz zzzz    111110vv 10wwwwww 10xxxxxx 10yyyyyy 10zzzzzz          *)
(* 0uvv vvvv wwww wwxx xxxx yyyy yyzz zzzz    1111110u 10vvvvvv 10wwwwww 10xxxxxx 10yyyyyy 10zzzzzz *)

PROCEDURE GetUtf8*(src: ARRAY OF CHAR; VAR i: INTEGER): INTEGER;
VAR n, result: INTEGER;
BEGIN ASSERT(i < LEN(src)); result := ORD(src[i]);  INC(i);
  IF result >= 0C0H THEN
    IF    result >= 0FCH THEN result := result MOD 2;  n := 5
    ELSIF result >= 0F8H THEN result := result MOD 4;  n := 4
    ELSIF result >= 0F0H THEN result := result MOD 8;  n := 3
    ELSIF result >= 0E0H THEN result := result MOD 16; n := 2
    ELSE                      result := result MOD 32; n := 1
    END;
    WHILE n > 0 DO
      result := LSL(result,6);  DEC(n);
      IF (i < LEN(src)) & (ORD(src[i]) DIV 40H = 2) THEN
        INC(result, ORD(src[i]) MOD 40H);  INC(i)
      END
    END
  END;
RETURN result END GetUtf8;

PROCEDURE PutUtf8*(c: INTEGER; VAR dst: ARRAY OF CHAR; VAR i: INTEGER);
VAR n: INTEGER;
BEGIN
  ASSERT(i < LEN(dst));
  ASSERT(c > 0);  ASSERT(c < 80000000H);
  IF i < LEN(dst) THEN
    IF c < 80H THEN dst[i] := CHR(c);  INC(i)
    ELSE
      IF    c < 800H     THEN  dst[i] := CHR(0C0H + ASR(c, 6));    n := 1;
      ELSIF c < 10000H   THEN  dst[i] := CHR(0E0H + ASR(c, 12));   n := 2;
      ELSIF c < 200000H  THEN  dst[i] := CHR(0F0H + ASR(c, 18));   n := 3;
      ELSIF c < 4000000H THEN  dst[i] := CHR(0F8H + ASR(c, 24));   n := 4;
      ELSE                     dst[i] := CHR(0FCH + ASR(c, 30));   n := 5;
      END;
      INC(i);
      WHILE (n > 0) & (i < LEN(dst)) DO
        DEC(n);  dst[i] := CHR(80H + ASR(c, n*6) MOD 40H);  INC(i)
      END;
    END
  END
END PutUtf8;


(* UTF-16:                                                                      *)
(* -------------- codepoint --------------    ------------- words ------------- *)
(* 0000 0000 0000 0000 zzzz zzzz zzzz zzzz    zzzzzzzzzzzzzzzz                  *)
(* 0000 0000 000x xxxx yyyy yyzz zzzz zzzz    110110wwwwyyyyyy 110111zzzzzzzzzz *)
(* Where xxxxx is 1-16, and wwww is xxxxx-1 (0-15).                             *)

PROCEDURE GetUtf16*(src: ARRAY OF SYSTEM.CARD16; VAR i: INTEGER): INTEGER;
VAR result: INTEGER;
BEGIN
  ASSERT(i < LEN(src));
  result := src[i];  INC(i);
  IF result DIV 400H = 36H THEN    (* High surrogate *)
    result := LSL(result MOD 400H, 10) + 10000H;
    IF (i < LEN(src)) & (src[i] DIV 400H = 37H) THEN  (* Low surrogate *)
      INC(result, src[i] MOD 400H);  INC(i)
    END
  END
RETURN result END GetUtf16;

PROCEDURE PutUtf16*(ch: INTEGER; VAR dst: ARRAY OF SYSTEM.CARD16; VAR i: INTEGER);
BEGIN
  ASSERT(i < LEN(dst));
  IF (ch < 10000H) & (i < LEN(dst)) THEN
    dst[i] := ch;  INC(i)
  ELSIF i+1 < LEN(dst) THEN
    DEC(ch, 10000H);
    dst[i] := 0D800H + ch DIV 400H;  INC(i);
    dst[i] := 0DC00H + ch MOD 400H;  INC(i);
  END
END PutUtf16;


PROCEDURE Utf8ToUtf16*(src: ARRAY OF CHAR;  VAR dst: ARRAY OF SYSTEM.CARD16): INTEGER;
VAR i, j: INTEGER;
BEGIN  i := 0;  j := 0;
  WHILE (i < LEN(src)) & (src[i] # 0X) DO PutUtf16(GetUtf8(src, i), dst, j) END;
  IF j < LEN(dst) THEN dst[j] := 0;  INC(j) END
RETURN j END Utf8ToUtf16;

PROCEDURE Utf16ToUtf8*(src: ARRAY OF SYSTEM.CARD16;  VAR dst: ARRAY OF CHAR): INTEGER;
VAR i, j: INTEGER;
BEGIN  i := 0;  j := 0;
  WHILE (i < LEN(src)) & (src[i] # 0) DO PutUtf8(GetUtf16(src, i), dst, j) END;
  IF j < LEN(dst) THEN dst[j] := 0X;  INC(j) END
RETURN j END Utf16ToUtf8;


(* -------------------------------------------------------------------------- *)
(* ---------------- Last resort error reporting - MessageBox ---------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE MessageBox*(title, msg: ARRAY OF CHAR);
VAR
  res:     INTEGER;
  title16: ARRAY 256 OF SYSTEM.CARD16;
  msg16:   ARRAY 256 OF SYSTEM.CARD16;
BEGIN
  res := Utf8ToUtf16(title, title16);
  res := Utf8ToUtf16(msg,   msg16);
  res := MessageBoxW(HWnd, SYSTEM.ADR(msg16), SYSTEM.ADR(title16), 0)
END MessageBox;


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


(* -------------------------------------------------------------------------- *)
(* ---------------------- Very basic string functions ----------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Length*(s: ARRAY OF BYTE): INTEGER;
VAR l: INTEGER;
BEGIN  l := 0;  WHILE (l < LEN(s)) & (s[l] # 0) DO INC(l) END
RETURN l END Length;

PROCEDURE Append*(s: ARRAY OF CHAR; VAR d: ARRAY OF CHAR);
VAR i, j: INTEGER;
BEGIN
  j := Length(d);
  i := 0; WHILE (i < LEN(s)) & (j < LEN(d)) & (s[i] # 0X) DO
    d[j] := s[i];  INC(i);  INC(j)
  END;
  IF j >= LEN( d) THEN DEC(j) END;  d[j] := 0X
END Append;

(* ---------------------------------------------------------------------------- *)

PROCEDURE WriteStdout(s: ARRAY OF BYTE);
VAR written, result: INTEGER;
BEGIN
  result := WriteFile(Stdout, SYSTEM.ADR(s), Length(s), SYSTEM.ADR(written), 0);
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
  HWnd := 0;
  Log  := NoLog;

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
  (*MessageBox("Winshim", "Complete");*)
  ExitProcess(0);
END Winshim.
