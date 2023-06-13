MODULE Kernel;  (*$RTL-*)

IMPORT SYSTEM, Boot, Heap;


CONST
  (* Windows constants *)
  StdOutputHandle = -11;
  UTF8              = 65001;

  (* Exception pointers record from Windows *)
  ExPtrExcDesc = 0;  (* ExceptionPointers record address of exception record *)
  ExPtrCtxDesc = 8;  (* ExceptionPointers record address of context record *)

  (* Exception description record from Windows *)
  ExcCode      = 0;
  ExcFlags     = 4;
  ExcNested    = 8;
  ExcAddress   = 16;
  ExcNumParams = 24;
  ExcParames   = 32;

TYPE
  ExceptionHandlerProc = PROCEDURE(p: INTEGER): INTEGER;
  LogWriter        = PROCEDURE(adr, len: INTEGER);

  (* Windows run time failure / exception handling *)
  (*
  Exception = POINTER [untraced] TO ExceptionDesc;
  ExceptionDesc = RECORD
    (* 0*) code:      SYSTEM.CARD32;
    (* 4*) flags:     SYSTEM.CARD32;
    (* 8*) nested:    Exception;
    (*16*) address:   INTEGER;
    (*24*) NumParams: SYSTEM.CARD32;
    (*32*) Params:    ARRAY 15 OF INTEGER
  END;

  Context     = POINTER [untraced] TO ContextDesc;
  ContextDesc = RECORD END;

  ExceptionPointers = POINTER [untraced] TO ExceptionPointersDesc;
  ExceptionPointersDesc = RECORD
    exception: Exception;
    context:   Context
  END;
  *)


VAR
  Kernel*: INTEGER;
  Gdi*:    INTEGER;
  User*:   INTEGER;
  Shell*:  INTEGER;
  ShCore*: INTEGER;

  MessageBoxW:                    PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER): INTEGER;
  AddVectoredExceptionHandler:    PROCEDURE(first: INTEGER; filter: ExceptionHandlerProc);
  GetSystemTimePreciseAsFileTime: PROCEDURE(tickAdr: INTEGER);
  GetModuleFileNameW:             PROCEDURE(hModule, lpFilename, nSize: INTEGER);
  GetCurrentDirectoryW:           PROCEDURE(nsize, pbuffer: INTEGER): INTEGER;
  GetStdHandle:                   PROCEDURE(nStdHandle: SYSTEM.CARD32): INTEGER;
  SetConsoleOutputCP:             PROCEDURE(codepage: INTEGER) (* : INTEGER *);

  WriteFile: PROCEDURE(hFile, lpBuffer, nNumberOfBytesToWrite,
                       lpNumberOfBytesWritten, lpOverlapped: INTEGER
                      ): SYSTEM.CARD32;

  VirtualAlloc: Heap.WindowsAllocator;

  (* Windows command line *)
  GetCommandLineW:    PROCEDURE(): INTEGER;
  CommandLineToArgvW: PROCEDURE(lpCmdLine, pNumArgs: INTEGER): INTEGER;
  ArgV:               INTEGER;
  NumArgs*:           INTEGER;
  CommandAdr:         INTEGER;
  ExecutablePath*:    ARRAY 1024 OF CHAR;
  InitialDirectory*:  ARRAY 1024 OF CHAR;
  StdOut*:            INTEGER;  (* Standard output file handle *)

  WriteLog*:          LogWriter;

  (* hwnd for messagebox owning window, if any. Set by SetHWnd. *)
  HWnd: INTEGER;



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


(* -------------------------------------------------------------------------- *)
(* ----------------------- Simple Integer formatting ------------------------ *)
(* -------------------------------------------------------------------------- *)

PROCEDURE IntToDecimal*(n: INTEGER; VAR s: ARRAY OF CHAR);
VAR i, j: INTEGER;  ch: CHAR;
BEGIN
  IF n = 8000000000000000H THEN s := "-9223372036854775808"
  ELSE i := 0;
    IF n < 0 THEN s[0] := "-";  i := 1;  n := -n END;
    j := i;
    REPEAT s[j] := CHR(n MOD 10 + 48);  INC(j);  n := n DIV 10 UNTIL n = 0;
    s[j] := 0X;  DEC(j);
    WHILE i < j DO ch:=s[i]; s[i]:=s[j]; s[j]:=ch; INC(i); DEC(j) END
  END
END IntToDecimal;

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
  WHILE i < j DO ch:=s[i]; s[i]:=s[j]; s[j]:=ch; INC(i); DEC(j) END
END IntToHex;


(* -------------------------------------------------------------------------- *)
(* ---------------------- Very basic string functions ----------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Length*(s: ARRAY OF CHAR): INTEGER;
VAR l: INTEGER;
BEGIN  l := 0;  WHILE (l < LEN(s)) & (s[l] # 0X) DO INC(l) END
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


(* -------------------------------------------------------------------------- *)
(* ---------------------- Primitive console/log output ---------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE WriteConsoleBytes(adr, len: INTEGER);
VAR written, result: INTEGER;
BEGIN result := WriteFile(StdOut, adr, len, SYSTEM.ADR(written), 0)
END WriteConsoleBytes;


(* -------------------------------------------------------------------------- *)
(* --------------------------- Exception handling --------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE WriteException(code: INTEGER; VAR s: ARRAY OF CHAR);
VAR number:  ARRAY 25 OF CHAR;
BEGIN
  IF    code = 0C0000005H THEN s := "Access violation"
  ELSIF code = 0C0000006H THEN s := "In-page error"
  ELSIF code = 0C000001DH THEN s := "Illegal instruction"
  ELSIF code = 0C000008EH THEN s := "Divide by zero"
  ELSIF code = 0C0000094H THEN s := "Integer divide by zero"
                          ELSE s := "Exception. Code: $";
                               IntToHex(code, number);  Append(number, s)
  END
END WriteException;

PROCEDURE AppendMemString(adr: INTEGER;  VAR str: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN
  i := 0;  WHILE str[i] # 0X DO INC(i) END;
  SYSTEM.GET(adr, str[i]);
  WHILE str[i] # 0X DO
    INC(adr);  INC(i);  SYSTEM.GET(adr, str[i]);
  END
END AppendMemString;


PROCEDURE GetCard32(adr: INTEGER): INTEGER;
VAR result: SYSTEM.CARD32;
BEGIN SYSTEM.GET(adr, result);
RETURN result END GetCard32;


PROCEDURE ExceptionHandler(p: INTEGER): INTEGER;
(* parameter is address of 64/exception desc adr, 64/context desc adr *)
VAR
  exdesc:   INTEGER;  (* Address of exception description record *)
  modhdr:   INTEGER;
  code:     SYSTEM.CARD32;
  next:     INTEGER;
  trapadr:  INTEGER;
  address:  INTEGER;
  detail:   INTEGER;
  trap:     INTEGER;
  line:     INTEGER;
  prevline: INTEGER;
  col:      INTEGER;
  adr:      INTEGER;
  report:   ARRAY 256 OF CHAR;
  number:   ARRAY 25 OF CHAR;
BEGIN
  SYSTEM.GET(p + ExPtrExcDesc,    exdesc);
  SYSTEM.GET(exdesc + ExcAddress, address);
  next := Boot.BootHeader;
  REPEAT
    modhdr := next;
    SYSTEM.GET(modhdr + Boot.OffModCode, code);
    SYSTEM.GET(modhdr + Boot.OffModTrap, trapadr);
    SYSTEM.GET(modhdr + Boot.OffModNext, next);
  UNTIL (next = 0) OR (address >= code) & (address < trapadr);
  IF (address < code) OR (address >= trapadr) THEN
    WriteException(GetCard32(exdesc + ExcCode), report);
    Append(" at address $", report);
    IntToHex(address, number);  Append(number, report);
    Append(" (not in loaded module code).", report);
  ELSE
    DEC(address, code);  (* Make address relative to modules code *)
    SYSTEM.GET(trapadr, detail);
    adr := 0;  prevline := 0;
    WHILE (detail # -1) & (address >= adr) DO
      prevline := line;
      trap     := ASR(detail, 60) MOD 10H;
      line     := ASR(detail, 40) MOD 100000H;
      col      := ASR(detail, 30) MOD 400H;
      adr      := detail MOD 40000000H;
      IF adr = address THEN
        detail := -1  (* End loop *)
      ELSE
        INC(trapadr, 8);  SYSTEM.GET(trapadr, detail);
      END
    END;
    IF (adr = address) & (trap <= 10) THEN
      IF    trap =  1 THEN report := "Array index out of range"
      ELSIF trap =  2 THEN report := "Type trap"
      ELSIF trap =  3 THEN report := "String size error"
      ELSIF trap =  4 THEN report := "NIL reference"
      ELSIF trap =  5 THEN report := "NIL procedure call"
      ELSIF trap =  6 THEN report := "Divide by zero"
      ELSIF trap =  7 THEN report := "Assertion FALSE"
      ELSIF trap =  9 THEN report := "SYSTEM.GET access violation"
      ELSIF trap = 10 THEN report := "SYSTEM.PUT access violation"
                      ELSE report := "Undefined trap"
      END;
      Append(" in module ", report);
      AppendMemString(modhdr + Boot.OffModName, report);
      Append(" at ", report);
      IntToDecimal(line, number); Append(number, report);
      Append(":", report);
      IntToDecimal(col,  number); Append(number, report);
    ELSE
      WriteException(GetCard32(exdesc + ExcCode), report);
      Append(" in module ", report);
      AppendMemString(modhdr + Boot.OffModName, report);
      Append(" at code offset $", report);
      IntToHex(address, number);  Append(number, report);
      Append(" between lines  ", report);
      IntToDecimal(prevline, number); Append(number, report);
      Append(" and ", report);
      IntToDecimal(line, number); Append(number, report);
      Append(".", report);
    END
  END;

  MessageBox("Exception", report);
  Boot.ExitProcess(99)  (* Immediate exit - bypass GC finalisation *)
RETURN 0 END ExceptionHandler;


(* -------------------------------------------------------------------------- *)
(* -------------------------- Windows command line -------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE GetArg*(n: INTEGER; VAR str: ARRAY OF CHAR);
VAR i, argAdr: INTEGER;  str16: ARRAY 1024 OF SYSTEM.CARD16;
BEGIN
  IF (n < 0) OR (n >= NumArgs) THEN
    str := ""
  ELSE
    SYSTEM.GET(ArgV + n * 8, argAdr);
    i := 0;  SYSTEM.GET(argAdr, str16[i]);
    WHILE str16[i] # 0 DO
      INC(argAdr, 2);  INC(i);  SYSTEM.GET(argAdr, str16[i])
    END;
    i := Utf16ToUtf8(str16, str)
  END;
END GetArg;


(* -------------------------------------------------------------------------- *)
(* ---------------- Time since since January 1, 1601 (UTC) ------------------ *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Ticks*(): INTEGER;  (* In 100ns ticks *)
VAR tick: INTEGER;
BEGIN GetSystemTimePreciseAsFileTime(SYSTEM.ADR(tick));
RETURN tick END Ticks;

PROCEDURE Time*(): INTEGER;   (* In ms *)
RETURN Ticks() DIV 10000 END Time;


(* -------------------------------------------------------------------------- *)
(* ------------------------ Halt - finalise and exit ------------------------ *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Halt*(exitCode: INTEGER);
BEGIN Heap.Finalise;  Boot.ExitProcess(exitCode) END Halt;


(* -------------------------------------------------------------------------- *)
(* ------- Kernel initialisation code - called following kernel link -------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE GetProc*(dll: INTEGER; name: ARRAY OF CHAR; VAR proc: ARRAY OF BYTE);
BEGIN
  SYSTEM.PUT(SYSTEM.ADR(proc), Boot.GetProcAddress(dll, SYSTEM.ADR(name)))
END GetProc;

PROCEDURE GetWindowsPaths;
VAR path: ARRAY 1024 OF SYSTEM.CARD16;  n: INTEGER;
BEGIN
  GetModuleFileNameW(0, SYSTEM.ADR(path), LEN(path));
  n := Utf16ToUtf8(path, ExecutablePath);

  InitialDirectory := "";
  IF GetCurrentDirectoryW(LEN(path), SYSTEM.ADR(path)) > 0 THEN
    n := Utf16ToUtf8(path, InitialDirectory)
  END;
END GetWindowsPaths;

PROCEDURE SetHWnd*(h: INTEGER);
BEGIN HWnd := h END SetHWnd;


BEGIN
  HWnd := 0;

  (* Set up some useful exports from standard procedures. *)
  Kernel := Boot.LoadLibraryA(SYSTEM.ADR("kernel32.dll"));
  Gdi    := Boot.LoadLibraryA(SYSTEM.ADR("gdi32.dll"));
  User   := Boot.LoadLibraryA(SYSTEM.ADR("user32.dll"));
  Shell  := Boot.LoadLibraryA(SYSTEM.ADR("shell32.dll"));
  ShCore := Boot.LoadLibraryA(SYSTEM.ADR("shCore.dll"));

  GetProc(User,   "MessageBoxW",                    MessageBoxW);                     ASSERT(MessageBoxW                    # NIL);
  GetProc(Kernel, "AddVectoredExceptionHandler",    AddVectoredExceptionHandler);     ASSERT(AddVectoredExceptionHandler    # NIL);
  GetProc(Kernel, "VirtualAlloc",                   VirtualAlloc);                    ASSERT(VirtualAlloc                   # NIL);
  GetProc(Kernel, "GetCommandLineW",                GetCommandLineW);                 ASSERT(GetCommandLineW                # NIL);
  GetProc(Shell,  "CommandLineToArgvW",             CommandLineToArgvW);              ASSERT(CommandLineToArgvW             # NIL);
  GetProc(Kernel, "GetSystemTimePreciseAsFileTime", GetSystemTimePreciseAsFileTime);  ASSERT(GetSystemTimePreciseAsFileTime # NIL);
  GetProc(Kernel, "GetModuleFileNameW",             GetModuleFileNameW);              ASSERT(GetModuleFileNameW             # NIL);
  GetProc(Kernel, "GetCurrentDirectoryW",           GetCurrentDirectoryW);            ASSERT(GetCurrentDirectoryW           # NIL);
  GetProc(Kernel, "WriteFile",                      WriteFile);                       ASSERT(WriteFile                      # NIL);
  GetProc(Kernel, "GetStdHandle",                   GetStdHandle);                    ASSERT(GetStdHandle                   # NIL);
  GetProc(Kernel, "SetConsoleOutputCP",             SetConsoleOutputCP);              ASSERT(SetConsoleOutputCP             # NIL);

  (* Initialise exception/trap handling *)
  AddVectoredExceptionHandler(1, ExceptionHandler);

  (* Initialise Heap and GC *)
  Heap.InitHeap(VirtualAlloc);
  SYSTEM.PUT(SYSTEM.ADR(Boot.New), Heap.New);

  (* Initialise command line access *)
  CommandAdr := GetCommandLineW();
  NumArgs    := 0;
  ArgV       := CommandLineToArgvW(CommandAdr, SYSTEM.ADR(NumArgs));
  GetWindowsPaths;

  (* Initialise console output *)
  StdOut := GetStdHandle(StdOutputHandle);
  SetConsoleOutputCP(UTF8);
  WriteLog := WriteConsoleBytes
END Kernel.
