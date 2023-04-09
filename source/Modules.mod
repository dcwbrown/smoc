MODULE Modules;  (*$CONSOLE*) (*$RTL-*)  (* Load and run .x64 modules *)
IMPORT SYSTEM;

TYPE
  Module = POINTER TO ModuleDesc;
  ModuleDesc = RECORD
    name:     ARRAY 64 OF CHAR;
    key:      ARRAY 2 OF INTEGER;
    refcount: INTEGER;
    start:    INTEGER;  (* Address of global VARs *)
    base:     INTEGER;  (* Address of start of staic data. *)
    code:     INTEGER;
    init:     PROCEDURE;
    traps:    INTEGER;
    exports:  INTEGER;
    exportNo: INTEGER;
    next:     Module
  END;


VAR
  TheByteBeforeBase: CHAR;  (* Used to address module base. MUST be the first variable *)
  ModulesBase:       INTEGER;

  Hx64: INTEGER;  (* Input file handle *)

  memadr, memlimit:  INTEGER;  (* Memory allocated for modules *)

  FirstModule:  Module;

  StandardPointers: RECORD  (* This data copied to the base of every module *)
  (*   0 *) GetProcAddress:    INTEGER;
  (*   8 *) LoadLibraryA:      INTEGER;
  (*  16 *) Halt:              PROCEDURE(exitcode: INTEGER);
  (*  24 *) dummy1:            INTEGER;
  (*  32 *) dummy2:            INTEGER;
  (*  40 *) dummy3:            INTEGER;
  (*  48 *) dummy4:            INTEGER;
  (*  56 *) dummy5:            INTEGER;
  (*  64 *) dummy6:            INTEGER;
  (*  72 *) dummy7:            INTEGER;
  (*  80 *) dummy8:            INTEGER;
  (*  88 *) dummy9:            INTEGER;
  (*  96 *) dummy10:           INTEGER;
  (* 104 *) adrOfStackPtrList: INTEGER;
  (* 112 *) adrOfPtrTable:     INTEGER;
  (* 120 *) New:               PROCEDURE(VAR ptr: INTEGER;  tdAdr: INTEGER)
  END;

  HKernel*, HUser*, HShell*:   INTEGER;
  HOut:                        INTEGER;  (* Console output file *)

  ExitProcess:             PROCEDURE(uExitCode: INTEGER);
  MessageBoxA:             PROCEDURE(hWnd, Text, Caption, Type: INTEGER): INTEGER;
  MessageBoxW:             PROCEDURE(hWnd, Text, Caption, Type: INTEGER): INTEGER;
  GetSystemTimeAsFileTime: PROCEDURE(lpSystemTimeAsFileTime: INTEGER);
  GetCommandLineW:         PROCEDURE(): INTEGER;
  CommandLineToArgvW:      PROCEDURE(lpCmdLine, pNumArgs: INTEGER): INTEGER;
  VirtualAlloc:            PROCEDURE(lpAddress, dwSize, flAllocationType, flProtect: INTEGER): INTEGER;
  GetStdHandle:            PROCEDURE(nStdHandle: SYSTEM.CARD32): INTEGER;
  SetConsoleOutputCP:      PROCEDURE(codepage:   INTEGER):       INTEGER;
  GetFileSizeEx:           PROCEDURE(hFile: INTEGER; lpFileSize: INTEGER): INTEGER;
  CloseHandle:             PROCEDURE(hObject:    INTEGER): INTEGER;
  CreateFileW:             PROCEDURE(lpFileName:            ARRAY [untagged] OF SYSTEM.CARD16;
                                     dwDesiredAccess,
                                     dwShareMode:           INTEGER;
                                     lpSecurityAttributes:  INTEGER;
                                     dwCreationDisposition,
                                     dwFlagsAndAttributes:  INTEGER;
                                     hTemplateFile:         INTEGER): INTEGER;
  ReadFile:                PROCEDURE(hFile:                 INTEGER;
                                     lpBuffer:              INTEGER;
                                     nNumberOfBytesToRead:  INTEGER;
                                     lpNumberOfBytesRead,
                                     lpOverlapped:          INTEGER): INTEGER;
  WriteFile:               PROCEDURE(hFile:                 INTEGER;
                                     lpBuffer:              INTEGER;
                                     nNumberOfBytesToWrite: INTEGER;
                                     lpNumberOfBytesWrite,
                                     lpOverlapped:          INTEGER): INTEGER;
  SetFilePointerEx:        PROCEDURE(hFile:                 INTEGER;
                                     liDistanceToMove:      INTEGER;
                                     lpNewFilePointer:      INTEGER;
                                     dwMoveMethod:          INTEGER): INTEGER;

  AddVectoredExceptionHandler: PROCEDURE(FirstHandler: INTEGER; VectoredHandler: INTEGER);

  CommandLineWAdr: INTEGER;
  ArgV, NumArgs:   INTEGER;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Align*(VAR a: INTEGER;  align: INTEGER);
BEGIN
  IF    a > 0 THEN a := (a + align - 1) DIV align * align
  ELSIF a < 0 THEN a :=        a        DIV align * align
  END
END Align;


(* -------------------------------------------------------------------------- *)

PROCEDURE Halt*(exitCode: INTEGER);
BEGIN
(*nMod := 0;  Collect;*)
  ExitProcess(exitCode)
END Halt;


(* -------------------------------------------------------------------------- *)

(* UTF8:                                                                                            *)
(* -------------- codepoint --------------    ----------------------- bytes ----------------------- *)
(* 0000 0000 0000 0000 0000 0000 0zzz zzzz    0zzzzzzz                                              *)
(* 0000 0000 0000 0000 0000 0yyy yyzz zzzz    110yyyyy 10zzzzzz                                     *)
(* 0000 0000 0000 0000 xxxx yyyy yyzz zzzz    1110xxxx 10yyyyyy 10zzzzzz                            *)
(* 0000 0000 000w wwxx xxxx yyyy yyzz zzzz    11110www 10xxxxxx 10yyyyyy 10zzzzzz                   *)
(* 0000 00vv wwww wwxx xxxx yyyy yyzz zzzz    111110vv 10wwwwww 10xxxxxx 10yyyyyy 10zzzzzz          *)
(* 0uvv vvvv wwww wwxx xxxx yyyy yyzz zzzz    1111110u 10vvvvvv 10wwwwww 10xxxxxx 10yyyyyy 10zzzzzz *)

PROCEDURE GetUtf8*(src: ARRAY OF BYTE; VAR i: INTEGER): INTEGER;
VAR n, result: INTEGER;
BEGIN ASSERT(i < LEN(src)); result := src[i];  INC(i);
  IF result >= 0C0H THEN
    IF    result >= 0FCH THEN result := result MOD 2;  n := 5
    ELSIF result >= 0F8H THEN result := result MOD 4;  n := 4
    ELSIF result >= 0F0H THEN result := result MOD 8;  n := 3
    ELSIF result >= 0E0H THEN result := result MOD 16; n := 2
    ELSE                      result := result MOD 32; n := 1
    END;
    WHILE n > 0 DO
      result := LSL(result,6);  DEC(n);
      IF (i < LEN(src)) & (src[i] DIV 40H = 2) THEN
        INC(result, src[i] MOD 40H);  INC(i)
      END
    END
  END;
RETURN result END GetUtf8;

PROCEDURE PutUtf8*(c: INTEGER; VAR dst: ARRAY OF BYTE; VAR i: INTEGER);
VAR n: INTEGER;
BEGIN
  ASSERT(i < LEN(dst));
  ASSERT(c > 0);  ASSERT(c < 80000000H);
  IF i < LEN(dst) THEN
    IF c < 80H THEN dst[i] := c;  INC(i)
    ELSE
      IF    c < 800H     THEN  dst[i] := 0C0H + ASR(c, 6);    n := 1;
      ELSIF c < 10000H   THEN  dst[i] := 0E0H + ASR(c, 12);   n := 2;
      ELSIF c < 200000H  THEN  dst[i] := 0F0H + ASR(c, 18);   n := 3;
      ELSIF c < 4000000H THEN  dst[i] := 0F8H + ASR(c, 24);   n := 4;
      ELSE                     dst[i] := 0FCH + ASR(c, 30);   n := 5;
      END;
      INC(i);
      WHILE (n > 0) & (i < LEN(dst)) DO
        DEC(n);  dst[i] := 80H + ASR(c, n*6) MOD 40H;  INC(i)
      END;
    END
  END
END PutUtf8;


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


PROCEDURE Utf8ToUtf16*(src: ARRAY OF BYTE;  VAR dst: ARRAY OF SYSTEM.CARD16): INTEGER;
VAR i, j: INTEGER;
BEGIN  i := 0;  j := 0;
  WHILE (i < LEN(src)) & (src[i] # 0) DO PutUtf16(GetUtf8(src, i), dst, j) END;
  IF j < LEN(dst) THEN dst[j] := 0;  INC(j) END
RETURN j END Utf8ToUtf16;

PROCEDURE Utf16ToUtf8*(src: ARRAY OF SYSTEM.CARD16;  VAR dst: ARRAY OF BYTE): INTEGER;
VAR i, j: INTEGER;
BEGIN  i := 0;  j := 0;
  WHILE (i < LEN(src)) & (src[i] # 0) DO PutUtf8(GetUtf16(src, i), dst, j) END;
  IF j < LEN(dst) THEN dst[j] := 0;  INC(j) END
RETURN j END Utf16ToUtf8;


(* -------------------------------------------------------------------------- *)

PROCEDURE GetArg(VAR out: ARRAY OF CHAR;  n: INTEGER);
VAR i, arg: INTEGER;  str16: ARRAY 1024 OF SYSTEM.CARD16;
BEGIN (* GetArg *)
  ASSERT(ArgV # 0);
  IF (n < NumArgs) & (n >= 0) THEN i := 0;
    SYSTEM.GET(ArgV+n*8, arg);  ASSERT(arg # 0);

    SYSTEM.GET(arg, str16[i]);
    WHILE str16[i] # 0 DO INC(arg, 2);  INC(i);  SYSTEM.GET(arg, str16[i]) END
  ELSE str16[0] := 0
  END;
  i := Utf16ToUtf8(str16, out);
END GetArg;

(* -------------------------------------------------------------------------- *)

PROCEDURE writebytes(adr, len: INTEGER);
VAR written, result: INTEGER;
BEGIN result := WriteFile(HOut, adr, len, SYSTEM.ADR(written), 0) END writebytes;

PROCEDURE writearray(bytes: ARRAY OF BYTE);
BEGIN writebytes(SYSTEM.ADR(bytes), LEN(bytes)) END writearray;

PROCEDURE writebyte(b: BYTE); BEGIN writebytes(SYSTEM.ADR(b), 1) END writebyte;

PROCEDURE writesz(bytes: ARRAY OF BYTE);
VAR len: INTEGER;
BEGIN
  len := 0;  WHILE (len < LEN(bytes)) & (bytes[len] # 0) DO INC(len) END;
  writebytes(SYSTEM.ADR(bytes), len)
END writesz;

PROCEDURE wl*();                  BEGIN writearray($0D0A$)           END wl;
PROCEDURE wc*(c: CHAR);           BEGIN writebytes(SYSTEM.ADR(c), 1) END wc;
PROCEDURE ws*(s: ARRAY OF CHAR);  BEGIN writesz(s)                   END ws;
PROCEDURE wsl*(t: ARRAY OF CHAR); BEGIN ws(t); wl                    END wsl;
PROCEDURE wh1*(i: INTEGER);
BEGIN IF i<10 THEN wc(CHR(i + 48)) ELSE wc(CHR(i + 87)) END END wh1;
PROCEDURE wh*(i: INTEGER);
BEGIN
  IF (i < 0) OR (i > 15) THEN wh((i DIV 16) MOD 1000000000000000H) END;
  wh1(i MOD 16);
END wh;
PROCEDURE wi*(i: INTEGER);
BEGIN
  IF i < 0 THEN writebyte(ORD("-")); i := -i END;
  IF i > 9 THEN wi(i DIV 10) END;
  wc(CHR(i MOD 10 + 48))
END wi;

(* -------------------------------------------------------------------------- *)

PROCEDURE OpenX64(name: ARRAY OF CHAR): INTEGER;
VAR len, hfile: INTEGER;  name16: ARRAY 261 OF SYSTEM.CARD16;
BEGIN
  len   := Utf8ToUtf16(name, name16);
  hfile := CreateFileW(name16, 31(*GENERIC_READ*),   0(*FILE_SHARE_READ*),
                       0,      3(* OPEN_EXISTING *), 0, 0);
RETURN hfile END OpenX64;

PROCEDURE FileLength(hfile: INTEGER): INTEGER;
VAR res, length: INTEGER;
BEGIN
  res := GetFileSizeEx(Hx64, SYSTEM.ADR(length));
  IF res = 0 THEN wsl("Could not get file length.");  Halt(99) END
RETURN length END FileLength;

PROCEDURE FilePos(hfile: INTEGER): INTEGER;
VAR res, pos: INTEGER;
BEGIN res := SetFilePointerEx(hfile, 0, SYSTEM.ADR(pos), 1(*FILE_CURRENT*));
  IF res = 0 THEN wsl("Could not get file position.");  Halt(99) END;
RETURN pos END FilePos;

PROCEDURE SetPos(hfile, newpos: INTEGER);
VAR res, pos: INTEGER;
BEGIN res := SetFilePointerEx(hfile, newpos, SYSTEM.ADR(pos), 0(*FILE_BEGIN*));
  IF res = 0 THEN wsl("Could not get file position.");  Halt(99) END;
END SetPos;

PROCEDURE ReadMem(hfile, adr, len: INTEGER);
VAR res, bytesRead: INTEGER;
BEGIN
  res := ReadFile(hfile, adr, len, SYSTEM.ADR(bytesRead), 0);
  IF (res = 0) OR (bytesRead < len) THEN wsl("EOF during Read."); Halt(99) END
END ReadMem;

PROCEDURE Read(hfile: INTEGER; VAR buf: ARRAY OF BYTE);
BEGIN ReadMem(hfile, SYSTEM.ADR(buf), LEN(buf)) END Read;

PROCEDURE ReadString(hfile: INTEGER; VAR string: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN
  i := 0;  Read(hfile, string[i]);
  WHILE string[i] # 0X DO INC(i); Read(hfile, string[i]) END
END ReadString;

(* -------------------------------------------------------------------------- *)

(*
PROCEDURE New*(VAR ptr: INTEGER;  tdAdr: INTEGER);
VAR p, size, need, lim: INTEGER;
BEGIN
  SYSTEM.GET(tdAdr, size);  need := size+16;  Rounding(need);
  IF need = 32 THEN p := GetBlock32()
  ELSIF need = 64 THEN p := GetBlock64()
  ELSIF need = 128 THEN p := GetBlock128()
  ELSIF need = 256 THEN p := GetBlock256()
  ELSE p := GetBlock(need)
  END;

  SYSTEM.PUT(p, tdAdr);  SYSTEM.PUT(p+8, 0);  ptr := p+16;
  INC(p, 16);  INC(allocated, need);  lim := (p+size+7) DIV 8 * 8;
  WHILE p < lim DO SYSTEM.PUT(p, 0);  INC(p, 8) END
END New;
*)

PROCEDURE New*(VAR ptr: INTEGER;  tdAdr: INTEGER);
BEGIN ExitProcess(99);
END New;


(* -------------------------------------------------------------------------- *)


PROCEDURE FindModule(name: ARRAY OF CHAR): Module;
VAR
  mod: Module;
BEGIN
  mod := FirstModule;
  WHILE (mod # NIL) & (mod.name # name) DO mod := mod.next END;
RETURN mod END FindModule;


PROCEDURE LoadX64;
VAR
  mod:          Module;
  modname:      ARRAY 64 OF CHAR;
  key0, key1:   INTEGER;
  refmod:       Module;
  refmods:      ARRAY 32 OF Module;
  refmodsno:    INTEGER;
  modno, impno: SYSTEM.CARD16;
  size:         INTEGER;
  init:         INTEGER;
  impadr:       INTEGER;
  expadr:       INTEGER;
  exptype:      INTEGER;
  modPtrTable:  INTEGER;
  importCount:  INTEGER;
  i:            INTEGER;

BEGIN
  (* 1. Module name and key *)
  wsl("1. Module name and key.");
  ReadString(Hx64, modname);
  Read(Hx64, key0);  Read(Hx64, key1);
  ws("Loading module '"); ws(modname); wsl("'.");
  mod := FindModule(modname);
  IF mod # NIL THEN wsl("Error - already loaded.");  Halt(4) END;

  Align(memadr, 16);
  SYSTEM.PUT(SYSTEM.ADR(mod), memadr);  (* Allocate Module  descriptor. *)
  INC(memadr, SYSTEM.SIZE(ModuleDesc));
  Align(memadr, 16);
  mod.next := FirstModule;  FirstModule := mod;
  mod.name := modname;
  mod.key[0] := key0;  mod.key[1] := key1;
  mod.refcount := 0;
  mod.start    := 0;
  mod.base     := 0;
  mod.code     := 0;
  mod.init     := NIL;
  mod.traps    := 0;
  mod.exports  := 0;

  Read(Hx64, importCount);
  Read(Hx64, modPtrTable);

  (* 7. Code offset of initialisation entry point, if any *)
  wsl("7. Code offset of initialisation entry point, if any.");
  Read(Hx64, init);
  ws("initialisation address: $"); wh(init); wsl(".");


  (* 2. List of referenced modules *)
  wsl("2. List of referenced modules.");
  refmodsno := 0;
  ReadString(Hx64, modname);
  WHILE modname[0] # 0X DO
    refmod := FindModule(modname);
    IF refmod = NIL THEN
      ws("Error - required module '"); ws(modname); wsl("' not already loaded.");
      Halt(4);
    END;
    Read(Hx64, key0);  Read(Hx64, key1);
    IF (refmod.key[0] # key0) OR (refmod.key[1] # key1) THEN
      ws("Error - required module '"); ws(modname); wsl("' loaded with mismatched module key.");
      Halt(4);
    END;
    refmods[refmodsno] := refmod;  INC(refmodsno);
    INC(refmod.refcount);
    ReadString(Hx64, modname);
  END;

  (* Load module memory *)
  Align(memadr, 16);
  mod.start := memadr;

  Read(Hx64, size);
  INC(memadr, size);  Align(memadr, 16);

  (* 4. Initialised static data *)
  wsl("4. Initialised static data.");
  Read(Hx64, size);
  IF size > 0 THEN
    mod.base := memadr;
    ws("Base at $"); wh(memadr); wsl(".");
    ReadMem(Hx64, memadr, size);
  END;
  INC(memadr, size);  Align(memadr, 16);

  (* 6. Code *)
  wsl("6. Code.");
  Read(Hx64, size);
  IF size > 0 THEN
    mod.code := memadr;
    IF init >= 0 THEN SYSTEM.PUT(SYSTEM.ADR(mod.init), mod.code + init) END;
    ws("Code at $"); wh(memadr); wsl(".");
    ReadMem(Hx64, memadr, size);
  END;
  INC(memadr, size);  Align(memadr, 16);

  (* Set ptr table address. *)
  SYSTEM.PUT(mod.base + 112, modPtrTable);  (* Global pointers *)
  SYSTEM.PUT(mod.base + 104, 0);            (* Stack frame pointers *)

  (* Fill in standard import addresses *)
  SYSTEM.COPY(SYSTEM.ADR(StandardPointers), mod.base, 128);

  (* Process import references *)
  FOR i := 0 TO importCount-1 DO
    impadr := mod.base + 128 + i * 8;
    SYSTEM.GET(impadr, impno);  SYSTEM.GET(impadr+4, modno);
    SYSTEM.GET(refmods[modno].exports + 8 * impno, expadr);
    SYSTEM.PUT(impadr, expadr);
  END;

  (* 8. Trap table *)
  wsl("8. Trap table.");
  Read(Hx64, size);
  IF size > 0 THEN
    mod.traps := memadr;
    ReadMem(Hx64, memadr, size);
  END;
  INC(memadr, size);  Align(memadr, 16);

  (* 9. List of export addresses in export number order *)
  wsl("9. List of export addresses in export number order.");
  mod.exportNo := 0;
  Read(Hx64, expadr);
  IF expadr # 0 THEN
    mod.exports := memadr;
    WHILE expadr # 0  DO
      exptype :=  ASR(expadr, 62) MOD 4;
      expadr := expadr MOD 4000000000000000H;
      IF exptype < 3 THEN  (* Data relative address *)
        ws("  "); wi(mod.exportNo); ws(". Data export $"); wh(mod.base + expadr); wsl(".");
        SYSTEM.PUT(memadr, mod.base + expadr)
      ELSE           (* Code relative address *)
        ws("  "); wi(mod.exportNo); ws(". Code export $"); wh(mod.code + expadr); wsl(".");
        SYSTEM.PUT(memadr, mod.code + expadr)
      END;
      INC(memadr, 8);  INC(mod.exportNo);
      Read(Hx64, expadr);
    END;
    Align(memadr, 16);
  END;

  (* Initialise the module *)
  IF mod.init # NIL THEN wsl("Initialising."); mod.init END;
  ws("Module "); ws(mod.name); wsl(" load complete.");
END LoadX64;


PROCEDURE LoadCollection();
VAR
  filename: ARRAY 256 OF CHAR;
  length:   INTEGER;
  res:      INTEGER;
BEGIN
  GetArg(filename, 1);
  IF filename[0] = 0X THEN
    wsl("Expected filename.")
  ELSE
    Hx64 := OpenX64(filename);
    IF Hx64 < 0 THEN
      ws("File '"); ws(filename); wsl("' not found.");
    ELSE
      length := FileLength(Hx64);
      ws("Loading modules from "); ws(filename);
      ws(", length "); wi(length); wsl(" bytes.");
      WHILE FilePos(Hx64) < length DO
        wl; ws("Loading from offset "); wi(FilePos(Hx64)); wsl(".");
        LoadX64
      END
    END
  END
END LoadCollection;


PROCEDURE AllocateModuleMemory(memsize: INTEGER);
CONST
  memtype    = 3000H; (* MEM_RESERVE | MEM_COMMIT *)
  memprotect = 40H;   (* PAGE_EXECUTE_READWRITE *)
BEGIN
  ASSERT(VirtualAlloc # NIL);
  memadr := VirtualAlloc(0, memsize, memtype, memprotect);
  ASSERT(memadr # 0);
  memlimit := memadr + memsize;
END AllocateModuleMemory;


PROCEDURE FillInStandardPointers;
VAR res: INTEGER;
BEGIN
  (* Extract the 5 procedure addresses provided by the PE import table *)
  SYSTEM.GET(ModulesBase +  0, StandardPointers.GetProcAddress);
  SYSTEM.GET(ModulesBase +  8, StandardPointers.LoadLibraryA);
  SYSTEM.GET(ModulesBase + 16, ExitProcess);
  SYSTEM.GET(ModulesBase + 32, AddVectoredExceptionHandler);

  SYSTEM.LoadLibraryA(HKernel, "kernel32.dll");
  SYSTEM.LoadLibraryA(HUser,   "user32.dll");
  SYSTEM.LoadLibraryA(HShell,  "shell32.dll");

  SYSTEM.GetProcAddress(MessageBoxA,             HUser,   SYSTEM.ADR("MessageBoxA"));
  SYSTEM.GetProcAddress(MessageBoxW,             HUser,   SYSTEM.ADR("MessageBoxW"));
  SYSTEM.GetProcAddress(GetSystemTimeAsFileTime, HKernel, SYSTEM.ADR("GetSystemTimeAsFileTime"));
  SYSTEM.GetProcAddress(GetCommandLineW,         HKernel, SYSTEM.ADR("GetCommandLineW"));
  SYSTEM.GetProcAddress(CommandLineToArgvW,      HShell,  SYSTEM.ADR("CommandLineToArgvW"));
  SYSTEM.GetProcAddress(VirtualAlloc,            HKernel, SYSTEM.ADR("VirtualAlloc"));
  SYSTEM.GetProcAddress(GetStdHandle,            HKernel, SYSTEM.ADR("GetStdHandle"));
  SYSTEM.GetProcAddress(SetConsoleOutputCP,      HKernel, SYSTEM.ADR("SetConsoleOutputCP"));
  SYSTEM.GetProcAddress(CreateFileW,             HKernel, SYSTEM.ADR("CreateFileW"));
  SYSTEM.GetProcAddress(GetFileSizeEx,           HKernel, SYSTEM.ADR("GetFileSizeEx"));
  SYSTEM.GetProcAddress(ReadFile,                HKernel, SYSTEM.ADR("ReadFile"));
  SYSTEM.GetProcAddress(WriteFile,               HKernel, SYSTEM.ADR("WriteFile"));
  SYSTEM.GetProcAddress(CloseHandle,             HKernel, SYSTEM.ADR("CloseHandle"));
  SYSTEM.GetProcAddress(SetFilePointerEx,        HKernel, SYSTEM.ADR("SetFilePointerEx"));

  StandardPointers.Halt := Halt;
  StandardPointers.New  := New;

  HOut := GetStdHandle(-11);          (* STD_OUTPUT_HANDLE *)
  res  := SetConsoleOutputCP(65001);  (* UTF8              *)

  CommandLineWAdr := GetCommandLineW();
  ArgV := CommandLineToArgvW(CommandLineWAdr, SYSTEM.ADR(NumArgs))

END FillInStandardPointers;

BEGIN
  ModulesBase := SYSTEM.ADR(TheByteBeforeBase)+1;
  FirstModule := NIL;
  FillInStandardPointers;

  ws("Modules base:   $"); wh(ModulesBase);                     wsl(".");
  ws("GetProcAddress: $"); wh(StandardPointers.GetProcAddress); wsl(".");
  ws("LoadLibraryA:   $"); wh(StandardPointers.LoadLibraryA);   wsl(".");
  ws("HKernel:        $"); wh(HKernel);                         wsl(".");

  AllocateModuleMemory(80000000H);  (* 512MB *)
  ws("Allocated module memory at $"); wh(memadr); wsl(".");

  LoadCollection;

END Modules.