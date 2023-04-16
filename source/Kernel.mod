MODULE Kernel;  (*$OBJECT*)

IMPORT SYSTEM;

TYPE
  ModuleHeader* = POINTER [untraced] TO ModuleHeaderDesc;
  ModuleHeaderDesc = RECORD
    length*:     INTEGER;          (*   0                                *)
    next*:       ModuleHeader;     (*   8                                *)
    name*:       ARRAY 32 OF CHAR; (*  16                                *)
    base:        INTEGER;          (*  48                                *)
    code*:       INTEGER;          (*  56                                *)
    init:        INTEGER;          (*  64                                *)
    trap*:       INTEGER;          (*  72                                *)
    key0, key1:  INTEGER;          (*  80                                *)
    imports:     INTEGER;          (*  88 list of import names and keys  *)
    importCount: INTEGER;          (*  96 number of imports at base+128  *)
    exports:     INTEGER           (* 104 array of export addresses      *)
  END;

  PEImportTable = POINTER [untraced] TO PEImportsDesc;
  PEImportsDesc = RECORD
    GetProcAddress: PROCEDURE(module, procname: INTEGER): INTEGER;
    LoadLibraryA:   PROCEDURE(filename: INTEGER): INTEGER;
    ExitProcess:    PROCEDURE(result: INTEGER);
    zeroterminator: INTEGER
  END;

  ModuleBase = POINTER [untraced] TO ModuleBaseDesc;
  ModuleBaseDesc = RECORD
    (*   0 *) GetProcAddress: PROCEDURE(module, procname: INTEGER): INTEGER;
    (*   8 *) LoadLibraryA:   PROCEDURE(filename: INTEGER): INTEGER;
    (*  16 *) ExitProcess:    PROCEDURE(result: INTEGER);
    (*  24 *) z1, z2, z3, z4: INTEGER;
    (*  56 *) z5, z6, z7, z8: INTEGER;
    (*  88 *) z9:             INTEGER;
    (*  96 *) ModHdrOffset:   INTEGER;
    (* 104 *) StackPtrTable:  INTEGER;
    (* 112 *) ModulePtrTable: INTEGER;
    (* 120 *) New:            PROCEDURE(VAR ptr: INTEGER;  tdAdr: INTEGER)
  END;

  ExceptionHandlerProc = PROCEDURE(p: INTEGER): INTEGER;

VAR
  oneByteBeforeBase: CHAR; (* MUST BE THE FIRST GLOBAL VARIABLE       *)
                           (* - its address locates the kernel's base *)

  User*:        INTEGER;
  Kernel*:      INTEGER;

  FirstModule*: ModuleHeader;
  PEImports:    PEImportTable;

  Halt*:        PROCEDURE(returnCode: INTEGER);

  MessageBoxW:  PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER): INTEGER;
  AddVectoredExceptionHandler: PROCEDURE(first: INTEGER; filter: ExceptionHandlerProc);


(* -------------------------------------------------------------------------- *)
(* ------------ Unicode Transformation Formats UTF-8 and UTF-16 ------------- *)
(* -------------------------------------------------------------------------- *)

(* UTF8:                                                                                           *)
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
  res := MessageBoxW(0, SYSTEM.ADR(msg16), SYSTEM.ADR(title16), 0)
END MessageBox;


(* -------------------------------------------------------------------------- *)
(* ----------------------- Simple Integer formatting ------------------------ *)
(* -------------------------------------------------------------------------- *)

PROCEDURE IntToDecimal*(n: INTEGER; VAR s: ARRAY OF CHAR);
VAR i, j: INTEGER;  ch: CHAR;
BEGIN
  IF n = 8000000000000000H THEN s := "-9223372036854775808"
  ELSE i := 0;
    IF n < 0 THEN s[0] := '-';  i := 1; END;
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
    d := n MOD 16;  n := n DIV 16;
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
                               IntToHex(code, number);
                               Append(number, s)
  END
END WriteException;

PROCEDURE ExceptionHandler(p: INTEGER): INTEGER;
TYPE
  Exception = POINTER TO ExceptionDesc;
  ExceptionDesc = RECORD
    code:         SYSTEM.CARD32;
    flags:        SYSTEM.CARD32;
    nested:       Exception;
    address:      INTEGER;
    NumberParams: SYSTEM.CARD32;
    Params:       ARRAY 15 OF INTEGER
  END;

  Context = POINTER TO ContextDesc;
  ContextDesc = RECORD
  END;

  ExceptionPointers = POINTER TO ExceptionPointersDesc;
  ExceptionPointersDesc = RECORD
    exception: Exception;
    context:   Context
  END;

VAR
  ep:      ExceptionPointers;
  module:  ModuleHeader;
  address: INTEGER;
  trapadr: INTEGER;
  detail:  INTEGER;
  trap:    INTEGER;
  line:    INTEGER;
  col:     INTEGER;
  adr:     INTEGER;
  report:  ARRAY 256 OF CHAR;
  number:  ARRAY 25 OF CHAR;
BEGIN
  ep := SYSTEM.VAL(ExceptionPointers, p);
  address := ep.exception.address;
  module  := FirstModule;
  WHILE (module # NIL) & (module.length # 0) & ((address < module.code) OR (address > module.trap)) DO
    module := module.next
  END;

  IF module = NIL THEN
    WriteException(ep.exception.code, report);
    Append(" at address $", report);
    IntToHex(address, number);  Append(number, report);
    Append(" (not in loaded module code).", report);
  ELSE
    trapadr := module.trap;
    DEC(address, module.code);  (* Make address relative to modules code *)
    SYSTEM.GET(trapadr, detail);
    WHILE detail # -1 DO
      trap := ASR(detail, 60) MOD 10H;
      line := ASR(detail, 40) MOD 100000H;
      col  := ASR(detail, 30) MOD 400H;
      adr  := detail MOD 40000000H;
      IF adr = address THEN
        detail := -1  (* End loop *)
      ELSE
        INC(trapadr, 8);  SYSTEM.GET(trapadr, detail);
      END
    END;
    IF (adr = address) & (trap <= 8) THEN
      CASE trap OF
      | 0: report := "Modkey trap in module "
      | 1: report := "Array trap in module "
      | 2: report := "Type trap in module "
      | 3: report := "String trap in module "
      | 4: report := "Nil trap in module "
      | 5: report := "NilProc trap in module "
      | 6: report := "Divide trap in module "
      | 7: report := "Assert trap in module "
      | 8: report := "Rtl trap in module "
      END;
      Append(module.name, report);
      Append(" at ", report);
      IntToDecimal(line, number); Append(number, report);
      Append(":", report);
      IntToDecimal(col,  number); Append(number, report);
    ELSE
      WriteException(ep.exception.code, report);
      Append(" in module ", report);  Append(module.name, report);
      Append(" at code offset $", report);
      IntToHex(address, number);  Append(number, report)
    END
  END;

  MessageBox("Exception", report);
  Halt(99)
RETURN 0 END ExceptionHandler;



(* -------------------------------------------------------------------------- *)

PROCEDURE Abort(str: ARRAY OF CHAR);
BEGIN MessageBox("Abort", str);  Halt(9) END Abort;

(* -------------------------------------------------------------------------- *)

PROCEDURE New*(VAR ptr: INTEGER;  tdAdr: INTEGER);
(*VAR p, size, need, lim: INTEGER;*)
BEGIN
  Abort("New not implemented.");
(*
  SYSTEM.GET(tdAdr, size);  need := size+16;  Rounding(need);
  IF    need = 32  THEN p := GetBlock32()
  ELSIF need = 64  THEN p := GetBlock64()
  ELSIF need = 128 THEN p := GetBlock128()
  ELSIF need = 256 THEN p := GetBlock256()
  ELSE p := GetBlock(need)
  END;

  SYSTEM.PUT(p, tdAdr);  SYSTEM.PUT(p+8, 0);  ptr := p+16;
  INC(p, 16);  INC(allocated, need);  lim := (p+size+7) DIV 8 * 8;
  WHILE p < lim DO SYSTEM.PUT(p, 0);  INC(p, 8) END
*)
END New;


(* -------------------------------------------------------------------------- *)
(* ----------------------- Extract string from memory ----------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE GetString(adr: INTEGER; VAR str: ARRAY OF CHAR): INTEGER;
VAR i: INTEGER;
BEGIN i := 0;
  REPEAT
    SYSTEM.GET(adr, str[i]);  INC(adr);  INC(i)
  UNTIL (i = LEN(str)) OR (str[i-1] = 0X);
  IF i = LEN(str) THEN str[i-1] := 0X END
RETURN i END GetString;


(* -------------------------------------------------------------------------- *)

PROCEDURE Link(header: ModuleHeader);
(* Convert offsets in the Module header to absolute addresses. *)
(* Populate procedure pointers.                                *)
(* Convert export offsets to absolute addresses.               *)
(* Lookup imported modules.                                    *)
(* Convert import references to absolute addresses.            *)
VAR
  base:      ModuleBase;
  export:    INTEGER;
  exportadr: INTEGER;
  i:         INTEGER;
  importAdr: INTEGER;
  imports:   ARRAY 64 OF INTEGER;
  modno:     INTEGER;
  expno:     INTEGER;
  impadr:    INTEGER;
  impname:   ARRAY 64 OF CHAR;
  impheader: ModuleHeader;
  hdrname:   ARRAY 64 OF CHAR;
  impkey0:   INTEGER;
  impkey1:   INTEGER;

BEGIN
  (* Convert module header offsets to absolute addresses *)
  IF header.base    # 0 THEN INC(header.base,    SYSTEM.ADR(header^)) END;
  IF header.code    # 0 THEN INC(header.code,    SYSTEM.ADR(header^)) END;
  IF header.init    # 0 THEN INC(header.init,    SYSTEM.ADR(header^)) END;
  IF header.trap    # 0 THEN INC(header.trap,    SYSTEM.ADR(header^)) END;
  IF header.imports # 0 THEN INC(header.imports, SYSTEM.ADR(header^)) END;
  IF header.exports # 0 THEN INC(header.exports, SYSTEM.ADR(header^)) END;

  (* Set standard procedure addresses into module static data *)
  base := SYSTEM.VAL(ModuleBase, header.base);
  base.GetProcAddress := PEImports.GetProcAddress;
  base.LoadLibraryA   := PEImports.LoadLibraryA;
  base.ExitProcess    := Halt;
  base.New            := New;

  (* Convert export offsets to absolute *)
  IF header.exports # 0 THEN
    export := header.exports;  SYSTEM.GET(export, exportadr);
    WHILE exportadr # 0 DO
      SYSTEM.PUT(export, exportadr + header.base);
      INC(export, 8);
      SYSTEM.GET(export, exportadr)
    END
  END;

  (* Convert imported module names to module export table addresses *)
  IF header.imports # 0 THEN
    importAdr := header.imports;
    INC(importAdr, GetString(importAdr, impname));
    i := 0;  imports[i] := 0;
    WHILE impname[0] # 0X DO
      SYSTEM.GET(importAdr, impkey0);  INC(importAdr, 8);
      SYSTEM.GET(importAdr, impkey1);  INC(importAdr, 8);
      impheader := FirstModule;
      WHILE (impheader # NIL) & (imports[i] = 0) DO
        IF (impname = impheader.name) & (impkey0 = impheader.key0) & (impkey1 = impheader.key1) THEN
          imports[i] := impheader.exports
        END;
        impheader := impheader.next
      END;
      INC(importAdr, GetString(importAdr, impname));  INC(i)
    END
  END;

  (* Link imports to exports *)
  FOR i := 0 TO header.importCount-1 DO
    SYSTEM.GET(header.base + 128 + i*8, expno);
    modno := expno DIV 100000000H;  expno := expno MOD 100000000H;
    SYSTEM.GET(imports[modno] + expno * 8, impadr);
    SYSTEM.PUT(header.base + 128 + i*8, impadr)
  END
END Link;


(* -------------------------------------------------------------------------- *)

PROCEDURE InitialiseKernel;  (* initialise kernel module *)
BEGIN
  (* Set up some useful exports from standard procedures. *)
  Halt   := PEImports.ExitProcess;
  Kernel := PEImports.LoadLibraryA(SYSTEM.ADR("kernel32.dll"));
  User   := PEImports.LoadLibraryA(SYSTEM.ADR("user32.dll"));

  SYSTEM.PUT(SYSTEM.ADR(MessageBoxW),
             PEImports.GetProcAddress(User, SYSTEM.ADR("MessageBoxW")));

  SYSTEM.PUT(SYSTEM.ADR(AddVectoredExceptionHandler),
             PEImports.GetProcAddress(Kernel, SYSTEM.ADR("AddVectoredExceptionHandler")));

  AddVectoredExceptionHandler(1, ExceptionHandler);
END InitialiseKernel;

(* -------------------------------------------------------------------------- *)


PROCEDURE Bootstrap;
VAR
  kernelBase:   ModuleBase;
  kernelHeader: ModuleHeader;
  module:       ModuleHeader;
  nextModule:   ModuleHeader;
  initialise:   PROCEDURE;
BEGIN
  (* Initialisation code for the first module - this is the first code that   *)
  (* runs when the PE is loaded. It runs before it has been linked in and it  *)
  (* is its responsibility to link (connect) in both itself and all modules   *)
  (* that follow in the PE 'Oberon' section.                                  *)

  (* The 128 byte pointers block is at the start of static data and is the    *)
  (* base address used within the module code.                                *)
  kernelBase := SYSTEM.VAL(ModuleBase, SYSTEM.ADR(oneByteBeforeBase) + 1);

  (* The kernelBase block includes the offset from the module header to the   *)
  (* kernelBase block.                                                        *)
  kernelHeader := SYSTEM.VAL(ModuleHeader,
                            SYSTEM.ADR(kernelBase^) - kernelBase.ModHdrOffset);

  (* A minimal set of Win32 function addresses sits just before the first     *)
  (* module header.                                                           *)
  PEImports := SYSTEM.VAL(PEImportTable,
                          SYSTEM.ADR(kernelHeader^) - SYSTEM.SIZE(PEImportsDesc));

  (* Link this module - the kernel *)
  FirstModule := kernelHeader;
  Link(kernelHeader);
  InitialiseKernel;

  (* Link remaining modules in EXE 'Oberon' section *)
  module := SYSTEM.VAL(ModuleHeader,
                          SYSTEM.ADR(kernelHeader^) + kernelHeader.length);
  kernelHeader.next := module;
  WHILE module # NIL DO
    Link(module);  module.next := NIL;

    IF module.init # 0 THEN
      SYSTEM.PUT(SYSTEM.ADR(initialise), module.init);  initialise;
    END;

    (* Set header next pointer to next header, if any. *)
    nextModule := SYSTEM.VAL(ModuleHeader, module.length + SYSTEM.ADR(module^));
    IF nextModule.length = 0 THEN nextModule := NIL END;

    module.next := nextModule;
    module      := nextModule
  END
END Bootstrap;

(* -------------------------------------------------------------------------- *)


BEGIN
  Bootstrap;
  Halt(4);
END Kernel.
