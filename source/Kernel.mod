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

VAR
  oneByteBeforeBase: CHAR;      (* MUST BE THE FIRST GLOBAL VARIABLE - its address locates base *)

  User*:        INTEGER;
  Kernel*:      INTEGER;

  FirstModule*: ModuleHeader;
  PEImports:    PEImportTable;

  MessageBoxA:  PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER);


(* -------------------------------------------------------------------------- *)

PROCEDURE Msg*(str: ARRAY OF CHAR);
VAR user: INTEGER;  adr: INTEGER;
BEGIN
  IF MessageBoxA = NIL THEN
    adr := PEImports.GetProcAddress(User, SYSTEM.ADR("MessageBoxA"));
    SYSTEM.PUT(SYSTEM.ADR(MessageBoxA), adr);
  END;
  MessageBoxA(0, SYSTEM.ADR(str), SYSTEM.ADR("Kernel"), 0);
END Msg;

PROCEDURE Msg2*(str1, str2: ARRAY OF CHAR);
VAR msg: ARRAY 256 OF CHAR;  i, j: INTEGER;
BEGIN i := 0;
  WHILE str1[i] # 0X DO msg[i] := str1[i];  INC(i) END;
  j := i;  i := 0;
  WHILE str2[i] # 0X DO msg[j] := str2[i];  INC(i);  INC(j) END;
  msg[j] := 0X;
  Msg(msg)
END Msg2;

PROCEDURE MsgI*(msg: ARRAY OF CHAR; n: INTEGER);
VAR i, j: INTEGER;  num: ARRAY 25 OF CHAR;
BEGIN i := LEN(num)-1;
  WHILE n > 0 DO
    num[i] := CHR(48 + n MOD 10);
    n := n DIV 10;
    DEC(i)
  END;
  IF i = LEN(num)-1 THEN num[i] := "0";  DEC(i) END;
  j := i + 1;
  i := 0;
  WHILE j < LEN(num) DO num[i] := num[j];  INC(i);  INC(j) END;
  num[i] := 0X;
  Msg2(msg, num)
END MsgI;

PROCEDURE Halt*(returnCode: INTEGER);
BEGIN PEImports.ExitProcess(returnCode) END Halt;

PROCEDURE Abort(str: ARRAY OF CHAR);
BEGIN Msg(str);  Halt(9) END Abort;

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
  Kernel := PEImports.LoadLibraryA(SYSTEM.ADR("kernel32.dll"));
  User   := PEImports.LoadLibraryA(SYSTEM.ADR("user32.dll"));
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
