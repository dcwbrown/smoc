MODULE ObjBoot;  (*$OBJECT*)

IMPORT SYSTEM;

TYPE
  Win32Imports = POINTER TO Win32ImportsDesc;
  Win32ImportsDesc = RECORD
    GetProcAddress: PROCEDURE(module, procname: INTEGER): INTEGER;
    LoadLibraryA:   PROCEDURE(filename: INTEGER): INTEGER;
    ExitProcess:    PROCEDURE(result: INTEGER);
    New:            PROCEDURE()
  END;

  ModuleHeader = POINTER TO ModuleHeaderDesc;
  ModuleHeaderDesc = RECORD
    next:            INTEGER; (*  0                                          *)
    base:            INTEGER; (*  8                                          *)
    code:            INTEGER; (* 16                                          *)
    init:            INTEGER; (* 24                                          *)
    trap:            INTEGER; (* 32                                          *)
    name:            INTEGER; (* 40 offset of sz module name string          *)
    key0, key1:      INTEGER; (* 48                                          *)
    imports:         INTEGER; (* 56 offset of array of import names and keys *)
    importCount:     INTEGER; (* 72 number of imports at base+128            *)
    exports:         INTEGER  (* 80 offset of array of export addresses      *)
  END;

  ModulePointers = POINTER TO ModulePointersDesc;
  ModulePointersDesc = RECORD
    (*   0 *) GetProcAddress: PROCEDURE(module, procname: INTEGER): INTEGER;
    (*   8 *) LoadLibraryA:   PROCEDURE(filename: INTEGER): INTEGER;
    (*  16 *) ExitProcess:    PROCEDURE(result: INTEGER);
    (*  24 *) z1, z2, z3, z4: INTEGER;
    (*  56 *) z5, z6, z7, z8: INTEGER;
    (*  88 *) z9:             INTEGER;
    (*  96 *) ModHdrOffset:   INTEGER;
    (* 104 *) StackPtrTable:  INTEGER;
    (* 112 *) ModulePtrTable: INTEGER;
    (* 120 *) New:            PROCEDURE()
  END;

VAR
  oneByteBeforeBase: CHAR;      (* MUST BE THE FIRST VARIABLE - its address locates base *)
  header:      ModuleHeader;
  imports:     Win32Imports;
  pointers:    ModulePointers;
  initCode:    INTEGER;
  FirstHeader: ModuleHeader;

  MessageBoxA: PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER);
  Initialise:  PROCEDURE;

(* -------------------------------------------------------------------------- *)

PROCEDURE Msg(str: ARRAY OF CHAR);
VAR User: INTEGER;
BEGIN
  IF MessageBoxA = NIL THEN
    SYSTEM.LoadLibraryA(User, "user32.dll");
    SYSTEM.GetProcAddress(MessageBoxA, User, SYSTEM.ADR("MessageBoxA"));
  END;
  MessageBoxA(0, SYSTEM.ADR(str), SYSTEM.ADR("X64Boot"), 0);
END Msg;

PROCEDURE Msg2(str1, str2: ARRAY OF CHAR);
VAR msg: ARRAY 256 OF CHAR;  i, j: INTEGER;
BEGIN i := 0;
  WHILE str1[i] # 0X DO msg[i] := str1[i];  INC(i) END;
  j := i;  i := 0;
  WHILE str2[i] # 0X DO msg[j] := str2[i];  INC(i);  INC(j) END;
  msg[j] := 0X;
  Msg(msg)
END Msg2;

PROCEDURE MsgI(msg: ARRAY OF CHAR; n: INTEGER);
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

PROCEDURE Abort(str: ARRAY OF CHAR);
BEGIN Msg(str);  imports.ExitProcess(9) END Abort;

(* -------------------------------------------------------------------------- *)

PROCEDURE GetString(VAR adr: INTEGER; VAR str: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN i := 0;
  REPEAT
    SYSTEM.GET(adr, str[i]);  INC(adr);  INC(i)
  UNTIL (i = LEN(str)) OR (str[i-1] = 0X);
  IF i = LEN(str) THEN str[i-1] := 0X END
END GetString;


PROCEDURE GetInteger(VAR adr, i: INTEGER);
BEGIN SYSTEM.GET(adr, i);  INC(adr, 8) END GetInteger;

(* -------------------------------------------------------------------------- *)

PROCEDURE FindImport(name: ARRAY OF CHAR; key0, key1: INTEGER;
                     limitheader: ModuleHeader): INTEGER;
(* Returns address of export table of named module *)
VAR
  header:  ModuleHeader;
  result:  INTEGER;
  adr:     INTEGER;
  hdrname: ARRAY 64 OF CHAR;
BEGIN result := 0;
  IF FirstHeader # NIL THEN
    header := FirstHeader;
    WHILE header # limitheader DO
      adr := header.name;  GetString(adr, hdrname);
      IF (name = hdrname) & (key0 = header.key0) & (key1 = header.key1) THEN
        result := header.exports;  header := limitheader
      ELSE
        header := SYSTEM.VAL(ModuleHeader, header.next)
      END
    END;
    IF (header # limitheader) THEN result := header.exports END
  END;
RETURN result END FindImport;


PROCEDURE Connect(wimports: Win32Imports; header: ModuleHeader);
(*  o  Populate procedure pointers                      *)
(*  o  Convert offsets in the Module header to pointers *)
(*  o  Convert import references to absolute addresses  *)
(*  o  Convert export offsets to absolute addresses     *)
VAR
  pointers:  ModulePointers;
  export:    INTEGER;
  exportadr: INTEGER;
  i:         INTEGER;
  import:    INTEGER;
  imports:   ARRAY 64 OF INTEGER;
  modno:     INTEGER;
  expno:     INTEGER;
  impadr:    INTEGER;
  impname:   ARRAY 64 OF CHAR;
  impkey0:   INTEGER;
  impkey1:   INTEGER;

BEGIN
  (* Convert module header offsets to absolute addresses *)
  IF header.next    # 0 THEN INC(header.next,    SYSTEM.ADR(header^)) END;
  IF header.base    # 0 THEN INC(header.base,    SYSTEM.ADR(header^)) END;
  IF header.code    # 0 THEN INC(header.code,    SYSTEM.ADR(header^)) END;
  IF header.init    # 0 THEN INC(header.init,    SYSTEM.ADR(header^)) END;
  IF header.trap    # 0 THEN INC(header.trap,    SYSTEM.ADR(header^)) END;
  IF header.name    # 0 THEN INC(header.name,    SYSTEM.ADR(header^)) END;
  IF header.imports # 0 THEN INC(header.imports, SYSTEM.ADR(header^)) END;
  IF header.exports # 0 THEN INC(header.exports, SYSTEM.ADR(header^)) END;

  (* Set standard procedure addresses into module static data *)
  pointers := SYSTEM.VAL(ModulePointers, header.base);
  pointers.GetProcAddress := wimports.GetProcAddress;
  pointers.LoadLibraryA   := wimports.LoadLibraryA;
  pointers.ExitProcess    := wimports.ExitProcess;
  pointers.New            := wimports.New;

  (* Convert export offsets to absolute *)
  IF header.exports # 0 THEN
    export := header.exports;  SYSTEM.GET(export, exportadr);
    WHILE exportadr # 0 DO
      SYSTEM.PUT(export, exportadr + header.base);
      INC(export, 8);
      SYSTEM.GET(export, exportadr)
    END
  END;

  (* Convert import module names to module export table addresses *)
  IF header.imports # 0 THEN
    import := header.imports;
    GetString(import, impname);  i := 0;
    WHILE impname[0] # 0X DO
      GetInteger(import, impkey0);  GetInteger(import, impkey1);
      imports[i] := FindImport(impname, impkey0, impkey1, header);
      GetString(import, impname);  INC(i)
    END
  END;

(*
  FOR i := 0 TO header.importNameCount-1 DO
    SYSTEM.GET(header.imports + i*8, import);
    imports[i] := FindImport(import + SYSTEM.ADR(header^), header);
    IF imports[i] = 0 THEN Abort("Cannot find import module.") END
  END;
*)

  (* Connect imports to exports *)
  FOR i := 0 TO header.importCount-1 DO
    SYSTEM.GET(header.base + 128 + i*8, expno);
    modno := expno DIV 100000000H;  expno := expno MOD 100000000H;
    SYSTEM.GET(imports[modno] + expno * 8, impadr);
    SYSTEM.PUT(header.base + 128 + i*8, impadr)
  END
END Connect;


BEGIN
  FirstHeader := NIL;

  (* Initialisation code for the first module - this is the first code that   *)
  (* runs when the PE is loaded. It runs before it has been linked in and it  *)
  (* is its responsibility to link (connect) in both itself and all modules   *)
  (* that follow in the PE 'Oberon' section.                                  *)

  (* The 128 byte pointers block is at the start of static data and is the    *)
  (* base address used within the module code.                                *)
  pointers := SYSTEM.VAL(ModulePointers, SYSTEM.ADR(oneByteBeforeBase) + 1);

  (* The pointers block includes the offset from the module header to the     *)
  (* pointers block.                                                          *)
  FirstHeader := SYSTEM.VAL(ModuleHeader,
                            SYSTEM.ADR(pointers^) - pointers.ModHdrOffset);

  (* A minimal set of Win32 function addresses sits just before the first     *)
  (* module header.                                                           *)
  imports := SYSTEM.VAL(Win32Imports,
                        SYSTEM.ADR(FirstHeader^) - SYSTEM.SIZE(Win32ImportsDesc));

  (* Connect the first module which is this (running) module.                 *)
  Connect(imports, FirstHeader);

  (* Connect the remaining modules *)
  header := SYSTEM.VAL(ModuleHeader, FirstHeader.next);
  WHILE header.next # 0 DO
    Connect(imports, header);

    IF header.init # 0 THEN
      SYSTEM.PUT(SYSTEM.ADR(Initialise), header.init);
      Initialise
    END;

    header := SYSTEM.VAL(ModuleHeader, header.next)
  END;


  imports.ExitProcess(4);
END ObjBoot.
