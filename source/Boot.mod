MODULE Boot;  (*$RTL-*)

(* Bootstrap loader - loads the subsequent modules in a PE *)

IMPORT SYSTEM;

CONST

TYPE
  ModuleHeader* = POINTER [untraced] TO ModuleHeaderDesc;
  ModuleHeaderDesc* = RECORD
    length*:      INTEGER;          (*   0                                *)
    next*:        ModuleHeader;     (*   8                                *)
    name*:        ARRAY 32 OF CHAR; (*  16                                *)
    base*:        INTEGER;          (*  48                                *)
    code*:        INTEGER;          (*  56                                *)
    init*:        INTEGER;          (*  64                                *)
    trap*:        INTEGER;          (*  72                                *)
    key0*, key1*: INTEGER;          (*  80                                *)
    importNames*: INTEGER;          (*  88 list of import names and keys  *)
    imports*:     INTEGER;          (*  96 adr of start of import list    *)
    exports*:     INTEGER           (* 104 array of export addresses      *)
  END;

  PEImportTable* = POINTER [untraced] TO PEImportsDesc;
  PEImportsDesc = RECORD
    GetProcAddress*: PROCEDURE(module, procname: INTEGER): INTEGER;
    LoadLibraryA*:   PROCEDURE(filename: INTEGER): INTEGER;
    ExitProcess*:    PROCEDURE(result: INTEGER);
    New*:            PROCEDURE(VAR ptr: INTEGER;  tdAdr: INTEGER)
                     (* Initially zero, kernel patches with impl of New *)
  END;

  ModuleBase = POINTER [untraced] TO ModuleBaseDesc;
  ModuleBaseDesc* = RECORD
    (*   0 00 *) GetProcAddress:  PROCEDURE(module, procname: INTEGER): INTEGER;
    (*   8 08 *) LoadLibraryA:    PROCEDURE(filename: INTEGER): INTEGER;
    (*  16 10 *) ExitProcess:     PROCEDURE(result: INTEGER);
    (*  24 18 *) z1, z2, z3, z4:  INTEGER;
    (*  56 38 *) ModHdrOffset*:   INTEGER;
    (*  64 40 *) z5, z6, z7, z8:  INTEGER;
    (*  96 60 *) z9:              INTEGER;
    (* 104 68 *) StackPtrTable:   INTEGER;
    (* 112 70 *) ModulePtrTable*: INTEGER;
    (* 120 78 *) New:             PROCEDURE(VAR ptr: INTEGER;  tdAdr: INTEGER)
  END;

VAR
  oneByteBeforeBase: CHAR; (* MUST BE THE FIRST GLOBAL VARIABLE        *)
                           (* - its address locates this module's base *)

  FirstModule*: ModuleHeader;
  PEImports*:   PEImportTable;

  WinBase:      ModuleBase;
  WinHdr:       ModuleHeader;
  module:       ModuleHeader;
  nextModule:   ModuleHeader;
  initialise:   PROCEDURE;


(* -------------------------------------------------------------------------- *)
(* --------- Link newly loaded module to previously loaded modules ---------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE GetString(adr: INTEGER; VAR str: ARRAY OF CHAR): INTEGER;
(* Extract string from memory *)
VAR i: INTEGER;
BEGIN i := 0;
  REPEAT
    SYSTEM.GET(adr, str[i]);  INC(adr);  INC(i)
  UNTIL (i = LEN(str)) OR (str[i-1] = 0X);
  IF i = LEN(str) THEN str[i-1] := 0X END
RETURN i END GetString;


PROCEDURE Link(header: ModuleHeader);
(* Convert offsets in the Module header to absolute addresses. *)
(* Populate procedure pointers.                                *)
(* Convert export offsets to absolute addresses.               *)
(* Lookup imported modules.                                    *)
(* Convert import references to absolute addresses.            *)
VAR
  base:         ModuleBase;
  export:       INTEGER;
  exportadr:    INTEGER;
  i:            INTEGER;
  importAdr:    INTEGER;
  imports:      ARRAY 64 OF INTEGER;
  modno:        INTEGER;
  expno:        INTEGER;
  impadr:       INTEGER;
  impname:      ARRAY 64 OF CHAR;
  impheader:    ModuleHeader;
  hdrname:      ARRAY 64 OF CHAR;
  impkey0:      INTEGER;
  impkey1:      INTEGER;
  importRefAdr: INTEGER;
  importRef:    INTEGER;

BEGIN
  (* Convert module header offsets to absolute addresses *)
  IF header.base        # 0 THEN INC(header.base,        SYSTEM.ADR(header^)) END;
  IF header.code        # 0 THEN INC(header.code,        SYSTEM.ADR(header^)) END;
  IF header.init        # 0 THEN INC(header.init,        SYSTEM.ADR(header^)) END;
  IF header.trap        # 0 THEN INC(header.trap,        SYSTEM.ADR(header^)) END;
  IF header.importNames # 0 THEN INC(header.importNames, SYSTEM.ADR(header^)) END;
  IF header.exports     # 0 THEN INC(header.exports,     SYSTEM.ADR(header^)) END;

  (* Set standard procedure addresses into module static data *)
  base := SYSTEM.VAL(ModuleBase, header.base);
  base.GetProcAddress := PEImports.GetProcAddress;
  base.LoadLibraryA   := PEImports.LoadLibraryA;
  base.ExitProcess    := PEImports.ExitProcess;
  base.New            := PEImports.New;
  IF base.ModulePtrTable # 0 THEN INC(base.ModulePtrTable, header.base) END;

  (* Convert export offsets to absolute *)
  IF header.exports # 0 THEN
    export := header.exports;  SYSTEM.GET(export, exportadr);
    WHILE exportadr # 8000000000000000H DO
      SYSTEM.PUT(export, exportadr + header.base);
      INC(export, 8);
      SYSTEM.GET(export, exportadr)
    END
  END;

  (* Convert imported module names to module export table addresses *)
  IF header.importNames # 0 THEN
    importAdr := header.importNames;
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
  importRefAdr := header.imports;
  WHILE importRefAdr # 0 DO
    SYSTEM.GET(header.base + importRefAdr, importRef);
    modno := ASR(importRef, 48);
    expno := ASR(importRef, 32) MOD 10000H;
    SYSTEM.GET(imports[modno] + expno * 8, impadr);
    SYSTEM.PUT(header.base + importRefAdr, impadr);
    importRefAdr := importRef MOD 100000000H;
  END;

  (*
  FOR i := 0 TO header.importCount-1 DO
    SYSTEM.GET(header.base + 128 + i*8, expno);
    modno := expno DIV 100000000H;  expno := expno MOD 100000000H;
    SYSTEM.GET(imports[modno] + expno * 8, impadr);
    SYSTEM.PUT(header.base + 128 + i*8, impadr)
  END
  *)
END Link;


(* -------------------------------------------------------------------------- *)
(* ----------- Win PE pre-initialisation code - Oberon bootstrap ------------ *)
(* -------------------------------------------------------------------------- *)

BEGIN
  (* Initialisation code for the first module - this is the first code that   *)
  (* runs when the PE is loaded. It runs before it has been linked in and it  *)
  (* is its responsibility to link (connect) in both itself and all modules   *)
  (* that follow in the PE 'Oberon' section.                                  *)

  (* The 128 byte pointers block is at the start of static data and is the    *)
  (* base address used within the module code.                                *)
  WinBase := SYSTEM.VAL(ModuleBase, SYSTEM.ADR(oneByteBeforeBase) + 1);

  IF WinBase.ModHdrOffset # 0 THEN  (* If loaded as PE bootstrap *)

    (* The WinBase block includes the offset from the module header to the   *)
    (* WinBase block.                                                        *)
    WinHdr := SYSTEM.VAL(ModuleHeader, SYSTEM.ADR(WinBase^) - WinBase.ModHdrOffset);

    (* A minimal set of Win32 function addresses sits just before the first     *)
    (* module header.                                                           *)
    PEImports := SYSTEM.VAL(PEImportTable, SYSTEM.ADR(WinHdr^) - SYSTEM.SIZE(PEImportsDesc));

    (* Link this module - the Windows PE bootstrap *)
    FirstModule := WinHdr;
    Link(WinHdr);  (* Note - does not call WinHdr's init address as that is *)
                   (* this code and we're already running.                  *)

    (* Link remaining modules in EXE 'Oberon' section *)
    module := SYSTEM.VAL(ModuleHeader, SYSTEM.ADR(WinHdr^) + WinHdr.length);
    WinHdr.next := module;
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
    END;

    PEImports.ExitProcess(4)
  END
END Boot.
