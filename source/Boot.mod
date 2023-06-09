MODULE Boot;  (*$RTL-*)

(* Bootstrap loader - loads the subsequent modules in a PE *)

IMPORT SYSTEM;

CONST
  (* Offsets within first 128 bytes of module base *)
  OffExitProcess*     = 16;
  OffModHeaderOffset* = 56;
  OffStackPtrTable*   = 104;
  OffModulePtrTable*  = 112;
  OffNew*             = 120;

  (* Module binary header offsets *)
  OffModLength*      =   0;
  OffModNext*        =   8;
  OffModName*        =  16;
  OffModBase*        =  48;
  OffModCode*        =  56;
  OffModInit*        =  64;
  OffModTrap*        =  72;
  OffModKey0*        =  80;
  OffModKey1*        =  88;
  OffModImportNames* =  96;  (* list of import names and keys  *)
  OffModImports*     = 104;  (* adr of start of import list    *)
  OffModExports*     = 112;  (* array of export addresses      *)
  OffModCommands*    = 120;
  ModHdrSize*        = 128;

TYPE
(*
  ModuleName*       = ARRAY 32 OF CHAR;
  ModuleHeader*     = POINTER [untraced] TO ModuleHeaderDesc;
  ModuleHeaderDesc* = RECORD
    length*:      INTEGER;       (*   0                                *)
    next*:        ModuleHeader;  (*   8                                *)
    name*:        ModuleName;    (*  16                                *)
    base*:        INTEGER;       (*  48                                *)
    code*:        INTEGER;       (*  56                                *)
    init*:        INTEGER;       (*  64                                *)
    trap*:        INTEGER;       (*  72                                *)
    key0*, key1*: INTEGER;       (*  80                                *)
    importNames*: INTEGER;       (*  88 list of import names and keys  *)
    imports*:     INTEGER;       (*  96 adr of start of import list    *)
    exports*:     INTEGER;       (* 104 array of export addresses      *)
    commands*:    INTEGER
  END;
*)


VAR
  oneByteBeforeBase: CHAR; (* MUST BE THE FIRST GLOBAL VARIABLE IN Boot.mod *)
                           (* - its address locates this module's base      *)

  BootBase:    INTEGER;  (* Base addres of Boot module - start of initialised data *)
  BootHeader*: INTEGER;  (* Start address of boot module *)
  ModHeader:   INTEGER;
  NextHeader:  INTEGER;
  initialise:  PROCEDURE;

  GetProcAddress*: PROCEDURE(module, procname: INTEGER): INTEGER;
  LoadLibraryA*:   PROCEDURE(filename: INTEGER): INTEGER;
  ExitProcess*:    PROCEDURE(result: INTEGER);
  New*:            PROCEDURE(VAR ptr: INTEGER;  tdAdr: INTEGER);


(* -------------------------------------------------------------------------- *)
(* --------- Link newly loaded module to previously loaded modules ---------- *)
(* -------------------------------------------------------------------------- *)


PROCEDURE GetInt(adr: INTEGER): INTEGER;
VAR res: INTEGER;
BEGIN SYSTEM.GET(adr, res);  RETURN res END GetInt;


PROCEDURE Relocate(adr, offset: INTEGER);
VAR v: INTEGER;
BEGIN
  SYSTEM.GET(adr, v);
  IF v # 0  THEN SYSTEM.PUT(adr, v + offset) END
END Relocate;


PROCEDURE MatchingImports(imp, mod: INTEGER): BOOLEAN;
VAR a, b, impkey, modkey: INTEGER;  ch1, ch2: CHAR;  result: BOOLEAN;
BEGIN
  result := FALSE;
  a := imp;  b := mod + OffModName;
  SYSTEM.GET(a, ch1);  SYSTEM.GET(b, ch2);
  WHILE (ch1 # 0X) & (ch2 # 0X) & (ch1 = ch2) DO
    INC(a);  SYSTEM.GET(a, ch1);
    INC(b);  SYSTEM.GET(b, ch2)
  END;
  IF ch1 = ch2 THEN
    INC(a);
    SYSTEM.GET(a, impkey);  SYSTEM.GET(mod + OffModKey0, modkey);
    IF impkey = modkey THEN
      INC(a, 8);
      SYSTEM.GET(a, impkey);  SYSTEM.GET(mod + OffModKey1, modkey);
      result := impkey = modkey
    END
  END
RETURN result END MatchingImports;

PROCEDURE NextImport(VAR adr: INTEGER);
VAR b: BYTE;
BEGIN
  REPEAT SYSTEM.GET(adr, b);  INC(adr) UNTIL b = 0;
  INC(adr, 16)
END NextImport;

PROCEDURE EndOfImports(adr: INTEGER): BOOLEAN;
VAR b: BYTE;
BEGIN SYSTEM.GET(adr, b)
RETURN b = 0 END EndOfImports;


PROCEDURE Link(headadr: INTEGER);
(* Convert offsets in the Module header to absolute addresses. *)
(* Populate procedure pointers.                                *)
(* Convert export offsets to absolute addresses.               *)
(* Lookup imported modules.                                    *)
(* Convert import references to absolute addresses.            *)
VAR
  baseadr:      INTEGER;
  export:       INTEGER;
  exportadr:    INTEGER;
  i:            INTEGER;
  importAdr:    INTEGER;
  import:       INTEGER;
  imports:      ARRAY 64 OF INTEGER;
  modno:        INTEGER;
  expno:        INTEGER;
  importedval:  INTEGER;
  moduleHdr:    INTEGER;
  importRefAdr: INTEGER;
  importRef:    INTEGER;

BEGIN
  (* Convert module header offsets to absolute addresses *)
  Relocate(headadr + OffModBase,        headadr);
  Relocate(headadr + OffModCode,        headadr);
  Relocate(headadr + OffModInit,        headadr);
  Relocate(headadr + OffModTrap,        headadr);
  Relocate(headadr + OffModImportNames, headadr);
  Relocate(headadr + OffModExports,     headadr);
  Relocate(headadr + OffModCommands,    headadr);

  baseadr := GetInt(headadr + OffModBase);

  SYSTEM.PUT(baseadr + OffExitProcess, GetInt(SYSTEM.ADR(ExitProcess)));
  SYSTEM.PUT(baseadr + OffNew,         GetInt(SYSTEM.ADR(New)));

  IF GetInt(baseadr + OffModulePtrTable) # 0 THEN
    SYSTEM.PUT(baseadr + OffModulePtrTable, GetInt(baseadr + OffModulePtrTable) + baseadr);
  END;

  (* Convert export offsets to absolute *)
  SYSTEM.GET(headadr + OffModExports, export);
  IF export # 0 THEN
    SYSTEM.GET(export, exportadr);
    WHILE exportadr # 8000000000000000H DO
      SYSTEM.PUT(export, exportadr + baseadr);
      INC(export, 8);
      SYSTEM.GET(export, exportadr)
    END
  END;

  (* Convert imported module names to module export table addresses *)
  SYSTEM.GET(headadr + OffModImportNames, importAdr);
  IF importAdr # 0 THEN
    i := 0;
    WHILE ~EndOfImports(importAdr) DO
      imports[i] := 0;
      moduleHdr  := BootHeader;
      WHILE (moduleHdr # 0) & (imports[i] = 0) DO
        IF MatchingImports(importAdr, moduleHdr) THEN
          SYSTEM.GET(moduleHdr + OffModExports, imports[i])
        END;
        SYSTEM.GET(moduleHdr + OffModNext, moduleHdr)
      END;
      INC(i);  NextImport(importAdr)
    END
  END;

  (* Link imports to exports *)
  SYSTEM.GET(headadr + OffModImports, importRefAdr);
  WHILE importRefAdr # 0 DO
    SYSTEM.GET(baseadr + importRefAdr, importRef);
    IF importRef < 0 THEN (* Local relocation *)
      SYSTEM.PUT(baseadr + importRefAdr, baseadr + ASR(importRef, 32) MOD 80000000H)
    ELSE
      modno := ASR(importRef, 48);
      expno := ASR(importRef, 32) MOD 10000H;
      SYSTEM.GET(imports[modno] + expno * 8, importedval);
      SYSTEM.PUT(baseadr + importRefAdr, importedval)
    END;
    importRefAdr := importRef MOD 100000000H;
  END;
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
  BootBase := SYSTEM.ADR(oneByteBeforeBase) + 1;
  SYSTEM.GET(BootBase + 56, BootHeader);
  IF BootHeader # 0 THEN
    BootHeader := BootBase - BootHeader;  (* Start of module image *)

    SYSTEM.PUT(SYSTEM.ADR(GetProcAddress), GetInt(BootHeader - 32));
    SYSTEM.PUT(SYSTEM.ADR(LoadLibraryA),   GetInt(BootHeader - 24));
    SYSTEM.PUT(SYSTEM.ADR(ExitProcess),    GetInt(BootHeader - 16));
    SYSTEM.PUT(BootBase + 16,              GetInt(BootHeader - 16));

    (* The WinBase block includes the offset from the module header to the   *)
    (* WinBase block.                                                        *)

    Link(BootHeader);

    (* Link and initialise remaining modules in EXE 'Oberon' section *)

    ModHeader  := BootHeader;
    NextHeader := ModHeader + GetInt(ModHeader + OffModLength);
    IF GetInt(NextHeader + OffModLength) = 0 THEN NextHeader := 0 END;
    WHILE NextHeader # 0 DO
      SYSTEM.PUT(ModHeader + OffModNext, NextHeader);
      ModHeader := NextHeader;

      Link(ModHeader);
      SYSTEM.PUT(SYSTEM.ADR(initialise), GetInt(ModHeader + OffModInit));
      IF initialise # NIL THEN initialise END;

      NextHeader := ModHeader + GetInt(ModHeader + OffModLength);
      IF GetInt(NextHeader + OffModLength) = 0 THEN NextHeader := 0 END
    END;
    SYSTEM.PUT(ModHeader + OffModNext, 0);

    ExitProcess(0)
  END
END Boot.
