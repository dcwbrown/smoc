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

TYPE
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


VAR
  oneByteBeforeBase: CHAR; (* MUST BE THE FIRST GLOBAL VARIABLE        *)
                           (* - its address locates this module's base *)

  BootBase:   INTEGER;  (* Base addres of Boot module - start of initialised data *)
  BootHeader: INTEGER;  (* Start address of boot module *)

  GetProcAddress*: PROCEDURE(module, procname: INTEGER): INTEGER;
  LoadLibraryA*:   PROCEDURE(filename: INTEGER): INTEGER;
  ExitProcess*:    PROCEDURE(result: INTEGER);
  New*:            PROCEDURE(VAR ptr: INTEGER;  tdAdr: INTEGER);

  FirstModule*: ModuleHeader;

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


PROCEDURE GetInt(adr: INTEGER): INTEGER;
VAR res: INTEGER;
BEGIN SYSTEM.GET(adr, res);  RETURN res END GetInt;


PROCEDURE Relocate(adr, offset: INTEGER);
VAR v: INTEGER;
BEGIN
  SYSTEM.GET(adr, v);
  IF v # 0  THEN SYSTEM.PUT(adr, v + offset) END
END Relocate;


PROCEDURE Link(header: ModuleHeader);
(* Convert offsets in the Module header to absolute addresses. *)
(* Populate procedure pointers.                                *)
(* Convert export offsets to absolute addresses.               *)
(* Lookup imported modules.                                    *)
(* Convert import references to absolute addresses.            *)
VAR
  headadr:      INTEGER;
  baseadr:      INTEGER;
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
  headadr := SYSTEM.ADR(header^);

  (* Convert module header offsets to absolute addresses *)
  Relocate(headadr + OffModBase,        headadr);
  Relocate(headadr + OffModCode,        headadr);
  Relocate(headadr + OffModInit,        headadr);
  Relocate(headadr + OffModTrap,        headadr);
  Relocate(headadr + OffModImportNames, headadr);
  Relocate(headadr + OffModExports,     headadr);
  Relocate(headadr + OffModCommands,    headadr);

  baseadr := GetInt(headadr + OffModBase);

  SYSTEM.PUT(baseadr + OffExitProcess, SYSTEM.VAL(INTEGER, ExitProcess));
  SYSTEM.PUT(baseadr + OffNew,         SYSTEM.VAL(INTEGER, New));

  IF GetInt(baseadr + OffModulePtrTable) # 0 THEN
    SYSTEM.PUT(baseadr + OffModulePtrTable, GetInt(baseadr + OffModulePtrTable) + baseadr);
  END;

  (* Convert export offsets to absolute *)
  IF header.exports # 0 THEN
    SYSTEM.GET(headadr + OffModExports, export);
    SYSTEM.GET(export, exportadr);
    WHILE exportadr # 8000000000000000H DO
      SYSTEM.PUT(export, exportadr + header.base);
      INC(export, 8);
      SYSTEM.GET(export, exportadr)
    END
  END;

  (* Convert imported module names to module export table addresses *)
  SYSTEM.GET(headadr + OffModImportNames, importAdr);
  IF importAdr # 0 THEN
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
    IF importRef < 0 THEN (* Local relocation *)
      SYSTEM.PUT(header.base + importRefAdr, header.base + ASR(importRef, 32) MOD 80000000H)
    ELSE
      modno := ASR(importRef, 48);
      expno := ASR(importRef, 32) MOD 10000H;
      SYSTEM.GET(imports[modno] + expno * 8, impadr);
      SYSTEM.PUT(header.base + importRefAdr, impadr)
    END;
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
  BootBase := SYSTEM.ADR(oneByteBeforeBase) + 1;
  SYSTEM.GET(BootBase + 56, BootHeader);
  IF BootHeader # 0 THEN
    BootHeader := BootBase - BootHeader;  (* Start of module image *)

    SYSTEM.PUT(SYSTEM.ADR(GetProcAddress), GetInt(BootHeader - 32));
    SYSTEM.PUT(SYSTEM.ADR(LoadLibraryA),   GetInt(BootHeader - 24));
    SYSTEM.PUT(SYSTEM.ADR(ExitProcess),    GetInt(BootHeader - 16));
    SYSTEM.PUT(BootBase + 16,              GetInt(BootHeader - 16));

    (*LoadPointers;*)

    (* The WinBase block includes the offset from the module header to the   *)
    (* WinBase block.                                                        *)
    WinHdr := SYSTEM.VAL(ModuleHeader, BootHeader);

    (* Link this module - the Windows PE bootstrap *)
    FirstModule := WinHdr;
    Link(WinHdr);  (* Note - do not call WinHdr's init address as that is *)
                   (* this code and we're already running.                *)

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

    ExitProcess(0)
  END
END Boot.
