MODULE Bootstrapper;  (*$OBJECT*)

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

  StdProcs = POINTER TO StdProcsDesc;
  StdProcsDesc = RECORD
    GetProcAddress: PROCEDURE(module, procname: INTEGER): INTEGER;
    LoadLibraryA:   PROCEDURE(filename: INTEGER): INTEGER;
    ExitProcess:    PROCEDURE(result: INTEGER);
    zeroterminator: INTEGER
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

  User*:        INTEGER;
  Kernel*:      INTEGER;

  FirstModule*: ModuleHeader;
  CurModule:    ModuleHeader;
  NextModule:   ModuleHeader;
  stdProcs:     StdProcs;
  pointers:     ModulePointers;
  initCode:     INTEGER;
  adr:          INTEGER;

  MessageBoxA:  PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER);
  Initialise:   PROCEDURE;
  New:          PROCEDURE;

(* -------------------------------------------------------------------------- *)

PROCEDURE Msg*(str: ARRAY OF CHAR);
VAR user: INTEGER;  adr: INTEGER;
BEGIN
  IF MessageBoxA = NIL THEN
    adr := stdProcs.GetProcAddress(User, SYSTEM.ADR("MessageBoxA"));
    SYSTEM.PUT(SYSTEM.ADR(MessageBoxA), adr);
  END;
  MessageBoxA(0, SYSTEM.ADR(str), SYSTEM.ADR("Bootstrapper"), 0);
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
BEGIN stdProcs.ExitProcess(returnCode) END Halt;

PROCEDURE Abort(str: ARRAY OF CHAR);
BEGIN Msg(str);  Halt(9) END Abort;

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

PROCEDURE Connect(header: ModuleHeader);
(* Convert offsets in the Module header to absolute addresses. *)
(* Populate procedure pointers.                                *)
(* Convert export offsets to absolute addresses.               *)
(* Lookup imported modules.                                    *)
(* Convert import references to absolute addresses.            *)
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
  pointers := SYSTEM.VAL(ModulePointers, header.base);
  pointers.GetProcAddress := stdProcs.GetProcAddress;
  pointers.LoadLibraryA   := stdProcs.LoadLibraryA;
  pointers.ExitProcess    := stdProcs.ExitProcess;
  pointers.New            := New;

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
    GetString(import, impname);  i := 0;  imports[i] := 0;
    WHILE impname[0] # 0X DO
      GetInteger(import, impkey0);  GetInteger(import, impkey1);
      impheader := FirstModule;
      WHILE (impheader # NIL) & (imports[i] = 0) DO
        IF (impname = impheader.name) & (impkey0 = impheader.key0) & (impkey1 = impheader.key1) THEN
          imports[i] := impheader.exports
        END;
        impheader := impheader.next
      END;
      GetString(import, impname);  INC(i)
    END
  END;

  (* Connect imports to exports *)
  FOR i := 0 TO header.importCount-1 DO
    SYSTEM.GET(header.base + 128 + i*8, expno);
    modno := expno DIV 100000000H;  expno := expno MOD 100000000H;
    SYSTEM.GET(imports[modno] + expno * 8, impadr);
    SYSTEM.PUT(header.base + 128 + i*8, impadr)
  END

END Connect;

(* -------------------------------------------------------------------------- *)


BEGIN
  (* Initialisation code for the first module - this is the first code that   *)
  (* runs when the PE is loaded. It runs before it has been linked in and it  *)
  (* is its responsibility to link (connect) in both itself and all modules   *)
  (* that follow in the PE 'Oberon' section.                                  *)

  (* The 128 byte pointers block is at the start of static data and is the    *)
  (* base address used within the module code.                                *)
  pointers := SYSTEM.VAL(ModulePointers, SYSTEM.ADR(oneByteBeforeBase) + 1);

  (* The pointers block includes the offset from the module header to the     *)
  (* pointers block.                                                          *)
  FirstModule := SYSTEM.VAL(ModuleHeader, SYSTEM.ADR(pointers^) - pointers.ModHdrOffset);

  (* A minimal set of Win32 function addresses sits just before the first     *)
  (* module header.                                                           *)
  stdProcs := SYSTEM.VAL(StdProcs, SYSTEM.ADR(FirstModule^) - SYSTEM.SIZE(StdProcsDesc));

  (* Set up some useful exports from standars procedures. *)
  Kernel := stdProcs.LoadLibraryA(SYSTEM.ADR("kernel32.dll"));
  User   := stdProcs.LoadLibraryA(SYSTEM.ADR("user32.dll"));

  CurModule := FirstModule;
  WHILE CurModule # NIL DO
    Connect(CurModule);

    IF CurModule.init # 0 THEN
      CurModule.next := NIL;
      SYSTEM.PUT(SYSTEM.ADR(Initialise), CurModule.init);
      Initialise
    END;

    (* Set header next pointer to next header, if any. *)
    NextModule := SYSTEM.VAL(ModuleHeader, CurModule.length + SYSTEM.ADR(CurModule^));
    IF NextModule.length = 0 THEN NextModule := NIL END;

    CurModule.next := NextModule;
    CurModule      := NextModule
  END;

  Halt(4);
END Bootstrapper.
