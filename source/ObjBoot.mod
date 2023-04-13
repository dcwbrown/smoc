MODULE ObjBoot;  (*$OBJECT*)

IMPORT SYSTEM;

TYPE
  ModuleHeader = POINTER [untraced] TO ModuleHeaderDesc;
  ModuleHeaderDesc = RECORD
    length:      INTEGER;      (*  0                                          *)
    next:        ModuleHeader; (*  8                                          *)
    base:        INTEGER;      (* 16                                          *)
    code:        INTEGER;      (* 24                                          *)
    init:        INTEGER;      (* 32                                          *)
    trap:        INTEGER;      (* 40                                          *)
    name:        INTEGER;      (* 48 offset of sz module name string          *)
    key0, key1:  INTEGER;      (* 56                                          *)
    imports:     INTEGER;      (* 72 offset of list of import names and keys  *)
    importCount: INTEGER;      (* 80 number of imports at base+128            *)
    exports:     INTEGER       (* 88 offset of array of export addresses      *)
  END;

  Win32Imports = POINTER TO Win32ImportsDesc;
  Win32ImportsDesc = RECORD
    GetProcAddress: PROCEDURE(module, procname: INTEGER): INTEGER;
    LoadLibraryA:   PROCEDURE(filename: INTEGER): INTEGER;
    ExitProcess:    PROCEDURE(result: INTEGER);
    New:            PROCEDURE()
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
  FirstHeader: ModuleHeader;
  CurHeader:   ModuleHeader;
  NextHeader:  ModuleHeader;
  imports:     Win32Imports;
  pointers:    ModulePointers;
  initCode:    INTEGER;
  adr:         INTEGER;


  MessageBoxA: PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER);
  Initialise:  PROCEDURE;

(* -------------------------------------------------------------------------- *)

PROCEDURE Msg(str: ARRAY OF CHAR);
VAR user: INTEGER;  MessageBoxAdr: INTEGER;
BEGIN
  IF MessageBoxA = NIL THEN
    user          := imports.LoadLibraryA(SYSTEM.ADR("user32.dll"));
    MessageBoxAdr := imports.GetProcAddress(user, SYSTEM.ADR("MessageBoxA"));
    SYSTEM.PUT(SYSTEM.ADR(MessageBoxA), MessageBoxAdr);
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

PROCEDURE Connect(header: ModuleHeader; wimports: Win32Imports);
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
    GetString(import, impname);  i := 0;  imports[i] := 0;
    WHILE impname[0] # 0X DO
      GetInteger(import, impkey0);  GetInteger(import, impkey1);
      impheader := FirstHeader;
      WHILE (impheader # NIL) & (imports[i] = 0) DO
        impadr := impheader.name;  GetString(impadr, hdrname);
        IF (impname = hdrname) & (impkey0 = impheader.key0) & (impkey1 = impheader.key1) THEN
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
  END;

  (* Set header next pointer to next header, if any. *)
  header.next := SYSTEM.VAL(ModuleHeader, header.length + SYSTEM.ADR(header^));
  IF header.next.length =  0 THEN header.next := NIL END;
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
  FirstHeader := SYSTEM.VAL(ModuleHeader, SYSTEM.ADR(pointers^) - pointers.ModHdrOffset);

  (* A minimal set of Win32 function addresses sits just before the first     *)
  (* module header.                                                           *)
  imports := SYSTEM.VAL(Win32Imports, SYSTEM.ADR(FirstHeader^) - SYSTEM.SIZE(Win32ImportsDesc));

  CurHeader := FirstHeader;
  WHILE CurHeader # NIL DO
    Connect(CurHeader, imports);

    IF CurHeader.init # 0 THEN
      SYSTEM.PUT(SYSTEM.ADR(Initialise), CurHeader.init);
      Initialise
    END;

    CurHeader := CurHeader.next
  END;


  imports.ExitProcess(4);
END ObjBoot.
