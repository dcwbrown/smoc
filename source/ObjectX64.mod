MODULE ObjectX64;  (* Write object file *)
IMPORT SYSTEM, Files, B := Base, w := Writer;

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
    length*:     INTEGER;          (*   0                                *)
    next*:       ModuleHeader;     (*   8                                *)
    name:        ARRAY 32 OF CHAR; (*  16                                *)
    base:        INTEGER;          (*  48                                *)
    code*:       INTEGER;          (*  56                                *)
    init:        INTEGER;          (*  64                                *)
    trap*:       INTEGER;          (*  72                                *)
    key0, key1:  INTEGER;          (*  80                                *)
    imports:     INTEGER;          (*  88 list of import names and keys  *)
    importCount: INTEGER;          (*  96 number of imports at base+128  *)
    exports:     INTEGER           (* 104 array of export addresses      *)
  END;

  ModulePointers = POINTER TO ModulePointersDesc;
  ModulePointersDesc = RECORD
    (*   0 *) GetProcAddress: PROCEDURE(module, procname: INTEGER): INTEGER;
    (*   8 *) LoadLibraryA:   PROCEDURE(filename: INTEGER): INTEGER;
    (*  16 *) ExitProcess:    PROCEDURE(result: INTEGER);
    (*  24 *) z1, z2, z3, z4: INTEGER;
    (*  56 *) z5, z6, z7, z8: INTEGER;
    (*  88 *) z9:             INTEGER;
    (*  96 *) ModHdr:   INTEGER;
    (* 104 *) StackPtrTable:  INTEGER;
    (* 112 *) ModulePtrTable: INTEGER;
    (* 120 *) New:            PROCEDURE()
  END;

VAR
  X64file: Files.File;  (* Output file *)
  X64:     Files.Rider;
  Header:  ModuleHeaderDesc;

(* -------------------------------------------------------------------------- *)

PROCEDURE Align(a: INTEGER;  align: INTEGER): INTEGER;
VAR result: INTEGER;
BEGIN
  IF    a > 0 THEN result := (a + align - 1) DIV align * align
  ELSIF a < 0 THEN result :=        a        DIV align * align
  END
RETURN result END Align;


(* -------------------------------------------------------------------------- *)

PROCEDURE WriteImportReferences;
VAR
  impmod:          B.Module;
  import:          B.Ident;
  impobj:          B.Object;
  importCount:     INTEGER;
  modno:           INTEGER;
  impadr:          INTEGER;
  expno:           INTEGER;
BEGIN
  impmod := B.modList;  modno := 0;  importCount := 0;
  WHILE impmod # NIL DO
    import := impmod.impList;
    WHILE import # NIL DO
      impobj := import.obj;
      IF impobj.class = B.cType THEN ASSERT(impobj.type.form = B.tRec);
                                  impadr := impobj.type.adr;    expno := impobj.type.expno
      ELSIF impobj IS B.Var  THEN impadr := impobj(B.Var).adr;  expno := impobj(B.Var).expno
      ELSIF impobj IS B.Proc THEN impadr := impobj(B.Proc).adr; expno := impobj(B.Proc).expno
      END;
      ASSERT(impadr >= 128);
      ASSERT(expno > 0);  (* Export indices are 1 based for GetProcAddress.      *)
      Files.Set(X64, X64file, Header.base + impadr);
      Files.WriteInt(X64, modno * `100000000H + expno-1);
      INC(importCount);
      import := import.next
    END;
    impmod := impmod.next;  INC(modno)
  END;
  Header.importCount := importCount
END WriteImportReferences;

(* -------------------------------------------------------------------------- *)

PROCEDURE WriteLiteralStrings;
VAR slist: B.StrList;  str: B.Str;  i: INTEGER;
BEGIN
  slist := B.strList;
  WHILE slist # NIL DO str := slist.obj;
    Files.Set(X64, X64file, Header.base + str.adr);  i := 0;
    WHILE i < str.len DO
      Files.WriteChar(X64, B.strBuf[str.bufpos+i]);  INC(i)
    END;
    slist := slist.next
  END
END WriteLiteralStrings;

(* -------------------------------------------------------------------------- *)

PROCEDURE Write_pointer_offset(offset: INTEGER;  type: B.Type);
VAR ident: B.Ident;       field: B.Field;
    size, k, n: INTEGER;  base:  B.Type;
BEGIN
  IF type.form = B.tRec THEN
    IF (type.base # NIL) & (type.base.nTraced > 0) THEN
      Write_pointer_offset(offset, type.base)
    END;
    ident := type.fields;
    WHILE ident # NIL DO field := ident.obj(B.Field);
      IF field.type.nTraced > 0 THEN
        n := offset + field.off;
        IF field.type.form = B.tPtr THEN
          Files.WriteInt(X64, n)
        ELSE
          Write_pointer_offset(n, field.type)
        END
      END;
      ident := ident.next
    END
  ELSIF type.form = B.tArray THEN
    base := type.base;  k := 0;
    IF base.form = B.tPtr THEN n := offset;
      WHILE k < type.len DO
        Files.WriteInt(X64, n);  INC(k);  INC(n, 8)
      END
    ELSE size := base.size;
      WHILE k < type.len DO
        Write_pointer_offset(offset, base);
        INC(k);  INC(offset, size)
      END
    END
  ELSE ASSERT(FALSE)
  END
END Write_pointer_offset;


PROCEDURE WriteRecordPointerTables;
VAR typelist: B.TypeList;
BEGIN
  typelist := B.recList;
  WHILE typelist # NIL DO
    Files.Set(X64, X64file, Header.base + typelist.type.adr);
    Files.WriteInt(X64, typelist.type.size);

    Files.Set(X64, X64file, Header.base + typelist.type.adr + 8 + B.MaxExt*8);
    IF typelist.type.nTraced > 0 THEN Write_pointer_offset(0, typelist.type) END;
    Files.WriteInt(X64, -1);

    typelist := typelist.next
  END;

END WriteRecordPointerTables;

(* -------------------------------------------------------------------------- *)

PROCEDURE WriteModulePointerTable(tableOffset: INTEGER);
VAR ident: B.Ident;  obj: B.Object;  adr: INTEGER;
BEGIN
  Files.Set(X64, X64file, Header.base + tableOffset);
  ident := B.universe.first;
  WHILE ident # NIL DO obj := ident.obj;
    IF (obj IS B.Var) & ~(obj IS B.Str) THEN
      IF obj.type.nTraced > 0 THEN adr := obj(B.Var).adr;
        IF obj.type.form = B.tPtr THEN Files.WriteInt(X64, adr)
        ELSE Write_pointer_offset(adr, obj.type)
        END
      END
    END;
    ident := ident.next
  END;
  Files.WriteInt(X64, -1);
END WriteModulePointerTable;

(* -------------------------------------------------------------------------- *)

PROCEDURE Write_proc_pointer_offset(ident: B.Ident);
VAR x: B.Proc;

  PROCEDURE Write(x: B.Proc);
  VAR ident: B.Ident;  adr: INTEGER;  y: B.Object;
  BEGIN
    Files.Set(X64, X64file, Header.base + x.descAdr);
    ident := x.decl;
    WHILE ident # NIL DO y := ident.obj;
      IF (y IS B.Var) & ~(y IS B.Str) & ~(y IS B.Par)
      OR (y IS B.Par) & ~y(B.Par).varpar & (y.type.size = 8) THEN
        IF y.type.nTraced > 0 THEN adr := y(B.Var).adr;
          IF y.type.form = B.tPtr THEN Files.WriteInt(X64, adr)
          ELSE Write_pointer_offset(adr, y.type)
          END
        END
      END;
      ident := ident.next
    END;
    Files.WriteInt(X64, -1)
  END Write;

BEGIN (* Write_proc_pointer_offset *)
  WHILE ident # NIL DO
    IF ident.obj IS B.Proc THEN
      x := ident.obj(B.Proc);
      Write_proc_pointer_offset(x.decl);
      IF x.nTraced > 0 THEN Write(x) END
    END;
    ident := ident.next
  END
END Write_proc_pointer_offset;


PROCEDURE WriteStackFramePointerTables;
BEGIN Write_proc_pointer_offset(B.universe.first);
END WriteStackFramePointerTables;

(* -------------------------------------------------------------------------- *)

PROCEDURE WriteImportNames;
VAR
  impmod:      B.Module;
  modno:       INTEGER;
BEGIN
  Files.Set(X64, X64file, Header.imports);
  impmod := B.modList;  modno := 0;
  WHILE impmod # NIL DO
    Files.WriteString(X64, impmod.id);
    Files.WriteInt(X64, impmod.key[0]);
    Files.WriteInt(X64, impmod.key[1]);
    impmod := impmod.next;
  END;
  Files.Write(X64, 0)
END WriteImportNames;

(* -------------------------------------------------------------------------- *)

PROCEDURE WriteExports;
VAR
  export: B.ObjList;
  adr:    INTEGER;
BEGIN
  Files.Set(X64, X64file, Header.exports);

  export := B.expList;
  WHILE export # NIL DO
    IF export.obj.class = B.cType THEN adr := export.obj.type.adr;
    ELSIF export.obj IS B.Var     THEN adr := export.obj(B.Var).adr;
    ELSIF export.obj IS B.Proc    THEN adr := export.obj(B.Proc).adr
                                            + Header.code - Header.base;
    END;
    Files.WriteInt(X64, adr);
    export := export.next
  END;
  Files.WriteInt(X64, 0)
END WriteExports;

(* -------------------------------------------------------------------------- *)

PROCEDURE Write*(debug:                Files.File;
                 code:                 ARRAY OF BYTE;
                 codesize,   initProc: INTEGER;
                 staticSize, varSize:  INTEGER;
                 modPtrTable:          INTEGER);
VAR
  filename: ARRAY 256 OF CHAR;
  r:        Files.Rider;
  readlen:  INTEGER;
  buffer:   ARRAY 200H OF BYTE;
  impmod:   B.Module;
  export:   B.ObjList;
  i, adr:   INTEGER;
BEGIN
  filename := B.Modid;  B.Append(".x64", filename);
  X64file := Files.New(filename);

  Header.base := Align(SYSTEM.SIZE(ModuleHeaderDesc), 16) + Align(varSize, 16);

  (* Insert pointers into the first 128 bytes of static data. *)
  Files.Set(X64, X64file, Header.base + 96);  Files.WriteInt(X64, Header.base);
  Files.Set(X64, X64file, Header.base + 112); Files.WriteInt(X64, modPtrTable);

  WriteImportReferences;
  WriteLiteralStrings;
  WriteRecordPointerTables;
  WriteModulePointerTable(modPtrTable);
  WriteStackFramePointerTables;

  Header.code := Header.base + Align(staticSize, 16);
  Header.init := Header.code + initProc;
  Files.Set(X64, X64file, Header.code);
  Files.WriteBytes(X64, code, codesize);

  Header.trap := Align(Header.code + codesize, 16);

  (* Trap table *)
  Files.Set(r, debug, 0);
  Files.Set(X64, X64file, Header.trap);
  WHILE ~r.eof DO
    Files.ReadBytes(r, buffer, LEN(buffer));
    readlen := LEN(buffer) - r.res;
    Files.WriteBytes(X64, buffer, readlen)
  END;
  Files.WriteInt(X64, -1);

  (* Module name *)
  Header.name := B.Modid;
  Header.key0 := B.modkey[0];  Header.key1 := B.modkey[1];

  (* Import names *)
  IF Header.importCount = 0 THEN
    Header.imports := 0
  ELSE
    Header.imports := Align(Files.Pos(X64), 16);
    WriteImportNames
  END;

  (* Exports *)
  IF B.expList = NIL THEN
    Header.exports := 0
  ELSE
    Header.exports := Align(Files.Pos(X64), 16);
    WriteExports
  END;

  (* Round file length to a mutiple of 16 bytes *)
  IF Files.Pos(X64) MOD 16 # 0 THEN
    Files.Set(X64, X64file, Align(Files.Pos(X64), 16)-1);
    Files.Write(X64,0)
  END;

  Header.length := Align(Files.Pos(X64), 16);
  Header.next   := NIL;

  (* Write the header *)
  Files.Set(X64, X64file, 0);
  Files.WriteBytes(X64, Header, SYSTEM.SIZE(ModuleHeaderDesc));

  Files.Register(X64file)
END Write;

END ObjectX64.