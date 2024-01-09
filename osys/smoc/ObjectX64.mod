MODULE ObjectX64;  (* Write object file *)
IMPORT SYSTEM, Files, B := Base, w := Writer, WritePE, Boot;

VAR
  X64file: Files.File;  (* Output file *)
  X64:     Files.Rider;

  (* Header fields. *) (*: Boot.ModuleHeaderDesc;*)
  HdrLength:      INTEGER;
  HdrBase:        INTEGER;
  HdrCode:        INTEGER;
  HdrInit:        INTEGER;
  HdrTrap:        INTEGER;
  HdrImportNames: INTEGER;
  HdrImports:     INTEGER;
  HdrExports:     INTEGER;
  HdrCommands:    INTEGER;

(* -------------------------------------------------------------------------- *)

PROCEDURE Align(a: INTEGER;  align: INTEGER): INTEGER;
VAR result: INTEGER;
BEGIN
  IF    a > 0 THEN result := (a + align - 1) DIV align * align
  ELSIF a < 0 THEN result :=        a        DIV align * align
  END
RETURN result END Align;


(* -------------------------------------------------------------------------- *)

PROCEDURE AddImport(adr, modno, expno: INTEGER);
BEGIN
  ASSERT((modno >= 0) & (modno < 8000H));
  ASSERT(expno > 0);
  Files.Set(X64, X64file, HdrBase + adr);
  Files.WriteInt(X64, HdrImports + LSL(expno - 1, 32) + LSL(modno, 48));
  HdrImports := adr;
END AddImport;

PROCEDURE WriteImportReferences;
VAR
  impmod:      B.Module;
  import:      B.Ident;
  impobj:      B.Object;
  modno:       INTEGER;
  impadr:      INTEGER;
  expno:       INTEGER;
BEGIN
  HdrImports := 0;
  IF B.modList # NIL THEN
    impmod := B.modList;  modno := 0;
    WHILE impmod # NIL DO
      import := impmod.impList;
      WHILE import # NIL DO
        impobj := import.obj;
        IF    impobj.class = B.cType THEN ASSERT(impobj.type.form = B.tRec);
                                          impadr := impobj.type.adr;
                                          expno  := impobj.type.expno
        ELSIF impobj      IS B.Var   THEN impadr := impobj(B.Var).adr;
                                          expno  := impobj(B.Var).expno
        ELSIF impobj      IS B.Proc  THEN impadr := impobj(B.Proc).adr;
                                          expno  := impobj(B.Proc).expno
        END;
        ASSERT(impadr >= 128);
        ASSERT(expno > 0);  (* Export indices are 1 based for GetProcAddress.      *)
        AddImport(impadr, modno, expno);
        import := import.next
      END;
      impmod := impmod.next;  INC(modno)
    END
  END;
END WriteImportReferences;

(* -------------------------------------------------------------------------- *)

PROCEDURE WriteLiteralStrings;
VAR slist: B.StrList;  str: B.Str;  i: INTEGER;
BEGIN
  slist := B.strList;
  WHILE slist # NIL DO str := slist.obj;
    Files.Set(X64, X64file, HdrBase + str.adr);  i := 0;
    WHILE i < str.len DO
      Files.Write(X64, B.strBuf[str.bufpos+i]);  INC(i)
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

PROCEDURE WriteTypeName(t: B.Type);
BEGIN
  IF (t.obj # NIL) & (t.obj.ident # NIL) THEN
    w.s(t.obj.ident.name)
  ELSE
    w.s("unnamed")
  END
END WriteTypeName;

PROCEDURE DumpType(t: B.Type);
BEGIN
  WriteTypeName(t);
  w.s(": ");  CASE t.form OF
  |B.tInt:   w.s("INTEGER")
  |B.tBool:  w.s("BOOLEAN")
  |B.tSet:   w.s("SET")
  |B.tChar:  w.s("CHAR")
  |B.tReal:  w.s("REAL")
  |B.tPtr:   w.s("POINTER")
  |B.tProc:  w.s("PROCEDURE")
  |B.tArray: w.s("ARRAY")
  |B.tRec:   w.s("RECORD")
  |B.tStr:   w.s("STRING")
  |B.tNil:   w.s("NIL")
  END;
  IF t.obj # NIL THEN
    w.s(", object class ");  CASE t.obj.class OF
    |B.cNull:   w.s("Null")
    |B.cModule: w.s("Module")
    |B.cType:   w.s("Type")
    |B.cNode:   w.s("Node")
    |B.cVar:    w.s("Var")
    |B.cConst:  w.s("Const")
    |B.cProc:   w.s("Proc")
    |B.cField:  w.s("Field")
    |B.cSProc:  w.s("SProc")
    |B.cSFunc:  w.s("SFunc")
    END
  END;
  w.s(", adr $");  w.h(t.adr);
  w.s(", expno "); w.i(t.expno);
  w.s(", ref ");   w.i(t.ref);
  w.s(", len ");   w.i(t.len);
  w.s(", size ");  w.i(t.size);
  w.s(", size0 "); w.i(t.size0);
  IF t.mod # NIL THEN
    w.s(" - in module "); w.s(t.mod.id); w.s(", no. "); w.i(t.mod.no)
  END;
  w.sl(".");
  IF t.base # NIL THEN w.s("      extension of "); DumpType(t.base) END
END DumpType;

PROCEDURE DumpRecList;
VAR reclist: B.TypeList;  type: B.Type;  i: INTEGER;
BEGIN reclist := B.recList;  i := 0;
  IF reclist # NIL THEN
    w.l; w.sl("reclist:");
    WHILE reclist # NIL DO
      type := reclist.type;
      w.s("  ["); w.i(i); w.s("] ");
      DumpType(type);
      reclist := reclist.next;  INC(i)
    END
  END
END DumpRecList;


PROCEDURE AddRelocation(adr, val: INTEGER);
BEGIN
  (*w.s("Add relocation at $"); w.h(adr); w.s(" value $"); w.h(val); w.sl(".");*)
  Files.Set(X64, X64file, HdrBase + adr);
  Files.WriteInt(X64, 8000000000000000H + LSL(val, 32) + HdrImports);
  HdrImports := adr;
END AddRelocation;


PROCEDURE GetModNo(mod: B.Module): INTEGER;
VAR modno: INTEGER;  m: B.Module;
BEGIN modno := 0;  m := B.modList;
  WHILE (m # NIL) & (m # mod) DO INC(modno); m := m.next END
RETURN modno END GetModNo;


PROCEDURE WriteRecordPointerTables;
VAR recType: B.TypeList;  type: B.Type;  typeadr: INTEGER;
BEGIN
  (*DumpRecList;*)
  recType := B.recList;
  WHILE recType # NIL DO
    type := recType.type;  typeadr := type.adr;
    Files.Set(X64, X64file, HdrBase + typeadr);
    Files.WriteInt(X64, type.size);

    (* Write extensions for extended record types *)
    WHILE type.len >= 1 DO
      IF type.mod = NIL THEN (* local *)
        (*
        w.s("Type extension ptr, typeadr $"); w.h(typeadr);
        w.s(", type.len ");                   w.i(type.len);
        w.s(", ext type adr $");              w.h(type.adr);
        w.s(", HdrBase $");               w.h(HdrBase);
        w.sl(".");
        *)
        AddRelocation(typeadr + type.len*8, type.adr)
      ELSE (* import *)
        AddImport(typeadr + type.len*8, GetModNo(type.mod), type.expno)
      END;
      type := type.base
    END;

    Files.Set(X64, X64file, HdrBase + typeadr + 8 + B.MaxExt*8);
    IF type.nTraced > 0 THEN Write_pointer_offset(0, recType.type) END;
    Files.WriteInt(X64, -1);

    recType := recType.next
  END;

END WriteRecordPointerTables;

(* -------------------------------------------------------------------------- *)

PROCEDURE WriteModulePointerTable(tableOffset: INTEGER);
VAR ident: B.Ident;  obj: B.Object;  adr: INTEGER;
BEGIN
  Files.Set(X64, X64file, HdrBase + tableOffset);
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
    Files.Set(X64, X64file, HdrBase + x.descAdr);
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
  impmod: B.Module;
  modno:  INTEGER;
BEGIN
  Files.Set(X64, X64file, HdrImportNames);
  impmod := B.modList;  modno := 0;
  WHILE impmod # NIL DO
    Files.WriteString(X64, impmod.id);
    Files.WriteInt(X64, impmod.key[0]);
    Files.WriteInt(X64, impmod.key[1]);
    impmod := impmod.next;
  END;
  Files.WriteByte(X64, 0)
END WriteImportNames;

(* -------------------------------------------------------------------------- *)

PROCEDURE WriteCommands;
VAR
  export: B.ObjList;
  proc:   B.Proc;
BEGIN
  Files.Set(X64, X64file, HdrCommands);
  export := B.expList;
  WHILE export # NIL DO
    IF export.obj IS B.Proc THEN
      proc := export.obj(B.Proc);
      IF (proc.type.nfpar = 0) & (proc.return = NIL) THEN  (* No parameters, no result *)
        Files.WriteString(X64, proc.ident.name);
        Files.WriteInt(X64, proc.adr + HdrCode - HdrBase)
      END
    END;
    export := export.next
  END;
  Files.Write(X64, 0X)
END WriteCommands;

(* -------------------------------------------------------------------------- *)

PROCEDURE WriteExports;
VAR
  export: B.ObjList;
  adr:    INTEGER;
BEGIN
  Files.Set(X64, X64file, HdrExports);

  export := B.expList;
  WHILE export # NIL DO
    IF export.obj.class = B.cType THEN adr := export.obj.type.adr;
    ELSIF export.obj IS B.Var     THEN adr := export.obj(B.Var).adr;
    ELSIF export.obj IS B.Proc    THEN adr := export.obj(B.Proc).adr
                                            + HdrCode - HdrBase
    END;
    Files.WriteInt(X64, adr);
    export := export.next
  END;
  Files.WriteInt(X64, 8000000000000000H)
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
  filename := B.BuildPath;
  B.Append(B.Modid, filename);
  B.Append(".x64",  filename);
  X64file := Files.New(filename);
  WritePE.AddObject(filename);

  HdrBase := Align(Boot.ModHdrSize, 16) + Align(varSize, 16);

  (* Insert pointers into the first 128 bytes of static data. *)

  Files.Set(X64, X64file, HdrBase + Boot.OffModHeaderOffset);
  Files.WriteInt(X64, HdrBase);

  Files.Set(X64, X64file, HdrBase + Boot.OffModulePtrTable);
  Files.WriteInt(X64, modPtrTable);

  WriteImportReferences;
  WriteLiteralStrings;
  WriteRecordPointerTables;
  WriteModulePointerTable(modPtrTable);
  WriteStackFramePointerTables;

  HdrCode := HdrBase + Align(staticSize, 16);
  HdrInit := HdrCode + initProc;
  Files.Set(X64, X64file, HdrCode);
  Files.WriteBytes(X64, code, codesize);

  HdrTrap := Align(HdrCode + codesize, 16);

  (* HdrTrap table *)
  Files.Set(r, debug, 0);
  Files.Set(X64, X64file, HdrTrap);
  WHILE ~r.eof DO
    Files.ReadBytes(r, buffer, LEN(buffer));
    readlen := LEN(buffer) - r.res;
    Files.WriteBytes(X64, buffer, readlen)
  END;
  Files.WriteInt(X64, -1);

  (* Module name *)
  (*
  Header.name := B.Modid;
  Header.key0 := B.modkey[0];  Header.key1 := B.modkey[1];
  *)

  (* Import names *)
  IF HdrImports = 0 THEN
    HdrImportNames := 0
  ELSE
    HdrImportNames := Align(Files.Pos(X64), 16);
    WriteImportNames
  END;

  (* Exports *)
  IF B.expList = NIL THEN
    HdrExports := 0
  ELSE
    HdrExports := Align(Files.Pos(X64), 16);
    WriteExports
  END;

  HdrCommands := Align(Files.Pos(X64), 16);
  WriteCommands;

  (* Round file length to a mutiple of 16 bytes *)
  IF Files.Pos(X64) MOD 16 # 0 THEN
    Files.Set(X64, X64file, Align(Files.Pos(X64), 16)-1);
    Files.WriteByte(X64,0)
  END;

  HdrLength := Align(Files.Pos(X64), 16);
  (*Header.next   := NIL;*)

  (* Write the header *)
  Files.Set(X64, X64file, 0);
  (*Files.WriteBytes(X64, Header, SYSTEM.SIZE(Boot.ModuleHeaderDesc));*)

  Files.WriteInt  (X64, HdrLength);
  Files.WriteInt  (X64, 0);            (* next *)
  Files.WriteBytes(X64, B.Modid, 32);  (* name *)
  Files.WriteInt  (X64, HdrBase);
  Files.WriteInt  (X64, HdrCode);
  Files.WriteInt  (X64, HdrInit);
  Files.WriteInt  (X64, HdrTrap);
  Files.WriteInt  (X64, B.modkey[0]);
  Files.WriteInt  (X64, B.modkey[1]);
  Files.WriteInt  (X64, HdrImportNames);
  Files.WriteInt  (X64, HdrImports);
  Files.WriteInt  (X64, HdrExports);
  Files.WriteInt  (X64, HdrCommands);

  Files.Register(X64file)
END Write;

BEGIN (* w.sl("ObjectX64 loaded.") *)
END ObjectX64.