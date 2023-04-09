MODULE Object;  (* Write object file *)
IMPORT SYSTEM, Files, B := Base, w := Writer;

VAR
  X64: Files.Rider;  (* Output file *)

  ImportCount: INTEGER;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Align(a: INTEGER;  align: INTEGER): INTEGER;
VAR result: INTEGER;
BEGIN
  IF    a > 0 THEN result := (a + align - 1) DIV align * align
  ELSIF a < 0 THEN result :=        a        DIV align * align
  END
RETURN result END Align;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Initialized globals *)

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


PROCEDURE Write_proc_pointer_offset(baseFPos: INTEGER; ident: B.Ident);
VAR x: B.Proc;

  PROCEDURE Write(baseFPos: INTEGER; x: B.Proc);
  VAR ident: B.Ident;  adr: INTEGER;  y: B.Object;
  BEGIN
    Files.Set(X64, Files.Base(X64), baseFPos + x.descAdr);
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
      Write_proc_pointer_offset(baseFPos, x.decl);
      IF x.nTraced > 0 THEN Write(baseFPos, x) END
    END;
    ident := ident.next
  END
END Write_proc_pointer_offset;


PROCEDURE WriteStaticData(staticSize: INTEGER);
VAR
  baseFPos: INTEGER;
  globpos:  INTEGER;
  i, adr:   INTEGER;
  b:        BYTE;
  imod:     B.Module;
  ident:    B.Ident;
  t:        B.TypeList;
  obj:      B.Object;
  str:      ARRAY 512 OF CHAR;
  slist8:   B.StrList;   y: B.Str;
  impmod:   B.Module;
  impobj:   B.Object;
  import:   B.Ident;
  modno:    INTEGER;
  impadr:   INTEGER;
  expno:    INTEGER;
BEGIN
  (* Write initialised data *)
  baseFPos := Files.Pos(X64);

  (* Write the import references into static data where they will be converted *)
  (* to absolute addresses by the loader.                                      *)
  impmod := B.modList;  modno := 0;  ImportCount := 0;
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
      Files.Set(X64, Files.Base(X64), baseFPos + impadr);
      Files.WriteInt(X64, modno * 100000000H + expno-1);
      INC(ImportCount);
      import := import.next
    END;
    impmod := impmod.next;  INC(modno)
  END;

  (* Write literal strings *)
  slist8 := B.strList;
  WHILE slist8 # NIL DO y := slist8.obj;
    ASSERT(y.adr < staticSize);
    Files.Set(X64, Files.Base(X64), baseFPos + y.adr);  i := 0;
    WHILE i < y.len DO
      Files.WriteChar(X64, B.strBuf[y.bufpos+i]);  INC(i)
    END;
    slist8 := slist8.next
  END;

  (* Write record type pointer tables *)
  t := B.recList;
  WHILE t # NIL DO
    ASSERT(t.type.adr < staticSize);
    Files.Set(X64, Files.Base(X64), baseFPos + t.type.adr);
    Files.WriteInt(X64, t.type.size);

    ASSERT(t.type.adr + 8 + B.MaxExt*8 < staticSize);
    Files.Set(X64, Files.Base(X64), baseFPos + t.type.adr + 8 + B.MaxExt*8);
    IF t.type.nTraced > 0 THEN Write_pointer_offset(0, t.type) END;
    Files.WriteInt(X64, -1);  t := t.next
  END;

  (* Write the module global variable pointer table *)
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

  (* Write procedure stack frame pointer tables *)
  Write_proc_pointer_offset(baseFPos, B.universe.first);

  Files.Set(X64, Files.Base(X64), baseFPos + staticSize);
END WriteStaticData;


PROCEDURE WriteImportCount;
VAR
  impmod:      B.Module;
  import:      B.Ident;
  importCount: INTEGER;
BEGIN
  impmod := B.modList;  importCount := 0;
  WHILE impmod # NIL DO
    import := impmod.impList;
    WHILE import # NIL DO INC(importCount);  import := import.next END;
    impmod := impmod.next
  END;
  Files.WriteInt(X64, importCount);
END WriteImportCount;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)


PROCEDURE Write*(debug:                Files.File;
                 code:                 ARRAY OF BYTE;
                 codesize,   initProc: INTEGER;
                 staticSize, varSize:  INTEGER;
                 modPtrTable:          INTEGER);
VAR
  filename: ARRAY 256 OF CHAR;
  x64file:  Files.File;
  r:        Files.Rider;
  readlen:  INTEGER;
  buffer:   ARRAY 200H OF BYTE;
  impmod:   B.Module;
  export:   B.ObjList;
  adr:      INTEGER;
BEGIN
  filename := B.modid;  B.Append(".x64", filename);
  x64file := Files.New(filename);
  Files.Set(X64, x64file, 0);

  (* 1. Module name and key *)
  Files.WriteString(X64, B.modid);
  Files.WriteInt(X64, B.modkey[0]);  Files.WriteInt(X64, B.modkey[1]);

  WriteImportCount;

  (* 5b. Module ptr table address *)
  Files.WriteInt(X64, modPtrTable);

  (* 7. Code offset of initialisation entry point, if any *)
  Files.WriteInt(X64, initProc);

  (* 2. List of referenced modules *)
  impmod := B.modList;
  WHILE impmod # NIL DO
    Files.WriteString(X64, impmod.id);
    Files.WriteInt(X64, impmod.key[0]);  Files.WriteInt(X64, impmod.key[1]);
    impmod := impmod.next
  END;
  Files.WriteChar(X64, 0X);

  (* 3. Length of module VAR *)
  Files.WriteInt(X64, varSize);

  (* 4. Initialised static data *)
  Files.WriteInt(X64, staticSize);
  WriteStaticData(staticSize);

  (* 6. Code *)
  Files.WriteInt(X64, codesize);
  Files.WriteBytes(X64, code, codesize);

  (* 8. Trap table *)
  Files.WriteInt(X64, Files.Length(debug));
  Files.Set(r, debug, 0);
  WHILE ~ r.eof DO
    Files.ReadBytes(r, buffer, LEN(buffer));
    readlen := LEN(buffer) - r.res;
    Files.WriteBytes(X64, buffer, readlen)
  END;

  (* 9. List of export addresses in export number order *)
  export := B.expList;
  WHILE export # NIL DO
    IF export.obj.class = B.cType THEN adr := export.obj.type.adr    + LSL(1, 62);
    ELSIF export.obj IS B.Var     THEN adr := export.obj(B.Var).adr  + LSL(2, 62);
    ELSIF export.obj IS B.Proc    THEN adr := export.obj(B.Proc).adr + LSL(3, 62)
    END;
    w.s("Export address: $"); w.h(adr); w.sl(".");
    Files.WriteInt(X64, adr);
    export := export.next
  END;
  Files.WriteInt(X64, 0);

  Files.Register(x64file)
END Write;

END Object.