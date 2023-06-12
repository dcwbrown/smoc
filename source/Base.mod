MODULE Base;
IMPORT SYSTEM, Files, Crypt, S := Scanner, w := Writer;

CONST
  MaxExt*    = 7;    MaxRecTypes* = 512;
  MaxImpMod* = 256;  MaxExpTypes* = 1024;  MaxModLev* = 255;

  (* Object class *)
  cNull*  = -1;  cModule* = 0;  cType*  = 1;
  cNode*  = 2;   cVar*    = 3;  cConst* = 4;  cProc* = 5;
  cField* = 6;   cSProc*  = 7;  cSFunc* = 8;

  (* Type form *)
  tInt*   = 0;  tBool* = 1;  tSet*   = 2;  tChar* = 3;  tReal* = 4;
  tPtr*   = 5;  tProc* = 6;  tArray* = 7;  tRec*  = 8;  tStr*  = 9;  tNil* = 10;

  typScalar* = {tInt,  tBool, tSet,  tChar,  tReal, tPtr, tProc, tNil};
  typEql*    = {tBool, tSet,  tPtr,  tProc,  tNil};
  typCmp*    = {tInt,  tReal, tChar, tStr};

TYPE
  ModuleKey* = ARRAY 2 OF INTEGER;

  Type*   = POINTER TO TypeDesc;
  Object* = POINTER TO ObjDesc;
  Node*   = POINTER TO NodeDesc;
  Ident*  = POINTER TO IdentDesc;

  ObjDesc* = RECORD
               class*: INTEGER;
               type*:  Type;
               ident*: Ident
             END;

  Const*   = POINTER TO RECORD (ObjDesc)  val*: INTEGER END;
  Field*   = POINTER TO RECORD (ObjDesc)  off*: INTEGER END;
  SProc*   = POINTER TO RECORD (ObjDesc)  id*:  INTEGER END;

  Var*     = POINTER TO RECORD (ObjDesc)
               adr*, expno*, lev*: INTEGER;
               ronly*:             BOOLEAN
             END;
  Par*     = POINTER TO RECORD (Var)  varpar*: BOOLEAN END;
  Str*     = POINTER TO RECORD (Var)  bufpos*, len*: INTEGER END;
  TempVar* = POINTER TO RECORD (Var)  inited*: BOOLEAN END;

  Proc* = POINTER TO RECORD (ObjDesc)
            (* Generator independent fields*)
            expno*, lev*:   INTEGER;
            nPtr*,  nProc*: INTEGER;
            nTraced*:       INTEGER;
            decl*:          Ident;
            statseq*:       Node;
            return*:        Object;
            (* Generator dependent fields *)
            adr*, descAdr*:      INTEGER;
            locblksize*:         INTEGER;
            usedReg*, usedXReg*: SET;
            homeSpace*, stack*:  INTEGER;
            fix*, lim*:          INTEGER
          END;

  ObjList*  = POINTER TO RECORD obj*:  Object;  next*: ObjList   END;
  TypeList* = POINTER TO RECORD type*: Type;    next*: TypeList  END;
  ProcList* = POINTER TO RECORD obj*:  Proc;    next*: ProcList  END;
  StrList*  = POINTER TO RECORD obj*:  Str;     next*: StrList  END;

  Module* = POINTER TO RECORD (ObjDesc)
    export*, import*: BOOLEAN;
    id*:              S.IdStr;
    key*:             ModuleKey;
    no*, lev*:        INTEGER;
    adr*:             INTEGER;
    next*:            Module;
    first*, impList*: Ident;
    types*:           TypeList
  END;

  NodeDesc* = RECORD (ObjDesc)
    (* Generator independent fields*)
    ronly*:            BOOLEAN;
    op*,   sourcePos*: INTEGER;
    left*, right*:     Object;
    (* Generator dependent fields *)
    link*:               Node;
    jmpSz*,    jmpPc*:   INTEGER;
    xRegUsed*, regUsed*: SET
  END;

  IdentDesc* = RECORD
    export*: BOOLEAN;
    name*:   S.IdStr;
    obj*:    Object;
    next*:   Ident
  END;

  Scope* = POINTER TO RECORD first*, last: Ident;  dsc*: Scope END;

  TypeDesc* = RECORD
    notag*, untagged*,   union*: BOOLEAN;
    mark,   predef:              BOOLEAN;
    nPtr*,  nTraced*,    nProc*: INTEGER;
    len*,   size*,       size0*: INTEGER;
    align*, parblksize*, nfpar*: INTEGER;
    adr*,   expno*,      ref*:   INTEGER;
    base*:                       Type;
    form*:                       BYTE;
    fields*:                     Ident;
    obj*:                        Object;
    mod*:                        Module
  END;

VAR
  topScope*, universe*,
  systemScope:          Scope;
  curLev*, modlev*:     INTEGER;
  Modid*:               S.IdStr;
  modkey*:              ModuleKey;
  system*:              BOOLEAN;
  expList*, lastExp:    ObjList;
  strList*:             StrList;
  recList*:             TypeList;

  (* Predefined Types *)
  nilType*,    noType*,
  boolType*,   setType*,
  realType*,   byteType*,
  card16Type*, card32Type*,
  intType*,    int8Type*,
  int16Type*,  int32Type*,
  charType*,   strType*:    Type;

  predefTypes: TypeList;

  Flag*: RECORD main*, console*, rtl*: BOOLEAN END;

  imod:     Module;
  modList*: Module;  (* List of imported modules *)

  good: BOOLEAN;

  Symfile: Files.File;  rider: Files.Rider;

  refno, preTypeNo, expno*, modno*: INTEGER;

  strBufSize*:  INTEGER;
  strBuf*:      ARRAY 8192 OF CHAR;

  SymFilename:  ARRAY 1024 OF CHAR;
  SrcPath*:     ARRAY 1024 OF CHAR;
  BuildPath*:   ARRAY 1024 OF CHAR;

  ExportType0: PROCEDURE(typ: Type);
  ImportType0: PROCEDURE(VAR typ: Type;  mod: Module);

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Utility *)

PROCEDURE Insert*(    src: ARRAY OF CHAR;
                  VAR dst: ARRAY OF CHAR;
                  VAR pos: INTEGER);
VAR i, j: INTEGER;
BEGIN i := pos;  j := 0;
  WHILE src[j] # 0X DO  dst[i] := src[j];  INC(i);  INC(j)  END;
  dst[i] := 0X;  pos := i
END Insert;

PROCEDURE Append* (src: ARRAY OF CHAR;  VAR dst: ARRAY OF CHAR);
VAR i, j: INTEGER;
BEGIN
  i := 0;  WHILE dst[i] # 0X DO INC(i) END;  j := 0;
  WHILE src[j] # 0X DO  dst[i] := src[j];  INC(i);  INC(j)  END;
  dst[i] := 0X
END Append;

PROCEDURE strLen* (str: ARRAY OF CHAR): INTEGER;
VAR len: INTEGER;
BEGIN
  len := 0;  WHILE (len < LEN(str)) & (str[len] # 0X) DO INC(len) END;
  RETURN len
END strLen;

PROCEDURE Align*(VAR a: INTEGER;  align: INTEGER);
BEGIN
  IF    a > 0 THEN a := (a + align - 1) DIV align * align
  ELSIF a < 0 THEN a :=        a        DIV align * align
  END
END Align;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE IsOpenArray*(tp: Type): BOOLEAN;
  RETURN (tp.form = tArray) & (tp.len < 0)
END IsOpenArray;

PROCEDURE IsNormalArray*(tp: Type): BOOLEAN;
  RETURN (tp.form = tArray) & (tp.len >= 0)
END IsNormalArray;

PROCEDURE IsStr*(t: Type): BOOLEAN;
  RETURN (t = strType) OR (t.form = tArray) & (t.base.form = tChar)
END IsStr;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Symbol table *)

PROCEDURE NewVar*(tp: Type): Var;
VAR v: Var;
BEGIN
  NEW(v);  v.class := cVar;  v.ronly := FALSE;
  v.type := tp;  v.lev := curLev;
  RETURN v
END NewVar;

PROCEDURE NewConst*(tp: Type;  val: INTEGER): Const;
VAR c: Const;
BEGIN
  NEW(c);  c.class := cConst;
  c.type := tp;  c.val := val;
  RETURN c
END NewConst;

PROCEDURE NewPar*(VAR proc: TypeDesc;  tp: Type;  varpar: BOOLEAN): Par;
VAR p: Par;
BEGIN
  NEW(p);  p.class := cVar;
  p.type := tp;  p.lev := curLev;
  p.varpar := varpar;  INC(proc.nfpar);
  p.ronly := ~varpar & (tp.form IN {tArray, tRec});
  RETURN p
END NewPar;

PROCEDURE NewField*(VAR rec: TypeDesc;  tp: Type): Field;
VAR fld: Field;
BEGIN
  NEW(fld);  fld.class := cField;  fld.type := tp;
  rec.nPtr    := rec.nPtr    + tp.nPtr;
  rec.nProc   := rec.nProc   + tp.nProc;
  rec.nTraced := rec.nTraced + tp.nTraced;
  RETURN fld
END NewField;

PROCEDURE LookupString(s: ARRAY OF CHAR;  slen: INTEGER): Str;
VAR str: StrList;  result: Str;  i: INTEGER;
BEGIN
  str := strList;  result := NIL;
IF FALSE THEN
  WHILE str # NIL DO
    i := 0;
    IF str.obj.bufpos >= 0 THEN
      WHILE (i < slen) & (s[i] = strBuf[str.obj.bufpos + i]) DO INC(i) END
    END;
    IF i = slen THEN
      result := str.obj; str := NIL
    ELSE
      str := str.next
    END
  END
END
RETURN result END LookupString;

PROCEDURE NewStr*(str: ARRAY OF CHAR;  slen: INTEGER): Str;
VAR x: Str;  i: INTEGER;  p: StrList;
BEGIN
  x := LookupString(str, slen);
  IF x = NIL THEN
    NEW(x);  x.class := cVar;  x.ronly := TRUE;
    x.type := strType;  x.lev := curLev;  x.len := slen;
    IF x.lev >= -1 (* need to alloc buffer *) THEN
      IF strBufSize + slen >= LEN(strBuf) THEN
        S.Mark("too many strings");  x.bufpos := -1
      ELSE
        x.bufpos := strBufSize;  strBufSize := strBufSize + slen;
        FOR i := 0 TO slen-1 DO strBuf[x.bufpos+i] := str[i] END;
        NEW(p);  p.obj := x;  p.next := strList;  strList := p
      END
    ELSE x.bufpos := -1
    END
  END;
  RETURN x
END NewStr;

PROCEDURE NewStrZ*(str: ARRAY OF CHAR): Str;
VAR slen: INTEGER;
BEGIN
  slen := 0;  WHILE str[slen] # 0X DO INC(slen) END;  INC(slen);
  RETURN NewStr(str, slen)
END NewStrZ;

PROCEDURE NewProc*(): Proc;
VAR p: Proc;
BEGIN
  NEW(p);  p.class := cProc;  p.lev := curLev;
  p.nPtr := 0;  p.nProc := 0;  p.nTraced := 0;
  RETURN p
END NewProc;

PROCEDURE NewTypeObj*(tp: Type): Object;
VAR x: Object;
BEGIN
  NEW(x);  x.class := cType;
  x.type := tp;  IF tp.obj = NIL THEN tp.obj := x END;
  RETURN x
END NewTypeObj;

PROCEDURE NewSProc*(id, cls: INTEGER): SProc;
VAR x: SProc;
BEGIN
  NEW(x);  x.class := cls;
  x.id := id;  x.type := noType;
  RETURN x
END NewSProc;

PROCEDURE NewTempVar*(tp: Type): TempVar;
VAR t: TempVar;
BEGIN
  NEW(t);  t.class := cVar;  t.ronly := FALSE;
  t.type := tp;  t.lev := curLev;  t.inited := FALSE;
  RETURN t
END NewTempVar;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE NewType*(VAR typ: Type;  form: INTEGER);
BEGIN
  NEW(typ);  typ.form := form;  typ.size := 0;  typ.align := 0;
  typ.ref := -1;  typ.mark := FALSE;  typ.predef := FALSE;
  typ.nPtr := 0;  typ.nProc := 0;  typ.nTraced := 0
END NewType;

PROCEDURE NewArray*(len: INTEGER): Type;
VAR tp: Type;
BEGIN
  NewType(tp, tArray);  tp.len := len;  tp.untagged := FALSE;
  RETURN tp
END NewArray;

PROCEDURE CompleteArray*(VAR tp: TypeDesc);
BEGIN
  IF tp.base.form = tArray THEN CompleteArray(tp.base^) END;
  tp.nPtr := tp.len * tp.base.nPtr;
  tp.nTraced := tp.len * tp.base.nTraced;
  tp.nProc := tp.len * tp.base.nProc
END CompleteArray;

PROCEDURE NewRecord*(): Type;
VAR tp: Type;  p: TypeList;
BEGIN
  NewType(tp, tRec);  tp.len := 0;  tp.union := FALSE;
  IF curLev >= 0 THEN (* not external record *)
    NEW(p);  p.type := tp;
    p.next := recList;  recList := p
  ELSIF curLev = -1 THEN ASSERT(FALSE)
  END;
  RETURN tp
END NewRecord;

PROCEDURE ExtendRecord*(VAR recType: TypeDesc);
VAR baseType: Type;
BEGIN
  IF recType.base # NIL THEN
    baseType := recType.base;
    recType.len := baseType.len + 1;
    recType.nPtr := baseType.nPtr;
    recType.nTraced := baseType.nTraced;
    recType.nProc := baseType.nProc
  END
END ExtendRecord;

PROCEDURE NewPointer*(): Type;
VAR tp: Type;
BEGIN
  NewType(tp, tPtr);  tp.nPtr := 1;  tp.nTraced := 1;
  RETURN tp
END NewPointer;

PROCEDURE NewProcType*(): Type;
VAR tp: Type;
BEGIN
  NewType(tp, tProc);  tp.nfpar := 0;  tp.nProc := 1;
  RETURN tp
END NewProcType;

PROCEDURE NewPredefinedType(VAR typ: Type;  form: INTEGER);
VAR p: TypeList;
BEGIN
  NewType(typ, form);  INC(preTypeNo);
  typ.predef := TRUE;  typ.ref := preTypeNo;
  NEW(p);  p.type := typ;  p.next := predefTypes;  predefTypes := p
END NewPredefinedType;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE OpenScope*;
VAR scp: Scope;
BEGIN NEW(scp);  scp.dsc := topScope;  topScope := scp
END OpenScope;

PROCEDURE CloseScope*;
BEGIN topScope := topScope.dsc
END CloseScope;

PROCEDURE IncLev*(n: INTEGER);
BEGIN curLev := curLev + n
END IncLev;

PROCEDURE Enter(x: Object;  name: S.IdStr);
VAR ident: Ident;
BEGIN
  NEW(ident);  ident.name := name;  ident.export := FALSE;
  ident.obj := x;  x.ident := ident;
  ident.next := topScope.first;  topScope.first := ident
END Enter;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE FindMod(name: S.IdStr): Module;
VAR mod: Module;
BEGIN
  mod := modList;
  WHILE (mod # NIL) & (mod.id # name) DO mod := mod.next END;
  RETURN mod
END FindMod;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Export symbol file *)

PROCEDURE NewExport(VAR exp: ObjList);
BEGIN
  NEW(exp);  INC(expno);
  IF lastExp = NIL THEN expList := exp;  lastExp := exp
  ELSE lastExp.next := exp;  lastExp := exp
  END
END NewExport;

PROCEDURE WriteModkey(key: ModuleKey);
BEGIN
  Files.WriteInt(rider, key[0]);
  Files.WriteInt(rider, key[1])
END WriteModkey;

PROCEDURE DetectType(typ: Type);
BEGIN
  IF typ # NIL THEN
    IF typ.predef THEN
      Files.WriteNum(rider, 1);  Files.WriteNum(rider, typ.ref)
    ELSIF typ.mod = NIL THEN
      Files.WriteNum(rider, 2);  Files.WriteNum(rider, typ.ref);
      IF typ.ref < 0 THEN ExportType0(typ) END
    ELSE
      Files.WriteNum(rider, 3);  Files.WriteString(rider, typ.mod.id);
      Files.WriteBool(rider, ~typ.mod.export);
      IF ~typ.mod.export THEN
        WriteModkey(typ.mod.key);  typ.mod.export := TRUE
      END;
      Files.WriteNum(rider, typ.ref);
      IF typ.ref < 0 THEN ExportType0(typ) END
    END
  ELSE Files.WriteNum(rider, 0)
  END
END DetectType;

PROCEDURE ExportProc(typ: Type);
VAR par: Ident;  x: Par;
BEGIN
  Files.WriteNum(rider, typ.size);  Files.WriteNum(rider, typ.align);
  Files.WriteNum(rider, typ.parblksize);  DetectType(typ.base);
  par := typ.fields;
  WHILE par # NIL DO x := par.obj(Par);
    Files.WriteNum(rider, cVar);  Files.WriteString(rider, par.name);
    Files.WriteBool(rider, x.varpar);  DetectType(x.type);  par := par.next
  END;
  Files.WriteNum(rider, cNull)
END ExportProc;

PROCEDURE ExportType(typ: Type);
VAR fld: Ident;  ftyp: Type;  exp: ObjList;
BEGIN
  IF typ.mod = NIL THEN
    IF refno < MaxExpTypes THEN
      INC(refno);  typ.ref := refno
    ELSE S.Mark("Too many exported types")
    END
  ELSE typ.ref := -typ.ref
  END;
  Files.WriteNum(rider, -typ.ref);  Files.WriteNum(rider, typ.form);
  IF typ.form = tRec THEN
    NewExport(exp);  NEW(exp.obj);
    exp.obj.class := cType;  exp.obj.type := typ;
    Files.WriteNum(rider, expno);  Files.WriteNum(rider, typ.size0);
    Files.WriteNum(rider, typ.size);  Files.WriteNum(rider, typ.align);
    Files.WriteBool(rider, typ.union);  DetectType(typ.base);
    fld := typ.fields;
    WHILE fld # NIL DO ftyp := fld.obj.type;
      IF fld.export OR (ftyp.nPtr > 0) OR (ftyp.nProc > 0) THEN
        Files.WriteNum(rider, cField);
        IF fld.export THEN Files.WriteString(rider, fld.name)
        ELSE Files.WriteString(rider, 0X)
        END;
        Files.WriteNum(rider, fld.obj(Field).off);  DetectType(ftyp)
      END;
      fld := fld.next
    END;
    Files.WriteNum(rider, cNull)
  ELSIF typ.form = tArray THEN
    Files.WriteNum(rider, typ.len);  Files.WriteBool(rider, typ.notag);
    Files.WriteNum(rider, typ.size);  Files.WriteNum(rider, typ.align);
    DetectType(typ.base)
  ELSIF typ.form = tPtr THEN
    Files.WriteNum(rider, typ.size);
    Files.WriteNum(rider, typ.align);
    Files.WriteNum(rider, typ.nTraced);
    DetectType(typ.base)
  ELSIF typ.form = tProc THEN
    ExportProc(typ)
  END
END ExportType;


PROCEDURE WriteSymfile*;
VAR
  ident:         Ident;
  exp:           ObjList;
  x:             Object;
  i, k, n, size: INTEGER;
  hash:          Crypt.MD5Hash;
  chunk:         ARRAY 64 OF BYTE;
BEGIN
  refno := 0;  expno := 0;  i := 0;
  Symfile := Files.New(SymFilename);
  Files.Set(rider, Symfile, 16);
  Files.WriteNum(rider, modlev);

  imod := modList;
  WHILE imod # NIL DO
    Files.WriteNum(rider, cModule);  Files.WriteString(rider, imod.id);
    WriteModkey(imod.key);  imod := imod.next
  END;

  ident := universe.first;
  WHILE ident # NIL DO
    IF ident.export THEN x := ident.obj;
      IF x.class = cConst THEN
        Files.WriteNum(rider, cConst);
        Files.WriteString(rider, ident.name);
        Files.WriteNum(rider, x(Const).val);
        DetectType(x.type)
      ELSIF x.class = cType THEN
        Files.WriteNum(rider, cType);
        Files.WriteString(rider, ident.name);
        DetectType(x.type)
      ELSIF x.class = cVar THEN
        IF x IS Str THEN
          Files.WriteNum(rider, cConst);
          Files.WriteString(rider, ident.name);
          NewExport(exp);  exp.obj := x;
          Files.WriteNum(rider, expno);  DetectType(x.type);
          Files.WriteNum(rider, x(Str).len)
        ELSE
          Files.WriteNum(rider, cVar);
          Files.WriteString(rider, ident.name);
          NewExport(exp);  exp.obj := x;
          Files.WriteNum(rider, expno);  DetectType(x.type)
        END
      ELSIF x.class = cProc THEN
        Files.WriteNum(rider, cProc);
        Files.WriteString(rider, ident.name);
        NewExport(exp);  exp.obj := x;
        Files.WriteNum(rider, expno);  DetectType(x.type)
      ELSE ASSERT(FALSE)
      END
    END;
    ident := ident.next
  END;
  Files.WriteNum(rider, cNull);

  size := Files.Pos(rider);  Files.Set(rider, Symfile, 0);
  Crypt.InitMD5Hash(hash);  i := 0;
  REPEAT
    Files.ReadBytes(rider, chunk, LEN(chunk));
    k := LEN(chunk) - rider.res;  INC(i, k);
    Crypt.MD5ComputeChunk(hash, SYSTEM.ADR(chunk), k)
  UNTIL i = size;

  Files.Set(rider, Symfile, 0);
  modkey[0] := Crypt.MD5GetLowResult(hash);
  modkey[1] := Crypt.MD5GetHighResult(hash);
  WriteModkey(modkey);

  IF S.errCnt = 0 THEN Files.Register(Symfile) END
END WriteSymfile;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Import symbol file *)

PROCEDURE FindType(ref: INTEGER;  types: TypeList): Type;
VAR p: TypeList;  typ: Type;
BEGIN p := types;
  WHILE (p # NIL) & (p.type.ref # ref) DO p := p.next END;
  IF p # NIL THEN typ := p.type END;
  RETURN typ
END FindType;

PROCEDURE NewImport(name: S.IdStr;  x: Object);
VAR ident, p: Ident;
BEGIN
  NEW(ident);  p := topScope.last;
  IF p # NIL THEN p.next := ident ELSE topScope.first := ident END;
  topScope.last := ident;  ident.export := FALSE;  ident.obj := x;
  ident.name := name;  IF x.ident = NIL THEN x.ident := ident END
END NewImport;

PROCEDURE ReadModkey(VAR key: ModuleKey);
BEGIN
  Files.ReadInt(rider, key[0]);
  Files.ReadInt(rider, key[1])
END ReadModkey;

PROCEDURE DetectTypeI(VAR typ: Type);
VAR
  n, ref: INTEGER;
  first:  BOOLEAN;
  mod:    Module;
  id:     S.IdStr;
  key:    ModuleKey;
BEGIN
  Files.ReadNum(rider, n);
  IF n = 0 THEN typ := NIL
  ELSIF (n = 1) OR (n = 2) THEN Files.ReadNum(rider, ref);
    IF ref > 0 THEN
      IF n = 1 THEN typ := FindType(ref, predefTypes)
      ELSE typ := FindType(-ref, imod.types)
      END
    ELSE ImportType0(typ, imod)
    END
  ELSIF n = 3 THEN
    Files.ReadString(rider, id);  Files.ReadBool(rider, first);
    mod := FindMod(id);
    IF first THEN ReadModkey(key);
      IF mod # NIL THEN
        IF (mod.key[0] # key[0]) OR (mod.key[1] # key[1]) THEN
          S.Mark("Modkey mismatched")
        END
      ELSE
        NEW(mod);  mod.id := id;  mod.key := key;
        mod.next := modList;  modList := mod;
        mod.no := modno;  DEC(modno);  mod.import := FALSE
      END
    END;
    Files.ReadNum(rider, ref);
    IF S.errCnt = 0 THEN
      IF ref < 0 THEN ImportType0(typ, mod)
      ELSE typ := FindType(-ref, mod.types)
      END
    END
  ELSE ASSERT(FALSE)
  END
END DetectTypeI;

PROCEDURE AddToTypeList(typ: Type;  mod: Module);
VAR i: INTEGER;  p: TypeList;
BEGIN
  NEW(p);  p.next := mod.types;
  mod.types := p;  p.type := typ
END AddToTypeList;

PROCEDURE ImportType(VAR typ: Type;  mod: Module);
VAR typ0: TypeDesc;  form, ref, len: INTEGER;

  PROCEDURE ImportRecord(VAR typ: TypeDesc;  new: BOOLEAN);
    VAR cls, off: INTEGER;  fltype: Type;  x: Field;  name: S.IdStr;
  BEGIN
    Files.ReadNum(rider, typ.expno);  Files.ReadNum(rider, typ.size0);
    Files.ReadNum(rider, typ.size);  Files.ReadNum(rider, typ.align);
    Files.ReadBool(rider, typ.union);  typ.adr := 0;  DetectTypeI(typ.base);
    IF S.errCnt = 0 THEN ExtendRecord(typ);
      Files.ReadNum(rider, cls);  OpenScope;
      WHILE (cls = cField) & (S.errCnt = 0) DO
        Files.ReadString(rider, name);  Files.ReadNum(rider, off);
        DetectTypeI(fltype);  Files.ReadNum(rider, cls);
        IF new & (S.errCnt = 0) THEN
          x := NewField(typ, fltype);
          x.off := off;  NewImport(name, x)
        END
      END;
      IF S.errCnt = 0 THEN ASSERT(cls = cNull) END;
      typ.fields := topScope.first;  CloseScope
    END
  END ImportRecord;

  PROCEDURE ImportArray(VAR typ: TypeDesc);
  BEGIN
    Files.ReadBool(rider, typ.notag);
    Files.ReadNum(rider, typ.size);  Files.ReadNum(rider, typ.align);
    DetectTypeI(typ.base);  IF S.errCnt = 0 THEN CompleteArray(typ) END
  END ImportArray;

  PROCEDURE ImportPointer(VAR typ: TypeDesc);
  BEGIN
    Files.ReadNum(rider, typ.size);  Files.ReadNum(rider, typ.align);
    Files.ReadNum(rider, typ.nTraced);  DetectTypeI(typ.base)
  END ImportPointer;

  PROCEDURE ImportProc(VAR typ: TypeDesc;  new: BOOLEAN);
    VAR cls: INTEGER;  varpar: BOOLEAN;
      par: Ident;  x: Par;  xtype: Type;  name: S.IdStr;
  BEGIN
    Files.ReadNum(rider, typ.size);  Files.ReadNum(rider, typ.align);
    Files.ReadNum(rider, typ.parblksize);  DetectTypeI(typ.base);
    IF S.errCnt = 0 THEN
      Files.ReadNum(rider, cls);  OpenScope;
      WHILE (cls = cVar) & (S.errCnt = 0) DO
        Files.ReadString(rider, name);  Files.ReadBool(rider, varpar);
        DetectTypeI(xtype);  Files.ReadNum(rider, cls);
        IF new & (S.errCnt = 0) THEN
          x := NewPar(typ, xtype, varpar);  NewImport(name, x)
        END
      END;
      IF S.errCnt = 0 THEN ASSERT(cls = cNull) END;
      typ.fields := topScope.first;  CloseScope
    END
  END ImportProc;

BEGIN (* ImportType *)
  Files.ReadNum(rider, ref);  Files.ReadNum(rider, form);
  IF (mod = imod) & mod.import THEN typ := NIL
  ELSE typ := FindType(ref, mod.types)
  END;
  IF form = tRec THEN
    IF typ = NIL THEN
      typ := NewRecord();  typ.ref := ref;
      AddToTypeList(typ, mod);  typ.mod := mod;
      ImportRecord(typ^, TRUE)
    ELSE ImportRecord(typ0, FALSE)
    END
  ELSIF form = tArray THEN
    Files.ReadNum(rider, typ0.len);
    IF typ = NIL THEN
      typ := NewArray(typ0.len);  typ.ref := ref;
      AddToTypeList(typ, mod);  typ.mod := mod;
      ImportArray(typ^)
    ELSE ImportArray(typ0)
    END
  ELSIF form = tPtr THEN
    IF typ = NIL THEN
      typ := NewPointer();  typ.ref := ref;
      AddToTypeList(typ, mod);  typ.mod := mod;
      ImportPointer(typ^)
    ELSE ImportPointer(typ0)
    END
  ELSIF form = tProc THEN
    IF typ = NIL THEN
      typ := NewProcType();  typ.ref := ref;
      AddToTypeList(typ, mod);  typ.mod := mod;
      ImportProc(typ^, TRUE)
    ELSE ImportProc(typ0, FALSE)
    END
  END
END ImportType;

PROCEDURE Import(imodid: S.IdStr): Module;
VAR dep:       Module;
    x:         Object;
    key:       ModuleKey;
    good:      BOOLEAN;
    lev, val,
    cls, slen: INTEGER;
    tp:        Type;
    depid:     S.IdStr;
    name:      S.IdStr;
    msg:       ARRAY 512 OF CHAR;
BEGIN
  Files.Set(rider, Symfile, 0);
  imod := FindMod(imodid);
  ReadModkey(key);
  Files.ReadNum(rider, lev);

  IF imod = NIL THEN
    NEW(imod);  imod.id := imodid;  imod.adr := 0;  imod.import := TRUE;
    imod.next := modList;  modList := imod;  imod.no := modno;
    DEC(modno);  imod.key := key;  imod.lev := lev;  imod.export := FALSE;
    IF lev >= modlev THEN
      modlev := lev + 1;
      IF modlev > MaxModLev THEN S.Mark("Module level too high") END
    END
  ELSIF (key[0] = imod.key[0]) & (key[1] = imod.key[1]) THEN
    imod.adr := 0;  imod.lev := lev;  imod.export := FALSE
  ELSE S.Mark("Was imported with a different key")
  END;

  IF S.errCnt = 0 THEN
    OpenScope;  curLev := imod.no;  Files.ReadNum(rider, cls);
    WHILE (cls = cModule) & (S.errCnt = 0) DO
      Files.ReadString(rider, depid);
      ReadModkey(key);
      dep := FindMod(depid);
      IF depid = Modid THEN S.Mark("Circular dependency")
      ELSIF dep # NIL THEN
        IF (dep.key[0] # key[0]) OR (dep.key[1] # key[1]) THEN
          msg := "Module ";                  Append(depid, msg);
          Append(" was imported by ", msg);  Append(imodid, msg);
          Append(" with a different key", msg);  S.Mark(msg)
        END
      END;
      Files.ReadNum(rider, cls)
    END;
    WHILE (cls = cConst) & (S.errCnt = 0) DO
      Files.ReadString(rider, name);
      Files.ReadNum(rider, val);  DetectTypeI(tp);
      IF S.errCnt = 0 THEN
        IF tp = strType THEN
          Files.ReadNum(rider, slen);  x := NewStr("", slen);
          x(Str).adr := 0;  x(Str).expno := val
        ELSE
          x := NewConst(tp, val)
        END;
        NewImport(name, x)
      END;
      Files.ReadNum(rider, cls)
    END;
    WHILE (cls = cType) & (S.errCnt = 0) DO
      Files.ReadString(rider, name);  DetectTypeI(tp);
      IF S.errCnt = 0 THEN x := NewTypeObj(tp);  NewImport(name, x) END;
      Files.ReadNum(rider, cls)
    END;
    WHILE (cls = cVar) & (S.errCnt = 0) DO
      Files.ReadString(rider, name);
      Files.ReadNum(rider, val);  DetectTypeI(tp);
      IF S.errCnt = 0 THEN
        x := NewVar(tp);  x(Var).ronly := TRUE;
        x(Var).expno := val;  x(Var).adr := 0;  NewImport(name, x)
      END;
      Files.ReadNum(rider, cls)
    END;
    WHILE (cls = cProc) & (S.errCnt = 0) DO
      Files.ReadString(rider, name);
      Files.ReadNum(rider, val);  DetectTypeI(tp);
      IF S.errCnt = 0 THEN
        x := NewProc();  x(Proc).adr := 0;
        x(Proc).type := tp;  tp.obj := x;
        x(Proc).expno := val;  NewImport(name, x)
      END;
      Files.ReadNum(rider, cls)
    END;
    IF S.errCnt = 0 THEN ASSERT(cls = cNull) END;
    curLev := 0;  imod.import := TRUE;
    imod.first := topScope.first;  CloseScope
  END;
  RETURN imod
END Import;

PROCEDURE NewSystemModule*(modident: Ident);
VAR mod: Module;
BEGIN
  NEW(mod);
  mod.lev := -1;  mod.first := systemScope.first;
  modident.obj := mod;  mod.ident := modident;  system := TRUE
END NewSystemModule;


(* Import module - ident has local name, id is real (defined) name *)
PROCEDURE NewModule*(ident: Ident;  id: S.IdStr);
VAR mod: Module;  filename: ARRAY 1024 OF CHAR;
BEGIN (* NewModule *)
  mod := FindMod(id);  IF (mod # NIL) & ~mod.import THEN mod := NIL END;
  IF id = Modid THEN S.Mark("Cannot import self")
  ELSIF mod = NIL THEN
    filename := BuildPath;  Append(id, filename);  Append(".sym", filename);
    Symfile := Files.Old(filename);
    IF Symfile # NIL THEN
      ident.obj := Import(id)
    ELSE
      filename := "Symbol file not found: ";
      Append(BuildPath, filename);  Append(id, filename);  Append(".sym", filename);
      S.Mark(filename)
    END
  END
END NewModule;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Compiler Flag *)

PROCEDURE SetCompilerFlag(pragma: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN
  IF    pragma = "MAIN"    THEN Flag.main   := TRUE
  ELSIF pragma = "CONSOLE" THEN Flag.main   := TRUE;  Flag.console := TRUE
  ELSIF pragma = "RTL-"    THEN Flag.rtl    := FALSE
  END
END SetCompilerFlag;

PROCEDURE InitCompilerFlag;
BEGIN Flag.main := FALSE;  Flag.console := FALSE;  Flag.rtl  := TRUE;
END InitCompilerFlag;

PROCEDURE SetSourcePathFromFilename*(path: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN
  SrcPath := path;  i := 0;  WHILE SrcPath[i] # 0X DO INC(i) END;
  WHILE (i >= 0) & (SrcPath[i] # '\') & (SrcPath[i] # '/') DO DEC(i) END;
  SrcPath[i+1] := 0X
END SetSourcePathFromFilename;

PROCEDURE EnsureSeparator(VAR path: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN
  i := 0;  WHILE (i < LEN(path)) & (path[i] # 0X) DO INC(i) END;
  IF i > 0 THEN
    IF (path[i-1] # '/') & (path[i-1] # '\') THEN
      path[i] := '/';  path[i+1] := 0X
    END
  END
END EnsureSeparator;

PROCEDURE SetSourcePath*(path: ARRAY OF CHAR);
BEGIN SrcPath := path;    EnsureSeparator(SrcPath)    END SetSourcePath;

PROCEDURE SetBuildPath*(path: ARRAY OF CHAR);
BEGIN BuildPath := path;  EnsureSeparator(BuildPath)  END SetBuildPath;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Init*(modid: S.IdStr);
VAR i, res: INTEGER;
BEGIN
  Modid := modid;  i := 0;
  Insert(BuildPath, SymFilename, i);
  Insert(Modid,     SymFilename, i);
  Insert(".sym",    SymFilename, i);
  Files.Delete(SymFilename, res);

  NEW(universe);     topScope := universe;  curLev := -1;
  system  := FALSE;  modno    := -2;        strBufSize := 0;
  expList := NIL;    lastExp  := NIL;       strList    := NIL;  recList := NIL;
  InitCompilerFlag;

  Enter(NewTypeObj(intType),  "INTEGER");
  Enter(NewTypeObj(byteType), "BYTE");
  Enter(NewTypeObj(realType), "REAL");
  Enter(NewTypeObj(setType),  "SET");
  Enter(NewTypeObj(boolType), "BOOLEAN");
  Enter(NewTypeObj(charType), "CHAR");

  Enter(NewSProc(S.spINC,    cSProc), "INC");
  Enter(NewSProc(S.spDEC,    cSProc), "DEC");
  Enter(NewSProc(S.spINCL,   cSProc), "INCL");
  Enter(NewSProc(S.spEXCL,   cSProc), "EXCL");
  Enter(NewSProc(S.spNEW,    cSProc), "NEW");
  Enter(NewSProc(S.spASSERT, cSProc), "ASSERT");
  Enter(NewSProc(S.spPACK,   cSProc), "PACK");
  Enter(NewSProc(S.spUNPK,   cSProc), "UNPK");

  Enter(NewSProc(S.sfABS,    cSFunc), "ABS");
  Enter(NewSProc(S.sfODD,    cSFunc), "ODD");
  Enter(NewSProc(S.sfLEN,    cSFunc), "LEN");
  Enter(NewSProc(S.sfLSL,    cSFunc), "LSL");
  Enter(NewSProc(S.sfASR,    cSFunc), "ASR");
  Enter(NewSProc(S.sfROR,    cSFunc), "ROR");
  Enter(NewSProc(S.sfFLOOR,  cSFunc), "FLOOR");
  Enter(NewSProc(S.sfFLT,    cSFunc), "FLT");
  Enter(NewSProc(S.sfORD,    cSFunc), "ORD");
  Enter(NewSProc(S.sfCHR8,   cSFunc), "CHR");

  OpenScope;
  Enter(NewSProc(S.spGET,            cSProc), "GET");
  Enter(NewSProc(S.spPUT,            cSProc), "PUT");
  Enter(NewSProc(S.spCOPY,           cSProc), "COPY");
(*Enter(NewSProc(S.spLoadLibraryA,   cSProc), "LoadLibraryA");  *)
(*Enter(NewSProc(S.spGetProcAddress, cSProc), "GetProcAddress");*)
  Enter(NewSProc(S.spINT3,           cSProc), "INT3");
  Enter(NewSProc(S.spPAUSE,          cSProc), "PAUSE");

  Enter(NewSProc(S.sfADR,            cSFunc), "ADR");
  Enter(NewSProc(S.sfSIZE,           cSFunc), "SIZE");
  Enter(NewSProc(S.sfBIT,            cSFunc), "BIT");
  Enter(NewSProc(S.sfVAL,            cSFunc), "VAL");
  Enter(NewSProc(S.sfNtCurrentTeb,   cSFunc), "NtCurrentTeb");
  Enter(NewSProc(S.sfCAS,            cSFunc), "CAS");

  Enter(NewTypeObj(byteType),   "BYTE");
  Enter(NewTypeObj(card16Type), "CARD16");
  Enter(NewTypeObj(card32Type), "CARD32");
  Enter(NewTypeObj(int8Type),   "INT8");
  Enter(NewTypeObj(int16Type),  "INT16");
  Enter(NewTypeObj(int32Type),  "INT32");
  systemScope := topScope;  CloseScope;

  curLev := 0
END Init;

PROCEDURE Cleanup*;
VAR i: INTEGER;
BEGIN
  universe := NIL;  topScope := NIL;  systemScope := NIL;  modList := NIL;
  expList  := NIL;  lastExp  := NIL;  strList     := NIL;  recList := NIL
END Cleanup;

BEGIN
  ExportType0 := ExportType;  ImportType0 := ImportType;
  S.InstallSetCompilerFlag(SetCompilerFlag);

  preTypeNo := 0;  curLev := -1;

  NewPredefinedType(intType,    tInt);
  NewPredefinedType(byteType,   tInt);
  NewPredefinedType(boolType,   tBool);
  NewPredefinedType(setType,    tSet);
  NewPredefinedType(charType,   tChar);
  NewPredefinedType(nilType,    tNil);
  NewPredefinedType(realType,   tReal);
  NewPredefinedType(strType,    tStr);
  NewPredefinedType(noType,     tPtr);    noType.base := intType;
  NewPredefinedType(card16Type, tInt);
  NewPredefinedType(card32Type, tInt);
  NewPredefinedType(int8Type,   tInt);
  NewPredefinedType(int16Type,  tInt);
  NewPredefinedType(int32Type,  tInt)
END Base.