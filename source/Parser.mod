MODULE Parser;
IMPORT S := Scanner, B := Base, G := Generator, w := Write8;

TYPE
  UndefPtrList = POINTER TO RECORD
    name: S.IdStr8;  tp: B.Type;  next: UndefPtrList
  END;

VAR
  sym:       INTEGER;
  undefList: UndefPtrList;

  type0:              PROCEDURE(): B.Type;
  expression0:        PROCEDURE(): B.Object;
  StatementSequence0: PROCEDURE(): B.Node;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE GetSym;                      BEGIN S.Get(sym)   END GetSym;
PROCEDURE Mark8(msg: ARRAY OF CHAR8);  BEGIN S.Mark8(msg) END Mark8;

PROCEDURE Missing(s: INTEGER);
BEGIN
  IF    s = S.ident     THEN Mark8(`Missing identifier`)
  ELSIF s = S.return    THEN Mark8(`Missing 'RETURN'`)
  ELSIF s = S.comma     THEN Mark8(`Missing ','`)
  ELSIF s = S.semicolon THEN Mark8(`Missing ';'`)
  ELSIF s = S.period    THEN Mark8(`Missing '.'`)
  ELSIF s = S.semicolon THEN Mark8(`Missing ';'`)
  ELSIF s = S.eql       THEN Mark8(`Missing '='`)
  ELSIF s = S.colon     THEN Mark8(`Missing ':'`)
  ELSIF s = S.of        THEN Mark8(`Missing 'OF'`)
  ELSIF s = S.end       THEN Mark8(`Missing 'END'`)
  ELSIF s = S.to        THEN Mark8(`Missing 'TO'`)
  ELSIF s = S.rparen    THEN Mark8(`Missing ')'`)
  ELSIF s = S.rbrak     THEN Mark8(`Missing ']'`)
  ELSIF s = S.rbrace    THEN Mark8(`Missing '}'`)
  ELSIF s = S.then      THEN Mark8(`Missing 'THEN'`)
  ELSIF s = S.do        THEN Mark8(`Missing 'DO'`)
  ELSIF s = S.until     THEN Mark8(`Missing 'UNTIL'`)
  ELSIF s = S.becomes   THEN Mark8(`Missing ':='`)
  ELSIF s = S.period    THEN Mark8(`Missing '.'`)
  ELSIF s = S.comma     THEN Mark8(`Missing ','`)
  ELSE                       Mark8(`Missing something`)
  END
END Missing;

PROCEDURE CheckSym(expect: INTEGER);
BEGIN
  IF sym = expect THEN GetSym ELSE Missing(expect) END;
END CheckSym;

PROCEDURE IsExt0(t1, t2: B.Type): BOOLEAN;
  RETURN (t1 = t2) OR (t1 # NIL) & IsExt0(t1.base, t2)
END IsExt0;

PROCEDURE IsExt(t1, t2: B.Type): BOOLEAN;
BEGIN
  IF t1.form = B.tPtr THEN t1 := t1.base END;
  IF t2.form = B.tPtr THEN t2 := t2.base END;
  RETURN IsExt0(t1, t2)
END IsExt;

PROCEDURE IsVarPar(x: B.Object): BOOLEAN;
  RETURN (x IS B.Par) & x(B.Par).varpar
END IsVarPar;

PROCEDURE IsConst(x: B.Object): BOOLEAN;
  RETURN (x IS B.Const)
  OR     (x IS B.Str16) & (x(B.Str16).bufpos >= 0)
  OR     (x IS B.Str8)  & (x(B.Str8) .bufpos >= 0)
END IsConst;

PROCEDURE IsOpenArray(tp: B.Type): BOOLEAN;
  RETURN (tp.form = B.tArray) & (tp.len < 0)
END IsOpenArray;

PROCEDURE IsOpenArray0(tp: B.Type): BOOLEAN;
  RETURN (tp.form = B.tArray) & (tp.len < 0) & (tp.notag)
END IsOpenArray0;

PROCEDURE CompArray(t1, t2: B.Type): BOOLEAN;
  RETURN (t1.form = B.tArray) & (t2.form = B.tArray)
    & ((t1.base = t2.base) OR CompArray(t1.base, t2.base))
END CompArray;

PROCEDURE SameParType(t1, t2: B.Type): BOOLEAN;
  RETURN (t1 = t2)
  OR (t1.form = B.tArray) & (t1.form = B.tArray)
    & (t1.base = t2.base) & (t1.notag = t2.notag)
    & (t1.len = -1) & (t2.len = -1)
END SameParType;

PROCEDURE SamePars(p1, p2: B.Ident): BOOLEAN;
  RETURN (p1 = NIL) & (p2 = NIL)
  OR (p1 # NIL) & (p2 # NIL)
    & (p1.obj(B.Par).varpar = p2.obj(B.Par).varpar)
    & SameParType(p1.obj.type, p2.obj.type)
    & SamePars(p1.next, p2.next)
END SamePars;

PROCEDURE SameProc(t1, t2: B.Type): BOOLEAN;
  RETURN (t1.base = t2.base) & (t1.nfpar = t2.nfpar)
    & SamePars(t1.fields, t2.fields)
END SameProc;

PROCEDURE CompTypes(t1, t2: B.Type): BOOLEAN;
  RETURN (t1 = t2)
  OR (t1.form = B.tInt) & (t2.form = B.tInt)
  OR B.IsStr8(t1)  & B.IsStr8(t2)
  OR B.IsStr16(t1) & B.IsStr16(t2)
  OR (t1.form IN {B.tProc, B.tPtr}) & (t2 = B.nilType)
  OR (t1.form IN {B.tRec, B.tPtr}) & (t1.form = t2.form) & IsExt(t2, t1)
  OR (t1.form = B.tProc) & (t2.form = B.tProc) & SameProc(t1, t2)
END CompTypes;

PROCEDURE CompTypes2(t1, t2: B.Type): BOOLEAN;
  RETURN CompTypes(t1, t2) OR CompTypes(t2, t1)
END CompTypes2;

PROCEDURE IsChar8Str(x: B.Object): BOOLEAN;
  RETURN (x IS B.Str8) & (x(B.Str8).len <= 2)
END IsChar8Str;

PROCEDURE IsChar16Str(x: B.Object): BOOLEAN;
  RETURN (x IS B.Str16) & (x(B.Str16).len <= 2)
END IsChar16Str;

PROCEDURE CheckInt(x: B.Object);
BEGIN
  IF x.type.form # B.tInt THEN Mark8(`not int`) END
END CheckInt;

PROCEDURE CheckInt2(x: B.Object);
BEGIN
  IF x.type # B.intType THEN Mark8(`not INTEGER`) END
END CheckInt2;

PROCEDURE CheckBool(x: B.Object);
BEGIN
  IF x.type # B.boolType THEN Mark8(`not bool`) END
END CheckBool;

PROCEDURE CheckSet(x: B.Object);
BEGIN
  IF x.type # B.setType THEN Mark8(`not set`) END
END CheckSet;

PROCEDURE CheckReal(x: B.Object);
BEGIN
  IF x.type.form # B.tReal THEN Mark8(`not real number`) END
END CheckReal;

PROCEDURE TypeTestable(x: B.Object): BOOLEAN;
  RETURN (x.type.form = B.tPtr) & (x.type.base # NIL)
  OR (x.type.form = B.tRec) & IsVarPar(x)
END TypeTestable;

PROCEDURE CheckVar(x: B.Object;  ronly: BOOLEAN);
VAR op: INTEGER;
BEGIN
  IF x.class = B.cNode THEN op := x(B.Node).op END;
  IF x IS B.Var THEN
    IF ~ronly & x(B.Var).ronly THEN Mark8(`read only`) END
  ELSIF (x.class = B.cNode)
    & ((op = S.arrow) OR (op = S.period)
    OR (op = S.lparen) OR (op = S.lbrak))
  THEN
    IF ~ronly & x(B.Node).ronly THEN Mark8(`read only`) END
  ELSE Mark8(`not var`)
  END
END CheckVar;

PROCEDURE CheckStrLen(xtype: B.Type;  y: B.Object);
BEGIN
  IF (xtype.len >= 0) & (    (y IS B.Str8)  & (y(B.Str8).len >  xtype.len)
                         OR  (y IS B.Str16) & (y(B.Str16).len > xtype.len)) THEN
    Mark8(`string longer than dest`)
  END
END CheckStrLen;

PROCEDURE CheckPar(fpar: B.Par;  x: B.Object);
VAR xtype, ftype: B.Type;  xform, fform: INTEGER;
BEGIN xtype := x.type;  ftype := fpar.type;
  IF IsOpenArray(ftype) THEN CheckVar(x, fpar.ronly);
    IF IsOpenArray0(xtype) & ~ftype.notag THEN Mark8(`untagged open array`)
    ELSIF CompArray(ftype, xtype)
       OR (ftype.base = B.byteType)
       OR B.IsStr8(xtype)  & B.IsStr8(ftype)
       OR B.IsStr16(xtype) & B.IsStr16(ftype) THEN (*valid*)
    ELSE Mark8(`invalid par type`)
    END
  ELSIF ~fpar.varpar THEN
    IF ~CompTypes(ftype, xtype) THEN
      IF ((ftype = B.char16Type) & (x IS B.Str16) & (x(B.Str16).len <= 2))
      OR ((ftype = B.char8Type)  & (x IS B.Str8)  & (x(B.Str8).len  <= 2))
      THEN (*valid*) ELSE Mark8(`invalid par type`)
      END
    ELSIF B.IsStr8(ftype) OR B.IsStr16(ftype) THEN CheckStrLen(ftype, x)
    END
  ELSIF fpar.varpar THEN
    CheckVar(x, fpar.ronly);  xform := xtype.form;  fform := ftype.form;
    IF (xtype = ftype) OR CompArray(xtype, ftype) & (ftype.len = xtype.len)
    OR (fform = B.tRec) & (xform = B.tRec) & IsExt0(xtype, ftype)
    THEN (*valid*) ELSE Mark8(`invalid par type`)
    END
  END
END CheckPar;

PROCEDURE CheckLeft(x: B.Object;  op: INTEGER);
BEGIN
  IF (op >= S.eql) & (op <= S.geq) THEN
    IF IsOpenArray0(x.type) THEN Mark8(`untagged open array`)
    ELSIF (x.type.form IN B.typCmp) OR B.IsStr8(x.type) OR B.IsStr16(x.type)
    OR (op <= S.neq) & (x.type.form IN B.typEql) THEN (*valid*)
    ELSE Mark8(`Invalid type`)
    END
  ELSIF op = S.is THEN
    IF TypeTestable(x) THEN (*valid*) ELSE Mark8(`Invalid type`) END
  END
END CheckLeft;

PROCEDURE Check1(x: B.Object;  forms: SET);
BEGIN
  IF ~(x.type.form IN forms) THEN Mark8(`Invalid type`) END
END Check1;

PROCEDURE Str8ToChar8(x: B.Object): B.Object;
  RETURN B.NewConst(B.char8Type, B.str8buf[x(B.Str8).bufpos])
END Str8ToChar8;

PROCEDURE Str16ToChar16(x: B.Object): B.Object;
  RETURN B.NewConst(B.char16Type, ORD(B.str16buf[x(B.Str16).bufpos]))
END Str16ToChar16;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE NewIdent(name: S.IdStr8): B.Ident;
VAR ident, p: B.Ident;
BEGIN
  NEW(ident);  ident.name := name;  ident.next := NIL;  ident.export := FALSE;
  IF B.topScope.first = NIL THEN B.topScope.first := ident
  ELSE p := B.topScope.first;
    WHILE (p.next # NIL) & (p.name # name) DO p := p.next END;
    IF p.name = name THEN Mark8(`Ident already used`);  ident := NIL
    ELSE p.next := ident
    END
  END;
  RETURN ident
END NewIdent;

PROCEDURE NewNode(op: INTEGER;  x, y: B.Object): B.Node;
VAR z: B.Node;
BEGIN NEW(z);
  z.class := B.cNode;  z.op := op;  z.sourcePos := S.SourcePos();
  z.left := x;  z.right := y;  z.ronly := FALSE;
  RETURN z
END NewNode;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE FindIdent(): B.Object;
VAR found: BOOLEAN;  xlev: INTEGER;
    x: B.Object;  ident: B.Ident;  scope: B.Scope;
BEGIN scope := B.topScope;  found := FALSE;
  WHILE (scope # NIL) & ~found DO ident := scope.first;
    WHILE (ident # NIL) & (ident.name # S.id8) DO ident := ident.next END;
    IF ident # NIL THEN x := ident.obj;  found := TRUE;
      IF x = NIL THEN Mark8(`identifier undefined`)
      ELSIF x IS B.Var THEN xlev := x(B.Var).lev;
        IF (xlev > 0) & (xlev # B.curLev) THEN
          Mark8(`Access to non strictly local variable`)
        END
      END
    ELSE scope := scope.dsc
    END
  END;
  RETURN x
END FindIdent;

PROCEDURE CheckImport(x: B.Object;  mod: B.Module);
VAR t: B.Object;  tp: B.Type;
BEGIN
  IF (x IS B.Var) & (x(B.Var).lev < -1) & (x(B.Var).adr = 0)
  OR (x IS B.Proc) & (x(B.Proc).lev < -1) & (x(B.Proc).adr = 0)
  THEN G.AllocImport(x, mod)
  ELSIF x.class = B.cType THEN
    IF x.type.form = B.tRec THEN
      IF x.type.adr = 0 THEN G.AllocImport(x, mod) END
    ELSIF x.type.form = B.tPtr THEN tp := x.type.base;
      IF (tp = NIL) & (S.errCnt # 0) THEN (* ignore *)
      ELSIF tp.adr = 0 THEN t := tp.obj;
        IF t = NIL THEN t := B.NewTypeObj(tp) END;
        G.AllocImport(t, mod)
      END
    END
  END
END CheckImport;

PROCEDURE qualident(): B.Object;
VAR x: B.Object;  mod: B.Module;  ident: B.Ident;
BEGIN x := FindIdent();  GetSym;
  IF (x # NIL) & (x IS B.Module) & (sym = S.period) THEN GetSym;
    IF sym = S.ident THEN
      mod := x(B.Module);  ident := mod.first;
      WHILE (ident # NIL) & (ident.name # S.id8) DO
        ident := ident.next
      END;
      IF ident # NIL THEN
        x := ident.obj;  CheckImport(x, mod)
      ELSE Mark8(`identifier not declared`);  x := NIL
      END;  GetSym
    ELSE Missing(S.ident);  x := NIL
    END
  ELSIF (x # NIL) & (x IS B.Module) THEN
    x := NIL;  Missing(S.period)
  END;
  RETURN x
END qualident;

PROCEDURE Call(x: B.Object): B.Node;
VAR call, last: B.Node;  proc: B.Type;
    fpar: B.Ident;  nact: INTEGER;

  PROCEDURE Parameter(VAR last: B.Node;  fpar: B.Ident);
    VAR y: B.Object;  par: B.Node;
  BEGIN y := expression0();
    IF fpar # NIL THEN CheckPar(fpar.obj(B.Par), y) END;
    par := NewNode(S.par, y, NIL);  last.right := par;  last := par
  END Parameter;

BEGIN (* Call *)
  proc := x.type;  call := NewNode(S.call, x, NIL);  call.type := proc.base;
  IF sym = S.lparen THEN GetSym;
    IF sym # S.rparen THEN last := call;
      fpar := proc.fields;  Parameter(last, fpar);  nact := 1;
      WHILE sym = S.comma DO
        IF fpar # NIL THEN fpar := fpar.next END;  GetSym;
        IF sym # S.rparen THEN Parameter(last, fpar);  nact := nact + 1
        ELSE Mark8(`remove ,`)
        END
      END;
      IF nact = proc.nfpar THEN (*valid*)
      ELSIF nact > proc.nfpar THEN Mark8(`too many params`)
      ELSE Mark8(`not enough params`)
      END;
      CheckSym(S.rparen)
    ELSIF sym = S.rparen THEN
      IF proc.nfpar # 0 THEN Mark8(`need params`) END;  GetSym
    END
  ELSIF proc.nfpar # 0 THEN Mark8(`need params`)
  END;
  RETURN call
END Call;

PROCEDURE designator(): B.Object;
VAR x, y: B.Object;  fid: S.IdStr8;  fld: B.Ident;  ronly: BOOLEAN;
    node, next: B.Node;  xtype, ytype, recType: B.Type;
BEGIN x := qualident();
  IF (x = NIL) OR (x.class <= B.cType) THEN
    IF x = NIL THEN Mark8(`identifier not declared`) ELSE Mark8(`invalid value`) END;
    x := B.NewConst(B.intType, 0)
  END;
  IF x IS B.Var THEN ronly := x(B.Var).ronly END;
  WHILE sym = S.period DO
    Check1(x, {B.tPtr, B.tRec});  GetSym;
    IF sym # S.ident THEN Mark8(`no field?`)
    ELSE fid := S.id8;  recType := x.type;
      IF (recType.form = B.tPtr) & (recType.base # NIL) THEN
        x := NewNode(S.arrow, x, NIL);
        x(B.Node).ronly := FALSE;  ronly := FALSE;
        x.type := recType.base;  recType := recType.base
      END;
      IF recType.form = B.tRec THEN
        REPEAT fld := recType.fields;
          WHILE (fld # NIL) & (fld.name # fid) DO
            fld := fld.next
          END;
          IF fld # NIL THEN
            y := fld.obj;  x := NewNode(S.period, x, y);
            x.type := y.type;  x(B.Node).ronly := ronly
          ELSE recType := recType.base
          END;
          IF recType = NIL THEN Mark8(`Field not found`) END
        UNTIL (fld # NIL) OR (recType = NIL)
      END;
      GetSym
    END
  ELSIF sym = S.lbrak DO
    Check1(x, {B.tArray});  GetSym;  y := expression0();  CheckInt(y);
    xtype := x.type;  x := NewNode(S.lbrak, x, y);  x(B.Node).ronly := ronly;
    IF (xtype.form = B.tArray) & (xtype.len >= 0) THEN
      IF (y IS B.Const) & (y(B.Const).val >= xtype.len) THEN
        Mark8(`index out of range`)
      END
    END;
    IF xtype.base # NIL THEN x.type := xtype.base ELSE x.type := xtype END;
    WHILE sym = S.comma DO
      IF x.type.form # B.tArray THEN Mark8(`not multi-dimension`) END;
      GetSym;  y := expression0();  CheckInt(y);  xtype := x.type;
      x := NewNode(S.lbrak, x, y);  x(B.Node).ronly := ronly;
      IF xtype.base # NIL THEN x.type := xtype.base
      ELSE x.type := xtype
      END
    END;
    CheckSym(S.rbrak)
  ELSIF sym = S.arrow DO
    Check1(x, {B.tPtr});  xtype := x.type;  x := NewNode(S.arrow, x, NIL);
    IF xtype.base # NIL THEN x.type := xtype.base ELSE x.type := xtype END;
    x(B.Node).ronly := FALSE;  ronly := FALSE;  GetSym
  ELSIF (sym = S.lparen) & ~(x IS B.SProc) & TypeTestable(x) DO
    xtype := x.type;  GetSym;  y := NIL;
    IF sym = S.ident THEN y := qualident() ELSE Missing(S.ident) END;
    IF (y # NIL) & (y.class = B.cType) THEN ytype := y.type;
      IF y.type.form = xtype.form THEN (*valid*)
      ELSE Mark8(`invalid type`);  ytype := xtype
      END
    ELSE Mark8(`not type`);  ytype := xtype
    END;
    IF ytype # xtype THEN
      x := NewNode(S.lparen, x, y);  x(B.Node).ronly := ronly;
      IF ~IsExt(ytype, xtype) THEN Mark8(`not extension`) END;
      x.type := ytype
    END;
    CheckSym(S.rparen)
  ELSIF sym = S.lbrace DO
    IF ~B.system THEN Mark8(`Casting not allowed`) END;
    CheckInt(x);  GetSym;  y := NIL;  ytype := x.type;
    IF sym = S.ident THEN y := qualident() ELSE Missing(S.ident) END;
    IF (y # NIL) & (y.class = B.cType) THEN
      IF y.type.form = B.tRec THEN ytype := y.type
      ELSE Mark8(`not record type`)
      END
    ELSE Mark8(`not type`)
    END;
    x := NewNode(S.lbrace, x, y);  x.type := ytype;
    x(B.Node).ronly := FALSE;  ronly := FALSE;  CheckSym(S.rbrace)
  END;
  RETURN x
END designator;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE StdFunc(f: B.SProc): B.Object;
VAR par, par2: B.Node;  x, y, z: B.Object;
    ch: CHAR16;
BEGIN GetSym;
  IF f.id = S.sfABS THEN y := expression0();  Check1(y, {B.tInt, B.tReal});
    IF y IS B.Const THEN x := G.AbsConst(y)
    ELSE x := NewNode(S.sfABS, y, NIL);
      IF y.type.form = B.tInt THEN x.type := B.intType
      ELSE x.type := y.type
      END
    END
  ELSIF f.id = S.sfODD THEN y := expression0();  CheckInt(y);
    IF y IS B.Const THEN x := G.OddConst(y)
    ELSE x := NewNode(S.sfODD, y, NIL);  x.type := B.boolType
    END
  ELSIF f.id = S.sfLEN THEN y := designator();  Check1(y, {B.tArray, B.tStr16});
    IF (y.type.form = B.tArray) & (y.type.len >= 0) THEN
      x := B.NewConst(B.intType, y.type.len)
    ELSIF y.type.form = B.tStr8 THEN
      x := B.NewConst(B.intType, y(B.Str8).len)
    ELSIF y.type.form = B.tStr16 THEN
      x := B.NewConst(B.intType, y(B.Str16).len)
    ELSIF ~y.type.notag THEN
      x := NewNode(S.sfLEN, y, NIL);  x.type := B.intType
    ELSE Mark8(`open array without length tag`)
    END
  ELSIF (f.id >= S.sfLSL) & (f.id <= S.sfROR) THEN
    y := expression0();  CheckInt(y);
    CheckSym(S.comma);  z := expression0();  CheckInt(z);
    IF (y IS B.Const) & (z IS B.Const) THEN x := G.ShiftConst(f.id, y, z)
    ELSE x := NewNode(f.id, y, z);  x.type := B.intType
    END
  ELSIF f.id = S.sfFLOOR THEN y := expression0();  CheckReal(y);
    IF y IS B.Const THEN x := G.FloorConst(y)
    ELSE x := NewNode(S.sfFLOOR, y, NIL);  x.type := B.intType
    END
  ELSIF f.id = S.sfFLT THEN y := expression0();  CheckInt(y);
    IF y IS B.Const THEN x := G.FltConst(y)
    ELSE x := NewNode(S.sfFLT, y, NIL);  x.type := B.realType
    END
  ELSIF f.id = S.sfORD THEN y := expression0();
    IF (y.type = B.str8Type)  & (y(B.Str8).len  <= 2)
    OR (y.type = B.str16Type) & (y(B.Str16).len <= 2) THEN (* ORD ok *)
    ELSE
      Check1(y, {B.tSet, B.tBool, B.tChar8, B.tChar16})
    END;
    IF IsConst(y) THEN x := G.TypeTransferConst(B.intType, y)
    ELSE x := NewNode(S.sfORD, y, NIL);  x.type := B.intType
    END
  ELSIF f.id = S.sfCHR8 THEN y := expression0();  CheckInt(y);
    IF y IS B.Const THEN x := G.TypeTransferConst(B.char8Type, y)
    ELSE x := NewNode(S.sfCHR8, y, NIL);  x.type := B.char8Type
    END
  ELSIF f.id = S.sfCHR THEN y := expression0();  CheckInt(y);
    IF y IS B.Const THEN x := G.TypeTransferConst(B.char16Type, y)
    ELSE x := NewNode(S.sfCHR, y, NIL);  x.type := B.char16Type
    END
  ELSIF f.id = S.sfADR THEN
    y := expression0();  CheckVar(y, TRUE);
    x := NewNode(S.sfADR, y, NIL);  x.type := B.intType
  ELSIF f.id = S.sfSIZE THEN y := qualident();
    IF y.class # B.cType THEN Mark8(`not type`) END;
    x := B.NewConst(B.intType, y.type.size)
  ELSIF f.id = S.sfBIT THEN
    y := expression0();  CheckInt(y);
    CheckSym(S.comma);  z := expression0();  CheckInt(z);
    x := NewNode(S.sfBIT, y, z);  x.type := B.boolType
  ELSIF f.id = S.sfVAL THEN y := qualident();
    IF y.class # B.cType THEN Mark8(`not type`)
    ELSIF y.type.form IN {B.tArray, B.tRec} THEN Mark8(`not scalar`)
    END;
    CheckSym(S.comma);  z := expression0();
    IF z.type.form IN {B.tArray, B.tRec} THEN Mark8(`not scalar`)
    ELSIF (z IS B.Str8)  & (z(B.Str8).len  > 2)
       OR (z IS B.Str16) & (z(B.Str16).len > 2) THEN Mark8(`not scalar`)
    END;
    IF IsConst(z) THEN x := G.TypeTransferConst(y.type, z)
    ELSE x := NewNode(S.sfVAL, z, NIL);  x.type := y.type
    END
  ELSIF f.id = S.sfNtCurrentTeb THEN
    x := NewNode(f.id, NIL, NIL);  x.type := B.intType
  ELSIF f.id = S.sfCAS THEN
    x := expression0();  CheckInt(x);  CheckVar(x, FALSE);
    CheckSym(S.comma);  y := expression0();  CheckInt(y);
    CheckSym(S.comma);  z := expression0();  CheckInt(z);
    y := NewNode(S.null, y, z);  x := NewNode(S.sfCAS, x, y);
    x.type := B.intType
  ELSE ASSERT(FALSE)
  END;
  CheckSym(S.rparen);
  RETURN x
END StdFunc;

PROCEDURE element(): B.Object;
VAR x, y: B.Object;
BEGIN
  x := expression0();  CheckInt(x);  G.CheckSetElement(x);
  IF sym = S.upto THEN
    GetSym;  y := expression0();  CheckInt(y);  G.CheckSetElement(y);
    IF IsConst(x) & IsConst(y) THEN x := G.ConstRangeSet(x, y)
    ELSE x := NewNode(S.upto, x, y);  x.type := B.setType
    END;
  ELSIF IsConst(x) THEN x := G.ConstSingletonSet(x)
  ELSE x := NewNode(S.bitset, x, NIL);  x.type := B.setType
  END;
  RETURN x
END element;

PROCEDURE set(): B.Object;
VAR const, x, y: B.Object;  node, next: B.Node;
BEGIN
  const := B.NewConst(B.setType, 0);  GetSym;
  IF sym # S.rbrace THEN y := element();
    IF ~IsConst(y) THEN x := y
    ELSE const := G.FoldConst(S.plus, const, y)
    END;
    WHILE sym = S.comma DO GetSym;
      IF sym # S.rbrace THEN y := element();
        IF IsConst(y) THEN const := G.FoldConst(S.plus, const, y)
        ELSIF x # NIL THEN
          x := NewNode(S.plus, x, y);  x.type := B.setType
        ELSE x := y
        END
      ELSE Mark8(`remove ,`)
      END
    END;
    IF (const(B.Const).val # 0) & (x # NIL) THEN
      x := NewNode(S.plus, x, const);  x.type := B.setType
    END
  END;
  CheckSym(S.rbrace);  IF x = NIL THEN x := const END;
  RETURN x
END set;

PROCEDURE factor(): B.Object;
VAR x: B.Object;
BEGIN
  IF    sym = S.int     THEN x := B.NewConst(B.intType, S.ival);   GetSym
  ELSIF sym = S.real    THEN x := B.NewConst(B.realType, S.ival);  GetSym
  ELSIF sym = S.string8 THEN x := B.NewStr8 (S.str8, S.slen);      GetSym
  ELSIF sym = S.string  THEN x := B.NewStr16(S.str, S.slen);       GetSym
  ELSIF sym = S.nil     THEN x := B.NewConst(B.nilType, 0);        GetSym
  ELSIF sym = S.true    THEN x := B.NewConst(B.boolType, 1);       GetSym
  ELSIF sym = S.false   THEN x := B.NewConst(B.boolType, 0);       GetSym
  ELSIF sym = S.lbrace  THEN x := set()
  ELSIF sym = S.ident   THEN x := designator();
    IF x.class = B.cSProc THEN Mark8(`not function`);
      x := B.NewConst(B.intType, 0)
    ELSIF x.class = B.cSFunc THEN
      IF sym # S.lparen THEN Mark8(`invalid factor`);
        x := B.NewConst(B.intType, 0)
      ELSE x := StdFunc(x(B.SProc))
      END
    ELSIF (sym = S.lparen) & (x.type.form = B.tProc) THEN
      IF x.type.base = NIL THEN Mark8(`not function`) END;
      x := Call(x);  IF x.type = NIL THEN x.type := B.intType END
    END
  ELSIF sym = S.lparen THEN GetSym;  x := expression0();  CheckSym(S.rparen)
  ELSIF sym = S.not THEN GetSym;  x := factor();  CheckBool(x);
    IF IsConst(x) THEN x := G.NegateConst(x)
    ELSE x := NewNode(S.not, x, NIL);  x.type := B.boolType
    END
  ELSE Mark8(`Invalid factor`);  x := B.NewConst(B.intType, 0)
  END;
  RETURN x
END factor;

PROCEDURE term(): B.Object;
VAR x, y: B.Object;  xtype: B.Type;  op: INTEGER;
BEGIN x := factor();
  WHILE sym = S.times DO
    Check1(x, {B.tInt, B.tReal, B.tSet});  GetSym;  y := factor();
    IF ~CompTypes(x.type, y.type) THEN Mark8(`invalid type`) END;
    IF IsConst(x) & IsConst(y) THEN x := G.FoldConst(S.times, x, y)
    ELSE xtype := x.type;  x := NewNode(S.times, x, y);  x.type := xtype
    END
  ELSIF sym = S.rdiv DO
    Check1(x, {B.tReal, B.tSet});  GetSym;  y := factor();
    IF ~CompTypes(x.type, y.type) THEN Mark8(`invalid type`) END;
    IF IsConst(x) & IsConst(y) THEN x := G.FoldConst(S.rdiv, x, y)
    ELSE xtype := x.type;  x := NewNode(S.rdiv, x, y);  x.type := xtype
    END
  ELSIF (sym = S.div) OR (sym = S.mod) DO
    CheckInt(x);  op := sym;  GetSym;  y := factor();  CheckInt(y);
    IF IsConst(x) & IsConst(y) THEN x := G.FoldConst(op, x, y)
    ELSE x := NewNode(op, x, y);  x.type := B.intType
    END
  ELSIF sym = S.and DO
    CheckBool(x);  GetSym;  y := factor();  CheckBool(y);
    IF IsConst(x) & IsConst(y) THEN x := G.FoldConst(S.and, x, y)
    ELSE x := NewNode(S.and, x, y);  x.type := B.boolType
    END
  END;
  RETURN x
END term;

PROCEDURE SimpleExpression(): B.Object;
VAR x, y: B.Object;  xtype: B.Type;  op: INTEGER;
BEGIN
  IF sym = S.plus THEN GetSym;  x := term()
  ELSIF sym = S.minus THEN
    GetSym;  x := term();  xtype := x.type;
    Check1(x, {B.tInt, B.tReal, B.tSet});
    IF IsConst(x) THEN x := G.NegateConst(x)
    ELSE x := NewNode(S.minus, x, NIL);  x.type := xtype
    END
  ELSE x := term()
  END;
  WHILE (sym = S.plus) OR (sym = S.minus) DO
    Check1(x, {B.tInt, B.tReal, B.tSet});  op := sym;  GetSym;  y := term();
    IF ~CompTypes(x.type, y.type) THEN Mark8(`invalid type`) END;
    IF IsConst(x) & IsConst(y) THEN x := G.FoldConst(op, x, y)
    ELSE xtype := x.type;  x := NewNode(op, x, y);  x.type := xtype
    END
  ELSIF sym = S.or DO
    CheckBool(x);  GetSym;  y := term();  CheckBool(y);
    IF IsConst(x) & IsConst(y) THEN x := G.FoldConst(S.or, x, y)
    ELSE x := NewNode(S.or, x, y);  x.type := B.boolType
    END
  END;
  RETURN x
END SimpleExpression;

PROCEDURE expression(): B.Object;
VAR x, y: B.Object;  xt, tp: B.Type;  op: INTEGER;
BEGIN x := SimpleExpression();
  IF (sym >= S.eql) & (sym <= S.geq) THEN
    CheckLeft(x, sym);  op := sym;  GetSym;  y := SimpleExpression();
    IF x.type = y.type THEN
      IF IsConst(x) & IsConst(y) THEN x := G.FoldConst(op, x, y)
      ELSE x := NewNode(op, x, y);  x.type := B.boolType
      END
    ELSIF CompTypes2(x.type, y.type) THEN
      IF IsOpenArray0(y.type) THEN Mark8(`untagged open array`) END;
      IF IsConst(x) & IsConst(y) THEN x := G.FoldConst(op, x, y)
      ELSE x := NewNode(op, x, y);  x.type := B.boolType
      END
    ELSIF (x.type = B.char16Type) & IsChar16Str(y) THEN
      x := NewNode(op, x, Str16ToChar16(y));  x.type := B.boolType
    ELSIF (y.type = B.char16Type) & IsChar16Str(x) THEN
      x := NewNode(op, Str16ToChar16(x), y);  x.type := B.boolType
    ELSIF (x.type = B.char8Type) & IsChar8Str(y) THEN
      x := NewNode(op, x, Str8ToChar8(y));  x.type := B.boolType
    ELSIF (y.type = B.char8Type) & IsChar8Str(x) THEN
      x := NewNode(op, Str8ToChar8(x), y);  x.type := B.boolType
    ELSE Mark8(`invalid type`)
    END
  ELSIF sym = S.in THEN
    CheckInt(x);  G.CheckSetElement(x);
    GetSym;  y := SimpleExpression();  CheckSet(y);
    IF IsConst(x) & IsConst(y) THEN x := G.FoldConst(op, x, y)
    ELSE x := NewNode(S.in, x, y);  x.type := B.boolType
    END
  ELSIF sym = S.is THEN
    CheckLeft(x, S.is);  GetSym;  xt := x.type;
    IF sym = S.ident THEN y := qualident() ELSE Missing(S.ident) END;
    IF (y # NIL) & (y.class = B.cType) THEN tp := y.type;
      IF xt.form = tp.form THEN (* valid *)
      ELSE Mark8(`invalid type`);  tp := xt
      END
    ELSE Mark8(`not type`);  tp := xt
    END;
    IF xt # tp THEN
      IF ~IsExt(tp, xt) THEN Mark8(`not extension`) END;
      x := NewNode(S.is, x, y);  x.type := B.boolType
    ELSE x := B.NewConst(B.boolType, 1)
    END
  END;
  RETURN x
END expression;

PROCEDURE ConstExpression(): B.Object;
VAR x: B.Object;
BEGIN x := expression();
  IF IsConst(x) THEN (*valid*)
  ELSE Mark8(`not const`);  x := B.NewConst(B.intType, 0)
  END;
  RETURN x
END ConstExpression;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE StdProc(f: B.SProc): B.Object;
VAR x, y, z, t: B.Object;  bType: B.Type;  hasParen: BOOLEAN;
BEGIN hasParen := TRUE;
  IF (f.id # S.spINT3) & (f.id # S.spPAUSE) THEN CheckSym(S.lparen)
  ELSIF sym = S.lparen THEN GetSym ELSE hasParen := FALSE
  END;
  IF (f.id = S.spINC) OR (f.id = S.spDEC) THEN
    x := designator();  CheckInt(x);  CheckVar(x, FALSE);
    IF sym = S.comma THEN
      GetSym;  y := expression();  CheckInt(y);  x := NewNode(f.id, x, y)
    ELSE x := NewNode(f.id, x, NIL)
    END
  ELSIF (f.id = S.spINCL) OR (f.id = S.spEXCL) THEN
    x := designator();  CheckSet(x);  CheckVar(x, FALSE);  CheckSym(S.comma);
    y := expression();  CheckInt(y);  x := NewNode(f.id, x, y)
  ELSIF f.id = S.spNEW THEN
    IF ~B.Flag.rtl THEN Mark8(`Must have RTL to call NEW`) END;
    x := designator();  Check1(x, {B.tPtr});
    CheckVar(x, FALSE);  bType := x.type.base;
    IF (S.errCnt # 0) & (bType = NIL) THEN (* ignore *)
    ELSIF (bType.mod # NIL) & (bType.adr = 0) THEN
      t := bType.obj;  IF t = NIL THEN t := B.NewTypeObj(bType) END;
      G.AllocImport(t, bType.mod)
    END;
    x := NewNode(S.spNEW, x, NIL)
  ELSIF f.id = S.spASSERT THEN
    x := expression();  CheckBool(x);  x := NewNode(S.spASSERT, x, NIL)
  ELSIF f.id = S.spPACK THEN
    x := designator();  CheckReal(x);  CheckVar(x, FALSE);  CheckSym(S.comma);
    y := expression();  CheckInt(y);  x := NewNode(S.spPACK, x, y)
  ELSIF f.id = S.spUNPK THEN
    x := designator();  CheckReal(x);  CheckVar(x, FALSE);  CheckSym(S.comma);
    y := designator();  CheckInt(y);  CheckVar(y, FALSE);
    x := NewNode(S.spUNPK, x, y)
  ELSIF f.id = S.spGET THEN
    x := expression();  CheckInt(x);  CheckSym(S.comma);
    y := designator();  CheckVar(y, FALSE);  x := NewNode(S.spGET, x, y);
    IF y.type.form IN {B.tArray, B.tRec} THEN Mark8(`invalid type`) END
  ELSIF f.id = S.spPUT THEN
    x := expression();  CheckInt(x);  CheckSym(S.comma);
    y := expression();  x := NewNode(S.spPUT, x, y);
    IF y.type.form IN {B.tArray, B.tRec} THEN Mark8(`invalid type`) END
  ELSIF f.id = S.spCOPY THEN
    x := expression();  CheckInt(x);  CheckSym(S.comma);
    y := expression();  CheckInt(y);  CheckSym(S.comma);
    z := expression();  CheckInt(z);
    x := NewNode(S.spCOPY, x, NewNode(S.null, y, z))
  ELSIF f.id = S.spLoadLibraryA THEN
    x := designator();  CheckVar(x, FALSE);
    IF x.type # B.intType THEN Mark8(`not INTEGER`) END;  CheckSym(S.comma);
    y := expression();  IF ~B.IsStr8(y.type) THEN Mark8(`not string`) END;
    x := NewNode(S.spLoadLibraryA, x, y)
  ELSIF f.id = S.spLoadLibraryW THEN
    x := designator();  CheckVar(x, FALSE);
    IF x.type # B.intType THEN Mark8(`not INTEGER`) END;  CheckSym(S.comma);
    y := expression();  IF ~B.IsStr16(y.type) THEN Mark8(`not string`) END;
    x := NewNode(S.spLoadLibraryW, x, y)
  ELSIF f.id = S.spGetProcAddress THEN
    x := designator();  CheckVar(x, FALSE);
    IF (x.type.form # B.tProc) & (x.type # B.intType) THEN
      Mark8(`not INTEGER or procedure variable`)
    END;  CheckSym(S.comma);
    y := expression();  CheckInt(y);  CheckSym(S.comma);
    z := expression();  CheckInt(z);
    x := NewNode(S.spGetProcAddress, x, NewNode(S.null, y, z))
  ELSIF f.id = S.spINT3 THEN
    x := NewNode(S.spINT3, NIL, NIL)
  ELSIF f.id = S.spPAUSE THEN
    x := NewNode(S.spPAUSE, NIL, NIL)
  ELSE Mark8(`unsupported`);
  END;
  IF hasParen THEN CheckSym(S.rparen) END;
  RETURN x
END StdProc;

PROCEDURE If(lev: INTEGER): B.Node;
VAR x: B.Object;  if, then: B.Node;
BEGIN
  GetSym;  x := expression();  CheckBool(x);  CheckSym(S.then);
  then := NewNode(S.then, StatementSequence0(), NIL);
  if := NewNode(S.if, x, then);
  IF sym = S.elsif THEN then.right := If(lev+1)
  ELSIF sym = S.else THEN GetSym;  then.right := StatementSequence0()
  ELSE then.right := NIL
  END;
  IF lev = 0 THEN CheckSym(S.end) END;
  RETURN if
END If;

PROCEDURE While(lev: INTEGER): B.Node;
VAR x: B.Object;  while, do: B.Node;
BEGIN
  GetSym;  x := expression();  CheckBool(x);  CheckSym(S.do);
  do := NewNode(S.do, StatementSequence0(), NIL);
  while := NewNode(S.while, x, do);
  IF sym = S.elsif THEN do.right := While(lev+1)
  ELSE do.right := NIL
  END;
  IF lev = 0 THEN CheckSym(S.end) END;
  RETURN while
END While;

PROCEDURE For(): B.Node;
VAR x: B.Object;  for, control, beg, end: B.Node;
BEGIN
  for := NewNode(S.for, NIL, NIL);  GetSym;
  IF sym = S.ident THEN x := FindIdent();  GetSym ELSE Missing(S.ident) END;
  IF (x # NIL) THEN CheckInt2(x);  CheckVar(x, FALSE) END;
  control := NewNode(S.null, x, NIL);  for.left := control;
  CheckSym(S.becomes);  x := expression();  CheckInt2(x);
  beg := NewNode(S.null, x, NIL);  control.right := beg;
  CheckSym(S.to);  x := expression();  CheckInt2(x);
  end := NewNode(S.null, x, NIL);  beg.right := end;
  IF sym = S.by THEN
    GetSym;  x := ConstExpression();  CheckInt(x);
    end.right := NewNode(S.by, x, NIL)
  END;
  CheckSym(S.do);  for.right := StatementSequence0();  CheckSym(S.end);
  RETURN for
END For;

PROCEDURE Case(): B.Node;
VAR x, y: B.Object;  xform: INTEGER;
    case: B.Node;  isTypeCase: BOOLEAN;

  PROCEDURE TypeCase(x: B.Object): B.Node;
  VAR bar, colon: B.Node;  y, z: B.Object;  org, xt, yt: B.Type;
  BEGIN
    IF sym = S.ident THEN
      xt := x.type;  org := x.type;  y := qualident();
      IF (y # NIL) & (y.class = B.cType) THEN yt := y.type;
        IF xt.form = yt.form THEN (* valid *)
        ELSE Mark8(`invalid type`);  yt := xt
        END
      ELSE Mark8(`not type`);  yt := xt
      END;
      IF yt # xt THEN y := NewNode(S.is, x, y);
        IF IsExt(yt, xt) THEN x.type := yt
        ELSE Mark8(`not extension`)
        END
      ELSE y := B.NewConst(B.boolType, 1)
      END;
      CheckSym(S.colon);  z := StatementSequence0();  x.type := org;
      colon := NewNode(S.colon, z, NIL);
      IF sym = S.bar THEN GetSym;  colon.right := TypeCase(x) END;
      bar := NewNode(S.bar, y, colon)
    ELSIF sym = S.bar THEN GetSym;  bar := TypeCase(x)
    END;
    RETURN bar
  END TypeCase;

  PROCEDURE label(x: B.Object;  VAR y: B.Object);
  VAR xform: INTEGER;
  BEGIN xform := x.type.form;
    IF sym = S.int THEN y := factor();
      IF xform # B.tInt THEN Mark8(`Invalid value`) END
    ELSIF sym = S.string THEN
      IF xform # B.tChar16 THEN Mark8(`Invalid value`) END;
      IF S.slen > 2 THEN Mark8(`not char`) END;
      y := B.NewConst(B.char16Type, ORD(S.str[0]));  GetSym
    ELSIF sym = S.ident THEN y := qualident();
      IF y = NIL THEN Mark8(`Invalid value`)
      ELSIF y IS B.Const THEN
        IF xform = B.tInt THEN CheckInt(y)
        ELSIF xform = B.tChar16 THEN
          IF y.type.form # B.tChar16 THEN Mark8(`not char`) END
        END
      ELSIF y IS B.Str8 THEN
        IF xform # B.tChar8 THEN Mark8(`Invalid value`) END;
        IF y(B.Str8).len > 2 THEN Mark8(`not char`) END;
        y := B.NewConst(B.char8Type, ORD(S.str[0]));  GetSym
      ELSIF y IS B.Str16 THEN
        IF xform # B.tChar16 THEN Mark8(`Invalid value`) END;
        IF y(B.Str16).len > 2 THEN Mark8(`not char`) END;
        y := B.NewConst(B.char16Type, ORD(S.str[0]));  GetSym
      ELSE Mark8(`Invalid value`);  y := NIL
      END
    ELSE Mark8(`Integer or char required`)
    END
  END label;

  PROCEDURE LabelRange(x: B.Object): B.Node;
  VAR y: B.Object;  cond: B.Node;
  BEGIN label(x, y);
    IF sym # S.upto THEN cond := NewNode(S.eql, x, y)
    ELSE
      cond := NewNode(S.geq, x, y);  GetSym;  label(x, y);
      cond := NewNode(S.and, cond, NewNode(S.leq, x, y))
    END;
    RETURN cond
  END LabelRange;

  PROCEDURE NumericCase(x: B.Object): B.Node;
  VAR bar, colon: B.Node;  y: B.Node;
  BEGIN
    IF (sym = S.int) OR (sym = S.string) OR (sym = S.ident) THEN
      y := LabelRange(x);
      WHILE sym = S.comma DO
        GetSym;  y := NewNode(S.or, y, LabelRange(x))
      END;  CheckSym(S.colon);
      colon := NewNode(S.colon, StatementSequence0(), NIL);
      IF sym = S.bar THEN GetSym;  colon.right := NumericCase(x) END;
      bar := NewNode(S.bar, y, colon)
    ELSIF sym = S.bar THEN GetSym;  bar := NumericCase(x)
    END
    RETURN bar
  END NumericCase;

BEGIN (* Case *)
  case := NewNode(S.case, NIL, NIL);
  GetSym;  x := expression();  xform := x.type.form;
  isTypeCase := (x.class = B.cVar) & TypeTestable(x);
  IF xform = B.tInt THEN
    y := B.NewTempVar(B.intType);  case.left := NewNode(S.becomes, y, x)
  ELSIF (xform = B.tChar16) OR IsChar16Str(x) THEN
    y := B.NewTempVar(B.char16Type);  case.left := NewNode(S.becomes, y, x)
  ELSIF isTypeCase THEN (*valid*)
  ELSE Mark8(`invalid case expression`)
  END;
  CheckSym(S.of);
  IF isTypeCase THEN case.right := TypeCase(x)
  ELSE case.right := NumericCase(y)
  END;
  CheckSym(S.end);
  RETURN case
END Case;

PROCEDURE StatementSequence(): B.Node;
VAR x, y: B.Object;  statseq, stat, nextstat, repeat: B.Node;
BEGIN
  statseq := NewNode(S.semicolon, NIL, NIL);  stat := statseq;
  REPEAT (*sync*)
    IF (sym = S.ident) OR (sym >= S.semicolon)
    OR (sym >= S.if) & (sym <= S.for) THEN (*valid*)
    ELSE
      Mark8(`Statement?`);
      REPEAT GetSym UNTIL (sym = S.ident) OR (sym >= S.semicolon)
    END;
    IF sym = S.ident THEN x := designator();
      IF sym = S.becomes THEN CheckVar(x, FALSE);
        IF x.type.notag THEN Mark8(`untagged open array`) END;
        GetSym;  y := expression();
        IF x.type = y.type THEN stat.left := NewNode(S.becomes, x, y)
        ELSIF CompTypes(x.type, y.type) THEN
          IF B.IsStr8(x.type) OR B.IsStr16(x.type) THEN CheckStrLen(x.type, y) END;
          stat.left := NewNode(S.becomes, x, y)
        ELSIF (x.type = B.char8Type) & IsChar8Str(y) THEN
          stat.left := NewNode(S.becomes, x, Str8ToChar8(y))
        ELSIF (x.type = B.char16Type) & IsChar16Str(y) THEN
          stat.left := NewNode(S.becomes, x, Str16ToChar16(y))
        ELSIF CompArray(x.type, y.type) & IsOpenArray(y.type) THEN
          IF y.type.notag THEN Mark8(`untagged open array`) END;
          stat.left := NewNode(S.becomes, x, y)
        ELSE
          Mark8(`Invalid assignment`)
        END
      ELSIF sym = S.eql THEN
        Mark8(`'=' Should be ':='`);  GetSym;  y := expression()
      ELSIF x.type.form = B.tProc THEN
        IF x.type.base # NIL THEN Mark8(`Not proper procedure`) END;
        stat.left := Call(x)
      ELSIF x.class = B.cSProc THEN
        stat.left := StdProc(x(B.SProc))
      ELSE Mark8(`Invalid statement`)
      END
    ELSIF sym = S.if THEN stat.left := If(0)
    ELSIF sym = S.while THEN stat.left := While(0)
    ELSIF sym = S.repeat THEN
      GetSym;  repeat := NewNode(S.repeat, StatementSequence0(), NIL);
      CheckSym(S.until);  x := expression();  CheckBool(x);
      repeat.right := x;  stat.left := repeat
    ELSIF sym = S.for THEN stat.left := For()
    ELSIF sym = S.case THEN stat.left := Case()
    END;
    IF sym <= S.semicolon THEN CheckSym(S.semicolon);
      nextstat := NewNode(S.semicolon, NIL, NIL);
      stat.right := nextstat;  stat := nextstat
    END
  UNTIL sym > S.semicolon;
  RETURN statseq
END StatementSequence;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE CheckExport(ident: B.Ident);
BEGIN
  IF sym = S.times THEN
    IF B.curLev = 0 THEN
      IF ident # NIL THEN ident.export := TRUE END
    ELSE Mark8(`remove *`)
    END;
    GetSym
  END
END CheckExport;

PROCEDURE FormalType(): B.Type;
VAR x: B.Object;  tp: B.Type;
BEGIN tp := B.intType;
  IF sym = S.ident THEN x := qualident();
    IF (x # NIL) & (x.class = B.cType) THEN tp := x.type
    ELSE Mark8(`not type`)
    END
  ELSIF sym = S.array THEN
    tp := B.NewArray(-1);  GetSym;
    IF sym = S.lbrak THEN GetSym;
      IF (sym = S.ident) & (S.id8 = `untagged`) THEN
        IF B.system THEN tp.notag := TRUE
        ELSE Mark8(`untagged not allowed`)
        END;  GetSym
      END;  CheckSym(S.rbrak)
    END;
    CheckSym(S.of);
    IF sym = S.array THEN Mark8(`Multi-dim open array not supported`) END;
    tp.base := FormalType()
  END;
  RETURN tp
END FormalType;

PROCEDURE FPSection(proc: B.Type);
VAR ronly, varpar: BOOLEAN;
    first, ident: B.Ident;  tp: B.Type;
BEGIN
  IF sym = S.var THEN varpar := TRUE;  GetSym ELSE varpar := FALSE END;
  IF sym = S.ident THEN
    first := NewIdent(S.id8);  GetSym;
    WHILE sym = S.comma DO GetSym;
      IF sym = S.ident THEN
        ident := NewIdent(S.id8);  GetSym;
        IF first = NIL THEN first := ident END
      ELSE Mark8(`remove ,`)
      END
    END
  ELSE Mark8(`No params?`)
  END;
  CheckSym(S.colon);  tp := FormalType();  ident := first;
  WHILE ident # NIL DO
    ident.obj := B.NewPar(proc^, tp, varpar);
    ident.obj.ident := ident;  ident := ident.next
  END
END FPSection;

PROCEDURE FormalParameters(proc: B.Type);
VAR ident: B.Ident;  x: B.Object;
BEGIN GetSym;
  IF (sym = S.ident) OR (sym = S.var) THEN
    B.OpenScope;  FPSection(proc);
    WHILE sym = S.semicolon DO GetSym;
      IF (sym = S.ident) OR (sym = S.var) THEN FPSection(proc)
      ELSE Mark8(`param section?`)
      END
    END;
    proc.fields := B.topScope.first;  B.CloseScope
  END;
  CheckSym(S.rparen);
  IF sym = S.colon THEN GetSym;
    IF sym = S.ident THEN x := qualident() ELSE Missing(S.ident) END;
    IF (x # NIL) & (x.class = B.cType) THEN
      IF ~(x.type.form IN {B.tArray, B.tRec}) THEN proc.base := x.type
      ELSE Mark8(`invalid type`)
      END
    ELSE Mark8(`not type`)
    END
  END
END FormalParameters;

PROCEDURE PointerType(defobj: B.Object): B.Type;
VAR ptrType: B.Type;  ident: B.Ident;  x: B.Object;
    undef: UndefPtrList;
BEGIN
  ptrType := B.NewPointer();  GetSym;
  IF sym = S.lbrak THEN GetSym;
    IF (sym = S.ident) & (S.id8 = `untraced`) THEN
      IF B.system THEN ptrType.nTraced := 0
      ELSE Mark8(`untraced not allowed`)
      END;  GetSym
    END;  CheckSym(S.rbrak)
  END;
  CheckSym(S.to);  IF defobj # NIL THEN defobj.type := ptrType END;
  IF sym = S.ident THEN ident := B.universe.first;
    WHILE (ident # NIL) & (ident.name # S.id8) DO ident := ident.next END;
    IF ident # NIL THEN x := ident.obj;
      IF x = NIL THEN Mark8(`Type not defined yet`)
      ELSIF (x.class = B.cType) & (x.type.form = B.tRec) THEN
        ptrType.base := x.type
      ELSE Mark8(`not record type`)
      END
    ELSE NEW(undef);  undef.tp := ptrType;  undef.name := S.id8;
      undef.next := undefList;  undefList := undef
    END;
    GetSym
  ELSIF sym = S.record THEN ptrType.base := type0()
  ELSE Mark8(`base type?`)
  END;
  G.SetTypeSize(ptrType);
  RETURN ptrType
END PointerType;

PROCEDURE FieldList(rec: B.Type);
VAR first, field: B.Ident;  ft: B.Type;
BEGIN
  first := NewIdent(S.id8);  GetSym;  CheckExport(first);
  WHILE sym = S.comma DO GetSym;
    IF sym = S.ident THEN
      field := NewIdent(S.id8);  GetSym;  CheckExport(field);
      IF first = NIL THEN first := field END
    ELSIF sym < S.ident THEN Missing(S.ident)
    ELSE Mark8(`remove ,`)
    END
  END;
  CheckSym(S.colon);  ft := type0();  field := first;
  WHILE field # NIL DO
    field.obj := B.NewField(rec^, ft);  field := field.next
  END
END FieldList;

PROCEDURE BaseType(): B.Type;
VAR btype, p: B.Type;  x: B.Object;
BEGIN
  IF sym = S.ident THEN x := qualident();
    IF x # NIL THEN
      IF (x.class = B.cType) & (x.type.form = B.tRec) THEN
        btype := x.type
      ELSIF (x.class = B.cType) & (x.type.form = B.tPtr) THEN
        p := x.type;
        IF p.base # NIL THEN btype := p.base
        ELSE Mark8(`this type is not defined yet`)
        END
      ELSE Mark8(`not record type`)
      END;
      IF (btype # NIL) & (btype.len >= B.MaxExt) THEN
        Mark8(`max extension limit reached`);  btype := NIL
      END
    END
  ELSE Missing(S.ident)
  END;
  RETURN btype
END BaseType;

PROCEDURE length(): INTEGER;
VAR x: B.Object;  n: INTEGER;
BEGIN x := ConstExpression();  n := 0;
  IF x.type.form = B.tInt THEN n := x(B.Const).val ELSE Mark8(`not int`) END;
  IF n < 0 THEN Mark8(`invalid array length`)
  ELSIF n >= 80000000H THEN Mark8(`too long`)
  END;
  RETURN n
END length;

PROCEDURE type(): B.Type;
VAR tp, lastArr, t: B.Type;  x: B.Object;  proc: B.Proc;
    ident: B.Ident;  len: INTEGER;
BEGIN tp := B.intType;
  IF sym = S.ident THEN x := qualident();
    IF (x # NIL) & (x.class = B.cType) THEN tp := x.type
    ELSE Mark8(`not type`)
    END
  ELSIF sym = S.array THEN
    GetSym;  len := length();  tp := B.NewArray(len);  lastArr := tp;
    WHILE sym = S.comma DO GetSym;
      IF sym <= S.ident THEN len := length();
        lastArr.base := B.NewArray(len);  lastArr := lastArr.base
      ELSE Mark8(`remove ,`)
      END
    END;
    CheckSym(S.of);  lastArr.base := type();  B.CompleteArray(tp^)
  ELSIF sym = S.record THEN
    tp := B.NewRecord();  GetSym;
    IF sym = S.lbrak THEN GetSym;
      IF (sym = S.ident) & (S.id8 = `union`) THEN
        IF B.system THEN tp.union := TRUE
        ELSE Mark8(`union not allowed`)
        END;  GetSym
      END;  CheckSym(S.rbrak)
    END;
    IF sym = S.lparen THEN
      GetSym;  tp.base := BaseType();  CheckSym(S.rparen);
      IF tp.union THEN Mark8(`Cannot extend union`) END;
      IF tp.base # NIL THEN B.ExtendRecord(tp^) END
    END;
    B.OpenScope;
    IF sym = S.ident THEN FieldList(tp);
      WHILE sym = S.semicolon DO GetSym;
        IF sym = S.ident THEN FieldList(tp);
        ELSE Mark8(`no fieldlist, remove ;`)
        END
      END
    END;
    tp.fields := B.topScope.first;  B.CloseScope;  CheckSym(S.end)
  ELSIF sym = S.pointer THEN
    tp := PointerType(NIL)
  ELSIF sym = S.procedure THEN
    GetSym;  tp := B.NewProcType();
    IF sym = S.lparen THEN FormalParameters(tp) END
  ELSE Mark8(`no type?`)
  END;
  G.SetTypeSize(tp);
  RETURN tp
END type;

PROCEDURE DeclarationSequence(owner: B.Proc);
VAR first, ident, par, procid: B.Ident;  x: B.Object;  tp: B.Type;
    proc: B.Proc;  parobj: B.Par;  statseq: B.Node;
    undef, prev: UndefPtrList;
BEGIN
  IF sym = S.const THEN GetSym;
    WHILE sym = S.ident DO
      ident := NewIdent(S.id8);  GetSym;  CheckExport(ident);
      CheckSym(S.eql);  x := ConstExpression();
      IF ident # NIL THEN ident.obj := x;  x.ident := ident END;
      CheckSym(S.semicolon)
    END
  END;
  IF sym = S.type THEN GetSym;  undefList := NIL;
    WHILE sym = S.ident DO
      ident := NewIdent(S.id8);  GetSym;  CheckExport(ident);  CheckSym(S.eql);
      IF sym # S.pointer THEN
        tp := type();  x := B.NewTypeObj(tp);
        IF ident # NIL THEN ident.obj := x;  x.ident := ident END
      ELSE
        x := B.NewTypeObj(B.intType);
        IF ident # NIL THEN ident.obj := x;  x.ident := ident END;
        tp := PointerType(x);  IF tp.obj = NIL THEN tp.obj := x END
      END;
      CheckSym(S.semicolon);
      IF (ident # NIL) & (x.type.form = B.tRec) THEN
        undef := undefList;  prev := NIL;
        WHILE (undef # NIL) & (undef.name # ident.name) DO
          prev := undef;  undef := undef.next
        END;
        IF undef # NIL THEN undef.tp.base := x.type;
          IF prev # NIL THEN prev.next := undef.next
          ELSE undefList := undef.next
          END
        END
      END
    END;
    IF undefList # NIL THEN
      undefList := NIL;  Mark8(`some pointers didnt have base type`)
    END
  END;
  IF sym = S.var THEN GetSym;
    WHILE sym = S.ident DO
      first := NewIdent(S.id8);  GetSym;  CheckExport(first);
      WHILE sym = S.comma DO GetSym;
        IF sym = S.ident THEN
          ident := NewIdent(S.id8);  GetSym;  CheckExport(ident);
          IF first = NIL THEN first := ident END
        ELSE Missing(S.ident);  ident := NIL
        END
      END;
      CheckSym(S.colon);  tp := type();  ident := first;
      WHILE ident # NIL DO
        x := B.NewVar(tp);  ident.obj := x;  x.ident := ident;
        IF owner # NIL THEN
          G.SetProcVarSize(owner, x);  INC(owner.nPtr, tp.nPtr);
          INC(owner.nTraced, tp.nTraced);  INC(owner.nProc, tp.nProc)
        ELSE G.SetGlobalVarSize(x)
        END;
        ident := ident.next
      END;
      CheckSym(S.semicolon)
    END
  END;
  WHILE sym = S.procedure DO GetSym;
    IF sym # S.ident THEN procid := NIL;  Mark8(`Expected procedure name`)
    ELSE procid := NewIdent(S.id8);  GetSym;  CheckExport(procid)
    END;
    proc := B.NewProc();  tp := B.NewProcType();  proc.type := tp;
    IF sym = S.lparen THEN FormalParameters(tp) END;  CheckSym(S.semicolon);
    G.SetTypeSize(tp);  B.OpenScope;  B.IncLev(1);  par := tp.fields;
    WHILE par # NIL DO
      ident := NewIdent(par.name);  NEW(parobj);  ident.obj := parobj;
      parobj^ := par.obj(B.Par)^;  parobj.ident := ident;
      INC(parobj.lev);  par := par.next;
      IF (parobj.type.form = B.tPtr) & ~parobj.varpar
      OR (parobj.type.form = B.tRec) & (parobj.type.size = 8)
        & (parobj.type.nTraced = 1) & ~parobj.varpar
      THEN INC(proc.nTraced)
      END
    END;
    IF procid # NIL THEN procid.obj := proc;  proc.ident := procid END;
    DeclarationSequence(proc);  proc.decl := B.topScope.first;
    IF sym = S.begin THEN GetSym;  proc.statseq := StatementSequence() END;
    IF sym = S.return THEN
      IF tp.base = NIL THEN Mark8(`Not function procedure`) END;
      GetSym;  x := expression();  proc.return := x;
      IF x.type.form IN {B.tArray, B.tRec} THEN Mark8(`Invalid type`) END
    ELSIF tp.base # NIL THEN Missing(S.return)
    END;
    B.CloseScope;  B.IncLev(-1);  CheckSym(S.end);
    IF sym = S.ident THEN
      IF (procid # NIL) & (procid.name # S.id8) THEN
        Mark8(`Wrong procedure name`)
      END;
      GetSym
    ELSIF procid # NIL THEN Missing(S.ident)
    END;
    CheckSym(S.semicolon)
  END
END DeclarationSequence;

PROCEDURE import;
VAR ident: B.Ident;  name: S.IdStr8;
BEGIN
  ident := NewIdent(S.id8);  name := S.id8;  GetSym;
  IF sym = S.becomes THEN GetSym;
    IF sym = S.ident THEN name := S.id8;  GetSym ELSE Missing(S.ident) END
  END;
  IF S.errCnt = 0 THEN
    IF name = `SYSTEM` THEN B.NewSystemModule(ident)
    ELSE B.NewModule(ident, name) END
  END
END import;

PROCEDURE ImportList;
BEGIN GetSym;
  IF sym = S.ident THEN import ELSE Missing(S.ident) END;
  WHILE sym = S.comma DO GetSym;
    IF sym = S.ident THEN import ELSE Missing(S.ident) END
  END;
  CheckSym(S.semicolon)
END ImportList;

PROCEDURE Module*(): B.Node;
VAR modid: S.IdStr8;  modinit: B.Node;
BEGIN
  GetSym;  IF sym # S.ident THEN Missing(S.ident)
  ELSE modid := S.id8;  GetSym END;
  IF S.errCnt = 0 THEN
    B.Init(modid);  G.Init;  CheckSym(S.semicolon);
    IF sym = S.import THEN ImportList END
  END;
  IF S.errCnt = 0 THEN
    DeclarationSequence(NIL);
    IF sym = S.begin THEN GetSym;  modinit := StatementSequence() END;
    CheckSym(S.end);
    IF sym = S.ident THEN
      IF S.id8 # modid THEN Mark8(`Module name mismatch`) END;  GetSym
    ELSE Missing(S.ident)
    END;
    CheckSym(S.period)
  END;
  RETURN modinit
END Module;

BEGIN
  type0 := type;  expression0 := expression;
  StatementSequence0 := StatementSequence
END Parser.