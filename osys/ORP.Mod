MODULE ORP;  (*N. Wirth 1.7.97 / 17.9.2018  Oberon compiler for RISC in Oberon-07*)

IMPORT Texts, Oberon, ORS, ORB, X64, ORG, l := Listing;

  (*Author: Niklaus Wirth, 2014.
    Parser of Oberon-RISC compiler. Uses Scanner ORS to obtain symbols (tokens),
    ORB for definition of data structures and for handling import and export, and
    ORG to produce binary code. ORP performs type checking and data allocation.
    Parser is target-independent, except for part of the handling of allocations.*)

TYPE
  PtrBase     = POINTER TO PtrBaseDesc;
  PtrBaseDesc = RECORD  (*list of names of pointer base types*)
    name: ORS.Ident;
    type: ORB.Type;
    next: PtrBase
  END;

VAR
  sym:         INTEGER;                     (* last symbol read *)
  dc:          INTEGER;                     (* data counter *)
  level, exno: INTEGER;
  newSF:       BOOLEAN;                     (* option flag *)
  modid:       ORS.Ident;
  pbsList:     PtrBase;                     (* list of names of pointer base types *)
  dummy:       ORB.Object;
  W:           Texts.Writer;

  (* Forward procedures *)
  expression:  PROCEDURE(VAR x: X64.Item);
  Type:        PROCEDURE(VAR type: ORB.Type);
  FormalType:  PROCEDURE(VAR typ: ORB.Type;  dim: INTEGER);

  (* Trace *)
  Tr: l.Buffer;

PROCEDURE Check(s: INTEGER;  msg: ARRAY OF CHAR);
BEGIN
  IF sym = s THEN ORS.Get(sym) ELSE ORS.Mark(msg) END
END Check;

PROCEDURE qualident(VAR obj: ORB.Object);
BEGIN obj := ORB.thisObj();  ORS.Get(sym);
  IF obj = NIL THEN ORS.Mark("undef");  obj := dummy END;
  IF (sym = ORS.period) & (obj.class = ORB.Mod) THEN
    ORS.Get(sym);
    IF sym = ORS.ident THEN obj := ORB.thisimport(obj);  ORS.Get(sym);
      IF obj = NIL THEN ORS.Mark("undef");  obj := dummy END
    ELSE ORS.Mark("identifier expected");  obj := dummy
    END
  END
END qualident;

PROCEDURE CheckBool(VAR x: X64.Item);
BEGIN
  IF x.type.form # ORB.Bool THEN ORS.Mark("not Boolean");  x.type := ORB.boolType END
END CheckBool;

PROCEDURE CheckInt(VAR x: X64.Item);
BEGIN
  IF x.type.form # ORB.Int64 THEN ORS.Mark("not Integer");  x.type := ORB.intType END
END CheckInt;

PROCEDURE CheckReal(VAR x: X64.Item);
BEGIN
  IF x.type.form # ORB.Real THEN ORS.Mark("not Real");  x.type := ORB.realType END
END CheckReal;

PROCEDURE CheckSet(VAR x: X64.Item);
BEGIN
  IF x.type.form # ORB.Set THEN ORS.Mark("not Set");  x.type := ORB.setType END
END CheckSet;

PROCEDURE CheckSetVal(VAR x: X64.Item);
BEGIN
  IF x.type.form # ORB.Int64 THEN
    ORS.Mark("not Int");  x.type := ORB.setType
  ELSIF (x.mode = X64.Const) THEN
    IF (x.n < 0) OR (x.n >= 64) THEN ORS.Mark("invalid set") END
  END
END CheckSetVal;

PROCEDURE CheckConst(VAR x: X64.Item);
BEGIN
  IF ~((x.mode = X64.Const) OR (x.type.form IN {ORB.Proc, ORB.String})) THEN
    ORS.Mark("not a constant");  ORG.MakeConstItem(x, ORB.intType, 0);
  END
END CheckConst;

PROCEDURE CheckReadOnly(VAR x: X64.Item);
BEGIN
  IF x.readonly THEN ORS.Mark("read-only") END
END CheckReadOnly;

PROCEDURE CheckExport(VAR expo: BOOLEAN);
BEGIN
  IF sym = ORS.times THEN
    expo := TRUE;  ORS.Get(sym);
    IF level # 0 THEN ORS.Mark("remove asterisk") END
  ELSE expo := FALSE
  END
END CheckExport;

PROCEDURE IsExtension(t0, t1: ORB.Type): BOOLEAN;
BEGIN (*t1 is an extension of t0*)
  RETURN (t0 = t1) OR (t1 # NIL) & IsExtension(t0, t1.base)
END IsExtension;

(* expressions *)

PROCEDURE TypeTest(VAR x: X64.Item;  T: ORB.Type;  guard: BOOLEAN);
VAR xt: ORB.Type;
BEGIN xt := x.type;
  IF (T.form = xt.form )
   & (   (T.form = ORB.Pointer)
      OR (T.form = ORB.Record) & (x.mode = X64.Stkind)) THEN
    WHILE (xt # T) & (xt # NIL) DO xt := xt.base END;
    IF xt # T THEN xt := x.type;
      IF xt.form = ORB.Pointer THEN
        IF IsExtension(xt.base, T.base) THEN
          ORG.TypeTest(x, T.base, FALSE, guard);
          x.type := T
        ELSE ORS.Mark("not an extension")
        END
      ELSIF (xt.form = ORB.Record) & (x.mode = X64.Stkind) THEN
        IF IsExtension(xt, T) THEN  ORG.TypeTest(x, T, TRUE, guard);  x.type := T
        ELSE ORS.Mark("not an extension")
        END
      ELSE ORS.Mark("incompatible types")
      END
    ELSIF ~guard THEN ORG.MakeConstItem(x, ORB.boolType, 1)
    END
  ELSE ORS.Mark("type mismatch")
  END;
  IF ~guard THEN x.type := ORB.boolType END
END TypeTest;

PROCEDURE selector(VAR x: X64.Item);
VAR y: X64.Item;  obj: ORB.Object;
BEGIN
  WHILE (sym = ORS.lbrak) OR (sym = ORS.period) OR (sym = ORS.arrow)
      OR (sym = ORS.lparen) & (x.type.form IN {ORB.Record, ORB.Pointer}) DO
    IF sym = ORS.lbrak THEN
      REPEAT ORS.Get(sym);  expression(y);
        IF x.type.form = ORB.Array THEN
          CheckInt(y);  ORG.Index(x, y);  x.type := x.type.base
        ELSE ORS.Mark("not an array")
        END
      UNTIL sym # ORS.comma;
      Check(ORS.rbrak, "no ]")
    ELSIF sym = ORS.period THEN ORS.Get(sym);
      IF sym = ORS.ident THEN
        IF x.type.form = ORB.Pointer THEN ORG.DeRef(x);  x.type := x.type.base END;
        IF x.type.form = ORB.Record THEN
          obj := ORB.thisfield(x.type);  ORS.Get(sym);
          IF obj # NIL THEN ORG.Field(x, obj);  x.type := obj.type
          ELSE ORS.Mark("undef")
          END
        ELSE ORS.Mark("not a record")
        END
      ELSE ORS.Mark("ident?")
      END
    ELSIF sym = ORS.arrow THEN
      ORS.Get(sym);
      IF x.type.form = ORB.Pointer THEN ORG.DeRef(x);  x.type := x.type.base
      ELSE ORS.Mark("not a pointer")
      END
    ELSIF (sym = ORS.lparen) & (x.type.form IN {ORB.Record, ORB.Pointer}) THEN (*type guard*)
      ORS.Get(sym);
      IF sym = ORS.ident THEN
        qualident(obj);
        IF obj.class = ORB.Typ THEN TypeTest(x, obj.type, TRUE)
        ELSE ORS.Mark("guard type expected")
        END
      ELSE ORS.Mark("not an identifier")
      END;
      Check(ORS.rparen, " ) missing")
    END
  END
END selector;

PROCEDURE EqualSignatures(t0, t1: ORB.Type): BOOLEAN;
VAR p0, p1: ORB.Object;  com: BOOLEAN;
BEGIN com := TRUE;
  IF (t0.base = t1.base) & (t0.nofpar = t1.nofpar) THEN
    p0 := t0.dsc;  p1 := t1.dsc;
    WHILE p0 # NIL DO
      IF (p0.class = p1.class) &  (p0.rdo = p1.rdo) &
        ((p0.type = p1.type) OR
        (p0.type.form = ORB.Array) & (p1.type.form = ORB.Array) & (p0.type.len = p1.type.len) & (p0.type.base = p1.type.base) OR
        (p0.type.form = ORB.Proc) & (p1.type.form = ORB.Proc) & EqualSignatures(p0.type, p1.type))
      THEN p0 := p0.next;  p1 := p1.next
      ELSE p0 := NIL;  com := FALSE
      END
    END
  ELSE com := FALSE
  END;
  RETURN com
END EqualSignatures;

PROCEDURE CompTypes(t0, t1: ORB.Type;  varpar: BOOLEAN): BOOLEAN;
BEGIN (*check for assignment compatibility*)
  RETURN (t0 = t1)    (*openarray assignment disallowed in ORG*)
    OR (t0.form = ORB.Array) & (t1.form = ORB.Array) & (t0.base =  t1.base) & (t0.len = t1.len)
    OR (t0.form = ORB.Record) & (t1.form = ORB.Record)  & IsExtension(t0, t1)
    OR ~varpar &
      ((t0.form = ORB.Pointer) & (t1.form = ORB.Pointer)  & IsExtension(t0.base, t1.base)
      OR (t0.form = ORB.Proc) & (t1.form = ORB.Proc) & EqualSignatures(t0, t1)
      OR (t0.form IN {ORB.Pointer, ORB.Proc}) & (t1.form = ORB.NilTyp))
      OR (t0.form = ORB.Int64) & (t1.form = ORB.Int64)  (* All int types are value compatible *)
END CompTypes;

PROCEDURE Parameter(par: ORB.Object; native: BOOLEAN);
VAR x: X64.Item;  varpar: BOOLEAN;  stkoff: INTEGER;
BEGIN
  l.StartRightCol; l.s("Parameter: ", Tr); l.Object(par, Tr); l.sl(".", Tr);
  expression(x);
  IF par # NIL THEN
    varpar := par.class = ORB.Par;
    IF native THEN stkoff := par.val ELSE stkoff := 0 END;
    IF CompTypes(par.type, x.type, varpar) THEN
      IF ~varpar THEN ORG.ValueParam(x, stkoff)
      ELSE (*par.class = Par*)
        IF ~par.rdo THEN CheckReadOnly(x) END;
        ORG.VarParam(x, par.type, stkoff)
      END
    ELSIF (x.type.form = ORB.Array)     & (par.type.form = ORB.Array)
        & (x.type.base = par.type.base) & (par.type.len < 0) THEN
      IF ~par.rdo THEN CheckReadOnly(x) END;
      ORG.OpenArrayParam(x, stkoff)
    ELSIF (x.type.form = ORB.String) & varpar & par.rdo
        & (par.type.form = ORB.Array) & (par.type.base.form = ORB.Char)
        & (par.type.len < 0) THEN
      ORG.StringParam(x, stkoff)
  (*ELSIF ~varpar & (par.type.form = ORB.Int64) & (x.type.form = ORB.Int64) THEN
      ORG.ValueParam(x)  (*BYTE*) *)
    ELSIF (x.type.form = ORB.String) & (x.n = 2)
        & (par.class = ORB.Var) & (par.type.form = ORB.Char) THEN
      ASSERT(x.mode = X64.String);
      ORG.StrToChar(x);  ORG.ValueParam(x, stkoff)
    ELSIF (par.type.form = ORB.Array) & (par.type.base = ORB.byteType)
     (* & (par.type.len >= 0) & (par.type.size = x.type.size) *) THEN
      ORG.VarParam(x, par.type, stkoff)
    ELSE ORS.Mark("incompatible parameters")
    END
  END
END Parameter;

PROCEDURE ParamList(VAR x: X64.Item);
VAR n: INTEGER;  par: ORB.Object;
BEGIN par := x.type.dsc;  n := 0;
  IF sym # ORS.rparen THEN
    Parameter(par, x.type.native);  n := 1;
    WHILE sym <= ORS.comma DO
      Check(sym, "comma?");
      IF par # NIL THEN par := par.next END;
      INC(n);  Parameter(par, x.type.native)
    END;
    Check(ORS.rparen, ") missing")
  ELSE ORS.Get(sym);
  END;
  IF n < x.type.nofpar THEN ORS.Mark("too few params")
  ELSIF n > x.type.nofpar THEN ORS.Mark("too many params")
  END
END ParamList;

PROCEDURE StandFunc(VAR x: X64.Item;  fct: INTEGER;  restyp: ORB.Type);
VAR y: X64.Item;  n, npar: INTEGER;
BEGIN Check(ORS.lparen, "no (");
  npar := fct MOD 10;  fct := fct DIV 10;  expression(x);  n := 1;
  WHILE sym = ORS.comma DO ORS.Get(sym);  expression(y);  INC(n) END;
  Check(ORS.rparen, "no )");
  IF n = npar THEN
    IF fct = 0 THEN (*ABS*)
      IF x.type.form IN {ORB.Int64, ORB.Real} THEN ORG.Abs(x);  restyp := x.type ELSE ORS.Mark("bad type") END
    ELSIF fct = 1 THEN (*ODD*) CheckInt(x);  ORG.Odd(x)
    ELSIF fct = 2 THEN (*FLOOR*) CheckReal(x);  ORG.Floor(x)
    ELSIF fct = 3 THEN (*FLT*) CheckInt(x);  ORG.Float(x)
    ELSIF fct = 4 THEN (*ORD*)
      IF x.type.form <= ORB.Proc THEN ORG.Ord(x)
      ELSIF (x.type.form = ORB.String) & (x.n = 2) THEN ASSERT(x.mode = X64.String); ORG.StrToChar(x)
      ELSE ORS.Mark("bad type")
      END
    ELSIF fct = 5 THEN (*CHR*) CheckInt(x);  ORG.Ord(x)
    ELSIF fct = 6 THEN (*LEN*)
        IF x.type.form = ORB.Array THEN ORG.Len(x) ELSE ORS.Mark("not an array") END
    ELSIF fct IN {7, 8, 9} THEN (*LSL, ASR, ROR*) CheckInt(y);
      IF x.type.form IN {ORB.Int64, ORB.Set} THEN ORG.Shift(fct-7, x, y);  restyp := x.type ELSE ORS.Mark("bad type") END
    ELSIF fct = 11 THEN (*ADC*) ORG.ADC(x, y)
    ELSIF fct = 12 THEN (*SBC*) ORG.SBC(x, y)
    ELSIF fct = 13 THEN (*UML*) ORG.UML(x, y)
    ELSIF fct = 14 THEN (*BIT*) CheckInt(x);  CheckInt(y);  ORG.Bit(x, y)
    ELSIF fct = 15 THEN (*REG*) CheckConst(x);  CheckInt(x);  ORG.REG(x)
    ELSIF fct = 16 THEN (*VAL*)
      IF (x.mode= ORB.Typ) & (x.type.size <= y.type.size) THEN restyp := x.type;  x := y
      ELSE ORS.Mark("casting not allowed")
      END
    ELSIF fct = 17 THEN (*ADR*) ORG.Adr(x)
    ELSIF fct = 18 THEN (*SIZE*)
      IF x.mode = ORB.Typ THEN ORG.MakeConstItem(x, ORB.intType, x.type.size)
      ELSE ORS.Mark("must be a type")
      END
    ELSIF fct = 19 THEN (*COND*) CheckConst(x);  CheckInt(x);  ORG.COND(x)
    END;
    x.type := restyp
  ELSE ORS.Mark("wrong nof params")
  END
END StandFunc;

PROCEDURE element(VAR x: X64.Item);
VAR y: X64.Item;
BEGIN expression(x);  CheckSetVal(x);
  IF sym = ORS.upto THEN ORS.Get(sym);  expression(y);  CheckSetVal(y);  ORG.Set(x, y)
  ELSE ORG.Singleton(x)
  END;
  x.type := ORB.setType
END element;

PROCEDURE set(VAR x: X64.Item);
VAR y: X64.Item;
BEGIN
  IF sym >= ORS.if THEN
    IF sym # ORS.rbrace THEN ORS.Mark(" } missing") END;
    ORG.MakeConstItem(x, ORB.setType, 0) (*empty set*)
  ELSE element(x);
    WHILE (sym < ORS.rparen) OR (sym > ORS.rbrace) DO
      IF sym = ORS.comma THEN ORS.Get(sym)
      ELSIF sym # ORS.rbrace THEN ORS.Mark("missing comma")
      END;
      element(y);  ORG.SetOp(ORS.plus, x, y)
    END
  END
END set;

PROCEDURE factor(VAR x: X64.Item);
VAR obj: ORB.Object;  saved: SET;
BEGIN (*sync*)
  IF (sym < ORS.char) OR (sym > ORS.ident) THEN
    ORS.Mark("expression expected");
    l.s(": sym = ", Tr); l.i(sym, Tr); l.sl(".", Tr);
    REPEAT ORS.Get(sym) UNTIL (sym >= ORS.char) & (sym <= ORS.for) OR (sym >= ORS.then)
  END;
  IF sym = ORS.ident THEN
    qualident(obj);
    IF obj.class = ORB.SFunc THEN
      StandFunc(x, obj.val, obj.type)
    ELSE ORG.MakeItem(x, obj, level);  selector(x);
      IF sym = ORS.lparen THEN
        ORS.Get(sym);
        IF (x.type.form = ORB.Proc) & (x.type.base.form # ORB.NoTyp) THEN
          ORG.PrepCall(x, saved);
          ParamList(x);
          ORG.Call(x, saved);
          x.type := x.type.base
        ELSE ORS.Mark("not a function");  ParamList(x)
        END
      END
    END
  ELSIF sym = ORS.int    THEN ORG.MakeConstItem (x, ORB.intType,  ORS.ival); ORS.Get(sym)
  ELSIF sym = ORS.char   THEN ORG.MakeConstItem (x, ORB.charType, ORS.ival); ORS.Get(sym)
  ELSIF sym = ORS.nil    THEN ORG.MakeConstItem (x, ORB.nilType,  0);        ORS.Get(sym)
  ELSIF sym = ORS.false  THEN ORG.MakeConstItem (x, ORB.boolType, 0);        ORS.Get(sym);
  ELSIF sym = ORS.true   THEN ORG.MakeConstItem (x, ORB.boolType, 1);        ORS.Get(sym);
  ELSIF sym = ORS.real   THEN ORG.MakeRealItem  (x, ORS.rval);               ORS.Get(sym)
  ELSIF sym = ORS.string THEN ORG.MakeStringItem(x, ORS.slen);               ORS.Get(sym)
  ELSIF sym = ORS.lparen THEN ORS.Get(sym);  expression(x);  Check(ORS.rparen, "no )")
  ELSIF sym = ORS.lbrace THEN ORS.Get(sym);  set(x);         Check(ORS.rbrace, "no }")
  ELSIF sym = ORS.not    THEN ORS.Get(sym);  factor(x);  CheckBool(x);  ORG.Not(x)
  ELSE ORS.Mark("not a factor");  ORG.MakeConstItem(x, ORB.intType, 0)
  END
END factor;

PROCEDURE term(VAR x: X64.Item);
VAR y: X64.Item;  op, f: INTEGER;
BEGIN factor(x);  f := x.type.form;
  WHILE (sym >= ORS.times) & (sym <= ORS.and) DO
    op := sym;  ORS.Get(sym);
    IF op = ORS.times THEN
      IF    f = ORB.Int64 THEN factor(y);  CheckInt(y);  ORG.MulOp(x, y)
      ELSIF f = ORB.Real  THEN factor(y);  CheckReal(y); ORG.RealOp(op, x, y)
      ELSIF f = ORB.Set   THEN factor(y);  CheckSet(y);  ORG.SetOp(op, x, y)
      ELSE ORS.Mark("bad type")
      END
    ELSIF (op = ORS.div) OR (op = ORS.mod) THEN
      CheckInt(x);  factor(y);  CheckInt(y);  ORG.DivOp(op, x, y)
    ELSIF op = ORS.rdiv THEN
      IF    f = ORB.Real THEN factor(y);  CheckReal(y);  ORG.RealOp(op, x, y)
      ELSIF f = ORB.Set  THEN factor(y);  CheckSet(y);  ORG.SetOp(op, x, y)
      ELSE ORS.Mark("bad type")
      END
    ELSE (*op = and*) CheckBool(x);  ORG.And1(x);  factor(y);  CheckBool(y);  ORG.And2(x, y)
    END
  END
END term;

PROCEDURE SimpleExpression(VAR x: X64.Item);
VAR y: X64.Item;  op: INTEGER;
BEGIN
  IF sym = ORS.minus THEN ORS.Get(sym);  term(x);
    IF x.type.form IN {ORB.Int64, ORB.Real, ORB.Set} THEN ORG.Neg(x) ELSE CheckInt(x) END
  ELSIF sym = ORS.plus THEN ORS.Get(sym);  term(x);
  ELSE term(x)
  END;
  WHILE (sym >= ORS.plus) & (sym <= ORS.or) DO
    op := sym;  ORS.Get(sym);
    IF op = ORS.or THEN CheckBool(x);  ORG.Or1(x);  term(y);  CheckBool(y);  ORG.Or2(x, y)
    ELSIF x.type.form = ORB.Int64 THEN term(y);  CheckInt(y);  ORG.AddOp(op, x, y)
    ELSIF x.type.form = ORB.Real THEN term(y);  CheckReal(y);  ORG.RealOp(op, x, y)
    ELSE CheckSet(x);  term(y);  CheckSet(y);  ORG.SetOp(op, x, y)
    END
  END
END SimpleExpression;

PROCEDURE expression0(VAR x: X64.Item);
VAR y: X64.Item;  obj: ORB.Object;  rel, xf, yf: INTEGER;
BEGIN SimpleExpression(x);
  IF (sym >= ORS.eql) & (sym <= ORS.geq) THEN
    rel := sym;  ORS.Get(sym);  SimpleExpression(y);  xf := x.type.form;  yf := y.type.form;
    IF x.type = y.type THEN
      IF (xf IN {ORB.Char, ORB.Int64}) THEN ORG.IntRelation(rel, x, y)
      ELSIF xf = ORB.Real THEN ORG.RealRelation(rel, x, y)
      ELSIF (xf IN {ORB.Set, ORB.Pointer, ORB.Proc, ORB.NilTyp, ORB.Bool}) THEN
        IF rel <= ORS.neq THEN ORG.IntRelation(rel, x, y) ELSE ORS.Mark("only = or #") END
      ELSIF (xf = ORB.Array) & (x.type.base.form = ORB.Char) OR (xf = ORB.String) THEN
        ORG.StringRelation(rel, x, y)
      ELSE ORS.Mark("illegal comparison")
      END
    ELSIF (xf IN {ORB.Pointer, ORB.Proc}) & (yf = ORB.NilTyp)
        OR (yf IN {ORB.Pointer, ORB.Proc}) & (xf = ORB.NilTyp) THEN
      IF rel <= ORS.neq THEN ORG.IntRelation(rel, x,  y) ELSE ORS.Mark("only = or #") END
    ELSIF (xf = ORB.Pointer) & (yf = ORB.Pointer) &
        (IsExtension(x.type.base, y.type.base) OR IsExtension(y.type.base, x.type.base))
        OR (xf = ORB.Proc) & (yf = ORB.Proc) & EqualSignatures(x.type, y.type) THEN
      IF rel <= ORS.neq THEN ORG.IntRelation(rel,  x, y) ELSE ORS.Mark("only = or #") END
    ELSIF (xf = ORB.Array) & (x.type.base.form = ORB.Char) &
          ((yf = ORB.String) OR (yf = ORB.Array) & (y.type.base.form = ORB.Char))
        OR (yf = ORB.Array) & (y.type.base.form = ORB.Char) & (xf = ORB.String) THEN
      ORG.StringRelation(rel, x, y)
    ELSIF (xf = ORB.Char) & (yf = ORB.String) & (y.n = 2) THEN ASSERT(y.mode = X64.String);
      ORG.StrToChar(y);  ORG.IntRelation(rel, x, y)
    ELSIF (yf = ORB.Char) & (xf = ORB.String) & (x.n = 2) THEN ASSERT(x.mode = X64.String);
      ORG.StrToChar(x);  ORG.IntRelation(rel, x, y)
    ELSIF (xf = ORB.Int64) & (yf = ORB.Int64) THEN ORG.IntRelation(rel,  x, y)  (*BYTE*)
    ELSE ORS.Mark("illegal comparison")
    END;
    x.type := ORB.boolType
  ELSIF sym = ORS.in THEN
    ORS.Get(sym);  CheckInt(x);  SimpleExpression(y);  CheckSet(y);  ORG.In(x, y) ;
    x.type := ORB.boolType
  ELSIF sym = ORS.is THEN
    ORS.Get(sym);  qualident(obj);  TypeTest(x, obj.type, FALSE) ;
    x.type := ORB.boolType
  END
END expression0;

(* statements *)

PROCEDURE StandProc(pno: INTEGER);
VAR nap, npar: INTEGER;  (*nof actual/formal parameters*)
    x, y, z: X64.Item;
BEGIN Check(ORS.lparen, "no (");
  npar := pno MOD 10;  pno := pno DIV 10;  expression(x);  nap := 1;
  IF sym = ORS.comma THEN
    ORS.Get(sym);  expression(y);  nap := 2;  z.type := ORB.noType;
    WHILE sym = ORS.comma DO ORS.Get(sym);  expression(z);  INC(nap) END
  ELSE y.type := ORB.noType
  END;
  Check(ORS.rparen, "no )");
  IF (npar = nap) OR (pno IN {0, 1}) THEN
    IF pno IN {0, 1} THEN (*INC, DEC*)
      CheckInt(x);  CheckReadOnly(x);
      IF y.type # ORB.noType THEN CheckInt(y) END;
      ORG.Increment(pno, x, y)
    ELSIF pno IN {2, 3} THEN (*INCL, EXCL*)
      CheckSet(x);  CheckReadOnly(x);  CheckInt(y);  ORG.Include(pno-2, x, y)
    ELSIF pno = 4 THEN CheckBool(x);  ORG.Assert(x)
    ELSIF pno = 5 THEN(*NEW*) CheckReadOnly(x);
       IF (x.type.form = ORB.Pointer) & (x.type.base.form = ORB.Record) THEN ORG.New(x)
       ELSE ORS.Mark("not a pointer to record")
       END
    ELSIF pno = 6 THEN CheckReal(x);  CheckInt(y);  CheckReadOnly(x);  ORG.Pack(x, y)
    ELSIF pno = 7 THEN CheckReal(x);  CheckInt(y);  CheckReadOnly(x);  ORG.Unpk(x, y)
    ELSIF pno = 8 THEN
      IF x.type.form <= ORB.Set THEN ORG.Led(x) ELSE ORS.Mark("bad type") END
    ELSIF pno = 10 THEN CheckInt(x);  ORG.Get(x, y)
    ELSIF pno = 11 THEN CheckInt(x);  ORG.Put(x, y)
    ELSIF pno = 12 THEN CheckInt(x);  CheckInt(y);  CheckInt(z);  ORG.Copy(x, y, z)
    ELSIF pno = 13 THEN CheckConst(x);  CheckInt(x);  ORG.LDPSR(x)
    ELSIF pno = 14 THEN CheckInt(x);  ORG.LDREG(x, y)
    END
  ELSE ORS.Mark("wrong nof parameters")
  END
END StandProc;

PROCEDURE StatSequence;
VAR obj:         ORB.Object;
    orgtype:     ORB.Type;  (*original type of case var*)
    x, y, z, by: X64.Item;
    L0, L1:      INTEGER;
    saved:       SET;

  PROCEDURE TypeCase(obj: ORB.Object;  VAR x: X64.Item);
    VAR typobj: ORB.Object;
  BEGIN
    IF sym = ORS.ident THEN
      qualident(typobj);  ORG.MakeItem(x, obj, level);
      IF typobj.class # ORB.Typ THEN ORS.Mark("not a type") END;
      TypeTest(x, typobj.type, FALSE);  obj.type := typobj.type;
      ORG.CFJump(x);  Check(ORS.colon, ": expected");  StatSequence
    ELSE ORG.CFJump(x);  ORS.Mark("type id expected")
    END
   END TypeCase;

  PROCEDURE SkipCase;
  BEGIN
    WHILE sym # ORS.colon DO ORS.Get(sym) END;
    ORS.Get(sym);  StatSequence
  END SkipCase;

BEGIN (* StatSequence *)
  IF X64.SPO # 0 THEN l.s("StatSequence, SPO ", Tr); l.i(X64.SPO, Tr); l.l(Tr) END;
  REPEAT (*sync*) obj := NIL;
    IF ~((sym >= ORS.ident)  & (sym <= ORS.for) OR (sym >= ORS.semicolon)) THEN
      ORS.Mark("statement expected");
      REPEAT ORS.Get(sym) UNTIL (sym >= ORS.ident)
    END;
    IF sym = ORS.ident THEN
      qualident(obj);  ORG.MakeItem(x, obj, level);
      IF x.mode = ORB.SProc THEN StandProc(obj.val)
      ELSE selector(x);
        IF sym = ORS.becomes THEN (*assignment*)
          ORS.Get(sym);  CheckReadOnly(x);  expression(y);
          IF CompTypes(x.type, y.type, FALSE) THEN
            IF (x.type.form <= ORB.Pointer) OR (x.type.form = ORB.Proc) THEN ORG.Store(x, y)
            ELSE ORG.StoreStruct(x, y)
            END
          ELSIF (x.type.form = ORB.Array) & (y.type.form = ORB.Array) & (x.type.base = y.type.base) & (y.type.len < 0) THEN
            ORG.StoreStruct(x, y)
          ELSIF (x.type.form = ORB.Array) & (x.type.base.form = ORB.Char) & (y.type.form = ORB.String) THEN
            ORG.CopyString(x, y)
          ELSIF (x.type.form = ORB.Int64) & (y.type.form = ORB.Int64) THEN ORG.Store(x, y)  (*BYTE*)
          ELSIF (x.type.form = ORB.Char) & (y.type.form = ORB.String) & (y.n = 2) THEN ASSERT(y.mode = X64.String);
            ORG.StrToChar(y);  ORG.Store(x, y)
          ELSE ORS.Mark("illegal assignment")
          END
        ELSIF sym = ORS.eql THEN ORS.Mark("should be :=");  ORS.Get(sym);  expression(y)
        ELSIF sym = ORS.lparen THEN (*procedure call*)
          ORS.Get(sym);
          IF (x.type.form = ORB.Proc) & (x.type.base.form = ORB.NoTyp) THEN
            ORG.PrepCall(x, saved);  ParamList(x);  ORG.Call(x, saved)
          ELSE ORS.Mark("not a procedure");  ParamList(x)
          END
        ELSIF x.type.form = ORB.Proc THEN (*procedure call without parameters*)
          IF x.type.nofpar > 0 THEN ORS.Mark("missing parameters") END;
          IF x.type.base.form = ORB.NoTyp THEN
            ORG.PrepCall(x, saved);  ORG.Call(x, saved)
          ELSE
            ORS.Mark("not a procedure")
          END
        ELSIF x.mode = ORB.Typ THEN ORS.Mark("illegal assignment")
        ELSE ORS.Mark("not a procedure")
        END
      END
    ELSIF sym = ORS.if THEN
      ORS.Get(sym);
      expression(x);  CheckBool(x);
      ORG.CFJump(x);
      Check(ORS.then, "no THEN");
      StatSequence;
      L0 := 0;
      WHILE sym = ORS.elsif DO
        ORS.Get(sym);
        ORG.FJump(L0);
        ORG.FixLink(x.offset);
        expression(x);  CheckBool(x);
        ORG.CFJump(x);
        Check(ORS.then, "no THEN");
        StatSequence
      END;
      IF sym = ORS.else THEN
        ORS.Get(sym);  ORG.FJump(L0);  ORG.FixLink(x.offset);
        StatSequence
      ELSE
        ORG.FixLink(x.offset)
      END;
      ORG.FixLink(L0);
      Check(ORS.end, "no END")
    ELSIF sym = ORS.while THEN ORS.Get(sym);
      L0 := ORG.Here();
      expression(x);  CheckBool(x);
      ORG.CFJump(x);
      Check(ORS.do, "no DO");  StatSequence;
      ORG.BJump(L0);
      WHILE sym = ORS.elsif DO
        ORS.Get(sym);  ORG.FixLink(x.offset);  expression(x);  CheckBool(x);  ORG.CFJump(x);
        Check(ORS.do, "no DO");  StatSequence;  ORG.BJump(L0)
      END;
      ORG.FixLink(x.offset);
      Check(ORS.end, "no END")
    ELSIF sym = ORS.repeat THEN
      ORS.Get(sym);  L0 := ORG.Here();  StatSequence;
      IF sym = ORS.until THEN
        ORS.Get(sym);  expression(x);  CheckBool(x);  ORG.CBJump(x, L0)
      ELSE ORS.Mark("missing UNTIL")
      END
    ELSIF sym = ORS.for THEN  (* FOR val := ini TO lim BY step *)
      ORS.Get(sym);
      IF sym # ORS.ident THEN ORS.Mark("identifier expected")
      ELSE
        qualident(obj);  ORG.MakeItem(x, obj, level);  CheckInt(x);  CheckReadOnly(x);
        IF sym # ORS.becomes THEN ORS.Mark(":= expected")
        ELSE
          ORS.Get(sym);  expression(y);  CheckInt(y);
          ORG.For0(x, y);  (* val, ini *)
          L0 := ORG.Here();
          Check(ORS.to, "TO expected");
          expression(z);  CheckInt(z);  obj.rdo := TRUE;
          IF sym # ORS.by THEN ORG.MakeConstItem(by, ORB.intType, 1)
          ELSE
            ORS.Get(sym);  expression(by);  CheckConst(by);  CheckInt(by)
          END;
          Check(ORS.do, "DO expected");
          ORG.For1(x, y, z, by, L1);  (* val, ini, lim, step *)
          StatSequence;
          Check(ORS.end, "END expected");
          ORG.For2(x, y, by);
          ORG.BJump(L0);  (* val, ini, step *)
          ORG.FixLink(L1);  obj.rdo := FALSE
        END
      END
    ELSIF sym = ORS.case THEN
      ORS.Get(sym);
      IF sym = ORS.ident THEN
        qualident(obj);  orgtype := obj.type;
        IF (orgtype.form = ORB.Pointer) OR (orgtype.form = ORB.Record) & (obj.class = ORB.Par) THEN
          Check(ORS.of, "OF expected");  TypeCase(obj, x);  L0 := 0;
          WHILE sym = ORS.bar DO
            ORS.Get(sym);  ORG.FJump(L0);  ORG.FixLink(x.offset);  obj.type := orgtype;  TypeCase(obj, x)
          END;
          ORG.FixLink(x.offset);  ORG.FixLink(L0);  obj.type := orgtype
        ELSE ORS.Mark("numeric case not implemented");
          Check(ORS.of, "OF expected");  SkipCase;
          WHILE sym = ORS.bar DO SkipCase END
        END
      ELSE ORS.Mark("ident expected")
      END;
      Check(ORS.end, "no END")
    END;
    ORG.CheckRegs("StatSequence");
    IF sym = ORS.semicolon THEN ORS.Get(sym)
    ELSIF sym < ORS.semicolon THEN ORS.Mark("missing semicolon?")
    END
  UNTIL sym > ORS.semicolon
END StatSequence;

(* Types and declarations *)

PROCEDURE IdentList(class: INTEGER;  VAR first: ORB.Object);
VAR obj: ORB.Object;
BEGIN
  IF sym = ORS.ident THEN
    ORB.NewObj(first, ORS.id, class);  ORS.Get(sym);  CheckExport(first.expo);
    WHILE sym = ORS.comma DO
      ORS.Get(sym);
      IF sym = ORS.ident THEN ORB.NewObj(obj, ORS.id, class);  ORS.Get(sym);  CheckExport(obj.expo)
      ELSE ORS.Mark("ident?")
      END
    END;
    IF sym = ORS.colon THEN ORS.Get(sym) ELSE ORS.Mark(":?") END
  ELSE first := NIL
  END
END IdentList;

PROCEDURE ArrayType(VAR type: ORB.Type);
VAR x: X64.Item;  typ: ORB.Type;  len: INTEGER;
BEGIN NEW(typ);
  typ.form := ORB.NoTyp;
  expression(x);
  IF (x.mode = X64.Const) & (x.type.form = ORB.Int64) & (x.n >= 0) THEN
    len := x.n
  ELSE
    len := 1;  ORS.Mark("not a valid length")
  END;
  IF sym = ORS.of THEN ORS.Get(sym);
    Type(typ.base);
    IF (typ.base.form = ORB.Array) & (typ.base.len < 0) THEN ORS.Mark("dyn array not allowed") END
  ELSIF sym = ORS.comma THEN ORS.Get(sym);
    ArrayType(typ.base)
  ELSE
    ORS.Mark("missing OF");  typ.base := ORB.intType
  END;
  typ.size := (len * typ.base.size + 7) DIV 8 * 8;
  typ.form := ORB.Array;
  typ.len  := len;
  type     := typ
END ArrayType;

PROCEDURE RecordType(VAR type: ORB.Type);
VAR obj, obj0, new, bot, base: ORB.Object;
    typ, tp: ORB.Type;
    offset, off, n: INTEGER;
BEGIN NEW(typ);  typ.form := ORB.NoTyp;  typ.base := NIL;  typ.mno := -level;  typ.nofpar := 0;
  offset := 0;  bot := NIL;
  IF sym = ORS.lparen THEN
    ORS.Get(sym);  (*record extension*)
    IF level # 0 THEN ORS.Mark("extension of local types not implemented") END;
    IF sym = ORS.ident THEN
      qualident(base);
      IF base.class = ORB.Typ THEN
        IF base.type.form = ORB.Record THEN typ.base := base.type
        ELSE typ.base := ORB.intType;  ORS.Mark("invalid extension")
        END;
        typ.nofpar := typ.base.nofpar + 1;  (*"nofpar" here abused for extension level*)
        bot    := typ.base.dsc;
        offset := typ.base.size
      ELSE ORS.Mark("type expected")
      END
    ELSE ORS.Mark("ident expected")
    END;
    Check(ORS.rparen, "no )")
  END;
  WHILE sym = ORS.ident DO  (*fields*)
    n := 0;  obj := bot;
    WHILE sym = ORS.ident DO
      obj0 := obj;
      WHILE (obj0 # NIL) & (obj0.name # ORS.id) DO obj0 := obj0.next END;
      IF obj0 # NIL THEN ORS.Mark("mult def") END;
      NEW(new);  ORS.CopyId(new.name);  new.class := ORB.Fld;  new.next := obj;
      obj := new;  INC(n);
      ORS.Get(sym);  CheckExport(new.expo);
      IF (sym # ORS.comma) & (sym # ORS.colon) THEN ORS.Mark("comma expected")
      ELSIF sym = ORS.comma THEN ORS.Get(sym)
      END
    END;
    Check(ORS.colon, "colon expected");  Type(tp);
    IF (tp.form = ORB.Array) & (tp.len < 0) THEN ORS.Mark("dyn array not allowed") END;

    (*IF tp.size > 1 THEN offset := (offset + 7) DIV 8 * 8 END;*)

    (* Align suitably for size of field *)
    IF    tp.size > 4 THEN offset := (offset + 7) DIV 8 * 8
    ELSIF tp.size > 2 THEN offset := (offset + 3) DIV 4 * 4
    ELSIF tp.size > 1 THEN offset := (offset + 1) DIV 2 * 2
    END;

    offset := offset + n * tp.size;  off := offset;  obj0 := obj;
    WHILE obj0 # bot DO obj0.type := tp;  obj0.lev := 0;  off := off - tp.size;  obj0.val := off;  obj0 := obj0.next END;
    bot := obj;
    IF sym = ORS.semicolon THEN ORS.Get(sym) ELSIF sym # ORS.end THEN ORS.Mark(" ;  or END") END
  END;
  typ.form := ORB.Record;  typ.dsc := bot;  typ.size := (offset + 7) DIV 8 * 8;  type := typ
END RecordType;

(* Formal parameter section - one or more comma separated vars of one type   *)
(* Each parameter takes either one or two 64 bit cells on the stack          *)
(* Parameter addresses advance downwards corresponding to x86 push behaviour *)
PROCEDURE FPSection(VAR adr: INTEGER;  VAR nofpar: INTEGER);
VAR obj, first: ORB.Object;  tp: ORB.Type;
    parsize: INTEGER;  cl: INTEGER;  rdo: BOOLEAN;
BEGIN
  IF sym = ORS.var THEN ORS.Get(sym);  cl := ORB.Par ELSE cl := ORB.Var END;
  IdentList(cl, first);  FormalType(tp, 0);  rdo := FALSE;

  IF (cl = ORB.Var) & (tp.form >= ORB.Array) THEN
    cl := ORB.Par;  rdo := TRUE
  END;
  IF (tp.form = ORB.Array) & (tp.len < 0)
  OR (tp.form = ORB.Record) THEN  (* open arrays and records need a second *)
    parsize := 2 * ORG.WordSize   (* word for length or type tag           *)
  ELSE parsize := ORG.WordSize
  END;
  obj := first;
  WHILE obj # NIL DO
    INC(nofpar);  obj.class := cl;  obj.type := tp;  obj.rdo := rdo;  obj.lev := level;
    DEC(adr, parsize);
    obj.val := adr;
    obj := obj.next
  END
END FPSection;

PROCEDURE ProcedureType(ptype: ORB.Type);
VAR obj: ORB.Object;  offset: INTEGER;  nofpar: INTEGER;
BEGIN ptype.base := ORB.noType;  offset := 0;  nofpar := 0;  ptype.dsc := NIL;
  IF sym = ORS.lparen THEN                     (* Parse formal parameters *)
    ORS.Get(sym);
    IF sym = ORS.rparen THEN ORS.Get(sym)
    ELSE
      FPSection(offset, nofpar);
      WHILE sym = ORS.semicolon DO
        ORS.Get(sym);
        FPSection(offset, nofpar)
      END;
      Check(ORS.rparen, "no )")
    END;
    IF sym = ORS.colon THEN                    (* Parse function result type *)
      ORS.Get(sym);
      IF sym = ORS.ident THEN
        qualident(obj);  ptype.base := obj.type;
        IF ~((obj.class = ORB.Typ) & (obj.type.form IN {ORB.Byte .. ORB.Pointer, ORB.Proc})) THEN
          ORS.Mark("illegal function type")
        END
      ELSE ORS.Mark("type identifier expected")
      END
    END
  END;
  ptype.nofpar := nofpar;
  ptype.len    := -offset;
END ProcedureType;

PROCEDURE FormalType0(VAR typ: ORB.Type;  dim: INTEGER);
VAR obj: ORB.Object;  dmy: INTEGER;
BEGIN
  IF sym = ORS.ident THEN
    qualident(obj);
    IF obj.class = ORB.Typ THEN typ := obj.type ELSE ORS.Mark("not a type");  typ := ORB.intType END
  ELSIF sym = ORS.array THEN
    ORS.Get(sym);  Check(ORS.of, "OF ?");
    IF dim >= 1 THEN ORS.Mark("multi-dimensional open arrays not implemented") END;
    NEW(typ);  typ.form := ORB.Array;  typ.len := -1;  typ.size := 2*ORG.WordSize;
    FormalType(typ.base, dim+1)
  ELSIF sym = ORS.procedure THEN
    ORS.Get(sym);  ORB.OpenScope;
    NEW(typ);  typ.form := ORB.Proc;  typ.size := ORG.WordSize;
    typ.native := FALSE; dmy := 0;
    ProcedureType(typ);
    typ.dsc := ORB.topScope.next;  ORB.CloseScope
  ELSE ORS.Mark("identifier expected");  typ := ORB.noType
  END
END FormalType0;

PROCEDURE CheckRecLevel(lev: INTEGER);
BEGIN
  IF lev # 0 THEN ORS.Mark("ptr base must be global") END
END CheckRecLevel;

PROCEDURE Type0(VAR type: ORB.Type);
VAR obj: ORB.Object;  ptbase: PtrBase;
BEGIN type := ORB.intType;  (*sync*)
  IF (sym # ORS.ident) & (sym < ORS.array) THEN ORS.Mark("not a type");
    REPEAT ORS.Get(sym) UNTIL (sym = ORS.ident) OR (sym >= ORS.array)
  END;
  IF sym = ORS.ident THEN
    qualident(obj);
    IF obj.class = ORB.Typ THEN
      IF (obj.type # NIL) & (obj.type.form # ORB.NoTyp) THEN type := obj.type END
    ELSE ORS.Mark("not a type or undefined")
    END
  ELSIF sym = ORS.array THEN ORS.Get(sym);  ArrayType(type)
  ELSIF sym = ORS.record THEN
    ORS.Get(sym);  RecordType(type);  Check(ORS.end, "no END")
  ELSIF sym = ORS.pointer THEN
    ORS.Get(sym);  Check(ORS.to, "no TO");
    NEW(type);  type.form := ORB.Pointer;  type.size := ORG.WordSize;  type.base := ORB.intType;
    IF sym = ORS.ident THEN
      obj := ORB.thisObj();
      IF obj # NIL THEN
        IF (obj.class = ORB.Typ) & (obj.type.form IN {ORB.Record, ORB.NoTyp}) THEN
          CheckRecLevel(obj.lev);  type.base := obj.type
        ELSIF obj.class = ORB.Mod THEN ORS.Mark("external base type not implemented")
        ELSE ORS.Mark("no valid base type")
        END
      ELSE CheckRecLevel(level);  (*enter into list of forward references to be fixed in Declarations*)
        NEW(ptbase);  ORS.CopyId(ptbase.name);  ptbase.type := type;  ptbase.next := pbsList;  pbsList := ptbase
      END;
      ORS.Get(sym)
    ELSE Type(type.base);
      IF (type.base.form # ORB.Record) OR (type.base.typobj = NIL) THEN ORS.Mark("must point to named record") END;
      CheckRecLevel(level)
    END
  ELSIF sym = ORS.procedure THEN
    ORS.Get(sym);  ORB.OpenScope;
    NEW(type);  type.form := ORB.Proc;  type.size := ORG.WordSize;
    type.native := sym = ORS.neq;  IF type.native THEN ORS.Get(sym) END;
    ProcedureType(type);  type.dsc := ORB.topScope.next;  ORB.CloseScope
  ELSE ORS.Mark("illegal type")
  END
END Type0;


PROCEDURE AlignUp(VAR adr: INTEGER; size: INTEGER);
(* Advance adr to be aligned suitably for a variable of given size *)
BEGIN
  IF    size > 4 THEN adr := (adr + 7) DIV 8 * 8
  ELSIF size > 2 THEN adr := (adr + 3) DIV 4 * 4
  ELSIF size > 1 THEN adr := (adr + 1) DIV 2 * 2
  END
END AlignUp;

PROCEDURE AlignDown(VAR adr: INTEGER; size: INTEGER);
(* Reduce adr to be aligned suitably for a variable of given size *)
BEGIN
  IF    size > 4 THEN adr := adr DIV 8 * 8
  ELSIF size > 2 THEN adr := adr DIV 4 * 4
  ELSIF size > 1 THEN adr := adr DIV 2 * 2
  END
END AlignDown;


PROCEDURE Declarations(VAR varsize: INTEGER);
VAR
  obj, first: ORB.Object;
  x:          X64.Item;
  tp:         ORB.Type;
  ptbase:     PtrBase;
  expo:       BOOLEAN;
  id:         ORS.Ident;
BEGIN (*sync*) pbsList := NIL;
  IF (sym < ORS.const) & (sym # ORS.end) & (sym # ORS.return) THEN
    ORS.Mark("declaration?");
    REPEAT ORS.Get(sym) UNTIL (sym >= ORS.const) OR (sym = ORS.end) OR (sym = ORS.return)
  END;

  IF sym = ORS.const THEN
    ORS.Get(sym);
    WHILE sym = ORS.ident DO
      ORS.CopyId(id);  ORS.Get(sym);  CheckExport(expo);
      IF sym = ORS.eql THEN ORS.Get(sym) ELSE ORS.Mark("= ?") END;
      expression(x);
      IF (x.type.form = ORB.String) & (x.n = 2) THEN ASSERT(x.mode = X64.String); ORG.StrToChar(x) END;
      ORB.NewObj(obj, id, ORB.Const);
      obj.expo := expo;
      IF (x.mode = X64.Const) OR (x.type.form = ORB.String) THEN
        obj.type := x.type;
        IF x.type.form = ORB.String THEN
          obj.val := 0;    obj.lev := x.n
        ELSE
          obj.val := x.n;  obj.lev := 0
        END
      ELSE ORS.Mark("expression not constant");  obj.type := ORB.intType
      END;
      Check(ORS.semicolon, "; missing")
    END
  END;

  IF sym = ORS.type THEN
    ORS.Get(sym);
    WHILE sym = ORS.ident DO
      ORS.CopyId(id);  ORS.Get(sym);  CheckExport(expo);
      IF sym = ORS.eql THEN ORS.Get(sym) ELSE ORS.Mark("=?") END;
      Type(tp);
      ORB.NewObj(obj, id, ORB.Typ);
      obj.type := tp;
      obj.expo := expo;
      obj.lev  := level;
      IF tp.typobj = NIL THEN tp.typobj := obj END;
      IF expo & (obj.type.form = ORB.Record) THEN
        obj.exno := exno;  INC(exno)
      ELSE
        obj.exno := 0
      END;
      IF tp.form = ORB.Record THEN
        ptbase := pbsList;  (*check whether this is base of a pointer type;  search and fixup*)
        WHILE ptbase # NIL DO
          IF obj.name = ptbase.name THEN ptbase.type.base := obj.type END;
          ptbase := ptbase.next
        END;
        IF level = 0 THEN
          ORG.BuildTD(tp, dc)  (* type descriptor;  len used as its address *)
        END
      END;
      Check(ORS.semicolon, "; missing")
    END
  END;

  IF sym = ORS.var THEN
    ORS.Get(sym);
    WHILE sym = ORS.ident DO
      IdentList(ORB.Var, first);
      Type(tp);
      obj := first;
      WHILE obj # NIL DO
        obj.type := tp;
        obj.lev  := level;
        AlignUp(varsize, tp.size);
        obj.val  := varsize;
        varsize  := varsize + tp.size;
        IF obj.expo THEN obj.exno := exno;  INC(exno) END;
        obj := obj.next
      END;
      Check(ORS.semicolon, "; missing")
    END
  END;

  varsize := (varsize + 7) DIV 8 * 8;
  ptbase  := pbsList;

  WHILE ptbase # NIL DO
    IF ptbase.type.base.form = ORB.Int64 THEN ORS.Mark("undefined pointer base of") END;
    ptbase := ptbase.next
  END;

  IF (sym >= ORS.const) & (sym <= ORS.var) THEN ORS.Mark("declaration in bad order") END
END Declarations;


PROCEDURE slen(s: ARRAY OF CHAR): INTEGER;
VAR len: INTEGER;
BEGIN len := 0;  WHILE (len < LEN(s)) & (s[len] # 0X) DO INC(len) END
RETURN len END slen;

PROCEDURE ProcedureDecl;
VAR proc, obj:   ORB.Object;
    type:        ORB.Type;
    procid:      ORS.Ident;
    x:           X64.Item;
    parmsize:    INTEGER;
    locsize:     INTEGER;
    framesize:   INTEGER;
    L:           INTEGER;
    i:           INTEGER;
    longestname: INTEGER;
BEGIN ORS.Get(sym);
  IF sym = ORS.ident THEN
    ORS.CopyId(procid);  ORS.Get(sym);
    ORB.NewObj(proc, ORS.id, ORB.Const);
    NEW(type);
    type.form   := ORB.Proc;
    type.size   := ORG.WordSize;
    type.native := FALSE;
    proc.type   := type;
    proc.val    := -1;
    proc.lev    := level;
    CheckExport(proc.expo);
    IF proc.expo THEN proc.exno := exno;  INC(exno) END;
    ORB.OpenScope;  INC(level);  type.base := ORB.noType;

    ProcedureType(type);  (*formal parameter list*)
    Check(ORS.semicolon, "no ;");

    parmsize := type.len;
    locsize  := 0; (* Local start at RSP + 0 *)
    Declarations(locsize);

    framesize := parmsize + locsize + 8;  (* Frame includes return address *)

    proc.val := ORG.Here();
    type.dsc := ORB.topScope.next;

    (* So far, parameter and local var addresses start at 0 and increment *)
    (* towards frame size (type.len). Since the X64 stack grows towards   *)
    (* lower addresses, and since we push the parameters first on the     *)
    (* stack (i.e. at higher addresses) and in source text order,we need  *)
    (* to reverse the addressing of parameters and local vars.            *)

    l.CatchupSource;
    l.s("Procedure declaration '", Tr);  l.s(procid, Tr);  l.sl("'.", Tr);
    (* --- dump objects --- *)
    l.s("parmsize: $",    Tr); l.h(parmsize, Tr);
    l.s(", locsize: $",   Tr); l.h(locsize, Tr);
    l.s(", framesize: $", Tr); l.h(framesize, Tr); l.l(Tr);
    longestname := 0;  obj := type.dsc;  WHILE obj # NIL DO
      IF slen(obj.name) > longestname THEN longestname := slen(obj.name) END;
      obj := obj.next
    END;
    obj := type.dsc;
    WHILE obj # NIL DO
      IF obj.class IN {ORB.Var, ORB.Par} THEN
        IF obj.val < 0 THEN (* Fixup parameter offsets *)
          INC(obj.val, framesize)
        END;
        l.Class(obj.class, Tr);  l.s(" ", Tr);  l.sn(obj.name, longestname, Tr);
        l.s(" adr $", Tr);       l.hn(obj.val, 6, Tr);
      (*
      ELSIF obj.class = ORB.Const THEN
        ORG.wclass(obj.class);  w.s(" "); w.sn(obj.name, longestname);
        w.s(" val $");  w.h(obj.val)
      ELSE
        ORG.wclass(obj.class);  w.s(" "); w.sn(obj.name, longestname);
      *)
      END;
      l.s(" type ", Tr);  l.Type(obj.type, Tr);
      l.l(Tr);
      obj := obj.next
    END;

    (* Parse nested procedures *)
    IF sym = ORS.procedure THEN
      L := 0;  ORG.FJump(L);
      REPEAT ProcedureDecl;  Check(ORS.semicolon, "no ;") UNTIL sym # ORS.procedure;
      ORG.FixLink(L);  proc.val := ORG.Here();  proc.type.dsc := ORB.topScope.next
    END;

    (* Compile procedure body *)
    ORG.Enter(parmsize, locsize);
    IF sym = ORS.begin THEN ORS.Get(sym);  StatSequence END;
    IF sym = ORS.return THEN
      ORS.Get(sym);  expression(x);
      IF type.base = ORB.noType THEN ORS.Mark("this is not a function")
      ELSIF ~CompTypes(type.base, x.type, FALSE) THEN ORS.Mark("wrong result type")
      END
    ELSIF type.base.form # ORB.NoTyp THEN
      ORS.Mark("function without result");  type.base := ORB.noType
    END;
    Check(ORS.end, "no END");
    IF sym = ORS.ident THEN
      IF ORS.id # procid THEN ORS.Mark("no match") END;
      ORS.Get(sym)
    ELSE ORS.Mark("no proc id")
    END;
    ORG.Return(type.base.form, x, parmsize, locsize);
    ORB.CloseScope;  DEC(level)
  END
END ProcedureDecl;

PROCEDURE Module(sourcefn: ARRAY OF CHAR);
VAR key: INTEGER;
    impid, impid1: ORS.Ident;
BEGIN Texts.WriteString(W, "  compiling ");  ORS.Get(sym);
  IF sym # ORS.module THEN ORS.Mark("must start with MODULE")
  ELSE
    ORS.Get(sym);
    ORB.Init;  ORB.OpenScope;
    IF sym = ORS.ident THEN
      ORS.CopyId(modid);  ORS.Get(sym);
      Texts.WriteString(W, modid);  Texts.Append(Oberon.Log, W.buf)
    ELSE ORS.Mark("identifier expected")
    END;
    Check(ORS.semicolon, "no ;");
    level := 0;  dc := 0;  exno := 1;  key := 0;

    IF sym = ORS.import THEN
      ORS.Get(sym);
      WHILE sym = ORS.ident DO
        ORS.CopyId(impid);  ORS.Get(sym);
        IF sym = ORS.becomes THEN
          ORS.Get(sym);
          IF sym = ORS.ident THEN ORS.CopyId(impid1);  ORS.Get(sym)
          ELSE ORS.Mark("id expected")
          END
        ELSE impid1 := impid
        END;
        ORB.Import(impid, impid1);
        IF sym = ORS.comma THEN ORS.Get(sym)
        ELSIF sym = ORS.ident THEN ORS.Mark("comma missing")
        END
      END;
      Check(ORS.semicolon, "no ;")
    END;

    ORG.Open(sourcefn, modid);
    Declarations(dc);
    ORG.SetDataSize((dc + 7) DIV 8 * 8);

    WHILE sym = ORS.procedure DO ProcedureDecl; Check(ORS.semicolon, "no ;") END;

    ORG.Header;

    IF sym = ORS.begin THEN ORS.Get(sym); StatSequence END;

    Check(ORS.end, "no END");
    IF sym = ORS.ident THEN
      IF ORS.id # modid THEN ORS.Mark("no match") END;
      ORS.Get(sym)
    ELSE ORS.Mark("identifier missing")
    END;
    IF sym # ORS.period THEN ORS.Mark("period missing") END;

    IF ORS.errcnt = 0 THEN
      ORB.Export(modid, newSF, key);
      IF newSF THEN Texts.WriteString(W, " new symbol file") END
    END;

    IF ORS.errcnt = 0 THEN
      ORG.Close(modid, key, exno);
      Texts.WriteInt(W, X64.PC, 6);  Texts.WriteInt(W, dc, 6);  Texts.WriteHex(W, key)
    ELSE Texts.WriteLn(W);  Texts.WriteString(W, "compilation FAILED")
    END;

    Texts.WriteLn(W);  Texts.Append(Oberon.Log, W.buf);
    ORB.CloseScope;  pbsList := NIL
  END
END Module;

PROCEDURE Option(VAR S: Texts.Scanner);
BEGIN newSF := FALSE;
  IF S.nextCh = "/" THEN
    Texts.Scan(S);  Texts.Scan(S);
    IF (S.class = Texts.Name) & (S.s[0] = "s") THEN newSF := TRUE END
  END
END Option;

(*
PROCEDURE Compile*;
VAR beg, end, time: INTEGER;
    T: Texts.Text;
    S: Texts.Scanner;
BEGIN Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos);  Texts.Scan(S);
  IF S.class = Texts.Char THEN
    IF S.c = "@" THEN
      Option(S);  Oberon.GetSelection(T, beg, end, time);
      IF time >= 0 THEN ORS.Init(T, beg);  Module END
    ELSIF S.c = "^" THEN
      Option(S);  Oberon.GetSelection(T, beg, end, time);
      IF time >= 0 THEN
        Texts.OpenScanner(S, T, beg);  Texts.Scan(S);
        IF S.class = Texts.Name THEN
          Texts.WriteString(W, S.s);  NEW(T);  Texts.Open(T, S.s);
          IF T.len > 0 THEN ORS.Init(T, 0);  Module END
        END
      END
    END
  ELSE
    WHILE S.class = Texts.Name DO
      NEW(T);  Texts.Open(T, S.s);
      IF T.len > 0 THEN Option(S);  ORS.Init(T, 0);  Module
      ELSE Texts.WriteString(W, S.s);  Texts.WriteString(W, " not found");
        Texts.WriteLn(W);  Texts.Append(Oberon.Log, W.buf)
      END;
      IF (T.len # 0) & (ORS.errcnt = 0) THEN Texts.Scan(S) ELSE S.class := 0 END
    END
  END;
  Oberon.Collect(0)
END Compile;
*)

PROCEDURE CompileFile*(fn: ARRAY OF CHAR);
VAR T: Texts.Text;
BEGIN
  NEW(T);  Texts.Open(T, fn);
  IF T.len > 0 THEN ORS.Init(T, 0);  Module(fn) END;
END CompileFile;

BEGIN
  Texts.OpenWriter(W);  Texts.WriteString(W, "OR Compiler  17.9.2018");
  Texts.WriteLn(W);  Texts.Append(Oberon.Log, W.buf);
  l.InitBuf(Tr);
  l.s("hn test: hn(0, 4, buf) ", Tr); l.hn(0, 4, Tr); l.sl(".", Tr);
  NEW(dummy);  dummy.class := ORB.Var;  dummy.type := ORB.intType;
  expression := expression0;  Type := Type0;  FormalType := FormalType0
END ORP.
