MODULE ORG;  (* DCWB June 2023; Code generator for X64 *)

(* Originally: N.Wirth, 16.4.2016 / 4.4.2017 / 17.9.2018  Oberon compiler; code generator for RISC*)

IMPORT SYSTEM, Files, ORS, ORB, w := Writer, K := Kernel, X64;

CONST
  WordSize* = 8;
  MaxStrx   = 8000;  (* String buffer size *)
  MaxPC     = 10000H;
  RightCol  = 70;

  (* Derived states *)
  Operand   = 10;     (* X64 register or immediate or direct or indirect effective address *)
  Condition = 12;     (* Condition codes set by last ALU operation *)

  (* X64 conditions *)
  CF  = 0;     (* False, Never                         *)
  CT  = 1;     (* True, always                         *)
  CO  = 80H;   (* Overflow                             *)
  CNO = 81H;   (* No overflow                          *)
  CC  = 82H;   (* Carry, Below                         *)
  CNC = 83H;   (* No carry, Above or equal             *)
  CZ  = 84H;   (* Zero, Equal                          *)
  CNZ = 85H;   (* Nonzero, Not equal                   *)
  CNA = 86H;   (* Not above, Below or equal            *)
  CA  = 87H;   (* Above, Not below or equal            *)
  CS  = 88H;   (* Sign, Negative                       *)
  CNS = 89H;   (* No sign, Positive or zero            *)
  CP  = 8AH;   (* Parity                               *)
  CNP = 8BH;   (* No parity                            *)
  CL  = 8CH;   (* Less than, Not greater or equal      *)
  CNL = 8DH;   (* Not less than, Greater or equal      *)
  CNG = 8EH;   (* Not greater than, Less than or equal *)
  CG  = 8FH;   (* Greater than, Not less than or equal *)


TYPE
  Item* = RECORD
    state*:    INTEGER;  (* Initially from object.class *)
    type*:     ORB.Type;
    o*:        X64.Operand;
    readonly*: BOOLEAN;
    length*:   INTEGER;  (* Only if type = strType *)
  END;


VAR
  Str:  ARRAY MaxStrx OF CHAR;
  Strx: INTEGER;

  sourcefile: Files.File;
  source:     Files.Rider;
  sourceBol:  INTEGER;      (* pos at beginiing of line *)
  sourceLine: INTEGER;      (* Line number *)
  sourcechar: CHAR;

  Saved: SET;               (* Registers saved around a function call *)


(* ---- Source display ---- *)

PROCEDURE Pos(): INTEGER;
BEGIN RETURN Files.Pos(source) - 1 END Pos;

PROCEDURE GetChar;
VAR prevch: CHAR;
BEGIN  ASSERT(~source.eof);
  prevch := sourcechar;
  Files.Read(source, sourcechar);
  IF source.eof THEN sourcechar := 0X
  ELSE
    IF (prevch = 0DX) OR (prevch = 0AX) THEN
      IF (prevch = 0DX) & (sourcechar = 0AX) THEN Files.Read(source, sourcechar) END;
      sourceBol := Pos();
      INC(sourceLine)
    END
  END
END GetChar;

PROCEDURE wl; BEGIN w.l      END wl;
PROCEDURE wc; BEGIN w.c(",") END wc;

PROCEDURE DisplaySourceToPos(pos: INTEGER);
VAR ch: CHAR;
BEGIN
  w.in(sourceLine, 4); w.s(": ");
  w.b(Pos() - sourceBol);  (* Space to current column *)

  (* Copy source text to pos *)
  WHILE ~source.eof & (Pos() < pos) DO
    IF (sourcechar = 0DX) OR (sourcechar = 0AX) THEN
      GetChar; w.l; w.in(sourceLine, 4); w.s(": ")
    ELSE
      w.c(sourcechar); GetChar
    END
  END;

  (* Space to RightCol allowing for 6 columns already used by linenumber *)
  IF Pos() - sourceBol < (RightCol - 6) THEN
    w.b((RightCol - 6) - (Pos() - sourceBol));
  ELSE
    w.s("  ")
  END
END DisplaySourceToPos;

PROCEDURE startRightCol*(str: ARRAY OF CHAR);
BEGIN
  DisplaySourceToPos(ORS.Pos());
  w.s(str)
END startRightCol;

PROCEDURE wbool(b: BOOLEAN);
BEGIN IF b THEN w.s("TRUE") ELSE w.s("FALSE") END END wbool;

PROCEDURE wform(form: INTEGER);
BEGIN
  IF    form = ORB.Byte    THEN w.s("Byte")
  ELSIF form = ORB.Bool    THEN w.s("Bool")
  ELSIF form = ORB.Char    THEN w.s("Char")
  ELSIF form = ORB.Int8    THEN w.s("Int8")
  ELSIF form = ORB.Int16   THEN w.s("Int16")
  ELSIF form = ORB.Int32   THEN w.s("Int32")
  ELSIF form = ORB.Int64   THEN w.s("Int64")
  ELSIF form = ORB.Card16  THEN w.s("Card16")
  ELSIF form = ORB.Card32  THEN w.s("Card32")
  ELSIF form = ORB.Real    THEN w.s("Real")
  ELSIF form = ORB.Set     THEN w.s("Set")
  ELSIF form = ORB.Pointer THEN w.s("Pointer")
  ELSIF form = ORB.NilTyp  THEN w.s("NilTyp")
  ELSIF form = ORB.NoTyp   THEN w.s("NoTyp")
  ELSIF form = ORB.Proc    THEN w.s("Proc")
  ELSIF form = ORB.String  THEN w.s("String")
  ELSIF form = ORB.Array   THEN w.s("Array")
  ELSIF form = ORB.Record  THEN w.s("Record")
                           ELSE w.i(form)
  END
END wform;

PROCEDURE wclass(class: INTEGER);
BEGIN
  IF    class = ORB.Head  THEN w.s("Head")
  ELSIF class = ORB.Const THEN w.s("Const")
  ELSIF class = ORB.Var   THEN w.s("Var")
  ELSIF class = ORB.Par   THEN w.s("Par")
  ELSIF class = ORB.Fld   THEN w.s("Fld")
  ELSIF class = ORB.Typ   THEN w.s("Typ")
  ELSIF class = ORB.SProc THEN w.s("SProc")
  ELSIF class = ORB.SFunc THEN w.s("SFunc")
  ELSIF class = ORB.Mod   THEN w.s("Mod")
                          ELSE w.i(class)
  END
END wclass;

PROCEDURE wsym(sym: INTEGER);
BEGIN
  IF    sym = ORS.times     THEN w.s("*")
  ELSIF sym = ORS.rdiv      THEN w.s("/")
  ELSIF sym = ORS.div       THEN w.s("DIV")
  ELSIF sym = ORS.mod       THEN w.s("MOD")
  ELSIF sym = ORS.and       THEN w.s("&")
  ELSIF sym = ORS.plus      THEN w.s("+")
  ELSIF sym = ORS.minus     THEN w.s("-")
  ELSIF sym = ORS.or        THEN w.s("OR")
  ELSIF sym = ORS.eql       THEN w.s("=")
  ELSIF sym = ORS.neq       THEN w.s("#")
  ELSIF sym = ORS.lss       THEN w.s("<")
  ELSIF sym = ORS.leq       THEN w.s("<=")
  ELSIF sym = ORS.gtr       THEN w.s(">")
  ELSIF sym = ORS.geq       THEN w.s(">=")
  ELSIF sym = ORS.in        THEN w.s("IN")
  ELSIF sym = ORS.is        THEN w.s("IS")
  ELSIF sym = ORS.arrow     THEN w.s("^")
  ELSIF sym = ORS.period    THEN w.s(".")
  ELSIF sym = ORS.char      THEN w.s("CHAR")
  ELSIF sym = ORS.int       THEN w.s("int")
  ELSIF sym = ORS.real      THEN w.s("real")
  ELSIF sym = ORS.false     THEN w.s("FALSE")
  ELSIF sym = ORS.true      THEN w.s("TRUE")
  ELSIF sym = ORS.nil       THEN w.s("NIL")
  ELSIF sym = ORS.string    THEN w.c(22X)
  ELSIF sym = ORS.not       THEN w.s("~")
  ELSIF sym = ORS.lparen    THEN w.s("(")
  ELSIF sym = ORS.lbrak     THEN w.s("[")
  ELSIF sym = ORS.lbrace    THEN w.s("{")
  ELSIF sym = ORS.ident     THEN w.s("ident")
  ELSIF sym = ORS.if        THEN w.s("IF")
  ELSIF sym = ORS.while     THEN w.s("WHILE")
  ELSIF sym = ORS.repeat    THEN w.s("REPEAT")
  ELSIF sym = ORS.case      THEN w.s("CASE")
  ELSIF sym = ORS.for       THEN w.s("FOR")
  ELSIF sym = ORS.comma     THEN w.s(",")
  ELSIF sym = ORS.colon     THEN w.s(":")
  ELSIF sym = ORS.becomes   THEN w.s(":=")
  ELSIF sym = ORS.upto      THEN w.s("..")
  ELSIF sym = ORS.rparen    THEN w.s(")")
  ELSIF sym = ORS.rbrak     THEN w.s("]")
  ELSIF sym = ORS.rbrace    THEN w.s("}")
  ELSIF sym = ORS.then      THEN w.s("THN")
  ELSIF sym = ORS.of        THEN w.s("OF")
  ELSIF sym = ORS.do        THEN w.s("DO")
  ELSIF sym = ORS.to        THEN w.s("TO")
  ELSIF sym = ORS.by        THEN w.s("BY")
  ELSIF sym = ORS.semicolon THEN w.s(";")
  ELSIF sym = ORS.end       THEN w.s("END")
  ELSIF sym = ORS.bar       THEN w.s("|")
  ELSIF sym = ORS.else      THEN w.s("ELSE")
  ELSIF sym = ORS.elsif     THEN w.s("ELSIF")
  ELSIF sym = ORS.until     THEN w.s("UNTIL")
  ELSIF sym = ORS.return    THEN w.s("RETURN")
  ELSIF sym = ORS.array     THEN w.s("ARRAY")
  ELSIF sym = ORS.record    THEN w.s("RECORD")
  ELSIF sym = ORS.pointer   THEN w.s("POINTER")
  ELSIF sym = ORS.const     THEN w.s("CONST")
  ELSIF sym = ORS.type      THEN w.s("TYPE")
  ELSIF sym = ORS.var       THEN w.s("VAR")
  ELSIF sym = ORS.procedure THEN w.s("PROCEDURE")
  ELSIF sym = ORS.begin     THEN w.s("BEGIN")
  ELSIF sym = ORS.import    THEN w.s("IMPORT")
  ELSIF sym = ORS.module    THEN w.s("MODULE")
                            ELSE w.i(sym)
  END
END wsym;

PROCEDURE wcond(cond: INTEGER);
BEGIN
  IF    cond = 0   THEN w.s("false")
  ELSIF cond = 1   THEN w.s("true")
  ELSIF cond = 80H THEN w.s("overflow");
  ELSIF cond = 81H THEN w.s("no overflow");
  ELSIF cond = 82H THEN w.s("carry");
  ELSIF cond = 83H THEN w.s("no carry");
  ELSIF cond = 84H THEN w.s("equal");
  ELSIF cond = 85H THEN w.s("not equal");
  ELSIF cond = 86H THEN w.s("not above");
  ELSIF cond = 87H THEN w.s("above");
  ELSIF cond = 88H THEN w.s("negative");
  ELSIF cond = 89H THEN w.s("positive");
  ELSIF cond = 8AH THEN w.s("parity 1");
  ELSIF cond = 8BH THEN w.s("parity 0");
  ELSIF cond = 8CH THEN w.s("less than");
  ELSIF cond = 8DH THEN w.s("greater or equal");
  ELSIF cond = 8EH THEN w.s("lesser or equal");
  ELSIF cond = 8FH THEN w.s("greater than");
  ELSE  ASSERT(FALSE)
  END
END wcond;

PROCEDURE wstate(state: INTEGER);
BEGIN
  IF    state = ORB.Head  THEN w.s("Head")
  ELSIF state = ORB.SProc THEN w.s("SProc")
  ELSIF state = ORB.Typ   THEN w.s("Typ")
  ELSIF state = Operand   THEN w.s("Operand")
  ELSIF state = Condition THEN w.s("Condition")
  ELSE                         w.i(state)
  END
END wstate;


PROCEDURE wtype(t: ORB.Type);
BEGIN
  IF t = NIL THEN w.s("(NIL)") ELSE
    w.s("(");
    IF t.ref # 0 THEN wform(t.ref) ELSE wform(t.form) END;
    w.s(", mno ");   w.i(t.mno);
    IF t.len # 0 THEN w.s(", len ");  w.i(t.len) END;
    w.s(", size ");  w.i(t.size);
    (*
    IF t.form IN {ORB.Array, ORB.Record, ORB.Pointer} THEN
      w.s(", base "); wtype(t.base)
    END;
    *)
    w.c(")")
  END
END wtype;

PROCEDURE wobject*(o: ORB.Object);
BEGIN
  wclass(o.class);
  w.s(": ");  wtype(o.type);
  w.s(" '");  w.s(o.name);
  w.s("' v"); w.i(o.val);
  w.s(" l");  w.i(o.lev);
  IF o.rdo THEN w.s(" ro") END
END wobject;

PROCEDURE wreg(r: INTEGER);
BEGIN
  IF    r =  0 THEN w.s("rax")
  ELSIF r =  1 THEN w.s("rcx")
  ELSIF r =  2 THEN w.s("rdx")
  ELSIF r =  3 THEN w.s("rbx")
  ELSIF r =  4 THEN w.s("rsp")
  ELSIF r =  5 THEN w.s("rbp")
  ELSIF r =  6 THEN w.s("rsi")
  ELSIF r =  7 THEN w.s("rdi")
  ELSIF r =  8 THEN w.s("r8")
  ELSIF r =  9 THEN w.s("r9")
  ELSIF r = 10 THEN w.s("r10")
  ELSIF r = 11 THEN w.s("r11")
  ELSIF r = 12 THEN w.s("r12")
  ELSIF r = 13 THEN w.s("r13")
  ELSIF r = 14 THEN w.s("r14")
  ELSIF r = 15 THEN w.s("r15")
  END
END wreg;

PROCEDURE wOperand(o: X64.Operand);
BEGIN
  IF o.direct THEN
    ASSERT(~o.parptr);
    IF o.base >= 0 THEN wreg(o.base) ELSE w.i(o.disp) END
  ELSE
    IF o.size # 8 THEN
      IF o.signed THEN w.c("s") ELSE w.c("u") END;
      IF    o.size = 1 THEN w.s("byte")
      ELSIF o.size = 2 THEN w.s("word")
      ELSIF o.size = 4 THEN w.s("dword")
      ELSE                  w.i(o.size)
      END
    END;
    w.c("[");
    IF (o.base < 0) & ~o.parptr THEN
      w.i(o.disp)
    ELSE
      IF o.parptr THEN
        w.s("[rsp+"); w.i(o.base); w.s("]")
      ELSE
        wreg(o.base)
      END;
      IF o.index >= 0 THEN w.c("+");  wreg(o.index);
        IF o.scale > 1 THEN w.c("*");  w.i(o.scale) END
      END;
      IF o.disp # 0 THEN w.s("+");  w.i(o.disp) END
    END;
    w.c("]");
    IF (o.base = X64.RSP) & (X64.SPO # 0) THEN w.s(" SPO "); w.i(X64.SPO) END
  END
END wOperand;

PROCEDURE witem(i: Item);
BEGIN
  w.s("(");  wstate(i.state);
  ASSERT(i.state IN {Operand, Condition, ORB.Head, ORB.Typ, ORB.SProc});
  IF i.state = Operand   THEN w.c(" "); wOperand(i.o) END;
  IF i.state = Condition THEN w.c(" "); wcond(i.o.disp) END;
  IF i.readonly THEN w.s(", readonly") END;
  IF i.type # NIL THEN
    w.s(", ");  wtype(i.type);
    IF i.type.form = ORB.String THEN w.s(", length "); w.i(i.length) END
  END;
  w.c(")")
END witem;

PROCEDURE wop(op: INTEGER);
BEGIN
  IF    op = X64.Plus  THEN w.s("Plus")
  ELSIF op = X64.Or    THEN w.s("Or")
  ELSIF op = X64.And   THEN w.s("And")
  ELSIF op = X64.Minus THEN w.s("Minus")
  ELSIF op = X64.Xor   THEN w.s("Xor")
  ELSIF op = X64.Cmp   THEN w.s("Cmp")
  ELSE w.i(op)
  END
END wop;

PROCEDURE wset(s: SET);
VAR i, f, l: INTEGER; ch: CHAR;
BEGIN
  IF s = {} THEN w.s("{}")
  ELSE
    ch := "{";  i := 0;  f := -1;  l := -1;
    FOR i := 0 TO 63 DO
      IF i IN s THEN
        IF f < 0 THEN
          f := i; l := i
        ELSIF i = l + 1 THEN
          l := i
        ELSE (* i > l + 1 *)
          w.c(ch);  ch := ",";
          IF f = l THEN w.i(f) ELSE w.i(f); w.c("-"); w.i(l) END;
          f := i;  l := i
        END
      END
    END;
    w.c(ch);
    IF f = l THEN w.i(f) ELSE w.i(f); w.c("-"); w.i(l) END;
    w.c("}")
  END
END wset;


(* ---------------------------- Code generation ----------------------------- *)


PROCEDURE ReserveNextFree*(): INTEGER;
VAR n: INTEGER;
BEGIN n := X64.FirstFreeReg();
  IF n < 0 THEN ORS.Mark("register stack underflow");  n := 15
  ELSE X64.ReserveReg(n) END
RETURN n END ReserveNextFree;

PROCEDURE CheckRegs*(msg: ARRAY OF CHAR);
BEGIN startRightCol("CheckRegs("); w.s(msg);
  w.s("): Used "); wset(X64.AllFree - X64.Free);
  w.s(", Free: "); wset(X64.Free);
  w.s(", SPO "); w.i(X64.SPO); w.sl(".");
  IF X64.Free # X64.AllFree   THEN ORS.Mark("Reg Stack"); X64.ClearRegs;  ASSERT(FALSE) END;
  IF X64.PC >= X64.MaxPC - 40 THEN ORS.Mark("program too long");          ASSERT(FALSE) END;
  IF X64.SPO # 0              THEN ORS.Mark("SPO error"); X64.ClearStack; ASSERT(FALSE) END
END CheckRegs;

PROCEDURE IsSigned(x: ORB.Type): BOOLEAN;  (* returns whether x represents a signed integer value *)
BEGIN RETURN (x.ref >= ORB.Int8) & (x.ref <= ORB.Int64) END IsSigned;

PROCEDURE IsStructured(x: ORB.Type): BOOLEAN;
BEGIN RETURN (x.form = ORB.Array) OR (x.form = ORB.Record) OR (x.form = ORB.Pointer) END IsStructured;

PROCEDURE IsReference(x: Item): BOOLEAN;
BEGIN RETURN (x.state = ORB.Par) OR IsStructured(x.type) END IsReference;

(*
PROCEDURE MakeCodeFixup(pc: INTEGER; x: Item): INTEGER;
VAR result: INTEGER;
BEGIN
  ASSERT(x.o.fixup >= 0);
  result := x.o.disp;
  IF x.o.fixup = 0 THEN
    ASSERT(result < 80000000H)                   (* 1/0, 31/module offset *)
  ELSE
    ASSERT(result < 800000H);
    INC(result, 80000000H + LSL(x.o.fixup-1, 24))  (* 1/1, 5/module no, 24/import index *)
  END
RETURN result END MakeCodeFixup;
*)


PROCEDURE MakeDataRef(t: ORB.Type; val: INTEGER): INTEGER;
BEGIN ASSERT(t.mno >= 0);
  IF t.mno = 0 THEN (* global, val is offset, set bit 31 *)
    ASSERT(val < 80000000H);
    val := val + 80000000H
  ELSE              (* module, val is import no, set module num in bits 30-26 *)
    ASSERT(val < 4000000H);  ASSERT(t.mno < 32);
    val := val + LSL(t.mno, 26)
  END
RETURN val END MakeDataRef;


PROCEDURE TypeTagAdr(t: ORB.Type): INTEGER;
BEGIN RETURN MakeDataRef(t, t.len)
END TypeTagAdr;


PROCEDURE IsImmediate*(i: Item): BOOLEAN;
BEGIN RETURN (i.state = Operand) & (i.o.direct) & (i.o.base < 0) END IsImmediate;


PROCEDURE Min(a, b: INTEGER): INTEGER;
BEGIN IF a < b THEN b := a END;  RETURN b END Min;

PROCEDURE Max(a, b: INTEGER): INTEGER;
BEGIN IF a > b THEN b := a END;  RETURN b END Max;


PROCEDURE SetCondition(VAR x: Item; cond: INTEGER);
BEGIN
  X64.ClearOperand(x.o);
  x.state    := Condition;
  x.o.base  := 0;       (* prev instr needing patching to after this instruction *)
  x.o.index := 0;       (* prev instruction needing patch to this instruction *)
  x.o.disp := cond
END SetCondition;


PROCEDURE LoadFlags(VAR x: Item);  (* Set Z condition flag based on x *)
BEGIN
  ASSERT(x.type.form = ORB.Bool);
  IF IsImmediate(x) THEN
    ASSERT((x.o.disp = 0) OR (x.o.disp = 1));
    SetCondition(x, x.o.disp)
  ELSE
    X64.Depar(x.o);
    IF x.o.direct THEN  (* test reg,reg *)
      X64.EmitRegRegOp(85H, x.o.base, 8, x.o.base)
    ELSE                 (* cmp mem,0 *)
      X64.EmitOp(83H, 7, x.type.size, x.o);
      X64.Emit(0);
    END;
    X64.FreeOperand(x.o);
    SetCondition(x, CNZ)
  END
END LoadFlags;


PROCEDURE InvertedCondition(cond: INTEGER): INTEGER;
BEGIN
  RETURN SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET, cond) / {0})
END InvertedCondition;


PROCEDURE IsDyadicNoOp(op: INTEGER; src: Item): BOOLEAN;
VAR result: BOOLEAN;
BEGIN result := FALSE;
  IF IsImmediate(src) THEN
    IF src.o.disp = 0 THEN
      result := (op = X64.Plus) OR (op = X64.Minus) OR (op = X64.Or) OR (op = X64.Xor)
    ELSIF src.o.disp = -1 THEN
      result := op = X64.And
    END
  END
RETURN result END IsDyadicNoOp;

PROCEDURE Dyadic(op: INTEGER;  VAR dest, src: Item);  (* dest := dest op src *)
VAR reg: INTEGER;
BEGIN
  ASSERT(op IN {X64.Plus, X64.Minus, X64.Cmp, X64.And, X64.Or, X64.Xor});
  startRightCol("Dyadic("); wop(op); w.s("; dest: "); witem(dest); w.s(", src: "); witem(src); wl;

  IF IsImmediate(src) & IsImmediate(dest) THEN

    IF    op = X64.Plus  THEN INC(dest.o.disp, src.o.disp)
    ELSIF op = X64.Minus THEN DEC(dest.o.disp, src.o.disp)
    ELSIF op = X64.Cmp   THEN DEC(dest.o.disp, src.o.disp)
    ELSIF op = X64.And   THEN dest.o.disp := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET,dest.o.disp) * SYSTEM.VAL(SET,src.o.disp));
    ELSIF op = X64.Or    THEN dest.o.disp := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET,dest.o.disp) + SYSTEM.VAL(SET,src.o.disp));
    ELSIF op = X64.Xor   THEN dest.o.disp := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET,dest.o.disp) / SYSTEM.VAL(SET,src.o.disp));
    END

  ELSIF ~IsDyadicNoOp(op, src) THEN
    (* TODO - avoid reg load when src is immediate such as: cmp [rsp+10],0 *)

    ASSERT(dest.state = Operand);

    X64.Load(dest.o);
    (*w.s("  dest "); witem(dest); wl;*)

    ASSERT(dest.o.direct);
    ASSERT(src.state    = Operand);

    X64.AluOp(op, dest.o.base, src.o);
    X64.FreeOperand(src.o);
  END
END Dyadic;


(* ---- Item construction ---- *)

PROCEDURE ZeroFill(VAR obj: ARRAY OF BYTE);  VAR i: INTEGER;
BEGIN FOR i := 0 TO LEN(obj)-1 DO obj[i] := 0 END END ZeroFill;

PROCEDURE MakeItem*(VAR x: Item; y: ORB.Object; curlev: INTEGER);
VAR base, section: INTEGER;
BEGIN startRightCol("MakeItem(y: "); wobject(y); w.s(", curlev: "); w.i(curlev); w.s(")");
  ASSERT(y.class IN {ORB.Const, ORB.Var, ORB.Par, ORB.SProc, ORB.Typ});

  ZeroFill(x);
  X64.ClearOperand(x.o);
  x.state    := y.class;
  x.type     := y.type;
  x.readonly := y.rdo;

  IF y.class IN {ORB.Const, ORB.Var, ORB.Par} THEN
    x.state := Operand;

    IF y.lev > 0 THEN  (* Stack relative *)
      section := -1;      base := X64.RSP
    ELSE
      section := -y.lev;  base := -1
    END;

    IF (y.class = ORB.Const) & (y.type.form = ORB.Proc) THEN
      X64.MakeMemoryOperand(base, y.val, section, 8, FALSE, FALSE, x.o)
    ELSIF y.class = ORB.Const THEN
      X64.MakeConstOperand(y.val, x.o)
    ELSIF y.class = ORB.Var THEN
      X64.MakeMemoryOperand(base, y.val, section, y.type.size, IsSigned(y.type), FALSE, x.o)
    ELSE ASSERT(y.class = ORB.Par);
      ASSERT(section < 0);  (* Must be on stack *)
      X64.MakeMemoryOperand(y.val, 0, section, y.type.size, IsSigned(y.type), TRUE, x.o)
    END
  ELSE
    X64.MakeConstOperand(y.val, x.o)
  END;

  w.s(" -> ");
  wstate(x.state); w.s("/"); wform(x.type.form);
  IF x.readonly THEN w.s(", readonly") END;
  IF x.state = Operand THEN w.s(" "); wOperand(x.o) END;
  IF x.type.form = ORB.String THEN w.s(", length "); w.i(x.length) END;
  wl
END MakeItem;

PROCEDURE MakeConstItem*(VAR x: Item;  t: ORB.Type;  v: INTEGER);
BEGIN
  startRightCol("MakeConstItem, value: "); w.i(v); w.s(", type "); wtype(t); wl;
  ZeroFill(x);
  x.state := Operand;
  x.type  := t;
  X64.MakeConstOperand(v, x.o)
END MakeConstItem;

PROCEDURE MakeRealItem*(VAR x: Item; val: REAL);
BEGIN
  startRightCol("MakeRealItem(x; val)"); wl;
  ZeroFill(x);
  x.type  := ORB.realType;
  x.state := Operand;
  X64.MakeConstOperand(SYSTEM.VAL(INTEGER, val), x.o)
END MakeRealItem;

PROCEDURE FindString(str: ARRAY OF CHAR; len: INTEGER): INTEGER;
VAR i, j: INTEGER;
(* Returns offset of string in ORG.Str *)
BEGIN
  (* First see if ORS.str can found amongst previous strings *)
  i := -1;
  REPEAT
    INC(i);
    WHILE (i < Strx) & (Str[i] # ORS.str[0]) DO INC(i) END;
    j := 1;
    WHILE (i+j < Strx) & (j < len) & (Str[i+j] = ORS.str[j]) DO INC(j) END
  UNTIL (i+len >= Strx) OR (j >= len);

  (* Add new string if not found *)
  IF (i+j > Strx) OR (j < len) THEN
    IF Strx + len > MaxStrx THEN
      ORS.Mark("too much string")
    ELSE
      i := Strx;  j := 0;
      WHILE len > 0 DO Str[Strx] := ORS.str[j]; INC(Strx); INC(j); DEC(len) END;
    END
  END
RETURN i END FindString;

PROCEDURE MakeStringItem*(VAR x: Item; len: INTEGER); (* copies string from ORS-buffer *)
VAR i: INTEGER;
BEGIN
  ZeroFill(x);
  x.type     := ORB.strType;
  x.readonly := TRUE;
  x.length   := len;
  x.state    := Operand;

  IF len <= 8 THEN (* Strings of up to 9 bytes are recorded in item.disp *)
    SYSTEM.GET(SYSTEM.ADR(ORS.str), i);
    X64.MakeConstOperand(i, x.o)
  ELSE             (* Strings of 9 or more bytes are stored in the module's string table *)
    X64.MakeMemoryOperand(-1, FindString(ORS.str, len), 0, 8, FALSE, FALSE, x.o)
  END;

  startRightCol("MakeStringItem('");
  i := 0;
  WHILE (i < len) & (ORS.str[i] # 0X) DO w.c(ORS.str[i]); INC(i) END;
  w.s("')");
  IF (len > 8) & (x.o.disp + len < Strx) THEN w.s(" (reused)") END;
  wl
END MakeStringItem;



(* ---- Item conversion ---- *)

PROCEDURE StrToChar*(VAR x: Item);
BEGIN startRightCol("StrToChar(x)  x"); witem(x); wl;
  ASSERT(x.state     = Operand);
  ASSERT(x.o.base    < 0);
  ASSERT(x.type.form = ORB.String);
  ASSERT(x.length    = 2);
  ASSERT(~x.o.parptr);
  ASSERT(x.o.direct);
  ASSERT(x.readonly);

  x.type   := ORB.charType;
  x.o.disp := x.o.disp MOD 256
END StrToChar;


(* ---- Assignment ---- *)


PROCEDURE Store*(VAR dest, src: Item); (* dest := src *)
BEGIN startRightCol("Store(dest: "); witem(dest); w.s(", src: "); witem(src); wl;
  IF IsImmediate(src) THEN
    X64.StoreImmediate(src.o.disp, dest.o);
  ELSE
    X64.Load(src.o);
    X64.StoreReg(src.o.base, dest.o);
  END;
  X64.Disassemble("Store");
  X64.FreeOperand(src.o);  X64.FreeOperand(dest.o)
END Store;


PROCEDURE StoreStruct*(VAR x, y: Item); (* x := y, frame = 0 *)
BEGIN startRightCol("StoreStruct(x: "); witem(x);  w.s(", y: "); witem(y); w.sl(")");
END StoreStruct;

PROCEDURE CopyString*(VAR x, y: Item);  (* x := y *)
BEGIN startRightCol("CopyString(x: "); witem(x);  w.s(", y: "); witem(y); w.sl(")");
END CopyString;


(* ---- selection - fields, indices & pointers ---- *)

PROCEDURE Index*(VAR x, y: Item);   (* x := x[y] *)
VAR elemsize: INTEGER;
BEGIN startRightCol("Index(x: "); witem(x); w.s(", y: "); witem(y); wl;
  ASSERT(x.type.form = ORB.Array);
  ASSERT(x.state = Operand);
  ASSERT(~x.o.direct);
  ASSERT(y.state = Operand);
  elemsize := x.type.base.size;
  w.b(RightCol + 2); w.s("x base type: "); wtype(x.type.base); wl;
  w.b(RightCol + 2); w.s("elemsize: "); w.i(elemsize); wl;

  IF IsImmediate(y) THEN
      INC(x.o.disp, y.o.disp * elemsize)
  ELSE
    X64.Depar(x.o);
    X64.Load(y.o);
    (* Use X64 SIB index and scale if possible *)
    IF (x.o.base >= 0) & (x.o.index < 0) & (elemsize IN {1, 2, 4, 8}) THEN
      x.o.index := y.o.base;  x.o.scale := 0;
      IF    elemsize = 2 THEN x.o.scale := 1
      ELSIF elemsize = 4 THEN x.o.scale := 2
      ELSIF elemsize = 8 THEN x.o.scale := 3
      END;
      X64.ClearOperand(y.o)
    ELSE
      X64.AluOpRegToReg(X64.Plus, x.o.base, y.o.base);
      X64.FreeOperand(y.o)
    END
  END;
  X64.Disassemble("Index")
END Index;

PROCEDURE DeRef*(VAR x: Item);
BEGIN startRightCol("DeRef(x), x: "); witem(x); wl;
  ASSERT(x.type.form = ORB.Pointer);
  ASSERT(x.state = Operand);
  ASSERT(~x.o.direct);
  X64.Load(x.o);
  X64.Disassemble("DeRef");
  x.o.direct := FALSE;
END DeRef;

PROCEDURE Field*(VAR x: Item; y: ORB.Object);   (* x := x.y *)
BEGIN startRightCol("Field(x: "); witem(x); w.s(", y: "); wobject(y); w.sl(")");
  ASSERT(x.state = Operand);
  ASSERT(~x.o.direct);
  INC(x.o.disp, y.val)
END Field;


(* ---- Paramter passing ---- *)

PROCEDURE ValueParam*(VAR x: Item; adr: INTEGER);
VAR stkoffs: INTEGER;
BEGIN startRightCol("ValueParam(adr: "); w.i(adr); w.s(", x: Item)  x: "); witem(x); w.sl(")");
  stkoffs := - 8 * (X64.SPO + 1);  (* Address called proc's stack frame *)
  X64.Load(x.o);
  ASSERT(x.state = Operand);  ASSERT(x.o.direct);  ASSERT(x.o.base >= 0);
  X64.StoreRegToMem(x.o.base, 8, X64.RSP, -1, 0, stkoffs + adr);  X64.FreeOperand(x.o);
  X64.Disassemble("ValueParam")
END ValueParam;

PROCEDURE VarParam*(VAR x: Item;  ftype: ORB.Type;  adr: INTEGER);
VAR r1, r2, stkoffs: INTEGER;
BEGIN startRightCol("VarParam(adr: "); w.i(adr); w.s(", x: "); witem(x); w.s(", ftype: "); wtype(ftype); w.sl(")");
  ASSERT(x.state = Operand);
  stkoffs := - 8 * (X64.SPO + 1);  (* Address called proc's stack frame *)
  IF x.o.parptr & ((ftype.form = ORB.Array) & (ftype.len < 0) OR (ftype.form = ORB.Record)) THEN
    (* Passing open array or record to same *)
    ASSERT(x.o.index < 0);  ASSERT(x.o.disp = 0);
    r1 := X64.FirstFreeReg();  X64.ReserveReg(r1);
    r2 := X64.FirstFreeReg();  X64.ReleaseReg(r1);
    X64.LoadMem(r1, FALSE, 8, X64.RSP, -1, 0, x.o.base);
    X64.LoadMem(r2, FALSE, 8, X64.RSP, -1, 0, x.o.base + 8);
    X64.StoreRegToMem(r1, 8, X64.RSP, -1, 0, stkoffs + adr);
    X64.StoreRegToMem(r2, 8, X64.RSP, -1, 0, stkoffs + adr + 8);
  ELSE
    X64.LoadAdr(x.o);
    X64.StoreRegToMem(x.o.base, 8, X64.RSP, -1, 0, stkoffs + adr);
    IF (ftype.form = ORB.Array) & (ftype.len < 0) THEN
      ASSERT(x.type.len >= 0);
      (* Pass length of array *)
      X64.StoreImmediateToMem(x.type.len, 8, X64.RSP, -1, 0, stkoffs + adr + 8)
    ELSIF ftype.form = ORB.Record THEN
      (* Pass address of type decriptor *)
      X64.StoreImmediateToMem(TypeTagAdr(x.type), 8, X64.RSP, -1, 0, stkoffs + adr + 8)
    END
  END;
  X64.FreeOperand(x.o);
  X64.Disassemble("VarParam")
END VarParam;

PROCEDURE OpenArrayParam*(VAR x: Item; adr: INTEGER);
(* Formal parameter is open array *)
VAR r1, r2, stkoffs: INTEGER;
BEGIN startRightCol("OpenArrayParam(adr: "); w.i(adr); w.s(", x: "); witem(x);  w.sl(")");
  ASSERT(x.state = Operand);  ASSERT(~x.o.direct);
  stkoffs := - 8 * (X64.SPO + 1);  (* Address called proc's stack frame *)
  IF x.o.parptr THEN (* address and length are at RSP+x.o.base *)
    ASSERT(FALSE);  (* Hmm, turns out these get passed to VarParam *)
    (*
    ASSERT(x.o.index < 0);  ASSERT(x.o.disp = 0);
    r1 := X64.FirstFreeReg();  X64.ReserveReg(r1);
    r2 := X64.FirstFreeReg();  X64.ReleaseReg(r1);
    X64.LoadMem(r1, FALSE, 8, X64.RSP, -1, 0, x.o.base);
    X64.LoadMem(r2, FALSE, 8, X64.RSP, -1, 0, x.o.base + 8);
    X64.StoreRegToMem(r1, 8, X64.RSP, -1, 0, stkoffs + adr);
    X64.StoreRegToMem(r2, 8, X64.RSP, -1, 0, stkoffs + adr + 8);
    *)
  ELSE
    ASSERT(~x.o.direct);
    IF (x.o.base < 0) & (x.o.index < 0) THEN
      X64.StoreImmediateToMem(x.o.disp,   8, X64.RSP, -1, 0, stkoffs + adr)
    ELSE
      X64.LoadAdr(x.o);
      X64.StoreRegToMem(x.o.base, 8, X64.RSP, -1, 0, stkoffs + adr)
    END;
    X64.StoreImmediateToMem(x.type.len, 8, X64.RSP, -1, 0, stkoffs + adr + 8)
  END;
  X64.FreeOperand(x.o);
  X64.Disassemble("OpenArrayParam")
END OpenArrayParam;

PROCEDURE StringParam*(VAR x: Item; adr: INTEGER);
VAR stkoffs: INTEGER;
BEGIN startRightCol("StringParam(adr: "); w.i(adr); w.s(", x: "); witem(x);  w.sl(")");
  stkoffs := - 8 * (X64.SPO + 1);  (* Address called proc's stack frame *)

END StringParam;


(* ---- Procedure / function calls ---- *)

PROCEDURE SaveRegisterSet(regs: SET);
VAR i: INTEGER;
BEGIN
  FOR i := 0 TO 15 DO
    IF i IN regs THEN
      X64.EmitPrefices(i, 4, -1, -1);
      X64.Emit(50H + i MOD 8);  (* Push reg i *)
      X64.AdjustStack(1);
    END
  END
END SaveRegisterSet;

PROCEDURE RestoreRegisters;
VAR i: INTEGER;
BEGIN
  IF Saved # {} THEN
    FOR i := 0 TO 15 DO
      IF (15-i) IN Saved THEN
        X64.EmitPrefices(15-i, 4, -1, -1);
        X64.Emit(58H + (15-i) MOD 8);  (* Pop reg (15-i) *)
        X64.ReserveReg(15-i);
        X64.AdjustStack(-1);
      END
    END;
    Saved := {}
  END;
  IF X64.SPO # 0 THEN w.s("** SPO "); w.i(X64.SPO); w.sl(" **") END;
  ASSERT(X64.SPO = 0)
END RestoreRegisters;

PROCEDURE PrepCall*(VAR x: Item);
VAR regs: SET;
BEGIN startRightCol("PrepCall(x: "); witem(x); wl;
  (*
  * regs := X64.AllFree - X64.Free;  (* Potential registers to save *)
  * (* Don't save registers being used for the proc address *)
  * IF ~x.o.parptr & X64.IsDataReg(x.o.base) THEN EXCL(regs, x.o.base)  END;
  * IF X64.IsDataReg(x.o.index) THEN EXCL(regs, x.o.index) END;
  * SaveRegisterSet(regs);
  * IF X64.IsDataReg(x.o.base) THEN
  *   (* Move proc address to the stack *)
  *   X64.Depar(x.o);
  *   X64.Push(x.o);
  *   X64.AdjustStack(1);
  *   X64.FreeOperand(x.o);
  *   x.o.direct := FALSE;
  *   x.o.base   := X64.RSP;
  *   x.o.disp   := -8 * X64.SPO;
  * END;
  * X64.Disassemble("PrepCall")
  *)
  IF X64.IsDataReg(x.o.base) THEN  (* Push proc address to stack *)
    X64.Push(x.o);
    X64.AdjustStack(1);
    x.o.direct := FALSE;
    x.o.base   := X64.RSP;
    x.o.disp   := -8 * X64.SPO;
    X64.Disassemble("proc address to be called")
  END;
  IF x.type.len > 0 THEN
    X64.AluOpImmediateToReg(X64.Minus, X64.RSP, x.type.len);
    X64.AdjustStack(x.type.len DIV 8);
    X64.Disassemble("reserve stack frame")
  END
END PrepCall;

PROCEDURE Call*(VAR x: Item);
VAR reg: INTEGER;
BEGIN startRightCol("Call(x: "); witem(x); wl;
  ASSERT(x.state = Operand);
  X64.Call(x.o);
  X64.Disassemble("Call");

  X64.AdjustStack(-(x.type.len DIV 8));  (* matches stack adjustment by <ret n> *)

  IF (x.o.direct = FALSE) & (x.o.base = X64.RSP) & (x.o.disp < 0) THEN
    (* Drop called proc address from stack *)
    X64.AluOpImmediateToReg(X64.Plus, X64.RSP, 8);
    X64.AdjustStack(-1);
    X64.Disassemble("Drop called proc address")
  END;

  IF x.type.base.form = ORB.NoTyp THEN (* procedure *)
    X64.ClearRegs
  ELSE                                 (* function *)
    X64.ClearRegs;  X64.ReserveReg(0);
    x.state := Operand;
    X64.FreeOperand(x.o);
    x.o.direct := FALSE;
    x.o.base   := 0 (* function result in rax *)
  END
END Call;

PROCEDURE Enter*(locstart, locsize: INTEGER);
VAR reg: INTEGER;
BEGIN
  startRightCol("Enter(locstart "); w.i(locstart); w.s(", locsize "); w.i(locsize); w.sl(")");
  X64.ClearStack;  X64.ClearRegs;

  (* TODO: clear from rsp+locstart to rsp+locstart+locsize *)

END Enter;

PROCEDURE Return*(form: INTEGER; VAR x: Item; framesize: INTEGER);
BEGIN
  startRightCol("Return("); wform(form);
  w.s(", x: "); witem(x);
  w.s(", framesize "); w.i(framesize); wl;

  IF form # ORB.NoTyp THEN X64.Load(x.o); X64.Disassemble("Load function result") END;

  X64.Emit(0C2H);  X64.EmitBytes(2, framesize);
  X64.Disassemble("Return")
END Return;


(* ---- Standard functions ---- *)

PROCEDURE Abs*(VAR x: Item);
BEGIN startRightCol("Abs(x)  x "); witem(x); wl
END Abs;

PROCEDURE Odd*(VAR x: Item);
BEGIN startRightCol("Odd(x)  x "); witem(x); wl
END Odd;

PROCEDURE Floor*(VAR x: Item);
BEGIN startRightCol("Floor(x)  x "); witem(x); wl
END Floor;

PROCEDURE Float*(VAR x: Item);
BEGIN startRightCol("Float(x)  x "); witem(x); wl
END Float;

PROCEDURE Ord*(VAR x: Item);
BEGIN startRightCol("Ord(x)  x "); witem(x); wl
END Ord;

PROCEDURE Len*(VAR x: Item);
BEGIN startRightCol("Len(x)  x "); witem(x); wl
END Len;

PROCEDURE Shift*(fct: INTEGER; VAR x, y: Item);
BEGIN startRightCol("Shift(");
  IF    fct = 0 THEN w.s("LSL")
  ELSIF fct = 1 THEN w.s("ASR")
  ELSIF fct = 2 THEN w.s("ROR")
  ELSE               w.i(fct)
  END;
  w.s("; x: "); witem(x); w.s(", y: "); witem(y); wl
END Shift;

PROCEDURE ADC*(VAR x, y: Item);
BEGIN startRightCol("ADC(x, y)"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl
  END ADC;

PROCEDURE SBC*(VAR x, y: Item);
BEGIN startRightCol("SBC(x, y)"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl
  END SBC;

PROCEDURE UML*(VAR x, y: Item);
BEGIN startRightCol("UML(x, y)"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl
  END UML;

PROCEDURE Bit*(VAR x, y: Item);
BEGIN startRightCol("Bit(x, y)"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl
  END Bit;

PROCEDURE REG*(VAR x: Item);
BEGIN startRightCol("REG(x)  x "); witem(x); wl
END REG;

PROCEDURE Adr*(VAR x: Item);
BEGIN startRightCol("Adr(x)  x "); witem(x); wl
END Adr;

PROCEDURE COND*(VAR x: Item);
BEGIN startRightCol("COND(x)  x "); witem(x); wl
END COND;


(* ---- Standard procedures --- *)

PROCEDURE Increment*(upordown: INTEGER; VAR x, y: Item);  (* x := x +/- y *)
VAR op: INTEGER;
BEGIN startRightCol("Increment(");
  IF upordown = 0 THEN
    op := X64.Plus;   w.s("up")
  ELSE
    op := X64.Minus;  w.s("down")
  END;
  w.s(", x: "); witem(x); w.s(", y: "); witem(y); wl;

  ASSERT(x.state = Operand);  ASSERT((y.state = Operand) OR (y.state = 0));

  X64.Depar(x.o);  ASSERT(~x.o.direct);
  ASSERT(~IsImmediate(x));

  IF y.state = 0 THEN
    X64.AluOpImmediateToMem(op, 1, x.o.size, x.o.base, x.o.index, x.o.scale, x.o.disp)
  ELSIF IsImmediate(y) THEN
    X64.AluOpImmediateToMem(op, y.o.disp, x.o.size, x.o.base, x.o.index, x.o.scale, x.o.disp)
  ELSE
    X64.Load(y.o);
    X64.AluOpRegToMem(op, y.o.base, x.o.size, x.o.base, x.o.index, x.o.scale, x.o.disp)
  END;

  X64.Disassemble("INC/DEC");
  X64.FreeOperand(x.o);  X64.FreeOperand(y.o)
END Increment;

PROCEDURE Include*(inorex: INTEGER; VAR x, y: Item);
BEGIN startRightCol("Include"); wl
END Include;

PROCEDURE Assert*(VAR x: Item);
BEGIN startRightCol("Assert(x)  x: ");  witem(x);  wl
END Assert;

PROCEDURE New*(VAR x: Item);
BEGIN startRightCol("New(x)  x: ");  witem(x);  wl
END New;

PROCEDURE Led*(VAR x: Item);
BEGIN startRightCol("Led(x)  x: ");  witem(x);  wl
END Led;

PROCEDURE LDPSR*(VAR x: Item);
BEGIN startRightCol("LDPSR(x)  x: ");  witem(x);  wl
END LDPSR;

PROCEDURE Pack*(VAR x, y: Item);
BEGIN startRightCol("Pack(x, y)"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl
END Pack;

PROCEDURE Unpk*(VAR x, y: Item);
BEGIN startRightCol("Unpk(x, y)"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl
END Unpk;

PROCEDURE Get*(VAR x, y: Item);  (* y := [x] *)
BEGIN startRightCol("Get(x: "); witem(x); w.s(", y: "); witem(y); wl;
  X64.Load(x.o);  x.type := y.type;  x.o.direct := FALSE;  Store(y, x);
  X64.FreeOperand(x.o);  X64.FreeOperand(y.o)
END Get;

PROCEDURE Put*(VAR x, y: Item);
BEGIN startRightCol("Put(x: "); witem(x); w.s(", y: "); witem(y); wl;
  X64.Load(x.o);  x.type := y.type;  x.o.direct := FALSE;  Store(x, y);
  X64.FreeOperand(x.o);  X64.FreeOperand(y.o)
END Put;

PROCEDURE LDREG*(VAR x, y: Item);
BEGIN startRightCol("LDREG(x, y)"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl
END LDREG;

PROCEDURE Copy*(VAR x, y, z: Item);
BEGIN startRightCol("Copy(x, y, z)"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl;
  w.b(RightCol + 2); w.s("z: "); witem(z); wl
END Copy;


(* ---- Sets ---- *)

PROCEDURE Set*(VAR x, y: Item);   (* x := {x .. y} *)
BEGIN startRightCol("Set(x, y)  x := {x..y}"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl;
END Set;

PROCEDURE Singleton*(VAR x: Item);  (* x := {x} *)
VAR reg: INTEGER;
BEGIN startRightCol("Singleton(x: "); witem(x); w.sl(")");
  IF IsImmediate(x) THEN
    x.o.disp := LSL(1, x.o.disp)
  ELSE
    X64.Load(x.o);
    reg := ReserveNextFree();
    X64.LoadImmediate(reg, 0);
    (* bts reg, x *)
    X64.EmitPrefices(reg, 8, x.o.base, -1);
    X64.Emit(0FH); X64.Emit(0ABH);
    X64.Emit(0C0H  +  reg MOD 8 * 8  +  x.o.base MOD 8);
    X64.Disassemble("Singleton");
    X64.FreeOperand(x.o);
    x.o.base := reg
  END
END Singleton;


PROCEDURE In*(VAR x, y: Item);  (* x := x IN y *)
BEGIN startRightCol("In(x: "); witem(x);  w.s(", y: "); witem(y); wl;
  X64.Depar(x.o);  X64.Depar(y.o);
  IF IsImmediate(y) THEN X64.Load(y.o) END;
  IF IsImmediate(x) THEN
    (* bt x,imm *)
    X64.EmitOp(0FBAH, 4, 8, y.o);
    X64.Emit(x.o.disp)
  ELSE
    X64.Load(x.o);
    (* bt x,y *)
    X64.EmitOp(0FAFH, x.o.base, 8, y.o)
  END;
  X64.Disassemble("In");
  X64.FreeOperand(x.o);
  SetCondition(x, CC)
END In;

PROCEDURE SetOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR xs, ys: SET;
BEGIN startRightCol("SetOp("); wsym(op); w.s("; x: "); witem(x); w.s(", y: "); witem(y); wl;
  IF IsImmediate(x) & IsImmediate(y) THEN
    xs := SYSTEM.VAL(SET, x.o.disp);  ys := SYSTEM.VAL(SET, y.o.disp);
    IF    op = ORS.plus  THEN xs := xs + ys
    ELSIF op = ORS.minus THEN xs := xs - ys
    ELSIF op = ORS.times THEN xs := xs * ys
    ELSIF op = ORS.rdiv  THEN xs := xs / ys
    END;
    x.o.disp := SYSTEM.VAL(INTEGER, xs)
  ELSE
    (* Convert op to x86 opcode *)
    IF op = ORS.minus THEN  (* Invert y *)
      IF IsImmediate(y) THEN
        y.o.disp := -y.o.disp - 1  (* = invert *)
      ELSE
        X64.Load(y.o);
        X64.EmitOp(0F7H, 2, 8, y.o);
        X64.Disassemble("Invert for SET -")
      END
    END;
    IF    op = ORS.times THEN op := X64.And
    ELSIF op = ORS.minus THEN op := X64.And
    ELSIF op = ORS.or    THEN op := X64.Or
    ELSIF op = ORS.rdiv  THEN op := X64.Xor
    ELSE ASSERT(FALSE)
    END;
    Dyadic(op, x, y)
  END;
  X64.Disassemble("SetOp")
END SetOp;


(* ---- Boolean operators ---- *)

PROCEDURE FixLink*(L: INTEGER);
VAR l: INTEGER;
BEGIN startRightCol("FixLink(L $"); w.h(L); w.sl(")");
  WHILE L # 0 DO
    w.b(RightCol); w.s("-> Patch at $"); w.h(L);
    w.s(" with offset $"); w.h(X64.PC-L-4);
    w.s(", target: $"); w.h(L + X64.PC-L-4); w.sl(".");
    l := X64.PeekUnsigned(L, 4);
    X64.Patch(L, 4, X64.PC-L-2);
    L := l
  END
END FixLink;


PROCEDURE Not*(VAR x: Item);   (* x := ~x *)
BEGIN startRightCol("Not(x: "); witem(x); w.sl(")");
  IF x.state # Condition THEN LoadFlags(x) END;
  x.o.disp := InvertedCondition(x.o.disp)
END Not;

PROCEDURE And1*(VAR x: Item);   (* x := x & *)
BEGIN startRightCol("And1(x: "); witem(x); w.sl(")");
  IF x.state # Condition THEN LoadFlags(x) END;
  X64.Emit(0FH); X64.Emit(InvertedCondition(x.o.disp));
  X64.EmitBytes(4, x.o.base);    x.o.base  := X64.PC - 4;
  X64.Disassemble("And1");
  FixLink(x.o.index); x.o.index := 0
END And1;

PROCEDURE Or1*(VAR x: Item);   (* x := x OR *)
BEGIN startRightCol("Or1(x: "); witem(x); w.sl(")");
  IF x.state # Condition THEN LoadFlags(x) END;
  X64.Emit(0FH); X64.Emit(x.o.disp);
  X64.EmitBytes(4, x.o.index);  x.o.index := X64.PC - 4;
  X64.Disassemble("Or1");
  FixLink(x.o.base); x.o.base  := 0
END Or1;


PROCEDURE merged(L0, L1: INTEGER): INTEGER;  (* Insert L0 at the start of L1 *)
VAR l2, l3: INTEGER;
BEGIN
  IF L0 # 0 THEN
    l2 := L0;
    REPEAT  l3 := l2;  l2 := X64.PeekUnsigned(l2, 4) UNTIL l2 = 0;
    X64.Patch(l3, 4, L1);
    L1 := L0
  END;
  RETURN L1
END merged;


PROCEDURE And2*(VAR x, y: Item);   (* x := x & y *)
BEGIN startRightCol("And2(x: "); witem(x); w.sl(")");
  IF y.state # Condition THEN LoadFlags(y) END;
  x.o.base  := merged(y.o.base, x.o.base);
  x.o.index := y.o.index;
  x.o.disp := y.o.disp
END And2;

PROCEDURE Or2*(VAR x, y: Item);   (* x := x OR y *)
BEGIN startRightCol("Or2(x: "); witem(x); w.sl(")");
  IF y.state # Condition THEN LoadFlags(y) END;
  x.o.base  := y.o.base;
  x.o.index := merged(y.o.index, x.o.index);
  x.o.disp := y.o.disp
END Or2;

PROCEDURE OpToIntCondition(op: INTEGER): INTEGER;
VAR result: INTEGER;
BEGIN
  IF    op = ORS.eql THEN result := CZ
  ELSIF op = ORS.neq THEN result := CNZ
  ELSIF op = ORS.lss THEN result := CL
  ELSIF op = ORS.leq THEN result := CNG
  ELSIF op = ORS.gtr THEN result := CG
  ELSIF op = ORS.geq THEN result := CNL
  ELSE ASSERT(FALSE)
  END
RETURN result END OpToIntCondition;

PROCEDURE Compare(VAR x, y: Item);
VAR immsize: INTEGER;
BEGIN
  ASSERT(x.state = Operand);  ASSERT(y.state = Operand);
  X64.Depar(x.o);  X64.Depar(y.o);
  IF IsImmediate(y) & (y.o.disp >= -80000000H) & (y.o.disp < 80000000H) THEN
    IF (y.o.disp >= -80H) & (y.o.disp < 80H) THEN immsize := 1 ELSE immsize := 4 END;
    IF    x.type.size = 1 THEN X64.EmitOp(80H, 7, x.type.size, x.o)
    ELSIF immsize     = 1 THEN X64.EmitOp(83H, 7, x.type.size, x.o)
    ELSE                       X64.EmitOp(81H, 7, x.type.size, x.o)
    END;
    X64.EmitBytes(immsize, y.o.disp);
  ELSE
    Dyadic(X64.Cmp, x, y);
  END;
  X64.Disassemble("Compare")
END Compare;

PROCEDURE IntRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
VAR immsize: INTEGER;
BEGIN startRightCol("IntRelation("); wsym(op); w.s(", x: "); witem(x);  w.s(", y: "); witem(y); wl;
  ASSERT(x.state = Operand);
  IF y.state # Operand THEN w.s("** y.state # Operand, is "); w.i(y.state); w.sl(" **") END;
  ASSERT(y.state = Operand);
  Compare(x, y);
  X64.FreeOperand(x.o);  X64.FreeOperand(y.o);  (* Result is in flags *)
  SetCondition(x, OpToIntCondition(op))
END IntRelation;

PROCEDURE RealRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
BEGIN startRightCol("RealRelation("); wsym(op); w.sl(", x, y)");
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl
END RealRelation;

PROCEDURE StringRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
BEGIN startRightCol("StringRelation("); wsym(op); w.sl(", x, y)");
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl
END StringRelation;



(* ---- Arithmetic operators ---- *)

PROCEDURE AddOp*(op: INTEGER; VAR x, y: Item);   (* x := x +- y *)
BEGIN startRightCol("AddOp("); wsym(op); w.s("; x: "); witem(x); w.s(", y: "); witem(y); wl;
  (* Convert op to x86 opcode *)
  IF    op = ORS.plus  THEN op := X64.Plus
  ELSIF op = ORS.minus THEN op := X64.Minus
  ELSIF op = ORS.or    THEN op := X64.Or
  ELSE ASSERT(FALSE)
  END;
  Dyadic(op, x, y);
  X64.Disassemble("AddOp")
END AddOp;

PROCEDURE MulOp*(VAR x, y: Item);   (* x := x * y *)
BEGIN startRightCol("MulOp(x, y)  x := x * y"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl
END MulOp;

PROCEDURE DivOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
BEGIN startRightCol("DivOp("); wsym(op); w.sl("; x, y) x := x /DIV y");
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl
END DivOp;

PROCEDURE RealOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
BEGIN startRightCol("RealOp("); wsym(op); w.sl("; x, y) x := x op y");
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl
END RealOp;

PROCEDURE Neg*(VAR x: Item);   (* x := -x *)
VAR size, op: INTEGER;
BEGIN startRightCol("Neg(x: "); witem(x); wl;
  IF IsImmediate(x) THEN
    x.o.disp := -x.o.disp
  ELSE
    X64.Load(x.o);
    X64.EmitRegRegOp(0F7H, 3, 8, x.o.base);
    X64.Disassemble("Neg")
  END
END Neg;


(* ---- Control flow ---- *)

(* Use of Item.o fields for conditions:
*
*  o.disp - the condition code (uses symbol encodings ORS.eql .. ORS.geq)
*  o.base  - address of prev instr if any needing patching with curr target
*  o.index - address of any forward jump list targeting this instruction
*)

PROCEDURE CFJump*(VAR x: Item);
VAR cond: INTEGER;
BEGIN startRightCol("CFJump(x: "); witem(x);  w.sl(")");
  IF x.state # Condition THEN LoadFlags(x) END;
  cond := InvertedCondition(x.o.disp);
  ASSERT(cond DIV 16 = 8);
  X64.Emit(0FH);  X64.Emit(cond);
  FixLink(x.o.index);  (* Fix any links that target this instruction *)
  X64.EmitBytes(4, x.o.base);  x.o.base := X64.PC - 4;
  X64.Disassemble("CFJump")
END CFJump;

PROCEDURE FJump*(VAR L: INTEGER);
BEGIN startRightCol("FJump(L "); w.i(L); w.sl(")");
  X64.Emit(0E9H);  X64.EmitBytes(4, L);  L := X64.PC-4;
  X64.Disassemble("FJump")
END FJump;

PROCEDURE BJump*(L: INTEGER);
BEGIN startRightCol("BJump(L "); w.i(L); w.sl(")");
  X64.Emit(0E9H);  X64.EmitBytes(4, L - X64.PC);
  X64.Disassemble("BJump")
END BJump;

PROCEDURE CBJump*(VAR x: Item; L: INTEGER);
BEGIN startRightCol("CBJump(x: "); witem(x); w.sl(", L: "); w.i(L); w.sl(")");
END CBJump;

PROCEDURE FixOne*(at: INTEGER);
BEGIN startRightCol("FixOne(at "); w.i(at); w.sl(")")
END FixOne;

PROCEDURE Here*(): INTEGER;  BEGIN RETURN X64.PC END Here;

(*
FOR var := ctl TO lim BY inc DO ... END
For0:      w is immediate
           LoadReg(ctl)
           X64.Depar(var.o)

For1:  L0: Compare(ctl, lim);  X64.FreeOperand(lim.o)
           ja/jb  L1
           StoreReg(var, ctl)  [Leaves any x & y reg usage unchanged]

           StatSeq

For2:      Dyadic(add, ctl, inc)

           jmp    L0
       L1:
*)

PROCEDURE For0*(VAR var, ctl: Item);
BEGIN startRightCol("For0(var: "); witem(var); w.s(", ctl: "); witem(ctl); w.sl(")");
  X64.Load(ctl.o);
  X64.Disassemble("For0");
END For0;

PROCEDURE For1*(VAR var, ctl, lim, inc: Item; VAR L: INTEGER);
BEGIN startRightCol("For1(var: "); witem(var); w.s(", ctl: "); witem(ctl); w.sl(",");
  w.b(RightCol);w.s("     lim: "); witem(lim); w.s(", inc: "); witem(inc); w.sl(")");
  ASSERT(IsImmediate(inc));
  IF inc.o.disp = 0 THEN ORS.Mark("zero increment"); inc.o.disp := 1 END;
  Dyadic(X64.Cmp, ctl, lim);
  X64.Emit(0FH);
  IF inc.o.disp > 0 THEN X64.Emit(87H) ELSE X64.Emit(83H) END; (* ja/jb *)
  L := X64.PC;
  X64.EmitBytes(4, 0);
  X64.Load(ctl.o);
  X64.StoreReg(ctl.o.base, var.o);
  X64.FreeOperand(var.o);  X64.FreeOperand(ctl.o);
  X64.Disassemble("For1");
END For1;

PROCEDURE For2*(VAR var, ctl, inc: Item);
BEGIN startRightCol("For2(var: "); witem(var); w.s(", ctl: "); witem(ctl); w.sl(")");
  Dyadic(X64.Plus, ctl, inc);
  X64.FreeOperand(ctl.o);
  X64.Disassemble("For2");
END For2;


(* ---- ---- *)

PROCEDURE BuildTD*(T: ORB.Type; VAR dc: INTEGER);
BEGIN startRightCol("BuildTD(T, dc "); w.i(dc); w.sl(")");
  w.b(RightCol + 2); w.s("T: "); wtype(T); wl
END BuildTD;

PROCEDURE TypeTest*(VAR x: Item; T: ORB.Type; varpar, isguard: BOOLEAN);
BEGIN startRightCol("TypeTest(x, T, ");
  IF varpar  THEN w.s("varpar, ")  ELSE w.s("~varpar, ")  END;
  IF isguard THEN w.sl("isguard)") ELSE w.sl("~isguard)") END;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("T: "); wtype(T); wl
END TypeTest;


(* ---- ---- *)

PROCEDURE Open*(v: INTEGER; sourcefn: ARRAY OF CHAR);
BEGIN
  w.s("ORG Open(v "); w.i(v); w.s("; '"); w.s(sourcefn); w.sl("')");

  sourcefile := Files.Old(sourcefn);  Files.Set(source, sourcefile, 0);
  sourceBol := 0;  sourceLine := 1;
  GetChar;
  Strx := 0;  X64.ClearStack;  X64.ClearRegs;
  X64.Init;
END Open;

PROCEDURE SetDataSize*(dc: INTEGER);
BEGIN w.s("SetDataSize(dc "); w.i(dc); w.sl(")");
END SetDataSize;

PROCEDURE Header*;
BEGIN w.sl("Header")
END Header;

PROCEDURE Close*(VAR modid: ORS.Ident; key, nofent: INTEGER);
BEGIN w.s("Close("); w.s(modid);
  w.s("; key ");     w.i(key);
  w.s(", nofent ");  w.i(nofent); w.sl(")")
END Close;


(* ---- Initialisation ---- *)

BEGIN
END ORG.
