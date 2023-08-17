MODULE ORG;  (* DCWB June 2023; Code generator for X64 *)

(* Originally: N.Wirth, 16.4.2016 / 4.4.2017 / 17.9.2018  Oberon compiler; code generator for RISC*)

IMPORT SYSTEM, Files, ORS, ORB, w := Writer, K := Kernel, X64;

CONST
  WordSize* = 8;
  MaxStrx   = 8000;  (* String buffer size *)
  MaxPC     = 10000H;
  RightCol  = 80;

  (* Derived states *)
  RegRm     = 10;     (* X64 register or effective address *)
  RegRmInd  = 11;     (* X64 register or effective address containing address *)
  Condition = 12;     (* Condition codes set by last ALU operation *)

  (* Dyadic ALU operations, values correspond to x86 instruction set *)
  Plus  = 00H;
  Or    = 08H;
  And   = 20H;
  Minus = 28H;
  Xor   = 30H;
  Cmp   = 38H;

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
    ea*:       X64.EffectiveAddress;
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
  outcol:     INTEGER;      (* Output column *)

  RH:    INTEGER;           (* Register high = top of register stack = first free register *)
  SPO:   INTEGER;           (* Stack pointer offset - nonzero after temporary pushes *)


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

PROCEDURE wl; BEGIN w.l;  outcol := 0 END wl;
PROCEDURE wc; BEGIN w.c(",") END wc;

PROCEDURE DisplaySourceToPos(pos: INTEGER);
VAR ch: CHAR;
BEGIN
  WHILE sourcechar <= " " DO GetChar END;  (* Skip to next non-whitespace character *)

  (* Copy source text *)
  WHILE ~source.eof & (Pos() < pos) DO
    w.in(sourceLine, 4); w.s(": "); w.b(Pos() - sourceBol);
    outcol := 6 + Pos() - sourceBol;

    (* Copy characters to pos or EOL *)
    WHILE ~source.eof & (Pos() < pos) & (sourcechar # 0DX) & (sourcechar # 0AX) DO
      w.c(sourcechar);  INC(outcol);  GetChar
    END;

    (* Skip whitespace *)
    WHILE ~source.eof & (sourcechar <= " ") DO GetChar END;

    IF ~source.eof & (Pos() < pos) THEN wl END
  END
END DisplaySourceToPos;

PROCEDURE startRightCol(str: ARRAY OF CHAR);
BEGIN DisplaySourceToPos(ORS.Pos());
  IF outcol < RightCol THEN w.b(RightCol - outcol) END;
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
  ELSIF state = RegRm     THEN w.s("RegRm")
  ELSIF state = RegRmInd  THEN w.s("RegRmInd")
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

PROCEDURE wobject(o: ORB.Object);
BEGIN
  wclass(o.class);
  w.s(": ");  wtype(o.type);
  w.s(" '");  w.s(o.name);
  w.s("' v"); w.i(o.val);
  w.s(" l");  w.i(o.lev);
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

PROCEDURE wRegRm(ea: X64.EffectiveAddress);
BEGIN
  IF ea.direct THEN
    IF ea.base >= 0 THEN wreg(ea.base) ELSE w.i(ea.value) END
  ELSE
    w.c("[");
    IF ea.base < 0 THEN w.i(ea.value)
    ELSE
      wreg(ea.base);
      IF ea.index >= 0 THEN w.c("+");  wreg(ea.index);
        IF ea.scale > 1 THEN w.c("*");  w.i(ea.scale) END
      END;
      IF ea.value # 0 THEN w.s("+");  w.i(ea.value) END
    END;
    w.c("]");
    IF (ea.base = X64.RSP) & (SPO # 0) THEN w.s(" SPO "); w.i(SPO) END
  END
END wRegRm;

PROCEDURE witem(i: Item);
BEGIN
  w.s("(");  wstate(i.state);
  ASSERT(i.state IN {RegRm, RegRmInd, Condition, ORB.Head, ORB.Typ, ORB.SProc});
  IF i.state IN {RegRm, RegRmInd} THEN w.c(" "); wRegRm(i.ea) END;
  IF i.state = Condition THEN w.c(" "); wcond(i.ea.value) END;
  IF i.readonly THEN w.s(", readonly") END;
  IF i.type # NIL THEN
    w.s(", ");  wtype(i.type);
    IF i.type.form = ORB.String THEN w.s(", length "); w.i(i.length) END
  END;
  w.c(")")
END witem;

PROCEDURE wop(op: INTEGER);
BEGIN
  IF    op = Plus  THEN w.s("Plus")
  ELSIF op = Or    THEN w.s("Or")
  ELSIF op = And   THEN w.s("And")
  ELSIF op = Minus THEN w.s("Minus")
  ELSIF op = Xor   THEN w.s("Xor")
  ELSIF op = Cmp   THEN w.s("Cmp")
  ELSE w.i(op)
  END
END wop;


(* ---------------------------- Code generation ----------------------------- *)


PROCEDURE FixAddr(x: Item): INTEGER;
BEGIN
  RETURN x.ea.value
END FixAddr;

PROCEDURE IncReg;
BEGIN
  (*startRightCol("IncReg RH "); w.i(RH);*)
  IF RH >= 15 THEN ORS.Mark("register stack overflow") ELSE X64.IncReg(RH) END;
  (*w.s(" -> "); w.i(RH); wl*)
END IncReg;

PROCEDURE DecReg;
BEGIN
  IF RH <= 0 THEN ORS.Mark("register stack underflow") ELSE X64.DecReg(RH) END
END DecReg;

PROCEDURE IsSigned(x: ORB.Type): BOOLEAN;  (* returns whether x represents a signed value *)
BEGIN RETURN (x.ref >= ORB.Int8) & (x.ref <= ORB.Int64) END IsSigned;

PROCEDURE IsStructured(x: ORB.Type): BOOLEAN;
BEGIN RETURN (x.form = ORB.Array) OR (x.form = ORB.Record) OR (x.form = ORB.Pointer) END IsStructured;

PROCEDURE IsReference(x: Item): BOOLEAN;
BEGIN RETURN (x.state = ORB.Par) OR IsStructured(x.type) END IsReference;

PROCEDURE MakeCodeFixup(pc: INTEGER; x: Item): INTEGER;
VAR result: INTEGER;
BEGIN
  ASSERT(x.ea.fixup >= 0);
  result := x.ea.value;
  IF x.ea.fixup = 0 THEN
    ASSERT(result < 80000000H)                   (* 1/0, 31/module offset *)
  ELSE
    ASSERT(result < 800000H);
    INC(result, 80000000H + LSL(x.ea.fixup-1, 24))  (* 1/1, 5/module no, 24/import index *)
  END
RETURN result END MakeCodeFixup;


PROCEDURE IsImmediate*(i: Item): BOOLEAN;
BEGIN RETURN (i.state = RegRm) & i.ea.direct & (i.ea.base < 0) END IsImmediate;


PROCEDURE LoadImmediate(reg, val: INTEGER);
BEGIN
  IF val = 0 THEN                             (* Clear register with 32 bit XOR *)
    X64.EmitRex(FALSE, reg, 0, reg); X64.Emit(1, 31H); X64.Emit(1, 0C0H + LSL(reg MOD 8, 3) + reg MOD 8)
  ELSIF (val > 0) & (val < 100000000H) THEN   (* Load 32 bit unsigned value as 32 bit load with zero extension *)
    X64.EmitRex(FALSE, reg, 0,0); X64.Emit(1, 0B8H + reg MOD 8); X64.Emit(4, val);
  ELSIF (val < 0) & (val >= -80000000H) THEN
    (* Load 32 bit negative value with sign extended move *)
    X64.EmitRex(TRUE, reg, 0,0); X64.Emit(1, 0C7H); X64.Emit(1, 0C0H + reg MOD 8); X64.Emit(4,val);
  ELSE
    (* Need full 64 bit literal *)
    X64.EmitRex(TRUE, reg, 0,0); X64.Emit(1, 0B8H + reg MOD 8); X64.Emit(8,val);
  END
END LoadImmediate;


PROCEDURE LoadRegFromEa(reg, size: INTEGER; signed: BOOLEAN; ea: X64.EffectiveAddress);
(* Loads all 64 bits of reg from sized data at ea *)
BEGIN
  IF ea.direct & (ea.base < 0) THEN  (* Load immediate value *)
    LoadImmediate(reg, ea.value)
  ELSE                         (* Load reg from size at [base+index*scale+offset] *)
    IF signed THEN
      X64.EmitRex(TRUE, reg, 0, ea.base);
      IF    size = 1 THEN X64.Emit(1, 0FH); X64.Emit(1, 0BEH);   (* movsx  r64, r/m8  *)
      ELSIF size = 2 THEN X64.Emit(1, 0FH); X64.Emit(1, 0BFH);   (* movsx  r64, r/m16 *)
      ELSIF size = 4 THEN X64.Emit(1, 63H)                       (* movsxd r64, r/m32 *)
                     ELSE X64.Emit(1, 8BH)                       (* mov    r64, r/m64 *)
      END
    ELSE (* Unsigned *)
      X64.EmitRex(size # 4, reg, 0, ea.base);
      IF    size = 1 THEN X64.Emit(1, 0FH); X64.Emit(1, 0B6H);   (* movzx  r64, r/m8  *)
      ELSIF size = 2 THEN X64.Emit(1, 0FH); X64.Emit(1, 0B7H);   (* movzx  r64, r/m16 *)
                     ELSE X64.Emit(1, 8BH)                       (* mov    r32/64, rm32/64 *)
      END
    END;
    X64.EmitModRegRm(reg, SPO, ea)
  END
END LoadRegFromEa;


PROCEDURE FollowIndirect(VAR i: Item);
BEGIN
  ASSERT(i.state = RegRmInd);

  LoadRegFromEa(RH, 64, FALSE, i.ea);
  X64.Disassemble("FollowIndirect");

  X64.ClearEA(i.ea);
  i.state     := RegRm;
  i.ea.direct := FALSE;
  i.ea.base   := RH;
  IncReg;
END FollowIndirect;


PROCEDURE LoadValueToReg(VAR i: Item);  (* Ensure value is in register *)
VAR size: INTEGER;
BEGIN
  IF i.state = RegRmInd THEN FollowIndirect(i) END;

  (* Convert TRUE/FALSE conditions to immediate values *)
  IF (i.state = Condition) & (i.ea.value < 2) THEN
    i.state := RegRm;  i.ea.direct := TRUE;  i.ea.base := -1
  END;

  IF i.state = Condition THEN  (* Generate code to load condition to reg *)
    X64.EmitRex(FALSE, 0, 0, RH);
    X64.Emit(1, 0FH);  X64.Emit(1, i.ea.value + 10H);
    X64.Emit(1, 0C0H + RH MOD 8);
    X64.Disassemble("LoadValueToReg from condition");
    X64.EmitRex(FALSE, RH, 0, RH);
    X64.Emit(1, 0FH);
    X64.Emit(1, 0B6H);
    X64.Emit(1, 0C0H + RH MOD 8 * 8 + RH MOD 8);
    X64.Disassemble("");
    X64.ClearEA(i.ea);
    i.state     := RegRm;
    i.ea.direct := TRUE;
    i.ea.base   := RH;
    IncReg
  ELSIF IsImmediate(i) THEN
    LoadImmediate(RH, i.ea.value);
    X64.Disassemble("LoadValueToReg from immediate");
    X64.ClearEA(i.ea);
    i.state     := RegRm;
    i.ea.direct := TRUE;
    i.ea.base   := RH;
    IncReg
  ELSIF ~i.ea.direct THEN  (* Only load if not already in a register  *)
    LoadRegFromEa(RH, i.type.size, IsSigned(i.type), i.ea);
    X64.Disassemble("LoadValueToReg from EA");
    X64.ClearEA(i.ea);
    i.state     := RegRm;
    i.ea.direct := TRUE;
    i.ea.base   := RH;
    IncReg
  END
END LoadValueToReg;

PROCEDURE SetCondition(VAR x: Item; cond: INTEGER);
BEGIN
  X64.ClearEA(x.ea);
  x.state    := Condition;
  x.ea.base  := 0;       (* prev instr needing patching to after this instruction *)
  x.ea.index := 0;       (* prev instruction needing patch to this instruction *)
  x.ea.value := cond
END SetCondition;

PROCEDURE LoadCondition(VAR x: Item);  (* Set condition flags based on x *)
VAR oldrh: INTEGER;
BEGIN
  ASSERT(x.type.form = ORB.Bool);
  IF IsImmediate(x) THEN
    ASSERT((x.ea.value = 0) OR (x.ea.value = 1));
    SetCondition(x, x.ea.value)
  ELSE
    oldrh := RH;
    IF x.state = RegRmInd THEN FollowIndirect(x) END;
    (*LoadValueToReg(x);*)
    IF x.ea.direct THEN  (* test reg,reg *)
      X64.EmitPrefices(x.ea.base, 8, x.ea);
      X64.Emit(1, 85H);
      X64.EmitModRegRm(x.ea.base, SPO, x.ea)
    ELSE                 (* cmp mem,0 *)
      X64.EmitRex(FALSE, 0, 0, x.ea.base);
      X64.Emit(1, 80H);
      X64.EmitModRegRm(7, SPO, x.ea);
      X64.Emit(1, 0);
    END;
    X64.Disassemble("LoadCondition");
    RH := oldrh;
    SetCondition(x, CNZ)
  END
END LoadCondition;


PROCEDURE InvertedCondition(cond: INTEGER): INTEGER;
BEGIN
  RETURN SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET, cond) / {0})
END InvertedCondition;


PROCEDURE IsDyadicNoOp(op: INTEGER; src: Item): BOOLEAN;
VAR result: BOOLEAN;
BEGIN result := FALSE;
  IF IsImmediate(src) THEN
    IF src.ea.value = 0 THEN
      result := (op = Plus) OR (op = Minus) OR (op = Or) OR (op = Xor)
    ELSIF src.ea.value = -1 THEN
      result := op = And
    END
  END
RETURN result END IsDyadicNoOp;

PROCEDURE Dyadic(op: INTEGER;  VAR dest, src: Item);  (* dest := dest op src *)
BEGIN
  ASSERT(op IN {Plus, Minus, Cmp, And, Or, Xor});
  startRightCol("Dyadic("); wop(op); w.s("; dest: "); witem(dest); w.s(", src: "); witem(src); wl;

  IF IsImmediate(src) & IsImmediate(dest) THEN

    IF    op = Plus  THEN INC(dest.ea.value, src.ea.value)
    ELSIF op = Minus THEN DEC(dest.ea.value, src.ea.value)
    ELSIF op = Cmp   THEN DEC(dest.ea.value, src.ea.value)
    ELSIF op = And   THEN dest.ea.value := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET,dest.ea.value) * SYSTEM.VAL(SET,src.ea.value));
    ELSIF op = Or    THEN dest.ea.value := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET,dest.ea.value) + SYSTEM.VAL(SET,src.ea.value));
    ELSIF op = Xor   THEN dest.ea.value := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET,dest.ea.value) / SYSTEM.VAL(SET,src.ea.value));
    END

  ELSIF ~IsDyadicNoOp(op, src) THEN

    (*
    w.s("; dyadic op "); wop(op);  wl;
    w.s("  dest "); witem(dest); wl;
    w.s("  src  "); witem(src);  wl;
    *)

    (* TODO - avoid reg load when src is immediate such as: cmp [rsp+10],0 *)

    LoadValueToReg(dest);
    (*w.s("  dest "); witem(dest); wl;*)

    ASSERT(dest.state = RegRm);  ASSERT(dest.ea.direct);

    IF IsImmediate(src) & (src.ea.value = 1) & ((op = Plus) OR (op = Minus)) THEN
      (* INC / DEC *)
      X64.EmitRex(TRUE, 0, 0, dest.ea.base);
      X64.Emit(1, 0FFH);
      X64.Emit(1, 0C0H  +  op MOD 16  +  dest.ea.base MOD 8)
    ELSE
      (* Get source into a representation suitable for X64 dyadic alu operations *)

      IF src.state = RegRmInd THEN FollowIndirect(src) END;

      IF (IsImmediate(src))
       & ((src.ea.value >= 80000000H) OR (src.ea.value < -80000000H)) THEN
        LoadImmediate(RH, src.ea.value);
        X64.ClearEA(src.ea);
        src.ea.base := RH;
        IncReg;
        src.state := RegRm
      END;

      ASSERT(src.state = RegRm);

      (* Generate alu operation *)
      X64.EmitPrefices(dest.ea.base, 8, src.ea);
      IF IsImmediate(src) THEN (* op destreg,immediate *)
        ASSERT((src.ea.value >= -80000000H) & (src.ea.value < 80000000H));
        INC(op, 0C0H);
        IF (src.ea.value >= -80H) & (src.ea.value < 80H) THEN
          X64.Emit(1, 83H);  X64.Emit(1, op + dest.ea.base MOD 8);  X64.Emit(1, src.ea.value)
        ELSE
          X64.Emit(1, 81H);  X64.Emit(1, op + dest.ea.base MOD 8);  X64.Emit(4, src.ea.value)
        END
      ELSE
        X64.Emit(1, op + 3);
        X64.EmitModRegRm(dest.ea.base, SPO, src.ea)
      END
    END;
    X64.Disassemble("Dyadic operator");

    RH := dest.ea.base + 1
  END
END Dyadic;


(* ---- Item construction ---- *)

PROCEDURE ZeroFill(VAR obj: ARRAY OF BYTE);  VAR i: INTEGER;
BEGIN FOR i := 0 TO LEN(obj)-1 DO obj[i] := 0 END END ZeroFill;

PROCEDURE MakeItem*(VAR x: Item; y: ORB.Object; curlev: INTEGER);
BEGIN startRightCol("MakeItem(y: "); wobject(y); w.s(", curlev: "); w.i(curlev); w.s(")");
  ASSERT(y.class IN {ORB.Const, ORB.Var, ORB.Par, ORB.SProc, ORB.Typ});

  ZeroFill(x);
  X64.ClearEA(x.ea);
  x.state    := y.class;
  x.type     := y.type;
  x.ea.value := y.val;
  x.readonly := y.rdo;

  IF y.class IN {ORB.Const, ORB.Var, ORB.Par} THEN
    x.ea.direct := y.class = ORB.Const;

    IF (y.class # ORB.Const) OR (y.type.form = ORB.Proc) THEN
      IF y.class = ORB.Par THEN  ASSERT(y.lev > 0);
        x.state := RegRmInd
      ELSE
        x.state := RegRm
      END;
      IF y.lev > 0 THEN
        x.ea.base := X64.RSP      (* x.level := Stack *)
      ELSE
        x.ea.fixup := -y.lev  (* x.level := Module - y.lev *)
      END
    ELSE
      (* Immediate value *)
      x.state := RegRm
    END
  END;

  w.s(" -> ");
  wstate(x.state); w.s("/"); wform(x.type.form);
  IF x.readonly THEN w.s(", readonly") END;
  IF x.state IN {RegRm, RegRmInd} THEN w.s(" "); wRegRm(x.ea) END;
  IF x.type.form = ORB.String THEN w.s(", length "); w.i(x.length) END;
  wl
END MakeItem;

PROCEDURE MakeConstItem*(VAR x: Item;  t: ORB.Type;  v: INTEGER);
BEGIN
  startRightCol("MakeConstItem, value: "); w.i(v); w.s(", type "); wtype(t); wl;
  ZeroFill(x);
  X64.ClearEA(x.ea);
  x.state := RegRm;
  x.type  := t;
  x.ea.value := v;
END MakeConstItem;

PROCEDURE MakeRealItem*(VAR x: Item; val: REAL);
BEGIN
  startRightCol("MakeRealItem(x; val)"); wl;
  ZeroFill(x);
  X64.ClearEA(x.ea);
  x.state := RegRm;
  x.type  := ORB.realType;
  x.ea.value := SYSTEM.VAL(INTEGER, val)
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
  X64.ClearEA(x.ea);
  x.state := RegRm;
  x.type     := ORB.strType;
  x.readonly := TRUE;
  x.length   := len;

  IF len <= 8 THEN        (* Strings of up to 9 bytes are recorded in item.value *)
    SYSTEM.GET(SYSTEM.ADR(ORS.str), x.ea.value)
  ELSE                    (* Strings of 9 or more bytes are stored in the module's string table *)
    x.ea.direct := FALSE;
    x.ea.value  := FindString(ORS.str, len);
    x.ea.fixup  := 0;  (* Module data fixup *)
  END;

  startRightCol("MakeStringItem('");
  i := 0;
  WHILE (i < len) & (ORS.str[i] # 0X) DO w.c(ORS.str[i]); INC(i) END;
  w.s("')");
  IF (len > 8) & (x.ea.value + len < Strx) THEN w.s(" (reused)") END;
  wl
END MakeStringItem;



(* ---- Item conversion ---- *)

PROCEDURE StrToChar*(VAR x: Item);
BEGIN startRightCol("StrToChar(x)  x"); witem(x); wl;
  ASSERT(x.state     = RegRm);
  ASSERT(x.ea.base   < 0);
  ASSERT(x.type.form = ORB.String);
  ASSERT(x.length    = 2);
  ASSERT(x.readonly);
  ASSERT(x.ea.direct);

  x.type     := ORB.charType;
  x.ea.value := x.ea.value MOD 256
END StrToChar;


(* ---- Assignment ---- *)

PROCEDURE Store*(VAR dest, src: Item); (* dest := src *)
VAR size, op: INTEGER;
BEGIN startRightCol("Store(dest: "); witem(dest); w.s(", src: "); witem(src); wl;
  size  := dest.type.size;

  IF dest.state = RegRmInd THEN FollowIndirect(dest) END;

  ASSERT(dest.state = RegRm);
  ASSERT(~dest.ea.direct);

  IF (~IsImmediate(src)) OR (src.ea.value >= 80000000H) OR (src.ea.value < -80000000H) THEN
    LoadValueToReg(src)
  END;

  ASSERT(src.ea.direct);

  IF IsImmediate(src) THEN op := 0C6H ELSE op := 88H END;
  IF size # 1 THEN INC(op) END;

  X64.EmitPrefices(src.ea.base, size, dest.ea);
  X64.Emit(1, op);
  X64.EmitModRegRm(src.ea.base, SPO, dest.ea);

  IF IsImmediate(src) THEN
    IF size < 4 THEN X64.Emit(size, src.ea.value)
    ELSE             X64.Emit(4,    src.ea.value) END
  END;

  X64.Disassemble("Store");
  DecReg
END Store;


PROCEDURE StoreStruct*(VAR x, y: Item); (* x := y, frame = 0 *)
BEGIN startRightCol("StoreStruct(x, y) x := y"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl
END StoreStruct;

PROCEDURE CopyString*(VAR x, y: Item);  (* x := y *)
BEGIN startRightCol("CopyString(x, y) x := y"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl
END CopyString;


(* ---- selection - fields, indices & pointers ---- *)

PROCEDURE Index*(VAR x, y: Item);   (* x := x[y] *)
VAR elemsize: INTEGER;  oldrh: INTEGER;
BEGIN startRightCol("Index(x, y)"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl;
  ASSERT(~IsImmediate(x));
  ASSERT(~x.ea.direct);
  IF x.state = RegRmInd THEN FollowIndirect(x) END;
  ASSERT(x.type.form = ORB.Array);
  ASSERT(x.state = RegRm);
  ASSERT(~x.ea.direct);
  elemsize := x.type.base.size;
  w.b(RightCol + 2); w.s("x base type: "); wtype(x.type.base); wl;
  w.b(RightCol + 2); w.s("elemsize: "); w.i(elemsize); wl;

  IF IsImmediate(y) THEN
    INC(x.ea.value, y.ea.value * elemsize)
  ELSE
    oldrh := RH;
    LoadValueToReg(y);

    (* Use X64 SIB index and scale if possible *)
    IF (x.ea.base >= 0) & (x.ea.index < 0) & (elemsize IN {1, 2, 4, 8}) THEN
      x.ea.index := y.ea.base;  x.ea.scale := 0;
      IF    elemsize = 2 THEN x.ea.scale := 1
      ELSIF elemsize = 4 THEN x.ea.scale := 2
      ELSIF elemsize = 8 THEN x.ea.scale := 3
      END
    ELSE
      X64.EmitPrefices(x.ea.base, 8, y.ea);  (* Add y reg to x reg *)
      X64.Emit(1, Plus + 3);
      X64.EmitModRegRm(x.ea.base, SPO, y.ea)
    END;
    RH := oldrh;
  END
END Index;

PROCEDURE DeRef*(VAR x: Item);
BEGIN startRightCol("DeRef(x), x: "); witem(x); wl;
  ASSERT(x.type.form = ORB.Pointer);
  LoadValueToReg(x);
  x.ea.direct := FALSE
END DeRef;

PROCEDURE Field*(VAR x: Item; y: ORB.Object);   (* x := x.y *)
BEGIN startRightCol("Field(x: Item; y: Object)"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x);   wl;
  w.b(RightCol + 2); w.s("y: "); wobject(y); wl;
END Field;


(* ---- Paramter passing ---- *)

PROCEDURE ValueParam*(VAR x: Item);
BEGIN startRightCol("ValueParam(x: Item)  x: "); witem(x);
  w.s(" (RH "); w.i(RH); w.s(")"); wl;
  LoadValueToReg(x)
END ValueParam;

PROCEDURE VarParam*(VAR x: Item; ftype: ORB.Type);
BEGIN startRightCol("VarParam(x: Item; ftype: Type)"); wl;
  w.b(RightCol + 2); w.s("x:     "); witem(x);     wl;
  w.b(RightCol + 2); w.s("ftype: "); wtype(ftype); wl;
END VarParam;

PROCEDURE OpenArrayParam*(VAR x: Item);
BEGIN startRightCol("OpenArrayParam(x: Item)  x: "); witem(x);  wl;
END OpenArrayParam;

PROCEDURE StringParam*(VAR x: Item);
BEGIN startRightCol("StringParam(x: Item)  x: "); witem(x);  wl;
END StringParam;


(* ---- Procedure / function calls ---- *)

PROCEDURE SaveRegisters;
BEGIN
  ASSERT(SPO = 0);
  WHILE SPO < RH DO
    X64.EmitRex(FALSE, SPO, 0, 0);  X64.Emit(1, 50H + SPO MOD 8);  (* Push SPO *)
    X64.Disassemble("save reg");
    X64.IncReg(SPO)
  END;
  RH := 0
END SaveRegisters;

PROCEDURE RestoreRegisters;
BEGIN
  IF SPO > 0 THEN REPEAT
    X64.DecReg(SPO);
    X64.EmitRex(FALSE, SPO, 0, 0);  X64.Emit(1, 58H + SPO MOD 8);  (* Push SPO *)
    X64.Disassemble("restore reg");
  UNTIL SPO = 0 END
END RestoreRegisters;

PROCEDURE PrepCall*(VAR x: Item);
BEGIN startRightCol("PrepCall(x: "); witem(x); wl;
  (* From Wirth ??: IF x.mode > ORB.Par THEN load(x) END; *)
  IF x.type.base.form = ORB.NoTyp THEN RH := 0 END; (* procedure *)
  IF RH > 0 THEN SaveRegisters END
END PrepCall;

PROCEDURE Call*(VAR x: Item);
BEGIN startRightCol("Call(x: "); witem(x); wl;
  (* Make the call *)
  IF x.state = RegRmInd THEN FollowIndirect(x) END;
  IF IsImmediate(x) THEN
    X64.Emit(1, 0E8H);
    X64.Emit(4, x.ea.value - (X64.PC + 4))
  ELSE
    X64.EmitRex(FALSE, 0, 0, x.ea.base);  (* call instruction defaults to 64 bit arg *)
    X64.Emit(1, 0FFH);
    X64.EmitModRegRm(2, SPO, x.ea)
  END;
  X64.Disassemble("Call");

  (* Follow up on return *)
  IF x.type.base.form = ORB.NoTyp THEN (* procedure *)
    RH := 0
  ELSE
    RH := SPO;
    IF RH > 0 THEN (* Move function result from rax to RH *)
      X64.EmitRex(TRUE, RH, 0, 0);
      X64.Emit(1, 8BH);
      X64.Emit(1, 0C0H + RH MOD 8 * 8);
      X64.Disassemble("Move fn result to RH");
      RestoreRegisters
    END;
    x.state := RegRm;
    X64.ClearEA(x.ea);
    x.ea.base := RH; (* function result reg *)
    IncReg
  END
END Call;

PROCEDURE Enter*(parblksize, locblksize: INTEGER);
VAR i: INTEGER;
BEGIN
  startRightCol("Enter(parblksize ");  w.i(parblksize);
  w.s(", locblksize ");        w.i(locblksize);  w.sl(")");
  RH := 0;  SPO := 0;

  (* Reserve space for locals *)
  DEC(locblksize, parblksize); (* Parameter space will be reserved by pushes *)
  IF locblksize > 0 THEN
    X64.Emit(1, 48H);
    IF locblksize < 80H THEN
      X64.Emit(1, 83H);  X64.Emit(1, 0ECH);  X64.Emit(1, locblksize)
    ELSE
      X64.Emit(1, 81H);  X64.Emit(1, 0ECH);  X64.Emit(4, locblksize)
    END
  END;
  X64.Disassemble("Reserve local var space");

  (* Push parameters backwards *)
  i := parblksize;  RH := i DIV 8; IF RH >= 4 THEN INC(RH) END;
  WHILE i > 0 DO
    DecReg;
    X64.EmitRex(FALSE, RH, 0, 0);  X64.Emit(1, 50H + RH MOD 8);
    DEC(i, 8)
  END;
  X64.Disassemble("Save parameters");

  ASSERT(RH = 0)
END Enter;

PROCEDURE Return*(form: INTEGER; VAR x: Item; size: INTEGER);
BEGIN
  startRightCol("Return("); wform(form);
  w.s(", x: "); witem(x);
  w.s(", size "); w.i(size); wl;

  IF form # ORB.NoTyp THEN LoadValueToReg(x) END;

  IF size > 0 THEN
    X64.Emit(1, 48H);
    IF size < 80H THEN
      X64.Emit(1, 83H);  X64.Emit(1, 0C4H);  X64.Emit(1, size)
    ELSE
      X64.Emit(1, 81H);  X64.Emit(1, 0C4H);  X64.Emit(4, size)
    END;
    X64.Disassemble("Release local stack space")
  END;

  X64.Emit(1, 0C3H); (* ret *)
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
  w.s("; x, y)"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl
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
VAR size, op, reg, imm, immsize: INTEGER;
BEGIN startRightCol("Increment(");
  IF upordown = 0 THEN w.s("up") ELSE w.s("down") END;
  w.s(", x: "); witem(x); w.s(", y: "); witem(y); wl;

  IF x.state = RegRmInd THEN FollowIndirect(x) END;
  ASSERT(~IsImmediate(x));
  size := x.type.size;
  IF (y.state = 0)
  OR (IsImmediate(y) & (y.ea.value > -80000000H) & (y.ea.value < 80000000H)) THEN
    IF (y.state = 0) OR (y.ea.value = 1) THEN  (* Add 1 or -1 to x *)
      IF upordown = 0 THEN reg := 0    ELSE reg := 1    END;
      IF size = 1     THEN op  := 0FEH ELSE op  := 0FFH END;
      X64.EmitPrefices(0, size, x.ea);
      X64.Emit(1, op);
      X64.EmitModRegRm(reg, SPO, x.ea)
    ELSIF y.ea.value # 0 THEN  (* Add/Sub constant to/from x *)
      IF x.ea.direct THEN size := 8 END;  (* registers are always 64 bit *)
      imm := y.ea.value;

      IF    (imm >= -80H) & (imm < 80H) THEN immsize := 1
      ELSIF size >= 4                   THEN immsize := 4
      ELSE                                   immsize := size
      END;

      IF    size    = 1 THEN op := 80H  (* Byte immediate to byte memory *)
      ELSIF immsize = 1 THEN op := 83H  (* Byte immediate to 64 bit register *)
      ELSE                   op := 81H
      END;

      X64.EmitPrefices(0, size, x.ea);
      X64.Emit(1, op);
      IF upordown = 0 THEN
        X64.EmitModRegRm(0, SPO, x.ea)
      ELSE
        X64.EmitModRegRm(5, SPO, x.ea)
      END;
      X64.Emit(immsize, imm)
    END
  ELSE  (* Add/sub register to/from ea *)
    LoadValueToReg(y);
    IF upordown = 0 THEN op := Plus ELSE op := Minus END;
      IF size > 1 THEN INC(op) END;
      X64.EmitPrefices(y.ea.base, size, x.ea);
      X64.Emit(1, op);
      X64.EmitModRegRm(y.ea.base, SPO, x.ea)
  END;
  X64.Disassemble("INC/DEC")
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
  LoadValueToReg(x);  x.type := y.type;  x.ea.direct := FALSE;  Store(y, x);
END Get;

PROCEDURE Put*(VAR x, y: Item);
BEGIN startRightCol("Put(x: "); witem(x); w.s(", y: "); witem(y); wl;
  LoadValueToReg(x);  x.type := y.type;  x.ea.direct := FALSE;  Store(x, y);
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
BEGIN startRightCol("Singleton(x: "); witem(x); w.sl(")");
  IF IsImmediate(x) THEN
    x.ea.value := LSL(1, x.ea.value)
  ELSE
    LoadValueToReg(x);

    X64.EmitRex(TRUE, x.ea.base, 0, RH);  (* mov RH, x.ea.base *)
    X64.Emit(1, 89H);
    X64.Emit(1, 0C0H  +  x.ea.base MOD 8 * 8  +  RH MOD 8);
    X64.Disassemble("Singleton");

    X64.EmitRex(FALSE, x.ea.base, 0, x.ea.base);  (* xor x.ea.base, x.ea.base *)
    X64.Emit(1, 31H);
    X64.Emit(1, 0C0H +  x.ea.base MOD 8 * 8  +  x.ea.base MOD 8);
    X64.Disassemble("");

    X64.EmitRex(TRUE, RH, 0, x.ea.base);
    X64.Emit(1, 0FH);  X64.Emit(1, 0ABH);
    X64.Emit(1, 0C0H  +  RH MOD 8 * 8  +  x.ea.base MOD 8);
    X64.Disassemble("");
  END
END Singleton;


PROCEDURE In*(VAR x, y: Item);  (* x := x IN y *)
VAR oldrh: INTEGER;
BEGIN startRightCol("In(x: "); witem(x);  w.s(", y: "); witem(y); wl;
  oldrh := RH;
  IF x.state = RegRmInd THEN FollowIndirect(x) END;
  IF y.state = RegRmInd THEN FollowIndirect(y) END;
  ASSERT(x.state = RegRm);  ASSERT(y.state = RegRm);
  IF IsImmediate(y) THEN LoadValueToReg(y) END;
  IF IsImmediate(x) THEN
    X64.EmitPrefices(0, 8, y.ea);
    X64.Emit(1, 0FH);  X64.Emit(1, 0BAH);
    X64.EmitModRegRm(4, SPO, y.ea);
    X64.Emit(1, x.ea.value);
    X64.Disassemble("In");
  ELSE
    LoadValueToReg(x);
    X64.EmitPrefices(x.ea.base, 8, y.ea);
    X64.Emit(1, 0FH);  X64.Emit(1, 0AFH);
    X64.EmitModRegRm(x.ea.base, SPO, y.ea);
    X64.Disassemble("In");
  END;
  SetCondition(x, CC);
  RH := oldrh
END In;

PROCEDURE SetOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR xs, ys: SET;
BEGIN startRightCol("SetOp("); wsym(op); w.s("; x: "); witem(x); w.s(", y: "); witem(y); wl;
  IF IsImmediate(x) & IsImmediate(y) THEN
    xs := SYSTEM.VAL(SET, x.ea.value);  ys := SYSTEM.VAL(SET, y.ea.value);
    IF    op = ORS.plus  THEN xs := xs + ys
    ELSIF op = ORS.minus THEN xs := xs - ys
    ELSIF op = ORS.times THEN xs := xs * ys
    ELSIF op = ORS.rdiv  THEN xs := xs / ys
    END ;
    x.ea.value := SYSTEM.VAL(INTEGER, xs)
  ELSE
    (* Convert op to x86 opcode *)
    IF op = ORS.minus THEN  (* Invert y *)
      IF IsImmediate(y) THEN
        y.ea.value := -y.ea.value - 1  (* = invert *)
      ELSE
        LoadValueToReg(y);
        X64.EmitRex(TRUE, 0, 0, y.ea.base);
        X64.Emit(1, 0F7H);
        X64.EmitModRegRm(2, SPO, y.ea);
        X64.Disassemble("Invert for SET -")
      END
    END;
    IF    op = ORS.times THEN op := And
    ELSIF op = ORS.minus THEN op := And
    ELSIF op = ORS.or    THEN op := Or
    ELSIF op = ORS.rdiv  THEN op := Xor
    ELSE ASSERT(FALSE)
    END;
    Dyadic(op, x, y)
  END
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
  IF x.state # Condition THEN LoadCondition(x) END;
  x.ea.value := InvertedCondition(x.ea.value)
END Not;

PROCEDURE And1*(VAR x: Item);   (* x := x & *)
BEGIN startRightCol("And1(x: "); witem(x); w.sl(")");
  IF x.state # Condition THEN LoadCondition(x) END;
  X64.Emit(1, 0FH); X64.Emit(1, InvertedCondition(x.ea.value));
  X64.Emit(4, x.ea.base);    x.ea.base  := X64.PC - 4;
  X64.Disassemble("And1");
  FixLink(x.ea.index); x.ea.index := 0
END And1;

PROCEDURE Or1*(VAR x: Item);   (* x := x OR *)
BEGIN startRightCol("Or1(x: "); witem(x); w.sl(")");
  IF x.state # Condition THEN LoadCondition(x) END;
  X64.Emit(1, 0FH); X64.Emit(1, x.ea.value);
  X64.Emit(4, x.ea.index);  x.ea.index := X64.PC - 4;
  X64.Disassemble("And2");
  FixLink(x.ea.base); x.ea.base  := 0
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
  IF y.state # Condition THEN LoadCondition(y) END;
  x.ea.base  := merged(y.ea.base, x.ea.base);
  x.ea.index := y.ea.index;
  x.ea.value := y.ea.value
END And2;

PROCEDURE Or2*(VAR x, y: Item);   (* x := x OR y *)
BEGIN startRightCol("Or2(x: "); witem(x); w.sl(")");
  IF y.state # Condition THEN LoadCondition(y) END;
  x.ea.base  := y.ea.base;
  x.ea.index := merged(y.ea.index, x.ea.index);
  x.ea.value := y.ea.value
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

PROCEDURE IntRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
VAR oldrh, immsize: INTEGER;
BEGIN startRightCol("IntRelation("); wsym(op); w.s(", x: "); witem(x);  w.s(", y: "); witem(y); wl;
  ASSERT(x.state IN {RegRm, RegRmInd});  ASSERT(y.state IN {RegRm, RegRmInd});
  oldrh := RH;
  IF x.state = RegRmInd THEN FollowIndirect(x) END;
  IF y.state = RegRmInd THEN FollowIndirect(y) END;

  IF IsImmediate(y) & (y.ea.value >= -80000000H) & (y.ea.value < 80000000H) THEN
    IF (y.ea.value >= -80H) & (y.ea.value < 80H) THEN immsize := 1 ELSE immsize := 4 END;
    X64.EmitPrefices(0, x.type.size, x.ea);
    IF    x.type.size = 1 THEN X64.Emit(1, 80H)
    ELSIF immsize     = 1 THEN X64.Emit(1, 83H)
    ELSE                       X64.Emit(1, 81H)
    END;
    X64.EmitModRegRm(7, SPO, x.ea);
    X64.Emit(immsize, y.ea.value);
    X64.Disassemble("IntRelation rhs immediate")
  ELSE
    Dyadic(Cmp, x, y);
  END;
  RH := oldrh;  (* Result is in flags *)
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
  IF    op = ORS.plus  THEN op := Plus
  ELSIF op = ORS.minus THEN op := Minus
  ELSIF op = ORS.or    THEN op := Or
  ELSE ASSERT(FALSE)
  END;
  Dyadic(op, x, y)
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
    x.ea.value := -x.ea.value
  ELSE
    LoadValueToReg(x);
    X64.EmitPrefices(3, 8, x.ea);
    X64.Emit(1, 0F7H);
    X64.EmitModRegRm(3, SPO, x.ea);
    X64.Disassemble("Neg")
  END
END Neg;


(* ---- Control flow ---- *)

(* Use of Item.ea fields for conditions:
*
*  ea.value - the condition code (uses symbol encodings ORS.eql .. ORS.geq)
*  ea.base  - address of prev instr if any needing patching with curr target
*  ea.index - address of any forward jump list targeting this instruction
*)

PROCEDURE CFJump*(VAR x: Item);
VAR cond: INTEGER;
BEGIN startRightCol("CFJump(x: "); witem(x);  w.sl(")");
  IF x.state # Condition THEN LoadCondition(x) END;
  cond := InvertedCondition(x.ea.value);
  ASSERT(cond DIV 16 = 8);
  X64.Emit(1, 0FH);  X64.Emit(1, cond);
  FixLink(x.ea.index);  (* Fix any links that target this instruction *)
  X64.Emit(4, x.ea.base);  x.ea.base := X64.PC - 4;
  X64.Disassemble("CFJump")
END CFJump;

PROCEDURE FJump*(VAR L: INTEGER);
BEGIN startRightCol("FJump(L "); w.i(L); w.sl(")");
  X64.Emit(1, 0E9H);  X64.Emit(4, L);  L := X64.PC-4;
  X64.Disassemble("FJump")
END FJump;

PROCEDURE BJump*(L: INTEGER);
BEGIN startRightCol("BJump(L "); w.i(L); w.sl(")");
  X64.Emit(1, 0E9H);  X64.Emit(4, L - X64.PC);
  X64.Disassemble("BJump")
END BJump;

PROCEDURE CBJump*(VAR x: Item; L: INTEGER);
BEGIN startRightCol("CBJump(x: "); witem(x); w.sl(", L: "); w.i(L); w.sl(")");
END CBJump;

PROCEDURE FixOne*(at: INTEGER);
BEGIN startRightCol("FixOne(at "); w.i(at); w.sl(")")
END FixOne;

(*
PROCEDURE Fixup*(VAR x: Item);  BEGIN FixLink(x.a) END Fixup;
*)

PROCEDURE Here*(): INTEGER;
BEGIN RETURN X64.PC END Here;

PROCEDURE For0*(VAR x, y: Item);
BEGIN startRightCol("For0(x, y)"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl
END For0;

PROCEDURE For1*(VAR x, y, z, incr: Item; VAR L: INTEGER);
BEGIN startRightCol("For1(x, y, z, incr; L "); w.i(L);  w.sl(")");
  w.b(RightCol + 2); w.s("x:    "); witem(x);    wl;
  w.b(RightCol + 2); w.s("y:    "); witem(y);    wl;
  w.b(RightCol + 2); w.s("z:    "); witem(z);    wl;
  w.b(RightCol + 2); w.s("incr: "); witem(incr); wl
END For1;

PROCEDURE For2*(VAR x, y, incr: Item);
BEGIN startRightCol("For2(x, y)"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl
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

PROCEDURE CheckRegs*;
BEGIN startRightCol("CheckRegs"); wl
END CheckRegs;


(* ---- ---- *)

PROCEDURE Open*(v: INTEGER; sourcefn: ARRAY OF CHAR);
BEGIN
  w.s("ORG Open(v "); w.i(v); w.s("; '"); w.s(sourcefn); w.sl("')");

  sourcefile := Files.Old(sourcefn);  Files.Set(source, sourcefile, 0);
  sourceBol := 0;  sourceLine := 1;   outcol := 0;
  GetChar;
  Strx := 0;  RH := 0;  SPO := 0;
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
