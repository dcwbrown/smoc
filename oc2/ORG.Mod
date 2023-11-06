MODULE ORG;  (* DCWB June 2023; Code generator for X64 *)

(* Originally: N.Wirth, 16.4.2016 / 4.4.2017 / 17.9.2018  Oberon compiler; code generator for RISC*)

IMPORT SYSTEM, Files, ORS, ORB, w := Writer, K := Kernel, X64;

CONST
  WordSize* = ORB.WordSize;
  MaxPC     = 10000H;
  RightCol  = 70;
  MaxImpMod = 32;     (* Max no. imported modules *)

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

  (* System library procedure indices *)
  NewProc                = 0;
  (* error traps *)
  AssertionFailureProc   = 1;
  ArraySizeMismatchProc  = 2;
  UnterminatedStringProc = 3;

  (* Trace flags *)
  TraceMakeItem = 0;

  (* Promote X64 reg names to ORG *)
  RAX = X64.RAX;  RCX = X64.RCX;  RDX = X64.RDX;  RBX = X64.RBX;
  RSP = X64.RSP;  RBP = X64.RBP;  RSI = X64.RSI;  RDI = X64.RDI;


TYPE
  Item* = RECORD
    state*:    INTEGER;  (* Initially from object.class *)
    type*:     ORB.Type;
    o*:        X64.Operand;
    readonly*: BOOLEAN;
    length*:   INTEGER;  (* Only if type = strType *)
  END;


VAR
  Sourcefile: Files.File;
  Source:     Files.Rider;
  SourceBol:  INTEGER;      (* pos at beginiing of line *)
  SourceLine: INTEGER;      (* Line number *)
  Sourcechar: CHAR;

  InCode:     BOOLEAN;

  Str:        ARRAY 1024 OF CHAR;  (* Max strings per procedure/module body *)
  Strx:       INTEGER;
  StrFix:     INTEGER;      (* Latest code offset with string offset needing fixing *)

  fixorgT:    INTEGER;      (* TODO - manage fixorgs ... *)
  Varsize:    INTEGER;      (* Module global VAR size *)

  PrevImport: ARRAY MaxImpMod OF INTEGER;

  Trace:      SET;



(* ---- Source display ---- *)

PROCEDURE Pos(): INTEGER;
BEGIN RETURN Files.Pos(Source) - 1 END Pos;

PROCEDURE GetChar;
VAR prevch: CHAR;
BEGIN  ASSERT(~Source.eof);
  prevch := Sourcechar;
  Files.Read(Source, Sourcechar);
  IF Source.eof THEN Sourcechar := 0X
  ELSE
    IF (prevch = 0DX) OR (prevch = 0AX) THEN
      IF (prevch = 0DX) & (Sourcechar = 0AX) THEN Files.Read(Source, Sourcechar) END;
      SourceBol := Pos();
      INC(SourceLine)
    END
  END
END GetChar;

PROCEDURE wl; BEGIN w.l      END wl;
PROCEDURE wc; BEGIN w.c(",") END wc;

PROCEDURE DisplaySourceToPos(pos: INTEGER);
VAR ch: CHAR;
BEGIN
  w.in(SourceLine, 4); w.s(": ");
  w.b(Pos() - SourceBol);  (* Space to current column *)

  (* Copy Source text to pos *)
  WHILE ~Source.eof & (Pos() < pos) DO
    IF (Sourcechar = 0DX) OR (Sourcechar = 0AX) THEN
      GetChar; w.l; w.in(SourceLine, 4); w.s(": ")
    ELSE
      w.c(Sourcechar); GetChar
    END
  END;

  (* Space to RightCol allowing for 6 columns already used by linenumber *)
  IF Pos() - SourceBol < (RightCol - 6) THEN
    w.b((RightCol - 6) - (Pos() - SourceBol));
  ELSE
    w.s("  ")
  END
END DisplaySourceToPos;

PROCEDURE startRightCol*(str: ARRAY OF CHAR);
BEGIN
  IF ORB.ListCodegen THEN
    DisplaySourceToPos(ORS.Pos());
    w.s(str)
  ELSE
    w.SkipLine
  END
END startRightCol;

PROCEDURE Disassemble*(str: ARRAY OF CHAR);
BEGIN
  IF ORB.ListAssembly THEN
    (* Skip source to first non-blank *)
    WHILE ~Source.eof & (Sourcechar <= " ") & (Pos() < ORS.Pos()) DO GetChar END;
    IF Pos() < ORS.Pos() THEN
      w.in(SourceLine, 4); w.s(": ");
      w.b(Pos() - SourceBol);  (* Space to current column *)
      (* Copy Source text to pos *)
      WHILE ~Source.eof & (Pos() < ORS.Pos()) DO
        IF (Sourcechar = 0DX) OR (Sourcechar = 0AX) THEN
          GetChar; w.l; w.in(SourceLine, 4); w.s(": ")
        ELSE
          w.c(Sourcechar); GetChar
        END
      END;
      wl
    END;
    X64.Disassemble(str)
  END
END Disassemble;

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

PROCEDURE wclass*(class: INTEGER);
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


PROCEDURE wtype*(t: ORB.Type);
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
  IF o = NIL THEN w.s("NIL") ELSE
    wclass(o.class);
    w.s(": ");  wtype(o.type);
    w.s(" '");  w.s(o.name);
    w.s("' v"); w.i(o.val);
    w.s(" l");  w.i(o.lev);
    IF o.rdo THEN w.s(" ro") END
  END
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
      IF    o.size = 1  THEN w.s("byte")
      ELSIF o.size = 2  THEN w.s("word")
      ELSIF o.size = 4  THEN w.s("dword")
      ELSE w.i(o.size); w.s("byte")
      END
    END;
    w.c("[");
    IF (o.base < 0) & ~o.parptr THEN
      w.i(o.disp)
    ELSE
      IF o.parptr THEN
        w.s("[rsp");
        IF o.base >= 0 THEN w.c("+") END;
        w.i(o.base); w.s("]")
      ELSE
        wreg(o.base)
      END;
      IF o.index >= 0 THEN w.c("+");  wreg(o.index);
        IF o.scale > 1 THEN w.c("*");  w.i(o.scale) END
      END;
      IF o.disp # 0 THEN IF o.disp > 0 THEN w.s("+") END;  w.i(o.disp) END
    END;
    w.c("]");
    IF (o.base = RSP) & (X64.SPO # 0) THEN w.s(" SPO "); w.i(X64.SPO) END
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
  IF n < 0 THEN ORS.Mark("out of registers");  n := 15
  ELSE X64.ReserveReg(n) END
RETURN n END ReserveNextFree;

PROCEDURE FindAndReserveReg(desired: INTEGER; allow: SET): INTEGER;
VAR n: INTEGER;
BEGIN n := X64.FindFreeReg(desired, allow);
  IF n < 0 THEN ORS.Mark("out of registers");  n := 15
  ELSE X64.ReserveReg(n) END
RETURN n END FindAndReserveReg;

PROCEDURE CheckRegs*(msg: ARRAY OF CHAR);
BEGIN
  IF (X64.Free # X64.AllFree) OR (X64.SPO # 0) THEN
    startRightCol("CheckRegs("); w.s(msg);
    w.s("): Used "); wset(X64.AllFree - X64.Free);
    w.s(", Free: "); wset(X64.Free);
    w.s(", SPO "); w.i(X64.SPO); w.sl(".")
  END;
  IF X64.Free # X64.AllFree   THEN ORS.Mark("Reg Stack"); X64.ClearRegs;  ASSERT(FALSE) END;
  IF X64.PC >= X64.MaxPC - 40 THEN ORS.Mark("program too long");          ASSERT(FALSE) END;
  IF X64.SPO # 0              THEN ORS.Mark("SPO error"); X64.ClearStack; ASSERT(FALSE) END
END CheckRegs;

PROCEDURE IsSigned(x: ORB.Type): BOOLEAN;  (* returns whether x represents a signed integer value *)
BEGIN RETURN (x.ref >= ORB.Int8) & (x.ref <= ORB.Int64) END IsSigned;

PROCEDURE EmitImportAddress(module, expno: INTEGER);
VAR offset: INTEGER;
BEGIN
  IF PrevImport[module] # 0 THEN
    offset := X64.PC - PrevImport[module]
  ELSE
    offset := 0
  END;
  ASSERT(offset < 1000000H);
  PrevImport[module] := X64.PC;
  ASSERT(expno < 256);
  X64.EmitBytes(4, LSL(expno, 24) + offset);
END EmitImportAddress;

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
  ASSERT((cond >= CF) & (cond <= CG) & ((cond <= CT) OR (cond >= CO)));
  x.state   := Condition;
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


PROCEDURE Load(VAR x: Item);
VAR reg: INTEGER;
BEGIN
  IF    x.state = Operand   THEN X64.LoadOperand(x.o)
  ELSIF x.state = Condition THEN
    reg := ReserveNextFree();
    ASSERT(x.o.disp >= 0);
    IF x.o.disp < 2 THEN
      X64.LoadImmediate(reg, x.o.disp);
    ELSE
      ASSERT(x.o.disp DIV 16 = 8);
      X64.LoadCondition(reg, x.o.disp)
    END;
    X64.ClearOperand(x.o);
    x.state := Operand;
    x.o.base := reg
  ELSE
    ASSERT(FALSE)
  END
END Load;

PROCEDURE LoadStructure(rb, rl: INTEGER; VAR x: Item);
(*  returns  rb - base address of array/record/string
             rl - length in bytes *)
BEGIN
  ASSERT(x.type.form IN {ORB.Array, ORB.Record, ORB.String});
  ASSERT(~x.o.direct);
  IF (x.type.form = ORB.Array) & (x.type.len < 0) THEN
    (* Open array parameter *)
    ASSERT(x.o.parptr);
    X64.LoadMem(rb, FALSE, 8, RSP, -1, 0, x.o.base);
    X64.LoadMem(rl, FALSE, 8, RSP, -1, 0, x.o.base + 8);
    ASSERT(x.type.base.size >= 0);
    IF x.type.base.size > 1 THEN
      ASSERT(x.type.base.size < 80000000H);
      (* TODO - optimise powers of 2 with a shift *)
      IF x.type.base.size >= 80H THEN
        X64.EmitOp(69H, rl, 8, x.o);  X64.EmitBytes(4, x.type.base.size)
      ELSE
        X64.EmitOp(6BH, rl, 8, x.o);  X64.Emit(x.type.base.size)
      END;
    END
  ELSE
    (* Record, or array with length known at compile time, or string *)
    X64.LoadAdrToReg(rb, x.o);
    IF x.type.form = ORB.String THEN
      X64.LoadImmediate(rl, x.length)
    ELSE
      X64.LoadImmediate(rl, x.type.size)  (* array length in bytes *)
    END
  END;
  X64.FreeOperand(x.o);
  x.o.parptr := FALSE;  x.o.direct := FALSE;
  x.o.base   := rb;     x.o.index  := rl;
  x.o.scale  := 0;      x.o.disp   := 0;
  X64.ReserveReg(rb);   X64.ReserveReg(rl)
END LoadStructure;


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

    ASSERT(op # X64.Cmp);

    IF    op = X64.Plus  THEN INC(dest.o.disp, src.o.disp)
    ELSIF op = X64.Minus THEN DEC(dest.o.disp, src.o.disp)
    ELSIF op = X64.And   THEN dest.o.disp := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET,dest.o.disp) * SYSTEM.VAL(SET,src.o.disp));
    ELSIF op = X64.Or    THEN dest.o.disp := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET,dest.o.disp) + SYSTEM.VAL(SET,src.o.disp));
    ELSIF op = X64.Xor   THEN dest.o.disp := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET,dest.o.disp) / SYSTEM.VAL(SET,src.o.disp));
    END

  ELSIF ~IsDyadicNoOp(op, src) THEN
    (* TODO - avoid reg load when src is immediate such as: cmp [rsp+10],0 *)

    ASSERT(dest.state = Operand);

    X64.LoadOperand(dest.o);
    (*w.s("  dest "); witem(dest); wl;*)

    ASSERT(dest.o.direct);
    ASSERT(src.state = Operand);

    X64.AluOp(op, dest.o.base, src.o);
    X64.FreeOperand(src.o);
  END
END Dyadic;


(* ---- Item construction ---- *)

PROCEDURE ZeroFill(VAR obj: ARRAY OF BYTE);  VAR i: INTEGER;
BEGIN FOR i := 0 TO LEN(obj)-1 DO obj[i] := 0 END END ZeroFill;

PROCEDURE MakeItem*(VAR x: Item; y: ORB.Object; curlev: INTEGER);
VAR base: INTEGER;
BEGIN
  IF TraceMakeItem IN Trace THEN
    startRightCol("MakeItem(y: "); wobject(y); w.s(", curlev: "); w.i(curlev); w.s(")")
  END;
  ASSERT(y.class IN {ORB.Const, ORB.Var, ORB.Par, ORB.SProc, ORB.Typ});

  ZeroFill(x);
  X64.ClearOperand(x.o);
  x.state    := y.class;
  x.type     := y.type;
  x.readonly := y.rdo;

  IF y.class IN {ORB.Const, ORB.Var, ORB.Par} THEN
    x.state := Operand;

    IF y.lev > 0 THEN  (* Stack relative *)
      base := RSP
    ELSE
      base := -1
    END;

    IF (y.class = ORB.Const) & (y.type.form = ORB.Proc) THEN
      ASSERT(y.type.size = WordSize);
      X64.MakeMemoryOperand(base, y.val, 8, FALSE, FALSE, x.o)
    ELSIF y.class = ORB.Const THEN
      X64.MakeConstOperand(y.val, x.o)
    ELSIF y.class = ORB.Var THEN
      X64.MakeMemoryOperand(base, y.val, y.type.size, IsSigned(y.type), FALSE, x.o)
    ELSE ASSERT(y.class = ORB.Par);
      ASSERT(y.lev > 0);  (* Must be on stack *)
      X64.MakeMemoryOperand(y.val, 0, y.type.size, IsSigned(y.type), TRUE, x.o)
    END
  ELSE
    X64.MakeConstOperand(y.val, x.o)
  END;

  IF TraceMakeItem IN Trace THEN
    w.s(" -> ");
    wstate(x.state); w.s("/"); wform(x.type.form);
    IF x.readonly THEN w.s(", readonly") END;
    IF x.state = Operand THEN w.s(" "); wOperand(x.o) END;
    IF x.type.form = ORB.String THEN w.s(", length "); w.i(x.length) END;
    wl
  END
END MakeItem;

PROCEDURE MakeConstItem*(VAR x: Item;  t: ORB.Type;  v: INTEGER);
BEGIN
  IF TraceMakeItem IN Trace THEN
    startRightCol("MakeConstItem, value: "); w.i(v); w.s(", type "); wtype(t); wl
  END;
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

(*
PROCEDURE FindString(str: ARRAY OF CHAR; len: INTEGER): INTEGER;
VAR i, j: INTEGER;
(* Returns offset of string in ORG.Str *)
BEGIN
  (* First see if ORS.str can be found amongst previous strings *)
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
*)

PROCEDURE MakeStringItem*(VAR x: Item; len: INTEGER); (* copies string from ORS-buffer *)
VAR i: INTEGER;
BEGIN
  IF TraceMakeItem IN Trace THEN
    startRightCol("MakeStringItem('");
    i := 0;
    WHILE (i < len) & (ORS.str[i] # 0X) DO w.c(ORS.str[i]); INC(i) END;
    w.sl("')");
  END;

  ZeroFill(x);
  x.type     := ORB.strType;
  x.readonly := TRUE;
  x.length   := len;
  x.state    := Operand;

  IF ~InCode THEN  (* Add string directly to code buffer *)
    X64.MakeMemoryOperand(-1, X64.PC, 8, FALSE, FALSE, x.o);
    X64.EmitStrings(ORS.str, len);
  ELSE  (* Add string to temp buffer *)
    w.s("** Add string item at Str["); w.i(Strx); w.sl("]. **");
    X64.MakeMemoryOperand(-1, -Strx, 8, FALSE, FALSE, x.o);
    IF Strx+len >= LEN(Str) THEN ORS.Mark("Too much string literal")
    ELSE
      FOR i := 0 TO len-1 DO Str[Strx+i] := ORS.str[i] END;
      INC(Strx, len);
      IF ORS.str[len-1] # 0X THEN Str[Strx] := 0X; INC(Strx) END
    END;
  END
END MakeStringItem;



(* ---- Item conversion ---- *)

PROCEDURE StrToChar*(VAR x: Item);
BEGIN startRightCol("StrToChar(x)  x"); witem(x); wl;
  ASSERT(x.type.form = ORB.String);
  ASSERT(x.state     = Operand);
  ASSERT(x.o.base    < 0);
  ASSERT(x.length    = 2);
  ASSERT(~x.o.parptr);
  ASSERT(~x.o.direct);
  ASSERT(x.readonly);

  x.type := ORB.charType;
  IF x.o.disp > 0 THEN (* string already emitted to code *)
    X64.MakeConstOperand(X64.PeekUnsigned(x.o.disp, 1), x.o);
  ELSE (* String still in string buffer *)
    X64.MakeConstOperand(ORD(Str[-x.o.disp]), x.o);
  END
END StrToChar;


(* ---- Assignment ---- *)


PROCEDURE Store*(VAR dest, src: Item); (* dest := src *)
BEGIN startRightCol("Store(dest: "); witem(dest); w.s(", src: "); witem(src); wl;
  IF IsImmediate(src) THEN
    X64.StoreImmediate(src.o.disp, dest.o);
  ELSE
    Load(src);
    X64.StoreReg(src.o.base, dest.o);
  END;
  Disassemble("Store");
  X64.FreeOperand(src.o);  X64.FreeOperand(dest.o)
END Store;

PROCEDURE SaveExchange(current, required: INTEGER; VAR save: INTEGER);
BEGIN
  IF current < 0 THEN
    IF required IN X64.Free THEN
      current := required
    ELSE
      current := X64.FirstFreeReg()
    END
  END;
  IF current = required THEN
    save := -1
  ELSE
    save := current;  X64.EmitRegRegOp(87H, current, 8, required);  (* xchg *)
  END
END SaveExchange;

PROCEDURE RestoreReg(reg, exchanged: INTEGER);
BEGIN
  IF exchanged >= 0 THEN
    X64.MoveReg(reg, exchanged)
  ELSE
    X64.ReleaseReg(reg)
  END
END RestoreReg;

PROCEDURE StoreStruct*(VAR x, y: Item);   (* x := y *)
VAR sadr, slen, dadr, dlen, xchgrsi, xchgrdi, xchgrcx: INTEGER;
BEGIN startRightCol("StoreStruct(x: "); witem(x);  w.s(", y: "); witem(y); w.sl(")");

  IF y.type.size # 0 THEN

    (* Destination structure *)
    dadr := FindAndReserveReg(RDI, -{RSI, RCX});
    dlen := FindAndReserveReg(RCX, -{RSI});
    LoadStructure(dadr, dlen, x);

    (* Source structure *)
    sadr := FindAndReserveReg(RSI, -{});
    slen := ReserveNextFree();
    LoadStructure(sadr, slen, y);

    (* For arrays need to check lengths *)
    IF (x.type.form = ORB.Array) & (y.type.form = ORB.Array) THEN
      IF (x.type.len >= 0) & (y.type.len >= 0) THEN
        (* Check lengths at compile time *)
        IF x.type.size # y.type.size THEN
          ORS.Mark("different array length/size, not implemented")
        END
      ELSE
        (* Generate code to fail on differing size *)
        X64.EmitRegRegOp(X64.Cmp+1, slen, 8, dlen);
        X64.Emit(74H);   X64.Emit(5);  (* je: if sizes match skip failure *)
        X64.Emit(0E8H);  EmitImportAddress(0, ArraySizeMismatchProc)
      END
    END;

    (* Temporary register exchanges to use correct regs for rep movsb *)
    SaveExchange(sadr, RSI, xchgrsi);
    SaveExchange(dadr, RDI, xchgrdi);
    SaveExchange(dlen, RCX, xchgrcx);

    X64.Emit(0F3H);  X64.Emit(0A4H);  (* rep movsb *)

    (* Restore temporarily exchanged registers *)
    RestoreReg(RSI, xchgrsi);
    RestoreReg(RDI, xchgrdi);
    RestoreReg(RCX, xchgrcx);

    X64.FreeOperand(x.o);  X64.FreeOperand(y.o);
    Disassemble("StoreStruct");
  END
END StoreStruct;

PROCEDURE CopyString*(VAR x, y: Item);  (* x := y *)
VAR len: INTEGER;
BEGIN startRightCol("CopyString(x: "); witem(x);  w.s(", y: "); witem(y); w.sl(")");
  len := x.type.len;
  IF len >= 0 THEN
    IF len < y.length THEN ORS.Mark("string too long") END
  ELSE
    (* TODO - runtime check string length *)
  END;

  (* TODO stop on zero byte like StringRelation does *)

  ASSERT(x.state = Operand);  X64.LoadAdrToReg(RDI, x.o);
  ASSERT(y.state = Operand);  X64.LoadAdrToReg(RSI, y.o);
  ASSERT(y.length > 0);       X64.LoadImmediate(RCX, y.length);
  X64.Emit(0F3H);  X64.Emit(0A4H);  (* rep movsb *)
  X64.FreeOperand(x.o);  X64.FreeOperand(y.o);
  Disassemble("CopyString")
END CopyString;


(* ---- selection - fields, indices & pointers ---- *)

PROCEDURE Field*(VAR x: Item; y: ORB.Object);   (* x := x.y *)
BEGIN startRightCol("Field(x: "); witem(x); w.s(", y: "); wobject(y); w.sl(")");
  ASSERT(x.state = Operand);
  ASSERT(~x.o.direct);
  INC(x.o.disp, y.val)
END Field;

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
    X64.LoadOperand(y.o);
    (* Use X64 SIB index and scale if possible *)
    IF (x.o.base >= 0) & (x.o.index < 0) & (elemsize IN {1, 2, 4, 8}) THEN
      x.o.index := y.o.base;  x.o.scale := 0;
      IF    elemsize = 2 THEN x.o.scale := 1
      ELSIF elemsize = 4 THEN x.o.scale := 2
      ELSIF elemsize = 8 THEN x.o.scale := 3
      END;
      X64.ClearOperand(y.o)
    ELSE
      (*X64.AluOpRegToReg(X64.Plus, x.o.base, y.o.base);*)
      X64.EmitRegRegOp(X64.Plus, x.o.base, 8, y.o.base);
      X64.FreeOperand(y.o)
    END
  END;
  x.o.size := elemsize;
  Disassemble("Index")
END Index;

PROCEDURE DeRef*(VAR x: Item);
BEGIN startRightCol("DeRef(x), x: "); witem(x); wl;
  ASSERT(x.type.form = ORB.Pointer);
  ASSERT(x.state = Operand);
  ASSERT(~x.o.direct);
  X64.LoadOperand(x.o);
  Disassemble("DeRef");
  x.o.direct := FALSE;
END DeRef;


(* ---- Paramter passing ---- *)

PROCEDURE ValueParam*(VAR x: Item; adr: INTEGER);
BEGIN startRightCol("ValueParam(adr: "); w.i(adr); w.s(", x: Item)  x: "); witem(x); w.sl(")");
  ASSERT(x.state = Operand);
  X64.Push(x.o);  X64.AdjustStack(1);
  Disassemble("ValueParam")
END ValueParam;


PROCEDURE VarParam*(VAR x: Item;  ftype: ORB.Type;  adr: INTEGER);
BEGIN startRightCol("VarParam(adr: "); w.i(adr); w.s(", x: "); witem(x);
  w.s(", ftype: "); wtype(ftype); w.s(") SPO "); w.i(X64.SPO); wl;
  ASSERT(x.state = Operand);
  IF x.o.parptr & ((ftype.form = ORB.Array) & (ftype.len < 0) OR (ftype.form = ORB.Record)) THEN
    (* Passing open array or record to same *)
    (* Array length / record descriptor *)
    X64.PushMem(RSP, -1, 0, x.o.base + X64.SPO + 8);  X64.AdjustStack(1);
    (* Array / record base address *)
    X64.PushMem(RSP, -1, 0, x.o.base + X64.SPO);      X64.AdjustStack(1);
  ELSE
    IF (ftype.form = ORB.Array) & (ftype.len < 0) THEN  (* Pass array length *)
      ASSERT(x.type.len >= 0);
      X64.PushImmediate(x.type.len);
      X64.AdjustStack(1)
    ELSIF ftype.form = ORB.Record THEN  (* Pass record descriptor *)
      X64.PushImmediate(TypeTagAdr(x.type));
      X64.AdjustStack(1)
    END;
    X64.LoadAdr(x.o);  (* Pass array or record address *)
    X64.PushReg(x.o.base);
    startRightCol("VarParam AdjustStack before call: SPO "); w.i(X64.SPO);  wl;
    X64.AdjustStack(1);
    startRightCol("VarParam AdjustStack after call: SPO "); w.i(X64.SPO);  wl
  END;
  X64.FreeOperand(x.o);
  Disassemble("VarParam");
  startRightCol("VarParam exit: SPO "); w.i(X64.SPO);  wl
END VarParam;

PROCEDURE IsOpenArray(t: ORB.Type): BOOLEAN;
BEGIN
  RETURN (t.form = ORB.Array) & (t.len < 0)

END IsOpenArray;


PROCEDURE OpenArrayParam*(VAR x: Item; adr: INTEGER);
(* x being passed to open array formal parameter *)
BEGIN startRightCol("OpenArrayParam(adr: "); w.i(adr); w.s(", x: "); witem(x);
  w.s(") SPO "); w.i(X64.SPO);  wl;
  ASSERT(x.state = Operand);
  ASSERT(x.type.form = ORB.Array);
  ASSERT(x.type.len >= 0);  (* ?? *)
  ASSERT(~x.o.direct);
  X64.Depar(x.o);
  X64.PushImmediate(x.type.len);  X64.AdjustStack(1);
  IF (x.o.base < 0) & (x.o.index < 0) THEN
    X64.PushImmediate(x.o.disp);  X64.AdjustStack(1)
  ELSE
    X64.LoadAdr(x.o);
    X64.PushReg(x.o.base);  X64.AdjustStack(1)
  END;
  X64.FreeOperand(x.o);
  Disassemble("OpenArrayParam");
  startRightCol("OpenArrayParam exit: SPO "); w.i(X64.SPO);  wl
END OpenArrayParam;

PROCEDURE StringParam*(VAR x: Item; adr: INTEGER);
BEGIN startRightCol("StringParam(adr: "); w.i(adr); w.s(", x: "); witem(x);  w.sl(")");
  ASSERT(x.state = Operand);  ASSERT(~x.o.direct & ~x.o.parptr);  ASSERT(x.o.base < 0);
  X64.PushImmediate(x.length);  X64.AdjustStack(1);
  X64.PushImmediate(x.o.disp);  X64.AdjustStack(1);
  Disassemble("StringParam");
END StringParam;


(* ---- Procedure / function calls ---- *)

PROCEDURE PrepCall*(VAR x: Item);
BEGIN startRightCol("PrepCall(x: "); witem(x); w.s("), SPO "); w.i(X64.SPO); wl;
  IF X64.Free # X64.AllFree THEN
    w.s("** Need to save partially evaluated expression in regs ");
    wset(X64.AllFree - X64.Free);
    w.sl(" **")
  END;
  IF X64.IsDataReg(x.o.base) THEN  (* Push proc address to stack *)
    X64.Push(x.o);
    X64.AdjustStack(1);
    x.o.direct := FALSE;
    x.o.base   := RSP;
    x.o.disp   := -8 * X64.SPO;
    Disassemble("proc address to be called")
  END;
END PrepCall;


PROCEDURE Parmsize(t: ORB.Type): INTEGER;
VAR obj: ORB.Object; s, i: INTEGER;
BEGIN ASSERT(t.form = ORB.Proc);
  obj := t.dsc;  s := 0;  i := 0;
  WHILE i < t.nofpar DO
    INC(s);
    IF (obj.type.form = ORB.Array) & (obj.type.len < 0) OR (obj.type.form = ORB.Record) THEN INC(s) END;
    obj := obj.next;  INC(i)
  END
RETURN s END Parmsize;


PROCEDURE Call*(VAR x: Item);
VAR reg: INTEGER;
BEGIN startRightCol("Call(x: "); witem(x); w.s("), SPO "); w.i(X64.SPO); wl;
  ASSERT(x.state = Operand);
  X64.Call(x.o);
  Disassemble("Call");

  X64.AdjustStack(-Parmsize(x.type));  (* matches stack adjustment by <ret n> *)

  IF (x.o.direct = FALSE) & (x.o.base = RSP) & (x.o.disp < 0) THEN
    (* Drop called proc address from stack *)
    X64.AluOpImmediateToReg(X64.Plus, RSP, 8);
    X64.AdjustStack(-1);
    Disassemble("Drop called proc address")
  END;

  IF x.type.base.form = ORB.NoTyp THEN (* procedure *)
    X64.ClearRegs
  ELSE                                 (* function *)
    X64.ClearRegs;  X64.ReserveReg(RAX);
    x.state := Operand;
    X64.FreeOperand(x.o);
    x.o.direct := TRUE;
    x.o.base   := 0 (* function result in rax *)
  END
END Call;

PROCEDURE Enter*(parmsize, locsize: INTEGER);
VAR i, count: INTEGER;
BEGIN
  startRightCol("Enter(parmsize $"); w.h(parmsize); w.s(", locsize $"); w.h(locsize); w.sl(")");
  InCode := TRUE;
  ASSERT(locsize MOD 8 = 0);
  count := locsize DIV 8;
  IF count > 0 THEN
    IF count < 3 THEN
      FOR i := 1 TO count DO X64.PushImmediate(0) END
    ELSIF count < 8 THEN
      X64.LoadImmediate(0, 0);        (*     xor    eax,eax   *)
      FOR i := 1 TO count DO X64.PushReg(0) END
    ELSE
      X64.LoadImmediate(1, count);    (*     mov    rcx,count *)
      X64.PushImmediate(0);           (* l1: push   0         *)
      X64.Emit(0E2H); X64.Emit(0FCH)  (*     loop   l1        *)
    END;
    Disassemble("Reserve and clear local variables")
  END;
  X64.ClearStack;  X64.ClearRegs
END Enter;

PROCEDURE WriteStrings();
VAR stroffset, prevoffset: INTEGER;
BEGIN
  InCode := FALSE;
  IF Strx > 0 THEN
    wl;
    (*ASSERT(StrFix # 0);*)
    WHILE StrFix # 0 DO
      stroffset  := X64.PeekUnsigned(StrFix, 4);  (* 16/prev, 16/str *)
      prevoffset := stroffset DIV 10000H;
      stroffset  := stroffset MOD 10000H;
      X64.Patch(StrFix, 4, X64.PC + stroffset - StrFix);
      IF prevoffset # 0 THEN DEC(StrFix, prevoffset) END;
    END;
    X64.EmitStrings(Str, Strx);
    Strx := 0;  StrFix :=0
  END;
END WriteStrings;

PROCEDURE Return*(form: INTEGER;  VAR x: Item;  parmsize, locsize: INTEGER);
BEGIN
  startRightCol("Return("); wform(form);
  w.s(", x: ");       witem(x);
  w.s(", locsize ");  w.i(locsize);
  w.s(", parmsize "); w.i(parmsize);
  w.s(", SPO ");      w.i(X64.SPO); wl;

  IF form # ORB.NoTyp THEN Load(x); Disassemble("Load function result") END;

  IF locsize > 0 THEN
    X64.AluOpImmediateToReg(X64.Plus, RSP, locsize);
    Disassemble("release local variable space")
  END;

  IF parmsize = 0 THEN
    X64.Emit(0C3H)
  ELSE
    X64.Emit(0C2H);  X64.EmitBytes(2, parmsize)
  END;
  Disassemble("Return");

  WriteStrings;
  X64.ClearStack;  X64.ClearRegs
END Return;


(* ---- Standard functions ---- *)

PROCEDURE Abs*(VAR x: Item);
VAR reg: INTEGER;
BEGIN startRightCol("Abs(x "); witem(x); w.sl(")");
  IF IsImmediate(x) THEN x.o.disp := ABS(x.o.disp)
  ELSIF x.type.form = ORB.Real THEN ASSERT(FALSE)
  ELSE
    Load(x);  reg := X64.FirstFreeReg();
    X64.MoveReg(reg, x.o.base);                 (* mov     reg,x.o.base *)
    X64.EmitRegRegOp(0F7H,  3, 8, x.o.base);    (* neg     x.o.base     *)
    X64.EmitRegRegOp(0F4CH, x.o.base, 8, reg)   (* cmovl   x.o.base,reg *)
  END;
  Disassemble("Abs")
END Abs;

PROCEDURE Odd*(VAR x: Item);
BEGIN startRightCol("Odd(x: "); witem(x); w.sl(")");
  Load(x);
  X64.AluOpImmediateToReg(X64.And, x.o.base, 1);
  X64.FreeOperand(x.o);
  SetCondition(x, CNZ);  (* odd if x & 1 nonzero *)
  Disassemble("Abs")
END Odd;

PROCEDURE Floor*(VAR x: Item);
BEGIN startRightCol("Floor(x: "); witem(x); w.sl(")");  ASSERT(FALSE);
  ASSERT(FALSE)
END Floor;

PROCEDURE Float*(VAR x: Item);
BEGIN startRightCol("Float(x: "); witem(x); w.sl(")");  ASSERT(FALSE);
  ASSERT(FALSE)
END Float;

PROCEDURE Ord*(VAR x: Item);
BEGIN startRightCol("Ord(x: "); witem(x); w.sl(")");
  IF ~IsImmediate(x) THEN Load(x) END;
  Disassemble("ORD")
END Ord;

PROCEDURE Len*(VAR x: Item);
BEGIN startRightCol("Len(x: "); witem(x); w.sl(")");
  ASSERT(x.state = Operand);
  IF x.o.parptr THEN (* open array *)
    x.o.direct := FALSE;
    x.o.parptr := FALSE;
    x.o.disp   := x.o.base + 8;  (* Length is at second qword of parameter *)
    x.o.base   := RSP
  ELSE
    ASSERT(x.length >= 0);
    x.o.direct := TRUE;
    x.o.base   := -1;
    x.o.disp   := x.length
  END
END Len;

PROCEDURE Shift*(fct: INTEGER; VAR x, y: Item);
VAR op: INTEGER;  name: ARRAY 4 OF CHAR;  restorecx: BOOLEAN;
BEGIN
  IF    fct = 0 THEN name := "LSL";  op := 4  (* shl *)
  ELSIF fct = 1 THEN name := "ASR";  op := 7  (* sar *)
  ELSIF fct = 2 THEN name := "ROR";  op := 1  (* ror *)
  ELSE ASSERT(FALSE)
  END;
  startRightCol("Shift("); w.s(name); w.s(", x: "); witem(x); w.s(", y: "); witem(y); wl;
  Load(x);
  IF IsImmediate(y) THEN
    X64.EmitRegRegOp(0C1H, op, 8, x.o.base);
    X64.Emit(y.o.disp);
  ELSE
    (* Get shift amount to rcx *)
    restorecx := FALSE;
    IF ~y.o.direct OR (y.o.base # RCX) THEN
      IF ~(RCX IN X64.Free) THEN
        X64.PushReg(RCX);  X64.ReleaseReg(RCX);  restorecx := TRUE
      END;
      X64.LoadOperandToReg(RCX, y.o)
    END;
    X64.EmitRegRegOp(0D3H, op, 8, x.o.base);
    IF restorecx THEN X64.PopReg(RCX); X64.ReserveReg(RCX) ELSE X64.ReleaseReg(RCX) END
  END;
  Disassemble(name)
END Shift;

PROCEDURE ADC*(VAR x, y: Item);
BEGIN startRightCol("ADC(x: "); witem(x); w.s(", y: "); witem(y); w.sl(")");
  ASSERT(FALSE)
END ADC;

PROCEDURE SBC*(VAR x, y: Item);
BEGIN startRightCol("SBC(x: "); witem(x); w.s(", y: "); witem(y); w.sl(")");
  ASSERT(FALSE)
END SBC;

PROCEDURE UML*(VAR x, y: Item);
BEGIN startRightCol("UML(x: "); witem(x); w.s(", y: "); witem(y); w.sl(")");
  ASSERT(FALSE)
END UML;

PROCEDURE Bit*(VAR x, y: Item);
BEGIN startRightCol("Bit(x: "); witem(x); w.s(", y: "); witem(y); w.sl(")");
  ASSERT(FALSE)
END Bit;

PROCEDURE REG*(VAR x: Item);
BEGIN startRightCol("REG(x "); witem(x); w.sl(")");
  ASSERT(FALSE)
END REG;

PROCEDURE Adr*(VAR x: Item);
BEGIN startRightCol("Adr(x "); witem(x); w.sl(")");
  IF (x.state # Operand) OR (x.o.direct) THEN
    ORS.Mark("not addressable")
  ELSIF (x.o.base < 0) & (x.o.index < 0) THEN
    x.o.direct := TRUE
  ELSE
    X64.LoadAdr(x.o)
  END;
  Disassemble("ADR")
END Adr;

PROCEDURE COND*(VAR x: Item);
BEGIN startRightCol("COND(x "); witem(x); w.sl(")");
  ASSERT(FALSE)
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
    Load(y);
    (*X64.AluOpRegToMem(op, y.o.base, x.o.size, x.o.base, x.o.index, x.o.scale, x.o.disp)*)
    X64.EmitRegMemOp(op+1, y.o.base, x.o.size, x.o.base, x.o.index, x.o.scale, x.o.disp)
  END;

  Disassemble("INC/DEC");
  X64.FreeOperand(x.o);  X64.FreeOperand(y.o)
END Increment;

PROCEDURE Assert*(VAR x: Item);
BEGIN startRightCol("Assert(x: ");  witem(x);  w.sl(")");
  IF x.state # Condition THEN LoadFlags(x) END;
  IF x.o.disp > 1 THEN
    ASSERT(x.o.disp DIV 16 = 8);
    X64.Emit(x.o.disp - 10H);  X64.Emit(5);  (* Skip over call instruction if condition met *)
  END;
  IF x.o.disp # 1 THEN
    X64.Emit(0E8H);  EmitImportAddress(0, AssertionFailureProc)  (* Jump to assert failure handler *)
  END;
  Disassemble("ASSERT")
END Assert;

PROCEDURE New*(VAR x: Item);
BEGIN startRightCol("New(x: ");  witem(x);  w.s("), base type "); wtype(x.type.base); wl;
  ASSERT(x.state = Operand);
  X64.PushAdr(x.o);  X64.FreeOperand(x.o);  X64.AdjustStack(1);
  X64.PushImmediate(x.type.base.size);   X64.AdjustStack(1);
  X64.Emit(0E8H);  EmitImportAddress(0, NewProc);
  X64.AdjustStack(-2);
  Disassemble("New")
END New;

PROCEDURE Led*(VAR x: Item);
BEGIN startRightCol("Led(x: ");  witem(x);  w.sl(")");
  ASSERT(FALSE)
END Led;

PROCEDURE LDPSR*(VAR x: Item);
BEGIN startRightCol("LDPSR(x: ");  witem(x);  w.sl(")");
  ASSERT(FALSE)
END LDPSR;

PROCEDURE Pack*(VAR x, y: Item);
BEGIN startRightCol("Pack(x: "); witem(x); w.s(", y: "); witem(y); w.sl(")");
  ASSERT(FALSE)
END Pack;

PROCEDURE Unpk*(VAR x, y: Item);
BEGIN startRightCol("Unpk(x: "); witem(x); w.s(", y: "); witem(y); w.sl(")");
  ASSERT(FALSE)
END Unpk;

PROCEDURE Get*(VAR x, y: Item);  (* y := [x] *)
BEGIN startRightCol("Get(x: "); witem(x); w.s(", y: "); witem(y); wl;
  Load(x);  x.type := y.type;  x.o.direct := FALSE;  Store(y, x);
  X64.FreeOperand(x.o);  X64.FreeOperand(y.o)
END Get;

PROCEDURE Put*(VAR x, y: Item);
BEGIN startRightCol("Put(x: "); witem(x); w.s(", y: "); witem(y); wl;
  Load(x);  x.type := y.type;  x.o.direct := FALSE;  Store(x, y);
  X64.FreeOperand(x.o);  X64.FreeOperand(y.o)
END Put;

PROCEDURE LDREG*(VAR x, y: Item);
BEGIN startRightCol("LDREG(x, y)"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl;
  ASSERT(FALSE)
END LDREG;

PROCEDURE Copy*(VAR x, y, z: Item);
BEGIN startRightCol("Copy(x, y, z)"); wl;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl;
  w.b(RightCol + 2); w.s("z: "); witem(z); wl;
  ASSERT(x.state = Operand); X64.LoadOperandToReg(RSI, x.o);
  ASSERT(y.state = Operand); X64.LoadOperandToReg(RDI, y.o);
  ASSERT(z.state = Operand); X64.LoadOperandToReg(RCX, z.o);

  (* TODO Check rcx >=0 & <= LEN(y) *)

  X64.Emit(0F3H);  X64.Emit(0A4H);  (* rep movsb *)
  X64.FreeOperand(x.o);  X64.FreeOperand(y.o);  X64.FreeOperand(z.o);
  Disassemble("Copy");
END Copy;


(* ---- Sets ---- *)

PROCEDURE Set*(VAR x, y: Item);   (* x := {x .. y} *)
BEGIN startRightCol("Set(x: "); witem(x); w.s(", y: "); witem(y); w.sl(")");
  IF IsImmediate(x) & IsImmediate(y) THEN
    IF x.o.disp <= y.o.disp THEN
      x.o.disp := LSL(2, y.o.disp) - LSL(1, x.o.disp) ELSE x.o.disp := 0
    END
  ELSE
    ASSERT(FALSE)
  END
END Set;

PROCEDURE Singleton*(VAR x: Item);  (* x := {x} *)
VAR reg: INTEGER;
BEGIN startRightCol("Singleton(x: "); witem(x); w.sl(")");
  IF IsImmediate(x) THEN
    x.o.disp := LSL(1, x.o.disp)
  ELSE
    Load(x);
    reg := ReserveNextFree();
    X64.LoadImmediate(reg, 0);
    (* bts reg, x *)
    X64.EmitPrefices(reg, 8, x.o.base, -1);
    X64.Emit(0FH); X64.Emit(0ABH);
    X64.Emit(0C0H  +  reg MOD 8 * 8  +  x.o.base MOD 8);
    Disassemble("Singleton");
    X64.FreeOperand(x.o);
    x.o.base := reg
  END
END Singleton;


PROCEDURE In*(VAR x, y: Item);  (* x := x IN y *)
BEGIN startRightCol("In(x: "); witem(x);  w.s(", y: "); witem(y); wl;
  ASSERT(x.state = Operand);  ASSERT(y.state = Operand);

  (* TODO - optimise immediate x cases where x<32 with AND 1<<x *)

  X64.Depar(x.o);  X64.Depar(y.o);
  IF IsImmediate(y) THEN X64.LoadOperand(y.o) END;
  IF IsImmediate(x) THEN
    X64.EmitOp(0FBAH, 4, 8, y.o);        (* bt x,imm *)
    X64.Emit(x.o.disp)
  ELSE
    Load(x);
    X64.EmitOp(0FA3H, x.o.base, 8, y.o)  (* bt x,y *)
  END;
  Disassemble("In");
  X64.FreeOperand(x.o);  X64.FreeOperand(y.o);
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
        Load(y);
        X64.EmitOp(0F7H, 2, 8, y.o);
        Disassemble("Invert for SET -")
      END
    END;
    IF    op = ORS.plus  THEN op := X64.Or
    ELSIF op = ORS.minus THEN op := X64.And
    ELSIF op = ORS.times THEN op := X64.And
    ELSIF op = ORS.rdiv  THEN op := X64.Xor
    ELSE w.s("** op = "); w.i(op); w.sl(" **"); ASSERT(FALSE)
    END;
    Dyadic(op, x, y)
  END;
  Disassemble("SetOp")
END SetOp;

PROCEDURE Include*(inorex: INTEGER; VAR x, y: Item);  (* INCL/EXCL(x, y) *)
VAR op: INTEGER;
BEGIN startRightCol("Include(");
  IF inorex = 0 THEN w.s("INCL, x: ") ELSE w.s("EXCL, x: ") END; witem(x);
  w.s(", y: "); witem(y); wl;
  ASSERT(x.state = Operand);  ASSERT(~x.o.direct);  ASSERT(y.state = Operand);
  X64.Depar(x.o);  X64.Depar(y.o);
  IF IsImmediate(y) THEN
    IF op = 0 THEN
      X64.EmitOp(0FBAH, 5, 8, x.o)  (* bts x,imm *)
    ELSE
      X64.EmitOp(0FBAH, 6, 8, x.o)  (* btr x,imm *)
    END;
    X64.Emit(y.o.disp)
  ELSE
    Load(y);
    IF op = 0 THEN
      X64.EmitOp(0FABH, y.o.base, 8, x.o)  (* bts x,y *)
    ELSE
      X64.EmitOp(0FB3H, y.o.base, 8, x.o)  (* btr x,y *)
    END
  END;
  X64.FreeOperand(x.o);  X64.FreeOperand(y.o);
  IF op = 0 THEN Disassemble("INCL") ELSE Disassemble("EXCL") END;
END Include;


(* ---- Boolean operators ---- *)

PROCEDURE FixLinkWith(L, target: INTEGER);
(*  Patch chain of 4 byte addresses starting at L.                         *)
(*  Chain continues at (L)                                                 *)
(*  Patch each address with offset (from byte following address) to target *)
VAR l, offset: INTEGER;
BEGIN
  startRightCol("FixLinkWith: chain at $"); w.h(L);
  w.s(", target $"); w.h(target); w.sl(".");
  WHILE L # 0 DO
    offset := target - (L+4);

    w.b(RightCol); w.s("-> Patch at $"); w.h(L);
    w.s(" currently $"); w.h(X64.PeekUnsigned(L, 4));
    w.s(", with offset $"); w.h(offset);
    w.s(", target: $"); w.h(target); w.sl(".");

    l := X64.PeekUnsigned(L, 4);
    X64.Patch(L, 4, offset);
    L := l
  END
END FixLinkWith;

PROCEDURE FixLink*(L: INTEGER);
BEGIN FixLinkWith(L, X64.PC) END FixLink;


PROCEDURE merged(L0, L1: INTEGER): INTEGER;  (* Insert L0 at the start of L1 *)
VAR l2, l3: INTEGER;
BEGIN startRightCol("Merged(L0: $"); w.h(L0); w.s(", L1: $"); w.h(L1); w.sl(")");
  IF L0 # 0 THEN
    l2 := L0;
    REPEAT  l3 := l2;  l2 := X64.PeekUnsigned(l2, 4) UNTIL l2 = 0;
    w.b(RightCol); w.s("-> Patch at $"); w.h(l3);
    w.s(" currently $"); w.h(X64.PeekUnsigned(l3, 4));
    w.s(", with offset $"); w.h(L1); w.sl(".");
    X64.Patch(l3, 4, L1);
    L1 := L0
  END;
  RETURN L1
END merged;


PROCEDURE Not*(VAR x: Item);   (* x := ~x *)
VAR t: INTEGER;
BEGIN startRightCol("Not(x: "); witem(x); w.sl(")");
  IF x.state # Condition THEN LoadFlags(x) END;
  x.o.disp := InvertedCondition(x.o.disp);
  t := x.o.base;  x.o.base := x.o.index;  x.o.index := t
END Not;

PROCEDURE Or1*(VAR x: Item);   (* x := x OR *)
BEGIN startRightCol("Or1(x: "); witem(x); w.sl(")");
  IF x.state # Condition THEN LoadFlags(x) END;
  IF x.o.disp > 0 THEN
    IF x.o.disp = 1 THEN  (* condition TRUE *)
      X64.Emit(0E9H);
    ELSE
      X64.Emit(0FH); X64.Emit(x.o.disp);
    END;
    X64.EmitBytes(4, x.o.index);
    x.o.index := X64.PC - 4;
    Disassemble("Or1")
  END;
  FixLink(x.o.base);  x.o.base := 0
END Or1;

PROCEDURE Or2*(VAR x, y: Item);   (* x := x OR y *)
BEGIN startRightCol("Or2(x: "); witem(x); w.sl(")");
  IF y.state # Condition THEN LoadFlags(y) END;
  x.o.base  := y.o.base;
  x.o.index := merged(y.o.index, x.o.index);
  x.o.disp  := y.o.disp
END Or2;


PROCEDURE And1*(VAR x: Item);   (* x := x & *)
BEGIN startRightCol("And1(x: "); witem(x); w.sl(")");
  IF x.state # Condition THEN LoadFlags(x) END;
  IF x.o.disp # 1 THEN
    IF x.o.disp = 0 THEN
      X64.Emit(0E9H)
    ELSE
      X64.Emit(0FH); X64.Emit(InvertedCondition(x.o.disp));
    END;
    X64.EmitBytes(4, x.o.base);
    x.o.base := X64.PC - 4;
    Disassemble("And1")
  END;
  FixLink(x.o.index); x.o.index := 0
END And1;

PROCEDURE And2*(VAR x, y: Item);   (* x := x & y *)
BEGIN startRightCol("And2(x: "); witem(x); w.sl(")");
  IF y.state # Condition THEN LoadFlags(y) END;
  x.o.base  := merged(y.o.base, x.o.base);
  x.o.index := y.o.index;
  x.o.disp  := y.o.disp
END And2;


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
  Disassemble("Compare")
END Compare;

PROCEDURE IntRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
VAR immrel: BOOLEAN;
BEGIN startRightCol("IntRelation("); wsym(op); w.s(", x: "); witem(x);  w.s(", y: "); witem(y); wl;
  ASSERT(x.state = Operand);
  IF y.state # Operand THEN w.s("** y.state # Operand, is "); w.i(y.state); w.sl(" **") END;
  ASSERT(y.state = Operand);
  IF IsImmediate(x) & IsImmediate(y) THEN
    IF    op = ORS.lss THEN immrel := x.o.disp <  y.o.disp
    ELSIF op = ORS.leq THEN immrel := x.o.disp <= y.o.disp
    ELSIF op = ORS.eql THEN immrel := x.o.disp =  y.o.disp
    ELSIF op = ORS.geq THEN immrel := x.o.disp >= y.o.disp
    ELSIF op = ORS.gtr THEN immrel := x.o.disp >  y.o.disp
    ELSIF op = ORS.neq THEN immrel := x.o.disp #  y.o.disp
    ELSE ASSERT(FALSE); immrel := FALSE
    END;
    IF immrel THEN SetCondition(x, 1) ELSE SetCondition(x, 0) END
  ELSE
    Compare(x, y);
    X64.FreeOperand(x.o);  X64.FreeOperand(y.o);  (* Result is in flags *)
    SetCondition(x, OpToIntCondition(op))
  END
END IntRelation;

PROCEDURE RealRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
BEGIN
  startRightCol("RealRelation("); wsym(op); w.sl(", x, y)");
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl;
  ASSERT(FALSE)
END RealRelation;



PROCEDURE StringRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
VAR xadr, xlen, yadr, ylen, saveax, xchgcx, xchgsi, lbl: INTEGER;
BEGIN
startRightCol("StringRelation("); wsym(op);
  w.sl(", ");
  w.b(RightCol); w.s("x: "); witem(x); w.sl(",");
  w.b(RightCol); w.s("y: "); witem(y); w.sl(")");

  xadr := FindAndReserveReg(RSI, -{RAX, RCX});
  xlen := FindAndReserveReg(RCX, -{RAX});
  LoadStructure(xadr, xlen, x);
  Disassemble("Load string x");

  yadr := FindAndReserveReg(RDI, -{RAX});
  ylen := FindAndReserveReg(RDX, -{RAX});
  LoadStructure(yadr, ylen, y);
  Disassemble("Load string y");

  SaveExchange(xadr, RSI, xchgsi);
  SaveExchange(xlen, RCX, xchgcx);
  SaveExchange(-1,   RAX, saveax);
  Disassemble("Adjust registers for loop lodsb");

  X64.EmitRegRegOp(3BH,   RCX, 8, y.o.index);  (* cmp   xlen,ylen *)
  X64.EmitRegRegOp(0F47H, RCX, 8, y.o.index);  (* cmova xlen,ylen *)
  Disassemble("Determine shorter string length");

  X64.EmitRegRegOp(X64.Minus+3, y.o.base, 8, RSI);         (* sub    y.o.base,rsi      *)
  X64.AluOpImmediateToReg(X64.Minus, y.o.base, 1);         (* dec    y.o.base          *)
  Disassemble("String offset y[n] from x[n+1]");
  lbl := X64.PC;                                           (* lbl:                     *)
  X64.Emit(0ACH);                                          (* lodsb                    *)
  X64.EmitRegMemOp(X64.Cmp+2, 0, 1, RSI, y.o.base, 0, 0);  (* cmp    al,[rsi+y.o.base] *)
  Disassemble("Compare one byte");

  X64.Emit(75H);  X64.Emit(0BH);                           (* jne    end               *)
  Disassemble("If string mismatch");

  X64.Emit(8); X64.Emit(0C0H);                             (* or     al,al             *)
  X64.Emit(0E0H); X64.Emit(lbl-(X64.PC+1));                (* loopne lbl               *)
  Disassemble("If not out of bytes, loop back");

  X64.Emit(74H);  X64.Emit(5);                             (* je     end               *)
  Disassemble("If strings match");

  X64.Emit(0E8H);  EmitImportAddress(0, UnterminatedStringProc);  (* error handler *)
  Disassemble("Unterminated string error trap");

  RestoreReg(RSI, xchgsi);
  RestoreReg(RCX, xchgcx);
  RestoreReg(RAX, saveax);
  Disassemble("Restore saved registers");

  X64.FreeOperand(x.o);  X64.FreeOperand(y.o);

  SetCondition(x, OpToIntCondition(op))
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
  Disassemble("AddOp")
END AddOp;


PROCEDURE log2(m: INTEGER; VAR exponent: INTEGER): INTEGER;
BEGIN exponent := 0;
  WHILE ~ODD(m) DO m := m DIV 2; INC(exponent) END;
  RETURN m
END log2;

PROCEDURE MulImmediate(VAR x: Item; y: INTEGER);
VAR reg: INTEGER;
BEGIN
  ASSERT(~x.o.direct);
  IF (y < -80000000H) OR (y >= 80000000H) THEN
    Load(x);
    reg := X64.FirstFreeReg();
    X64.LoadImmediate(reg, y);
    X64.EmitRegRegOp(0FAFH, x.o.base, 8, reg);
  ELSE
    X64.Depar(x.o);
    IF X64.IsDataReg(x.o.base) THEN reg := x.o.base ELSE reg := X64.FirstFreeReg() END;
    IF (y < -80H) OR (y >= 80H) THEN
      X64.EmitOp(69H, reg, 8, x.o);  X64.EmitBytes(4, y)
    ELSE
      X64.EmitOp(6BH, reg, 8, x.o);  X64.Emit(y)
    END;
    X64.FreeOperand(x.o);
    x.o.base := reg;  X64.ReserveReg(reg)
  END
END MulImmediate;

PROCEDURE MulOp*(VAR x, y: Item);   (* x := x * y *)
VAR exponent: INTEGER;  ti: Item;
BEGIN startRightCol("MulOp(x: "); witem(x); w.s(", y: "); witem(y); w.sl(")");
  ASSERT(x.state = Operand);  ASSERT(y.state = Operand);
  IF IsImmediate(x) & IsImmediate(y) THEN
    x.o.disp := x.o.disp * y.o.disp
  ELSE
    IF IsImmediate(x) THEN ti := x;  x:= y;  y := ti END;
    IF IsImmediate(y) THEN
      IF (y.o.disp >= 2) & (log2(y.o.disp, exponent) = 1) THEN
        Load(x);
        X64.EmitRegRegOp(0C1H, 4, 8, x.o.base);  X64.Emit(exponent);  (* shl *)
      ELSE
        MulImmediate(x, y.o.disp)
      END
    ELSE
      Load(x); X64.Depar(y.o);
      X64.EmitOp(0FAFH, x.o.base, 8, y.o)
    END;
    X64.FreeOperand(y.o);
    Disassemble("MulOp")
  END
END MulOp;

PROCEDURE DivOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR
  exponent: INTEGER;
  name:     ARRAY 4 OF CHAR;
  saveRAX:  BOOLEAN;
  saveRDX:  BOOLEAN;
  reg:      INTEGER;
BEGIN
  IF op = ORS.div THEN name := "DIV" ELSE name := "MOD" END;
  startRightCol("DivOp("); w.s(name); w.s(", x: "); witem(x); w.s(", y: "); witem(y); w.sl(")");
  ASSERT(x.state = Operand);  ASSERT(y.state = Operand);
  IF IsImmediate(x) & IsImmediate(y) THEN
    IF op = ORS.div THEN
      x.o.disp := x.o.disp DIV y.o.disp
    ELSE
      x.o.disp := x.o.disp MOD y.o.disp
    END
  ELSE
    IF IsImmediate(y) & (y.o.disp >= 2) & (log2(y.o.disp, exponent) = 1) THEN
      Load(x);
      IF op = ORS.div THEN
        X64.EmitRegRegOp(0C1H, 7, 8, x.o.base);  X64.Emit(exponent);  (* sar *)
      ELSE
        (* MOD power of 2 *)
        X64.AluOpImmediateToReg(X64.And, x.o.base, y.o.disp-1)
      END;
      Disassemble("DivOp DIV/MOD by power of 2")

    ELSE
      (* DIV or MOD non-power of 2 *)


      saveRAX := FALSE;  saveRDX := FALSE;

      IF x.o.direct & (x.o.base = RAX) THEN
        (* numerator is already in rax *)
      ELSE
        IF ~(RAX IN X64.Free) THEN
          (* Save rax temporarily *)
          saveRAX := TRUE;
          X64.PushReg(RAX);  X64.AdjustStack(1);  X64.ReleaseReg(RAX);
          Disassemble("DivOp save rax")
        END;
        X64.LoadOperandToReg(RAX, x.o);
        Disassemble("DivOp load numerator to rax")
      END;

      IF ~(RDX IN X64.Free) THEN
        X64.Depar(y.o);
        saveRDX := TRUE;
        X64.PushReg(RDX);  X64.AdjustStack(1);  X64.ReleaseReg(RDX);
        IF y.o.direct & (y.o.base = RDX) THEN
          y.o.direct := FALSE;
          y.o.base   := RSP;
          y.o.disp   := -8 * X64.SPO;
        END
      END;
      X64.LoadImmediate(RDX, 0);  X64.ReserveReg(RDX);
      Disassemble("DivOp prepare rdx");

      IF IsImmediate(y) THEN
        reg := X64.FirstFreeReg();
        X64.LoadImmediate(reg, y.o.disp);
        X64.EmitRegRegOp(0F7H, 7, 8, reg)
      ELSE
        X64.EmitOp(0F7H, 7, 8, y.o);  (* IDIV *)
      END;
      Disassemble("DivOp divide");

      IF op = ORS.div THEN  (* DIV Result in RAX *)

        IF saveRDX THEN
          X64.PopReg(RDX);  X64.AdjustStack(-1);  X64.ReserveReg(RDX);
          Disassemble("Divop restore rdx")
        END;

        IF saveRAX THEN
          reg := X64.FirstFreeReg();  X64.MoveReg(reg, RAX);
          Disassemble("Move result out of rax");
          X64.PopReg(RAX);  X64.AdjustStack(-1);  X64.ReserveReg(RAX);
          Disassemble("Divop restore rax")
        ELSE
          reg := X64.RAX
        END;

      ELSE (* MOD result in RDX *)

        IF saveRDX THEN
          reg := X64.FirstFreeReg();  X64.MoveReg(reg, RDX);
          Disassemble("Move result out of rdx");
          X64.PopReg(RDX); X64.AdjustStack(-1);  X64.ReserveReg(RDX);
          Disassemble("Divop restore rdx")
        ELSE
          reg := RDX
        END;

        IF saveRAX THEN
          X64.PopReg(RAX);  X64.AdjustStack(-1);  X64.ReserveReg(RAX);
          Disassemble("Divop restore rax")
        END

      END;

      IF ~saveRAX THEN X64.ReleaseReg(RAX) END;
      IF ~saveRDX THEN X64.ReleaseReg(RDX) END;

      X64.FreeOperand(x.o);
      X64.FreeOperand(y.o);
      x.o.direct := TRUE;
      x.o.base   := reg;
      X64.ReserveReg(reg)
    END;
    Disassemble(name)
  END;
END DivOp;

PROCEDURE RealOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
BEGIN startRightCol("RealOp("); wsym(op); w.sl("; x, y) x := x op y");
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("y: "); witem(y); wl;
  ASSERT(FALSE)
END RealOp;

PROCEDURE Neg*(VAR x: Item);   (* x := -x *)
VAR size, op: INTEGER;
BEGIN startRightCol("Neg(x: "); witem(x); wl;
  IF IsImmediate(x) THEN
    x.o.disp := -x.o.disp
  ELSE
    Load(x);
    X64.EmitRegRegOp(0F7H, 3, 8, x.o.base);
    Disassemble("Neg")
  END
END Neg;


(* ---- Control flow ---- *)

(* Use of Item.o fields for conditions:
*
*  o.disp  - the condition code (uses symbol encodings ORS.eql .. ORS.geq)
*  o.base  - address of prev instr if any needing patching with curr target
*  o.index - address of any forward jump list targeting this instruction
*)

PROCEDURE CFJump*(VAR x: Item);
VAR cond: INTEGER;
BEGIN startRightCol("CFJump(x: "); witem(x);  w.sl(")");
  IF x.state # Condition THEN LoadFlags(x) END;
  cond := InvertedCondition(x.o.disp);
  ASSERT(cond >= 0);
  IF cond > 0 THEN
    IF cond = 1 THEN  (* unconditional forward jump *)
      X64.Emit(0E9H)
    ELSE
      ASSERT(cond DIV 16 = 8);
      X64.Emit(0FH);  X64.Emit(cond)
    END;
    X64.EmitBytes(4, x.o.base);  x.o.base := X64.PC - 4;
    FixLink(x.o.index);  (* Fix any links that target following this instruction *)
    Disassemble("CFJump")
  END
END CFJump;

PROCEDURE FJump*(VAR L: INTEGER);
BEGIN startRightCol("FJump(L "); w.i(L); w.sl(")");
  X64.Emit(0E9H);  X64.EmitBytes(4, L);  L := X64.PC-4;
  Disassemble("FJump")
END FJump;

PROCEDURE BJump*(L: INTEGER);
BEGIN startRightCol("BJump(L "); w.i(L); w.sl(")");
  X64.Emit(0E9H);  X64.EmitBytes(4, L - (X64.PC + 4));
  Disassemble("BJump")
END BJump;

PROCEDURE CBJump*(VAR x: Item; L: INTEGER);
VAR cond, disp: INTEGER;
BEGIN startRightCol("CBJump(x: "); witem(x); w.sl(", L: "); w.i(L); w.sl(")");
  IF x.state # Condition THEN LoadFlags(x) END;
  cond := InvertedCondition(x.o.disp);  ASSERT(cond DIV 16 = 8);
  ASSERT(L < X64.PC);
  IF (X64.PC + 2 - L) >= -80H THEN
    X64.Emit(70H + cond MOD 16);    X64.Emit(X64.PC + 1 - L)
  ELSE
    X64.Emit(0FH);  X64.Emit(cond); X64.EmitBytes(4, X64.PC + 4 - L)
  END;
  Disassemble("CBJump");
  FixLink(x.o.index);
  FixLinkWith(x.o.base, L)
END CBJump;

PROCEDURE FixOne*(at: INTEGER);
BEGIN startRightCol("FixOne(at "); w.i(at); w.sl(")");
  ASSERT(FALSE)
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
  Load(ctl);
  Disassemble("For0");
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
  Load(ctl);
  X64.StoreReg(ctl.o.base, var.o);
  X64.FreeOperand(var.o);  X64.FreeOperand(ctl.o);
  Disassemble("For1");
END For1;

PROCEDURE For2*(VAR var, ctl, inc: Item);
BEGIN startRightCol("For2(var: "); witem(var); w.s(", ctl: "); witem(ctl); w.sl(")");
  Dyadic(X64.Plus, ctl, inc);
  X64.FreeOperand(ctl.o);
  Disassemble("For2");
END For2;


(* ---- Type descriptor construction and testing ---- *)

PROCEDURE Q(T: ORB.Type);
BEGIN (*one entry of type descriptor extension table*)
  IF T.base # NIL THEN
    Q(T.base);
    startRightCol("Q(T: "); wtype(T); w.sl(")");
    X64.DefInt((T.mno*1000000H + T.len) * 1000000H + X64.PC - fixorgT,
                "type extension: 16/mno, 24/adr, 24/fixoffset");
    fixorgT := X64.PC - 8;
  END
END Q;

PROCEDURE FindPtrFlds(typ: ORB.Type; off: INTEGER);
VAR
  fld:  ORB.Object;
  i, s: INTEGER;
BEGIN
  IF (typ.form = ORB.Pointer) OR (typ.form = ORB.NilTyp) THEN
    X64.DefInt(off, "pointer/niltype field offset")
  ELSIF typ.form = ORB.Record THEN
    fld := typ.dsc;
    WHILE fld # NIL DO
      FindPtrFlds(fld.type, fld.val + off);
      fld := fld.next
    END
  ELSIF typ.form = ORB.Array THEN
    s := typ.base.size;
    FOR i := 0 TO typ.len-1 DO FindPtrFlds(typ.base, i*s + off) END
  END
END FindPtrFlds;


PROCEDURE ShowFields(T: ORB.Type);
VAR f: ORB.Object;
BEGIN
  f := T.dsc;
  WHILE f # NIL DO
    startRightCol("  '"); w.s(f.name); w.s("': "); wtype(f.type); wl;
    f := f.next
  END
END ShowFields;

PROCEDURE BuildTD*(T: ORB.Type; VAR adr: INTEGER);
VAR k, s: INTEGER;
BEGIN startRightCol("BuildTD(T "); wtype(T); w.s(", adr "); w.i(adr); w.sl(")");
  ASSERT(adr MOD 8 = 0);
  ShowFields(T);

  s := T.size; (* Convert size for heap allocation *)
  IF    s <= 24  THEN s := 32
  ELSIF s <= 56  THEN s := 64
  ELSIF s <= 120 THEN s := 128
  ELSE                s := (s+263) DIV 256 * 256
  END;

  T.len := X64.PC;  (* T.len overriden to be adddress of descriptor for record types *)

  X64.DefInt(s, "Type size including heap overhead");
  k          := T.nofpar;      (* extension level! *)
  IF k > 3 THEN
    ORS.Mark("ext level too large")
  ELSE
    Q(T);
    WHILE k < 3 DO
      X64.DefInt(-1, "unused extension level");
      INC(k)
    END
  END;
  FindPtrFlds(T, 0);
  X64.DefInt(-1, "end of type");


  (*
  T.len      := adr;
  Tdesc[Tdx] := s;  INC(Tdx);
  k          := T.nofpar;      (* extension level! *)
  IF k > 3 THEN
    ORS.Mark("ext level too large")
  ELSE
    Q(T);
    WHILE k < 3 DO Tdesc[Tdx] := -1; INC(Tdx); INC(k) END
  END;
  FindPtrFlds(T, 0);
  Tdesc[Tdx] := -1;  INC(Tdx);
  IF Tdx >= MaxTD THEN
    ORS.Mark("too many record types"); Tdx := 0
  END
  *)
END BuildTD;

PROCEDURE TypeTest*(VAR x: Item; T: ORB.Type; varpar, isguard: BOOLEAN);
BEGIN startRightCol("TypeTest(x, T, ");
  IF varpar  THEN w.s("varpar, ")  ELSE w.s("~varpar, ")  END;
  IF isguard THEN w.sl("isguard)") ELSE w.sl("~isguard)") END;
  w.b(RightCol + 2); w.s("x: "); witem(x); wl;
  w.b(RightCol + 2); w.s("T: "); wtype(T); wl;
  (*ASSERT(FALSE)*)
END TypeTest;


(* ---- ---- *)

PROCEDURE Open*(sourcefn: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN
  Trace := {TraceMakeItem};
  FOR i := 0 TO LEN(PrevImport) - 1 DO PrevImport[i] := 0 END;
  w.s("ORG Open('"); w.s(sourcefn); w.sl("')");
  Sourcefile := Files.Old(sourcefn);  Files.Set(Source, Sourcefile, 0);
  SourceBol := 0;  SourceLine := 1;
  GetChar;
  Strx := 0;  (*Tdx := 0;*)
  X64.ClearStack;  X64.ClearRegs;  InCode := FALSE;  StrFix := 0;
  X64.Init;
END Open;

PROCEDURE SetDataSize*(dc: INTEGER);
BEGIN w.s("SetDataSize(dc "); w.i(dc); w.sl(")");
  Varsize := dc
END SetDataSize;

PROCEDURE Header*;
BEGIN w.sl("Header");
  (* Module init code entry point *)
  InCode := FALSE;
END Header;

PROCEDURE Close*(VAR modid: ORS.Ident; key, nofent: INTEGER);
BEGIN
  w.sl("Close:");
  w.s("  modid    ");  w.sl(modid);
  w.s("  key      ");  w.i(key);     w.l;
  w.s("  nofent   ");  w.i(nofent);  w.l;
  w.s("  str size ");  w.i(Strx);    w.l;
  (* Write compiled module *)
  WriteStrings;
END Close;


(* ---- Initialisation ---- *)

BEGIN
END ORG.
