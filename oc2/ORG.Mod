MODULE ORG;  (* DCWB June 2023; Code generator for X64 *)

(* Originally: N.Wirth, 16.4.2016 / 4.4.2017 / 17.9.2018  Oberon compiler; code generator for RISC*)

IMPORT SYSTEM, Files, ORS, ORB, w := Writer, K := Kernel, DisassemblerX64;

CONST
  WordSize* = 8;
  MaxStrx   = 8000;  (* String buffer size *)
  MaxPC     = 10000H;

  RSP       = 4;     (* X64 stack pointer *)

  (* Unloaded item states - from ORB.Object.class  *)
  Const = ORB.Const;  (* Immediate value *)
  Var   = ORB.Var;    (* Item on stack, in module memory, or imported memory *)
  Par   = ORB.Par;    (* Reference to item on stack, in module memory, or imported memory *)
  (* ALU generated item states*)
  Address   = 10;     (* Register containing address *)
  Value     = 11;     (* Register containing value *)
  Condition = 12;     (* Condition codes set by last ALU operation *)

  (* Item levels *)
  Immediate* = 0;
  Stack*     = 1;
  Module*    = 2;
  (* Import levels are 3.. *)


  (* Dyadic ALU operations, values correspond to x86 instruction set *)
  Plus  = 00H;
  Or    = 08H;
  And   = 20H;
  Minus = 28H;
  Xor   = 30H;
  Cmp   = 38H;


TYPE
  Item* = RECORD
    state*:    INTEGER;  (* Initially from object.class *)
    level*:    INTEGER;  (* 0 constant, 1 stack, 2 module, 3 import *)
    type*:     ORB.Type;
    value*:    INTEGER;
    readonly*: BOOLEAN;
    offset:    INTEGER;  (* e.g. for field or literal index *)
    length*:   INTEGER;  (* Only if type = strType *)
  END;


VAR
  Str:  ARRAY MaxStrx OF CHAR;
  Strx: INTEGER;

  Program: ARRAY MaxPC OF BYTE;
  PC*:     INTEGER;
  Oldpc:   INTEGER;

  sourcefile: Files.File;
  source:     Files.Rider;
  sourceBol:  INTEGER;      (* pos at beginiing of line *)
  sourceLine: INTEGER;      (* Line number *)
  sourcechar: CHAR;

  RH:    INTEGER;           (* Register high = top of register stack = next available register *)


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

PROCEDURE DisplaySourceToPos(pos: INTEGER);
VAR ch: CHAR;
BEGIN
  (* Skip trailing blanks to start of next non-blank line *)
  WHILE sourcechar = " " DO
    GetChar;
    WHILE (sourcechar = 0DX) OR (sourcechar = 0AX) DO GetChar END;
  END;

  (* Copy source text *)
  WHILE ~source.eof & (Pos() < pos) DO
    w.in(sourceLine, 4); w.s(": "); w.b(Pos() - sourceBol);

    WHILE ~source.eof & (Pos() < pos) & (sourcechar # 0DX) & (sourcechar # 0AX) DO
      w.c(sourcechar);  GetChar
    END;

    IF (sourcechar = 0DX) OR (sourcechar = 0AX) THEN
      GetChar;
      WHILE sourcechar = " " DO
        GetChar;
        WHILE (sourcechar = 0DX) OR (sourcechar = 0AX) DO GetChar END;
      END
    END;
    w.l
  END
END DisplaySourceToPos;

PROCEDURE start(str: ARRAY OF CHAR);
BEGIN DisplaySourceToPos(ORS.Pos());  w.s(str)
END start;

PROCEDURE wl; BEGIN w.l END wl;
PROCEDURE wc; BEGIN w.c(",") END wc;

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

PROCEDURE wstate(state: INTEGER);
BEGIN
  IF    state = Const     THEN w.s("Const")
  ELSIF state = Var       THEN w.s("Var")
  ELSIF state = Par       THEN w.s("Par")
  ELSIF state = ORB.SProc THEN w.s("SProc")
  ELSIF state = ORB.Typ   THEN w.s("Typ")
  ELSIF state = Address   THEN w.s("Address")
  ELSIF state = Value     THEN w.s("Value")
  ELSIF state = Condition THEN w.s("Condition")
  (*
  ELSIF state = SysProc   THEN w.s("SysProc")
  ELSIF state = TypeId    THEN w.s("TypeId")
  *)
  ELSE                        w.i(state)
  END
END wstate;


PROCEDURE wtype(t: ORB.Type);
BEGIN
  IF t = NIL THEN w.s("(NIL)") ELSE
    w.s("(");  wform(t.form);  w.s(", mno ");   w.i(t.mno);
    w.s(", len ");  w.i(t.len);     w.s(", size ");  w.i(t.size);  w.c(")")
  END
END wtype;

PROCEDURE wobject(o: ORB.Object);
BEGIN
  w.s("(class ");  wclass(o.class);
  w.s(", type ");  wtype(o.type);
  w.s(", name '"); w.s(o.name);
  w.s("', val ");  w.i(o.val);
  w.s(", lev ");   w.i(o.lev);
  w.c(")")
END wobject;

PROCEDURE wlevel(level: INTEGER);
BEGIN
  IF    level = Immediate THEN w.s("Immediate")
  ELSIF level = Stack     THEN w.s("Stack")
  ELSIF level = Module    THEN w.s("Module")
  ELSIF level > Module    THEN w.s("Import["); w.i(level - 3); w.c("]")
  ELSE                         w.i(level)
  END
END wlevel;

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

PROCEDURE witem(i: Item);
BEGIN
  w.s("(");    wstate(i.state);
  IF i.state IN {Const, Var, Par} THEN
    IF    i.level = Immediate THEN w.c(" ");         w.i(i.value)
    ELSIF i.level = Stack     THEN w.s(" (rsp+");    w.i(i.value); w.c(")")
    ELSIF i.level = Module    THEN w.s(" (");        w.i(i.value); w.c(")")
    ELSIF i.level > Module    THEN w.s(" (import "); w.i(i.level-Module+1); w.c(":"); w.h(i.value); w.s("H)")
    ELSE                           w.s(" level ");   w.i(i.level)
    END
  ELSIF i.state = Address THEN w.s(" ("); wreg(i.value); w.c("+"); w.i(i.offset); w.c(")")
  ELSIF i.state = Value   THEN w.c(" "); wreg(i.value)
  ELSE                         w.s(", level ");   wlevel(i.level);
                               w.s(", value ");   w.i(i.value)
  END;
  IF i.readonly THEN w.s(", readonly") END;
  IF (i.offset # 0) & (i.state # Address) THEN w.s(", offset "); w.i(i.offset) END;
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


(* ---- Code generation ---- *)

PROCEDURE Emit(n, v: INTEGER);
BEGIN
  IF PC + n <= LEN(Program) THEN
    SYSTEM.COPY(SYSTEM.ADR(v), SYSTEM.ADR(Program) + PC, n);
    INC(PC, n)
  ELSE
    ORS.Mark("program too long")
  END
END Emit;

PROCEDURE EmitRex(W: BOOLEAN; reg, index, base: INTEGER);
VAR rex: INTEGER;
BEGIN  rex := 0;
  IF W         THEN INC(rex, 8) END;
  IF reg   > 7 THEN INC(rex, 4) END;
  IF index > 7 THEN INC(rex, 2) END;
  IF base  > 7 THEN INC(rex)    END;
  IF rex   > 0 THEN Emit(1, 40H + rex) END
END EmitRex;

PROCEDURE FixAddr(x: Item): INTEGER;
BEGIN
  RETURN x.value
END FixAddr;

PROCEDURE NextReg;
BEGIN
  IF RH >= 15 THEN ORS.Mark("register stack overflow") ELSE INC(RH) END;
  IF RH = 4 THEN RH := 5 END  (* Skip stack pointer *)
END NextReg;

PROCEDURE PopRegStack;
BEGIN
  IF RH = 0 THEN ORS.Mark("register stack underflow") ELSE DEC(RH) END;
  IF RH = 4 THEN RH := 3 END  (* Skip stack pointer *)
END PopRegStack;

PROCEDURE IsSigned(x: ORB.Type): BOOLEAN;  (* returns whether x represents a signed value *)
BEGIN RETURN (x.form >= ORB.Int8) & (x.form <= ORB.Int64) END IsSigned;

PROCEDURE IsStructured(x: ORB.Type): BOOLEAN;
BEGIN RETURN (x.form = ORB.Array) OR (x.form = ORB.Record) OR (x.form = ORB.Pointer) END IsStructured;

PROCEDURE IsReference(x: Item): BOOLEAN;
BEGIN RETURN (x.state = ORB.Par) OR IsStructured(x.type) END IsReference;

PROCEDURE EmitIndexedEffectiveAddress(reg, base, index, scale, disp: INTEGER);
VAR mode, sc: INTEGER;
BEGIN  (* Emit mod/reg/rm, sib byte, and disp *)
  reg := reg MOD 8;
  IF base  > 0 THEN base  := base  MOD 8 END;
  IF index > 0 THEN index := index MOD 8 END;

  IF (base < 0) & (index < 0) THEN        (* disp only - always 32 bits *)
    Emit(1, reg * 8 + 5); Emit(4, disp)
  ELSE                                    (* modregrm and possibly SIB *)
    ASSERT(base >= 0);  ASSERT(index # 4);
    IF    (disp = 0)     & (base # 5)   THEN mode := 0
    ELSIF (disp >= -80H) & (disp < 80H) THEN mode := 1
    ELSE                                     mode := 2
    END;
    IF (base # 4) & (index < 0) THEN      (* modregem, no SIB *)
      Emit(1, mode * 64 + reg * 8 + base)
    ELSE
      Emit(1, mode * 64 + reg * 8 + 4);   (* modregrm specifies SIB follows *)
      IF index < 0 THEN index := 4 END;   (* specifies no index *)
      IF    scale >= 8  THEN sc := 3
      ELSIF scale >= 4  THEN sc := 2
      ELSIF scale >= 2  THEN sc := 1
      ELSE                   sc := 0
      END;
      Emit(1, sc * 64  +  index * 8  +  base);
    END;
    IF mode = 1 THEN Emit(1, disp) ELSIF mode = 2 THEN Emit(4, disp) END
  END
END EmitIndexedEffectiveAddress;

PROCEDURE EmitEffectiveAddress(reg, base, disp: INTEGER);
BEGIN EmitIndexedEffectiveAddress(reg, base, -1, 1, disp) END EmitEffectiveAddress;

PROCEDURE MakeCodeFixup(pc: INTEGER; x: Item): INTEGER;
VAR result: INTEGER;
BEGIN
  ASSERT(x.level > 1);
  result := x.value;
  IF x.level = Module THEN
    ASSERT(result < 80000000H)                   (* 1/0, 31/module offset *)
  ELSE
    ASSERT(result < 800000H);
    INC(result, 80000000H + LSL(x.level-2, 24))  (* 1/1, 5/module no, 24/import index *)
  END
RETURN result END MakeCodeFixup;

PROCEDURE EmitModRegRm(reg: INTEGER; x: Item);
VAR mode, sc, srcreg, disp: INTEGER;
BEGIN
  ASSERT(x.state IN {Var, Value, Address});
  IF x.offset # 0 THEN ASSERT(x.state = Address) END;
  reg := reg MOD 8;

  IF x.state = Value THEN  (* Register to register *)
    Emit(1, 0C0H + reg*8 + x.value MOD 8)
  ELSE
    (* memory reference *)
    srcreg := -1;  disp := 0;

    IF x.state = Address THEN
      srcreg := x.value MOD 8; disp := x.offset
    ELSIF x.level = Stack THEN
      srcreg := RSP;  ASSERT(x.state = Var)
    END;

    IF srcreg < 0 THEN  (* disp only - always 32 bits *)
      Emit(1, reg * 8 + 5);  mode := 2;                        (* 32 bit disp *)
    ELSE                (* modregrm and possibly SIB *)
      ASSERT(srcreg >= 0);
      IF    (x.state = Address) OR (x.level >= 2)
         OR (disp < -80H)     OR (disp >= 80H) THEN mode := 2  (* 32 bit disp *)
      ELSIF (disp = 0) & (srcreg # 5)          THEN mode := 0  (* 0 valued disp omitted *)
      ELSE                                          mode := 1  (* 8 bit signed disp *)
      END;
      IF srcreg # 4 THEN                (* modregem, no SIB *)
        Emit(1, mode*64 + reg*8 + srcreg)
      ELSE
        Emit(1, mode*64 + reg*8 + 4);   (* modregrm specifies SIB follows *)
        Emit(1, 4*8 + srcreg);
      END
    END;
    IF mode = 1 THEN
      Emit(1, disp)
    ELSIF mode = 2 THEN
      IF (x.state = Var) & (x.level >= 2) THEN
        Emit(4, MakeCodeFixup(PC, x))
      ELSE
        Emit(4, disp)
      END
    END
  END
END EmitModRegRm;

PROCEDURE LoadBasedToReg(VAR x: Item);
VAR size: INTEGER;  signed: BOOLEAN;
BEGIN
  (* Loads the value at a based address to a register *)
  (* Does not change indirect status *)
  Oldpc := PC;
  ASSERT((x.state IN {Var, Par}) & (x.level >= 1));
  IF x.state = Par THEN
    ASSERT(x.level = Stack);
    size := 8;  signed := FALSE
  ELSE
    size := x.type.size;  signed := IsSigned(x.type)
  END;
  ASSERT(size IN {1,2,4,8});

  IF signed THEN
    EmitRex(TRUE, RH, 0,0);
    IF    size = 1 THEN Emit(1, 0FH); Emit(1, 0BEH);
    ELSIF size = 2 THEN Emit(1, 0FH); Emit(1, 0BFH);
    ELSIF size = 4 THEN Emit(1, 63H)
                   ELSE Emit(1, 8BH)
    END
  ELSE (* Unsigned *)
    EmitRex(size # 4, RH, 0, 0);
    IF    size = 1 THEN Emit(1, 0FH); Emit(1, 0B6H);
    ELSIF size = 2 THEN Emit(1, 0FH); Emit(1, 0B7H);
                   ELSE Emit(1, 8BH)
    END
  END;
  (*
  IF x.state = Stack THEN
    EmitEffectiveAddress(RH, RSP, x.value)
  ELSE
    EmitEffectiveAddress(RH, -1, FixAddr(x))
  END;
  *)
  EmitModRegRm(RH, x);
  DisassemblerX64.Disassemble(Program, Oldpc, "Load based item to register");

  x.value := RH;  NextReg;
  IF IsStructured(x.type) THEN
    x.state := Address;  x.offset := 0
  ELSE
    x.state := Value
  END
END LoadBasedToReg;


PROCEDURE LoadIndirectRegToReg(VAR x: Item);
VAR size: INTEGER;
BEGIN  w.sl("LoadIndirectRegToReg");
  (* Load VAR parameter address to reg replacing existing reg value *)
  Oldpc := PC;
  ASSERT(x.state = Par);
  size := x.type.size;
  IF ~(size IN {1,2,4,8}) THEN w.s("** size: "); w.i(size); w.sl(" **") END;
  ASSERT(size IN {1,2,4,8});

  IF IsSigned(x.type) THEN
    EmitRex(TRUE, RH, 0, RH);
    IF    size = 1 THEN Emit(1, 0FH); Emit(1, 0BEH);
    ELSIF size = 2 THEN Emit(1, 0FH); Emit(1, 0BFH);
    ELSIF size = 4 THEN Emit(1, 63H)
                   ELSE Emit(1, 8BH)
    END
  ELSE (* Unsigned *)
    EmitRex(size # 4, RH, 0, RH);
    IF    size = 1 THEN Emit(1, 0FH); Emit(1, 0B6H);
    ELSIF size = 2 THEN Emit(1, 0FH); Emit(1, 0B7H);
                   ELSE Emit(1, 8BH)
    END
  END;
  EmitEffectiveAddress(RH, RH, x.offset);
  DisassemblerX64.Disassemble(Program, Oldpc, "Load indirect register to register");
END LoadIndirectRegToReg;


PROCEDURE LoadAddress(VAR x: Item);
(* Loads the address of a based item to a register. *)
BEGIN  w.sl("LoadAddress");
  ASSERT((x.state = Var) OR (x.state = Par));
  IF x.state = Var THEN
    Oldpc := PC;
    (* Generate load effect address instruction *)
    IF x.level = Stack THEN
      EmitRex(TRUE, RH, 0, RSP);  Emit(1, 8DH);
      EmitModRegRm(RH, x)
    ELSE
      EmitRex(TRUE, RH, 0, 0);  Emit(1, 8DH);
      EmitModRegRm(RH, x)
    END;
    DisassemblerX64.Disassemble(Program, Oldpc, "Load address");
    x.state := Address;  x.value := RH;  x.offset := 0
  ELSE
    LoadBasedToReg(x)
  END
END LoadAddress;


PROCEDURE Load(VAR x: Item); (* Loads to register *)
BEGIN
  Oldpc := PC;
  w.s("; Load "); witem(x); wl;
  ASSERT(x.state <= Address);
  IF (x.level = 0) THEN  (* Load immediate value *)
    ASSERT(~IsReference(x));

    Oldpc := PC;
    IF x.value = 0 THEN
      (* Clear register with 32 bit XOR *)
      EmitRex(FALSE, RH, 0, RH); Emit(1, 31H); Emit(1, 0C0H + LSL(RH MOD 8, 3) + RH MOD 8);
    ELSIF (x.value > 0) & (x.value < 100000000H) THEN
      (* Load 32 bit unsigned value as 32 bit load with zero extension *)
      EmitRex(FALSE, RH, 0,0); Emit(1, 0B8H + RH MOD 8); Emit(4, x.value);
    ELSIF (x.value < 0) & (x.value >= -80000000H) THEN
      (* Load 32 bit negative value with sign extended move *)
      EmitRex(TRUE, RH, 0,0); Emit(1, 0C7H); Emit(1, 0C0H + RH MOD 8); Emit(4,x.value);
    ELSE
      (* Need full 64 bit literal *)
      EmitRex(TRUE, RH, 0,0); Emit(1, 0B8H + RH MOD 8); Emit(8,x.value);
    END;
    DisassemblerX64.Disassemble(Program, Oldpc, "Load immediate");

    x.state := Value;  x.value := RH;  NextReg
  ELSE
    IF x.state # Address THEN LoadBasedToReg(x) END;
    IF IsReference(x) THEN LoadIndirectRegToReg(x) END
  END
END Load;

PROCEDURE IsDyadicNoOp(op: INTEGER; src: Item): BOOLEAN;
VAR result: BOOLEAN;
BEGIN result := FALSE;
  IF src.level = 0 THEN
    IF src.value = 0 THEN
      result := (op = Plus) OR (op = Minus) OR (op = Or) OR (op = Xor)
    ELSIF src.value = -1 THEN
      result := op = And
    END
  END
RETURN result END IsDyadicNoOp;

PROCEDURE Dyadic(op: INTEGER;  VAR dest, src: Item);
BEGIN
  ASSERT(op IN {Plus, Minus, Cmp, And, Or, Xor});

  IF (src.level = 0) & (dest.level = 0) THEN
    IF    op = Plus  THEN INC(dest.value, src.value)
    ELSIF op = Minus THEN DEC(dest.value, src.value)
    ELSIF op = Cmp   THEN DEC(dest.value, src.value)
    ELSIF op = And   THEN dest.value := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET,dest.value) * SYSTEM.VAL(SET,src.value));
    ELSIF op = Or    THEN dest.value := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET,dest.value) + SYSTEM.VAL(SET,src.value));
    ELSIF op = Xor   THEN dest.value := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET,dest.value) / SYSTEM.VAL(SET,src.value));
    END

  ELSIF ~IsDyadicNoOp(op, src) THEN

    w.s("; dyadic op "); wop(op);  wl;
    w.s("  dest "); witem(dest); wl;
    w.s("  src  "); witem(src);  wl;

    Load(dest);
    w.s("  dest "); witem(dest); wl;

    ASSERT(dest.state = Value);

    Oldpc := PC;
    IF (src.level = 0) & (src.value = 1) & ((op = Plus) OR (op = Minus)) THEN
      (* INC / DEC *)
      EmitRex(TRUE, 0, 0, dest.value);
      Emit(1, 0FFH);
      Emit(1, 0C0H  +  op MOD 16  +  dest.value MOD 8)
    ELSE
      (* Get source into a representation suitable for X64 dyadic alu operations *)
      IF IsReference(src) THEN LoadBasedToReg(src) END;
      IF (src.level = 0)
       & ((src.value >= 80000000H) OR (src.value < -80000000H)) THEN Load(src) END;

      (* Generate alu operation *)
      IF src.level = 0 THEN (* op destreg,immediate *)
        ASSERT(~IsReference(src));
        ASSERT((src.value >= -80000000H) & (src.value < 80000000H));
        EmitRex(TRUE, dest.value, 0,0);
        IF (src.value >= -80H) & (src.value < 80H) THEN Emit(1, 83H) ELSE Emit(1, 81H) END;
        Emit(1, 0C0H + op + dest.value MOD 8);
        IF (src.value >= -80H) & (src.value < 80H) THEN Emit(1, src.value)
                                                   ELSE Emit(4, src.value) END
      ELSIF IsReference(src) THEN  (* op destreg,[srcreg+offset] *)
        ASSERT(src.state = Value);
        EmitRex(TRUE, dest.value, 0, src.value);
        Emit(1, op+3);
        EmitEffectiveAddress(dest.value, src.value, src.offset)
      ELSIF src.level = Stack THEN (* op destreg,[RSP+offset] *)
        EmitRex(TRUE, dest.value, 0, 0);
        Emit(1, op+3);
        EmitEffectiveAddress(dest.value, RSP, src.value);
      ELSE (* op destreg,[reladdress] *)
        EmitRex(TRUE, dest.value, 0,0);
        Emit(1, op + dest.value MOD 8);
        EmitEffectiveAddress(dest.value, -1, FixAddr(src));
      END
    END;
    DisassemblerX64.Disassemble(Program, Oldpc, "Dyadic operator");

    RH := dest.value
  END
END Dyadic;


(* ---- Item construction ---- *)

PROCEDURE ZeroFill(VAR obj: ARRAY OF BYTE);  VAR i: INTEGER;
BEGIN FOR i := 0 TO LEN(obj)-1 DO obj[i] := 0 END END ZeroFill;

PROCEDURE MakeItem*(VAR x: Item; y: ORB.Object; curlev: INTEGER);
BEGIN
  IF y.type.base = NIL THEN
    IF IsStructured(y.type) THEN
      w.l; w.sl("** unexpected structured type without base: **");
      wtype(y.type); wl; ASSERT(FALSE)
    END
  ELSE
    IF ~IsStructured(y.type) & (y.type.form # ORB.Proc) THEN
      w.l; w.sl("** unexpected unstructured type with base: **");
      wtype(y.type); wl; ASSERT(FALSE)
    END
  END;

  ASSERT(y.class IN {Const, Var, Par, ORB.SProc, ORB.Typ});

  ZeroFill(x);
  x.state    := y.class;
  x.type     := y.type;
  x.value    := y.val;
  x.readonly := y.rdo;

  IF (y.class = ORB.Const) & (y.type.form # ORB.Proc) THEN x.level := 0 (* Immediate *)
  ELSE (* Var, Par or Const Proc *)
    IF y.class = ORB.Par THEN ASSERT(y.lev > 0) END;
    IF y.lev > 0 THEN x.level := Stack           (* Stack *)
    ELSE              x.level := Module - y.lev  (* Module / Import *)
    END
  END;

  start("MakeItem ");
  wstate(x.state); w.s("/"); wform(x.type.form);
  w.s(" state "); wstate(x.state);
  w.s(", value $"); w.h(x.value);
  IF x.readonly THEN w.s(", readonly") END;
  IF x.offset # 0 THEN w.s(", offset "); w.i(x.offset) END;
  IF x.type.form = ORB.String THEN w.s(", length "); w.i(x.length) END;
  w.s(" from object: "); wobject(y);
  w.s(", curlev: ");        w.i(curlev); w.l
END MakeItem;

PROCEDURE MakeConstItem*(VAR x: Item;  t: ORB.Type;  v: INTEGER);
BEGIN
  start("MakeConstItem, value: "); w.i(v); w.s(", type "); wtype(t); wl;
  ZeroFill(x);
  x.state := ORB.Const;
  x.type  := t;
  x.level := Immediate;
  x.value := v;
END MakeConstItem;

PROCEDURE MakeRealItem*(VAR x: Item; val: REAL);
BEGIN
  start("MakeRealItem(x; val)"); wl;
  ZeroFill(x);
  x.state := ORB.Const;
  x.type  := ORB.realType;
  x.level := Immediate;
  x.value := SYSTEM.VAL(INTEGER, val)
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

PROCEDURE MakeStringItem*(VAR x: Item; len: INTEGER); (*copies string from ORS-buffer to ORG-string array*)
VAR i: INTEGER;
BEGIN
  ZeroFill(x);
  x.state    := ORB.Const;
  x.type     := ORB.strType;
  x.level    := Module;
  x.readonly := TRUE;
  x.length   := len;

  IF len <= 8 THEN        (* Strings of up to 9 bytes are recorded in item.value *)
    x.level := Immediate;
    SYSTEM.GET(SYSTEM.ADR(ORS.str), x.value)
  ELSE                    (* Strings of 9 or more bytes are stored in the module's string table *)
    x.level := Module;
    x.value := FindString(ORS.str, len)
  END;

  start("MakeStringItem('");
  i := 0;
  WHILE (i < len) & (ORS.str[i] # 0X) DO w.c(ORS.str[i]); INC(i) END;
  w.s("')");
  IF (len > 8) & (x.value + len < Strx) THEN w.s(" (reused)") END;
  wl
END MakeStringItem;



(* ---- Item conversion ---- *)

PROCEDURE StrToChar*(VAR x: Item);
BEGIN start("StrToChar(x)  x"); witem(x); wl;
  ASSERT(x.level     = Immediate);
  ASSERT(x.state     = ORB.Const);
  ASSERT(x.type.form = ORB.String);
  ASSERT(x.length    = 2);
  ASSERT(x.readonly);

  x.type  := ORB.charType;
  x.value := x.value MOD 256
END StrToChar;


(* ---- selection - fields, indices & pointers ---- *)

PROCEDURE Index*(VAR x, y: Item);   (* x := x[y] *)
VAR elemsize: INTEGER;
BEGIN start("Index(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl;
  ASSERT(x.type.form = ORB.Array);
  elemsize := x.type.base.size;
  w.s("  x base type: "); wtype(x.type.base); wl;
  w.s("  elemsize: "); w.i(elemsize); wl;
  IF x.state # Address THEN LoadAddress(x) END;
  (*
  IF elemsize # 1 THEN
    Load(y);
    (* TODO: Multiply y by element size *)
  END;
  *)
  IF y.level = 0 THEN
    x.offset := y.value * elemsize;
  ELSE
    Dyadic(Plus, x, y)
  END;
END Index;

PROCEDURE DeRef*(VAR x: Item);
BEGIN start("DeRef(x), x: "); witem(x); wl;
  ASSERT(x.type.form = ORB.Pointer);
  Load(x)
END DeRef;

PROCEDURE Field*(VAR x: Item; y: ORB.Object);   (* x := x.y *)
BEGIN start("Field(x: Item; y: Object)"); wl;
  w.s("  x: "); witem(x);   wl;
  w.s("  y: "); wobject(y); wl;
END Field;


(* ---- Paramter passing ---- *)

PROCEDURE ValueParam*(VAR x: Item);
BEGIN start("ValueParam(x: Item)  x: "); witem(x);  wl; (* load(x) *)
END ValueParam;

PROCEDURE VarParam*(VAR x: Item; ftype: ORB.Type);
BEGIN start("VarParam(x: Item; ftype: Type)"); wl;
  w.s("  x:     "); witem(x);     wl;
  w.s("  ftype: "); wtype(ftype); wl;
END VarParam;

PROCEDURE OpenArrayParam*(VAR x: Item);
BEGIN start("OpenArrayParam(x: Item)  x: "); witem(x);  wl;
END OpenArrayParam;

PROCEDURE StringParam*(VAR x: Item);
BEGIN start("StringParam(x: Item)  x: "); witem(x);  wl;
END StringParam;


(* ---- Procedure / function calls ---- *)

PROCEDURE PrepCall*(VAR x: Item; VAR r: INTEGER);
BEGIN start("PrepCall(x: Item; r "); w.i(r); w.s(")  x "); witem(x); wl;
END PrepCall;

PROCEDURE Call*(VAR x: Item; r: INTEGER);
BEGIN start("Call(x: Item; r "); w.i(r); w.s(")  x "); witem(x); wl;
END Call;

PROCEDURE Enter*(parblksize, locblksize: INTEGER; int: BOOLEAN);
BEGIN
  start("Enter(parblksize ");  w.i(parblksize);
  w.s(", locblksize ");        w.i(locblksize);  w.sl(")")
END Enter;

PROCEDURE Return*(form: INTEGER; VAR x: Item; size: INTEGER; int: BOOLEAN);
BEGIN start("Return("); wform(form);
  w.s("; x; size "); w.i(size);
  IF int THEN w.sl("; int)") ELSE w.sl("; ~interrupt)") END;
  w.s("  x:    "); witem(x);    wl;
END Return;


(* ---- Standard functions ---- *)

PROCEDURE Abs*(VAR x: Item);
BEGIN start("Abs(x)  x "); witem(x); wl
END Abs;

PROCEDURE Odd*(VAR x: Item);
BEGIN start("Odd(x)  x "); witem(x); wl
END Odd;

PROCEDURE Floor*(VAR x: Item);
BEGIN start("Floor(x)  x "); witem(x); wl
END Floor;

PROCEDURE Float*(VAR x: Item);
BEGIN start("Float(x)  x "); witem(x); wl
END Float;

PROCEDURE Ord*(VAR x: Item);
BEGIN start("Ord(x)  x "); witem(x); wl
END Ord;

PROCEDURE Len*(VAR x: Item);
BEGIN start("Len(x)  x "); witem(x); wl
END Len;

PROCEDURE Shift*(fct: INTEGER; VAR x, y: Item);
BEGIN start("Shift(");
  IF    fct = 0 THEN w.s("LSL")
  ELSIF fct = 1 THEN w.s("ASR")
  ELSIF fct = 2 THEN w.s("ROR")
  ELSE               w.i(fct)
  END;
  w.s("; x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END Shift;

PROCEDURE ADC*(VAR x, y: Item);
BEGIN start("ADC(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
  END ADC;

PROCEDURE SBC*(VAR x, y: Item);
BEGIN start("SBC(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
  END SBC;

PROCEDURE UML*(VAR x, y: Item);
BEGIN start("UML(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
  END UML;

PROCEDURE Bit*(VAR x, y: Item);
BEGIN start("Bit(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
  END Bit;

PROCEDURE REG*(VAR x: Item);
BEGIN start("REG(x)  x "); witem(x); wl
END REG;

PROCEDURE Adr*(VAR x: Item);
BEGIN start("Adr(x)  x "); witem(x); wl
END Adr;

PROCEDURE COND*(VAR x: Item);
BEGIN start("COND(x)  x "); witem(x); wl
END COND;


(* ---- Standard procedures --- *)

PROCEDURE Increment*(upordown: INTEGER; VAR x, y: Item);
BEGIN start("Increment(");
  IF upordown = 0 THEN w.s("up") ELSE w.s("down") END;
  w.sl(", x, y) ");
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END Increment;

PROCEDURE Include*(inorex: INTEGER; VAR x, y: Item);
BEGIN start("Include"); wl
END Include;

PROCEDURE Assert*(VAR x: Item);
BEGIN start("Assert(x)  x: ");  witem(x);  wl
END Assert;

PROCEDURE New*(VAR x: Item);
BEGIN start("New(x)  x: ");  witem(x);  wl
END New;

PROCEDURE Led*(VAR x: Item);
BEGIN start("Led(x)  x: ");  witem(x);  wl
END Led;

PROCEDURE LDPSR*(VAR x: Item);
BEGIN start("LDPSR(x)  x: ");  witem(x);  wl
END LDPSR;

PROCEDURE Pack*(VAR x, y: Item);
BEGIN start("Pack(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END Pack;

PROCEDURE Unpk*(VAR x, y: Item);
BEGIN start("Unpk(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END Unpk;

PROCEDURE Get*(VAR x, y: Item);
BEGIN start("Get(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END Get;

PROCEDURE Put*(VAR x, y: Item);
BEGIN start("Put(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END Put;

PROCEDURE LDREG*(VAR x, y: Item);
BEGIN start("LDREG(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END LDREG;

PROCEDURE Copy*(VAR x, y, z: Item);
BEGIN start("Copy(x, y, z)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl;
  w.s("  z: "); witem(z); wl
END Copy;


(* ---- Sets ---- *)

PROCEDURE Set*(VAR x, y: Item);   (* x := {x .. y} *)
BEGIN start("Set(x, y)  x := {x..y}"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl;
END Set;

PROCEDURE Singleton*(VAR x: Item);  (* x := {x} *)
BEGIN start("Singleton(x)  x := {x} "); witem(x); wl
END Singleton;

PROCEDURE In*(VAR x, y: Item);  (* x := x IN y *)
BEGIN start("In(x, y)  x := x IN y"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl;
END In;

PROCEDURE SetOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
BEGIN start("SetOp("); wsym(op); w.sl("; x, y)  x := x op y");
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl;
END SetOp;


(* ---- Boolean operators ---- *)

PROCEDURE Not*(VAR x: Item);   (* x := ~x *)
BEGIN start("Not(x)  x "); witem(x); wl
END Not;

PROCEDURE And1*(VAR x: Item);   (* x := x & *)
BEGIN start("And1(x)  x "); witem(x); wl
END And1;

PROCEDURE Or1*(VAR x: Item);   (* x := x OR *)
BEGIN start("Or1(x)  x "); witem(x); wl
END Or1;

PROCEDURE And2*(VAR x, y: Item);   (* x := x & y *)
BEGIN start("And2(x)  x "); witem(x); wl
END And2;

PROCEDURE Or2*(VAR x, y: Item);   (* x := x OR y *)
BEGIN start("Or2(x)  x "); witem(x); wl
END Or2;

PROCEDURE IntRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
BEGIN start("IntRelation("); wsym(op); w.sl(", x, y)");
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl;
  END IntRelation;

PROCEDURE RealRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
BEGIN start("RealRelation("); wsym(op); w.sl(", x, y)");
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END RealRelation;

PROCEDURE StringRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
BEGIN start("StringRelation("); wsym(op); w.sl(", x, y)");
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END StringRelation;



(* ---- Arithmetic operators ---- *)

PROCEDURE AddOp*(op: INTEGER; VAR x, y: Item);   (* x := x +- y *)
BEGIN start("AddOp("); wsym(op); w.sl("; x, y) x := x +- y");
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl;
  (* Convert op to x86 opcode *)
  IF    op = ORS.plus  THEN op := Plus
  ELSIF op = ORS.minus THEN op := Minus
  ELSIF op = ORS.or    THEN op := Or
  ELSE ASSERT(FALSE)
  END;
  Dyadic(op, x, y)
END AddOp;

PROCEDURE MulOp*(VAR x, y: Item);   (* x := x * y *)
BEGIN start("MulOp(x, y)  x := x * y"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END MulOp;

PROCEDURE DivOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
BEGIN start("DivOp("); wsym(op); w.sl("; x, y) x := x /DIV y");
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END DivOp;

PROCEDURE RealOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
BEGIN start("RealOp("); wsym(op); w.sl("; x, y) x := x op y");
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END RealOp;

PROCEDURE Neg*(VAR x: Item);   (* x := -x *)
BEGIN start("Neg(x)  x := -x"); wl;
  w.s("  x: "); witem(x); wl;
END Neg;


(* ---- Control flow ---- *)

PROCEDURE CFJump*(VAR x: Item);
BEGIN start("CFJump(x)  x "); witem(x);  wl
END CFJump;

PROCEDURE FJump*(VAR L: INTEGER);
BEGIN start("FJump(L "); w.i(L); w.sl(")")
END FJump;

PROCEDURE BJump*(VAR L: INTEGER);
BEGIN start("BJump(L "); w.i(L); w.sl(")")
END BJump;

PROCEDURE CBJump*(VAR x: Item; L: INTEGER);
BEGIN start("CBJump(x, L "); w.i(L);
  w.s(")  x "); witem(x); wl
END CBJump;

PROCEDURE FixLink*(L: INTEGER);
BEGIN start("FixLink(L "); w.i(L); w.sl(")")
END FixLink;

PROCEDURE FixOne*(at: INTEGER);
BEGIN start("FixOne(at "); w.i(at); w.sl(")")
END FixOne;

(*
PROCEDURE Fixup*(VAR x: Item);  BEGIN FixLink(x.a) END Fixup;
*)

PROCEDURE Here*(): INTEGER;
BEGIN start("Here -> pc "); w.i(PC); wl;
RETURN PC END Here;

PROCEDURE For0*(VAR x, y: Item);
BEGIN start("For0(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END For0;

PROCEDURE For1*(VAR x, y, z, incr: Item; VAR L: INTEGER);
BEGIN start("For1(x, y, z, incr; L "); w.i(L);  w.sl(")");
  w.s("  x:    "); witem(x);    wl;
  w.s("  y:    "); witem(y);    wl;
  w.s("  z:    "); witem(z);    wl;
  w.s("  incr: "); witem(incr); wl
END For1;

PROCEDURE For2*(VAR x, y, incr: Item);
BEGIN start("For2(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END For2;


(* ---- Assignment ---- *)

PROCEDURE Store*(VAR x, y: Item); (* x := y *)
VAR size, op, srcreg, basereg: INTEGER;
BEGIN start("Store(x, y) x := y"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl;
  size  := x.type.size;

  ASSERT(x.state IN {Var, Par});
  IF IsReference(x) & (x.state # Address) THEN LoadBasedToReg(x) END;

  IF y.state <= Address THEN
    IF (y.level # Immediate)
    OR (y.level = Immediate) & ((y.value >= 80000000H) OR (y.value < -80000000H))
    OR (y.level = Immediate) & (y.value = 0) THEN
      Load(y)
    END
  END;

  Oldpc := PC;
  IF y.level = 0 THEN
    IF size = 1 THEN op := 0C6H ELSE op := 0C7H END;
    srcreg := -1
  ELSE
    IF size = 1 THEN op := 088H ELSE op := 089H END;
    ASSERT(y.state = Value);
    srcreg := y.value
  END;

  IF    x.state = Value THEN basereg := x.value
  ELSIF x.level = Stack    THEN basereg := RSP
  ELSE                         basereg := -1 END;

  IF size = 4 THEN Emit(1, 66H) END;
  EmitRex(size=8, srcreg, 0, basereg);
  Emit(1, op);

  IF x.state = Value THEN
    EmitEffectiveAddress(srcreg, basereg, 0)
  ELSIF x.level = Stack THEN
    EmitEffectiveAddress(srcreg, RSP, x.value)
  ELSE
    EmitEffectiveAddress(srcreg, -1, FixAddr(x))
  END;

  IF y.level = 0 THEN
    IF size < 4 THEN Emit(size, y.value) ELSE Emit(4, y.value) END
  END;

  DisassemblerX64.Disassemble(Program, Oldpc, "Store");
  RH := 0;
END Store;


PROCEDURE StoreStruct*(VAR x, y: Item); (* x := y, frame = 0 *)
BEGIN start("StoreStruct(x, y) x := y"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END StoreStruct;

PROCEDURE CopyString*(VAR x, y: Item);  (* x := y *)
BEGIN start("CopyString(x, y) x := y"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END CopyString;


(* ---- ---- *)

PROCEDURE BuildTD*(T: ORB.Type; VAR dc: INTEGER);
BEGIN start("BuildTD(T, dc "); w.i(dc); w.sl(")");
  w.s("  T: "); wtype(T); wl
END BuildTD;

PROCEDURE TypeTest*(VAR x: Item; T: ORB.Type; varpar, isguard: BOOLEAN);
BEGIN start("TypeTest(x, T, ");
  IF varpar  THEN w.s("varpar, ")  ELSE w.s("~varpar, ")  END;
  IF isguard THEN w.sl("isguard)") ELSE w.sl("~isguard)") END;
  w.s("  x: "); witem(x); wl;
  w.s("  T: "); wtype(T); wl
END TypeTest;

PROCEDURE CheckRegs*;
BEGIN start("CheckRegs"); wl
END CheckRegs;


(* ---- ---- *)

PROCEDURE Open*(v: INTEGER; sourcefn: ARRAY OF CHAR);
BEGIN
  w.s("ORG Open(v "); w.i(v); w.s("; '"); w.s(sourcefn); w.sl("')");

  sourcefile := Files.Old(sourcefn);  Files.Set(source, sourcefile, 0);
  sourceBol := 0;  sourceLine := 1;  GetChar;

  Strx := 0;  PC := 0;  RH := 0;
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
