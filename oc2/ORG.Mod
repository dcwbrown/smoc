MODULE ORG;  (* DCWB June 2023; Code generator for X64 *)

(* Originally: N.Wirth, 16.4.2016 / 4.4.2017 / 17.9.2018  Oberon compiler; code generator for RISC*)

IMPORT SYSTEM, Files, ORS, ORB, w := Writer, K := Kernel;

CONST
  WordSize* = 8;
  MaxStrx   = 8000;  (* String buffer size *)
  MaxPC     = 10000H;

  RSP       = 4;     (* X64 stack pointer *)

  (* Item base kinds *)
  Immediate* = 0;
  Stack      = 1;
  Module     = 2;
  Import     = 3;
  Register   = 4;
  TypeId     = 5;  (* Note, does not have to be same as ORB.Typ *)
  SysProc    = 6;  (* Note, does not have to be same as ORB.SProc *)
  Condition  = 7;

  (* Dyadic ALU operations, values correspond to x86 instruction set *)
  Plus  = 00H;
  Or    = 08H;
  And   = 20H;
  Minus = 28H;
  Xor   = 30H;
  Cmp   = 38H;


TYPE
  Item* = RECORD
    class*:    INTEGER;  (* From original object *)
    type*:     ORB.Type;
    base*:     INTEGER;  (* Immediate/Stack/Module/Import/Reg/TypeId/SysProc/Condition *)
    value*:    INTEGER;
    indirect:  BOOLEAN;
    readonly*: BOOLEAN;
    offset:    INTEGER;  (* Only if indirect *)
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

  (* Buffers for building one line of siddasembly *)
  Hexbuf:  ARRAY 40 OF CHAR;  Hbi: INTEGER;
  Mnembuf: ARRAY 80 OF CHAR;  Mbi: INTEGER;


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
  ELSIF form = ORB.Int     THEN w.s("Int")
  ELSIF form = ORB.Int16   THEN w.s("Int16")
  ELSIF form = ORB.Int32   THEN w.s("Int32")
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

PROCEDURE wbase(base: INTEGER);
BEGIN
  IF    base = Immediate THEN w.s("Immediate")
  ELSIF base = Stack     THEN w.s("Stack")
  ELSIF base = Module    THEN w.s("Module")
  ELSIF base = Import    THEN w.s("Import")
  ELSIF base = Register  THEN w.s("Register")
  ELSIF base = SysProc   THEN w.s("SysProc")
  ELSIF base = TypeId    THEN w.s("TypeId")
                         ELSE w.i(base)
  END
END wbase;


PROCEDURE wtype(t: ORB.Type);
BEGIN
  IF t = NIL THEN w.s("(NIL)") ELSE
    w.s("(form ");  wform(t.form);  w.s(", mno ");   w.i(t.mno);
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

PROCEDURE witem(i: Item);
BEGIN
  w.s("(class ");    wclass(i.class);
  w.s(", base ");    wbase(i.base);
  w.s(", value ");   w.i(i.value);
  IF i.readonly THEN w.s(", readonly") END;
  IF i.indirect THEN w.s(", indirect, offset "); w.i(i.offset) END;
  IF i.type # NIL THEN
    IF i.type.form = ORB.String THEN w.s(", length "); w.i(i.length) END;
    w.s(", type ");  wtype(i.type)
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

PROCEDURE hexdigit(d: INTEGER): CHAR;  VAR ch: CHAR;
BEGIN IF d < 10 THEN ch := CHR(d + 48) ELSE ch := CHR(d + 55) END
RETURN ch END hexdigit;

PROCEDURE whex(n, v: INTEGER);  (* n, v corresponding to Emit params *)
BEGIN
  IF n > 1 THEN whex(n-1, v DIV 256) END;
  w.c(hexdigit(v MOD 256 DIV 16));
  w.c(hexdigit(v MOD 16))
END whex;

PROCEDURE waddr;
BEGIN whex(4, PC); w.s(": ") END waddr;


(* ---- Disassembly ---- *)

PROCEDURE Ins(s: ARRAY OF CHAR; VAR buf: ARRAY OF CHAR; VAR bi: INTEGER);
VAR i: INTEGER;
BEGIN  i := 0;
  WHILE s[i] # 0X DO buf[bi] := s[i]; INC(bi); INC(i) END
END Ins;

PROCEDURE InsDec(v: INTEGER; VAR buf: ARRAY OF CHAR; VAR bi: INTEGER);
BEGIN
  IF v >= 10 THEN InsDec(v DIV 10, buf, bi); v := v MOD 10 END;
  buf[bi] := CHR(v + 48);
  INC(bi)
END InsDec;

PROCEDURE InsHex(v: INTEGER; VAR buf: ARRAY OF CHAR; VAR bi: INTEGER);
VAR upper: INTEGER;
BEGIN
  upper := v DIV 16 MOD 1000000000000000H;
  IF upper # 0 THEN InsHex(upper, buf, bi); v := v MOD 16 END;
  IF v < 10 THEN buf[bi] := CHR(v + 48) ELSE buf[bi] := CHR(v + 55) END;
  INC(bi)
END InsHex;

PROCEDURE AddMnem(s: ARRAY OF CHAR);  (* Add string to mnemonic buffer *)
BEGIN Ins(s, Mnembuf, Mbi) END AddMnem;

PROCEDURE InsReg(size, r: INTEGER); (* r is X64 reg num *)
BEGIN
  IF size = 64 THEN
    IF    r =  0 THEN AddMnem("rax")
    ELSIF r =  1 THEN AddMnem("rcx")
    ELSIF r =  2 THEN AddMnem("rdx")
    ELSIF r =  3 THEN AddMnem("rbx")
    ELSIF r =  4 THEN AddMnem("rsp")
    ELSIF r =  5 THEN AddMnem("rbp")
    ELSIF r =  6 THEN AddMnem("rsi")
    ELSIF r =  7 THEN AddMnem("rdi")
    ELSIF r =  8 THEN AddMnem("r8")
    ELSIF r =  9 THEN AddMnem("r9")
    ELSIF r = 10 THEN AddMnem("r10")
    ELSIF r = 11 THEN AddMnem("r11")
    ELSIF r = 12 THEN AddMnem("r12")
    ELSIF r = 13 THEN AddMnem("r13")
    ELSIF r = 14 THEN AddMnem("r14")
    ELSIF r = 15 THEN AddMnem("r15")
    END
  ELSIF size = 32 THEN
    IF    r =  0 THEN AddMnem("eax")
    ELSIF r =  1 THEN AddMnem("ecx")
    ELSIF r =  2 THEN AddMnem("edx")
    ELSIF r =  3 THEN AddMnem("ebx")
    ELSIF r =  4 THEN AddMnem("esp")
    ELSIF r =  5 THEN AddMnem("ebp")
    ELSIF r =  6 THEN AddMnem("esi")
    ELSIF r =  7 THEN AddMnem("edi")
    ELSIF r =  8 THEN AddMnem("r8d")
    ELSIF r =  9 THEN AddMnem("r9d")
    ELSIF r = 10 THEN AddMnem("r10d")
    ELSIF r = 11 THEN AddMnem("r11d")
    ELSIF r = 12 THEN AddMnem("r12d")
    ELSIF r = 13 THEN AddMnem("r13d")
    ELSIF r = 14 THEN AddMnem("r14d")
    ELSIF r = 15 THEN AddMnem("r15d")
    END
  ELSIF size = 16 THEN
    IF    r =  0 THEN AddMnem("ax")
    ELSIF r =  1 THEN AddMnem("cx")
    ELSIF r =  2 THEN AddMnem("dx")
    ELSIF r =  3 THEN AddMnem("bx")
    ELSIF r =  4 THEN AddMnem("sp")
    ELSIF r =  5 THEN AddMnem("bp")
    ELSIF r =  6 THEN AddMnem("si")
    ELSIF r =  7 THEN AddMnem("di")
    ELSIF r =  8 THEN AddMnem("r8w")
    ELSIF r =  9 THEN AddMnem("r9w")
    ELSIF r = 10 THEN AddMnem("r10w")
    ELSIF r = 11 THEN AddMnem("r11w")
    ELSIF r = 12 THEN AddMnem("r12w")
    ELSIF r = 13 THEN AddMnem("r13w")
    ELSIF r = 14 THEN AddMnem("r14w")
    ELSIF r = 15 THEN AddMnem("r15w")
    END
  ELSIF size = 8 THEN
    IF    r =  0 THEN AddMnem("al")
    ELSIF r =  1 THEN AddMnem("cl")
    ELSIF r =  2 THEN AddMnem("dl")
    ELSIF r =  3 THEN AddMnem("bl")
    ELSIF r =  4 THEN AddMnem("spl")
    ELSIF r =  5 THEN AddMnem("bpl")
    ELSIF r =  6 THEN AddMnem("sil")
    ELSIF r =  7 THEN AddMnem("dil")
    ELSIF r =  8 THEN AddMnem("r8l")
    ELSIF r =  9 THEN AddMnem("r9l")
    ELSIF r = 10 THEN AddMnem("r10l")
    ELSIF r = 11 THEN AddMnem("r11l")
    ELSIF r = 12 THEN AddMnem("r12l")
    ELSIF r = 13 THEN AddMnem("r13l")
    ELSIF r = 14 THEN AddMnem("r14l")
    ELSIF r = 15 THEN AddMnem("r15l")
    ELSIF r = 16 THEN AddMnem("ah")    ELSIF r = 17 THEN AddMnem("ch")
    ELSIF r = 18 THEN AddMnem("dh")    ELSIF r = 19 THEN AddMnem("bh")
    END
  ELSE ASSERT(FALSE)
  END
END InsReg;

PROCEDURE HexByte(h: INTEGER; VAR buf: ARRAY OF CHAR; VAR bi: INTEGER);
BEGIN
  buf[bi] := hexdigit(h DIV 16 MOD 16); INC(bi);
  buf[bi] := hexdigit(h        MOD 16); INC(bi)
END HexByte;

PROCEDURE HexVal(n, h: INTEGER; VAR buf: ARRAY OF CHAR; VAR bi: INTEGER);
BEGIN
  IF n > 32 THEN HexVal(32, h DIV 100000000H MOD 100000000H, buf, bi) END;
  IF n > 16 THEN HexVal(16, h DIV 10000H     MOD 10000H,     buf, bi) END;
  IF n > 8  THEN HexByte(h DIV 100H, buf, bi) END;
  HexByte(h, buf, bi)
END HexVal;

PROCEDURE InsAluOp(op: INTEGER);
BEGIN
  IF    op = 0 THEN AddMnem("add    ")  ELSIF op = 1 THEN AddMnem("or     ")
  ELSIF op = 2 THEN AddMnem("adc    ")  ELSIF op = 3 THEN AddMnem("sbb    ")
  ELSIF op = 4 THEN AddMnem("and    ")  ELSIF op = 5 THEN AddMnem("sub    ")
  ELSIF op = 6 THEN AddMnem("xor    ")  ELSIF op = 7 THEN AddMnem("cmp    ")
  END
END InsAluOp;

PROCEDURE InsBaseIndexScaleDisp(base, index, scale, disp: INTEGER);
BEGIN
  AddMnem("[");
  IF base >= 0  THEN InsReg(64, base) END;
  IF index >= 0 THEN
    AddMnem("+"); InsReg(64, index);
    IF scale > 1 THEN AddMnem("*"); InsDec(scale, Mnembuf, Mbi) END
  END;
  IF disp # 0 THEN AddMnem("+");  InsHex(disp, Mnembuf, Mbi); AddMnem("H") END;
  AddMnem("]")
END InsBaseIndexScaleDisp;

PROCEDURE AddHex(size, val: INTEGER);
BEGIN
  HexVal(size, val, Hexbuf, Hbi);  Hexbuf[Hbi] := " ";  INC(Hbi);
END AddHex;

PROCEDURE GetSigned(size: INTEGER; VAR pc, val: INTEGER);
VAR i8: SYSTEM.INT8;  i16: SYSTEM.INT16;  i32: SYSTEM.INT32;
BEGIN
  IF    size = 8  THEN SYSTEM.GET(SYSTEM.ADR(Program[pc]), i8);  val := i8
  ELSIF size = 16 THEN SYSTEM.GET(SYSTEM.ADR(Program[pc]), i16); val := i16
  ELSIF size = 32 THEN SYSTEM.GET(SYSTEM.ADR(Program[pc]), i32); val := i32
  ELSIF size = 64 THEN SYSTEM.GET(SYSTEM.ADR(Program[pc]), val)
  END;
  AddHex(size, val);
  INC(pc, size DIV 8)
END GetSigned;

PROCEDURE GetUnsigned(size: INTEGER; VAR pc, val: INTEGER);
VAR c8: BYTE;  c16: SYSTEM.CARD16;  c32: SYSTEM.CARD32;
BEGIN
  IF    size = 8  THEN SYSTEM.GET(SYSTEM.ADR(Program[pc]), c8);  val := c8
  ELSIF size = 16 THEN SYSTEM.GET(SYSTEM.ADR(Program[pc]), c16); val := c16
  ELSIF size = 32 THEN SYSTEM.GET(SYSTEM.ADR(Program[pc]), c32); val := c32
  ELSIF size = 64 THEN SYSTEM.GET(SYSTEM.ADR(Program[pc]), val)
  END;
  AddHex(size, val);
  INC(pc, size DIV 8)
END GetUnsigned;

PROCEDURE DisSIB(VAR pc, base, index, scale: INTEGER);  (* Returns X64 reg nums in range 0-7 *)
BEGIN
  AddHex(8, Program[pc]);
  scale := LSL(1, Program[pc] DIV 64);
  base  := Program[pc] MOD 8;
  IF Program[pc] DIV 8 MOD 8 = 4 THEN
    index := -1; scale := 1
  ELSE
    index := Program[pc] DIV 8 MOD 8;
  END;
  INC(pc)
END DisSIB;

PROCEDURE DisModRegRm(VAR pc, reg, base, index, disp, scale: INTEGER; size: INTEGER; VAR indirect: BOOLEAN);
VAR mode, rm: INTEGER;
BEGIN
  AddHex(8, Program[pc]);
  mode     := Program[pc] DIV 64;
  reg      := Program[pc] DIV 8 MOD 8;
  rm       := Program[pc] MOD 8;
  scale    := 1;
  index    := -1;
  base     := -1;
  disp     := 0;
  indirect := mode < 3;
  INC(pc);
  IF mode = 0 THEN
    IF rm = 4 THEN (* sib *)
      DisSIB(pc, base, index, scale);
    ELSIF rm = 5 THEN (* disp32 *)
      GetSigned(32, pc, disp)
    ELSE base := rm
    END
  ELSIF mode = 1 THEN
    IF rm = 4 THEN (* sib + disp8 *)
      DisSIB(pc, base, index, scale)
    ELSE base := rm
    END;
    GetSigned(8, pc, disp)
  ELSIF mode = 2 THEN
    IF rm = 4 THEN (* sib + disp32 *)
      DisSIB(pc, base, index, scale)
    ELSE base := rm
    END;
    GetSigned(32, pc, disp)
  ELSE  (* Mode = 3 *)
    IF (size = 8) & (rm >= 4) THEN (* ah/bh/ch/dh *)
      base := 16 + rm-4
    ELSE
      base := rm
    END
  END
END DisModRegRm;

PROCEDURE Disassemble(VAR pc: INTEGER; comment: ARRAY OF CHAR);
VAR
  reghigh, indexhigh, basehigh: BOOLEAN;
  reg, base, index:             INTEGER;
  regsize, dispsize:            INTEGER;
  disp:                         INTEGER;
  mode, scale:                  INTEGER;
  opcode:                       INTEGER;
  indirect, tofirst:            BOOLEAN;
BEGIN
  reghigh  := FALSE;  indexhigh := FALSE;  basehigh := FALSE;
  reg      := -1;     base      := -1;     index    := -1;
  regsize  := 32;     dispsize  := 0;
  disp     := 0;      mode      := 0;      scale    := 1;
  indirect := FALSE;  tofirst   := FALSE;
  Hbi      := 0;      Mbi       := 0;

  whex(4, pc);  w.s(":  ");

  IF Program[pc] = 66H THEN
    AddHex(8, Program[pc]);
    regsize := 16
  END;

  IF Program[pc] DIV 16 = 4 THEN (* REX prefix *)
    AddHex(8, Program[pc]);
    IF Program[pc] DIV 8 MOD 2 = 1 THEN regsize := 64 END;
    reghigh   := Program[pc] DIV 4 MOD 2 = 1;
    indexhigh := Program[pc] DIV 2 MOD 2 = 1;
    basehigh  := Program[pc]       MOD 2 = 1;
    INC(pc)
  END;

  opcode := Program[pc];  INC(pc);  AddHex(8, opcode);

  IF (opcode < 64) & (opcode MOD 8 < 6) THEN (* alu op *)

    InsAluOp(opcode DIV 8);
    IF ~ODD(opcode) THEN regsize := 8 END;
    tofirst := opcode MOD 4 >= 2;
    IF opcode MOD 8 >= 4 THEN (* rax/al Immediate *)
      InsReg(regsize, 0);  (* al/ax/eax/eax *)
      IF regsize < 64 THEN GetSigned(regsize, pc, disp) ELSE GetSigned(32, pc, disp) END;
    ELSE (* modregrm *)
      DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect);
      IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
      IF indexhigh & (index < 8) THEN INC(index, 8) END;
      IF basehigh  & (base  < 8) THEN INC(base,  8) END;
      IF indirect THEN
        IF tofirst THEN
          InsReg(regsize, reg);
          AddMnem(", ");
          InsBaseIndexScaleDisp(base, index, scale, disp)
        ELSE
          InsBaseIndexScaleDisp(base, index, scale, disp);
          AddMnem(", ");
          InsReg(regsize, reg);
        END
      ELSE
        IF tofirst THEN
          InsReg(regsize, reg);   AddMnem(", ");  InsReg(regsize, base)
        ELSE
          InsReg(regsize, base);  AddMnem(", ");  InsReg(regsize, reg)
        END
      END
    END

  ELSIF (opcode >= 80H) & (opcode <= 83H) THEN (* alu with immediate op *)

    IF opcode = 81H THEN dispsize := 32 ELSE dispsize := 8 END;
    DisModRegRm(pc, opcode, base, index, disp, scale, regsize, indirect);
    InsAluOp(opcode);
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    IF indirect THEN
      InsBaseIndexScaleDisp(base, index, scale, disp)
    ELSE
      InsReg(regsize, base);
    END;
    AddMnem(", ");
    GetSigned(dispsize, pc, disp);  InsHex(disp, Mnembuf, Mbi); AddMnem("H")

  ELSIF (opcode >= 88H) & (opcode <= 8BH) THEN (* mov *)

    AddMnem("mov    ");
    IF ~ODD(opcode) THEN regsize := 8 END;
    tofirst := opcode >= 8AH;
    DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect);
    IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    IF tofirst THEN
      InsReg(regsize, reg);
      AddMnem(", ");
      InsBaseIndexScaleDisp(base, index, scale, disp)
    ELSE
      InsBaseIndexScaleDisp(base, index, scale, disp);
      AddMnem(", ");
      InsReg(regsize, reg);
    END

  ELSIF opcode = 08DH THEN (* load effective address *)

    AddMnem("lea    ");
    DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect);
    IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    InsReg(regsize, reg);
    AddMnem(", ");
    InsBaseIndexScaleDisp(base, index, scale, disp)

  ELSIF opcode DIV 16 = 0BH THEN  (* Move immediate to register *)

    AddMnem("mov    ");
    base := opcode MOD 8;
    IF basehigh THEN INC(base, 8) END;
    IF opcode < 0B8H THEN regsize := 8 END;
    InsReg(regsize, base);
    AddMnem(", ");
    IF regsize < 64 THEN GetUnsigned(regsize, pc, disp) ELSE GetUnsigned(32, pc, disp) END;
    InsHex(disp, Mnembuf, Mbi); AddMnem("H")

  ELSIF (opcode = 0C6H) OR (opcode = 0C7H) THEN (* Move immediate to modregrm *)

    IF opcode = 0C6H THEN regsize := 8 END;
    AddMnem("mov    ");
    DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect);
    ASSERT(indirect);
    IF    regsize = 8  THEN AddMnem("byte")   ELSIF regsize = 16 THEN AddMnem("word")
    ELSIF regsize = 32 THEN AddMnem("dword")  ELSIF regsize = 64 THEN AddMnem("qword") END;
    InsBaseIndexScaleDisp(base, index, scale, disp);
    AddMnem(", ");
    IF regsize < 64 THEN GetUnsigned(regsize, pc, disp) ELSE GetUnsigned(32, pc, disp) END;
    InsHex(disp, Mnembuf, Mbi); AddMnem("H")

  ELSIF opcode = 0FFH THEN  (* Group 5 extensions to primary opcode map *)

    DisModRegRm(pc, opcode, base, index, disp, scale, regsize, indirect);
    IF opcode < 2 THEN (* INC/DEC *)
      IF opcode = 0 THEN AddMnem("inc    ") ELSE AddMnem("dec    ") END;
      IF indirect THEN
        InsBaseIndexScaleDisp(base, index, scale, disp)
      ELSE
        InsReg(regsize, base)
      END
    ELSE
      AddMnem("??5")
    END

  ELSIF opcode = 0FH THEN  (* Secondary opcode map *)

    opcode := Program[pc];  INC(pc);  AddHex(8, opcode);

    IF (opcode = 0B6H) OR (opcode = 0B6H) THEN  (* movzx *)
      IF opcode = 0B6H THEN regsize := 8 ELSE regsize := 16 END;
      DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect);
      AddMnem("movzx  ");  InsReg(64, reg);  AddMnem(",");

      IF indirect THEN
        IF regsize = 8 THEN AddMnem(" byte") ELSE AddMnem(" word") END;
        InsBaseIndexScaleDisp(base, index, scale, disp)
      ELSE
        InsReg(regsize, base)
      END
    ELSE
      AddMnem("??")
    END

  ELSE
    AddMnem("unknown")
  END;

  WHILE Hbi < 24 DO Hexbuf[Hbi] := " "; INC(Hbi) END;  Hexbuf[Hbi] := 0X;
  IF comment # "" THEN
    WHILE Mbi < 24 DO Mnembuf[Mbi] := " "; INC(Mbi) END;
    AddMnem("; ");  AddMnem(comment)
  END;
  Mnembuf[Mbi] := 0X;
  w.s(Hexbuf); w.sl(Mnembuf)
END Disassemble;


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

PROCEDURE Signed(x: ORB.Type): BOOLEAN;  (* returns whether x represents a signed value *)
BEGIN RETURN (x.form >= ORB.Int) & (x.form <= ORB.Int32) END Signed;

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

PROCEDURE LoadBasedToReg(VAR x: Item);
VAR size: INTEGER;  signed: BOOLEAN;
BEGIN
  (* Loads the value at a based address to a register *)
  (* Does not change indirect status *)
  Oldpc := PC;
  ASSERT(x.base IN {Stack, Module, Import});
  IF x.indirect THEN
    ASSERT(x.base = Stack);
    size := 8;  signed := FALSE
  ELSE
    size := x.type.size;  signed := x.type.form IN {ORB.Int, ORB.Int16, ORB.Int32};
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
  IF x.base = Stack THEN
    EmitEffectiveAddress(RH, RSP, x.value)
  ELSE
    EmitEffectiveAddress(RH, -1, FixAddr(x))
  END;
  Disassemble(Oldpc, "Load based item to register");

  x.base := Register;  x.value := RH;  NextReg
END LoadBasedToReg;


PROCEDURE LoadIndirectRegToReg(VAR x: Item);
VAR size: INTEGER;
BEGIN
  (* Load VAR parameter addres to reg replacing existing reg value *)
  Oldpc := PC;
  ASSERT(x.indirect & (x.base = Register));
  size := x.type.size;
  ASSERT(size IN {1,2,4,8});

  IF x.type.form IN {ORB.Int, ORB.Int16, ORB.Int32} THEN (* signed *)
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
  Disassemble(Oldpc, "Load indirect register to register");
  x.indirect := FALSE
END LoadIndirectRegToReg;


PROCEDURE LoadAddress(VAR x: Item);
(* Loads the address of a based item to a register. *)
BEGIN
  IF x.indirect THEN
    LoadBasedToReg(x);  x.indirect := FALSE;
  ELSE
    ASSERT(x.base IN {Stack, Module, Import});
    Oldpc := PC;
    (* Generate load effect address instruction *)
    IF x.base = Stack THEN
      EmitRex(TRUE, RH, 0, RSP);  Emit(1, 8DH);
      EmitEffectiveAddress(RH, RSP, x.value)
    ELSE
      EmitRex(TRUE, RH, 0, RSP);  Emit(1, 8DH);
      EmitEffectiveAddress(RH, -1, FixAddr(x))
    END;
    Disassemble(Oldpc, "Load address");
    x.base := Register;  x.value := RH;
  END
END LoadAddress;


PROCEDURE Load(VAR x: Item); (* Loads to register *)
BEGIN
  Oldpc := PC;
  w.s("; Load "); witem(x); wl;
  ASSERT(x.base <= Register);
  IF x.base = Immediate THEN  (* Load immediate value *)
    ASSERT(~x.indirect);

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
    Disassemble(Oldpc, "Load immediate");

    x.base := Register;  x.value := RH;  NextReg
  ELSE
    IF x.base # Register THEN LoadBasedToReg(x) END;
    IF x.indirect THEN LoadIndirectRegToReg(x) END
  END
END Load;

PROCEDURE IsDyadicNoOp(op: INTEGER; src: Item): BOOLEAN;
VAR result: BOOLEAN;
BEGIN result := FALSE;
  IF src.base = Immediate THEN
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

  IF (src.base = Immediate) & (dest.base = Immediate) THEN
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

    Load(dest);  ASSERT(dest.base = Register);

    Oldpc := PC;
    IF (src.base = Immediate) & (src.value = 1) & ((op = Plus) OR (op = Minus)) THEN
      (* INC / DEC *)
      EmitRex(TRUE, 0, 0, dest.value);
      Emit(1, 0FFH);
      Emit(1, 0C0H  +  op MOD 16  +  dest.value MOD 8)
    ELSE
      (* Get source into a representation suitable for X64 dyadic alu operations *)
      IF src.indirect & (src.base IN {Stack, Module, Import}) THEN LoadBasedToReg(src) END;
      IF (src.base = Immediate)
       & ((src.value >= 80000000H) OR (src.value < -80000000H)) THEN Load(src) END;

      (* Generate alu operation *)
      IF src.base = Immediate THEN (* op destreg,immediate *)
        ASSERT(~src.indirect);
        ASSERT((src.value >= -80000000H) & (src.value < 80000000H));
        EmitRex(TRUE, dest.value, 0,0);
        IF (src.value >= -80H) & (src.value < 80H) THEN Emit(1, 83H) ELSE Emit(1, 81H) END;
        Emit(1, 0C0H + op + dest.value MOD 8);
        IF (src.value >= -80H) & (src.value < 80H) THEN Emit(1, src.value)
                                                   ELSE Emit(4, src.value) END
      ELSIF src.indirect THEN  (* op destreg,[srcreg+offset] *)
        ASSERT(src.base = Register);
        EmitRex(TRUE, dest.value, 0, src.value);
        Emit(1, op+3);
        EmitEffectiveAddress(dest.value, src.value, src.offset)
      ELSIF src.base = Stack THEN (* op destreg,[RSP+offset] *)
        EmitRex(TRUE, dest.value, 0, 0);
        Emit(1, op+3);
        EmitEffectiveAddress(dest.value, RSP, src.value);
      ELSE (* op destreg,[reladdress] *)
        EmitRex(TRUE, dest.value, 0,0);
        Emit(1, op + dest.value MOD 8);
        EmitEffectiveAddress(dest.value, -1, FixAddr(src));
      END
    END;
    Disassemble(Oldpc, "Dyadic operator");

    RH := dest.value
  END
END Dyadic;


(* ---- Item construction ---- *)

PROCEDURE ZeroFill(VAR obj: ARRAY OF BYTE);  VAR i: INTEGER;
BEGIN FOR i := 0 TO LEN(obj)-1 DO obj[i] := 0 END END ZeroFill;

PROCEDURE MakeItem*(VAR x: Item; y: ORB.Object; curlev: INTEGER);
BEGIN
  ZeroFill(x);
  x.class    := y.class;
  x.type     := y.type;
  x.value    := y.val;
  x.readonly := y.rdo;
  IF    y.class = ORB.SProc      THEN x.base := SysProc
  ELSIF y.class = ORB.Typ        THEN x.base := TypeId
  ELSIF (y.class = ORB.Const)
      & (y.type.form # ORB.Proc) THEN x.base := Immediate
  ELSE (* Var, Par or Const Proc *)
    IF y.class = ORB.Par THEN ASSERT(y.lev > 0);  x.indirect := TRUE END;
    IF    y.lev < 0 THEN x.base := Import;  INC(x.value, LSL(-y.lev-1, 20))
    ELSIF y.lev = 0 THEN x.base := Module
                    ELSE x.base := Stack
    END
  END;
  start("MakeItem ");
  wclass(x.class); w.s("/"); wform(x.type.form);
  w.s(" base "); wbase(x.base);
  w.s(", value $"); w.h(x.value);
  IF x.readonly THEN w.s(", readonly") END;
  IF x.indirect THEN w.s(", indirect, offset "); w.i(x.offset) END;
  IF x.type.form = ORB.String THEN w.s(", length "); w.i(x.length) END;
  w.s(" from object: "); wobject(y);
  w.s(", curlev: ");        w.i(curlev); w.l
END MakeItem;

PROCEDURE MakeConstItem*(VAR x: Item;  t: ORB.Type;  v: INTEGER);
BEGIN
  start("MakeConstItem, value: "); w.i(v); w.s(", type "); wtype(t); wl;
  ZeroFill(x);
  x.class := ORB.Const;
  x.type  := t;
  x.base  := Immediate;
  x.value := v;
END MakeConstItem;

PROCEDURE MakeRealItem*(VAR x: Item; val: REAL);
BEGIN
  start("MakeRealItem(x; val)"); wl;
  ZeroFill(x);
  x.class := ORB.Const;
  x.type  := ORB.realType;
  x.base  := Immediate;
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
  x.class    := ORB.Const;
  x.type     := ORB.strType;
  x.base     := Module;
  x.readonly := TRUE;
  x.length   := len;

  IF len <= 8 THEN        (* Strings of up to 9 bytes are recorded in item.value *)
    x.base := Immediate;
    SYSTEM.GET(SYSTEM.ADR(ORS.str), x.value)
  ELSE                    (* Strings of 9 or more bytes are stored in the module's string table *)
    x.base  := Module;
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
  ASSERT(x.base      = Immediate);
  ASSERT(x.class     = ORB.Const);
  ASSERT(x.type.form = ORB.String);
  ASSERT(x.length    = 2);
  ASSERT(x.readonly);
  ASSERT(~x.indirect);

  x.type  := ORB.charType;
  x.value := x.value MOD 256
END StrToChar;


(* ---- selection - fields, indices & pointers ---- *)

PROCEDURE Index*(VAR x, y: Item);   (* x := x[y] *)
VAR Oldpc: INTEGER;
BEGIN start("Index(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl;
  Oldpc := PC;
  LoadAddress(x);
  Dyadic(Plus, x, y);
  x.indirect := TRUE;  x.offset := 0;
END Index;

PROCEDURE DeRef*(VAR x: Item);
BEGIN start("DeRef(x), x: "); witem(x); wl;
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
  IF int THEN w.sl("; int)") ELSE w.sl("; ~int)") END;
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

  ASSERT(x.base IN {Register, Stack, Module, Import});
  IF x.indirect & (x.base # Register) THEN LoadBasedToReg(x) END;

  IF (y.base # Immediate)
  OR (y.base = Immediate) & ((y.value >= 80000000H) OR (y.value < -80000000H))
  OR (y.base = Immediate) & (y.value = 0) THEN
    Load(y)
  END;

  Oldpc := PC;
  IF y.base = Immediate THEN
    IF size = 1 THEN op := 0C6H ELSE op := 0C7H END;
    srcreg := -1
  ELSE
    IF size = 1 THEN op := 088H ELSE op := 089H END;
    ASSERT(y.base = Register);
    srcreg := y.value
  END;

  IF    x.base = Register THEN basereg := x.value
  ELSIF x.base = Stack    THEN basereg := RSP
  ELSE                         basereg := -1 END;

  IF size = 4 THEN Emit(1, 66H) END;
  EmitRex(size=8, srcreg, 0, basereg);
  Emit(1, op);

  IF x.base = Register THEN
    EmitEffectiveAddress(srcreg, basereg, 0)
  ELSIF x.base = Stack THEN
    EmitEffectiveAddress(srcreg, RSP, x.value)
  ELSE
    EmitEffectiveAddress(srcreg, -1, FixAddr(x))
  END;

  IF y.base = Immediate THEN
    IF size < 4 THEN Emit(size, y.value) ELSE Emit(4, y.value) END
  END;

  Disassemble(Oldpc, "Store");
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
