MODULE X64;  (* DCWB August 2023; Assembler for X64 *)

(* Originally: N.Wirth, 16.4.2016 / 4.4.2017 / 17.9.2018  Oberon compiler; code generator for RISC*)

IMPORT SYSTEM, ORS, w := Writer;

CONST
  MaxPC*   = 10000H;
  RSP*     = 4;     (* X64 stack pointer *)
  AllFree* = {0,1,2,3,5,6,7,8,9,10,11,12,13,14,15};  (* RSP (4) is reserved *)

  (* levels *)
  Register*         = 0;  (* immediate value (in disp) or individual register (in base) *)
  Memory*       = 1;  (* [base-reg + index-reg * scale + disp]                      *)
  DoubleIndirect* = 2;  (* [ [base-reg + index-reg * scale + disp] + offset]          *)

  (* Dyadic ALU operations, values correspond to x86 instruction set *)
  Plus*  = 00H;
  Or*    = 08H;
  And*   = 20H;
  Minus* = 28H;
  Xor*   = 30H;
  Cmp*   = 38H;


TYPE
  Operand* = RECORD
    direct*:  BOOLEAN;  (* TRUE: register/immediate, FALSE memory            *)
    parptr*:  BOOLEAN;  (* param ptr (base = stack offset)                   *)
    signed*:  BOOLEAN;
    base*:    INTEGER;  (* direct/base address register or stack offset      *)
    index*:   INTEGER;  (* index register (memory only)                      *)
    scale*:   INTEGER;  (* index register scale 0,1,2,3 (memory only)        *)
    disp*:    INTEGER;  (* address offset (memory only)                      *)
    section*: INTEGER;  (* -1: none(abs), 0: module, >=1: import             *)
    size*:    INTEGER;
  END;

VAR
  Program*: ARRAY MaxPC OF BYTE;
  PC*:      INTEGER;  (* Code generation PC *)
  disasmpc: INTEGER;  (* Disassembly PC - trails generation pc *)

  (* Buffers for building one line of Disassembly *)
  Hexbuf:  ARRAY 40 OF CHAR;  Hbi: INTEGER;
  Mnembuf: ARRAY 80 OF CHAR;  Mbi: INTEGER;

  Free*: SET;      (* Free registers *)
  SPO*:  INTEGER;  (* Stack pointer offset - space used by temmporaries *)


PROCEDURE Min(a, b: INTEGER): INTEGER;
BEGIN IF a < b THEN b := a END;  RETURN b END Min;

PROCEDURE Max(a, b: INTEGER): INTEGER;
BEGIN IF a > b THEN b := a END;  RETURN b END Max;


(* -------------------------------- Assembly -------------------------------- *)

PROCEDURE ClearOperand*(VAR o: Operand);  (* Sets o to immediate value 0 *)
BEGIN
  o.direct  := TRUE;
  o.parptr  := FALSE;
  o.base    := -1;
  o.index   := -1;
  o.scale   := 0;
  o.disp    := 0;
  o.section := -1;
  o.size    := 8;
  o.signed  := FALSE;
END ClearOperand;


PROCEDURE Emit*(value: INTEGER);
BEGIN
  IF PC < LEN(Program) THEN
    Program[PC] := value;  INC(PC)
  ELSE
    ORS.Mark("program too long")
  END
END Emit;

PROCEDURE EmitBytes*(size, value: INTEGER);
BEGIN
  ASSERT(size IN {1, 2, 4, 8});
  IF PC + size <= LEN(Program) THEN
    SYSTEM.COPY(SYSTEM.ADR(value), SYSTEM.ADR(Program) + PC, size);
    INC(PC, size)
  ELSE
    ORS.Mark("program too long")
  END
END EmitBytes;


PROCEDURE Patch*(pc, size, value: INTEGER);
BEGIN
  IF (pc < 0) OR (pc + size > MaxPC) THEN
    w.s("** Patch address $"); w.h(pc); w.sl(" out of range **");
    ASSERT(FALSE)
  END;
  ASSERT(size IN {1, 2, 4, 8});
  SYSTEM.COPY(SYSTEM.ADR(value), SYSTEM.ADR(Program) + pc, size)
END Patch;


PROCEDURE PeekUnsigned*(pc, size: INTEGER): INTEGER;
VAR c8: BYTE;  c16: SYSTEM.CARD16;  c32: SYSTEM.CARD32;  result: INTEGER;
BEGIN
  IF (pc < 0) OR (pc + size > MaxPC) THEN
    w.s("** PeekUnsigned address $"); w.h(pc); w.sl(" out of range **")
  END;
  IF    size = 1 THEN SYSTEM.GET(SYSTEM.ADR(Program[pc]), c8);  result := c8
  ELSIF size = 2 THEN SYSTEM.GET(SYSTEM.ADR(Program[pc]), c16); result := c16
  ELSIF size = 4 THEN SYSTEM.GET(SYSTEM.ADR(Program[pc]), c32); result := c32
  ELSIF size = 8 THEN SYSTEM.GET(SYSTEM.ADR(Program[pc]), result)
  ELSE ASSERT(FALSE)
  END
RETURN result END PeekUnsigned;

PROCEDURE PeekSigned*(pc, size: INTEGER): INTEGER;
VAR i8: SYSTEM.INT8;  i16: SYSTEM.INT16;  i32: SYSTEM.INT32;  result: INTEGER;
BEGIN
  IF (pc < 0) OR (pc + size > MaxPC) THEN
    w.s("** PeekSigned address $"); w.h(pc); w.sl(" out of range **")
  END;
  IF    size = 1 THEN SYSTEM.GET(SYSTEM.ADR(Program[pc]), i8);  result := i8
  ELSIF size = 2 THEN SYSTEM.GET(SYSTEM.ADR(Program[pc]), i16); result := i16
  ELSIF size = 4 THEN SYSTEM.GET(SYSTEM.ADR(Program[pc]), i32); result := i32
  ELSIF size = 8 THEN SYSTEM.GET(SYSTEM.ADR(Program[pc]), result)
  ELSE ASSERT(FALSE)
  END
RETURN result END PeekSigned;


PROCEDURE EmitModRegMem*(reg, base, index, scale, disp: INTEGER);
VAR mode: INTEGER;
BEGIN
  reg := reg MOD 8;
  IF base < 0 THEN  (* offset only *)
    ASSERT(index < 0);
    ASSERT((disp >= -80000000H) & (disp < 80000000H));
    Emit(reg * 8 + 5);  mode := 2;  (* 32 bit disp *)
  ELSE
    (* Emit register based address *)
    IF base = RSP THEN INC(disp, 8 * SPO) END;  (* Allow for temporaries pushed to stack *)
    base := base MOD 8;
    (* Determine mode and offset size *)
    IF    (disp < -80H) OR (disp >= 80H) THEN mode := 2  (* 32 bit disp *)
    ELSIF (disp = 0)    &  (base  # 5)   THEN mode := 0  (* 0 valued disp omitted exept for RBP *)
    ELSE                                      mode := 1  (* 8 bit signed disp *)
    END;
    IF (index < 0) & (base # 4) THEN
      Emit(mode*64 + reg*8 + base)    (* ModRegRm *)
    ELSE
      Emit(mode*64 + reg*8 + 4);      (* ModRegRm *)
      IF index < 0 THEN
        Emit(4*8 + base)              (* SIB with no index reg *)
      ELSE
        ASSERT(index # 4);  (* Cannot use rsp as index *)
        ASSERT((scale >= 0) & (scale <= 3));
        Emit(scale*64 + index MOD 8 * 8 + base) (* SIB with index reg *)
      END
    END
  END;
  IF    mode = 1 THEN Emit(disp)
  ELSIF mode = 2 THEN EmitBytes(4, disp)
  END
END EmitModRegMem;


PROCEDURE EmitModRegReg*(reg, base: INTEGER);
VAR mode: INTEGER;
BEGIN
  reg := reg MOD 8;
  ASSERT(base >= 0);
  Emit(0C0H + reg*8 + base MOD 8)
END EmitModRegReg;


PROCEDURE EmitPrefices*(reg, size, base, index: INTEGER);
VAR rex: INTEGER;
BEGIN  rex := 0;
  (* Emit prefices *)
  IF size  =  2 THEN Emit(66H)       END;
  IF size  >  4 THEN INC(rex, 8)     END;
  IF reg   >= 8 THEN INC(rex, 4)     END;
  IF index >= 8 THEN INC(rex, 2)     END;
  IF base  >= 8 THEN INC(rex, 1)     END;
  IF rex   #  0 THEN Emit(40H + rex) END;
END EmitPrefices;

PROCEDURE EmitRegMemOp*(op, reg, size, base, index, scale, disp: INTEGER);
BEGIN
  EmitPrefices(reg, size, base, index);
  IF op >= 100H THEN Emit(op DIV 100H) END;
  Emit(op MOD 100H);
  EmitModRegMem(reg, base, index, scale, disp)
END EmitRegMemOp;

PROCEDURE EmitRegRegOp*(op, reg, size, base: INTEGER);
BEGIN
  EmitPrefices(reg, size, base, -1);
  IF op >= 100H THEN Emit(op DIV 100H) END;
  Emit(op MOD 100H);
  EmitModRegReg(reg, base)
END EmitRegRegOp;

PROCEDURE EmitOp*(op, reg, size: INTEGER; o: Operand);
BEGIN
  IF o.direct THEN
    ASSERT(o.base >= 0);
    EmitRegRegOp(op, reg, size, o.base)
  ELSE
    EmitRegMemOp(op, reg, size, o.base, o.index, o.scale, o.disp)
  END
END EmitOp;




(* ------------------------------ Disassembly ------------------------------- *)


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

PROCEDURE hexdigit(d: INTEGER): CHAR;  VAR ch: CHAR;
BEGIN IF d < 10 THEN ch := CHR(d + 48) ELSE ch := CHR(d + 55) END
RETURN ch END hexdigit;

PROCEDURE whex(n, v: INTEGER);  (* n, v corresponding to Emit params *)
BEGIN
  IF n > 1 THEN whex(n-1, v DIV 256) END;
  w.c(hexdigit(v MOD 256 DIV 16));
  w.c(hexdigit(v MOD 16))
END whex;

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

PROCEDURE InsBaseIndexScaleDisp(indirect: BOOLEAN; otherreg, regsize, base, index, scale, disp: INTEGER);
BEGIN
  IF indirect THEN
    IF otherreg < 0 THEN
      IF    regsize = 8  THEN AddMnem("byte")   ELSIF regsize = 16 THEN AddMnem("word")
      ELSIF regsize = 32 THEN AddMnem("dword")  ELSIF regsize = 64 THEN AddMnem("qword") END
    END;
    AddMnem("[");
    IF base >= 0  THEN InsReg(64, base) END;
    IF index >= 0 THEN
      AddMnem("+"); InsReg(64, index);
      IF    scale = 1 THEN AddMnem("*2")
      ELSIF scale = 2 THEN AddMnem("*4")
      ELSIF scale = 2 THEN AddMnem("*8")
      END
    END;
    IF disp # 0 THEN AddMnem("+");  InsHex(disp, Mnembuf, Mbi); AddMnem("H") END;
    AddMnem("]")
  ELSE
    ASSERT(base >= 0);  InsReg(regsize, base)
  END
END InsBaseIndexScaleDisp;

PROCEDURE AddHex(size, val: INTEGER);
BEGIN
  HexVal(size, val, Hexbuf, Hbi);  Hexbuf[Hbi] := " ";  INC(Hbi);
END AddHex;


PROCEDURE GetUnsigned(size: INTEGER; VAR pc, val: INTEGER);
BEGIN
  val := PeekUnsigned(pc, size DIV 8);
  AddHex(size, val);
  INC(pc, size DIV 8)
END GetUnsigned;

PROCEDURE GetSigned(size: INTEGER; VAR pc, val: INTEGER);
BEGIN
  val := PeekSigned(pc, size DIV 8);
  AddHex(size, val);
  INC(pc, size DIV 8)
END GetSigned;


PROCEDURE DisSIB(VAR pc, base, index, scale: INTEGER);  (* Returns X64 reg nums in range 0-7 *)
BEGIN
  AddHex(8, Program[pc]);
  scale := Program[pc] DIV 64;
  base  := Program[pc] MOD 8;
  IF Program[pc] DIV 8 MOD 8 = 4 THEN
    index := -1; scale := 0
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
  scale    := 0;
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


PROCEDURE DisassembleInstruction(VAR pc: INTEGER; comment: ARRAY OF CHAR);
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
  disp     := 0;      mode      := 0;      scale    := 0;
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
      IF tofirst THEN
        InsReg(regsize, reg);
        AddMnem(",");
        InsBaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp)
      ELSE
        InsBaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp);
        AddMnem(",");
        InsReg(regsize, reg);
      END
    END

  ELSIF (opcode >= 50H) & (opcode <= 57H) THEN

    AddMnem("push   ");
    reg := opcode MOD 8;
    IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
    InsReg(64, reg)

  ELSIF (opcode >= 58H) & (opcode <= 5FH) THEN

    AddMnem("pop    ");
    reg := opcode MOD 8;
    IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
    InsReg(64, reg)

  ELSIF (opcode >= 80H) & (opcode <= 83H) THEN (* alu with immediate op *)

    IF opcode = 81H THEN dispsize := 32 ELSE dispsize := 8 END;
    DisModRegRm(pc, opcode, base, index, disp, scale, regsize, indirect);
    InsAluOp(opcode);
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    InsBaseIndexScaleDisp(indirect, -1, regsize, base, index, scale, disp);
    AddMnem(",");
    GetSigned(dispsize, pc, disp);  InsHex(disp, Mnembuf, Mbi); AddMnem("H")

  ELSIF (opcode = 84H) OR (opcode = 85H) THEN

    AddMnem("test   ");
    IF opcode = 84H THEN regsize := 8 END;
    DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect);
    IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    InsReg(regsize, reg);
    AddMnem(",");
    InsBaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp)

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
      AddMnem(",");
      InsBaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp)
    ELSE
      InsBaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp);
      AddMnem(",");
      InsReg(regsize, reg);
    END

  ELSIF opcode = 08DH THEN (* load effective address *)

    AddMnem("lea    ");
    DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect);
    IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    InsReg(regsize, reg);
    AddMnem(",");
    InsBaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp)

  ELSIF opcode DIV 16 = 0BH THEN  (* Move immediate to register *)

    AddMnem("mov    ");
    base := opcode MOD 8;
    IF basehigh THEN INC(base, 8) END;
    IF opcode < 0B8H THEN regsize := 8 END;
    InsReg(regsize, base);
    AddMnem(",");
    (*IF regsize < 64 THEN GetUnsigned(regsize, pc, disp) ELSE GetUnsigned(32, pc, disp) END;*)
    GetUnsigned(regsize, pc, disp);
    InsHex(disp, Mnembuf, Mbi); AddMnem("H")

  ELSIF opcode = 0C2H THEN

    AddMnem("ret    ");  GetUnsigned(16, pc, disp);
    InsHex(disp, Mnembuf, Mbi); AddMnem("H")

  ELSIF opcode = 0C3H THEN

    AddMnem("ret")

  ELSIF (opcode = 0C6H) OR (opcode = 0C7H) THEN (* Move immediate to modregrm *)

    IF opcode = 0C6H THEN regsize := 8 END;
    AddMnem("mov    ");
    DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect);
    ASSERT(indirect);  ASSERT(reg = 0);
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    InsBaseIndexScaleDisp(indirect, -1, regsize, base, index, scale, disp);
    AddMnem(",");
    IF regsize < 64 THEN GetUnsigned(regsize, pc, disp) ELSE GetUnsigned(32, pc, disp) END;
    InsHex(disp, Mnembuf, Mbi); AddMnem("H")

  ELSIF opcode = 0C9H THEN

    AddMnem("leave")

  ELSIF opcode = 0E8H THEN

    AddMnem("call   $");
    GetSigned(32, pc, disp);
    InsHex(pc + disp, Mnembuf, Mbi);
    AddMnem(" (disp ");
    IF disp < 0 THEN AddMnem("-");  disp := -disp ELSE AddMnem("+") END;
    InsDec(disp, Mnembuf, Mbi);
    AddMnem(")")

  ELSIF (opcode = 0E9H) OR (opcode = 0EBH) THEN

    AddMnem("jmp    $");
    IF opcode = 0E9H THEN GetSigned(32, pc, disp) ELSE GetSigned(8, pc, disp) END;
    InsHex(pc + disp, Mnembuf, Mbi);
    AddMnem(" (disp ");
    IF disp < 0 THEN AddMnem("-");  disp := -disp ELSE AddMnem("+") END;
    InsDec(disp, Mnembuf, Mbi);
    AddMnem(")")

  ELSIF (opcode = 0F6H) OR (opcode = 0F7H) THEN

    IF opcode = 0F6H THEN regsize := 8 END;
    DisModRegRm(pc, opcode, base, index, disp, scale, regsize, indirect);
    IF    opcode < 2 THEN AddMnem("test   ")
    ELSIF opcode = 2 THEN AddMnem("not    ")
    ELSIF opcode = 3 THEN AddMnem("neg    ")
    ELSIF opcode = 4 THEN AddMnem("mul    ")
    ELSIF opcode = 5 THEN AddMnem("imul   ")
    ELSIF opcode = 6 THEN AddMnem("div    ")
    ELSIF opcode = 7 THEN AddMnem("idiv   ")
    ELSE
      AddMnem("??")
    END;
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    InsBaseIndexScaleDisp(indirect, -1, regsize, base, index, scale, disp)

  ELSIF opcode = 0FFH THEN  (* Group 5 extensions to primary opcode map *)

    DisModRegRm(pc, opcode, base, index, disp, scale, regsize, indirect);
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    IF opcode < 3 THEN (* INC/DEC/CALL *)

      IF    opcode = 0 THEN AddMnem("inc    ")
      ELSIF opcode = 1 THEN AddMnem("dec    ")
      ELSE                  AddMnem("call   "); regsize := 64 END;
      InsBaseIndexScaleDisp(indirect, -1, regsize, base, index, scale, disp)

    ELSE
      AddMnem("??5")
    END

  ELSIF opcode = 0FH THEN  (* Secondary opcode map *)

    opcode := Program[pc];  INC(pc);  AddHex(8, opcode);

    IF (opcode = 0B6H) OR (opcode = 0B7H)
    OR (opcode = 0BEH) OR (opcode = 0BFH) THEN  (* movsx/movzx *)
      IF    opcode = 0B6H THEN AddMnem("movzx  ");  regsize := 8
      ELSIF opcode = 0B7H THEN AddMnem("movzx  ");  regsize := 16
      ELSIF opcode = 0BEH THEN AddMnem("movsx  ");  regsize := 8
      ELSIF opcode = 0BFH THEN AddMnem("movsx  ");  regsize := 16
      END;
      DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect);
      IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
      IF indexhigh & (index < 8) THEN INC(index, 8) END;
      IF basehigh  & (base  < 8) THEN INC(base,  8) END;
      InsReg(64, reg);  AddMnem(",");
      InsBaseIndexScaleDisp(indirect, -1, regsize, base, index, scale, disp)

    ELSIF (opcode >= 80H) & (opcode <= 8FH) THEN  (* 32 bit conditional jump *)

      IF    opcode = 80H THEN AddMnem("jo     ")
      ELSIF opcode = 81H THEN AddMnem("jno    ")
      ELSIF opcode = 82H THEN AddMnem("jc     ")
      ELSIF opcode = 83H THEN AddMnem("jnc    ")
      ELSIF opcode = 84H THEN AddMnem("jz     ")
      ELSIF opcode = 85H THEN AddMnem("jnz    ")
      ELSIF opcode = 86H THEN AddMnem("jbe    ")
      ELSIF opcode = 87H THEN AddMnem("ja     ")
      ELSIF opcode = 88H THEN AddMnem("js     ")
      ELSIF opcode = 89H THEN AddMnem("jns    ")
      ELSIF opcode = 8AH THEN AddMnem("jpe    ")
      ELSIF opcode = 8BH THEN AddMnem("jpo    ")
      ELSIF opcode = 8CH THEN AddMnem("jl     ")
      ELSIF opcode = 8DH THEN AddMnem("jge    ")
      ELSIF opcode = 8EH THEN AddMnem("jle    ")
      ELSIF opcode = 8FH THEN AddMnem("jg     ")
      END;
      GetSigned(32, pc, disp);
      IF disp < 0 THEN AddMnem("-");  disp := -disp  ELSE AddMnem("+") END;
      InsDec(disp, Mnembuf, Mbi)

    ELSIF (opcode >= 90H) & (opcode <= 9FH) THEN  (* SetCC *)

      IF    opcode = 90H THEN AddMnem("seto   ")
      ELSIF opcode = 91H THEN AddMnem("setno  ")
      ELSIF opcode = 92H THEN AddMnem("setc   ")
      ELSIF opcode = 93H THEN AddMnem("setnc  ")
      ELSIF opcode = 94H THEN AddMnem("setz   ")
      ELSIF opcode = 95H THEN AddMnem("setnz  ")
      ELSIF opcode = 96H THEN AddMnem("setbe  ")
      ELSIF opcode = 97H THEN AddMnem("seta   ")
      ELSIF opcode = 98H THEN AddMnem("sets   ")
      ELSIF opcode = 99H THEN AddMnem("setns  ")
      ELSIF opcode = 9AH THEN AddMnem("setpe  ")
      ELSIF opcode = 9BH THEN AddMnem("setpo  ")
      ELSIF opcode = 9CH THEN AddMnem("setl   ")
      ELSIF opcode = 9DH THEN AddMnem("setge  ")
      ELSIF opcode = 9EH THEN AddMnem("setle  ")
      ELSIF opcode = 9FH THEN AddMnem("setg   ")
      END;
      DisModRegRm(pc, reg, base, index, disp, scale, 8, indirect);
      IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
      IF indexhigh & (index < 8) THEN INC(index, 8) END;
      IF basehigh  & (base  < 8) THEN INC(base,  8) END;
      InsBaseIndexScaleDisp(indirect, reg, 8, base, index, scale, disp)

    ELSE
      AddMnem("??")
    END

  ELSE
    AddMnem("unknown")
  END;

  WHILE Hbi < 24 DO Hexbuf[Hbi] := " "; INC(Hbi) END;  Hexbuf[Hbi] := 0X;
  IF comment # "" THEN
    WHILE Mbi < 33 DO Mnembuf[Mbi] := " "; INC(Mbi) END;
    AddMnem("; ");  AddMnem(comment)
  END;
  Mnembuf[Mbi] := 0X;
  w.s(Hexbuf); w.sl(Mnembuf)
END DisassembleInstruction;


PROCEDURE Disassemble*(comment: ARRAY OF CHAR);
BEGIN
  IF disasmpc < PC THEN
    DisassembleInstruction(disasmpc, comment);
    WHILE disasmpc < PC DO
      DisassembleInstruction(disasmpc, "");
    END
  END
END Disassemble;




(* --------------------- Common instruction generation ---------------------- *)

PROCEDURE First(s: SET): INTEGER;  (* Returns first value in SET, or 64 if none *)
VAR f: INTEGER;
BEGIN
  f := 0;
  WHILE (f < 64) & ~(f IN s) DO INC(f) END;
  IF f >= 64 THEN f := -1 END
RETURN f END First;

PROCEDURE IsDataReg*(r: INTEGER): BOOLEAN;
BEGIN ASSERT(r < 16); RETURN (r >= 0) & (r # RSP) END IsDataReg;

PROCEDURE ReserveReg*(r: INTEGER);
BEGIN IF IsDataReg(r) THEN EXCL(Free, r) END END ReserveReg;

PROCEDURE ReleaseReg*(r: INTEGER);
BEGIN IF IsDataReg(r) THEN INCL(Free, r) END END ReleaseReg;

PROCEDURE FirstFreeReg*(): INTEGER;
VAR r: INTEGER;
BEGIN r := First(Free);
  IF r < 0 THEN ORS.Mark("Out of registers") END;
RETURN r END FirstFreeReg;

PROCEDURE ClearRegs*; BEGIN Free := AllFree END ClearRegs;

PROCEDURE FreeOperand*(VAR o: Operand);
BEGIN
  IF ~o.parptr & IsDataReg(o.base) THEN ReleaseReg(o.base)  END;
  IF IsDataReg(o.index)            THEN ReleaseReg(o.index) END;
  ClearOperand(o)
END FreeOperand;


PROCEDURE MakeMemoryOperand*(base, disp, section, size: INTEGER;
                            signed, parptr: BOOLEAN;
                            VAR o: Operand);
BEGIN
  o.direct  := FALSE;
  o.base    := base;
  o.index   := -1;
  o.scale   := 0;
  o.disp    := disp;
  o.section := section;
  o.size    := size;
  o.signed  := signed;
  o.parptr  := parptr
END MakeMemoryOperand;

PROCEDURE MakeConstOperand*(imm: INTEGER; VAR o:Operand);
BEGIN
  o.direct  := TRUE;
  o.base    := -1;
  o.index   := -1;
  o.scale   := 0;
  o.disp    := imm;
  o.section := 0;
  o.size    := 8;
  o.signed  := FALSE;
  o.parptr  := FALSE
END MakeConstOperand;


PROCEDURE AdjustStack*(delta: INTEGER);  (* delta is a count of quadwords *)
BEGIN INC(SPO, delta) END AdjustStack;

PROCEDURE ClearStack*; BEGIN SPO := 0 END ClearStack;


PROCEDURE LoadCondition*(reg, c: INTEGER);  (* Set register to 0/1 based on condition flags *)
BEGIN
  (* setcc  byte-reg*)
  IF reg >= 8 THEN Emit(41H) END;
  Emit(0FH);  Emit(c MOD 16 + 90H);
  Emit(0C0H + reg MOD 8);
  (* movzx qword-reg, byte-reg*)
  IF reg >= 8 THEN Emit(45H) END;
  Emit(0FH);
  Emit(0B6H);
  Emit(0C0H + reg MOD 8 * 8 + reg MOD 8);
END LoadCondition;


PROCEDURE LoadImmediate*(reg, val: INTEGER);
BEGIN
  ASSERT(reg >= 0);
  IF val = 0 THEN                             (* Clear register with 32 bit XOR *)
    EmitPrefices(reg, 4, reg, -1);
    Emit(31H);
    Emit(0C0H + reg MOD 8 * 8 + reg MOD 8)
  ELSIF (val > 0) & (val < 100000000H) THEN   (* Load 32 bit positive value as 32 bit load with zero extension *)
    EmitPrefices(reg, 4, -1, -1);
    Emit(0B8H + reg MOD 8);
    EmitBytes(4, val);
  ELSIF (val < 0) & (val >= -80000000H) THEN  (* Load 32 bit negative value with sign extended move *)
    EmitPrefices(reg, 8, -1, -1);
    Emit(0C7H);
    Emit(0C0H + reg MOD 8);
    EmitBytes(4,val);
  ELSE                                        (* Need full 64 bit literal *)
    EmitPrefices(reg, 8, -1, -1);
    Emit(0B8H + reg MOD 8);
    EmitBytes(8, val);
  END
END LoadImmediate;


PROCEDURE MoveReg*(r1, r2: INTEGER);
BEGIN
  ASSERT(r1 >= 0);  ASSERT(r2 >= 0);
  IF r1 # r2 THEN
    EmitPrefices(r1, 8, r2, -1);
    Emit(8BH);
    EmitModRegReg(r1, r2);
  END
END MoveReg;


PROCEDURE LoadMem*(reg: INTEGER; signed: BOOLEAN; size, base, index, scale, disp: INTEGER);
BEGIN
  IF signed THEN
    EmitPrefices(reg, 8, base, index);
    IF    size = 1 THEN Emit(0FH); Emit(0BEH);   (* movsx  r64, r/m8  *)
    ELSIF size = 2 THEN Emit(0FH); Emit(0BFH);   (* movsx  r64, r/m16 *)
    ELSIF size = 4 THEN Emit(63H)                (* movsxd r64, r/m32 *)
                   ELSE Emit(8BH)                (* mov    r64, r/m64 *)
    END
  ELSE (* Unsigned *)
    IF size = 4 THEN
      EmitPrefices(reg, 4, index, base)
    ELSE
      EmitPrefices(reg, 8, index, base)
    END;
    IF    size = 1 THEN Emit(0FH); Emit(0B6H);   (* movzx  r64, r/m8  *)
    ELSIF size = 2 THEN Emit(0FH); Emit(0B7H);   (* movzx  r64, r/m16 *)
                   ELSE Emit(8BH)                (* mov    r32/64, rm32/64 *)
    END
  END;
  EmitModRegMem(reg, base, index, scale, disp)
END LoadMem;


PROCEDURE LoadAddress(reg: INTEGER; base, index, scale, disp: INTEGER);
BEGIN
  (*
  w.sl("LoadAddress: reg "); w.i(reg);
  w.s(", base "); w.i(base);
  w.s(", index "); w.i(index);
  w.s(", scale "); w.i(scale);
  w.s(", disp "); w.i(disp); w.sl(".");
  *)
  EmitPrefices(reg, 8, index, base);
  Emit(8DH);
  EmitModRegMem(reg, base, index, scale, disp);
  (*Disassemble("LoadAddress")*)
END LoadAddress;


PROCEDURE Depar*(VAR o: Operand);
VAR reg: INTEGER;
BEGIN
  IF o.parptr THEN
    reg := FirstFreeReg();  ReserveReg(reg);
    EmitPrefices(reg, 8, RSP, -1);
    Emit(8BH);
    EmitModRegMem(reg, RSP, -1, -1, o.base);
    o.base   := reg;
    o.parptr := FALSE;
    Disassemble("Depar");
  END
END Depar;

PROCEDURE IsImmediate(o: Operand): BOOLEAN;
BEGIN RETURN o.direct & (o.base < 0) END IsImmediate;


PROCEDURE Load*(VAR o: Operand);
VAR reg: INTEGER;
BEGIN
  Depar(o);
  IF IsDataReg(o.base) THEN reg := o.base ELSE reg := FirstFreeReg() END;
  IF IsImmediate(o) OR ~o.direct THEN
    IF ~o.direct THEN
      LoadMem(reg, o.signed, o.size, o.base, o.index, o.scale, o.disp);
      ReleaseReg(o.base);
      ReleaseReg(o.index)
    ELSE
      LoadImmediate(reg, o.disp)
    END;
    ReserveReg(reg);
    o.direct := TRUE;
    o.base   := reg;
    o.index  := -1;
    o.disp   := 0
  END
END Load;

PROCEDURE LoadAdr*(VAR o: Operand);
VAR reg: INTEGER;
BEGIN
  Depar(o);  ASSERT(~o.direct);
  IF IsDataReg(o.base) THEN reg := o.base ELSE reg := FirstFreeReg() END;
  LoadAddress(reg, o.base, o.index, o.scale, o.disp);
  ReleaseReg(o.base);
  ReleaseReg(o.index);
  ReserveReg(reg);
  o.direct := FALSE;
  o.base   := reg;
  o.index  := -1;
  o.disp   := 0
END LoadAdr;




PROCEDURE StoreRegToMem*(sreg, size, base, index, scale, disp: INTEGER);
BEGIN
  EmitPrefices(sreg, size, base, index);
  IF size = 1 THEN Emit(88H) ELSE Emit(89H) END;
  EmitModRegMem(sreg, base, index, scale, disp)
END StoreRegToMem;

PROCEDURE StoreImmediateToMem*(imm, size, base, index, scale, disp: INTEGER);
VAR reg: INTEGER;
BEGIN
  IF (imm >= -80000000H) & (imm < 80000000H) THEN  (* imm fits 32 bits *)
    EmitPrefices(0, size, base, index);
    IF size = 1 THEN Emit(0C6H) ELSE Emit(0C7H) END;
    EmitModRegMem(0, base, index, scale, disp);
    EmitBytes(Min(size, 4), imm);
  ELSE
    (* 64 bit immediate - load immediate then store reg *)
    reg := FirstFreeReg();
    LoadImmediate(reg, imm);
    StoreRegToMem(reg, size, base, index, scale, disp);
  END
END StoreImmediateToMem;


PROCEDURE StoreReg*(r: INTEGER; VAR o: Operand);  (* Store register to addr in operand *)
BEGIN ASSERT(~o.direct);
  Depar(o);  ASSERT(~o.direct);
  StoreRegToMem(r, o.size, o.base, o.index, o.scale, o.disp)
END StoreReg;

PROCEDURE StoreImmediate*(imm: INTEGER; VAR o: Operand);
BEGIN ASSERT(~o.direct);
  Depar(o);  ASSERT(~o.direct);
  StoreImmediateToMem(imm, o.size, o.base, o.index, o.scale, o.disp)
END StoreImmediate;


(* Push *)

PROCEDURE PushReg*(r: INTEGER);
BEGIN EmitPrefices(r, 4, -1, -1); Emit(50H + r MOD 8) END PushReg;

PROCEDURE PopReg*(r: INTEGER);
BEGIN EmitPrefices(r, 4, -1, -1); Emit(58H + r MOD 8) END PopReg;

PROCEDURE PushImmediate*(i: INTEGER);
VAR reg: INTEGER;
BEGIN
  IF (i >= -80H) & (i < 80H) THEN
    Emit(6AH);  Emit(i)
  ELSIF (i >= -80000000H) & (i < 80000000H) THEN
    Emit(68H);  EmitBytes(4, i)
  ELSE
    reg := FirstFreeReg();
    EmitPrefices(reg, 8, -1, -1); Emit(0B8H + reg MOD 8); EmitBytes(8, i);  (* Load literal *)
    EmitPrefices(reg, 4, -1, -1); Emit(050H + reg MOD 8);                   (* Push reg *)
  END
END PushImmediate;

PROCEDURE Push*(VAR o: Operand);
VAR reg: INTEGER;
BEGIN
  Depar(o);
  IF o.direct THEN
    IF o.base >= 0 THEN
      PushReg(o.base)
    ELSE
      PushImmediate(o.disp)
    END
  ELSE  (* push indirect o *)
    Emit(0FFH);  EmitModRegMem(6, o.base, o.index, o.scale, o.disp)
  END;
  FreeOperand(o)
END Push;


PROCEDURE AluOpRegToReg*(op, r1, r2: INTEGER);
BEGIN
  EmitPrefices(r1, 8, r2, -1);
  Emit(op + 3);
  EmitModRegReg(r1, r2)
END AluOpRegToReg;

PROCEDURE AluOpImmediateToReg*(op, reg, imm: INTEGER);
VAR immreg, incdec: INTEGER;
BEGIN
  IF ((imm = 1) OR (imm = -1)) & ((op = Plus) OR (op = Minus)) THEN
    incdec := 0;
    IF op = Minus THEN incdec := 1 END;
    IF imm = -1 THEN incdec := 1 - incdec END;
    EmitPrefices(-1, 8, reg, -1);
    Emit(0FFH);
    Emit(0C0H + incdec * 8 + reg MOD 8);
  ELSE
    IF (imm < -80000000H) OR (imm >= 80000000H) THEN
      immreg := FirstFreeReg();
      LoadImmediate(immreg, imm);
      AluOpRegToReg(op, immreg, reg)
    ELSE
      EmitPrefices(-1, 8, reg, -1);
      IF (imm >= -80H) & (imm < 80H) THEN
        Emit(83H);  Emit(0C0H + op + reg MOD 8);  Emit(imm)
      ELSE
        Emit(81H);  Emit(0C0H + op + reg MOD 8);  EmitBytes(4, imm)
      END
    END
  END
END AluOpImmediateToReg;

PROCEDURE AluOpRegToMem*(op, reg, size, base, index, scale, disp: INTEGER);
BEGIN
  EmitPrefices(reg, size, base, index);
  Emit(op + 1);
  EmitModRegMem(reg, base, index, scale, disp)
END AluOpRegToMem;

PROCEDURE AluOpImmediateToMem*(op, imm, size, base, index, scale, disp: INTEGER);
VAR immsize, immreg, incdec: INTEGER;
BEGIN
  IF ((imm = 1) OR (imm = -1)) & ((op = Plus) OR (op = Minus)) THEN
    incdec := 0;
    IF op = Minus THEN incdec := 1 END;
    IF imm = -1 THEN incdec := 1 - incdec END;
    EmitPrefices(-1, size, base, index);
    Emit(0FFH);
    EmitModRegMem(incdec, base, index, scale, disp);
  ELSE
    IF (imm < -80000000H) OR (imm >= 80000000H) THEN
      immreg := FirstFreeReg();
      LoadImmediate(immreg, imm);
      AluOpRegToMem(op, immreg, size, base, index, scale, disp)
    ELSE
      EmitPrefices(-1, size, base, index);
      IF size = 1 THEN
        Emit(80H);  immsize := 1
      ELSIF (imm >= -80H) & (imm < 80H) THEN
        Emit(83H);  immsize := 1
      ELSE
        Emit(81H);  immsize := 4
      END;
      EmitModRegMem(op DIV 10H, base, index, scale, disp);
      EmitBytes(immsize, imm)
    END
  END
END AluOpImmediateToMem;

PROCEDURE AluOpMemToReg*(op, reg, size, base, index, scale, disp: INTEGER);
BEGIN
  EmitPrefices(reg, size, base, index);
  IF size = 1 THEN Emit(op + 2) ELSE Emit(op + 3) END;
  EmitModRegMem(reg, base, index, scale, disp)
END AluOpMemToReg;

PROCEDURE AluOp*(op, reg: INTEGER; VAR o: Operand);  (* Assumes reg is dest *)
BEGIN
  Depar(o);
  IF IsImmediate(o) THEN
    AluOpImmediateToReg(op, reg, o.disp)
  ELSIF o.direct THEN
    AluOpRegToReg(op, reg, o.base)
  ELSE
    IF (o.size = 8) OR (op = Cmp) THEN
      AluOpMemToReg(op, reg, o.size, o.base, o.index, o.scale, o.disp)
    ELSE
      Load(o);
      AluOpRegToReg(op, reg, o.base)
    END
  END
END AluOp;


PROCEDURE Call*(VAR o: Operand);
BEGIN
  ASSERT(~o.parptr);
  IF IsImmediate(o) THEN
    Emit(0E8H);
    EmitBytes(4, o.disp - (PC + 4))
  ELSE
    EmitPrefices(-1, 4, o.base, -1);  (* call instruction defaults to 64 bit arg *)
    Emit(0FFH);
    EmitModRegMem(2, o.base, o.index, o.scale, o.disp)
  END
END Call;






PROCEDURE Init*;
BEGIN PC := 0;  disasmpc := 0 END Init;

BEGIN  Init
END X64.
