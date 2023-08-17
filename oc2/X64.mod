MODULE X64;  (* DCWB August 2023; Assembler for X64 *)

(* Originally: N.Wirth, 16.4.2016 / 4.4.2017 / 17.9.2018  Oberon compiler; code generator for RISC*)

IMPORT SYSTEM, ORS, w := Writer;

CONST
  MaxPC = 10000H;
  RSP*  = 4;     (* X64 stack pointer *)


TYPE
  EffectiveAddress* = RECORD
    direct*: BOOLEAN;  (* use register as value rather than part of address *)
    base*:   INTEGER;  (* direct register or base address register *)
    value*:  INTEGER;  (* address offset (indirect only) *)
    index*:  INTEGER;  (* index register (indirect only) *)
    scale*:  INTEGER;  (* index register scale (1, 2, 4, 8) (indirect only) *)
    fixup*:  INTEGER;  (* -1: none, 0: module, 2+: import *)
  END;

VAR
  Program*: ARRAY MaxPC OF BYTE;
  PC*:      INTEGER;  (* Code generation PC *)
  disasmpc: INTEGER;  (* Disassembly PC - trails generation pc *)

  (* Buffers for building one line of Disassembly *)
  Hexbuf:  ARRAY 40 OF CHAR;  Hbi: INTEGER;
  Mnembuf: ARRAY 80 OF CHAR;  Mbi: INTEGER;


(* -------------------------------- Assembly -------------------------------- *)

PROCEDURE IncReg*(VAR r: INTEGER);
BEGIN
  ASSERT(r < 15);
  INC(r);
  IF r = RSP THEN INC(r) END
END IncReg;

PROCEDURE DecReg*(VAR r: INTEGER);
BEGIN
  ASSERT(r > 0);
  DEC(r);
  IF r = RSP THEN DEC(r) END
END DecReg;


PROCEDURE ClearEA*(VAR ea: EffectiveAddress);  (* Sets EA to immediate value 0 *)
BEGIN
  ea.direct := TRUE;
  ea.base   := -1;
  ea.value  := 0;
  ea.index  := -1;
  ea.scale  := 1;
  ea.fixup  := -1;
END ClearEA;


PROCEDURE Emit*(size, value: INTEGER);
BEGIN
  ASSERT(size IN {1, 2, 4, 8});
  IF PC + size <= LEN(Program) THEN
    SYSTEM.COPY(SYSTEM.ADR(value), SYSTEM.ADR(Program) + PC, size);
    INC(PC, size)
  ELSE
    ORS.Mark("program too long")
  END
END Emit;

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






PROCEDURE EmitRex*(W: BOOLEAN; reg, index, base: INTEGER);
VAR rex: INTEGER;
BEGIN  rex := 0;
  IF W         THEN INC(rex, 8) END;
  IF reg   > 7 THEN INC(rex, 4) END;
  IF index > 7 THEN INC(rex, 2) END;
  IF base  > 7 THEN INC(rex)    END;
  IF rex   > 0 THEN Emit(1, 40H + rex) END
END EmitRex;


PROCEDURE EmitModRegRm*(reg, spo: INTEGER; ea: EffectiveAddress);
VAR mode, base, disp: INTEGER;
BEGIN
  reg := reg MOD 8;
  IF ea.direct THEN  ASSERT(ea.base >= 0);  ASSERT(ea.value = 0);  ASSERT(ea.index < 0);
    Emit(1, 0C0H + reg*8 + ea.base MOD 8)
  ELSE
    disp := ea.value;
    IF ea.base < 0 THEN  (* offset only *)
      ASSERT(ea.index < 0);
      Emit(1, reg * 8 + 5);  mode := 2;                    (* 32 bit disp *)
    ELSE
      (* Emit register based address *)
      IF ea.base = RSP THEN INC(disp, 8 * spo) END;  (* Allow for temporaries pushed to stack *)
      base := ea.base MOD 8;
      (* Determine mode and offset size *)
      IF    (disp < -80H) OR (disp >= 80H) THEN mode := 2  (* 32 bit disp *)
      ELSIF (disp = 0)    &  (base  # 5)   THEN mode := 0  (* 0 valued disp omitted exept for RBP *)
      ELSE                                      mode := 1  (* 8 bit signed disp *)
      END;
      IF (ea.index < 0) & (base # 4) THEN
        Emit(1, mode*64 + reg*8 + base)    (* ModRegRm *)
      ELSE
        Emit(1, mode*64 + reg*8 + 4);      (* ModRegRm *)
        IF ea.index < 0 THEN
          Emit(1, 4*8 + base)              (* SIB with no index reg *)
        ELSE
          ASSERT(ea.index # 4);  (* Cannot use rsp as index *)
          Emit(1, ea.scale*64 + ea.index MOD 8 * 8 + base) (* SIB with index reg *)
        END
      END
    END;
    IF    mode = 1 THEN Emit(1, disp)
    ELSIF mode = 2 THEN Emit(4, disp)
    END
  END
END EmitModRegRm;

PROCEDURE EmitPrefices*(reg, size: INTEGER; ea: EffectiveAddress);
VAR rex: INTEGER;
BEGIN  rex := 0;
  (* Emit prefices *)
  IF size     =  2 THEN Emit(1, 66H)       END;
  IF size     >  4 THEN INC(rex, 8)        END;
  IF reg      >= 8 THEN INC(rex, 4)        END;
  IF ea.index >= 8 THEN INC(rex, 2)        END;
  IF ea.base  >= 8 THEN INC(rex, 1)        END;
  IF rex      #  0 THEN Emit(1, 40H + rex) END;
END EmitPrefices;


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
      IF scale > 1 THEN AddMnem("*"); InsDec(scale, Mnembuf, Mbi) END
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
    IF regsize < 64 THEN GetUnsigned(regsize, pc, disp) ELSE GetUnsigned(32, pc, disp) END;
    InsHex(disp, Mnembuf, Mbi); AddMnem("H")

  ELSIF opcode = 0C3H THEN

    AddMnem("ret")

  ELSIF (opcode = 0C6H) OR (opcode = 0C7H) THEN (* Move immediate to modregrm *)

    IF opcode = 0C6H THEN regsize := 8 END;
    AddMnem("mov    ");
    DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect);
    ASSERT(indirect);
    InsBaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp);
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
    InsBaseIndexScaleDisp(indirect, -1, regsize, base, index, scale, disp)

  ELSIF opcode = 0FFH THEN  (* Group 5 extensions to primary opcode map *)

    DisModRegRm(pc, opcode, base, index, disp, scale, regsize, indirect);
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
      InsBaseIndexScaleDisp(indirect, reg, 8, base, index, scale, disp)

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


PROCEDURE Init*;
BEGIN PC := 0;  disasmpc := 0 END Init;

BEGIN  Init
END X64.
