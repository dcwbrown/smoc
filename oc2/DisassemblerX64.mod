MODULE DisassemblerX64;  (* DCWB July 2023 *)

IMPORT SYSTEM, w := Writer;

VAR
  (* Buffers for building one line of siddasembly *)
  Hexbuf:  ARRAY 40 OF CHAR;  Hbi: INTEGER;
  Mnembuf: ARRAY 80 OF CHAR;  Mbi: INTEGER;


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

PROCEDURE GetSigned(size: INTEGER; Program: ARRAY OF BYTE; VAR pc, val: INTEGER);
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

PROCEDURE GetUnsigned(size: INTEGER; Program: ARRAY OF BYTE; VAR pc, val: INTEGER);
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

PROCEDURE DisSIB(Program: ARRAY OF BYTE; VAR pc, base, index, scale: INTEGER);  (* Returns X64 reg nums in range 0-7 *)
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

PROCEDURE DisModRegRm(Program: ARRAY OF BYTE; VAR pc, reg, base, index, disp, scale: INTEGER; size: INTEGER; VAR indirect: BOOLEAN);
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
      DisSIB(Program, pc, base, index, scale);
    ELSIF rm = 5 THEN (* disp32 *)
      GetSigned(32, Program, pc, disp)
    ELSE base := rm
    END
  ELSIF mode = 1 THEN
    IF rm = 4 THEN (* sib + disp8 *)
      DisSIB(Program, pc, base, index, scale)
    ELSE base := rm
    END;
    GetSigned(8, Program, pc, disp)
  ELSIF mode = 2 THEN
    IF rm = 4 THEN (* sib + disp32 *)
      DisSIB(Program, pc, base, index, scale)
    ELSE base := rm
    END;
    GetSigned(32, Program, pc, disp)
  ELSE  (* Mode = 3 *)
    IF (size = 8) & (rm >= 4) THEN (* ah/bh/ch/dh *)
      base := 16 + rm-4
    ELSE
      base := rm
    END
  END
END DisModRegRm;

PROCEDURE Disassemble*(Program: ARRAY OF BYTE; VAR pc: INTEGER; comment: ARRAY OF CHAR);
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
      IF regsize < 64 THEN GetSigned(regsize, Program, pc, disp) ELSE GetSigned(32, Program, pc, disp) END;
    ELSE (* modregrm *)
      DisModRegRm(Program, pc, reg, base, index, disp, scale, regsize, indirect);
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
    DisModRegRm(Program, pc, opcode, base, index, disp, scale, regsize, indirect);
    InsAluOp(opcode);
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    IF indirect THEN
      InsBaseIndexScaleDisp(base, index, scale, disp)
    ELSE
      InsReg(regsize, base);
    END;
    AddMnem(", ");
    GetSigned(dispsize, Program, pc, disp);  InsHex(disp, Mnembuf, Mbi); AddMnem("H")

  ELSIF (opcode >= 88H) & (opcode <= 8BH) THEN (* mov *)

    AddMnem("mov    ");
    IF ~ODD(opcode) THEN regsize := 8 END;
    tofirst := opcode >= 8AH;
    DisModRegRm(Program, pc, reg, base, index, disp, scale, regsize, indirect);
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
    DisModRegRm(Program, pc, reg, base, index, disp, scale, regsize, indirect);
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
    IF regsize < 64 THEN GetUnsigned(regsize, Program, pc, disp) ELSE GetUnsigned(32, Program, pc, disp) END;
    InsHex(disp, Mnembuf, Mbi); AddMnem("H")

  ELSIF (opcode = 0C6H) OR (opcode = 0C7H) THEN (* Move immediate to modregrm *)

    IF opcode = 0C6H THEN regsize := 8 END;
    AddMnem("mov    ");
    DisModRegRm(Program, pc, reg, base, index, disp, scale, regsize, indirect);
    ASSERT(indirect);
    IF    regsize = 8  THEN AddMnem("byte")   ELSIF regsize = 16 THEN AddMnem("word")
    ELSIF regsize = 32 THEN AddMnem("dword")  ELSIF regsize = 64 THEN AddMnem("qword") END;
    InsBaseIndexScaleDisp(base, index, scale, disp);
    AddMnem(", ");
    IF regsize < 64 THEN GetUnsigned(regsize, Program, pc, disp) ELSE GetUnsigned(32, Program, pc, disp) END;
    InsHex(disp, Mnembuf, Mbi); AddMnem("H")

  ELSIF opcode = 0FFH THEN  (* Group 5 extensions to primary opcode map *)

    DisModRegRm(Program, pc, opcode, base, index, disp, scale, regsize, indirect);
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
      DisModRegRm(Program, pc, reg, base, index, disp, scale, regsize, indirect);
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


(* ---- Initialisation ---- *)

BEGIN
END DisassemblerX64.
