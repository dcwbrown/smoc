MODULE Generator;
IMPORT
  SYSTEM, Files, S := Scanner, B := Base, ObjectX64, w := Writer;


(*    Windows x64 subroutine register conventions
*
*     Arguments    - rcx, rdx, r8, r9, [rsp+$20], [rsp+$28], ...
*     Return value - rax
*     Unsaved      - rax, rcx, rdx, r8,  r9,  r10, r11
*     Saved        - rbx, rsi, rdi, rbp, r12, r13, r14, r15
*)


CONST
  MaxInt = 9223372036854775807;
  MinInt = -MaxInt-1;

  MaxSize       = 80000000H;  (* 2 GB limit *)
  MaxLocBlkSize = 100000H;    (* 1 MB limit *)

  rAX = 0;   rCX = 1;   rDX = 2;   rBX = 3;
  rSP = 4;   rBP = 5;   rSI = 6;   rDI = 7;
  r8  = 8;   r9  = 9;   r10 = 10;  r11 = 11;
  r12 = 12;  r13 = 13;  r14 = 14;  r15 = 15;

  ccO  = 0;   ccNO = 1;  ccB  = 2;  ccAE = 3;   ccZ  = 4;   ccNZ = 5;   ccBE = 6;
  ccA  = 7;   ccS  = 8;  ccNS = 9;  ccP  = 10;  ccNP = 11;  ccL  = 12;  ccGE = 13;
  ccLE = 14;  ccG  = 15;

  ccAlways = 16;  ccNever = 17;  ccC = ccB;  ccNC = ccAE;

  (* Opcodes used with EmitRegRm *)
  ADD  = 00H;     ADDd = 02H;  AND = 20H;  ANDd = 22H;  XOR = 30H;  XORd = 32H;
  TEST = 84H;     XCHG = 86H;
  OR_  = 08H;     ORd  = 0AH;  SUB = 28H;  SUBd = 2AH;  CMP = 38H;  CMPd = 3AH;
  MOV  = 88H;     MOVd = 8AH;  LEA = 8DH;
  BT   = 0A30FH;  BTR  = 0B30FH;
  BTS  = 0AB0FH;  IMUL = 0AF0FH;

  (* Opcodes used with EmitRm *)
  POP  = 8FH;   ROR1  = 1D0H;  RORcl = 1D2H;  SHL1  = 4D0H;  SHLcl = 4D2H;
  SHR1 = 5D0H;  SHRcl = 5D2H;  SAR1  = 7D0H;  SARcl = 7D2H;
  NOT  = 2F6H;  NEG   = 3F6H;  IDIVa = 7F7H;  INC_  = 0FEH;  DEC_  = 1FEH;
  CALL = 2FFH;  JMP   = 4FFH;  PUSH  = 6FFH;
  LDMXCSR = 2AE0FH;  STMXCSR = 3AE0FH;

  (* Opcodes used with EmitRmImm *)
  ADDi  = 80H;   ORi  = 180H;    ANDi = 480H;    SUBi = 580H;    XORi = 680H;  CMPi = 780H;
  RORi  = 1C0H;  SHLi = 4C0H;    SHRi = 5C0H;    SARi = 7C0H;    MOVi = 0C6H;
  TESTi = 76H;   BTi  = 4BA0FH;  BTSi = 5BA0FH;  BTRi = 6BA0FH;  BTCi = 7BA0FH;
  IMULi = 69H (* Special case *);

  (* Opcodes used with EmitBare *)
  CQO   = 9948H;  LEAVE = 0C9H;    RET   = 0C3H;  INT3  = 0CCH;  UD2 = 0B0FH;
  CMPSB = 0A6H;   CMPSW = 0A766H;  CMPSD = 0A7H;  CMPSQ = 0A748H;
  LODSB = 0ACH;   LODSW = 0AD66H;  LODSD = 0ADH;  LODSQ = 0AD48H;
  STOSB = 0AAH;   STOSW = 0AB66H;  STOSD = 0ABH;  STOSQ = 0AB48H;
  PAUSE = 90F3H;

  (* REP instructions *)
  MOVSrep = 0A4H;

  (* Opcodes used with EmitXmmRm *)
  SseMOVD  = 6E0F66H;  SseMOVDd = 7E0F66H;
  MOVSS    = 100FF3H;  MOVSSd   = 110FF3H;  MOVSD  = 100FF2H;  MOVSDd = 110FF2H;
  ADDSD    = 580FF2H;  MULSD    = 590FF2H;  SUBSD  = 5C0FF2H;  DIVSD  = 5E0FF2H;
  ADDSS    = 580FF3H;  MULSS    = 590FF3H;  SUBSS  = 5C0FF3H;  DIVSS  = 5E0FF3H;
  ADDPS    = 580F00H;  MULPS    = 590F00H;  SUBPS  = 5C0F00H;  DIVPS  = 5E0F00H;
  ANDPS    = 540F00H;  ANDNPS   = 550F00H;  ORPS   = 560F00H;  XORPS  = 570F00H;
  ANDPD    = 540F66H;  ANDNPD   = 550F66H;  ORPD   = 560F66H;  XORPD  = 570F66H;
  MOVAPS   = 280F00H;  MOVAPSd  = 290F00H;  COMISS = 2F0F00H;  COMISD = 2F0F66H;
  CVTSS2SI = 2D0FF3H;  CVTSI2SS = 2A0FF3H;
  CVTSD2SI = 2D0FF2H;  CVTSI2SD = 2A0FF2H;

  (* Item mode *)
  mReg  = 0;  mXReg = 1;  mImm  = 2;  mRegI = 3;   mIP = 4;  mSP = 5;  mBP = 6;
  mCond = 7;  mProc = 8;  mType = 9;  mBX   = 10;  mNothing = 11;

  (* Trap code *)
  arrayTrap   = 1;
  typeTrap    = 2;
  stringTrap  = 3;
  nilTrap     = 4;
  nilProcTrap = 5;
  divideTrap  = 6;
  assertTrap  = 7;
  GetTrap     = 9;  (* SYSTEM.GET access violation *)
  PutTrap     = 10; (* SYSTEM.PUT access violation *)

TYPE
  Proc = B.Proc;
  Node = B.Node;

  Item = RECORD
    mode, op, r, rm: BYTE;
    ref, par:        BOOLEAN;
    type:            B.Type;
    a, b, c, strlen: INTEGER;
    aLink, bLink:    Node;
    obj:             B.Object
  END;

  MakeItemState = RECORD
    avoid,   xAvoid:   SET;
    bestReg, bestXReg: BYTE
  END;

VAR
  (* forward decl *)
  MakeItem0: PROCEDURE(VAR x: Item;  obj: B.Object);

  code: ARRAY 80000H OF BYTE;
  pc*, stack: INTEGER;

  sourcePos: INTEGER; (* Source position as 20/line, 10/column *)

  pass, varSize*, staticSize*: INTEGER;
  baseOffset: INTEGER;  (* Base address of initialsed data section *)

  procList, curProc: B.ProcList;
  modInitProc:       Proc;

  mem: RECORD mod, rm, bas, idx, scl, disp: INTEGER END;

  allocReg, allocXReg: SET;
  MkItmStat: MakeItemState;  (* State for MakeItem procedures in Pass 2 *)

  (* Static data address*)
  (* Win32 specifics *)
  ExitProcess,
  MessageBoxA:  INTEGER;

   (* others *)
  adrOfNEW, modPtrTable, adrOfPtrTable, adrOfStackPtrList: INTEGER;

  debug: Files.File;
  rider: Files.Rider;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE log2(n: INTEGER): INTEGER;
VAR e: INTEGER;
BEGIN e := 0;
  IF n > 1 THEN
    WHILE n > 1 DO
      IF ODD(n) THEN e := -1;  n := 0 ELSE INC (e);  n := n DIV 2 END
    END
  ELSIF n # 1 THEN e := -1
  END;
  RETURN e
END log2;

PROCEDURE IntToSet(n: INTEGER): SET;
  RETURN SYSTEM.VAL(SET, n)
END IntToSet;

PROCEDURE SmallConst(n: INTEGER): BOOLEAN;
  RETURN (n >= -80000000H) & (n < 80000000H)
END SmallConst;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Put(n, v: INTEGER);
BEGIN
  IF pass > 2 THEN
    ASSERT(pc+n <= LEN(code));
    SYSTEM.COPY(SYSTEM.ADR(v), SYSTEM.ADR(code)+pc, n)
  END;
  INC(pc, n)
END Put;

(* -------------------------------------------------------------------------- *)
(* Machine code emitter *)

PROCEDURE EmitREX(reg, rsize: INTEGER);
CONST W = 8;  R = 4;  X = 2;  B = 1;
VAR rex: INTEGER;
BEGIN
  rex := 40H;
  IF rsize = 8 THEN rex := rex + W END;
  IF reg >= r8 THEN rex := rex + R END;
  IF (mem.rm >= r8)
  OR (mem.mod # 3) & (mem.rm = rSP) & (mem.bas >= r8)
  THEN rex := rex + B
  END;
  IF (mem.mod # 3) & (mem.rm = rSP) & (mem.idx >= r8)
  THEN rex := rex + X
  END;
  IF (rex # 40H)
  OR (rsize = 1) & (   (reg IN {rSP..rDI})
                    OR (mem.mod = 3) & (mem.rm IN {rSP..rDI}))
  THEN Put(1, rex)
  END
END EmitREX;

PROCEDURE Emit16bitPrefix(rsize: INTEGER);
BEGIN IF rsize = 2 THEN Put(1, 66H) END
END Emit16bitPrefix;

PROCEDURE HandleMultibytesOpcode(VAR op: INTEGER);
BEGIN
  IF op MOD 256 = 0FH THEN
    Put(1, 0FH);  op := op DIV 256;
    IF (op MOD 256 = 38H) OR (op MOD 256 = 3AH) THEN
      Put(1, op);  op := op DIV 256
    END
  END
END HandleMultibytesOpcode;

PROCEDURE EmitModRM(reg: INTEGER);
BEGIN
  Put(1, mem.mod * 64 + reg MOD 8 * 8 + mem.rm MOD 8);
  IF mem.mod # 3 THEN
    IF mem.rm IN {rSP, r12} THEN
      Put(1, mem.scl * 64 + mem.idx MOD 8 * 8 + mem.bas MOD 8)
    END;
    IF (mem.mod = 0) & (mem.rm IN {rBP, r13})
    OR (mem.mod = 0) & (mem.rm IN {rSP, r12})
      & (mem.bas IN {rBP, r13})
    OR (mem.mod = 2) THEN Put(4, mem.disp)
    ELSIF mem.mod = 1 THEN Put(1, mem.disp)
    END
  END
END EmitModRM;

(* -------------------------------------------------------------------------- *)

PROCEDURE EmitRegRm(op, reg, rsize: INTEGER);
  CONST w = 1;
VAR org: INTEGER;
BEGIN
  Emit16bitPrefix(rsize);  EmitREX(reg, rsize);
  org := op;  HandleMultibytesOpcode(op);

  IF (rsize > 1) & (org < LEA) THEN op := op + w END;
  Put(1, op);  EmitModRM(reg)
END EmitRegRm;

PROCEDURE EmitRm(op, rsize: INTEGER);
  CONST w = 1;
VAR op3bits, org: INTEGER;
BEGIN
  Emit16bitPrefix(rsize);  EmitREX(0, rsize);
  org := op;  HandleMultibytesOpcode(op);

  op3bits := op DIV 256;  op := op MOD 256;
  IF (rsize > 1) & ~ODD(op) & (org # LDMXCSR) & (org # STMXCSR)
  THEN op := op + w
  END;
  Put(1, op);  EmitModRM(op3bits)
END EmitRm;

PROCEDURE EmitRmImm(op, rsize, imm: INTEGER);
  CONST w = 1;  s = 2;
VAR op3bits: INTEGER;
BEGIN
  Emit16bitPrefix(rsize);
  IF op MOD 256 # IMULi THEN EmitREX(0, rsize)
  ELSE EmitREX(op DIV 256, rsize)
  END;
  HandleMultibytesOpcode(op);

  op3bits := op DIV 256;  op := op MOD 256;
  IF rsize > 1 THEN
    IF (op = 0C0H) OR (op = 0BAH) THEN rsize := 1
    ELSIF (imm >= -128) & (imm <= 127) & (op = 80H) THEN
      op := op + s;  rsize := 1
    END;
    IF ~ODD(op) & (op # 0BAH) THEN op := op + w END
  END;
  Put(1, op);  EmitModRM(op3bits);

  IF rsize = 1 THEN Put(1, imm)
  ELSIF rsize = 2 THEN Put(2, imm) ELSE Put(4, imm)
  END
END EmitRmImm;

PROCEDURE EmitBare(op: INTEGER);
BEGIN WHILE op > 0 DO Put(1, op);  op := op DIV 256 END
END EmitBare;

PROCEDURE EmitXmmRm(op, xreg, rsize: INTEGER);
VAR prefix: INTEGER;
BEGIN
  prefix := op MOD 256;  op := op DIV 256;
  IF prefix # 0 THEN Put(1, prefix) END;
  EmitREX(xreg, rsize);  HandleMultibytesOpcode(op);
  Put(1, op MOD 256);  EmitModRM(xreg)
END EmitXmmRm;

PROCEDURE EmitMOVZX(reg, rmsize: INTEGER);
VAR rsize, op: INTEGER;
BEGIN rsize := 4;  op := 0B6H;
  IF rmsize = 1 THEN
    IF (mem.mod = 3) & (mem.rm IN {rSP..rDI})
    THEN rsize := 8
    END
  ELSIF rmsize = 2 THEN INC(op)
  ELSE ASSERT(FALSE)
  END;
  EmitREX(reg, rsize);  Put(1, 0FH);  Put(1, op);  EmitModRM(reg)
END EmitMOVZX;

PROCEDURE EmitMOVSX(reg, rmsize: INTEGER);
VAR op: INTEGER;
BEGIN
  IF rmsize = 1 THEN op := 0BE0FH
  ELSIF rmsize = 2 THEN op := 0BF0FH
  ELSIF rmsize = 4 THEN op := 63H
  ELSE ASSERT(FALSE)
  END;
  EmitREX(reg, 8);  HandleMultibytesOpcode(op);
  Put(1, op);  EmitModRM(reg)
END EmitMOVSX;

PROCEDURE EmitCMPXCHG(reg, rsize: INTEGER);
BEGIN
  Put(1, 0F0H);  (* LOCK prefix *)
  Emit16bitPrefix(rsize);  EmitREX(reg, rsize);
  Put(1, 0FH);  IF rsize > 1 THEN Put(1, 0B1H) ELSE Put(1, 0B0H) END;
  EmitModRM(reg)
END EmitCMPXCHG;

(* -------------------------------------------------------------------------- *)

PROCEDURE SetRm_reg(reg: INTEGER);
BEGIN mem.rm := reg;  mem.mod := 3
END SetRm_reg;

PROCEDURE EmitRmReg(op, rsize, regs: INTEGER);
BEGIN SetRm_reg(regs); EmitRm(op, rsize) END EmitRmReg;

PROCEDURE EmitRegRm_reg(op, regd, rsize, regs: INTEGER);
BEGIN SetRm_reg(regs); EmitRegRm(op, regd, rsize) END EmitRegRm_reg;


PROCEDURE SetRm_regI(reg, disp: INTEGER);
BEGIN
  mem.rm := reg;  mem.disp := disp;
  IF (disp >= -128) & (disp <= 127) THEN
    IF (disp = 0) & ~(reg IN {rBP, r13})
    THEN mem.mod := 0 ELSE mem.mod := 1
    END
  ELSE mem.mod := 2
  END;
  IF reg IN {rSP, r12} THEN
    mem.bas := rSP;  mem.idx := rSP;  mem.scl := 0
  END
END SetRm_regI;

PROCEDURE EmitRmRegI(op, rsize, regs, disp: INTEGER);
BEGIN SetRm_regI(regs, disp); EmitRm(op, rsize) END EmitRmRegI;

PROCEDURE EmitRmImmRegI(op, rsize, imm, regs, disp: INTEGER);
BEGIN SetRm_regI(regs, disp); EmitRmImm(op, rsize, imm) END EmitRmImmRegI;

PROCEDURE EmitRegRmRegI(op, regd, rsize, regs, disp: INTEGER);
BEGIN SetRm_regI(regs, disp); EmitRegRm(op, regd, rsize) END EmitRegRmRegI;

PROCEDURE EmitXmmRmRegI(op, xreg, rsize, regs, disp: INTEGER);
BEGIN SetRm_regI(regs, disp); EmitXmmRm(op, xreg, rsize) END EmitXmmRmRegI;


PROCEDURE SetRm_RIP(disp: INTEGER);
BEGIN mem.rm := rBP;  mem.disp := disp;  mem.mod := 0
END SetRm_RIP;

PROCEDURE EmitRegRmRip(op, regd, rsize, disp: INTEGER);
BEGIN SetRm_RIP(disp); EmitRegRm(op, regd, rsize) END EmitRegRmRip;



PROCEDURE SetRm_regX(reg, idx, scl, disp: INTEGER);
BEGIN
  mem.rm := rSP;  mem.disp := disp;  ASSERT(idx # rSP);
  mem.bas := reg;  mem.idx := idx;  mem.scl := scl;
  IF (disp >= -128) & (disp <= 127) THEN
    IF (disp = 0) & ~(reg IN {rBP, r13})
    THEN mem.mod := 0 ELSE mem.mod := 1
    END
  ELSE mem.mod := 2
  END
END SetRm_regX;

PROCEDURE EmitRegRmRegX(op, regd, rsize, regs, idx, scl, disp: INTEGER);
BEGIN SetRm_regX(regs, idx, scl, disp); EmitRegRm(op, regd, rsize) END EmitRegRmRegX;



PROCEDURE SetRmOperand(x: Item);
BEGIN
  IF x.mode = mSP THEN
    mem.rm := rSP;  mem.bas := rSP;  mem.idx := rSP;
    mem.scl := 0;  mem.disp := x.a;
    IF (x.a >= -128) & (x.a <= 127) THEN mem.mod := 1 ELSE mem.mod := 2 END
  ELSIF x.mode = mBP THEN
    mem.rm := rBP;  mem.disp := x.a;
    IF (x.a >= -128) & (x.a <= 127) THEN mem.mod := 1 ELSE mem.mod := 2 END
  ELSIF x.mode = mIP THEN mem.rm := rBP;  mem.disp := x.a;  mem.mod := 0
  ELSIF x.mode = mBX THEN mem.rm := rBX;  mem.disp := x.a;
    IF x.a = 0 THEN mem.mod := 0
    ELSIF (x.a >= -128) & (x.a <= 127) THEN mem.mod := 1
    ELSE mem.mod := 2
    END
  ELSIF x.mode = mRegI THEN SetRm_regI(x.r, x.a)
  ELSIF (x.mode = mReg) OR (x.mode = mXReg) THEN SetRm_reg(x.r)
  ELSIF x.mode = mProc THEN mem.rm := rBX;  mem.disp := x.a;
    IF x.a = 0 THEN mem.mod := 0
    ELSIF (x.a >= -128) & (x.a <= 127) THEN mem.mod := 1
    ELSE mem.mod := 2
    END
  ELSE ASSERT(FALSE)
  END
END SetRmOperand;

(* -------------------------------------------------------------------------- *)

PROCEDURE EmitRR(op, reg, rsize, rm: INTEGER);
BEGIN SetRm_reg(rm);  EmitRegRm(op, reg, rsize)
END EmitRR;

PROCEDURE EmitRI(op, rm, rsize, imm: INTEGER);
BEGIN
  SetRm_reg(rm);  IF op = IMULi THEN op := op + rm * 256 END;
  EmitRmImm(op, rsize, imm)
END EmitRI;

PROCEDURE EmitR(op, rm, rsize: INTEGER);
BEGIN SetRm_reg(rm);  EmitRm(op, rsize)
END EmitR;

(* -------------------------------------------------------------------------- *)

PROCEDURE MoveRI(rm, rsize, imm: INTEGER);
  CONST w = 8;
VAR op: INTEGER;
BEGIN
  SetRm_reg(rm);  Emit16bitPrefix(rsize);
  EmitREX(0, rsize);  op := 0B0H + rm MOD 8;
  IF rsize > 1 THEN op := op + w END;  Put(1, op);
  IF rsize = 1 THEN Put(1, imm) ELSIF rsize = 2 THEN Put(2, imm)
  ELSIF rsize = 4 THEN Put(4, imm) ELSE Put(8, imm)
  END
END MoveRI;

PROCEDURE CLR(reg: BYTE);
BEGIN EmitRR(XOR, reg, 4, reg)
END CLR;

PROCEDURE PushR(rm: INTEGER);
BEGIN SetRm_reg(rm);  EmitREX(0, 4);  Put(1, 50H + rm MOD 8)
END PushR;

PROCEDURE PopR(rm: INTEGER);
BEGIN SetRm_reg(rm);  EmitREX(0, 4);  Put(1, 58H + rm MOD 8)
END PopR;

PROCEDURE Jmp4(disp: INTEGER);
BEGIN Put(1, 0E9H);  Put(4, disp)
END Jmp4;

PROCEDURE Jmp1(disp: INTEGER);
BEGIN Put(1, 0EBH);  Put(1, disp)
END Jmp1;

PROCEDURE Jcc4(cond, disp: INTEGER);
BEGIN Put(1, 0FH);  Put(1, 80H + cond);  Put(4, disp)
END Jcc4;

PROCEDURE Jcc1(cond, disp: INTEGER);
BEGIN Put(1, 70H + cond);  Put(1, disp)
END Jcc1;

PROCEDURE CallNear(disp: INTEGER);
BEGIN Put(1, 0E8H);  Put(4, disp)
END CallNear;

PROCEDURE SetccRm(cond: INTEGER);
BEGIN EmitREX(0, 1);  Put(1, 0FH);  Put(1, 90H + cond);  EmitModRM(0)
END SetccRm;

PROCEDURE EmitRep(op, rsize, z: INTEGER);
  CONST w = 1;
BEGIN
  Put(1, 0F2H + z);  (* REP prefix *)
  Emit16bitPrefix(rsize);  EmitREX(0, rsize);
  IF (rsize > 1) & ~ODD(op) THEN op := op + w END;
  Put(1, op)
END EmitRep;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Pass 1 *)

PROCEDURE CheckTypeSize(VAR sz: INTEGER);
BEGIN
  IF sz > MaxSize THEN S.Mark("type too big");  sz := 8 END
END CheckTypeSize;

PROCEDURE SetTypeSize*(tp: B.Type);
VAR size, align, falg: INTEGER;
    ident: B.Ident;  ftype, btype: B.Type;
BEGIN
  IF tp.size = 0 THEN btype := tp.base;
    IF tp.form = B.tPtr THEN tp.size := 8;  tp.align := 8
    ELSIF tp.form = B.tProc THEN
      tp.size := 8;  tp.align := 8;  ident := tp.fields;  size := 0;
      WHILE ident # NIL DO
        ftype := ident.obj.type;  ident.obj(B.Var).adr := size + 16;
        IF B.IsOpenArray(ftype) & ~ftype.notag
        OR (ftype.form = B.tRec) & (ident.obj(B.Par).varpar)
        THEN INC(size, 16) ELSE INC(size, 8)
        END;
        ident := ident.next
      END;
      tp.parblksize := size
    ELSIF (tp.form = B.tArray) & (tp.len >= 0) THEN
      SetTypeSize(btype);  tp.align := btype.align;
      tp.size := btype.size * tp.len;  CheckTypeSize(tp.size)
    ELSIF tp.form = B.tRec THEN
      IF btype # NIL THEN SetTypeSize(btype);
        align := btype.align;  size := btype.size0
      ELSE size := 0;  align := 0
      END;
      ident := tp.fields;
      WHILE ident # NIL DO
        ftype := ident.obj.type;  SetTypeSize(ftype);
        IF ftype.align > align THEN align := ftype.align END;
        IF ~tp.union THEN
          B.Align(size, ftype.align);  ident.obj(B.Field).off := size;
          INC(size, ftype.size);  CheckTypeSize(size);
        ELSE
          ident.obj(B.Field).off := 0;
          IF ftype.size > size THEN size := ftype.size END
        END;
        ident := ident.next
      END;
      tp.size0 := size;  B.Align(size, align);
      CheckTypeSize(size);  tp.size := size;  tp.align := align
    ELSE ASSERT(FALSE)
    END
  END
END SetTypeSize;

PROCEDURE SetGlobalVarSize*(x: B.Object);
BEGIN
  B.Align(varSize, x.type.align);  INC(varSize, x.type.size);
  x(B.Var).adr := -varSize;
  IF varSize > MaxSize THEN varSize := 8;
    S.Mark("global var size limit reached")
  END
END SetGlobalVarSize;

PROCEDURE SetProcVarSize*(proc: B.Proc;  x: B.Object);
VAR size: INTEGER;
BEGIN size := proc.locblksize;
  B.Align(size, x.type.align);
  INC(size, x.type.size);
  x(B.Var).adr    := -size;
  proc.locblksize := size;
  IF size > MaxLocBlkSize THEN proc.locblksize := 8;
    S.Mark("local var size limit reached")
  END
END SetProcVarSize;

PROCEDURE AllocImport*(x: B.Object;  module: B.Module);
VAR p: B.Ident;
BEGIN
  NEW(p);  p.obj := x;  p.next := module.impList;
  module.impList := p;
  IF    x IS B.Var        THEN x(B.Var).adr  := staticSize
  ELSIF x IS B.Proc       THEN x(B.Proc).adr := staticSize
  ELSIF x.class = B.cType THEN x.type.adr    := staticSize;
    ASSERT(x.type.form = B.tRec)
  END;
  INC(staticSize, 8);
END AllocImport;

PROCEDURE AllocStaticData;
VAR str:    B.StrList;
    rec:    B.TypeList;
    tdSize: INTEGER;
    i, l:   INTEGER;
BEGIN
  (* Allocate 8 bit literal strings *)
  str := B.strList;
  WHILE str # NIL DO
    str.obj.adr := staticSize;  INC(staticSize, str.obj.len);
    str := str.next
  END;

  (* Allocate ptrTable *)
  rec := B.recList;  B.Align(staticSize, 16);
  WHILE rec # NIL DO
    IF rec.type.mod = NIL THEN
      tdSize := (24 + 8*(B.MaxExt + rec.type.nTraced)) DIV 16 * 16;
      rec.type.adr := staticSize;  INC(staticSize, tdSize)
    END ;
    rec := rec.next
  END;

  IF staticSize + varSize > MaxSize THEN
    S.Mark("static variables size too big");  ASSERT(FALSE)
  END
END AllocStaticData;


PROCEDURE ScanNode(node: B.Node);
VAR left, right: B.Object;
    fpar: B.Ident;  e: INTEGER;

  PROCEDURE ScanPar(node: B.Node;  fpar: B.Ident;  n: INTEGER);
  VAR vpar, open: BOOLEAN;
      i: INTEGER;  ftype: B.Type;
  BEGIN (* ScanPar *)
    ftype := fpar.obj.type;
    IF node.left IS B.Node THEN
      ScanNode(node.left(B.Node));
      node.regUsed := node.left(Node).regUsed;
      node.xRegUsed := node.left(Node).xRegUsed
    END;
    open := B.IsOpenArray(ftype) & ~ftype.notag;
    vpar := fpar.obj(B.Par).varpar;
    IF open OR vpar & (ftype.form = B.tRec) THEN i := 2 ELSE i := 1 END;
    WHILE i > 0 DO
      IF (ftype.form # B.tReal) OR vpar THEN
        IF n = 0 THEN INCL(node.regUsed, rCX)
        ELSIF n = 1 THEN INCL(node.regUsed, rDX)
        ELSIF n = 2 THEN INCL(node.regUsed, r8)
        ELSIF n = 3 THEN INCL(node.regUsed, r9)
        END
      ELSE INCL(node.xRegUsed, n)
      END;
      DEC(i);  INC(n)
    END;
    IF node.right # NIL THEN
      ScanPar(node.right(B.Node), fpar.next, n);
      node.regUsed := node.regUsed + node.right(Node).regUsed;
      node.xRegUsed := node.xRegUsed + node.right(Node).xRegUsed
    END
  END ScanPar;

BEGIN (* ScanNode *)
  left := node.left;  right := node.right;
  IF node.op # S.call THEN
    IF (left # NIL) & (left IS B.Node) THEN
      ScanNode(left(B.Node));
      IF node.op # S.semicolon THEN
        node.regUsed := left(Node).regUsed;
        node.xRegUsed := left(Node).xRegUsed
      END
    END;
    IF (right # NIL) & (right IS B.Node) THEN
      ScanNode(right(B.Node));
      IF node.op # S.semicolon THEN
        node.regUsed := right(Node).regUsed;
        node.xRegUsed := right(Node).xRegUsed
      END
    END;
    IF node.op = S.times THEN
      IF (node.type = B.intType) & (right IS B.Const) THEN
        e := log2(right(B.Const).val);
        IF e >= 0 THEN
          right := B.NewConst(B.intType, e);
          node.op := S.sfLSL;  node.right := right
        END
      END
    ELSIF (node.op = S.div) OR (node.op = S.mod) THEN e := -1;
      IF right IS B.Const THEN e := log2(right(B.Const).val) END;
      IF e = -1 THEN node.regUsed := node.regUsed + {rAX, rDX} END
    ELSIF (node.op >= S.sfLSL) & (node.op <= S.sfROR) THEN
      IF ~(right IS B.Const) THEN INCL(node.regUsed, rCX) END
    ELSIF (node.op >= S.eql) & (node.op <= S.geq) THEN
      IF B.IsStr(left.type) THEN
        node.regUsed := node.regUsed + {rSI, rDI}
      END
    ELSIF node.op = S.upto THEN INCL(node.regUsed, rCX)
    ELSIF node.op = S.becomes THEN
      IF left.type.form IN {B.tArray, B.tRec} THEN
        node.regUsed := node.regUsed + {rSI, rDI}
      END
    ELSIF node.op = S.sfCAS THEN INCL(node.regUsed, rAX)
    END
  ELSIF node.op = S.call THEN
    IF left IS B.Node THEN
      ScanNode(left(B.Node));
      node.regUsed := left(Node).regUsed;
      node.xRegUsed := left(Node).xRegUsed
    END;
    node.regUsed := node.regUsed + {0 .. 2, 8 .. 11};
    node.xRegUsed := node.xRegUsed + {0 .. 5};
    IF right # NIL THEN
      fpar := left.type.fields;  ScanPar(right(B.Node), fpar, 0);
      node.regUsed := node.regUsed + right(Node).regUsed;
      node.xRegUsed := node.xRegUsed + right(Node).xRegUsed
    END
  END
END ScanNode;

PROCEDURE ScanProc(proc: B.Proc);
BEGIN
  proc.adr := -1;  proc.fix := -1;
  IF proc.nTraced > 0 THEN
    INC(proc.locblksize, 24);  B.Align(staticSize, 16);
    proc.descAdr := staticSize;  INC(staticSize, (proc.nTraced+1)*8)
  END;
  IF curProc # NIL THEN NEW(curProc.next);  curProc := curProc.next
  ELSIF curProc = NIL THEN NEW(procList);  curProc := procList
  END;
  curProc.obj := proc;
  IF proc.statseq # NIL THEN ScanNode(proc.statseq) END;
  IF proc.return # NIL THEN
    IF proc.return IS B.Node THEN ScanNode(proc.return(B.Node)) END
  END
END ScanProc;

PROCEDURE ScanDeclaration(decl: B.Ident;  lev: INTEGER);
VAR ident: B.Ident;  obj: B.Object;  ptrTableSize: INTEGER;
BEGIN ident := decl;
  IF lev = 0 THEN ptrTableSize := 8 END;
  WHILE ident # NIL DO obj := ident.obj;
    IF obj IS B.Proc THEN
      ScanDeclaration(obj(B.Proc).decl, lev+1);  ScanProc(obj(B.Proc))
    ELSIF (lev = 0) & (obj IS B.Var) & ~(obj IS B.Str) THEN
      IF obj.type.nTraced > 0 THEN
        INC(ptrTableSize, obj.type.nTraced*8)
      END
    END;
    ident := ident.next
  END;
  IF lev = 0 THEN
    B.Align(staticSize, 16);  modPtrTable := staticSize;
    INC(staticSize, ptrTableSize)
  END
END ScanDeclaration;


PROCEDURE NewProc(VAR proc: Proc;  statseq: Node);
BEGIN
  proc := B.NewProc();  proc.statseq := statseq;
  proc.locblksize := 0;  proc.adr := -1;  proc.fix := -1;
  IF curProc # NIL THEN NEW(curProc.next);  curProc := curProc.next
  ELSIF curProc = NIL THEN NEW(procList);  curProc := procList
  END;
  curProc.obj := proc
END NewProc;


PROCEDURE Append(s: ARRAY OF CHAR; VAR d: ARRAY OF CHAR);
VAR si, di: INTEGER;
BEGIN  si:= 0;  di := 0;
  WHILE (di < LEN(d))  &  (d[di] # 0X) DO INC(di) END;
  WHILE (si < LEN(s))  &  (s[si] # 0X)  &  (di < LEN(d)) DO
    d[di] := s[si];  INC(di);  INC(si);
  END;
  IF di < LEN(d) THEN d[di] := 0X ELSE d[LEN(d)-1] := 0X END
END Append;


PROCEDURE Pass1(VAR modinit: B.Node);
VAR str: ARRAY 512 OF CHAR;
BEGIN
  AllocStaticData;
  ScanDeclaration(B.universe.first, 0);

  (* Determine address of initialised data section relative to code section *)
  baseOffset := -staticSize;
  B.Align(baseOffset, 16);
  (*LogAlloc("final static size", 0);*)
  IF modinit # NIL THEN
    ScanNode(modinit(B.Node));  NewProc(modInitProc, modinit)
  END
END Pass1;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Pass 2 *)

PROCEDURE negated(cond: INTEGER): INTEGER;
BEGIN IF ODD(cond) THEN DEC(cond) ELSE INC(cond) END;  RETURN cond
END negated;

PROCEDURE Jump(node, link: Node;  cond: INTEGER);
VAR jmpSz: INTEGER;
BEGIN
  IF pass = 2 THEN
    node.jmpPc := pc;  node.link := link;
    IF cond = ccAlways THEN INC(pc, 5);  node.jmpSz := 5
    ELSIF cond # ccNever THEN INC(pc, 6);  node.jmpSz := 6
    ELSE node.jmpSz := 0
    END
  ELSIF pass = 3 THEN
    node.jmpPc := pc;  node.link := link;  jmpSz := node.jmpSz;
    IF cond = ccAlways THEN
      IF jmpSz = 5 THEN Jmp4(0)
      ELSIF jmpSz = 2 THEN Jmp1(0)
      ELSE ASSERT(jmpSz = 0)
      END
    ELSIF cond # ccNever THEN
      IF jmpSz = 6 THEN Jcc4(cond, 0)
      ELSIF jmpSz = 2 THEN Jcc1(cond, 0)
      ELSE ASSERT(jmpSz = 0)
      END
    END
  ELSE ASSERT(FALSE)
  END
END Jump;

PROCEDURE BJump(dst, cond: INTEGER);
VAR off: INTEGER;
BEGIN off := dst - pc;
  IF cond = ccAlways THEN
    IF off-2 >= -128 THEN Jmp1(off-2) ELSE Jmp4(off-5) END
  ELSIF cond # ccNever THEN
    IF off-2 >= -128 THEN Jcc1(cond, off-2)
    ELSE Jcc4(cond, off-6)
    END
  END
END BJump;

PROCEDURE ValidOff(off, sz: INTEGER): BOOLEAN;
  RETURN (off < 0) & (off >= LSL(-1, sz-1))
  OR (off >= 0) & (off < LSL(1, sz-1))
END ValidOff;

PROCEDURE FixDisp(p, disp, dispSz: INTEGER);
VAR i: INTEGER;
BEGIN
  IF disp < 0 THEN ASSERT(disp >= LSL(-1, dispSz*8-1))
  ELSIF disp >= 0 THEN ASSERT(disp < LSL(1, dispSz*8-1))
  END;  i := 0;
  WHILE i < dispSz DO code[p+i] := ASR(disp, i*8);  INC(i) END
END FixDisp;

PROCEDURE Fixup(jmpPc, dst: INTEGER);
VAR op: BYTE;  off: INTEGER;
BEGIN
  IF pass = 2 THEN (* do nothing *)
  ELSIF pass = 3 THEN op := code[jmpPc];
    IF op = 0E9H (* jmp near *) THEN
      off := dst - (jmpPc + 5);  FixDisp(jmpPc+1, off, 4)
    ELSIF op = 0EBH (* jmp short *) THEN
      off := dst - (jmpPc + 2);  FixDisp(jmpPc+1, off, 1)
    ELSIF op = 0FH THEN (* jcc near *)
      ASSERT(code[jmpPc+1] DIV 10H = 8);
      off := dst - (jmpPc + 6);  FixDisp(jmpPc+2, off, 4)
    ELSIF op DIV 10H = 7 THEN (* jcc short *)
      off := dst - (jmpPc + 2);  FixDisp(jmpPc+1, off, 1)
    ELSE ASSERT(FALSE)
    END
  ELSE ASSERT(FALSE)
  END
END Fixup;

PROCEDURE FixLinkWith(L: Node;  dst: INTEGER);
VAR off: INTEGER;
BEGIN
  WHILE L # NIL DO
    off := dst - (L.jmpPc + L.jmpSz);
    IF pass = 2 THEN
      IF L.jmpSz # 0 THEN
        IF off = 0 THEN L.jmpSz := 0
        ELSIF (off >= -128) & (off < 128) THEN L.jmpSz := 2
        END
      END
    ELSIF pass = 3 THEN
      IF L.jmpSz > 0 THEN Fixup(L.jmpPc, dst) END
    ELSE ASSERT(FALSE)
    END;
    L := L.link
  END
END FixLinkWith;

PROCEDURE FixLink(L: Node);
BEGIN FixLinkWith(L, pc)
END FixLink;

PROCEDURE merged(L0, L1: Node): Node;
VAR L2, L3: Node;
BEGIN
  IF L0 # NIL THEN L3 := L0;
    REPEAT L2 := L3;  L3 := L3.link UNTIL L3 = NIL;
    L2.link := L1;  L1 := L0
  END;
    RETURN L1
END merged;

PROCEDURE SetCond(VAR x: Item;  c: INTEGER);
BEGIN x.mode := mCond;  x.aLink := NIL;  x.bLink := NIL;  x.c := c
END SetCond;

PROCEDURE OpToCc(op: INTEGER): INTEGER;
BEGIN
  IF op = S.eql THEN op := ccZ ELSIF op = S.neq THEN op := ccNZ
  ELSIF op = S.lss THEN op := ccB ELSIF op = S.gtr THEN op := ccA
  ELSIF op = S.leq THEN op := ccBE ELSE op := ccAE
  END;
  RETURN op
END OpToCc;

PROCEDURE IntOpToCc(op: INTEGER): INTEGER;
BEGIN
  IF op = S.eql THEN op := ccZ ELSIF op = S.neq THEN op := ccNZ
  ELSIF op = S.lss THEN op := ccL ELSIF op = S.gtr THEN op := ccG
  ELSIF op = S.leq THEN op := ccLE ELSE op := ccGE
  END;
  RETURN op
END IntOpToCc;

PROCEDURE CallProc(proc: B.Proc);
VAR L, adr: INTEGER;
BEGIN
  IF pass = 2 THEN INC(pc, 5)
  ELSIF pass = 3 THEN adr := proc.adr;
    IF adr >= 0 THEN CallNear(adr-pc-5)
    ELSE CallNear(proc.fix);  proc.fix := pc - 4
    END
  ELSE ASSERT(FALSE)
  END
END CallProc;

PROCEDURE LoadProc(reg: INTEGER;  obj: B.Proc);
VAR adr: INTEGER;
BEGIN
  IF pass = 2 THEN     EmitRegRmRip(LEA, reg, 0, 0);
  ELSIF pass = 3 THEN  EmitRegRmRip(LEA, reg, 8, 0);
    adr := obj.adr;
    IF adr >= 0 THEN FixDisp(pc-4, adr-pc, 4)
    ELSE FixDisp(pc-4, obj.fix, 4);  obj.fix := pc - 4
    END
  ELSE ASSERT(FALSE)
  END
END LoadProc;

PROCEDURE WriteDebug(pc, sourcePos, trapno: INTEGER);
(* debug section entry: 4/trapno, 20/line, 10/column, 30/pc *)
BEGIN
  IF pass = 3 THEN
    Files.WriteInt(rider,    (pc        MOD 40000000H)
                        + LSL(sourcePos MOD 40000000H, 30)
                        + LSL(trapno    MOD 10H,       60))
  END
END WriteDebug;

PROCEDURE Trap(cond, trapno: INTEGER);
VAR L: INTEGER;
BEGIN
  IF ~(cond IN {ccAlways, ccNever}) THEN
    L := pc;  Jcc1(negated(cond), 0);  EmitBare(UD2);
    WriteDebug(pc-2, sourcePos, trapno);  Fixup(L, pc)
  ELSIF cond = ccAlways THEN
    EmitBare(UD2);  WriteDebug(pc-2, sourcePos, trapno)
  END
END Trap;

(* -------------------------------------------------------------------------- *)

PROCEDURE ResetMkItmStat;
BEGIN
  MkItmStat.avoid  := {};  MkItmStat.bestReg  := 255;
  MkItmStat.xAvoid := {};  MkItmStat.bestXReg := 255
END ResetMkItmStat;

PROCEDURE ResetMkItmStat2(VAR oldStat: MakeItemState);
BEGIN oldStat := MkItmStat;
  MkItmStat.avoid  := {};  MkItmStat.bestReg  := 255;
  MkItmStat.xAvoid := {};  MkItmStat.bestXReg := 255
END ResetMkItmStat2;

PROCEDURE SetAlloc(reg: BYTE);
BEGIN INCL(allocReg, reg);  INCL(curProc.obj.usedReg, reg)
END SetAlloc;

PROCEDURE SetAllocX(reg: BYTE);
BEGIN INCL(allocXReg, reg);  INCL(curProc.obj.usedXReg, reg)
END SetAllocX;

PROCEDURE AllocReg(): BYTE;
VAR reg: BYTE;  cantAlloc: SET;
BEGIN
  cantAlloc := MkItmStat.avoid + allocReg + {rSP, rBP, rBX};
  IF (MkItmStat.bestReg = 255) OR (MkItmStat.bestReg IN cantAlloc) THEN
    reg := 0;  WHILE (reg < 3) & (reg IN cantAlloc) DO INC(reg) END;
    IF reg >= 3 THEN reg := 8;
      WHILE (reg < 12) & (reg IN cantAlloc) DO INC(reg) END;
      IF reg >= 12 THEN reg := 6;
        WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
        IF reg >= 16 THEN S.Mark("Reg stack overflow") END
      END
    END
  ELSE reg := MkItmStat.bestReg
  END;
  ASSERT(reg < 16);  SetAlloc(reg);
  RETURN reg
END AllocReg;

PROCEDURE AllocReg2(avoid: SET): BYTE;
VAR reg: BYTE;  cantAlloc: SET;
BEGIN cantAlloc := avoid + allocReg + {rSP, rBP, rBX};
  reg := 0;  WHILE (reg < 3) & (reg IN cantAlloc) DO INC(reg) END;
  IF reg >= 3 THEN reg := 8;
    WHILE (reg < 12) & (reg IN cantAlloc) DO INC(reg) END;
    IF reg >= 12 THEN reg := 6;
      WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
      IF reg >= 16 THEN S.Mark("Reg stack overflow") END
    END
  END;
  ASSERT(reg < 16);  SetAlloc(reg);
  RETURN reg
END AllocReg2;

PROCEDURE SetAvoid(reg: BYTE);
BEGIN INCL(MkItmStat.avoid, reg)
END SetAvoid;

PROCEDURE SetBestReg(reg: BYTE);
BEGIN MkItmStat.bestReg := reg
END SetBestReg;

PROCEDURE AllocXReg(): BYTE;
VAR reg: BYTE;  cantAlloc: SET;
BEGIN
  cantAlloc := MkItmStat.xAvoid + allocXReg;
  IF (MkItmStat.bestXReg = 255) OR (MkItmStat.bestXReg IN cantAlloc) THEN
    reg := 0;  WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
    IF reg >= 16 THEN S.Mark("Reg stack overflow");  ASSERT(FALSE) END
  ELSE reg := MkItmStat.bestXReg
  END;
  SetAllocX(reg);
  RETURN reg
END AllocXReg;

PROCEDURE AllocXReg2(avoid: SET): BYTE;
VAR reg: BYTE;  cantAlloc: SET;
BEGIN cantAlloc := avoid + allocXReg;
  reg := 0;  WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
  IF reg >= 16 THEN S.Mark("Reg stack overflow");  ASSERT(FALSE) END;
  SetAllocX(reg);
  RETURN reg
END AllocXReg2;

PROCEDURE FreeReg(reg: BYTE);
BEGIN EXCL(allocReg, reg)
END FreeReg;

PROCEDURE FreeXReg(reg: BYTE);
BEGIN EXCL(allocXReg, reg)
END FreeXReg;

PROCEDURE FreeReg2(x: Item);
BEGIN
  IF x.mode IN {mReg, mRegI} THEN FreeReg(x.r)
  ELSIF x.mode = mXReg THEN FreeXReg(x.r)
  END
END FreeReg2;

PROCEDURE AllocStack(size: INTEGER): INTEGER;
VAR adr: INTEGER;
BEGIN
  INC(stack, size);  adr := -stack;
  IF stack > curProc.obj.stack THEN curProc.obj.stack := stack END;
  RETURN adr
END AllocStack;

PROCEDURE FreeStack(size: INTEGER);
BEGIN DEC(stack, size)
END FreeStack;

(* -------------------------------------------------------------------------- *)

PROCEDURE RelocReg(VAR reg: BYTE;  newReg: BYTE);
BEGIN
  EmitRR(MOVd, newReg, 8, reg);
  FreeReg(reg);  reg := newReg;  SetAlloc(reg)
END RelocReg;

PROCEDURE RelocXReg(VAR reg: BYTE;  newReg: BYTE);
BEGIN
  SetRm_reg(reg);  EmitXmmRm(MOVSD, newReg, 8);
  FreeXReg(reg);  reg := newReg;  SetAllocX(reg)
END RelocXReg;

PROCEDURE RefToRegI(VAR x: Item);
VAR reg: BYTE;
BEGIN
  IF x.ref & (x.mode # mProc) THEN
    ASSERT(x.mode IN {mSP, mIP, mBP, mBX});
    reg := AllocReg();  SetRmOperand(x);  EmitRegRm(MOVd, reg, 8);
    x.mode := mRegI;  x.r := reg;  x.a := x.b;  x.ref := FALSE
  END
END RefToRegI;

PROCEDURE LoadImm(r: BYTE;  size, imm: INTEGER);
BEGIN
  IF imm = 0 THEN CLR(r)
  ELSIF size <= 4 THEN MoveRI(r, size, imm)
  ELSIF (imm > 0) & (imm < 100000000H) THEN MoveRI(r, 4, imm)
  ELSIF SmallConst(imm) THEN CLR(r);  EmitRI(ADDi, r, 8, imm)
  ELSE MoveRI(r, 8, imm)
  END
END LoadImm;

PROCEDURE LoadToReg0(r: BYTE;  x: Item;  type: B.Type);
BEGIN
  IF x.mode IN {mReg, mRegI, mBX, mBP} THEN
    SetRmOperand(x);
    IF type.size = 8 THEN EmitRegRm(MOVd, r, 8)
    ELSIF type.size = 4 THEN
      IF type = B.int32Type THEN EmitMOVSX(r, 4)
      ELSIF type = B.card32Type THEN EmitRegRm(MOVd, r, 4)
      ELSE ASSERT(FALSE)
      END
    ELSIF type.size = 2 THEN
      IF    type = B.card16Type THEN EmitMOVZX(r, 2)
      ELSIF type = B.int16Type  THEN EmitMOVSX(r, 2)
      ELSE ASSERT(FALSE)
      END
    ELSIF type.size = 1 THEN
      IF (type = B.byteType)
      OR (type = B.charType)
      OR (type = B.boolType)  THEN EmitMOVZX(r, 1)
      ELSIF type = B.int8Type THEN EmitMOVSX(r, 1)
      ELSE ASSERT(FALSE)
      END
    END
  ELSE ASSERT(FALSE)
  END
END LoadToReg0;

PROCEDURE Load(VAR x: Item);
VAR r, r2: BYTE;  size, L, L2: INTEGER;
BEGIN RefToRegI(x);
  IF x.type.form # B.tReal THEN
    IF x.mode # mReg THEN size := x.type.size;
      IF x.mode # mRegI THEN r := AllocReg() ELSE r := x.r END;
      IF x.mode = mImm THEN LoadImm(r, size, x.a)
      ELSIF x.mode IN {mRegI, mBP, mBX} THEN
        IF x.type = B.strType THEN
          ASSERT(x.strlen <= 2);  LoadToReg0(r, x, B.charType)
        ELSE LoadToReg0(r, x, x.type)
        END
      ELSIF x.mode = mProc THEN
        IF ~x.ref THEN LoadProc(r, x.obj(B.Proc))
        ELSE SetRmOperand(x);  EmitRegRm(MOVd, r, 8);  x.ref := FALSE
        END
      ELSIF x.mode = mCond THEN
        L := pc;  Jcc1(negated(x.c), 0);
        FixLink(x.bLink);  LoadImm(r, 4, 1);  L2 := pc;  Jmp1(0);
        FixLink(x.aLink);  Fixup(L, pc);  CLR(r);  Fixup(L2, pc)
      ELSE ASSERT(FALSE)
      END;
      x.mode := mReg;  x.r := r
    END
  ELSIF x.mode # mXReg THEN r := AllocXReg();
    IF x.mode = mImm THEN
      IF x.a = 0 THEN SetRm_reg(r);  EmitXmmRm(XORPS, r, 4)
      ELSE r2 := AllocReg2({});
        LoadImm(r2, x.type.size, x.a);  SetRm_reg(r2);
        EmitXmmRm(SseMOVD, r, x.type.size);  FreeReg(r2)
      END
    ELSE SetRmOperand(x);  EmitXmmRm(MOVSD, r, 4)
    END;
    FreeReg2(x);  x.mode := mXReg;  x.r := r
  END
END Load;

PROCEDURE LoadAdr(VAR x: Item);
VAR r: BYTE;
BEGIN
  RefToRegI(x);  SetRmOperand(x);
  IF x.mode = mRegI THEN r := x.r ELSE r := AllocReg() END;
  IF (x.mode # mRegI) OR (x.a # 0) THEN EmitRegRm(LEA, r, 8) END;
  x.r := r;  x.mode := mReg
END LoadAdr;

PROCEDURE ArrayLen(VAR x: Item;  obj: B.Object);
BEGIN
  IF    obj IS B.Str  THEN x.mode := mImm;  x.a := obj(B.Str).len
  ELSIF B.IsOpenArray(obj.type) THEN MakeItem0(x, obj);  INC(x.a, 8)
  ELSIF B.IsNormalArray(obj.type) THEN x.mode := mImm;  x.a := obj.type.len
  ELSE ASSERT(FALSE)
  END;
  x.type := B.card32Type;  x.ref := FALSE
END ArrayLen;

PROCEDURE SizeOf(VAR x: Item;  obj: B.Object);
VAR size, e: INTEGER;
BEGIN
  IF    obj IS B.Str  THEN x.mode := mImm;  x.a := obj(B.Str).len
  ELSIF B.IsOpenArray(obj.type) THEN size := obj.type.base.size;
    IF size = 0 THEN x.mode := mImm;  x.a := 0
    ELSE ArrayLen(x, obj);  Load(x);  e := log2(size);
      IF e > 0 THEN EmitRI(SHLi, x.r, 8, e)
      ELSIF e < 0 THEN EmitRI(IMULi, x.r, 8, size)
      END
    END
  ELSE x.mode := mImm;  x.a := obj.type.size
  END;
  x.type := B.card32Type;  x.ref := FALSE
END SizeOf;

PROCEDURE TypeTag(VAR x: Item);
BEGIN
  IF x.type.form = B.tPtr THEN
    Load(x);  x.mode := mRegI;
    EmitRR(TEST, x.r, 8, x.r);  Trap(ccZ, nilTrap);
    x.a := -16;
  ELSIF x.mode = mBP THEN x.a := x.a + 8;  x.ref := FALSE
  ELSE ASSERT(FALSE)
  END;
  x.type := B.intType
END TypeTag;

PROCEDURE TypeTag2(VAR tag: Item;  x: Item);
BEGIN tag := x;
  IF (x.type.form = B.tPtr) & (x.mode IN {mReg, mRegI}) THEN
    tag.r := AllocReg();  SetRmOperand(x);
    EmitRegRm(MOVd, tag.r, 8);  tag.mode := mReg
  END;  TypeTag(tag)
END TypeTag2;

PROCEDURE TypeDesc(VAR x: Item;  tp: B.Type);
BEGIN
  IF tp.form = B.tRec THEN x.a := tp.adr ELSE ASSERT(FALSE) END;
  x.mode := mBX;  x.type := B.intType;  x.b := 0;  x.ref := tp.mod # NIL;
  SetAlloc(rBX)
END TypeDesc;

PROCEDURE AvoidUsedBy(obj: B.Object);
VAR node: Node;
BEGIN
  IF (obj # NIL) & (obj IS Node) THEN node := obj(Node);
    IF (node.regUsed # {}) OR (node.xRegUsed # {}) THEN
      MkItmStat.avoid := MkItmStat.avoid + node.regUsed;
      MkItmStat.xAvoid := MkItmStat.xAvoid + node.xRegUsed
    END
  END
END AvoidUsedBy;

PROCEDURE LoadLeftRight2(VAR x, y: Item;  node: Node);
BEGIN
  AvoidUsedBy(node.right);  MakeItem0(x, node.left);  Load(x);
  ResetMkItmStat;  MakeItem0(y, node.right);  Load(y)
END LoadLeftRight2;

PROCEDURE Op2(VAR x: Item;  node: Node);
VAR oldStat: MakeItemState;  y: Item;  op: INTEGER;
BEGIN
  oldStat := MkItmStat;  AvoidUsedBy(node.right);
  MakeItem0(x, node.left);  Load(x);  ResetMkItmStat;
  MakeItem0(y, node.right);  RefToRegI(y);  op := node.op;
  IF x.type.form = B.tInt THEN
    IF (y.mode = mImm) & ~SmallConst(y.a)
    OR (y.type.size < 8) THEN Load(y)
    END;
    IF y.mode IN {mReg, mRegI, mBX, mBP} THEN SetRmOperand(y);
      IF op = S.plus THEN op := ADDd
      ELSIF op = S.minus THEN op := SUBd
      ELSIF op = S.times THEN op := IMUL
      ELSE ASSERT(FALSE)
      END;  EmitRegRm(op, x.r, 8)
    ELSIF y.mode = mImm THEN
      IF op = S.plus THEN op := ADDi
      ELSIF op = S.minus THEN op := SUBi
      ELSIF op = S.times THEN op := IMULi
      ELSE ASSERT(FALSE)
      END;  EmitRI(op, x.r, 8, y.a)
    ELSE ASSERT(FALSE)
    END
  ELSIF x.type = B.setType THEN
    IF op = S.minus THEN op := S.times;
      IF y.mode # mImm THEN Load(y);  EmitR(NOT, y.r, 8)
      ELSE y.a := ORD(-SYSTEM.VAL(SET, y.a))
      END
    END;
    IF (y.mode = mImm) & ~SmallConst(y.a) THEN Load(y) END;
    IF y.mode IN {mReg, mRegI, mBX, mBP} THEN SetRmOperand(y);
      IF op = S.plus THEN op := ORd
      ELSIF op = S.times THEN op := ANDd
      ELSIF op = S.rdiv THEN op := XORd
      ELSE ASSERT(FALSE)
      END;  EmitRegRm(op, x.r, 8)
    ELSIF y.mode = mImm THEN
      IF op = S.plus THEN op := ORi
      ELSIF op = S.times THEN op := ANDi
      ELSIF op = S.rdiv THEN op := XORi
      ELSE ASSERT(FALSE)
      END;  EmitRI(op, x.r, 8, y.a)
    ELSE ASSERT(FALSE)
    END
  ELSIF x.type = B.realType THEN
    IF y.mode = mImm THEN Load(y) END;
    IF y.mode IN {mXReg, mRegI, mBX, mBP} THEN SetRmOperand(y);
      IF op = S.plus THEN op := ADDSD
      ELSIF op = S.minus THEN op := SUBSD
      ELSIF op = S.times THEN op := MULSD
      ELSIF op = S.rdiv THEN op := DIVSD
      ELSE ASSERT(FALSE)
      END;  EmitXmmRm(op, x.r, 8)
    ELSE ASSERT(FALSE)
    END
  ELSE ASSERT(FALSE)
  END;
  FreeReg2(y);  MkItmStat := oldStat
END Op2;

PROCEDURE Negate(VAR x: Item;  node: Node);
VAR r: BYTE;
BEGIN
  MakeItem0(x, node.left);  Load(x);
  IF x.type.form = B.tInt THEN EmitR(NEG, x.r, 8)
  ELSIF x.type = B.setType THEN EmitR(NOT, x.r, 8)
  ELSIF x.type = B.realType THEN r := AllocXReg2({});
    SetRm_reg(r);  EmitXmmRm(XORPS, r, 4);
    SetRm_reg(x.r);  EmitXmmRm(SUBSD, r, 4);
    SetRm_reg(r);  EmitXmmRm(MOVSD, x.r, 4);  FreeXReg(r)
  ELSE ASSERT(FALSE)
  END
END Negate;

PROCEDURE IntDiv(VAR x: Item;  node: Node);
VAR oldStat: MakeItemState;  e, n, L: INTEGER;  r: BYTE;
    right: B.Object;  y: Item;
BEGIN
  e := -1;  right := node.right;
  IF right IS B.Const THEN n := right(B.Const).val;  e := log2(n) END;
  IF (e >= 0) THEN
    MakeItem0(x, node.left);  Load(x);
    IF node.op = S.div THEN
      IF e = 1 THEN EmitR(SAR1, x.r, 8)
      ELSIF e > 1 THEN EmitRI(SARi, x.r, 8, e)
      END
    ELSIF node.op = S.mod THEN
      IF e = 8 THEN LoadToReg0(x.r, x, B.byteType)
      ELSIF e = 16 THEN LoadToReg0(x.r, x, B.card16Type)
      ELSIF e = 32 THEN EmitRR(MOV, x.r, 4, x.r)
      ELSIF ~SmallConst(n-1) THEN
        r := AllocReg();  LoadImm(r, 8, n-1);
        EmitRR(ANDd, x.r, 8, r);  FreeReg(r)
      ELSIF n > 1 THEN EmitRI(ANDi, x.r, 4, n-1)
      ELSE CLR(x.r)
      END
    END
  ELSE
    ResetMkItmStat2(oldStat);  AvoidUsedBy(node.right);
    SetBestReg(rAX);  MakeItem0(x, node.left);  Load(x);
    ResetMkItmStat;  SetAvoid(rAX);  SetAvoid(rDX);
    MakeItem0(y, node.right);  Load(y);
    IF x.r # rAX THEN RelocReg(x.r, rAX) END;
    SetAlloc(rDX);  EmitBare(CQO);
    WriteDebug(pc, node.sourcePos, divideTrap);  EmitR(IDIVa, y.r, 8);
    EmitRR(TEST, rDX, 8, rDX);  L := pc;  Jcc1(ccGE, 0);
    IF node.op = S.div THEN EmitRI(SUBi, rAX, 8, 1)
    ELSE EmitRR(ADDd, rDX, 8, y.r)
    END;
    Fixup(L, pc);

    IF node.op = S.div THEN FreeReg(rDX)
    ELSE FreeReg(rAX);  x.r := rDX
    END;
    FreeReg(y.r);  MkItmStat := oldStat
  END
END IntDiv;

PROCEDURE LoadCond(VAR x: Item;  obj: B.Object);
VAR oldStat: MakeItemState;
BEGIN
  ResetMkItmStat2(oldStat);  MakeItem0(x, obj);
  IF x.mode # mCond THEN
    IF x.mode = mImm THEN SetCond(x, ccNever - x.a)
    ELSE Load(x);  EmitRR(TEST, x.r, 4, x.r);  FreeReg(x.r);  SetCond(x, ccNZ)
    END
  END;
  MkItmStat := oldStat
END LoadCond;

PROCEDURE And(VAR x: Item;  node: Node);
VAR y: Item;
BEGIN
  LoadCond(x, node.left);  Jump(node, x.aLink, negated(x.c));
  FixLink(x.bLink);  LoadCond(y, node.right);
  x.aLink := merged(y.aLink, node);  x.bLink := y.bLink;  x.c := y.c
END And;

PROCEDURE Or(VAR x: Item;  node: Node);
VAR y: Item;
BEGIN
  LoadCond(x, node.left);  Jump(node, x.bLink, x.c);
  FixLink(x.aLink);  LoadCond(y, node.right);
  x.bLink := merged(y.bLink, node);  x.aLink := y.aLink;  x.c := y.c
END Or;

PROCEDURE Not(VAR x: Item;  node: Node);
VAR t: Node;
BEGIN
  LoadCond(x, node.left);  x.c := negated(x.c);
  t := x.aLink;  x.aLink := x.bLink;  x.bLink := t
END Not;

PROCEDURE CompareInt(VAR x: Item;  node: Node);
VAR y: Item;
BEGIN
  IF node.right IS B.Const THEN
    MakeItem0(x, node.left);  RefToRegI(x);
    ASSERT(x.mode IN {mReg, mRegI, mBX, mBP});
    MakeItem0(y, node.right);  IF x.type.size < 8 THEN Load(x) END;
    IF ~SmallConst(y.a) THEN Load(y) END;  SetRmOperand(x);
    IF y.mode = mImm THEN
      IF (x.mode = mReg) & (y.a = 0)
      THEN EmitRegRm(TEST, x.r, 8) ELSE EmitRmImm(CMPi, 8, y.a)
      END
    ELSE EmitRegRm(CMP, y.r, 8)
    END
  ELSE
    AvoidUsedBy(node.right);  MakeItem0(x, node.left);  Load(x);
    ResetMkItmStat;  MakeItem0(y, node.right);  RefToRegI(y);
    IF y.type.size < 8 THEN Load(y) END;
    IF y.mode IN {mReg, mRegI, mBX, mBP} THEN
      SetRmOperand(y);  EmitRegRm(CMPd, x.r, 8)
    ELSE ASSERT(FALSE)
    END
  END;
  FreeReg2(x);  FreeReg2(y);  SetCond(x, IntOpToCc(node.op))
END CompareInt;

PROCEDURE CompareNonInt(VAR x: Item;  node: Node);
VAR y: Item;
BEGIN
  IF node.right IS B.Const THEN
    MakeItem0(x, node.left);  RefToRegI(x);
    ASSERT(x.mode IN {mReg, mRegI, mBX, mBP});
    MakeItem0(y, node.right);  IF ~SmallConst(y.a) THEN Load(y) END;
    SetRmOperand(x);
    IF y.mode = mImm THEN
      IF (x.mode = mReg) & (y.a = 0)
      THEN EmitRegRm(TEST, x.r, x.type.size)
      ELSE EmitRmImm(CMPi, x.type.size, y.a)
      END
    ELSE EmitRegRm(CMP, y.r, x.type.size)
    END
  ELSE
    AvoidUsedBy(node.right);  MakeItem0(x, node.left);  Load(x);
    ResetMkItmStat;  MakeItem0(y, node.right);  RefToRegI(y);
    IF y.mode IN {mReg, mRegI, mBX, mBP} THEN
      SetRmOperand(y);  EmitRegRm(CMPd, x.r, x.type.size)
    ELSE ASSERT(FALSE)
    END
  END;
  FreeReg2(x);  FreeReg2(y);  SetCond(x, OpToCc(node.op))
END CompareNonInt;

PROCEDURE CompareReal(VAR x: Item;  node: Node);
VAR y: Item;
BEGIN
  AvoidUsedBy(node.right);  MakeItem0(x, node.left);  Load(x);
  ResetMkItmStat;  MakeItem0(y, node.right);
  IF y.mode = mImm THEN Load(y) ELSE RefToRegI(y) END;
  IF y.mode IN {mXReg, mRegI, mBX, mBP} THEN
    SetRmOperand(y);  EmitXmmRm(COMISD, x.r, 4)
  ELSE ASSERT(FALSE)
  END;
  FreeXReg(x.r);  FreeReg2(y);  SetCond(x, OpToCc(node.op))
END CompareReal;

PROCEDURE Compare(VAR x: Item;  node: Node);
VAR cx, r, size: BYTE;  first, L: INTEGER;
    y, len: Item;  tp: B.Type;  oldStat: MakeItemState;
BEGIN
  ResetMkItmStat2(oldStat);  tp := node.left.type;
  IF tp.form IN B.typScalar THEN
    IF tp.form = B.tInt THEN CompareInt(x, node)
    ELSIF tp.form # B.tReal THEN CompareNonInt(x, node)
    ELSIF tp = B.realType THEN CompareReal(x, node)
    ELSE ASSERT(FALSE)
    END
  ELSIF B.IsStr(tp) THEN  (* TODO Test thouroughly 8 bit string compare *)
    SetBestReg(rSI);
    AvoidUsedBy(node.right);
    SetAvoid(rDI);
    MakeItem0(x, node.left);      LoadAdr(x);
    ResetMkItmStat;
    SetBestReg(rDI);
    MakeItem0(y, node.right);     LoadAdr(y);
    IF y.r # rDI THEN RelocReg(y.r, rDI) END;
    IF x.r # rSI THEN RelocReg(x.r, rSI) END;
    cx := AllocReg2({});          CLR(cx);

    first := pc;                  EmitR(INC_, cx, 4);
    ArrayLen(len, node.left);
    IF len.mode = mImm THEN       EmitRI(CMPi, cx, 4, len.a)
    ELSE SetRmOperand(len);       EmitRegRm(CMPd, cx, 4);  FreeReg2(len)
    END;
                                  Trap(ccA, stringTrap);

    ArrayLen(len, node.right);
    IF len.mode = mImm THEN       EmitRI(CMPi, cx, 4, len.a)
    ELSE SetRmOperand(len);       EmitRegRm(CMPd, cx, 4);  FreeReg2(len)
    END;
                                  Trap(ccA, stringTrap);

                                  EmitBare(CMPSB);
    L := pc;                      Jcc1(ccNZ, 0);
                                      EmitRmImmRegI(CMPi, 1, 0, rSI, -1);
                                  BJump(first, ccNZ);
    Fixup(L, pc);
    FreeReg(rDI);  FreeReg(rSI);
                                  SetCond(x, OpToCc(node.op))
  ELSE ASSERT(FALSE)
  END;
  MkItmStat := oldStat
END Compare;

PROCEDURE MemberTest(VAR x: Item;  node: Node);
VAR y: Item;  oldStat: MakeItemState;
BEGIN
  ResetMkItmStat2(oldStat);
  IF node.left IS B.Const THEN
    MakeItem0(x, node.left);  MakeItem0(y, node.right);  RefToRegI(y);
    SetRmOperand(y);  EmitRmImm(BTi, 8, x.a MOD 64);  FreeReg2(y)
  ELSE
    LoadLeftRight2(x, y, node);
    EmitRR(BT, x.r, 8, y.r);  FreeReg(x.r);  FreeReg(y.r)
  END;
  SetCond(x, ccC);  MkItmStat := oldStat
END MemberTest;

PROCEDURE TypeTest(VAR x: Item;  node: Node);
VAR y, tag: Item;  oldStat: MakeItemState;  tp: B.Type;
BEGIN
  ResetMkItmStat2(oldStat);  tp := node.right.type;
  IF tp.form = B.tPtr THEN tp := tp.base END;
  MakeItem0(x, node.left);  TypeTag(x);  Load(x);
  TypeDesc(y, tp);  LoadAdr(y);  SetRm_regI(x.r, tp.len * 8);
  EmitRegRm(CMP, y.r, 8);  FreeReg(x.r);  FreeReg(y.r);
  SetCond(x, ccZ);  MkItmStat := oldStat
END TypeTest;

PROCEDURE Deref(VAR x: Item;  node: Node);
BEGIN
  MakeItem0(x, node.left);  Load(x);
  EmitRR(TEST, x.r, 8, x.r);  Trap(ccZ, nilTrap);
  x.mode := mRegI;  x.a := 0
END Deref;

PROCEDURE Field(VAR x: Item;  node: Node);
VAR off: INTEGER;
BEGIN
  MakeItem0(x, node.left);  off := node.right(B.Field).off;
  IF x.ref THEN INC(x.b, off) ELSE INC(x.a, off) END
END Field;

PROCEDURE TypeCheck(VAR x: Item;  node: Node);
VAR tag, y: Item;  oldStat: MakeItemState;  tp: B.Type;
BEGIN
  MakeItem0(x, node.left);  ResetMkItmStat2(oldStat);
  tp := node.right.type;  IF tp.form = B.tPtr THEN tp := tp.base END;
  TypeDesc(y, tp);  LoadAdr(y);  TypeTag2(tag, x);  Load(tag);
                                 EmitRegRmRegI(CMP, y.r, 8, tag.r, tp.len * 8);
  FreeReg(tag.r);  FreeReg(y.r);  Trap(ccNZ, typeTrap);  MkItmStat := oldStat
END TypeCheck;

PROCEDURE Index(VAR x: Item;  node: Node);
VAR idx, size, align, e: INTEGER;  len, y: Item;
    bType: B.Type;  oldStat: MakeItemState;
BEGIN
  oldStat := MkItmStat;  AvoidUsedBy(node.right);
  MakeItem0(x, node.left);  bType := x.type.base;  size := bType.size;
  IF node.right IS B.Const THEN idx := node.right(B.Const).val;
    IF B.IsOpenArray(x.type) & ~x.type.notag THEN
      ArrayLen(len, node.left);  SetRmOperand(len);
      EmitRmImm(CMPi, 4, idx);  Trap(ccBE, arrayTrap)
    END;
    IF x.ref THEN INC(x.b, idx*size) ELSE INC(x.a, idx*size) END
  ELSE RefToRegI(x);
    IF x.mode # mRegI THEN LoadAdr(x);  x.mode := mRegI;  x.a := 0 END;
    ResetMkItmStat;  MakeItem0(y, node.right);  Load(y);
    IF ~x.type.notag THEN ArrayLen(len, node.left);
      IF len.mode = mImm THEN EmitRI(CMPi, y.r, 4, len.a)
      ELSE SetRmOperand(len);  EmitRegRm(CMPd, y.r, 4)
      END;  Trap(ccAE, arrayTrap)
    END;
    IF size > 0 THEN e := log2(size);
      IF e = 1 THEN EmitR(SHL1, y.r, 8)
      ELSIF e > 1 THEN EmitRI(SHLi, y.r, 8, e)
      ELSIF e < 0 THEN EmitRI(IMULi, y.r, 8, size)
      END;
      EmitRR(ADDd, x.r, 8, y.r)
    END;
    FreeReg(y.r)
  END;
  MkItmStat := oldStat
END Index;

PROCEDURE RecordCast(VAR x: Item;  node: Node);
BEGIN
  MakeItem0(x, node.left);  Load(x);  x.mode := mRegI;  x.a := 0
END RecordCast;

PROCEDURE SingletonSet(VAR x: Item;  node: Node);
VAR r: INTEGER;  oldStat: MakeItemState;
BEGIN
  r := AllocReg();  ResetMkItmStat2(oldStat);  MakeItem0(x, node.left);
  Load(x);  CLR(r);  EmitRR(BTS, x.r, 8, r);  FreeReg(x.r);  x.r := r;
  MkItmStat := oldStat
END SingletonSet;

PROCEDURE RangeSet(VAR x: Item;  node: Node);
VAR r, r2: INTEGER;  oldStat: MakeItemState;
BEGIN
  oldStat := MkItmStat;  SetAvoid(rCX);
  r := AllocReg();  r2 := AllocReg2({rCX});  ResetMkItmStat;
  SetBestReg(rCX);  MakeItem0(x, node.left);  Load(x);
  IF x.r # rCX THEN RelocReg(x.r, rCX) END;
  LoadImm(r, 8, -1);  EmitR(SHLcl, r, 8);  FreeReg(rCX);
  SetBestReg(rCX);  MakeItem0(x, node.right);  Load(x);
  IF x.r # rCX THEN RelocReg(x.r, rCX) END;
  LoadImm(r2, 8, -2);  EmitR(SHLcl, r2, 8);  FreeReg(rCX);
  EmitRR(XORd, r, 8, r2);  FreeReg(r2);  x.r := r;  MkItmStat := oldStat
END RangeSet;

PROCEDURE ParReg(n: INTEGER): BYTE;
BEGIN
  IF    n > 3 THEN n := 255
  ELSIF n = 0 THEN n := rCX  ELSIF n = 1 THEN n := rDX
  ELSIF n = 2 THEN n := r8 ELSIF n = 3 THEN n := r9
  END;
  RETURN n
END ParReg;

PROCEDURE LoadParam(VAR x: Item;  par: Node;  n: INTEGER;  ref: BOOLEAN);
VAR obj: B.Object;  tp: B.Type;
BEGIN
  IF n < 4 THEN AvoidUsedBy(par.right) END;
  obj := par.left;  tp := obj.type;
  IF ~ref THEN
    IF tp.form # B.tReal THEN SetBestReg(ParReg(n))
    ELSIF n < 4 THEN MkItmStat.bestXReg := n
    END;  MakeItem0(x, obj);
    IF    tp.form # B.tRec THEN Load(x)
    ELSIF tp.size >= 8     THEN x.type := B.intType;     Load(x)
    ELSIF tp.size >= 4     THEN x.type := B.card32Type;  Load(x)
    ELSIF tp.size >= 2     THEN x.type := B.card16Type;  Load(x)
    ELSIF tp.size = 1      THEN x.type := B.byteType;    Load(x)
    END
  ELSE SetBestReg(ParReg(n));  MakeItem0(x, obj);  LoadAdr(x)
  END;
  IF n >= 4 THEN SetRm_regI(rSP, n*8);
    IF x.mode = mReg THEN EmitRegRm(MOV, x.r, 8);  FreeReg(x.r)
    ELSE EmitXmmRm(MOVSDd, x.r, 8);  FreeXReg(x.r)
    END
  END
END LoadParam;

PROCEDURE Parameter(par: Node;  fpar: B.Ident;  n: INTEGER);
VAR varpar: BOOLEAN;  ftype: B.Type;  x, y: Item;  i: INTEGER;  r: BYTE;
BEGIN
  ResetMkItmStat;  i := 1;
  ftype := fpar.obj.type;  varpar := fpar.obj(B.Par).varpar;
  IF ftype.form = B.tArray THEN LoadParam(x, par, n, TRUE);
    IF B.IsOpenArray(ftype) & ~ftype.notag THEN INC(i) END
  ELSIF ftype = B.strType  THEN LoadParam(x, par, n, TRUE)
  ELSIF ftype.form = B.tRec THEN
    IF varpar THEN LoadParam(x, par, n, TRUE);  INC(i)
    ELSIF (ftype.size < 9) & (ftype.size IN {0, 1, 2, 4, 8}) THEN
      LoadParam(x, par, n, FALSE)
    ELSE LoadParam(x, par, n, TRUE)
    END
  ELSE LoadParam(x, par, n, varpar)
  END;
  IF par.right # NIL THEN Parameter(par.right(Node), fpar.next, n+i) END;
  IF n < 4 THEN
    IF x.mode = mReg THEN r := ParReg(n);
      IF x.r # r THEN RelocReg(x.r, r) END
    ELSIF x.r # n THEN RelocXReg(x.r, n)
    END
  END;
  IF i = 2 THEN ResetMkItmStat;  SetBestReg(ParReg(n+1));
    IF ftype.form = B.tArray THEN
      IF ftype.base # B.byteType THEN ArrayLen(y, par.left)
      ELSE SizeOf(y, par.left)
      END;  Load(y)
    ELSIF (par.left IS B.Par) & par.left(B.Par).varpar THEN
      MakeItem0(y, par.left);  TypeTag(y);  Load(y)
    ELSE TypeDesc(y, par.left.type);  LoadAdr(y)
    END;
    IF n > 2 THEN
                                     EmitRegRmRegI(MOV, y.r, 8, rSP, n*8+8);  FreeReg(y.r)
    ELSIF ParReg(n+1) # y.r THEN ASSERT(FALSE)
    END
  END
END Parameter;

PROCEDURE Call(VAR x: Item;  node: Node);
VAR y: Item;  oldStat: MakeItemState;  L: INTEGER;
    oldAlloc, oldXAlloc: SET;  procType: B.Type;
BEGIN
  procType := node.left.type;  ResetMkItmStat2(oldStat);
  IF procType.base = NIL THEN allocReg := {};  allocXReg := {} END;
  oldAlloc := allocReg;  oldXAlloc := allocXReg;
  IF curProc.obj.homeSpace < 32 THEN curProc.obj.homeSpace := 32 END;
  IF curProc.obj.homeSpace < procType.parblksize THEN
    curProc.obj.homeSpace := procType.parblksize
  END;

  IF node.left IS B.Proc THEN MakeItem0(x, node.left)
  ELSE AvoidUsedBy(node.right);  MakeItem0(x, node.left);
    Load(x);  EmitRR(TEST, x.r, 8, x.r);  Trap(ccZ, nilProcTrap)
  END;
  IF node.right # NIL THEN
    Parameter(node.right(Node), procType.fields, 0)
  END;
  IF (x.mode = mReg) OR x.ref THEN SetRmOperand(x);  EmitRm(CALL, 4)
  ELSE CallProc(x.obj(B.Proc))
  END;
  MkItmStat := oldStat;  allocReg := oldAlloc;  allocXReg := oldXAlloc;
  IF procType.base # NIL THEN
    IF procType.base.form # B.tReal THEN
      x.mode := mReg;  x.r := rAX;  SetAlloc(rAX)
    ELSE x.mode := mXReg;  x.r := 0;  SetAllocX(0)
    END;
    x.ref := FALSE
  ELSE x.mode := mNothing
  END
END Call;

PROCEDURE Shift(VAR x: Item;  node: Node);
VAR oldStat: MakeItemState;  y: Item;  op: INTEGER;
BEGIN
  op := node.op;
  IF node.right IS B.Const THEN
    MakeItem0(x, node.left);  Load(x);  MakeItem0(y, node.right);
    IF y.a MOD 64 = 1 THEN
      IF    op = S.sfLSL THEN op := SHL1
      ELSIF op = S.sfASR THEN op := SAR1
      ELSIF op = S.sfROR THEN op := ROR1
      ELSE ASSERT(FALSE)
      END;
      EmitR(op, x.r, 8)
    ELSE
      IF    op = S.sfLSL THEN op := SHLi
      ELSIF op = S.sfASR THEN op := SARi
      ELSIF op = S.sfROR THEN op := RORi
      ELSE ASSERT(FALSE)
      END;
      EmitRI(op, x.r, 8, y.a MOD 64)
    END
  ELSE
    oldStat := MkItmStat;  AvoidUsedBy(node.right);
    SetAvoid(rCX);  MakeItem0(x, node.left);  Load(x);
    ResetMkItmStat;  SetBestReg(rCX);  MakeItem0(y, node.right);
    Load(y);  IF y.r # rCX THEN RelocReg(y.r, rCX) END;
    IF    op = S.sfLSL THEN op := SHLcl
    ELSIF op = S.sfROR THEN op := RORcl
    ELSIF op = S.sfASR THEN op := SARcl
    END;
    EmitR(op, x.r, 8);  FreeReg(rCX);  MkItmStat := oldStat
  END
END Shift;

PROCEDURE StdFunc(VAR x: Item;  node: Node);
VAR id, op, L, valSize: INTEGER;  r: BYTE;  obj1, obj2, obj3: B.Object;
    oldStat: MakeItemState;  y, z: Item;  valType: B.Type;
BEGIN
  id := node.op;  obj1 := node.left;  obj2 := node.right;
  IF id = S.sfABS THEN MakeItem0(x, obj1);  Load(x);
    IF x.type.form = B.tInt THEN
      EmitRR(TEST, x.r, 8, x.r);  L := pc;  Jcc1(ccGE, 0);
      EmitR(NEG, x.r, 8);  Fixup(L, pc)
    ELSIF x.type = B.realType THEN
      r := AllocReg2({});  SetRm_reg(r);
      EmitXmmRm(SseMOVDd, x.r, 8);  EmitRI(BTRi, r, 8, 63);
      SetRm_reg(r);  EmitXmmRm(SseMOVD, x.r, 8);  FreeReg(r)
    END
  ELSIF id = S.sfODD THEN
    ResetMkItmStat2(oldStat);  MakeItem0(x, obj1);  Load(x);
    EmitRI(ANDi, x.r, 4, 1);  FreeReg(x.r);  SetCond(x, ccNZ);
    MkItmStat := oldStat
  ELSIF id = S.sfLEN THEN (* x is open array *)
    ArrayLen(x, obj1);  Load(x)
  ELSIF (id >= S.sfLSL) & (id <= S.sfROR) THEN
    Shift(x, node)
  ELSIF id = S.sfFLOOR THEN
    oldStat := MkItmStat;  r := AllocReg();  ResetMkItmStat;
    MakeItem0(x, obj1);  Load(x);  EmitRI(SUBi, rSP, 8, 8);
                                      EmitRmRegI(STMXCSR, 4, rSP, 0);
    EmitRmImm(BTSi, 4, 13);  EmitRm(LDMXCSR, 4);
    SetRm_reg(x.r);  EmitXmmRm(CVTSD2SI, r, 8);  FreeXReg(x.r);
                                      EmitRmImmRegI(BTSi, 4, 13, rSP, 0);
    EmitRm(LDMXCSR, 4);  EmitRI(ADDi, rSP, 8, 8);
    x.mode := mReg;  x.r := r;  MkItmStat := oldStat
  ELSIF id = S.sfFLT THEN
    oldStat := MkItmStat;  r := AllocXReg();  ResetMkItmStat;
    MakeItem0(x, obj1);  Load(x);  SetRm_reg(x.r);
    EmitXmmRm(CVTSI2SD, r, 8);  FreeReg(x.r);
    x.mode := mXReg;  x.r := r;  MkItmStat := oldStat
  ELSIF id = S.sfORD THEN MakeItem0(x, obj1);  Load(x)

  ELSIF id = S.sfCHR8 THEN MakeItem0(x, obj1);  RefToRegI(x);
    IF x.mode IN {mRegI, mBP, mBX} THEN x.type := B.charType;  Load(x)
    ELSIF x.mode = mReg            THEN LoadToReg0(x.r, x, B.charType)
    ELSE ASSERT(FALSE) END
  ELSIF id = S.sfADR THEN MakeItem0(x, obj1);  LoadAdr(x)
  ELSIF id = S.sfBIT THEN
    ResetMkItmStat2(oldStat);  AvoidUsedBy(obj2);
    MakeItem0(x, obj1);  Load(x);  ResetMkItmStat;
    MakeItem0(y, obj2);  SetRm_regI(x.r, 0);
    IF y.mode = mImm THEN EmitRmImm(BTi, 8, y.a MOD 64)
    ELSE Load(y);  EmitRegRm(BT, y.r, 8);  FreeReg(y.r)
    END;
    FreeReg(x.r);  SetCond(x, ccC)
  ELSIF id = S.sfVAL THEN
    valType := node.type;  valSize := valType.size;
    IF valType = obj1.type THEN MakeItem0(x, obj1);  Load(x)
    ELSIF (valType = B.realType) & (obj1.type.form # B.tReal) THEN
      oldStat := MkItmStat;  r := AllocXReg();  ResetMkItmStat;
      MakeItem0(x, obj1);  Load(x);  SetRm_reg(x.r);
      EmitXmmRm(SseMOVD, r, 8);  FreeReg(x.r);
      x.mode := mXReg;  x.r := r;  MkItmStat := oldStat
    ELSIF (obj1.type = B.realType) & (valType.form # B.tReal) THEN
      oldStat := MkItmStat;  r := AllocReg();  ResetMkItmStat;
      MakeItem0(x, obj1);  Load(x);  SetRm_reg(r);
      EmitXmmRm(SseMOVDd, x.r, 8);  FreeXReg(x.r);
      x.mode := mReg;  x.r := r;  MkItmStat := oldStat;
      IF valSize < 8 THEN LoadToReg0(x.r, x, valType) END
    ELSIF (obj1.type.form # B.tReal) & (valType.form # B.tReal) THEN
      MakeItem0(x, obj1);  RefToRegI(x);  Load(x);
      IF valSize < 8 THEN LoadToReg0(x.r, x, valType) END
    ELSE ASSERT(FALSE)
    END
  ELSIF id = S.sfNtCurrentTeb THEN
    x.mode := mReg;  x.r := AllocReg();
    mem.mod := 0;  mem.rm := rSP;  mem.bas := rBP;
    mem.idx := rSP;  mem.scl := 0;  mem.disp := 30H;
    (* GS prefix *) Put(1, 65H);  EmitREX(x.r, 8);
    Put(1, MOVd+1);  EmitModRM(x.r)
  ELSIF id = S.sfCAS THEN
    obj3 := obj2(B.Node).right;  obj2 := obj2(B.Node).left;
    oldStat := MkItmStat;  AvoidUsedBy(obj2);  AvoidUsedBy(obj3);
    SetAvoid(rAX);  MakeItem0(x, obj1);  RefToRegI(x);  ResetMkItmStat;
    AvoidUsedBy(obj3);  SetBestReg(rAX);  MakeItem0(y, obj2);  Load(y);
    ResetMkItmStat;  SetAvoid(rAX);  MakeItem0(z, obj3);  Load(z);
    IF y.r # rAX THEN RelocReg(y.r, rAX) END;

    SetRmOperand(x);  EmitCMPXCHG(z.r, x.type.size);
    FreeReg(z.r);  FreeReg2(x);  MkItmStat := oldStat;
    x.mode := mReg;  x.r := rAX
  ELSE ASSERT(FALSE)
  END
END StdFunc;

PROCEDURE Becomes(node: Node);
VAR x, y, z: Item;  cx, rsize, first, L: INTEGER;
BEGIN
  IF ~(node.left.type.form IN {B.tArray, B.tRec}) THEN
    AvoidUsedBy(node.right);  MakeItem0(x, node.left);
    RefToRegI(x);  ResetMkItmStat;  MakeItem0(y, node.right);
    IF (y.mode = mImm) & (x.type.size <= 2) THEN
      SetRmOperand(x);  EmitRmImm(MOVi, x.type.size, y.a)
    ELSE Load(y);  SetRmOperand(x);
      IF y.type = B.realType THEN EmitXmmRm(MOVSDd, y.r, 4)
      ELSE EmitRegRm(MOV, y.r, x.type.size)
      END
    END
  ELSE
    AvoidUsedBy(node.right);  SetAvoid(rSI);  SetBestReg(rDI);
    MakeItem0(x, node.left);  LoadAdr(x);  ResetMkItmStat;
    SetBestReg(rSI);  MakeItem0(y, node.right);  LoadAdr(y);
    IF y.r # rSI THEN RelocReg(y.r, rSI) END;
    IF x.r # rDI THEN RelocReg(x.r, rDI) END;
    IF y.type = B.strType THEN cx := y.strlen;  (* Copied from str16Type below - needs changes? *)
      IF B.IsOpenArray(x.type) THEN
        ArrayLen(z, node.left);
        SetRmOperand(z);         EmitRmImm(CMPi, 4, cx);
                                 Trap(ccB, stringTrap)
      END;
      SetAlloc(rCX);  LoadImm(rCX, 4, cx);  EmitRep(MOVSrep, 1, 1) (* Size 2 -> 1 for CHAR *)
    ELSIF B.IsStr(x.type) THEN       (* Changed to LODSB/STOSB otherwise test thoroughly *)
      SetAlloc(rAX);  SetAlloc(rCX);
      CLR(rCX);  CLR(rAX);
      first := pc;  EmitRI(ADDi, rCX, 4, 1);

      ArrayLen(z, node.left);
      IF z.mode = mImm THEN EmitRI(CMPi, rCX, 4, z.a)
      ELSE SetRmOperand(z);  EmitRegRm(CMPd, rCX, 4)
      END;  Trap(ccA, stringTrap);
      ArrayLen(z, node.right);
      IF z.mode = mImm THEN EmitRI(CMPi, rCX, 4, z.a)
      ELSE SetRmOperand(z);  EmitRegRm(CMPd, rCX, 4)
      END;  Trap(ccA, stringTrap);

      EmitBare(LODSB);  EmitBare(STOSB);
      EmitRR(TEST, rAX, 4, rAX);  BJump(first, ccNZ)
    ELSIF B.IsOpenArray(y.type) THEN
      SetBestReg(rCX);  ArrayLen(z, node.right);  Load(z);
      IF z.r # rCX THEN RelocReg(z.r, rCX) END;

      ArrayLen(z, node.left);
      IF z.mode = mImm THEN EmitRI(CMPi, rCX, 4, z.a)
      ELSE SetRmOperand(z);  EmitRegRm(CMPd, rCX, 4)
      END;  Trap(ccA, stringTrap);

      rsize := y.type.base.align;  cx := y.type.base.size DIV rsize;
      EmitRI(IMULi, rCX, 8, cx);  EmitRep(MOVSrep, rsize, 1)
    ELSE
      cx := x.type.size DIV x.type.align;
      SetAlloc(rCX);  LoadImm(rCX, 4, cx);
      EmitRep(MOVSrep, x.type.align, 1)
    END
  END;
END Becomes;

PROCEDURE If(node: Node);
VAR x, y: Item;  then: Node;
BEGIN
  LoadCond(x, node.left);  Jump(node, x.aLink, negated(x.c));
  FixLink(x.bLink);  then := node.right(Node);
  MakeItem0(y, then.left);  Jump(then, NIL, ccAlways);  FixLink(node);
  IF then.right # NIL THEN MakeItem0(y, then.right) END;  FixLink(then)
END If;

PROCEDURE While(node: Node;  first: INTEGER);
VAR x, y: Item;  do: Node;
BEGIN
  IF first < 0 THEN first := pc END;
  LoadCond(x, node.left);  Jump(node, x.aLink, negated(x.c));
  FixLink(x.bLink);  do := node.right(Node);
  MakeItem0(y, do.left);  BJump(first, ccAlways);  FixLink(node);
  IF do.right # NIL THEN While(do.right(Node), first) END
END While;

PROCEDURE Repeat(node: Node);
VAR x, y: Item;  first: INTEGER;
BEGIN
  first := pc;  MakeItem0(x, node.left);
  LoadCond(y, node.right);  Jump(node, y.aLink, negated(y.c));
  FixLinkWith(node, first);  FixLink(y.bLink)
END Repeat;

PROCEDURE For(node: Node);
VAR i, b, e: Item;  control, beg, end: Node;  by: B.Object;
    inc, op, opI, first: INTEGER;  r, cc: BYTE;
BEGIN
  control := node.left(Node);  beg := control.right(Node);
  end := beg.right(Node);  by := end.right;

  ASSERT(control.left IS B.Var);  MakeItem0(b, beg.left);
  IF (b.mode = mImm) & SmallConst(b.a) THEN
    MakeItem0(i, control.left);  RefToRegI(i);
    SetRmOperand(i);  EmitRmImm(MOVi, 8, b.a);  FreeReg2(i)
  ELSE Load(b);  MakeItem0(i, control.left);  RefToRegI(i);
    SetRmOperand(i);  EmitRegRm(MOV, b.r, 8);  FreeReg2(i);  FreeReg(b.r)
  END;
  IF by = NIL THEN inc := 1 ELSE inc := by(Node).left(B.Const).val END;
  IF inc >= 0 THEN cc := ccG;  opI := ADDi;  op := ADD
  ELSE cc := ccL;  opI := SUBi;  op := SUB
  END;

  first := pc;  AvoidUsedBy(end.left);
  MakeItem0(i, control.left);  RefToRegI(i);
  IF (end.left IS B.Const) & SmallConst(end.left(B.Const).val) THEN
    SetRmOperand(i);  EmitRmImm(CMPi, 8, end.left(B.Const).val)
  ELSE
    ResetMkItmStat;  MakeItem0(e, end.left);  Load(e);
    SetRmOperand(i);  EmitRegRm(CMP, e.r, 8);  FreeReg(e.r)
  END;
  FreeReg2(i);  Jump(node, NIL, cc);

  MakeItem0(i, node.right);  (* Statement sequence *)
  MakeItem0(i, control.left);  RefToRegI(i);  SetRmOperand(i);
  IF inc = 1 THEN EmitRm(INC_, 8) ELSIF inc = -1 THEN EmitRm(DEC_, 8)
  ELSIF SmallConst(inc) THEN EmitRmImm(opI, 8, inc)
  ELSE r := AllocReg();  LoadImm(r, 8, inc);  EmitRegRm(op, r, 8)
  END;
  BJump(first, ccAlways);  FixLink(node)
END For;

PROCEDURE Case(node: Node);
VAR tv: B.TempVar;

  PROCEDURE TypeCase(node0: B.Object);
  VAR x, y: Item;  node, colon: B.Node;
      obj: B.Object;  tp, org: B.Type;
  BEGIN node := node0(B.Node);
    IF node.left IS B.Node THEN
      tp := node.left(B.Node).right.type;
      obj := node.left(B.Node).left;  org := obj.type
    END;
    LoadCond(x, node.left);  Jump(node, x.aLink, negated(x.c));
    FixLink(x.bLink);  IF obj # NIL THEN obj.type := tp END;
    colon := node.right(Node);  MakeItem0(y, colon.left);
    Jump(colon, NIL, ccAlways);  FixLink(node);
    IF obj # NIL THEN obj.type := org END;
    IF colon.right # NIL THEN TypeCase(colon.right) END;  FixLink(colon)
  END TypeCase;

  PROCEDURE NumericCase(node0: B.Object);
  VAR x, y: Item;  node, colon: B.Node;
  BEGIN node := node0(B.Node);
    LoadCond(x, node.left);  Jump(node, x.aLink, negated(x.c));
    FixLink(x.bLink);  colon := node.right(Node);  MakeItem0(y, colon.left);
    Jump(colon, NIL, ccAlways);  FixLink(node);
    IF colon.right # NIL THEN NumericCase(colon.right) END;  FixLink(colon)
  END NumericCase;

BEGIN (* Case *)
  IF node.left = NIL (* type case *) THEN
    IF node.right # NIL THEN TypeCase(node.right) END
  ELSE tv := node.left(B.Node).left(B.TempVar);
    IF ~tv.inited THEN tv.adr := AllocStack(8);  tv.inited := TRUE END;
    Becomes(node.left(B.Node));
    IF node.right # NIL THEN NumericCase(node.right) END;
    FreeStack(8)
  END
END Case;

PROCEDURE StdProc(node: Node);
VAR id, size, op: INTEGER;  r, r2: BYTE;
    x, y, z: Item;  obj1, obj2, obj3: B.Object;
BEGIN
  id := node.op;  obj1 := node.left;  obj2 := node.right;
  IF (id = S.spINC) OR (id = S.spDEC) THEN
    AvoidUsedBy(obj2);  MakeItem0(x, obj1);  RefToRegI(x);
    IF (obj2 = NIL) OR (obj2 IS B.Const) & (obj2(B.Const).val = 1) THEN
      IF id = S.spINC THEN op := INC_ ELSE op := DEC_ END;
      SetRmOperand(x);  EmitRm(op, x.type.size)
    ELSIF obj2 IS B.Const THEN MakeItem0(y, obj2);
      IF ~SmallConst(y.a) THEN Load(y) END;  SetRmOperand(x);
      IF y.mode = mImm THEN
        IF id = S.spINC THEN op := ADDi ELSE op := SUBi END;
        EmitRmImm(op, x.type.size, y.a)
      ELSE
        IF id = S.spINC THEN op := ADD ELSE op := SUB END;
        EmitRegRm(op, y.r, x.type.size)
      END
    ELSE
      SetRmOperand(x);  r := AllocReg();  EmitRegRm(MOVd, r, x.type.size);
      ResetMkItmStat;  MakeItem0(y, obj2);  Load(y);
      IF id = S.spINC THEN op := ADDd ELSE op := SUBd END;
      EmitRR(op, r, x.type.size, y.r);  SetRmOperand(x);
      EmitRegRm(MOV, r, x.type.size)
    END
  ELSIF (id = S.spINCL) OR (id = S.spEXCL) THEN
    ASSERT(obj1.type = B.setType);
    AvoidUsedBy(obj2);  MakeItem0(x, obj1);  RefToRegI(x);
    IF obj2 IS B.Const THEN
      IF id = S.spINCL THEN op := BTSi ELSE op := BTRi END;
      MakeItem0(y, obj2);  SetRmOperand(x);  EmitRmImm(op, 8, y.a MOD 64)
    ELSE SetRmOperand(x);  r := AllocReg();  EmitRegRm(MOVd, r, 8);
      IF id = S.spINCL THEN op := BTS ELSE op := BTR END;
      ResetMkItmStat;  MakeItem0(y, obj2);  Load(y);
      EmitRR(op, y.r, 8, r);  SetRmOperand(x);  EmitRegRm(MOV, r, 8)
    END
  ELSIF id = S.spNEW THEN
    SetBestReg(rCX);  MakeItem0(x, obj1);  LoadAdr(x);
    IF x.r # rCX THEN RelocReg(x.r, rCX) END;
    SetBestReg(rDX);  TypeDesc(y, obj1.type.base);
    LoadAdr(y);  EmitRmRegI(CALL, 4, rBX, adrOfNEW);
    SetAlloc(rBX);
    IF curProc.obj.homeSpace < 32 THEN curProc.obj.homeSpace := 32 END
  ELSIF id = S.spASSERT THEN
    LoadCond(x, obj1);  Jump(node, x.bLink, x.c);  FixLink(x.aLink);
    EmitBare(UD2);  WriteDebug(pc-2, sourcePos, assertTrap);  FixLink(node)
  ELSIF id = S.spPACK THEN
    AvoidUsedBy(obj2);   MakeItem0(x, obj1);
    RefToRegI(x);        r := AllocReg();
    SetRmOperand(x);     EmitRegRm(MOVd, r, 8);
    ResetMkItmStat;
    MakeItem0(y, obj2);  Load(y);
                         EmitRI   (SHLi, y.r, 8, 52);
                         EmitRR   (ADDd, r, 8, y.r);
    SetRmOperand(x);     EmitRegRm(MOV,  r, 8)
  ELSIF id = S.spUNPK THEN
    AvoidUsedBy(obj2);   MakeItem0(x, obj1);
    RefToRegI(x);        r := AllocReg();
    SetRmOperand(x);     EmitRegRm(MOVd, r, 8);
    ResetMkItmStat;      MakeItem0(y, obj2);
    RefToRegI(y);        r2 := AllocReg();
                         EmitRR   (MOVd, r2, 8, r);
                         EmitRI   (SHRi, r2, 8, 52);
                         EmitRI   (SUBi, r2, 4, 1023);
    SetRmOperand(y);     EmitRegRm(MOV,  r2, 8);
                         EmitRI   (SHLi, r2, 8, 52);
                         EmitRR   (SUBd, r, 8, r2);
    SetRmOperand(x);     EmitRegRm(MOV,  r, 8)
  ELSIF id = S.spGET THEN
    AvoidUsedBy(obj2);   MakeItem0(x, obj1);
                                  Load(x);
    WriteDebug(pc, node.sourcePos, GetTrap);
                     EmitRegRmRegI(MOVd, x.r, obj2.type.size, x.r, 0);
    ResetMkItmStat;      MakeItem0(y, obj2);  RefToRegI(y);
    SetRmOperand(y);     EmitRegRm(MOV,  x.r, y.type.size)
  ELSIF id = S.spPUT THEN
    AvoidUsedBy(obj2);
    MakeItem0(x, obj1);    Load(x);
    ResetMkItmStat;        MakeItem0(y, obj2);
    IF (y.mode = mImm) & (y.type.size <= 2) THEN
      SetRm_regI(x.r, 0);  EmitRmImm(MOVi, y.type.size, y.a)
    ELSE                   Load(y);
      SetRm_regI(x.r, 0);  EmitRegRm(MOV, y.r, y.type.size)
    END;
    WriteDebug(pc-3, node.sourcePos, PutTrap)
  ELSIF id = S.spCOPY THEN
    obj3 := obj2(Node).right;  obj2 := obj2(Node).left;
    AvoidUsedBy(obj2);  AvoidUsedBy(obj3);   SetAvoid(rDI);
    SetAvoid(rCX);      SetBestReg(rSI);     MakeItem0(x, obj1);  Load(x);
    ResetMkItmStat;     AvoidUsedBy(obj3);   SetAvoid(rCX);
    SetBestReg(rDI);    MakeItem0(y, obj2);  Load(y);
    ResetMkItmStat;     SetBestReg(rCX);     MakeItem0(z, obj3);  size := 1;
    IF z.mode = mImm THEN
      IF    z.a <= 0      THEN z.a := 0;          size := 0
      ELSIF z.a MOD 8 = 0 THEN z.a := z.a DIV 8;  size := 8
      ELSIF z.a MOD 4 = 0 THEN z.a := z.a DIV 4;  size := 4
      ELSIF z.a MOD 2 = 0 THEN z.a := z.a DIV 2;  size := 2
      END
    END;
    IF size > 0 THEN Load(z);
      IF z.r # rCX  THEN RelocReg(z.r, rCX)  END;
      IF y.r # rDI THEN RelocReg(y.r, rDI) END;
      IF x.r # rSI THEN RelocReg(x.r, rSI) END;
      EmitRep(MOVSrep, size, 1)
    END
  ELSIF id = S.spINT3 THEN
    EmitBare(INT3)
  ELSIF id = S.spPAUSE THEN
    EmitBare(PAUSE)
  ELSE ASSERT(FALSE)
  END
END StdProc;

PROCEDURE MakeItem(VAR x: Item;  obj: B.Object);  (* x receives result value, if any *)
VAR objv: B.Var;  node: Node;  size, form: INTEGER;
BEGIN
  x.type := obj.type;  x.ref := FALSE;  x.a := 0;  x.b := 0;  x.c := 0;
  IF    obj IS B.Const THEN x.mode := mImm;  x.a := obj(B.Const).val;
  ELSIF obj IS B.Var   THEN x.obj  := obj;
    objv := obj(B.Var);  x.a := objv.adr;  form := objv.type.form;
    IF objv.lev <= 0 THEN x.mode := mBX ELSE x.mode := mBP END;
    IF objv.lev < 0  THEN x.ref  := TRUE END;
    IF    objv IS B.Str  THEN x.mode := mBX;  x.strlen := objv(B.Str).len
    ELSIF objv IS B.Par THEN
      size := objv.type.size;
      x.ref := objv(B.Par).varpar
            OR (form = B.tArray)
            OR (form = B.tRec) & ((size > 8) OR (size IN {3, 5, 6, 7}))
    ELSIF objv IS B.TempVar THEN x.mode := mBP
    END;
    IF x.mode = mBX THEN SetAlloc(rBX) END
  ELSIF obj IS B.Proc THEN
    x.mode := mProc;  x.a := obj(B.Proc).adr;
    IF obj(B.Proc).lev >= 0 THEN x.obj := obj
    ELSE x.ref := TRUE;  SetAlloc(rBX)
    END
  ELSIF obj.class = B.cType THEN ASSERT(FALSE)
  ELSIF obj IS Node THEN
    node := obj(Node);  sourcePos := node.sourcePos;  x.mode := mNothing;

    ASSERT((node.op >= S.times) & (node.op <= S.period)
       OR  (node.op >= S.not)   & (node.op <= S.lbrace)
       OR  (node.op >= S.if)    & (node.op <= S.for)
       OR  (node.op =  S.becomes)
       OR  (node.op =  S.upto)
       OR  (node.op =  S.semicolon)
       OR  (node.op =  S.call)
       OR  (node.op =  S.bitset)
       OR  (node.op >= S.begSf) & (node.op <= S.endSf)
       OR  (node.op >= S.begSp) & (node.op <= S.endSp));

    CASE node.op OF
      S.becomes, S.if.. S.for, S.begSp.. S.endSp: ResetMkItmStat;  allocReg := {};  allocXReg := {}
    END;

    CASE node.op OF
    | S.times:          Op2(x, node)
    | S.rdiv:           Op2(x, node)
    | S.div:            IntDiv(x, node)
    | S.mod:            IntDiv(x, node)
    | S.and:            And(x, node)
    | S.plus:           Op2(x, node)
    | S.minus:          IF node.right # NIL THEN Op2(x, node) ELSE Negate(x, node) END
    | S.or:             Or(x, node)
    | S.eql..S.geq:     Compare(x, node)
    | S.in:             MemberTest(x, node)
    | S.is:             TypeTest(x, node)
    | S.arrow:          Deref(x, node)
    | S.period:         Field(x, node)
    | S.not:            Not(x, node)
    | S.lparen:         TypeCheck(x, node)
    | S.lbrak:          Index(x, node)
    | S.lbrace:         RecordCast(x, node)
    | S.if:             If(node)
    | S.while:          While(node, -1)
    | S.repeat:         Repeat(node)
    | S.case:           Case(node)
    | S.for:            For(node)
    | S.becomes:        Becomes(node)
    | S.upto:           RangeSet(x, node)
    | S.semicolon:      IF node.left  # NIL THEN MakeItem(x, node.left)  END;
                        IF node.right # NIL THEN MakeItem(x, node.right) END
    | S.call:           Call(x, node)
    | S.bitset:         SingletonSet(x, node)
    | S.begSf..S.endSf: StdFunc(x, node)
    | S.begSp..S.endSp: StdProc(node)
    END;

    CASE node.op OF
      S.becomes, S.if.. S.for, S.begSp.. S.endSp: ResetMkItmStat;  allocReg := {};  allocXReg := {}
    END;

    IF x.mode # mNothing THEN
      x.type := node.type;
      IF    (x.mode IN {mReg, mRegI}) & (x.r IN MkItmStat.avoid)  THEN RelocReg(x.r, AllocReg())
      ELSIF (x.mode = mXReg)          & (x.r IN MkItmStat.xAvoid) THEN RelocReg(x.r, AllocXReg())
      END
    END
  ELSE ASSERT(FALSE)
  END
END MakeItem;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE BeginProc;
VAR proc: Proc;
BEGIN proc := curProc.obj;
  B.Align(proc.locblksize, 8);  stack := proc.locblksize;
  IF pass = 2 THEN
    proc.usedReg   := {}; proc.usedXReg := {};
    proc.homeSpace := 0;  proc.stack    := stack
  ELSIF pass = 3 THEN proc.adr := pc
  ELSE ASSERT(FALSE)
  END
END BeginProc;

PROCEDURE FinishProc;
VAR proc: Proc;  L, L2, adr: INTEGER;
BEGIN proc := curProc.obj;
  IF pass = 3 THEN
    WHILE pc MOD 16 # 0 DO Put(1, 90H) END;
    proc.lim := pc;  L := proc.fix;  adr := proc.adr;
    WHILE L # -1 DO
      L2 := code[L] + LSL(code[L+1], 8);
      INC(L2, LSL(code[L+2], 16) + LSL(code[L+3], 24));
      FixDisp(L, adr-L-4, 4);  L := ASR(LSL(L2, 32), 32)
    END
  END
END FinishProc;

PROCEDURE Procedure;
VAR locblksize, homeSpace, nSave, nSaveX, n, i, j, L: INTEGER;
    r: BYTE;  x: Item;  obj: B.Proc;  param, ident: B.Ident;  pType: B.Type;
BEGIN
  BeginProc;  obj := curProc.obj;
  IF pass = 3 THEN
    PushR(rBP);
    EmitRR(MOVd, rBP, 8, rSP);
    nSave := 0;  nSaveX := 0;  r := 0;
    WHILE r < 16 DO
      IF r IN obj.usedReg*{3 .. 7, 12 .. 15} THEN INC(nSave) END;
      IF r IN obj.usedXReg*{6 .. 15} THEN INC(nSaveX) END;
      INC(r)
    END;
    n := ((obj.stack + nSave*8 + 8) DIV 16 + nSaveX) * 16;
    B.Align(obj.homeSpace, 16);  homeSpace := obj.homeSpace;
    IF n + homeSpace # 0 THEN
      IF n + homeSpace >= 4096 THEN
        LoadImm      (rAX, 8, -4096);  L := pc;
        EmitRegRmRegX(MOV,  rAX, 1, rSP, rAX, 0, 0);
        EmitRI       (SUBi, rAX, 8, 4096);
        EmitRI       (CMPi, rAX, 8, -(n + homeSpace));
        BJump        (L,    ccGE)
      END;
      EmitRI       (SUBi, rSP, 8, n + homeSpace)
    END;

    r := 0;  i := 0;  j := 0;
    WHILE r < 16 DO
      IF r IN obj.usedReg*{3 .. 7, 12 .. 15} THEN
        EmitRegRmRegI(MOV, r, 8, rBP, -n+nSaveX*16 + i*8);  INC(i)
      END;
      IF r IN obj.usedXReg*{6 .. 15} THEN
        EmitXmmRmRegI(MOVAPSd, r, 4, rBP, -n + j*16);  INC(j)
      END;
      INC(r)
    END;

    (* Load the base of current module to RBX *)
    IF rBX IN obj.usedReg THEN
      EmitRegRmRip(LEA, rBX, 8, baseOffset-pc-7);
    END;

    IF obj.type # NIL THEN
      param := obj.type.fields;  i := 0;
      WHILE (param # NIL) & (i < 4) DO
        pType := param.obj.type;
        SetRm_regI(rBP, 16+i*8);
        IF pType.form # B.tReal THEN
          EmitRegRm(MOV, ParReg(i), 8);
          IF (pType.form = B.tRec) & (param.obj(B.Par).varpar)
          OR B.IsOpenArray(pType) & ~pType.notag THEN INC(i);
            IF i < 4 THEN
              EmitRegRmRegI(MOV, ParReg(i), 8, rBP, 16+i*8);
            END
          END
        ELSE
          EmitXmmRm(MOVSDd, i, 4)
        END;
        INC(i);  param := param.next
      END
    END
  END;

  IF (obj.statseq # NIL) OR (obj.return # NIL) THEN
    IF obj.nProc + obj.nPtr > 0 THEN
            CLR     (rAX);
            LoadImm      (rCX, 8, -obj.locblksize DIV 8);
  L := pc;  EmitRegRmRegX(MOV,  rAX, 8, rBP, rCX, 3, 0);
            EmitRI       (ADDi, rCX, 8, 1);
            BJump(L, ccL)
    END;

    IF obj.nTraced > 0 THEN
      EmitRegRmRegI(MOV,  rBP, 8, rBP, -obj.locblksize);
      EmitRegRmRegI(LEA,  rAX, 8, rBX,   obj.descAdr);
      EmitRegRmRegI(MOV,  rAX, 8, rBP, -obj.locblksize+8);
      EmitRegRmRegI(MOVd, rAX, 8, rBX,  adrOfStackPtrList);
      EmitRegRmRegI(MOV,  rAX, 8, rBP, -obj.locblksize+16);
      EmitRegRmRegI(LEA,  rAX, 8, rBP, -obj.locblksize);
      EmitRegRmRegI(MOV,  rAX, 8, rBX,  adrOfStackPtrList)
    END;

    IF obj.statseq # NIL THEN MakeItem(x, obj.statseq) END;
    IF obj.return # NIL THEN ResetMkItmStat;
      MakeItem(x, obj.return);  Load(x);
      IF x.r # 0 THEN
        IF x.mode = mReg THEN RelocReg(x.r, 0) ELSE RelocXReg(x.r, 0)
        END
      END
    END;
    IF obj.nTraced > 0 THEN
      EmitRegRmRegI(MOVd, rCX, 8, rBP, -obj.locblksize+16);
      EmitRegRmRegI(MOV,  rCX, 8, rBX, adrOfStackPtrList)
    END
  END;

  IF pass = 3 THEN r := 0;  i := 0;  j := 0;
    WHILE r < 16 DO
      IF r IN obj.usedReg*{3 .. 7, 12 .. 15} THEN
        EmitRegRmRegI(MOVd, r, 8, rBP, -n+nSaveX*16 + i*8);  INC(i)
      END;
      IF r IN obj.usedXReg*{6 .. 15} THEN
        EmitXmmRmRegI(MOVAPS, r, 4, rBP, -n + j*16);  INC(j)
      END;
      INC(r)
    END;
    EmitRR  (MOVd, rSP, 8, rBP);
            PopR(rBP);
    EmitBare(RET)
  END;
  FinishProc
END Procedure;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Const folding during parsing phase *)

PROCEDURE CheckSetElement*(x: B.Object);
VAR val: INTEGER;
BEGIN
  IF x IS B.Const THEN val := x(B.Const).val ELSE val := 0 END;
  IF (val < 0) OR (val > 63) THEN
    S.Mark("Set element must be >= 0 and <= 63")
  END
END CheckSetElement;

PROCEDURE ConstSingletonSet*(x: B.Object): B.Object;
VAR val: INTEGER;
BEGIN
  IF x IS B.Const THEN val := x(B.Const).val MOD 64 ELSE val := 0 END;
  x := B.NewConst(B.setType, ORD({val}));
  RETURN x
END ConstSingletonSet;

PROCEDURE ConstRangeSet*(x, y: B.Object): B.Object;
VAR beg, end: INTEGER;
BEGIN
  IF x IS B.Const THEN beg := x(B.Const).val MOD 64 ELSE beg := 0 END;
  IF y IS B.Const THEN end := y(B.Const).val MOD 64 ELSE end := 0 END;
  x := B.NewConst(B.setType, ORD({beg..end}));
  RETURN x
END ConstRangeSet;

PROCEDURE NegateConst*(x0: B.Object): B.Const;
VAR x: B.Const;  type: B.Type;  val: INTEGER;
BEGIN
  type := x0.type;  IF x0 IS B.Const THEN val := x0(B.Const).val END;
  x := B.NewConst(type, val);
  IF x.type = B.byteType THEN x.type := B.intType END;
  IF x.type = B.intType THEN x.val := -x.val
  ELSIF x.type = B.realType THEN
    x.val := SYSTEM.VAL(INTEGER, -SYSTEM.VAL(REAL, x.val))
  ELSIF x.type = B.setType THEN x.val := ORD(-SYSTEM.VAL(SET, x.val))
  ELSIF x.type = B.boolType THEN x.val := (x.val + 1) MOD 2
  END;
  RETURN x
END NegateConst;

PROCEDURE AbsConst*(x: B.Object): B.Object;
VAR type: B.Type;  val: INTEGER;  val2: SET;
BEGIN type := x.type;  val := x(B.Const).val;
  IF type = B.intType THEN
    IF val < 0 THEN val := -val END
  ELSIF type = B.byteType THEN type := B.intType
  ELSIF type = B.realType THEN
    val2 := SYSTEM.VAL(SET, val);  EXCL(val2, 63);  val := ORD(val2)
  END;
  x := B.NewConst(type, val)
  RETURN x
END AbsConst;

PROCEDURE OddConst*(x: B.Object): B.Object;
VAR val: INTEGER;
BEGIN val := x(B.Const).val;  x := B.NewConst(B.boolType, val MOD 2);
  RETURN x
END OddConst;

PROCEDURE ShiftConst*(fid: INTEGER;  x, y: B.Object): B.Object;
VAR xval, yval: INTEGER;
BEGIN xval := x(B.Const).val;  yval := y(B.Const).val;
  IF fid = S.sfLSL THEN xval := LSL(xval, yval)
  ELSIF fid = S.sfASR THEN xval := ASR(xval, yval)
  ELSIF fid = S.sfROR THEN xval := ROR(xval, yval)
  END;
  x := B.NewConst(B.intType, xval);
  RETURN x
END ShiftConst;

PROCEDURE FloorConst*(x: B.Object): B.Object;
VAR val, fraction, exp, p: INTEGER;  sign: BOOLEAN;
BEGIN
  IF x.type = B.realType THEN
    val := x(B.Const).val;  fraction := val MOD 10000000000000H;
    exp := val DIV 10000000000000H MOD 800H;  sign := val < 0;
    IF exp = 0 (* subnormal *) THEN val := 0
    ELSIF exp = 7FFH (* Inf or NaN *) THEN S.Mark("Float too large")
    ELSE DEC(exp, 1023);  INC(fraction, 10000000000000H);  p := 52;
      IF exp < 0 THEN val := 0 ELSIF exp = 0 THEN val := 1
      ELSE WHILE (p > 0) & (exp > 0) DO DEC(p);  DEC(exp) END;
        IF exp = 0 THEN val := ASR(fraction, p)
        ELSIF exp <= 11 THEN val := LSL(fraction, exp)
        ELSE S.Mark("Float too large")
        END
      END;
      IF sign THEN val := -val END
    END
  END;
  x := B.NewConst(B.intType, val);
  RETURN x
END FloorConst;

PROCEDURE FltConst*(x: B.Object): B.Object;
  CONST n52 = 10000000000000H;
VAR val, exp, r: INTEGER;  sign: BOOLEAN;
BEGIN val := x(B.Const).val;
  IF val = MinInt THEN val := -3C20000000000000H
  ELSIF val # 0 THEN
    exp := 52;  sign := val < 0;  IF sign THEN val := -val END;  r := 0;
    WHILE val < n52 DO val := val * 2;  DEC(exp) END;
    WHILE val >= n52 * 2 DO
      INC(r, LSL(val MOD 2, exp));  val := val DIV 2;  INC(exp)
    END;
    IF (exp > 0) & (r >= LSL(1, exp-1)) THEN INC(val);
      IF val >= n52 * 2 THEN val := val DIV 2;  INC(exp) END
    END;
    INC(exp, 1023);  val := val MOD n52 + exp * n52;
    IF sign THEN val := ORD(SYSTEM.VAL(SET, val) + {63}) END
  END;
  x := B.NewConst(B.realType, val);
  RETURN x
END FltConst;

PROCEDURE TypeTransferConst*(type: B.Type;  x: B.Object): B.Object;
VAR val: INTEGER;
BEGIN
  IF x IS B.Str  THEN val := ORD(B.strBuf[x(B.Str).bufpos])
  ELSE val := x(B.Const).val
  END;
  IF type # x.type THEN
    IF type.size = 1 THEN val := val MOD 100H;
      IF (type = B.int8Type) & (val >= 80H) THEN DEC(val, 100H) END
    ELSIF type.size = 2 THEN val := val MOD 10000H;
      IF (type = B.int16Type) & (val >= 8000H) THEN DEC(val, 10000H) END
    ELSIF type.size = 4 THEN val := val MOD 100000000H;
      IF (type = B.int32Type) & (val >= 80000000H) THEN
        DEC(val, 100000000H)
      END
    END;
    x := B.NewConst(type, val)
  END;
  RETURN x
END TypeTransferConst;

(* Compile time constant operations *)
PROCEDURE FoldConst*(op: INTEGER;  x, y: B.Object): B.Object;
VAR val, xval, yval, i, k: INTEGER;  type: B.Type;  r1, r2: REAL;
    xstr8,  ystr8:  B.Str;   cb1, cb2: CHAR;
BEGIN
  IF (op >= S.eql) & (op <= S.in) THEN
    IF (x IS B.Const) & (y IS B.Const) & (x.type # B.realType) THEN
      xval := x(B.Const).val;  yval := y(B.Const).val;
      IF (op = S.eql) & (xval = yval) OR (op = S.neq) & (xval # yval)
      OR (op = S.gtr) & (xval > yval) OR (op = S.geq) & (xval >= yval)
      OR (op = S.lss) & (xval < yval) OR (op = S.leq) & (xval <= yval)
      OR (op = S.in) & (xval IN SYSTEM.VAL(SET,yval))
      THEN val := 1 ELSE val := 0
      END
    ELSIF (x IS B.Const) & (y IS B.Const) & (x.type = B.realType) THEN
      xval := x(B.Const).val;  yval := y(B.Const).val;
      r1 := SYSTEM.VAL(REAL, xval);  r2 := SYSTEM.VAL(REAL, yval);
      IF (op = S.eql) & (r1 = r2) OR (op = S.neq) & (r1 # r2)
      OR (op = S.gtr) & (r1 > r2) OR (op = S.geq) & (r1 >= r2)
      OR (op = S.lss) & (r1 < r2) OR (op = S.leq) & (r1 <= r2)
      THEN val := 1 ELSE val := 0
      END
    ELSIF (x IS B.Str) & (y IS B.Str) THEN
      xstr8 := x(B.Str);  ystr8 := y(B.Str);
      IF (xstr8.bufpos >= 0) & (ystr8.bufpos >= 0) THEN
        i := xstr8.bufpos;  k := ystr8.bufpos;
        cb1 := B.strBuf[i];  cb2 := B.strBuf[k];
        WHILE (cb1 = cb2) & (cb1 # 0X) DO
          INC(i);  INC(k);  cb1 := B.strBuf[i];  cb2 := B.strBuf[k]
        END;
        IF (op = S.eql) & (cb1 = cb2) OR (op = S.neq) & (cb1 # cb2)
        OR (op = S.gtr) & (cb1 > cb2) OR (op = S.geq) & (cb1 >= cb2)
        OR (op = S.lss) & (cb1 < cb2) OR (op = S.leq) & (cb1 <= cb2)
        THEN val := 1 ELSE val := 0
        END
      END
    END;
    type := B.boolType
  ELSIF (x IS B.Const) & (y IS B.Const) THEN
    xval := x(B.Const).val;  yval := y(B.Const).val;
    IF x.type.form = B.tInt THEN type := B.intType;
      IF op = S.plus THEN val := xval + yval
      ELSIF op = S.minus THEN val := xval - yval
      ELSIF op = S.times THEN val := xval * yval
      ELSIF (op = S.div) OR (op = S.mod) THEN
        IF yval <= 0 THEN S.Mark("invalid divisor")
        ELSIF op = S.div THEN val := xval DIV yval
        ELSE val := xval MOD yval
        END
      END
    ELSIF x.type = B.setType THEN type := B.setType;
      IF op = S.plus THEN
        val := ORD(SYSTEM.VAL(SET, xval) + SYSTEM.VAL(SET, yval))
      ELSIF op = S.minus THEN
        val := ORD(SYSTEM.VAL(SET, xval) - SYSTEM.VAL(SET, yval))
      ELSIF op = S.times THEN
        val := ORD(SYSTEM.VAL(SET, xval) * SYSTEM.VAL(SET, yval))
      ELSIF op = S.rdiv THEN
        val := ORD(SYSTEM.VAL(SET, xval) / SYSTEM.VAL(SET, yval))
      END
    ELSIF x.type = B.realType THEN type := B.realType;
      r1 := SYSTEM.VAL(REAL, xval);  r2 := SYSTEM.VAL(REAL, yval);
      IF op = S.plus THEN val := SYSTEM.VAL(INTEGER, r1 + r2)
      ELSIF op = S.minus THEN val := SYSTEM.VAL(INTEGER, r1 - r2)
      ELSIF op = S.times THEN val := SYSTEM.VAL(INTEGER, r1 * r2)
      ELSIF op = S.rdiv THEN val := SYSTEM.VAL(INTEGER, r1 / r2)
      END
    ELSIF x.type = B.boolType THEN type := B.boolType;
      IF op = S.or THEN
        IF (xval = 1) OR (yval = 1) THEN val := 1 ELSE val := 0 END
      ELSIF op = S.and THEN
        IF (xval = 1) & (yval = 1) THEN val := 1 ELSE val := 0 END
      END
    END
  END;
  x := B.NewConst(type, val);
  RETURN x
END FoldConst;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Init*;
BEGIN
  varSize    := 0;
  staticSize := 128;  (* Leave 128 bytes for standard function addresses *)
  procList   := NIL;
  curProc    := NIL;

  B.intType.size    := 8;  B.intType.align    := 8;
  B.byteType.size   := 1;  B.byteType.align   := 1;
  B.charType.size   := 1;  B.charType.align   := 1;
  B.boolType.size   := 1;  B.boolType.align   := 1;
  B.setType.size    := 8;  B.setType.align    := 8;
  B.realType.size   := 8;  B.realType.align   := 8;
  B.nilType.size    := 8;  B.nilType.align    := 8;
  B.card16Type.size := 2;  B.card16Type.align := 2;
  B.card32Type.size := 4;  B.card32Type.align := 4;
  B.int8Type.size   := 1;  B.int8Type.align   := 1;
  B.int16Type.size  := 2;  B.int16Type.align  := 2;
  B.int32Type.size  := 4;  B.int32Type.align  := 4;

  adrOfNEW          := 120;
  adrOfPtrTable     := 112;
  adrOfStackPtrList := 104;
  MessageBoxA       := 88;
  (* Note: offset 80 is reserved for the RVA of the static data from the      *)
  (* Windows module base.                                                     *)
  ExitProcess       := 16;

  debug := Files.New(".DebugInfo");  Files.Set(rider, debug, 0)
END Init;

PROCEDURE Generate*(VAR modinit: B.Node);
VAR initadr: INTEGER;
BEGIN
  (* Pass 1 *)
  pass := 1;  pc := 0;  Pass1(modinit);

  (* Pass 2 *)
  pass := 2;  curProc := procList;  pc := 0;
  WHILE curProc # NIL DO Procedure;  curProc := curProc.next END;

  (* Pass 3 *)
  pass := 3;  curProc := procList;  pc := 0;
  WHILE curProc # NIL DO Procedure;  curProc := curProc.next END;

  IF modInitProc # NIL THEN initadr := modInitProc.adr ELSE initadr := -1 END;
  ObjectX64.Write(debug, code, pc, initadr, staticSize, varSize, modPtrTable)
END Generate;

PROCEDURE Cleanup*;
VAR i: INTEGER;
BEGIN
  debug := NIL;  Files.Set(rider, NIL, 0);

  procList   := NIL;  curProc     := NIL;  modInitProc := NIL
END Cleanup;

BEGIN
  MakeItem0 := MakeItem
END Generator.