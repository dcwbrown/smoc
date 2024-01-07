MODULE ORG;  (* DCWB June 2023; Code generator for X64 *)

(* Originally: N.Wirth, 16.4.2016 / 4.4.2017 / 17.9.2018  Oberon compiler; code generator for RISC*)

(*
    It may make sense to have a separate 'local' mode, independent of Eadr:
    Currently there are a lot of places which do:
        IF (mode = Eadr) & (n = RSP) THEN INC(offset, X64.SPO * 8) END;
    This allows mode Eadr to be used both for local variable access, and for
    direct access to the stack when manipulating it at a lower level.
    With a local mode x.n would be a stack offset, the same as Stkind, and
    would always have SPO applied, while Eadr, n = RSP would never have SPO
    applied.
*)

IMPORT SYSTEM, Files, ORS, ORB, X64, l := Listing, K := Kernel;

CONST
  WordSize* = ORB.WordSize;
  MaxImpMod = 32;     (* Max no. imported modules *)


  (* System library procedure indices *)
  NewProc                = 0;
  (* error traps *)
  AssertionFailureProc   = 1;
  ArraySizeMismatchProc  = 2;
  UnterminatedStringProc = 3;

  (* Trace flags *)
  TraceAny*      = 0;
  TraceMakeItem* = 1;

  (* Promote X64 conditions to ORG *)
  CF  = X64.CF;   CT  = X64.CT;
  CO  = X64.CO;   CNO = X64.CNO;  CC  = X64.CC;   CNC = X64.CNC;
  CZ  = X64.CZ;   CNZ = X64.CNZ;  CNA = X64.CNA;  CA  = X64.CA;
  CS  = X64.CS;   CNS = X64.CNS;  CP  = X64.CP;   CNP = X64.CNP;
  CL  = X64.CL;   CNL = X64.CNL;  CNG = X64.CNG;  CG  = X64.CG;

  (* Promote X64 reg names to ORG *)
  RAX = X64.RAX;  RCX = X64.RCX;  RDX = X64.RDX;  RBX = X64.RBX;
  RSP = X64.RSP;  RBP = X64.RBP;  RSI = X64.RSI;  RDI = X64.RDI;

  AllFree* = {0..3,5..15};  (* RSP (4) is reserved *)

  (* Promote X64 item modes to ORG *)
  (* Mode                   Description        n                   offset                      *)
  (*                        -----------------  ------------------  --------------------------- *)
  Cond    = X64.Cond;    (* Condition          CF, CT, CO..CG      0                           *)
  Const   = X64.Const;   (* Constant           value               0                           *)
  Reg     = X64.Reg;     (* Register           register (0-15)     0                           *)
  Stkind  = X64.Stkind;  (* Stack indirect     stack offset        0                           *)
  Eadr    = X64.Eadr;    (* Effective address  base register       relative to base            *)
  Code    = X64.Code;    (* Code offset        0                   code offset                 *)
  String  = X64.String;  (* String offset      string length       current string block offset *)
  Global  = X64.Global;  (* VAR offset         0                   VAR block offset            *)
  Import  = X64.Import;  (* Import             16/modno, 16/impno  0                           *)

  (* Dyadic ALU operations, values correspond to x86 instruction set *)
  Plus*  = X64.Plus;
  Or*    = X64.Or;
  And*   = X64.And;
  Minus* = X64.Minus;
  Xor*   = X64.Xor;
  Cmp*   = X64.Cmp;


TYPE
  Item = X64.Item;

VAR
  Str:     ARRAY 1024 OF CHAR;  (* Max strings per procedure/module body *)
  Strx:    INTEGER;
  StrFix:  ARRAY 128 OF SYSTEM.CARD32;  (* Code offsets needing fixup *)
  StrFInx: INTEGER;

  Varsize: INTEGER;      (* Module global VAR size *)
  VarFix:  ARRAY 1024 OF SYSTEM.CARD32;
  VarFInx: INTEGER;

  ImpFix:  ARRAY 2048 OF RECORD offset, fix: SYSTEM.CARD32 END;
  ImpFInx: INTEGER;

  Free*:   SET;      (* Free registers *)


(* -------------------------------- Tracing --------------------------------- *)

  Trflags:  SET;      (* Listing trace options *)
  Trc*:     l.Buffer;
  Disasmpc: INTEGER;


PROCEDURE Trace*(kind: INTEGER; str: ARRAY OF CHAR);
BEGIN
  IF kind IN Trflags THEN
    l.StartRightCol;
    Trc.p := 0;  l.s(str, Trc)  (* Start trace line *)
  ELSE
    Trc.p := LEN(Trc.c) + 1  (* Suppress output on this line *)
  END
END Trace;

PROCEDURE Disassemble(mode: INTEGER; comment: ARRAY OF CHAR);
(* Disassemble up to X64.PC, or opcode 2EH, whichever comes first *)
BEGIN
  IF ORB.ListAssembly & (Disasmpc < X64.PC) & (X64.Text[Disasmpc] # 2EH) THEN
    IF mode = X64.Import THEN INC(mode, ImpFix[ImpFInx-1].fix * 256) END;
    l.DisassembleInstruction(Disasmpc, mode, comment);
    WHILE (Disasmpc < X64.PC) & (X64.Text[Disasmpc] # 2EH) DO
      l.DisassembleInstruction(Disasmpc, mode, "");
    END
  END;
  Disasmpc := X64.PC
END Disassemble;


PROCEDURE Min(a, b: INTEGER): INTEGER;
BEGIN IF a < b THEN b := a END;  RETURN b END Min;

PROCEDURE Max(a, b: INTEGER): INTEGER;
BEGIN IF a > b THEN b := a END;  RETURN b END Max;


(* ------------------------- Instruction generation ------------------------- *)

PROCEDURE Prefices*(mode, reg, size, n, index: INTEGER);
VAR rex: INTEGER;
BEGIN  rex := 0;
  IF ~(mode IN {Reg, Eadr}) THEN n := -1 END;  (* Cases where n is not a register *)
  (* Emit prefices *)
  IF size  =  2 THEN X64.Emit(66H) END;
  IF size  >  4 THEN INC(rex, 8)   END;
  IF reg   >= 8 THEN INC(rex, 4)   END;
  IF index >= 8 THEN INC(rex, 2)   END;
  IF n     >= 8 THEN INC(rex, 1)   END;

  IF (rex # 0) OR (size = 1) & (reg IN {4,5,6,7}) THEN X64.Emit(40H + rex) END;
END Prefices;


(*----  mod/reg/rm for effective addresses

  base  index  offset  mod  rm    s      i      b     disp
  ----  -----  ------  ---  ----  -----  -----  ----  -------
    N     N       N    0    5                         32/0
    N     N       Y    0    5                         32/offset
    N     Y       N    0    4     scale  index  5                  *1
    N     Y       Y    2    4     scale  index  base  32/offset    *1
    RSP   N       N    0    4     scale  4      4
    RBP   N       N    1    5                         8/0
    Y     N       N    0    base
    RSP   N      8/    1    4     0      4      4     8/offset
    RSP   N     32/    2    4     0      4      4     32/offset
    Y     N      8/    1    base                      8/offset
    Y     N     32/    2    base                      32/offset
    Y     Y       N    0    4     scale  index  base               *1
    Y     Y       Y    2    4     scale  index  base  32/offset    *1

*1 - there is no encoding for index = RSP.

*)

PROCEDURE ModRegRm*(mode, reg, n, index, scale, offset, immsize: INTEGER);
VAR
  needsib:  BOOLEAN;
  dispsize: INTEGER;
  base:     INTEGER;
  mod:      INTEGER;
  rm:       INTEGER;
  modno:    INTEGER;
BEGIN
  reg := reg MOD 8;

  IF mode = Reg THEN

    (* reg to reg *)
    ASSERT(n >= 0);  ASSERT(index < 0);  ASSERT(offset = 0);
    X64.Emit(0C0H + reg*8 + n MOD 8)

  ELSE

    (* reg from/to mem *)

    ASSERT(mode IN {Eadr, Code, String, Global, Import});
    ASSERT(index # RSP);  (* Not supported by X64 *)
    IF mode = Eadr THEN base := n MOD 8 ELSE base := -1 END;
    IF index >= 0 THEN index := index MOD 8 END;

    (* Determine presence and size of disp, ignoring special cases *)
    dispsize := 4;  (* Most cases including all fixup cases are 32 bit *)
    IF mode = Eadr THEN
      IF    offset = 0                        THEN dispsize := 0
      ELSIF (offset >= -80H) & (offset < 80H) THEN dispsize := 1
      END
    END;

    (* Determine mod based on disp size, ignoring special cases *)
    IF    dispsize = 0 THEN mod := 0
    ELSIF dispsize = 1 THEN mod := 1
    ELSE                    mod := 2
    END;

    (* Adjust for sib and special cases *)
    needsib := FALSE;
    rm := base;
    IF index < 0 THEN
      IF    base < 0                    THEN rm := 5;  dispsize := 4;  mod := 0
      ELSIF (base = RBP) & (offset = 0) THEN rm := 5;  dispsize := 1;  mod := 1
      ELSIF base = RSP                  THEN rm := 4;  needsib  := TRUE; index := 4;
      END;
    ELSE
      rm := 4;  needsib := TRUE;
      IF base < 0 THEN dispsize := 4;  mod := 0;  base := 5 END;
    END;

    X64.Emit(mod*64 + reg*8 + rm);          (* modregrm *)
    IF needsib THEN
      X64.Emit(scale*64 + index*8 + base);  (* sib *)
    END;

    (* Fixup Code/String/Global/Import offsets *)
    IF mode IN {Code, String, Global, Import} THEN
      ASSERT(dispsize = 4);
      DEC(offset, X64.PC + 4 + immsize)  (* Make offset relative to next instruction *)
    END;
    IF    mode = String THEN StrFix[StrFInx] := X64.PC;  INC(StrFInx)
    ELSIF mode = Global THEN VarFix[VarFInx] := X64.PC;  INC(VarFInx)
    ELSIF mode = Import THEN ImpFix[ImpFInx].offset := X64.PC;
                             ImpFix[ImpFInx].fix := n; INC(ImpFInx)
    END;

    IF dispsize = 1 THEN
      ASSERT((offset >= -80H) & (offset < 80H));
      X64.Emit(offset)
    ELSIF dispsize = 4 THEN
      ASSERT((offset >= -80000000H) & (offset < 80000000H));
      X64.EmitBytes(4, offset)
    ELSE
      ASSERT(dispsize = 0)
    END
  END
END ModRegRm;

PROCEDURE OpModRegRm(op, mode, reg, size, n, index, scale, offset, immsize: INTEGER);
BEGIN
  IF ~(mode IN {Reg, Eadr, Code, String, Global, Import}) THEN
    Trace(TraceAny, "** mode "); l.i(mode, Trc); l.sl(" **", Trc);
  END;
  ASSERT(mode IN {Reg, Eadr, Code, String, Global, Import});
  Prefices(mode, reg, size, n, index);
  IF op >= 100H THEN X64.Emit(op DIV 100H) END;
  X64.Emit(op MOD 100H);
  ModRegRm(mode, reg, n, index, scale, offset, immsize)
END OpModRegRm;

PROCEDURE OpItem*(op, reg, size, immsize: INTEGER; x: Item);
VAR offset: INTEGER;
BEGIN
  offset := x.offset;
  IF (x.mode = Eadr) & (x.n = RSP) THEN INC(offset, X64.SPO * 8) END;
  OpModRegRm(op, x.mode, reg, size, x.n, x.index, x.scale, offset, immsize)
END OpItem;


(* --------------------------- Register tracking ---------------------------- *)


PROCEDURE FirstReg*(s: SET): INTEGER;  (* Returns first reg in SET, or -1 if none *)
VAR f: INTEGER;
BEGIN
  f := 0;
  WHILE (f < 16) & ~(f IN s) DO INC(f) END;
  IF f >= 16 THEN f := -1 END
RETURN f END FirstReg;

PROCEDURE IsDataReg*(r: INTEGER): BOOLEAN;
BEGIN ASSERT(r < 16); RETURN (r >= 0) & (r # RSP) END IsDataReg;

PROCEDURE ReserveReg*(r: INTEGER);
BEGIN ASSERT(r >= 0);  IF IsDataReg(r) THEN EXCL(Free, r) END END ReserveReg;

PROCEDURE ReleaseReg*(r: INTEGER);
BEGIN IF IsDataReg(r) THEN INCL(Free, r) END END ReleaseReg;

PROCEDURE FirstFreeReg*(): INTEGER;
VAR r: INTEGER;
BEGIN r := FirstReg(Free);
  IF r < 0 THEN ORS.Mark("Out of registers") END;
RETURN r END FirstFreeReg;

PROCEDURE FindFreeReg*(desired: INTEGER; allow: SET): INTEGER;
(* Return desired if free, else return first free in allow
   Common usage like FindFreeReg(RSI, -{RAX, RCX, RDI}):
   return si if available, else anything but ax, cx or di *)
VAR result: INTEGER;
BEGIN
  IF desired IN Free THEN
    result := desired
  ELSE
    result := FirstReg(Free * allow)
  END
RETURN result END FindFreeReg;

PROCEDURE ReserveNextFree*(): INTEGER;
VAR n: INTEGER;
BEGIN n := FirstFreeReg();
  IF n < 0 THEN ORS.Mark("out of registers");  n := 15
  ELSE ReserveReg(n) END
RETURN n END ReserveNextFree;

PROCEDURE FindAndReserveReg(desired: INTEGER; allow: SET): INTEGER;
VAR n: INTEGER;
BEGIN n := FindFreeReg(desired, allow);
  IF n < 0 THEN ORS.Mark("out of registers");  n := 15
  ELSE ReserveReg(n) END
RETURN n END FindAndReserveReg;

PROCEDURE ClearRegs; BEGIN Free := AllFree END ClearRegs;

PROCEDURE CheckRegs*(msg: ARRAY OF CHAR);
BEGIN
  IF (Free # AllFree) OR (X64.SPO # 0) THEN
    l.s("CheckRegs(", Trc); l.s(msg, Trc);
    l.s("): Used ", Trc); l.Set(AllFree - Free, Trc);
    l.s(", Free: ", Trc); l.Set(Free, Trc);
    l.s(", SPO ",   Trc); l.i(X64.SPO, Trc); l.sl(".", Trc)
  END;
  IF Free # AllFree           THEN ORS.Mark("Reg Stack"); ClearRegs;  ASSERT(FALSE) END;
  IF X64.PC >= X64.MaxPC - 40 THEN ORS.Mark("program too long");      ASSERT(FALSE) END;
  IF X64.SPO # 0              THEN ORS.Mark("SPO error"); X64.ClearStack; ASSERT(FALSE) END
END CheckRegs;

PROCEDURE FreeRegs(VAR x: Item);
BEGIN
  IF x.mode IN {Reg, Eadr} THEN ReleaseReg(x.n) END;
  ReleaseReg(x.index);
  X64.ClearMode(x)
END FreeRegs;

(* ---------------------------- Code generation ----------------------------- *)


PROCEDURE SetCondition(VAR x: Item; cond: INTEGER);
BEGIN
  X64.ClearMode(x);
  ASSERT((cond >= CF) & (cond <= CG) & ((cond <= CT) OR (cond >= CO)));
  x.mode  := Cond;
  x.n     := cond;
  x.scale := 0;       (* prev instr needing patching to after this instruction *)
  x.index := 0;       (* prev instruction needing patch to this instruction *)
END SetCondition;

PROCEDURE DeparToReg(reg: INTEGER; VAR x: Item);
BEGIN
  ASSERT(x.mode = Stkind);
  OpModRegRm(8BH, Eadr, reg, 8, RSP, -1, 0, x.n + 8 * X64.SPO, 0);  (* mov reg,[rsp+x] *)
  x.mode := Eadr;
  x.n    := reg;
  Disassemble(0, "Depar")
END DeparToReg;

PROCEDURE Depar(VAR x: Item);
BEGIN
  IF x.mode = Stkind THEN DeparToReg(ReserveNextFree(), x) END
END Depar;


PROCEDURE LoadFlags(VAR x: Item);  (* Set Z condition flag based on x *)
BEGIN
  ASSERT(x.type.form = ORB.Bool);
  IF x.mode = Const THEN
    ASSERT((x.n = 0) OR (x.n = 1));
    SetCondition(x, x.n)
  ELSE
    Depar(x);
    IF x.mode = Reg THEN  (* use test reg,reg *)
      OpModRegRm(85H, Reg, x.n, 8, x.n, -1, 0, 0, 0)
    ELSE  (* use cmp mem,0 *)
      ASSERT(x.mode IN {Eadr, Code, String, Global, Import});
      OpItem(83H, 7, x.type.size, 1, x);  X64.Emit(0)
    END;
    Disassemble(x.mode, "LoadFlags");
    FreeRegs(x);
    SetCondition(x, CNZ)
  END
END LoadFlags;


PROCEDURE InvertedCondition(cond: INTEGER): INTEGER;
BEGIN RETURN SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET, cond) / {0})
END InvertedCondition;


PROCEDURE LoadImmediate(reg, val: INTEGER);
BEGIN
  ASSERT(reg >= 0);
  IF val = 0 THEN
    OpModRegRm(31H, Reg, reg, 4, reg, -1, 0, 0, 0)  (* xor reg,reg *)
  ELSIF (val > 0) & (val < 100000000H) THEN   (* Load 32 bit positive value as 32 bit load with zero extension *)
    Prefices(Reg, -1, 4, reg, -1);
    X64.Emit(0B8H + reg MOD 8);
    X64.EmitBytes(4, val);
  ELSIF (val < 0) & (val >= -80000000H) THEN  (* Load 32 bit negative value with sign extended move *)
    OpModRegRm(0C7H, Reg, 0, 8, reg, -1, 0, 0, 4);
    X64.EmitBytes(4,val);
  ELSE                                        (* Need full 64 bit literal *)
    Prefices(Reg, reg, 8, -1, -1);
    X64.Emit(0B8H + reg MOD 8);
    X64.EmitBytes(8, val);
  END
END LoadImmediate;

PROCEDURE MoveReg(r1, r2: INTEGER);  (* r1 := r2 *)
BEGIN
  ASSERT(r1 >= 0);  ASSERT(r2 >= 0);  ASSERT(r1 # r2);
  OpModRegRm(89H, Reg, r2, 8, r1, -1, 0, 0, 0)
END MoveReg;

PROCEDURE LoadMem(reg, mode: INTEGER; signed: BOOLEAN; size, n, index, scale, offset: INTEGER);
BEGIN
  Trace(TraceAny, "LoadMem(");  l.Reg(8, reg, Trc);
  l.s(", ", Trc);         l.Mode(mode, Trc);
  l.s(", signed ", Trc);  l.Bool(signed, Trc);
  l.s(", size ", Trc);    l.i(size, Trc);
  l.s(", n ", Trc);       l.i(n, Trc);
  l.s(", index ", Trc);   l.i(index, Trc);
  l.s(", scale ", Trc);   l.i(scale, Trc);
  l.s(", offset ", Trc);  l.i(offset, Trc);  l.l(Trc);

  IF (mode = Eadr) & (n = RSP) THEN INC(offset, X64.SPO * 8) END;

  ASSERT(size IN {1, 2, 4, 8});
  IF signed THEN
    IF    size = 1 THEN OpModRegRm(0FBEH, mode, reg, 8, n, index, scale, offset, 0)  (* movsx  r64, r/m8  *)
    ELSIF size = 2 THEN OpModRegRm(0FBFH, mode, reg, 8, n, index, scale, offset, 0)  (* movsx  r64, r/m16 *)
    ELSIF size = 4 THEN OpModRegRm(63H,   mode, reg, 8, n, index, scale, offset, 0)  (* movsxd r64, r/m32 *)
    ELSIF size = 8 THEN OpModRegRm(8BH,   mode, reg, 8, n, index, scale, offset, 0)  (* mov    r64, r/m64 *)
    END
  ELSE (* unsigned *)
    IF    size = 1 THEN OpModRegRm(0FB6H, mode, reg, 8, n, index, scale, offset, 0)  (* movzx  r64, r/m8  *)
    ELSIF size = 2 THEN OpModRegRm(0FB7H, mode, reg, 8, n, index, scale, offset, 0)  (* movzx  r64, r/m16 *)
    ELSIF size = 4 THEN OpModRegRm(8BH,   mode, reg, 4, n, index, scale, offset, 0)  (* mov    r32, r/m32 *)
    ELSIF size = 8 THEN OpModRegRm(8BH,   mode, reg, 8, n, index, scale, offset, 0)  (* mov    r64, r/m64 *)
    END
  END
END LoadMem;

PROCEDURE LoadCondition(reg, c: INTEGER);  (* Set register to 0/1 based on condition flags *)
BEGIN
  ASSERT((reg >= 0) & (reg <= 15));
  OpModRegRm(0F90H + c MOD 16, Reg, 0,   4, reg, -1, 0, 0, 0);  (* setcc byte-reg           *)
  OpModRegRm(0FB6H,            Reg, reg, 4, reg, -1, 0, 0, 0);  (* movzx qword-reg,byte-reg *)
  Disassemble(0, "LoadCondition")
END LoadCondition;


PROCEDURE LoadReg(reg: INTEGER; VAR x: Item);
BEGIN
  IF x.mode = Stkind THEN DeparToReg(reg, x) END;
  IF x.mode = Cond THEN
    IF x.n < 2 THEN LoadImmediate(reg, x.n) ELSE LoadCondition(reg, x.n) END;
    x.index := -1;  x.offset := 0
  ELSIF x.mode = Const THEN
    LoadImmediate(reg, x.n)
  ELSIF x.mode = Reg THEN
    IF reg # x.n THEN MoveReg(reg, x.n); ReleaseReg(x.n) END
  ELSIF x.mode IN {Eadr, Code, String, Global, Import} THEN
    LoadMem(reg, x.mode, X64.IsSigned(x.type), x.type.size, x.n, x.index, x.scale, x.offset)
  ELSE ASSERT(FALSE)
  END;
  FreeRegs(x);  x.mode := Reg;  x.n := reg;  ReserveReg(x.n);
  Disassemble(x.mode, "Load")
END LoadReg;

PROCEDURE LoadAddressReg(reg: INTEGER; VAR x: Item);
VAR offset: INTEGER;
BEGIN
  offset := x.offset;
  IF (x.mode = Eadr) & (x.n = RSP) THEN INC(offset, 8 * X64.SPO) END;
  IF x.mode = Stkind THEN
    DeparToReg(reg, x)
  ELSIF (x.mode = Eadr) & (x.index < 0) & (offset = 0) THEN
    MoveReg(reg, x.n)
  ELSIF (x.mode IN {Eadr, Code, String, Global, Import}) THEN
    OpModRegRm(8DH, x.mode, reg, 8, x.n, x.index, x.scale, offset, 0);  (* lea *)
    Disassemble(x.mode, "LoadAddress")
  ELSE
    ASSERT(FALSE)
  END
END LoadAddressReg;

PROCEDURE Load(VAR x: Item);
VAR reg: INTEGER;
BEGIN
  IF x.mode = Reg THEN reg := x.n ELSE reg := ReserveNextFree() END;
  IF (x.mode = Code) & (x.type.form = ORB.Proc) THEN
    LoadAddressReg(reg, x); FreeRegs(x);  x.mode := Reg;  x.n := reg;  ReserveReg(x.n);
  ELSE
    LoadReg(reg, x)
  END
END Load;

PROCEDURE LoadAddress(VAR x: Item);
VAR reg: INTEGER;
BEGIN
  IF x.mode = Reg THEN reg := x.n ELSE reg := ReserveNextFree() END;
  LoadAddressReg(reg, x);
  FreeRegs(x);  x.mode := Reg;  x.n := reg
END LoadAddress;


PROCEDURE LoadStructure(rbase, rlength: INTEGER; VAR x: Item);  (* length returned as byte count *)
BEGIN
  ASSERT(x.type.form IN {ORB.Array, ORB.Record, ORB.String});
  ASSERT(x.mode IN {Stkind, Eadr, Code, String, Global, Import});
  IF (x.type.form = ORB.Array) & (x.type.len < 0) THEN
    (* Open array parameter *)
    ASSERT(x.mode = Stkind);
    OpModRegRm(8BH, Eadr, rbase,   8, RSP, -1, 0, x.n + 8 * X64.SPO,     0);  (* mov reg,[rsp+x] *)
    OpModRegRm(8BH, Eadr, rlength, 8, RSP, -1, 0, x.n + 8 * X64.SPO + 8, 0);  (* mov reg,[rsp+x] *)
    ASSERT(x.type.base.size >= 0);
    IF x.type.base.size > 1 THEN  (* multiply length by type size to get byte count *)
      ASSERT(x.type.base.size < 80000000H);
      (* TODO - optimise powers of 2 with a shift *)
      ASSERT(FALSE);  (* Why does this code involve the value of x? Should just be rlen *= size *)
      IF x.type.base.size >= 80H THEN
        OpItem(69H, rlength, 8, 4, x);  X64.EmitBytes(4, x.type.base.size)
      ELSE
        OpItem(6BH, rlength, 8, 1, x);  X64.Emit(x.type.base.size)
      END;
    END
  ELSE
    (* Record or array with length known at compile time, or string *)
    LoadAddressReg(rbase, x);
    IF x.type.form = ORB.String THEN
      LoadImmediate(rlength, x.n)
    ELSE
      LoadImmediate(rlength, x.type.size)  (* array length in bytes *)
    END
  END;
  FreeRegs(x);
  x.mode := Eadr;  x.n := rbase;  x.index := rlength;
  ReserveReg(rbase);   ReserveReg(rlength);
  Disassemble(x.mode, "LoadStructure")
END LoadStructure;


PROCEDURE StoreRegToMem*(sreg, mode, size, n, index, scale, offset: INTEGER);
VAR op: INTEGER;
BEGIN
  IF size = 1 THEN op := 088H ELSE op := 089H END;
  OpModRegRm(op, mode, sreg, size, n, index, scale, offset, 0);
  Disassemble(mode, "StoreRegToMem")
END StoreRegToMem;

PROCEDURE StoreReg(reg: INTEGER; VAR x: Item);
BEGIN
  ASSERT((reg >= 0) & (reg <= 15));
  (*
  Trace(TraceAny, "StoreReg(reg: "); l.Reg(8, reg, Trc);
  l.s(", x: ", Trc);                 l.Item(x, Trc); l.sl(")", Trc);
  *)
  StoreRegToMem(reg, x.mode, x.type.size, x.n, x.index, x.scale, x.offset)
END StoreReg;

PROCEDURE StoreImmediateToMem(imm, mode, size, n, index, scale, offset: INTEGER);
VAR op, reg, immsize: INTEGER;
BEGIN
  IF (imm >= -80000000H) & (imm < 80000000H) THEN  (* imm fits 32 bits *)
    IF size = 1 THEN op := 0C6H ELSE op := 0C7H END;
    immsize := Min(size, 4);
    OpModRegRm(op, mode, 0, size, n, index, scale, offset, immsize);
    X64.EmitBytes(immsize, imm);
    Disassemble(mode, "StoreImmediate")
  ELSE  (* 64 bit immediate - load immediate then store reg *)
    reg := FirstFreeReg();
    LoadImmediate(reg, imm);
    StoreRegToMem(reg, mode, size, n, index, scale, offset);
  END
END StoreImmediateToMem;

PROCEDURE StoreImmediate(imm: INTEGER; VAR x: Item);
VAR offset: INTEGER;
BEGIN
  offset := x.offset;
  IF (x.mode = Eadr) & (x.n = RSP) THEN INC(offset, X64.SPO * 8) END;
  StoreImmediateToMem(imm, x.mode, x.type.size, x.n, x.index, x.scale,  offset);
END StoreImmediate;

(*----*)

PROCEDURE PushReg*(r: INTEGER);
BEGIN Prefices(Reg, r, 4, -1, -1); X64.Emit(50H + r MOD 8) END PushReg;

PROCEDURE PopReg*(r: INTEGER);
BEGIN Prefices(Reg, r, 4, -1, -1); X64.Emit(58H + r MOD 8) END PopReg;

PROCEDURE PushImmediate*(i: INTEGER);
VAR reg: INTEGER;
BEGIN
  IF (i >= -80H) & (i < 80H) THEN
    X64.Emit(6AH);  X64.Emit(i)
  ELSIF (i >= -80000000H) & (i < 80000000H) THEN
    X64.Emit(68H);  X64.EmitBytes(4, i)
  ELSE
    reg := FirstFreeReg();
    Prefices(Reg, reg, 8, -1, -1); X64.Emit(0B8H + reg MOD 8); X64.EmitBytes(8, i);  (* Load literal *)
    Prefices(Reg, reg, 4, -1, -1); X64.Emit(050H + reg MOD 8);                       (* Push reg *)
  END
END PushImmediate;

PROCEDURE PushMem*(mode: INTEGER; signed: BOOLEAN; size, n, index, scale, offset: INTEGER);
VAR reg: INTEGER;
BEGIN
  IF size < 8 THEN
    reg := FirstFreeReg();
    LoadMem(reg, mode, signed, size, n, index, scale, offset);
    PushReg(reg)
  ELSE
    Prefices(mode, -1, 8, n, index);
    X64.Emit(0FFH);  ModRegRm(mode, 6, n, index, scale, offset, 0)
  END
END PushMem;

PROCEDURE PushAdr*(VAR x: Item);
BEGIN
  IF x.mode = Stkind THEN
    PushMem(Eadr, FALSE, 8, RSP, -1, 0, x.n + 8 * X64.SPO)
  ELSIF (x.mode = Eadr)
      & (x.index < 0) & (x.offset = 0)
      & ((x.n # RSP) OR (X64.SPO = 0)) THEN
    PushReg(x.n)
  ELSIF x.mode IN {Eadr, Code, String, Global, Import} THEN
    LoadAddress(x);  PushReg(x.n)
  ELSE
    ASSERT(FALSE)
  END;
  FreeRegs(x)
END PushAdr;

PROCEDURE Push*(VAR x: Item);
VAR reg, offset: INTEGER;
BEGIN
  IF    (x.mode = Code) & (x.type.form = ORB.Proc) THEN PushAdr(x)
  ELSIF x.mode = Const THEN PushImmediate(x.n)
  ELSIF x.mode = Reg   THEN PushReg(x.n)
  ELSIF x.mode = Cond THEN
    IF x.n < 2 THEN PushImmediate(x.n) ELSE Load(x);  PushReg(x.n) END
  ELSIF x.mode IN {Stkind, Eadr, Code, String, Global, Import} THEN
    Depar(x);
    offset := x.offset;
    IF (x.mode = Eadr) & (x.n = RSP) THEN INC(offset, X64.SPO * 8) END;
    PushMem(x.mode, X64.IsSigned(x.type), x.type.size, x.n, x.index, x.scale, offset)
  ELSE
    ASSERT(FALSE)
  END;
  FreeRegs(x)
END Push;

(*----*)


PROCEDURE AluOpFromImmediate(op, mode, imm, size, n, index, scale, offset: INTEGER); (* dest := dest op imm *)
VAR immsize, immreg, incdec: INTEGER;
BEGIN
  ASSERT(mode IN {Reg, Eadr, Code, String, Global, Import});
  IF mode = Reg THEN size := 8 END;
  IF ((imm = 1) OR (imm = -1)) & ((op = Plus) OR (op = Minus)) THEN
    incdec := 0;
    IF op = Minus THEN incdec := 1 END;
    IF imm = -1 THEN incdec := 1 - incdec END;
    OpModRegRm(0FFH, mode, incdec, size, n, index, scale, offset, 0)  (* inc/dec dest *)
  ELSE
    IF (imm < -80000000H) OR (imm >= 80000000H) THEN
      immreg := FirstFreeReg();
      LoadImmediate(immreg, imm);                                                       (* mov immreg,imm64 *)
      OpModRegRm(op+1, mode, immreg, size, n, index, scale, offset, 0)                     (* op  dest,immreg  *)
    ELSIF size = 1 THEN
      OpModRegRm(80H, mode, op DIV 8, size, n, index, scale, offset, 1);  X64.Emit(imm)          (* op  dest8,imm8    *)
    ELSIF (imm >= -80H) & (imm < 80H) THEN
      OpModRegRm(83H, mode, op DIV 8, size, n, index, scale, offset, 1);  X64.Emit(imm)         (* op  dest,imm8     *)
    ELSE
      OpModRegRm(81H, mode, op DIV 8, size, n, index, scale, offset, 4);  X64.EmitBytes(4, imm)  (* op  dest,imm32    *)
    END
  END
END AluOpFromImmediate;

PROCEDURE AluOpToReg(  (* reg := reg op src *)
  op, mode, reg:                 INTEGER;
  signed:                        BOOLEAN;
  size, n, index, scale, offset: INTEGER);
VAR srcreg: INTEGER;
BEGIN
  ASSERT(mode IN {Reg, Eadr, Code, String, Global, Import});
  IF (mode # reg) & (size < 8) THEN
    srcreg := FirstFreeReg();
    LoadMem(srcreg, mode, signed, size, n, index, scale, offset);
    mode := Reg;  n := srcreg;  index := -1;  scale := 0;  offset := 0;
  END;
  OpModRegRm(op+3, mode, reg, 8, n, index, scale, offset, 0)
END AluOpToReg;

PROCEDURE IsDyadicNoOp(op: INTEGER; src: Item): BOOLEAN;
VAR result: BOOLEAN;
BEGIN result := FALSE;
  IF src.mode = Const THEN
    IF src.n = 0 THEN
      result := (op = Plus) OR (op = Minus) OR (op = Or) OR (op = Xor)
    ELSIF src.n = -1 THEN
      result := op = And
    END
  END
RETURN result END IsDyadicNoOp;

PROCEDURE Dyadic(op: INTEGER;  VAR dest, src: Item);  (* dest := dest op src *)
(* Note: If result value needs to be a register, Load(dest) before calling Dyadic *)
VAR opname: ARRAY 6 OF CHAR;  offset: INTEGER;
BEGIN
  IF    op = Plus  THEN opname := "Plus"  ELSIF op = Minus THEN opname := "Minus"
  ELSIF op = Cmp   THEN opname := "Cmp"   ELSIF op = And   THEN opname := "And"
  ELSIF op = Or    THEN opname := "Or"    ELSIF op = Xor   THEN opname := "Xor"
  ELSE  ASSERT(FALSE)
  END;
  ASSERT(op IN {Plus, Minus, Cmp, And, Or, Xor});
  Trace(TraceAny, "Dyadic(");  l.Op(op, Trc);
  l.s("; dest: ", Trc);  l.Item(dest, Trc);
  l.s(", src: ",  Trc);  l.Item(src, Trc); l.l(Trc);

  IF (dest.mode = Reg) & (dest.n IN Free) THEN
    l.s("** dest register ", Trc); l.Reg(8, dest.n, Trc); l.sl(" not reserved.", Trc);
  END;
  IF (src.mode = Reg) & (src.n IN Free) THEN
    l.s("** src register ", Trc); l.Reg(8, src.n, Trc); l.sl(" not reserved.", Trc);
  END;

  IF (src.mode = Const) & (dest.mode = Const) THEN

    ASSERT(op # Cmp);

    IF    op = Plus  THEN INC(dest.n, src.n)
    ELSIF op = Minus THEN DEC(dest.n, src.n)
    ELSIF op = And   THEN dest.n := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET,dest.n) * SYSTEM.VAL(SET,src.n));
    ELSIF op = Or    THEN dest.n := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET,dest.n) + SYSTEM.VAL(SET,src.n));
    ELSIF op = Xor   THEN dest.n := SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET,dest.n) / SYSTEM.VAL(SET,src.n));
    END

  ELSIF ~IsDyadicNoOp(op, src) THEN

    IF src.mode = Const THEN
      Depar(dest);
      offset := dest.offset;
      IF (dest.mode = Eadr) & (dest.n = RSP) THEN INC(offset, X64.SPO * 8) END;
      AluOpFromImmediate(op, dest.mode, src.n, dest.type.size, dest.n, dest.index, dest.scale, offset);
      Disassemble(dest.mode, opname)
    ELSE
      Depar(src);
      ASSERT(src.mode IN {Reg, Eadr, Code, String, Global, Import});
      offset := src.offset;
      IF (src.mode = Eadr) & (src.n = RSP) THEN INC(offset, X64.SPO * 8) END;
      IF dest.mode # Reg THEN Load(dest) END;
      AluOpToReg(op, src.mode, dest.n, X64.IsSigned(src.type), src.type.size, src.n, src.index, src.scale, offset);
      Disassemble(src.mode, opname)
    END;
    FreeRegs(src);

  END;

  IF (dest.mode = Reg) & (dest.n IN Free) THEN
    l.s("** Warning, register dest.n (", Trc); l.Reg(8, dest.n, Trc); l.sl(") not reserved **", Trc)
  END
END Dyadic;


(* ---- Item construction ---- *)

PROCEDURE ZeroFill(VAR obj: ARRAY OF BYTE);  VAR i: INTEGER;
BEGIN FOR i := 0 TO LEN(obj)-1 DO obj[i] := 0 END END ZeroFill;

PROCEDURE MakeItem*(VAR x: Item; y: ORB.Object; curlev: INTEGER);
VAR base: INTEGER;
BEGIN
  Trace(TraceMakeItem, "MakeItem(y: "); l.Object(y, Trc); l.s(", curlev: ", Trc);
  l.i(curlev, Trc); l.s(")", Trc);
  ASSERT(y.class IN {ORB.Const, ORB.Var, ORB.Par, ORB.SProc, ORB.Typ});

  ZeroFill(x);  X64.ClearMode(x);
  x.mode     := y.class;
  x.type     := y.type;
  x.readonly := y.rdo;

  IF y.class = ORB.Const THEN
    IF y.type.form = ORB.Proc THEN
      IF    y.lev > 0 THEN x.mode := Eadr;    x.n := RSP;  x.offset := y.val;
      ELSIF y.lev = 0 THEN x.mode := Code;    x.n := 0;    x.offset := y.val;
      ELSE                 x.mode := Import;  x.n := LSL(-y.lev, 16) + y.val;
      END
    ELSIF y.type.form = ORB.String THEN
      x.mode := String;  x.readonly := TRUE;  x.n := y.lev;  x.offset := y.val;
    ELSE
      x.mode := Const;  x.n := y.val
    END
  ELSIF y.class = ORB.Var THEN
      IF    y.lev > 0 THEN x.mode := Eadr;    x.n := RSP;  x.offset := y.val;
      ELSIF y.lev = 0 THEN x.mode := Global;  x.n := 0;    x.offset := y.val;
      ELSE(*y.lev < 0 *)   x.mode := Import;  x.n := LSL(-y.lev, 16) + y.val;
      END
  ELSIF y.class = ORB.Par THEN
    x.mode := Stkind;  x.n := y.val
  ELSE
    ASSERT(y.class IN {0, ORB.SProc, ORB.Typ})
  END;

  l.s(" -> ", Trc);  l.Item(x, Trc);  l.l(Trc)
END MakeItem;

PROCEDURE MakeConstItem*(VAR x: Item;  t: ORB.Type;  v: INTEGER);
BEGIN
  Trace(TraceMakeItem, "MakeConstItem, value: ");
  l.i(v, Trc); l.s(", type ", Trc); l.Type(t, Trc); l.l(Trc);
  ZeroFill(x);  X64.ClearMode(x);
  x.mode := Const;  x.type  := t;  x.n := v
END MakeConstItem;

PROCEDURE MakeRealItem*(VAR x: Item; v: REAL);
BEGIN
  Trace(TraceMakeItem, "MakeRealItem."); l.l(Trc);
  ZeroFill(x);  X64.ClearMode(x);
  x.mode := Const;  x.type  := ORB.realType;  x.n := SYSTEM.VAL(INTEGER, v)
END MakeRealItem;

PROCEDURE MakeStringItem*(VAR x: Item; len: INTEGER); (* copies string from ORS-buffer *)
VAR i: INTEGER;
BEGIN
  Trace(TraceMakeItem, "MakeStringItem('");
  i := 0;  WHILE (i < len) & (ORS.str[i] # 0X) DO l.c(ORS.str[i], Trc); INC(i) END;
  l.sl("')", Trc);

  ZeroFill(x);  X64.ClearMode(x);
  x.mode := String;  x.type   := ORB.strType;  x.readonly := TRUE;
  x.n    := len;     x.offset := Strx;

  IF Strx+len >= LEN(Str) THEN ORS.Mark("Too much string literal")
  ELSE
    FOR i := 0 TO len-1 DO Str[Strx+i] := ORS.str[i] END;
    INC(Strx, len);
    IF ORS.str[len-1] # 0X THEN Str[Strx] := 0X; INC(Strx) END
  END;
  Trace(TraceMakeItem, ".. StringItem ");  l.Item(x, Trc);  l.l(Trc);
END MakeStringItem;



(* ---- Item conversion ---- *)

PROCEDURE StrToChar*(VAR x: Item);
BEGIN Trace(TraceAny, "StrToChar(x: ");  l.Item(x, Trc);  l.l(Trc);
  ASSERT(x.type.form = ORB.String);
  ASSERT(x.mode      = String);
  ASSERT(x.n         = 2);  (* String length - a character and a zero terminator *)
  ASSERT(x.readonly);
  x.mode := Const;  x.n := ORD(Str[x.offset]);  x.offset := 0;
END StrToChar;


(* ---- Assignment ---- *)

PROCEDURE Store*(VAR dest, src: Item); (* dest := src *)
BEGIN
  Trace(TraceAny, "Store(dest: ");  l.Item(dest, Trc);
  l.s(", src: ", Trc);  l.Item(src, Trc);  l.l(Trc);
  Depar(dest);
  IF src.mode = Const THEN
    StoreImmediate(src.n, dest)
  ELSE
    Load(src);
    StoreReg(src.n, dest);
  END;
  FreeRegs(src);  FreeRegs(dest)
END Store;


PROCEDURE SaveExchange(current, required: INTEGER; VAR save: INTEGER);
BEGIN
  IF current < 0 THEN
    IF required IN Free THEN current := required
                        ELSE current := ReserveNextFree() END
  END;
  IF current = required THEN
    save := -1
  ELSE
    save := current;
    OpModRegRm(87H, Reg, current, 8, required, -1, 0, 0, 0)  (* xchg *)
  END
END SaveExchange;

PROCEDURE RestoreReg(reg, exchanged: INTEGER);
BEGIN
  IF exchanged >= 0 THEN
    MoveReg(reg, exchanged)
  ELSE
    ReleaseReg(reg)
  END
END RestoreReg;


PROCEDURE CallSysFn(sysfn: INTEGER);
BEGIN
  X64.Emit(0E8H);
  ImpFix[ImpFInx].offset := X64.PC;
  ImpFix[ImpFInx].fix    := sysfn;  (* 16/module 0, 16/impno sysfn *)
  INC(ImpFInx);
  X64.EmitBytes(4, -(X64.PC + 4))  (* Offset from next instruction to module address 0 *)
END CallSysFn;



PROCEDURE StoreStruct*(VAR x, y: Item);   (* x := y *)
VAR sadr, slen, dadr, dlen, xchgrsi, xchgrdi, xchgrcx: INTEGER;
BEGIN Trace(TraceAny, "StoreStruct(x: "); l.Item(x, Trc);  l.s(", y: ", Trc); l.Item(y, Trc); l.sl(")", Trc);

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
        OpModRegRm(Cmp+1, Reg, slen, 8, dlen, -1, 0, 0, 0);
        X64.Emit(74H);   X64.Emit(5);  (* je: if sizes match skip failure *)
        CallSysFn(ArraySizeMismatchProc)
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

    FreeRegs(x);  FreeRegs(y);
    Disassemble(0, "StoreStruct");
  END
END StoreStruct;


PROCEDURE CopyString*(VAR x, y: Item);  (* x := y *)
VAR sadr, slen, dadr, dlen: INTEGER;
BEGIN Trace(TraceAny, "CopyString(x: "); l.Item(x, Trc);  l.s(", y: ", Trc); l.Item(y, Trc); l.sl(")", Trc);
  ASSERT(x.mode IN {Stkind, Eadr, Global, Import});
  ASSERT(y.type.form = ORB.String);
  ASSERT(y.mode = String);

  IF (x.type.len >= 0) & (x.type.len < y.n) THEN
    ORS.Mark("string too long");
    y.n := x.type.len
  END;

  (* Source string *)
  sadr := FindAndReserveReg(RSI, {});  (* Must be RSI *)
  slen := FindAndReserveReg(RCX, {});  (* Must be RCX *)
  LoadStructure(sadr, slen, y);

  (* Destination string *)
  dadr := FindAndReserveReg(RDI, {});  (* Must be RDI *)
  IF x.type.len >= 0 THEN (* dest length known at compile time*)
    LoadAddressReg(dadr, x)
  ELSE
    dlen := ReserveNextFree();
    LoadStructure(dadr, dlen, x);
    (* Check length at run time *)
    OpModRegRm(Cmp+1, Reg, slen, 8, dlen, -1, 0, 0, 0);
    X64.Emit(76H);   X64.Emit(5);  (* jbe: if source fits in dest skip failure *)
    CallSysFn(ArraySizeMismatchProc)
  END;

  (* Consider: stop copy at first 0, enforce 0 if none *)
  X64.Emit(0F3H);  X64.Emit(0A4H);  (* rep movsb *)
  ReleaseReg(RSI);  ReleaseReg(RDI);  ReleaseReg(RCX);
  FreeRegs(x);  FreeRegs(y);
  Disassemble(0, "CopyString")
END CopyString;


(* ---- selection - fields, indices & pointers ---- *)

PROCEDURE Field*(VAR x: Item; y: ORB.Object);   (* x := x.y *)
BEGIN
  Trace(TraceAny, "Field(x: "); l.Item(x, Trc);
  l.s(", y: ", Trc); l.Object(y, Trc); l.sl(")", Trc);
  ASSERT(x.mode IN {Stkind, Eadr, Global, Import});
  ASSERT(y.class = ORB.Fld);
  INC(x.offset, y.val)
END Field;

PROCEDURE Index*(VAR x, y: Item);   (* x := x[y] *)
VAR elemsize: INTEGER;
BEGIN Trace(TraceAny, "Index(x: "); l.Item(x, Trc); l.s(", y: ", Trc); l.Item(y, Trc); l.l(Trc);
  ASSERT(x.type.form = ORB.Array);
  ASSERT(x.mode IN {Stkind, Eadr, String, Global, Import});

  elemsize := x.type.base.size;
  Trace(TraceAny, "  "); l.s("x base type: ", Trc); l.Type(x.type.base, Trc); l.l(Trc);
  Trace(TraceAny, "  "); l.s("elemsize: ", Trc); l.i(elemsize, Trc); l.l(Trc);

  IF y.mode = Const THEN
    INC(x.offset, y.n * elemsize)
  ELSE
    IF (elemsize IN {1, 2, 4, 8})
     & (x.mode IN {Stkind, Eadr, Code, String, Global, Import})
     & (x.index < 0) THEN
      Depar(x);
      (* Use X64 SIB index and scale *)
      x.scale := 0;
      IF    elemsize = 2 THEN x.scale := 1
      ELSIF elemsize = 4 THEN x.scale := 2
      ELSIF elemsize = 8 THEN x.scale := 3
      END;
      Load(y);
      x.index := y.n;
      FreeRegs(y);
      ReserveReg(x.index)
    ELSE
      (* Apply index to base address in x *)
      LoadAddress(x);  x.mode := Eadr;
      AluOpToReg(Plus, y.mode, x.n, X64.IsSigned(y.type), y.type.size, y.n, y.index, y.scale, y.offset);
      FreeRegs(y)
    END
  END;
  Disassemble(y.mode, "Index")
END Index;

PROCEDURE DeRef*(VAR x: Item);
BEGIN Trace(TraceAny, "DeRef(x), x: "); l.Item(x, Trc); l.l(Trc);
  ASSERT(x.type.form = ORB.Pointer);
  ASSERT(x.mode IN {Stkind, Eadr, Code, String, Global, Import});
  Load(x);  x.mode := Eadr;
  Disassemble(0, "DeRef")
END DeRef;


(* ---- Paramter passing ---- *)

PROCEDURE ValueParam*(VAR x: Item;  stkoff: INTEGER);
VAR pos, reg: INTEGER;
BEGIN
  Trace(TraceAny, "ValueParam["); l.i(stkoff, Trc); l.s("](x: ", Trc); l.Item(x, Trc); l.sl(")", Trc);
  IF stkoff = 0 THEN  (* normal (simple) parameter passing *)
    Push(x); X64.AdjustStack(1)
  ELSE  (* Windows ABI parameter passing *)
    pos := -(stkoff DIV 8 + 1);
    IF    pos = 0 THEN reg := RCX
    ELSIF pos = 1 THEN reg := RDX
    ELSIF pos = 2 THEN reg := 8
    ELSIF pos = 3 THEN reg := 9
    ELSE reg := -1
    END;
    IF reg >= 0 THEN
      LoadReg(reg, x)
    ELSE
      IF x.mode = Const THEN
        StoreImmediateToMem(x.n, Eadr, 8, RBP, -1, 0, pos * 8)
      ELSE
        Load(x);  StoreRegToMem(x.n, Eadr, 8, RBP, -1, 0, pos * 8);  FreeRegs(x)
      END
    END
  END;
  Disassemble(x.mode, "ValueParam")
END ValueParam;


PROCEDURE VarParam*(VAR x: Item;  ftype: ORB.Type;  stkoff: INTEGER);
VAR reg: INTEGER;
BEGIN
  Trace(TraceAny, "VarParam["); l.i(stkoff, Trc);
  l.s("](x: ", Trc);            l.Item(x, Trc);
  l.s(", ftype: ", Trc);        l.Type(ftype, Trc);
  l.s(") SPO ", Trc);           l.i(X64.SPO, Trc);  l.l(Trc);
  ASSERT(stkoff = 0);  (* Not supported for Windows ABI calls *)
  IF x.mode = Reg THEN ORS.Mark("Not compatible with VAR parameter")
  ELSE
    ASSERT(x.mode IN {Stkind, Eadr, String, Global, Import});
    IF (x.mode = Stkind) & ((ftype.form = ORB.Array) & (ftype.len < 0) OR (ftype.form = ORB.Record)) THEN
      (* Passing open array or record parm to same *)
      PushMem(Eadr, FALSE, 8, RSP, -1, 0, x.n + 8 * X64.SPO + 8);  X64.AdjustStack(1);  (* Length or descriptor *)
      PushMem(Eadr, FALSE, 8, RSP, -1, 0, x.n + 8 * X64.SPO);      X64.AdjustStack(1);  (* Address *)
    ELSE
      (* First pass array length or type descriptor address, if any *)
      IF (ftype.form = ORB.Array) & (ftype.len < 0) THEN ASSERT(x.type.len >= 0);
        IF x.type.form = ORB.Array THEN
          PushImmediate(x.type.len)
        ELSE
          PushImmediate(x.type.size) (* Passing non-array to ARRAY OF BYTE *)
        END;
        X64.AdjustStack(1) (* Pass array length *)
      ELSIF ftype.form = ORB.Record THEN
        reg := FirstFreeReg();
        OpModRegRm(8DH, Code, reg, 8, 0, -1, 0, ftype.len, 0);  (* lea type desciptor *)
        PushReg(reg);  X64.AdjustStack(1)  (* Pass record descriptor *)
      END;
      (* Pass array or record address *)
      PushAdr(x);  X64.AdjustStack(1)
    END;
    Disassemble(x.mode, "VarParam")
  END;
  FreeRegs(x)
END VarParam;

PROCEDURE OpenArrayParam*(VAR x: Item; stkoff: INTEGER);
(* x being passed to open array formal parameter *)
BEGIN
  Trace(TraceAny, "OpenArrayParam["); l.i(stkoff, Trc);
  l.s("](x: ", Trc);                  l.Item(x, Trc);
  l.s(") SPO ", Trc);                 l.i(X64.SPO, Trc);  l.l(Trc);
  ASSERT(stkoff = 0);  (* Not supported for Windows ABI calls *)
  ASSERT(x.mode IN {Stkind, Eadr, String, Global, Import});
  ASSERT(x.type.form = ORB.Array);
  ASSERT(x.type.len >= 0);  (* Passing known length array to open array parameter *)
  PushImmediate(x.type.len);  X64.AdjustStack(1);  (* Pass array length *)
  PushAdr(x);                 X64.AdjustStack(1);  (* Pass array address *)
  FreeRegs(x);
  Disassemble(x.mode, "OpenArrayParam");
  Trace(TraceAny, "OpenArrayParam exit: SPO "); l.i(X64.SPO, Trc);  l.l(Trc)
END OpenArrayParam;

PROCEDURE StringParam*(VAR x: Item; stkoff: INTEGER);
BEGIN
  Trace(TraceAny, "StringParam["); l.i(stkoff, Trc);
  l.s("](x: ", Trc);               l.Item(x, Trc);
  l.s(") SPO ", Trc);              l.i(X64.SPO, Trc);  l.l(Trc);
  ASSERT(x.mode = String);
  ASSERT(stkoff = 0);  (* Not supported for Windows ABI calls *)
  PushImmediate(x.n);  X64.AdjustStack(1);  (* Pass string length *)
  PushAdr(x);          X64.AdjustStack(1);  (* Pass string address *)
  Disassemble(x.mode, "StringParam");
END StringParam;


(* ---- Procedure / function calls ---- *)

PROCEDURE PrepCall*(VAR x: Item; VAR saved: SET);
VAR i : INTEGER;  stackspace: INTEGER;
BEGIN
  Trace(TraceAny, "PrepCall(x: "); l.Item(x, Trc);
  l.s("), SPO ", Trc);             l.i(X64.SPO, Trc); l.l(Trc);
  saved := AllFree - Free;
  IF saved # {} THEN  (* Save partially evaluated expression *)
    FOR i := 0 TO 15 DO IF i IN saved THEN PushReg(i);  X64.AdjustStack(1) END END;
    Disassemble(0, "Save registers");
    Free := AllFree
  END;
  ASSERT(x.type.form = ORB.Proc);
  ASSERT(x.mode IN {Eadr, Global, Code, Import});

  stackspace := x.type.len;  (* Only used for Windows native ABI calls *)
  IF stackspace < 32 THEN stackspace := 32 END;

  IF x.mode = Eadr THEN  (* Push proc address to stack *)
    Push(x);  X64.AdjustStack(1);  FreeRegs(x);
    Disassemble(x.mode, "proc address to be called");
    ASSERT(x.type.len MOD 8 = 0);
    x.mode   := Eadr;
    IF ~x.type.native THEN  (* Normal, simple calling process *)
      x.n      := RSP;
      x.offset := -8 * (X64.SPO - (x.type.len DIV 8 + 1))
    ELSE  (* Windows ABI calling process *)
      x.n      := RBP;
      x.offset := -8 * (X64.SPO - (stackspace DIV 8 + 2))
    END
  END;
  IF x.type.native THEN  (* Prepare stack space for windows abi *)
    (* We need to place parameters aligned so that the first parameter (at the
       lowest address) is 16 byte aligned. This alignemnet will only be known at
       run time. However we need to keep SPO matching SP so that expressions
       referencing local vars and parameters work corectly. Therefore we use
       BP to place the parameters on the stack, and advance SP 8 bytes extra to
       allow for either alignment *)
    ReserveReg(RBP);  MoveReg(RBP, RSP);
    AluOpFromImmediate(Minus, Reg, stackspace,         8, RBP, -1, 0, 0);
    AluOpFromImmediate(And,   Reg, 0FFFFFFFFFFFFFFF0H, 8, RBP, -1, 0, 0);
    AluOpFromImmediate(Minus, Reg, stackspace + 8,     8, RSP, -1, 0, 0);
    X64.AdjustStack(stackspace DIV 8 + 1);
    Disassemble(0, "Reserve Windows ABI stack space")
  END
END PrepCall;


PROCEDURE Call*(VAR x: Item; saved: SET);
VAR i, reg, resultreg, stackspace: INTEGER;
BEGIN
  Trace(TraceAny, "Call(x: "); l.Item(x, Trc);
  l.s("), SPO ", Trc);         l.i(X64.SPO, Trc);
  l.s(", type.len ", Trc);  l.i(x.type.len, Trc);  l.l(Trc);
  ASSERT(x.type.form = ORB.Proc);
  ASSERT(x.mode IN {Eadr, Global, Code, Import});

  IF x.type.native THEN  (* Adjust to 16 byte aligned pos *)
    OpModRegRm(87H, Reg, RBP, 8, RSP, -1, 0, 0, 0);  (* xchg rsp,rbp *)
    Disassemble(0, "Switch to Windows ABI aligned stack frame")
  END;

  IF x.type.form = ORB.Proc THEN
    IF x.mode IN {Eadr, Global} THEN
      OpModRegRm(0FFH, x.mode, 2, 4, x.n, x.index, x.scale, x.offset, 0)  (* call [addr] *)
    ELSE
      X64.Emit(0E8H);
      IF x.mode = Code THEN
        X64.EmitBytes(4, x.offset - (X64.PC + 4))
      ELSIF x.mode = Import THEN
        ImpFix[ImpFInx].offset := X64.PC;
        ImpFix[ImpFInx].fix    := x.n;  INC(ImpFInx);
        X64.EmitBytes(4, -(X64.PC + 4))  (* Offset from next instruction to module address 0 *)
      ELSE
        l.s("** mode ", Trc);  l.Mode(x.mode, Trc);  l.sl(" **", Trc);
        ASSERT(FALSE)
      END
    END
  ELSE
    l.s("** form ", Trc);  l.Form(x.type.form, Trc);  l.sl(" **", Trc);
    ASSERT(FALSE)
  END;
  Disassemble(x.mode, "Call");

  IF ~x.type.native THEN   (* Normal (simple) stack handling *)
    X64.AdjustStack(-(x.type.len DIV 8));  (* matches stack adjustment by <ret n> *)
  ELSE (* Windows ABI stack handling *)
    stackspace := x.type.len;  IF stackspace < 32 THEN stackspace := 32 END;
    OpModRegRm(8DH, Eadr, RSP, 8, RBP, -1, 0, stackspace + 8, 0);  (* lea rsp, rbp + parmreserved *)
    Disassemble(0, "Drop Windows ABI parameter area from stack");
    X64.AdjustStack(-(stackspace DIV 8 + 1))  (* matches stack adjustment by <ret n> *)
  END;

  IF x.mode = Eadr THEN
    (* Drop called proc address from stack *)
    AluOpFromImmediate(Plus, Reg, 8, 8, RSP, -1, 0, 0);
    X64.AdjustStack(-1);
    Disassemble(0, "Drop called proc address")
  END;

  IF x.type.base.form = ORB.NoTyp THEN (* procedure *)
    ASSERT(saved = {});
    ClearRegs
  ELSE                                 (* function *)
    ClearRegs;
    FreeRegs(x);
    (* Move function result to another reg if RAX was in use before fn call *)
    Free := Free - saved;
    resultreg := FirstFreeReg();
    IF resultreg # RAX THEN MoveReg(resultreg, RAX) END;
    x.mode := Reg;  x.n := resultreg;  ReserveReg(resultreg);
    IF saved # {} THEN (* Restore saved registers *)
      i := 15;
      WHILE i >= 0 DO
        IF i IN saved THEN PopReg(i);  ReserveReg(i);  X64.AdjustStack(-1);  END;
        DEC(i)
      END;
      Disassemble(0, "Restore saved registers")
    END;
  END
END Call;

PROCEDURE Enter*(parmsize, locsize: INTEGER);
VAR i, count: INTEGER;
BEGIN
  Trace(TraceAny, "Enter(parmsize $"); l.h(parmsize, Trc);
  l.s(", locsize $", Trc);             l.h(locsize, Trc);   l.sl(")", Trc);
  ASSERT(locsize MOD 8 = 0);
  count := locsize DIV 8;
  IF count > 0 THEN
    IF count < 3 THEN
      FOR i := 1 TO count DO PushImmediate(0) END
    ELSIF count < 8 THEN
      LoadImmediate(RAX, 0);          (*     xor    eax,eax   *)
      FOR i := 1 TO count DO PushReg(RAX) END
    ELSE
      LoadImmediate(RCX, count);      (*     mov    rcx,count *)
      PushImmediate(0);               (* l1: push   0         *)
      X64.Emit(0E2H); X64.Emit(0FCH)  (*     loop   l1        *)
    END;
    Disassemble(0, "Reserve and clear local variables")
  END;
  X64.ClearStack;  ClearRegs
END Enter;


PROCEDURE EmitStrings();
VAR i: INTEGER;
BEGIN
  IF Disasmpc < X64.PC THEN Disassemble(0, "") END;
  IF Strx > 0 THEN
    l.l(Trc);
    i := 0;
    WHILE i < StrFInx DO  (* Relocate collected string addresses *)
      X64.Patch(StrFix[i], 4, X64.PC + X64.Peek(StrFix[i], 4, FALSE));  INC(i)
    END;

    i := 0;
    WHILE i < Strx DO  (* Emit 1 string  *)
      WHILE (Str[i] # 0X) & (i < Strx) DO X64.Emit(ORD(Str[i])); INC(i) END;
      IF i < Strx THEN ASSERT(Str[i] = 0X); INC(i) END;
      X64.Emit(0);
      l.DisassembleString(Disasmpc);
    END
  END;
  Strx := 0;  StrFInx := 0
END EmitStrings;

PROCEDURE Return*(form: INTEGER;  VAR x: Item;  parmsize, locsize: INTEGER);
BEGIN
  Trace(TraceAny, "Return("); l.Form(form, Trc);
  l.s(", x: ", Trc);       l.Item(x, Trc);
  l.s(", locsize ", Trc);  l.i(locsize, Trc);
  l.s(", parmsize ", Trc); l.i(parmsize, Trc);
  l.s(", SPO ", Trc);      l.i(X64.SPO, Trc); l.l(Trc);

  IF form # ORB.NoTyp THEN Load(x); Disassemble(x.mode, "Load function result") END;

  IF locsize > 0 THEN
    AluOpFromImmediate(Plus, Reg, locsize, 8, RSP, -1, 0, 0);
    Disassemble(0, "release local variable space")
  END;

  IF parmsize = 0 THEN
    X64.Emit(0C3H)  (* ret *)
  ELSE
    X64.Emit(0C2H);  X64.EmitBytes(2, parmsize)  (* retn *)
  END;
  Disassemble(0, "Return");

  X64.Emit(2EH);  (* Mark end of code with ignored instruction 'seg cs' *)
  Disasmpc := X64.PC;

  (*EmitStrings;*)
  X64.ClearStack;  ClearRegs
END Return;


(* ---- Standard functions ---- *)

PROCEDURE Abs*(VAR x: Item);
VAR reg: INTEGER;
BEGIN Trace(TraceAny, "Abs(x "); l.Item(x, Trc); l.sl(")", Trc);
  IF x.mode = Const THEN x.n := ABS(x.n)
  ELSIF x.type.form = ORB.Real THEN ASSERT(FALSE)
  ELSE
    Load(x);  reg := FirstFreeReg();
    MoveReg(reg, x.n);                              (* mov     reg,x.n *)
    OpModRegRm(0F7H,  Reg, 3,   8, x.n, -1, 0, 0, 0);  (* neg     x.n     *)
    OpModRegRm(0F4CH, Reg, x.n, 8, reg, -1, 0, 0, 0)   (* cmovl   x.n,reg *)
  END;
  Disassemble(x.mode, "Abs")
END Abs;

PROCEDURE Odd*(VAR x: Item);
BEGIN Trace(TraceAny, "Odd(x: "); l.Item(x, Trc); l.sl(")", Trc);
  Load(x);
  AluOpFromImmediate(And, Reg, 1, 8, x.n, -1, 0, 0);  (* and  x.n,1 *)
  FreeRegs(x);
  SetCondition(x, CNZ);  (* odd if x & 1 nonzero *)
  Disassemble(x.mode, "Odd")
END Odd;

PROCEDURE Floor*(VAR x: Item);
BEGIN Trace(TraceAny, "Floor(x: "); l.Item(x, Trc); l.sl(")", Trc);  ASSERT(FALSE);
  ASSERT(FALSE)
END Floor;

PROCEDURE Float*(VAR x: Item);
BEGIN Trace(TraceAny, "Float(x: "); l.Item(x, Trc); l.sl(")", Trc);  ASSERT(FALSE);
  ASSERT(FALSE)
END Float;

PROCEDURE Ord*(VAR x: Item);
BEGIN Trace(TraceAny, "Ord(x: "); l.Item(x, Trc); l.sl(")", Trc);
  IF x.mode # Const THEN Load(x) END;
  Disassemble(x.mode, "ORD")
END Ord;

PROCEDURE Len*(VAR x: Item);
BEGIN Trace(TraceAny, "Len(x: "); l.Item(x, Trc); l.sl(")", Trc);
  ASSERT(x.type.form IN {ORB.String, ORB.Array});
  ASSERT(x.mode IN {Stkind, Eadr, Code, String, Global, Import});
  IF x.mode = Stkind THEN  (* Length is runtime parameter *)
    x.mode   := Eadr;
    x.offset := x.n + 8;
    x.n      := RSP;
  ELSE  (* Length known at compile time *)
    x.mode := Const;
    IF x.mode # String THEN (* Length not already in x.n *)
      ASSERT(x.type.form = ORB.Array);
      x.n := x.type.size
    END;
    x.offset := 0;
  END;
  ASSERT(x.index < 0);  ASSERT(x.scale = 0)
END Len;

PROCEDURE Shift*(fct: INTEGER; VAR x, y: Item);
VAR op, xchgrcx: INTEGER;  name: ARRAY 4 OF CHAR;
BEGIN
  IF    fct = 0 THEN name := "LSL";  op := 4  (* shl *)
  ELSIF fct = 1 THEN name := "ASR";  op := 7  (* sar *)
  ELSIF fct = 2 THEN name := "ROR";  op := 1  (* ror *)
  ELSE ASSERT(FALSE)
  END;
  Trace(TraceAny, "Shift("); l.s(name, Trc); l.s(", x: ", Trc); l.Item(x, Trc); l.s(", y: ", Trc); l.Item(y, Trc); l.l(Trc);
  IF y.mode = Const THEN
    Load(x);
    OpModRegRm(0C1H, Reg, op, 8, x.n, -1, 0, 0, 1);  X64.Emit(y.n);
  ELSE
    IF x.mode # Reg THEN LoadReg(FindAndReserveReg(RAX, -{RCX}), x) END;
    IF x.n = RCX THEN  (* Use a different register as we'll need cl for shift count *)
      x.n := ReserveNextFree();  MoveReg(x.n, RCX);  ReleaseReg(RCX)
    END;
    (* Get shift amount to rcx *)
    IF y.mode # Reg THEN LoadReg(FindAndReserveReg(RCX, AllFree), y) END;
    SaveExchange(y.n, RCX, xchgrcx);
    OpModRegRm(0D3H, Reg, op, 8, x.n, -1, 0, 0, 0);  (* shiftop reg,cl *)
    RestoreReg(RCX, xchgrcx)
  END;
  Disassemble(0, name)
END Shift;

PROCEDURE ADC*(VAR x, y: Item);
BEGIN Trace(TraceAny, "ADC(x: "); l.Item(x, Trc); l.s(", y: ", Trc); l.Item(y, Trc); l.sl(")", Trc);
  ASSERT(FALSE)
END ADC;

PROCEDURE SBC*(VAR x, y: Item);
BEGIN Trace(TraceAny, "SBC(x: "); l.Item(x, Trc); l.s(", y: ", Trc); l.Item(y, Trc); l.sl(")", Trc);
  ASSERT(FALSE)
END SBC;

PROCEDURE UML*(VAR x, y: Item);
BEGIN Trace(TraceAny, "UML(x: "); l.Item(x, Trc); l.s(", y: ", Trc); l.Item(y, Trc); l.sl(")", Trc);
  ASSERT(FALSE)
END UML;

PROCEDURE Bit*(VAR x, y: Item);
BEGIN Trace(TraceAny, "Bit(x: "); l.Item(x, Trc); l.s(", y: ", Trc); l.Item(y, Trc); l.sl(")", Trc);
  ASSERT(FALSE)
END Bit;

PROCEDURE REG*(VAR x: Item);
BEGIN Trace(TraceAny, "REG(x "); l.Item(x, Trc); l.sl(")", Trc);
  ASSERT(FALSE)
END REG;

PROCEDURE Adr*(VAR x: Item);
BEGIN Trace(TraceAny, "Adr(x "); l.Item(x, Trc); l.sl(")", Trc);
  IF x.mode IN {Stkind, Eadr, Code, String, Global, Import} THEN
    LoadAddress(x);  x.mode := Reg
  ELSE
    ORS.Mark("not addressable")
  END;
  Disassemble(0, "ADR")
END Adr;

PROCEDURE COND*(VAR x: Item);
BEGIN Trace(TraceAny, "COND(x "); l.Item(x, Trc); l.sl(")", Trc);
  ASSERT(FALSE)
END COND;


(* ---- Standard procedures --- *)

PROCEDURE Increment*(upordown: INTEGER; VAR x, y: Item);  (* x := x +/- y *)
VAR op: INTEGER;
BEGIN Trace(TraceAny, "Increment(");
  IF upordown = 0 THEN
    op := Plus;   l.s("up", Trc)
  ELSE
    op := Minus;  l.s("down", Trc)
  END;
  l.s(", x: ", Trc); l.Item(x, Trc); l.s(", y: ", Trc); l.Item(y, Trc); l.l(Trc);

  ASSERT(x.mode IN {Stkind, Eadr, Global, Import});
  Depar(x);
  IF y.type = ORB.noType THEN
    AluOpFromImmediate(op, x.mode, 1,   x.type.size, x.n, x.index, x.scale, x.offset)
  ELSIF y.mode = Const THEN
    AluOpFromImmediate(op, x.mode, y.n, x.type.size, x.n, x.index, x.scale, x.offset);
  ELSE
    Load(y);
    (*X64.AluOpRegToMem(op, y.n, x.size, x.n, x.index, x.scale, x.offset)*)
    (*X64.EmitRegMemOp(op+1, y.n, x.size, x.n, x.index, x.scale, x.offset)*)
    OpItem(op+1, y.n, 8, 0, x)
  END;

  Disassemble(x.mode, "INC/DEC");
  FreeRegs(x);  FreeRegs(y)
END Increment;


PROCEDURE Assert*(VAR x: Item);
BEGIN Trace(TraceAny, "Assert(x: ");  l.Item(x, Trc);  l.sl(")", Trc);
  IF x.mode # Cond THEN LoadFlags(x) END;
  IF x.n > 1 THEN
    ASSERT(x.n DIV 16 = 8);
    X64.Emit(x.n - 10H);  X64.Emit(5);  (* Skip over call instruction if condition met *)
  END;
  IF x.n # 1 THEN CallSysFn(AssertionFailureProc) END;  (* Jump to assert failure handler *)
  Disassemble(x.mode, "ASSERT")
END Assert;

PROCEDURE New*(VAR x: Item);
BEGIN
  Trace(TraceAny, "New(x: ");  l.Item(x, Trc);
  l.s("), base type ", Trc);   l.Type(x.type.base, Trc); l.l(Trc);
  ASSERT(x.mode IN {Stkind, Eadr, Global, Import});
  PushAdr(x);  FreeRegs(x);          X64.AdjustStack(1);
  PushImmediate(x.type.base.size);   X64.AdjustStack(1);
  CallSysFn(NewProc);
  X64.AdjustStack(-2);
  Disassemble(x.mode, "New")
END New;

PROCEDURE Led*(VAR x: Item);
BEGIN Trace(TraceAny, "Led(x: ");  l.Item(x, Trc);  l.sl(")", Trc);
  ASSERT(FALSE)
END Led;

PROCEDURE LDPSR*(VAR x: Item);
BEGIN Trace(TraceAny, "LDPSR(x: ");  l.Item(x, Trc);  l.sl(")", Trc);
  ASSERT(FALSE)
END LDPSR;

PROCEDURE Pack*(VAR x, y: Item);
BEGIN Trace(TraceAny, "Pack(x: "); l.Item(x, Trc); l.s(", y: ", Trc); l.Item(y, Trc); l.sl(")", Trc);
  ASSERT(FALSE)
END Pack;

PROCEDURE Unpk*(VAR x, y: Item);
BEGIN Trace(TraceAny, "Unpk(x: "); l.Item(x, Trc); l.s(", y: ", Trc); l.Item(y, Trc); l.sl(")", Trc);
  ASSERT(FALSE)
END Unpk;

PROCEDURE Get*(VAR x, y: Item);  (* y := [x] *)
BEGIN Trace(TraceAny, "Get(x: "); l.Item(x, Trc); l.s(", y: ", Trc); l.Item(y, Trc); l.l(Trc);
  ASSERT(y.type.size <= 8);
  Load(x);  x.type := y.type;  x.mode := Eadr;  Store(y, x);
  FreeRegs(x);  FreeRegs(y)
END Get;

PROCEDURE Put*(VAR x, y: Item);
BEGIN Trace(TraceAny, "Put(x: "); l.Item(x, Trc); l.s(", y: ", Trc); l.Item(y, Trc); l.l(Trc);
  ASSERT(y.type.size <= 8);
  Load(x);  x.type := y.type;  x.mode := Eadr;  Store(x, y);
  FreeRegs(x);  FreeRegs(y)
END Put;

PROCEDURE LDREG*(VAR x, y: Item);
BEGIN
  Trace(TraceAny, "LDREG(x: "); l.Item(x, Trc);
  l.s(", y: ", Trc);            l.Item(y, Trc); l.l(Trc);
  ASSERT(FALSE)
END LDREG;

PROCEDURE Copy*(VAR x, y, z: Item);
BEGIN Trace(TraceAny, "Copy(x: "); l.Item(x, Trc);
  l.s(", y: ", Trc);               l.Item(y, Trc);
  l.s(", z: ", Trc);               l.Item(z, Trc); l.l(Trc);
  IF (z.mode = Const) & (z.n <= 0) THEN ORS.Mark("bad count") END;

  LoadReg(RSI, x);
  LoadReg(RDI, y);
  LoadReg(RCX, z);

  (* TODO Check rcx >=0 & <= LEN(y) *)

  X64.Emit(0F3H);  X64.Emit(0A4H);  (* rep movsb *)
  FreeRegs(x);  FreeRegs(y);  FreeRegs(z);
  Disassemble(0, "Copy");
END Copy;


(* ---- Sets ---- *)

PROCEDURE Set*(VAR x, y: Item);   (* x := {x .. y} *)
BEGIN
  Trace(TraceAny, "Set(x: "); l.Item(x, Trc);
  l.s(", y: ", Trc);          l.Item(y, Trc); l.sl(")", Trc);
  IF (x.mode = Const) & (y.mode = Const) THEN
    IF x.n <= y.n THEN
      x.n := LSL(2, y.n) - LSL(1, x.n) ELSE x.n := 0
    END
  ELSE
    ASSERT(FALSE)
  END
END Set;

PROCEDURE Singleton*(VAR x: Item);  (* x := {x} *)
VAR reg: INTEGER;
BEGIN Trace(TraceAny, "Singleton(x: "); l.Item(x, Trc); l.sl(")", Trc);
  IF x.mode = Const THEN
    x.n := LSL(1, x.n)
  ELSE
    Load(x);
    reg := ReserveNextFree();
    LoadImmediate(reg, 1);
    OpModRegRm(0FABH, Reg, reg, 8, x.n, -1, 0, 0, 0);  (* bts reg, x *)
    Disassemble(x.mode, "Singleton");
    FreeRegs(x);
    x.mode := Reg;  x.n := reg
  END
END Singleton;


PROCEDURE In*(VAR x, y: Item);  (* x := x IN y *)
BEGIN
  Trace(TraceAny, "In(x: "); l.Item(x, Trc);
  l.s(", y: ", Trc);         l.Item(y, Trc); l.sl(")", Trc);
  ASSERT(x.mode  >= Const);
  ASSERT(y.mode  >= Const);

  (* TODO - optimise immediate x cases where x<32 with AND 1<<x *)

  Depar(x);  Depar(y);
  IF y.mode = Const THEN Load(y) END;
  IF x.mode = Const THEN
    OpItem(0FBAH, 4, 8, 1, y);   (* bt y,imm *)
    X64.Emit(x.n)
  ELSE
    Load(x);
    OpItem(0FA3H, x.n, 8, 0, y)  (* bt y,x *)
  END;
  Disassemble(y.mode, "In");
  FreeRegs(x);  FreeRegs(y);
  SetCondition(x, CC)
END In;

PROCEDURE SetOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR xs, ys: SET;
BEGIN
  Trace(TraceAny, "SetOp("); l.Sym(op, Trc);
  l.s("; x: ", Trc);         l.Item(x, Trc);
  l.s(", y: ", Trc);         l.Item(y, Trc); l.sl(")", Trc);
  IF (x.mode = Const) & (y.mode = Const) THEN
    xs := SYSTEM.VAL(SET, x.n);  ys := SYSTEM.VAL(SET, y.n);
    IF    op = ORS.plus  THEN xs := xs + ys
    ELSIF op = ORS.minus THEN xs := xs - ys
    ELSIF op = ORS.times THEN xs := xs * ys
    ELSIF op = ORS.rdiv  THEN xs := xs / ys
    END;
    x.n := SYSTEM.VAL(INTEGER, xs)
  ELSE
    Load(x);
    (* Convert op to x86 opcode *)
    IF op = ORS.minus THEN  (* Invert y *)
      IF y.mode = Const THEN
        y.n := -y.n - 1  (* = invert *)
      ELSE
        Load(y);  OpItem(0F7H, 2, 8, 0, y);
        Disassemble(y.mode, "Invert for SET -")
      END
    END;
    IF    op = ORS.plus  THEN op := Or
    ELSIF op = ORS.minus THEN op := And
    ELSIF op = ORS.times THEN op := And
    ELSIF op = ORS.rdiv  THEN op := Xor
    ELSE l.s("** op = ", Trc); l.i(op, Trc); l.sl(" **", Trc); ASSERT(FALSE)
    END;
    Dyadic(op, x, y)
  END;
  Disassemble(0, "SetOp")
END SetOp;

PROCEDURE Include*(op: INTEGER; VAR x, y: Item);  (* INCL/EXCL(x, y): x := x +/- {y} *)
BEGIN
  Trace(TraceAny, "Include(");
  IF op = 0 THEN l.s("INCL", Trc) ELSE l.s("EXCL", Trc) END;
  l.s(", y: ", Trc); l.Item(x, Trc);
  l.s(", y: ", Trc); l.Item(y, Trc); l.sl(")", Trc);
  ASSERT(x.mode IN {Stkind, Eadr, Global, Import});
  ASSERT(y.mode >= Const);
  Depar(x);  Depar(y);
  IF y.mode = Const THEN
    IF op = 0 THEN
      OpItem(0FBAH, 5, 8, 0, x)  (* bts x,imm *)
    ELSE
      OpItem(0FBAH, 6, 8, 0, x)  (* btr x,imm *)
    END;
    X64.Emit(y.offset)
  ELSE
    Load(y);
    IF op = 0 THEN
      OpItem(0FABH, y.n, 8, 0, x)  (* bts x,y *)
    ELSE
      OpItem(0FB3H, y.n, 8, 0, x)  (* btr x,y *)
    END
  END;
  FreeRegs(x);  FreeRegs(y);
  IF op = 0 THEN Disassemble(x.mode, "INCL") ELSE Disassemble(x.mode, "EXCL") END;
END Include;


(* ---- Boolean operators ---- *)

PROCEDURE FixLinkWith(L, target: INTEGER);
(*  Patch chain of 4 byte addresses starting at L.                         *)
(*  Chain continues at (L)                                                 *)
(*  Patch each address with offset (from byte following address) to target *)
VAR lprev, offset: INTEGER;
BEGIN
  Trace(TraceAny, "FixLinkWith: chain at $"); l.h(L, Trc);
  l.s(", target $", Trc); l.h(target, Trc); l.sl(".", Trc);
  WHILE L # 0 DO
    offset := target - (L+4);

    Trace(TraceAny, "-> Patch at $"); l.h(L, Trc);
    l.s(" currently $", Trc);         l.h(X64.Peek(L, 4, FALSE), Trc);
    l.s(", with offset $", Trc);      l.h(offset, Trc);
    l.s(", target: $", Trc);          l.h(target, Trc); l.sl(".", Trc);

    lprev := X64.Peek(L, 4, FALSE);
    X64.Patch(L, 4, offset);
    L := lprev
  END
END FixLinkWith;

PROCEDURE FixLink*(L: INTEGER);
BEGIN FixLinkWith(L, X64.PC) END FixLink;


PROCEDURE merged(L0, L1: INTEGER): INTEGER;  (* Insert L0 at the start of L1 *)
VAR l2, l3: INTEGER;
BEGIN
  Trace(TraceAny, "Merged(L0: $"); l.h(L0, Trc);
  l.s(", L1: $", Trc);             l.h(L1, Trc);  l.sl(")", Trc);
  IF L0 # 0 THEN
    l2 := L0;
    REPEAT  l3 := l2;  l2 := X64.Peek(l2, 4, FALSE) UNTIL l2 = 0;
    Trace(TraceAny, "-> Patch at $"); l.h(l3, Trc);
    l.s(" currently $", Trc);         l.h(X64.Peek(l3, 4, FALSE), Trc);
    l.s(", with offset $", Trc);      l.h(L1, Trc);  l.sl(".", Trc);
    X64.Patch(l3, 4, L1);
    L1 := L0
  END;
  RETURN L1
END merged;


PROCEDURE Not*(VAR x: Item);   (* x := ~x *)
VAR t: INTEGER;
BEGIN Trace(TraceAny, "Not(x: "); l.Item(x, Trc); l.sl(")", Trc);
  IF x.mode # Cond THEN LoadFlags(x) END;
  x.n := InvertedCondition(x.n);
  t := x.offset;  x.offset := x.index;  x.index := t
END Not;

PROCEDURE Or1*(VAR x: Item);   (* x := x OR *)
BEGIN Trace(TraceAny, "Or1(x: "); l.Item(x, Trc); l.sl(")", Trc);
  IF x.mode # Cond THEN LoadFlags(x) END;
  IF x.n > 0 THEN
    IF x.n = 1 THEN  (* condition TRUE *)
      X64.Emit(0E9H);
    ELSE
      X64.Emit(0FH); X64.Emit(x.n);
    END;
    X64.EmitBytes(4, x.index);
    x.index := X64.PC - 4;
    Disassemble(x.mode, "Or1")
  END;
  FixLink(x.offset);  x.offset := 0
END Or1;

PROCEDURE Or2*(VAR x, y: Item);   (* x := x OR y *)
BEGIN
  Trace(TraceAny, "Or2(x: "); l.Item(x, Trc);
  l.s(", y: ", Trc);          l.Item(y, Trc); l.sl(")", Trc);
  IF y.mode # Cond THEN LoadFlags(y) END;
  x.index  := merged(y.index, x.index);
  x.offset := y.offset;
  x.n      := y.n
END Or2;


PROCEDURE And1*(VAR x: Item);   (* x := x & *)
BEGIN Trace(TraceAny, "And1(x: "); l.Item(x, Trc); l.sl(")", Trc);
  IF x.mode # Cond THEN LoadFlags(x) END;
  IF x.n # 1 THEN
    IF x.n = 0 THEN
      X64.Emit(0E9H)
    ELSE
      X64.Emit(0FH); X64.Emit(InvertedCondition(x.n));
    END;
    X64.EmitBytes(4, x.offset);
    x.offset := X64.PC - 4;
    Disassemble(x.mode, "And1")
  END;
  FixLink(x.index); x.index := 0
END And1;

PROCEDURE And2*(VAR x, y: Item);   (* x := x & y *)
BEGIN
  Trace(TraceAny, "And2(x: "); l.Item(x, Trc);
  l.s(", y: ", Trc);           l.Item(y, Trc); l.sl(")", Trc);
  IF y.mode # Cond THEN LoadFlags(y) END;
  x.offset := merged(y.offset, x.offset);
  x.index  := y.index;
  x.n      := y.n
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
  Depar(x);  Depar(y);
  ASSERT(x.mode >= Const);  ASSERT(y.mode >= Const);
  IF (y.mode = Const) & (y.n >= -80000000H) & (y.n < 80000000H) THEN
    IF (y.n >= -80H) & (y.n < 80H) THEN immsize := 1 ELSE immsize := 4 END;
    IF    x.type.size = 1 THEN OpItem(80H, 7, x.type.size, immsize, x)
    ELSIF immsize     = 1 THEN OpItem(83H, 7, x.type.size, immsize, x)
    ELSE                       OpItem(81H, 7, x.type.size, immsize, x)
    END;
    X64.EmitBytes(immsize, y.n);
  ELSE
    Dyadic(Cmp, x, y);
  END;
  Disassemble(x.mode, "Compare")
END Compare;

PROCEDURE IntRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
VAR immrel: BOOLEAN;
BEGIN
  Trace(TraceAny, "IntRelation("); l.Sym(op, Trc);
  l.s(", x: ", Trc);               l.Item(x, Trc);
  l.s(", y: ", Trc);               l.Item(y, Trc); l.sl(")", Trc);
  ASSERT(x.mode >= Const);
  IF y.mode < Const THEN l.s("** y.mode < Const, is ", Trc); l.i(y.mode, Trc); l.sl(" **", Trc) END;
  ASSERT(y.mode >= Const);
  IF (x.mode = Const) & (y.mode = Const) THEN
    IF    op = ORS.lss THEN immrel := x.n <  y.n
    ELSIF op = ORS.leq THEN immrel := x.n <= y.n
    ELSIF op = ORS.eql THEN immrel := x.n =  y.n
    ELSIF op = ORS.geq THEN immrel := x.n >= y.n
    ELSIF op = ORS.gtr THEN immrel := x.n >  y.n
    ELSIF op = ORS.neq THEN immrel := x.n #  y.n
    ELSE ASSERT(FALSE); immrel := FALSE
    END;
    IF immrel THEN SetCondition(x, 1) ELSE SetCondition(x, 0) END
  ELSE
    Compare(x, y);
    FreeRegs(x);  FreeRegs(y);  (* Result is in flags *)
    SetCondition(x, OpToIntCondition(op))
  END
END IntRelation;

PROCEDURE RealRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
BEGIN
  Trace(TraceAny, "RealRelation("); l.Sym(op, Trc);
  l.s(", x: ", Trc); l.Item(x, Trc);
  l.s(", y: ", Trc); l.Item(y, Trc); l.sl(")", Trc);
  ASSERT(FALSE)
END RealRelation;



PROCEDURE StringRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
VAR xadr, xlen, yadr, ylen, saveax, xchgcx, xchgsi, lbl: INTEGER;
BEGIN
Trace(TraceAny, "StringRelation("); l.Sym(op, Trc);
  l.s(", x: ", Trc); l.Item(x, Trc);
  l.s(", y: ", Trc); l.Item(y, Trc); l.sl(")", Trc);

  xadr := FindAndReserveReg(RSI, -{RAX, RCX});
  xlen := FindAndReserveReg(RCX, -{RAX});
  LoadStructure(xadr, xlen, x);
  Disassemble(x.mode, "Load string x");

  yadr := FindAndReserveReg(RDI, -{RAX});
  ylen := FindAndReserveReg(RDX, -{RAX});
  LoadStructure(yadr, ylen, y);
  Disassemble(y.mode, "Load string y");

  SaveExchange(xadr, RSI, xchgsi);
  SaveExchange(xlen, RCX, xchgcx);
  SaveExchange(-1,   RAX, saveax);
  Disassemble(0, "Adjust registers for loop lodsb");

  OpModRegRm(003BH, Reg, RCX, 8, y.index, -1, 0, 0, 0); (* cmp   xlen,ylen *)
  OpModRegRm(0F47H, Reg, RCX, 8, y.index, -1, 0, 0, 0); (* cmova xlen,ylen *)
  Disassemble(0, "Determine shorter string length");

  OpModRegRm(Minus+3, Reg, y.n, 8, RSI, -1, 0, 0, 0);   (* sub    y.n,rsi      *)
  AluOpFromImmediate(Minus, Reg, 1, 8, y.n, -1, 0, 0);  (* dec    y.n          *)
  Disassemble(0, "String offset y[n] from x[n+1]");
  lbl := X64.PC;                                        (* lbl:                     *)
  X64.Emit(0ACH);                                       (* lodsb                    *)
  OpModRegRm(Cmp+2, Eadr, RAX, 1, RSI, y.n, 0, 0, 0);   (* cmp    al,[rsi+y.n] *)
  Disassemble(0, "Compare one byte");

  X64.Emit(75H);  X64.Emit(0BH);                        (* jne    end               *)
  Disassemble(0, "If string mismatch");

  X64.Emit(8); X64.Emit(0C0H);                          (* or     al,al             *)
  X64.Emit(0E0H); X64.Emit(lbl-(X64.PC+1));             (* loopne lbl               *)
  Disassemble(0, "If not out of bytes, loop back");

  X64.Emit(74H);  X64.Emit(5);                          (* je     end               *)
  Disassemble(0, "If strings match");

  CallSysFn(UnterminatedStringProc);  (* error handler *)
  Disassemble(0, "Unterminated string error trap");

  RestoreReg(RSI, xchgsi);
  RestoreReg(RCX, xchgcx);
  RestoreReg(RAX, saveax);
  Disassemble(0, "Restore saved registers");

  FreeRegs(x);  FreeRegs(y);

  SetCondition(x, OpToIntCondition(op))
END StringRelation;




(* ---- Arithmetic operators ---- *)

PROCEDURE AddOp*(op: INTEGER; VAR x, y: Item);   (* x := x +- y *)
BEGIN
  Trace(TraceAny, "AddOp("); l.Sym(op, Trc);
  l.s("; x: ", Trc);         l.Item(x, Trc);
  l.s(", y: ", Trc);         l.Item(y, Trc); l.sl(")", Trc);
  ASSERT(op IN {ORS.plus, ORS.minus});

  IF (x.mode = Reg) & (x.n IN Free) THEN
    l.s("** x register ", Trc); l.Reg(8, x.n, Trc); l.sl(" not reserved.", Trc);
  END;
  IF (y.mode = Reg) & (y.n IN Free) THEN
    l.s("** y register ", Trc); l.Reg(8, y.n, Trc); l.sl(" not reserved.", Trc);
  END;

  IF (x.mode = Const) & (y.mode = Const) THEN
    IF op = ORS.plus THEN INC(x.n, y.n) ELSE DEC(x.n, y.n) END
  ELSE
    (* Convert op to x86 opcode *)
    IF op = ORS.plus THEN op := Plus ELSE op := Minus END;
    Load(x);
    Dyadic(op, x, y);
    Disassemble(y.mode, "AddOp")
  END
END AddOp;


PROCEDURE log2(m: INTEGER; VAR exponent: INTEGER): INTEGER;
BEGIN exponent := 0;
  WHILE ~ODD(m) DO m := m DIV 2; INC(exponent) END;
  RETURN m
END log2;

PROCEDURE MulImmediate(VAR x: Item; y: INTEGER);  (* x := x * y *)
VAR reg: INTEGER;
BEGIN
  ASSERT(x.mode >= Const);
  IF (y < -80000000H) OR (y >= 80000000H) THEN
    reg := FirstFreeReg();
    LoadImmediate(reg, y);
    OpModRegRm(0FAFH, Reg, x.n, 8, reg, -1, 0, 0, 0)
  ELSE
    Depar(x);
    IF x.mode IN {Reg, Eadr} THEN reg := x.n ELSE reg := FirstFreeReg() END;
    IF (y < -80H) OR (y >= 80H) THEN
      OpItem(69H, reg, 8, 4, x);  X64.EmitBytes(4, y)  (* imul reg,x,imm32 *)
    ELSE
      OpItem(6BH, reg, 8, 1, x);  X64.Emit(y)          (* imul reg,x,imm8 *)
    END;
    FreeRegs(x);  x.n := reg;  ReserveReg(reg)
  END
END MulImmediate;

PROCEDURE MulOp*(VAR x, y: Item);   (* x := x * y *)
VAR exponent: INTEGER;  ti: Item;
BEGIN Trace(TraceAny, "MulOp(x: "); l.Item(x, Trc); l.s(", y: ", Trc); l.Item(y, Trc); l.sl(")", Trc);
  ASSERT(x.mode >= Const);  ASSERT(y.mode >= Const);
  IF (x.mode = Const) & (y.mode = Const) THEN
    x.offset := x.offset * y.offset
  ELSE
    IF x.mode = Const THEN ti := x;  x:= y;  y := ti END;
    IF y.mode = Const THEN
      (* TODO: if y.n = 2 use shorter shl r/m64,1 instruction REX D1 /4 *)
      IF (y.n >= 2) & (log2(y.n, exponent) = 1) THEN
        Load(x);
        OpModRegRm(0C1H, Reg, 4, 8, x.n, -1, 0, 0, 1);  X64.Emit(exponent);  (* shl *)
      ELSE
        MulImmediate(x, y.n)
      END
    ELSE
      Load(x); Depar(y);
      OpItem(0FAFH, x.n, 8, 0, y)  (* imul x.n,y *)
    END;
    FreeRegs(y);
    Disassemble(y.mode, "MulOp")
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
  Trace(TraceAny, "DivOp("); l.s(name, Trc);
  l.s(", x: ", Trc);         l.Item(x, Trc);
  l.s(", y: ", Trc);         l.Item(y, Trc);  l.sl(")", Trc);
  ASSERT(x.mode >= Const);  ASSERT(y.mode >= Const);
  IF (x.mode = Const) & (y.mode = Const) THEN
    IF op = ORS.div THEN x.n := x.n DIV y.n ELSE x.n := x.n MOD y.n END
  ELSE
    IF (y.mode = Const) & (y.n >= 2) & (log2(y.n, exponent) = 1) THEN

      Load(x);
      IF op = ORS.div THEN (* DIV by power of 2 *)
        OpModRegRm(0C1H, Reg, 7, 8, x.n, -1, 0, 0, 1);  X64.Emit(exponent);  (* sar *)
      ELSE (* MOD power of 2 *)
        AluOpFromImmediate(And, Reg, y.n-1, 8, x.n, -1, 0, 0);
      END;
      reg := x.n;  (* Result in x.n *)
      Disassemble(0, "DivOp DIV/MOD by power of 2")

    ELSE (* DIV or MOD non-power of 2 *)

      saveRAX := FALSE;  saveRDX := FALSE;

      IF (x.mode = Reg) & (x.n = RAX) THEN
        (* numerator is already in rax *)
      ELSE
        IF ~(RAX IN Free) THEN
          (* Save rax temporarily *)
          saveRAX := TRUE;
          PushReg(RAX);  X64.AdjustStack(1);  ReleaseReg(RAX);
          Disassemble(0, "DivOp save rax")
        END;
        LoadReg(RAX, x);  ReserveReg(RAX);
        Disassemble(0, "DivOp load numerator to rax")
      END;

      IF ~(RDX IN Free) THEN
        Depar(y);
        saveRDX := TRUE;
        PushReg(RDX);  X64.AdjustStack(1);  ReleaseReg(RDX);
        IF (y.mode = Reg) & (y.n = RDX) THEN
          y.mode := Eadr;
          y.n    := RSP;
          y.n    := -8 * X64.SPO;
        END
      END;
      LoadImmediate(RDX, 0);  ReserveReg(RDX);
      Disassemble(0, "DivOp prepare rdx");

      IF y.mode = Const THEN
        reg := FirstFreeReg();  LoadImmediate(reg, y.n);
        OpModRegRm(0F7H, Reg, 7, 8, reg, -1, 0, 0, 0);   (* idiv reg *)
      ELSE
        Depar(y);
        OpItem(0F7H, 7, 8, 0, y);                        (* idiv y *)
      END;
      Disassemble(y.mode, "DivOp divide");

      IF op = ORS.div THEN  (* DIV Result in RAX *)

        IF saveRDX THEN
          PopReg(RDX);  X64.AdjustStack(-1);  ReserveReg(RDX);
          Disassemble(0, "Divop restore rdx")
        END;

        IF saveRAX THEN
          reg := FirstFreeReg();  MoveReg(reg, RAX);
          Disassemble(0, "Move result out of rax");
          PopReg(RAX);  X64.AdjustStack(-1);  ReserveReg(RAX);
          Disassemble(0, "Divop restore rax")
        ELSE
          reg := RAX
        END;

      ELSE (* MOD result in RDX *)

        IF saveRDX THEN
          reg := FirstFreeReg();  MoveReg(reg, RDX);
          Disassemble(0, "Move result out of rdx");
          PopReg(RDX);  X64.AdjustStack(-1);  ReserveReg(RDX);
          Disassemble(0, "Divop restore rdx")
        ELSE
          reg := RDX
        END;

        IF saveRAX THEN
          PopReg(RAX);  X64.AdjustStack(-1);  ReserveReg(RAX);
          Disassemble(0, "Divop restore rax")
        END

      END;

      IF ~saveRAX THEN ReleaseReg(RAX) END;
      IF ~saveRDX THEN ReleaseReg(RDX) END

    END;

    FreeRegs(x);  FreeRegs(y);
    x.mode := Reg;
    x.n    := reg;
    ReserveReg(reg);
    Disassemble(0, name)
  END;
END DivOp;


PROCEDURE RealOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
BEGIN
  Trace(TraceAny, "RealOp("); l.Sym(op, Trc);
  l.s("; x: ", Trc);         l.Item(x, Trc);
  l.s(", y: ", Trc);         l.Item(y, Trc); l.sl(")", Trc);
  ASSERT(FALSE)
END RealOp;

PROCEDURE Neg*(VAR x: Item);   (* x := -x *)
VAR size, op: INTEGER;
BEGIN Trace(TraceAny, "Neg(x: "); l.Item(x, Trc); l.l(Trc);
  IF x.mode = Const THEN
    x.n := -x.n
  ELSE
    Load(x);
    OpModRegRm(0F7H, Reg, 3, 8, x.n, -1, 0, 0, 0);
    Disassemble(x.mode, "Neg")
  END
END Neg;


(* ---- Control flow ---- *)

(* Use of Item.o fields for conditions:
*
*  o.n      - the condition code (uses symbol encodings ORS.eql .. ORS.geq)
*  o.offset - address of prev instr if any needing patching with curr target
*  o.index  - address of any forward jump list targeting this instruction
*)

PROCEDURE CFJump*(VAR x: X64.Item);
VAR cond: INTEGER;
BEGIN Trace(TraceAny, "CFJump(x: "); l.Item(x, Trc);  l.sl(")", Trc);
  IF x.mode # Cond THEN LoadFlags(x) END;
  cond := InvertedCondition(x.n);
  ASSERT(cond >= 0);
  IF cond > 0 THEN
    IF cond = 1 THEN  (* unconditional forward jump *)
      X64.Emit(0E9H)
    ELSE
      ASSERT(cond DIV 16 = 8);
      X64.Emit(0FH);  X64.Emit(cond)
    END;
    X64.EmitBytes(4, x.offset);  x.offset := X64.PC - 4;
    FixLink(x.index);  (* Fix any links that target following this instruction *)
    Disassemble(0, "CFJump")
  END
END CFJump;

PROCEDURE FJump*(VAR L: INTEGER);
BEGIN Trace(TraceAny, "FJump(L "); l.i(L, Trc); l.sl(")", Trc);
  X64.Emit(0E9H);  X64.EmitBytes(4, L);  L := X64.PC-4;
  Disassemble(0, "FJump")
END FJump;

PROCEDURE BJump*(L: INTEGER);
BEGIN Trace(TraceAny, "BJump(L "); l.i(L, Trc); l.sl(")", Trc);
  X64.Emit(0E9H);  X64.EmitBytes(4, L - (X64.PC + 4));
  Disassemble(0, "BJump")
END BJump;

PROCEDURE CBJump*(VAR x: X64.Item; L: INTEGER);
VAR cond, disp: INTEGER;
BEGIN
  Trace(TraceAny, "CBJump(x: "); l.Item(x, Trc);
  l.s(", L: ", Trc);             l.i(L, Trc); l.sl(")", Trc);
  IF x.mode # Cond THEN LoadFlags(x) END;
  cond := InvertedCondition(x.n);  ASSERT(cond DIV 16 = 8);
  ASSERT(L < X64.PC);
  IF (X64.PC + 2 - L) >= -80H THEN
    X64.Emit(70H + cond MOD 16);    X64.Emit(L - (X64.PC + 1))
  ELSE
    X64.Emit(0FH);  X64.Emit(cond); X64.EmitBytes(4, L - (X64.PC + 4))
  END;
  Disassemble(0, "CBJump");
  FixLink(x.index);
  FixLinkWith(x.offset, L)
END CBJump;

PROCEDURE FixOne*(at: INTEGER);
BEGIN Trace(TraceAny, "FixOne(at "); l.i(at, Trc); l.sl(")", Trc);
  ASSERT(FALSE)
END FixOne;

PROCEDURE Here*(): INTEGER;  BEGIN RETURN X64.PC END Here;

(*
FOR var := ctl TO lim BY inc DO ... END
For0:      w is immediate
           LoadReg(ctl)
           X64.Depar(var.o)

For1:  L0: Compare(ctl, lim);  FreeRegs(lim.o)
           ja/jb  L1
           StoreReg(var, ctl)  [Leaves any x & y reg usage unchanged]

           StatSeq

For2:      Dyadic(add, ctl, inc)

           jmp    L0
       L1:
*)

PROCEDURE For0*(VAR var, ctl: Item);
BEGIN
  Trace(TraceAny, "For0(var: "); l.Item(var, Trc);
  l.s(", ctl: ", Trc);           l.Item(ctl, Trc); l.sl(")", Trc);
  Load(ctl);
  Disassemble(0, "For0");
END For0;

PROCEDURE For1*(VAR var, ctl, lim, inc: Item; VAR L: INTEGER);
BEGIN
  Trace(TraceAny, "For1(var: "); l.Item(var, Trc);
  l.s(", ctl: ", Trc);           l.Item(ctl, Trc);
  l.s(", lim: ", Trc);           l.Item(lim, Trc);
  l.s(", inc: ", Trc);           l.Item(inc, Trc); l.sl(")", Trc);
  ASSERT(inc.mode = Const);
  IF inc.n = 0 THEN ORS.Mark("zero increment"); inc.n := 1 END;
  Dyadic(Cmp, ctl, lim);
  X64.Emit(0FH);
  IF inc.n > 0 THEN X64.Emit(87H) ELSE X64.Emit(83H) END; (* ja/jb *)
  L := X64.PC;
  X64.EmitBytes(4, 0);
  Load(ctl);
  StoreReg(ctl.n, var);
  FreeRegs(var);  FreeRegs(ctl);
  Disassemble(var.mode, "For1");
END For1;

PROCEDURE For2*(VAR var, ctl, inc: Item);
BEGIN
  Trace(TraceAny, "For2(var: "); l.Item(var, Trc);
  l.s(", ctl: ", Trc);           l.Item(ctl, Trc); l.sl(")", Trc);
  Dyadic(Plus, ctl, inc);
  FreeRegs(ctl);
  Disassemble(0, "For2");
END For2;


(* ---- Type descriptor construction and testing ---- *)

PROCEDURE EmitInt(i: INTEGER; comment: ARRAY OF CHAR);
BEGIN
  IF Disasmpc < X64.PC THEN Disassemble(0, "") END;
  ASSERT(Disasmpc = X64.PC);
  X64.EmitBytes(8, i);
  l.DisassembleInt(Disasmpc, comment)
END EmitInt;

PROCEDURE Q(T: ORB.Type);
BEGIN (*one entry of type descriptor extension table*)
  IF T.base # NIL THEN
    Q(T.base);
    Trace(TraceAny, "Q(T: "); l.Type(T, Trc); l.sl(")", Trc);
    EmitInt(LSL(T.mno, 16) + T.len,
            "type extension: 32/0, 16/mno, 16/import");
  END
END Q;

PROCEDURE FindPtrFlds(typ: ORB.Type; off: INTEGER);
VAR
  fld:  ORB.Object;
  i, s: INTEGER;
BEGIN
  IF (typ.form = ORB.Pointer) OR (typ.form = ORB.NilTyp) THEN
    EmitInt(off, "pointer/niltype field offset")
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
    Trace(TraceAny, "  '"); l.s(f.name, Trc);
    l.s("': ", Trc);        l.Type(f.type, Trc); l.l(Trc);
    f := f.next
  END
END ShowFields;

PROCEDURE BuildTD*(T: ORB.Type; VAR adr: INTEGER);
VAR k, s: INTEGER;
BEGIN
  Trace(TraceAny, "BuildTD(T "); l.Type(T, Trc);
  l.s(", adr ", Trc);            l.i(adr, Trc); l.sl(")", Trc);
  ASSERT(adr MOD 8 = 0);
  ShowFields(T);

  s := T.size; (* Convert size for heap allocation *)
  IF    s <= 24  THEN s := 32
  ELSIF s <= 56  THEN s := 64
  ELSIF s <= 120 THEN s := 128
  ELSE                s := (s+263) DIV 256 * 256
  END;

  T.len := X64.PC;  (* T.len overriden to be adddress of descriptor for record types *)

  EmitInt(s, "Type size including heap overhead");
  k          := T.nofpar;      (* extension level! *)
  IF k > 3 THEN
    ORS.Mark("ext level too large")
  ELSE
    Q(T);
    WHILE k < 3 DO
      EmitInt(-1, "unused extension level");
      INC(k)
    END
  END;
  FindPtrFlds(T, 0);
  EmitInt(-1, "end of type");


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
  FindPtrFlds(T, 0, Trc);
  Tdesc[Tdx] := -1;  INC(Tdx);
  IF Tdx >= MaxTD THEN
    ORS.Mark("too many record types"); Tdx := 0
  END
  *)
END BuildTD;

PROCEDURE TypeTest*(VAR x: X64.Item; T: ORB.Type; varpar, isguard: BOOLEAN);
BEGIN
  Trace(TraceAny, "TypeTest(x: "); l.Item(x, Trc);
  l.s(", T: ", Trc);               l.Type(T, Trc);
  IF varpar  THEN l.s(", varpar, ", Trc)  ELSE l.s("~varpar, ", Trc)  END;
  IF isguard THEN l.sl(", isguard)", Trc) ELSE l.sl(", ~isguard)", Trc) END;
  (*ASSERT(FALSE)*)
END TypeTest;


(* ---- ---- *)

PROCEDURE Open*(sourcefn, modid: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN
  l.Init(sourcefn);
  Trflags := {TraceAny, TraceMakeItem};  (*{TraceAny, TraceMakeItem};*)
  Trace(TraceAny, "ORG Open('"); l.s(sourcefn, Trc); l.sl("')", Trc);
  X64.ClearStack;   ClearRegs;
  X64.Init;
  Strx     := 0;
  StrFInx  := 0;
  Varsize  := 0;
  VarFInx  := 0;
  ImpFInx  := 0;
  Disasmpc := 0;
  FOR i := 1 TO SYSTEM.SIZE(X64.CodeHeader) DO X64.Emit(0) END;
  X64.Align(8); (* Increase alignment to match PO2013 where records are aligned on 8 bytes. *)
  X64.EmitString(modid);
  X64.Align(16);
  Disasmpc := X64.PC
END Open;

PROCEDURE SetDataSize*(dc: INTEGER);
BEGIN Varsize := dc END SetDataSize;

PROCEDURE Header*;
BEGIN
  Trace(TraceAny, "Header - module initialisation code entry point."); l.l(Trc);
  X64.Header.initcode := X64.PC;  (* Set entry point address *)
  (* Save non-volatile registers (for testing from Patchouli Oberon) *)
  PushReg(RBX);  PushReg(RBP);  PushReg(RSI);  PushReg(RDI);
  PushReg(12);   PushReg(13);   PushReg(14);   PushReg(15);
END Header;

PROCEDURE wsize(size: INTEGER);
BEGIN
  l.h(size,  Trc);  l.c("H", Trc);
  l.s(", ", Trc);   l.i(size, Trc);
END wsize;

PROCEDURE EmitPtrs(typ: ORB.Type; adr: INTEGER);
VAR fld: ORB.Object;  i, s: INTEGER;
BEGIN
  IF (typ.form = ORB.Pointer) OR (typ.form = ORB.NilTyp) THEN
    X64.EmitBytes(8, adr)
  ELSIF typ.form = ORB.Record THEN
    fld := typ.dsc;
    WHILE fld # NIL DO EmitPtrs(fld.type, fld.val + adr); fld := fld.next END
  ELSIF typ.form = ORB.Array THEN
    s := typ.base.size;
    FOR i := 0 TO typ.len-1 DO EmitPtrs(typ.base, i*s + adr) END
  END
END EmitPtrs;

PROCEDURE Close*(modid: ORS.Ident; key, exno: INTEGER);
VAR
  i:      INTEGER;
  reslen: INTEGER;
  obj:    ORB.Object;
  fname:  ARRAY 40 OF CHAR;
  F:      Files.File;
  R:      Files.Rider;
BEGIN
  PopReg(15);   PopReg(14);   PopReg(13);   PopReg(12);
  PopReg(RDI);  PopReg(RSI);  PopReg(RBP);  PopReg(RBX);
  X64.Emit(0C3H);  (* ret *)
  Disassemble(0, "Return");
  X64.Emit(2EH);  (* Mark end of code with ignored instruction 'seg cs' *)
  EmitStrings;

  Trace(TraceAny, "Close:"); l.l(Trc);
  l.s("  modid:         ", Trc);  l.sl(modid,  Trc);
  l.s("  key:           ", Trc);  l.h(key,     Trc);  l.l(Trc);
  l.s("  exno:          ", Trc);  l.i(exno,    Trc);  l.l(Trc);
  l.s("  code size:     ", Trc);  wsize(X64.PC);      l.l(Trc);
  l.s("  VAR size:      ", Trc);  wsize(Varsize);     l.l(Trc);
  l.s("  VAR fixups:    ", Trc);  l.i(VarFInx, Trc);  l.l(Trc);
  l.s("  Import fixups: ", Trc);  l.i(ImpFInx, Trc);  l.l(Trc);
  l.l(Trc);

  (* Prepare to patch header pointers directly into X64.Text *)

  X64.Header.varsize := Varsize;
  X64.Header.key     := key;

  (* Emit global VAR pointer table *)
  X64.Align(16);
  X64.Header.pointers := X64.PC;  (* Set global VAR pointer table address *)
  obj := ORB.topScope.next;
  WHILE obj # NIL DO
    IF obj.class = ORB.Var THEN EmitPtrs(obj.type, obj.val) END;
    obj := obj.next
  END;
  X64.EmitBytes(8, -1);

  (* Emit command table *)
  X64.Align(16);
  X64.Header.commands := X64.PC;   (* Set command table address *)
  obj := ORB.topScope.next;
  WHILE obj # NIL DO  (* commands *)
    IF (obj.exno # 0) & (obj.class = ORB.Const) & (obj.type.form = ORB.Proc)
     & (obj.type.nofpar = 0) & (obj.type.base = ORB.noType) THEN
      X64.EmitString(obj.name); X64.EmitBytes(4, obj.val)
    END;
    obj := obj.next
  END;
  X64.Emit(0);

  (* Determine export table offset and resident length *)
  X64.Align(16);
  X64.Header.exports := X64.PC;     (* Set export table address *)
  reslen := (X64.PC + exno * 4 + 15) DIV 16 * 16;

  (* Emit export table *)
  l.l(Trc);  l.sl("Exports:", Trc);
  obj := ORB.topScope.next;
  WHILE obj # NIL DO  (* entries *)
    IF obj.exno # 0 THEN
      IF    obj.class = ORB.Const THEN ASSERT(obj.type.form = ORB.Proc);
                                       l.s("  proc   ", Trc); l.h(obj.val, Trc); l.sl("H.", Trc);
                                       X64.EmitBytes(4, obj.val)
      ELSIF obj.class = ORB.Var   THEN l.s("  var    ", Trc); l.h(reslen + obj.val, Trc); l.sl("H.", Trc);
                                       X64.EmitBytes(4, reslen + obj.val)
      ELSIF obj.class = ORB.Typ   THEN
        IF obj.type.form = ORB.Record THEN
          l.s("  record ", Trc); l.h(obj.type.len, Trc); l.sl("H.", Trc);
          X64.EmitBytes(4, obj.type.len MOD 10000H)
        ELSIF (obj.type.form = ORB.Pointer)
            & ((obj.type.base.typobj = NIL) OR (obj.type.base.typobj.exno = 0)) THEN
          l.s("  ptr    ", Trc); l.h(obj.type.base.len, Trc); l.sl("H.", Trc);
          X64.EmitBytes(4, obj.type.base.len MOD 10000H)
        END
      END
    END;
    obj := obj.next
  END;
  l.s("Export count ", Trc); l.i((X64.PC - X64.Header.exports) DIV 4, Trc); l.sl(".", Trc);
  X64.EmitBytes(4, -1);  (* Mark end of table *)

  (* End of resident code/tables *)

  X64.Align(16);
  IF X64.PC # reslen THEN
    l.l(Trc); l.s("** PC ", Trc);       l.h(X64.PC, Trc);
    l.s("H, reslen ", Trc);             l.h(reslen, Trc);
    l.s("H, X64.Header.exports ", Trc); l.h(X64.Header.exports, Trc);
    l.s("H, exno ", Trc);               l.h(exno, Trc);
    l.sl("H **", Trc)
  END;
  ASSERT(X64.PC = reslen);
  X64.Header.imports := reslen; (* Set resident size / var offset *)

  (* Relocate code references to module global VARs *)
  FOR i:= 0 TO VarFInx-1 DO
    X64.Patch(VarFix[i], 4, X64.Peek(VarFix[i], 4, FALSE) + reslen)
  END;

  (* Emit imported module names and keys *)
  obj := ORB.topScope.next;
  WHILE (obj # NIL) & (obj.class = ORB.Mod) DO  (* imports *)
    IF obj.dsc # ORB.system THEN
      X64.EmitString(obj(ORB.Module).orgname);  (* name *)
      X64.EmitBytes(8, obj.val)                 (* key  *)
    END;
    obj := obj.next
  END;
  X64.Emit(0);

  (* Emit imports *)
  X64.Align(16);
  X64.EmitBytes(4, ImpFInx);
  FOR i := 0 TO ImpFInx - 1 DO
    X64.EmitBytes(4, ImpFix[i].offset);
    X64.EmitBytes(4, ImpFix[i].fix);
  END;
  X64.EmitBytes(4, -1);
  WHILE X64.PC MOD 16 # 0 DO X64.Emit(0) END;
  X64.Header.length := X64.PC;

  (* Display header content *)
  l.sl("Header:", Trc);
  l.s("  Length:        ", Trc);  l.h(X64.Header.length,   Trc);  l.sl("H.", Trc);
  l.s("  Init code:     ", Trc);  l.h(X64.Header.initcode, Trc);  l.sl("H.", Trc);
  l.s("  Pointers:      ", Trc);  l.h(X64.Header.pointers, Trc);  l.sl("H.", Trc);
  l.s("  Commands:      ", Trc);  l.h(X64.Header.commands, Trc);  l.sl("H.", Trc);
  l.s("  Exports:       ", Trc);  l.h(X64.Header.exports,  Trc);  l.sl("H.", Trc);
  l.s("  Imports:       ", Trc);  l.h(X64.Header.imports,  Trc);  l.sl("H.", Trc);
  l.s("  VAR size:      ", Trc);  l.h(X64.Header.varsize,  Trc);  l.sl("H.", Trc);

  (* Write compiled module *)
  fname := "";  K.Append(modid, fname);  K.Append(".code", fname);
  F := Files.New(fname);  Files.Set(R, F, 0);
  Files.WriteBytes(R, X64.Text, X64.PC);
  Files.Register(F)
END Close;


(* ---- Initialisation ---- *)

BEGIN
END ORG.
