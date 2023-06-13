MODULE ORG; (* N.Wirth, 16.4.2016 / 4.4.2017 / 17.9.2018  Oberon compiler; code generator for RISC*)

IMPORT SYSTEM, Files, ORS, ORB, w := Writer, K := Kernel;

(*Code generator for Oberon compiler for RISC processor.
   Procedural interface to Parser OSAP; result in array "code".
   Procedure Close writes code-files*)

CONST
  WordSize* = 4;

  StkOrg0 = -64; VarOrg0 = 0;  (*for RISC-0 only*)

  MT  = 12;   (*dedicated registers*)
  SP  = 14;
  LNK = 15;

  maxCode = 12000;
  maxStrx =  8000;
  maxTD   =   160;
  C24     = 1000000H;

  Reg  = 10;  (*internal item modes*)
  RegI = 11;
  Cond = 12;

  (*frequently used opcodes*)
  U = 2000H; V = 1000H;
  Mov = 0;  Lsl = 1;  Asr = 2;  Ror = 3;  And = 4;  Ann = 5; Ior = 6; Xor = 7;
  Add = 8;  Sub = 9;  Cmp = 9;  Mul = 10; Div = 11;
  Fad = 12; Fsb = 13; Fml = 14; Fdv = 15;
  Ldr = 8;  Str = 10;
  BR = 0;   BLR = 1;  BC = 2;   BL = 3;
  MI = 0;   PL = 8;   EQ = 1;   NE = 9; LT = 5; GE = 13; LE = 6; GT = 14;

  TYPE
    Item* = RECORD
      mode*:     INTEGER;
      type*:     ORB.Type;
      a*, b*, r: INTEGER;
      rdo*:      BOOLEAN  (*read only*)
    END;


(* ----------- Item forms and meaning of fields: ----------- *)
(*                                                           *)
(*      mode   r      a       b                              *)
(*      --------------------------------                     *)
(*   1  Const  -      value   (proc adr)  (immediate value)  *)
(*   2  Var    base   off     -           (direct adr)       *)
(*   3  Par    -      off0    off1        (indirect adr)     *)
(*  10  Reg    regno                                         *)
(*  11  RegI   regno  off     -                              *)
(*  12  Cond   cond   Fchain  Tchain                         *)
(*                                                           *)
(*  base:  < 0 => imported var, base = -(module number)      *)
(*         = 0 => module global - scope level 0              *)
(*         > 0 => procedure local stack - level = nesting    *)
(*  MakeItem sets x.r to objects .lev.                       *)
(*                                                           *)

VAR
  pc*, varsize: INTEGER;  (* program counter, data index *)
  tdx, strx:    INTEGER;
  entry:        INTEGER;  (* main entry point *)
  RH:           INTEGER;  (* available registers R[0] ... R[H-1] *)
  frame:        INTEGER;  (* frame offset changed in SaveRegs and RestoreRegs *)
  fixorgP:      INTEGER;  (* origins of lists of locations to be fixed up by loader *)
  fixorgD:      INTEGER;
  fixorgT:      INTEGER;
  check:        BOOLEAN;  (* emit run-time checks *)
  version:      INTEGER;  (* 0 = RISC-0, 1 = RISC-5 *)

  relmap: ARRAY 6       OF INTEGER;  (*condition codes for relations*)
  code:   ARRAY maxCode OF INTEGER;
  data:   ARRAY maxTD   OF INTEGER;  (*type descriptors*)
  str:    ARRAY maxStrx OF CHAR;



(* -------------------------- instruction listing --------------------------- *)

PROCEDURE ins(src: ARRAY OF CHAR;  VAR x: INTEGER;  VAR dst: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN i := 0;
  WHILE src[i] # 0X DO dst[x] := src[i];  INC(i); INC(x) END
END ins;

PROCEDURE dop(op, u: INTEGER; VAR x: INTEGER; VAR dis: ARRAY OF CHAR);
BEGIN
  IF    op =  0 THEN ins("mov", x, dis);  IF u # 0  THEN ins("u", x, dis) END;
  ELSIF op =  1 THEN ins("lsl", x, dis)
  ELSIF op =  2 THEN ins("asr", x, dis)
  ELSIF op =  3 THEN ins("ror", x, dis)
  ELSIF op =  4 THEN ins("and", x, dis)
  ELSIF op =  5 THEN ins("ann", x, dis)
  ELSIF op =  6 THEN ins("ior", x, dis)
  ELSIF op =  7 THEN ins("xor", x, dis)
  ELSIF op =  8 THEN ins("add", x, dis);  IF u # 0  THEN ins("c", x, dis) END;
  ELSIF op =  9 THEN ins("sub", x, dis);  IF u # 0  THEN ins("c", x, dis) END;
  ELSIF op = 10 THEN ins("mul", x, dis);  IF u # 0  THEN ins("u", x, dis) END;
  ELSIF op = 11 THEN ins("div", x, dis)
  ELSIF op = 12 THEN ins("fad", x, dis)
  ELSIF op = 13 THEN ins("fsb", x, dis)
  ELSIF op = 14 THEN ins("fml", x, dis)
  ELSIF op = 15 THEN ins("fdv", x, dis)
  END
END dop;

PROCEDURE ini(i: INTEGER;  VAR x: INTEGER;  VAR dis: ARRAY OF CHAR);
VAR num: ARRAY 30 OF CHAR;
BEGIN K.IntToDecimal(i, num);  ins(num, x, dis) END ini;

PROCEDURE inh(i: INTEGER;  VAR x: INTEGER;  VAR dis: ARRAY OF CHAR);
VAR num: ARRAY 30 OF CHAR;
BEGIN K.IntToHex(i, num);  ins(num, x, dis) END inh;

PROCEDURE dreg(r: INTEGER;  VAR x: INTEGER;  VAR dis: ARRAY OF CHAR);
BEGIN
  IF    r = 15 THEN ins("lnk", x, dis)
  ELSIF r = 14 THEN ins("sp", x, dis)
               ELSE ins("r", x, dis); ini(r, x, dis)
  END
END dreg;

PROCEDURE dcond(cond: INTEGER;  VAR x: INTEGER;  VAR dis: ARRAY OF CHAR);
BEGIN
  IF    cond = 0 THEN ins("mi",x,dis)  ELSIF cond =  8 THEN ins("pl",x,dis)
  ELSIF cond = 1 THEN ins("eq",x,dis)  ELSIF cond =  9 THEN ins("ne",x,dis)
  ELSIF cond = 2 THEN ins("cs",x,dis)  ELSIF cond = 10 THEN ins("cc",x,dis)
  ELSIF cond = 3 THEN ins("vs",x,dis)  ELSIF cond = 11 THEN ins("vc",x,dis)
  ELSIF cond = 4 THEN ins("ls",x,dis)  ELSIF cond = 12 THEN ins("hi",x,dis)
  ELSIF cond = 5 THEN ins("lt",x,dis)  ELSIF cond = 13 THEN ins("ge",x,dis)
  ELSIF cond = 6 THEN ins("le",x,dis)  ELSIF cond = 14 THEN ins("gt",x,dis)
  ELSIF cond = 7 THEN ins("br",x,dis)  ELSIF cond = 15 THEN ins("bn",x,dis)
  END
END dcond;

PROCEDURE dism(pc: INTEGER; comment: ARRAY OF CHAR);
VAR
  dis: ARRAY 41 OF CHAR;
  in, u, v, a, b, op, im, moff, boff, c, x, class: INTEGER;
BEGIN
  IF FALSE THEN
  dis := "                                        ";
  in  := code[pc] MOD 100000000H;
  IF    comment = "FIX"   THEN w.s("FIX     ")
  ELSIF comment = "MERGE" THEN w.s("MERGE   ")
  ELSE
    w.in(ORS.linenum, -3);  w.s(":");  w.in(ORS.Pos() - ORS.linebeg, 4)
  END;
  w.hn(pc, -6); w.s(": ");  w.hn(in, -8);  w.s("  ");

  class := ASR(in, 30) MOD 4;
  u     := ASR(in, 29) MOD 2;
  v     := ASR(in, 28) MOD 2;
  a     := ASR(in, 24) MOD 10H;
  b     := ASR(in, 20) MOD 10H;
  op    := ASR(in, 16) MOD 10H;
  moff  := ASR(LSL(in,44),44);  (* memory offset 20 bits sign extended *)
  boff  := ASR(LSL(in,40),40);  (* branch offset 24 bits sign extended *)
  im    := in MOD 10000H;
  c     := in MOD 10H;

  x := 0;
  IF class = 0 THEN (* Register operation *)
    dop(op,u,x,dis);  x := 5;  dreg(a,x,dis);  x := 9;
    IF op # 0 THEN dreg(b,x,dis); ins(",",x,dis) END;
    dreg(c,x,dis)
  ELSIF class = 1 THEN (* Immediate operation *)
    dop(op,u,x,dis);  x := 5;  dreg(a,x,dis);  x := 9;
    IF op # 0 THEN dreg(b,x,dis); ins(",",x,dis) END;
    IF v = 0 THEN ini(im,x,dis) ELSE ini(im+0FFFF0000H,x,dis) END
  ELSIF class = 2 THEN (* Memory operation *)
    IF u = 0 THEN ins("ld",x,dis) ELSE ins("st",x,dis) END;  x := 5;
    dreg(a,x,dis);  IF v # 0 THEN ins("b",x,dis) END;
    x := 9;  dreg(b,x,dis);
    IF moff # 0 THEN ins("+",x,dis);  ini(moff,x,dis) END;
  ELSIF class = 3 THEN (* Branch operation *)
    dcond(a,x,dis);  x := 9;
    IF u = 0 THEN dreg(c,x,dis) ELSE
      IF boff #0 THEN ins("$",x,dis);  inh(pc+boff+1,x,dis) END
    END;
    IF v # 0 THEN ins(",L",x,dis) END;
  END;

  dis[30] := 0X;  w.s(dis);
  IF (comment # "") & (comment # "FIX")  & (comment # "MERGE") THEN
    w.s("; ");  w.s(comment)
  END;
  w.l
  ;END
END dism;


(*instruction assemblers according to formats*)

PROCEDURE Put0(op, a, b, c: INTEGER; comment: ARRAY OF CHAR);
BEGIN (*emit format-0 instruction*)
  code[pc] := ((a*10H + b) * 10H + op) * 10000H + c; dism(pc, comment);  INC(pc)
END Put0;

PROCEDURE Put1(op, a, b, im: INTEGER; comment: ARRAY OF CHAR);
BEGIN (*emit format-1 instruction,  -10000H <= im < 10000H*)
  IF im < 0 THEN INC(op, V) END;
  code[pc] := (((a+40H) * 10H + b) * 10H + op) * 10000H + (im MOD 10000H);
  dism(pc, comment);  INC(pc)
END Put1;

PROCEDURE Put1a(op, a, b, im: INTEGER; comment: ARRAY OF CHAR);
BEGIN (*same as Put1, but with range test  -10000H <= im < 10000H*)
  IF (im >= -10000H) & (im <= 0FFFFH) THEN Put1(op, a, b, im, comment)
  ELSE Put1(Mov+U, RH, 0, im DIV 10000H, comment);
    IF im MOD 10000H # 0 THEN Put1(Ior, RH, RH, im MOD 10000H, "") END;
    Put0(op, a, b, RH, "")
  END
END Put1a;

PROCEDURE Put2(op, a, b, off: INTEGER; comment: ARRAY OF CHAR);
BEGIN (*emit load/store instruction*)
  code[pc] := ((op * 10H + a) * 10H + b) * 100000H + (off MOD 100000H);
  dism(pc, comment);  INC(pc)
END Put2;

PROCEDURE Put3(op, cond, off: INTEGER; comment: ARRAY OF CHAR);
BEGIN (*emit branch instruction*)
  code[pc] := ((op+12) * 10H + cond) * 1000000H + (off MOD 1000000H);
  dism(pc, comment);  INC(pc)
END Put3;

PROCEDURE incR;
BEGIN
  IF RH < MT-1 THEN INC(RH) ELSE ORS.Mark("register stack overflow") END
END incR;

PROCEDURE CheckRegs*;
BEGIN
  IF RH # 0 THEN ORS.Mark("Reg Stack"); RH := 0 END;
  IF pc >= maxCode - 40 THEN ORS.Mark("program too long") END;
  IF frame # 0 THEN ORS.Mark("frame error"); frame := 0 END
END CheckRegs;

PROCEDURE SetCC(VAR x: Item; n: INTEGER);
BEGIN x.mode := Cond; x.a := 0; x.b := 0; x.r := n
END SetCC;

PROCEDURE Trap(cond, num: INTEGER);
BEGIN Put3(BLR, cond, ORS.Pos()*100H + num*10H + MT, "TRAP")
END Trap;

(*handling of forward reference, fixups of branch addresses and constant tables*)

PROCEDURE negated(cond: INTEGER): INTEGER;
BEGIN
  IF cond < 8 THEN cond := cond+8 ELSE cond := cond-8 END;
  RETURN cond
END negated;

PROCEDURE fix(at, with: INTEGER);
BEGIN code[at] := code[at] DIV C24 * C24 + (with MOD C24); dism(at, "FIX");
END fix;

PROCEDURE FixOne*(at: INTEGER);
BEGIN fix(at, pc-at-1)
END FixOne;

PROCEDURE FixLink*(L: INTEGER);
  VAR L1: INTEGER;
BEGIN
  WHILE L # 0 DO L1 := code[L] MOD 40000H; fix(L, pc-L-1); L := L1 END
END FixLink;

PROCEDURE FixLinkWith(L0, dst: INTEGER);
  VAR L1: INTEGER;
BEGIN
  WHILE L0 # 0 DO
    L1 := code[L0] MOD C24;
    code[L0] := code[L0] DIV C24 * C24 + ((dst - L0 - 1) MOD C24);
    dism(L0, "FIX");  L0 := L1
  END
END FixLinkWith;

PROCEDURE merged(L0, L1: INTEGER): INTEGER;
  VAR L2, L3: INTEGER;
BEGIN
  IF L0 # 0 THEN L3 := L0;
    REPEAT L2 := L3; L3 := code[L2] MOD 40000H UNTIL L3 = 0;
    code[L2] := code[L2] + L1; dism(L2, "MERGE"); L1 := L0
  END;
  RETURN L1
END merged;


(* loading of operands and addresses into registers *)

PROCEDURE GetSB(base: INTEGER; comment: ARRAY OF CHAR);
BEGIN
  IF version = 0 THEN Put1(Mov, RH, 0, VarOrg0, comment)
  ELSE Put2(Ldr, RH, -base, pc-fixorgD, comment); fixorgD := pc-1
  END
END GetSB;

PROCEDURE NilCheck;
BEGIN IF check THEN Trap(EQ, 4) END
END NilCheck;

PROCEDURE load(VAR x: Item);
  VAR op: INTEGER;
BEGIN
  IF x.type.size = 1 THEN op := Ldr+1 ELSE op := Ldr END;
  IF x.mode # Reg THEN
    IF x.mode = ORB.Const THEN
      IF x.type.form = ORB.Proc THEN
        IF    x.r > 0 THEN ORS.Mark("not allowed")
        ELSIF x.r = 0 THEN Put3(BL, 7, 0, "Load module procedure");
                           Put1a(Sub, RH, LNK, pc*4 - x.a, "")
                      ELSE GetSB(x.r, "Load imported procedure"); (*mark as progbase-relative*)
                           Put1(Add, RH, RH, x.a + 100H, "")
        END
      ELSIF (x.a <= 0FFFFH) & (x.a >= -10000H) THEN
        Put1(Mov, RH, 0, x.a, "Load (short) constant")
      ELSE
        Put1(Mov+U, RH, 0, x.a DIV 10000H MOD 10000H, "Load (long) constant");
        IF x.a MOD 10000H # 0 THEN Put1(Ior, RH, RH, x.a MOD 10000H, "") END
      END;
      x.r := RH; incR
    ELSIF x.mode = ORB.Var THEN
      IF x.r > 0 THEN Put2(op, RH, SP, x.a + frame, "Load local var") (*local*)
                 ELSE GetSB(x.r, "Load global/imported var"); Put2(op, RH, RH, x.a, "")
      END;
      x.r := RH; incR
    ELSIF x.mode = ORB.Par THEN Put2(Ldr, RH, SP, x.a + frame, "Load parameter");
                                Put2(op, RH, RH, x.b, "");
                                x.r := RH; incR
    ELSIF x.mode = RegI    THEN Put2(op, x.r, x.r, x.a, "Load RegI")
    ELSIF x.mode = Cond THEN
      Put3(BC, negated(x.r), 2, "Load condition");
      FixLink(x.b); Put1(Mov, RH, 0, 1, ""); Put3(BC, 7, 1, "");
      FixLink(x.a); Put1(Mov, RH, 0, 0, ""); x.r := RH; incR
    END;
    x.mode := Reg
  END
END load;

PROCEDURE loadAdr(VAR x: Item);
BEGIN
  IF x.mode = ORB.Var THEN
    IF x.r > 0 THEN Put1a(Add, RH, SP, x.a + frame, "LoadAdr local var")  (*local*)
               ELSE GetSB(x.r, "LoadAdr global/imported var"); Put1a(Add, RH, RH, x.a, "")
    END;
    x.r := RH; incR
  ELSIF x.mode = ORB.Par THEN Put2(Ldr, RH, SP, x.a + frame, "LoadAdr parameter");
                              IF x.b # 0 THEN Put1a(Add, RH, RH, x.b, "") END;
                              x.r := RH; incR
  ELSIF x.mode = RegI    THEN IF x.a # 0 THEN Put1a(Add, x.r, x.r, x.a, "LoadAdr RagI") END
  ELSE ORS.Mark("address error")
  END;
  x.mode := Reg
END loadAdr;

PROCEDURE loadCond(VAR x: Item);
BEGIN
  IF x.type.form = ORB.Bool THEN
    IF x.mode = ORB.Const THEN x.r := 15 - x.a*8
                          ELSE load(x);
                               IF code[pc-1] DIV 40000000H # -2 THEN Put1(Cmp, x.r, x.r, 0, "LoadCond") END;
                               x.r := NE; DEC(RH)
    END;
    x.mode := Cond; x.a := 0; x.b := 0
  ELSE ORS.Mark("not Boolean?")
  END
END loadCond;

PROCEDURE loadTypTagAdr(T: ORB.Type);
  VAR x: Item;
BEGIN x.mode := ORB.Var; x.a := T.len; x.r := -T.mno; loadAdr(x)
END loadTypTagAdr;

PROCEDURE loadStringAdr(VAR x: Item);
BEGIN
  GetSB(0, "LoadStringAdr"); Put1a(Add, RH, RH, varsize+x.a, "");
  x.mode := Reg; x.r := RH; incR
END loadStringAdr;


(* Items: Conversion from constants or from Objects on the Heap to Items on the Stack*)

PROCEDURE MakeConstItem*(VAR x: Item; typ: ORB.Type; val: INTEGER);
BEGIN x.mode := ORB.Const; x.type := typ; x.a := val
END MakeConstItem;

PROCEDURE MakeRealItem*(VAR x: Item; val: REAL);
BEGIN x.mode := ORB.Const; x.type := ORB.realType; x.a := SYSTEM.VAL(INTEGER, val)
END MakeRealItem;

PROCEDURE MakeStringItem*(VAR x: Item; len: INTEGER); (*copies string from ORS-buffer to ORG-string array*)
  VAR i: INTEGER;
BEGIN x.mode := ORB.Const; x.type := ORB.strType; x.a := strx; x.b := len; i := 0;
  IF strx + len + 4 < maxStrx THEN
    WHILE len > 0 DO str[strx] := ORS.str[i]; INC(strx); INC(i); DEC(len) END;
    WHILE strx MOD 4 # 0 DO str[strx] := 0X; INC(strx) END
  ELSE ORS.Mark("too many strings")
  END
END MakeStringItem;

PROCEDURE MakeItem*(VAR x: Item; y: ORB.Object; curlev: INTEGER);
BEGIN x.mode := y.class; x.type := y.type; x.a := y.val; x.rdo := y.rdo;
  IF y.class = ORB.Par THEN x.b := 0
  ELSIF (y.class = ORB.Const) & (y.type.form = ORB.String) THEN x.b := y.lev  (*len*) ;
  ELSE x.r := y.lev
  END;
  IF (y.lev > 0) & (y.lev # curlev) & (y.class # ORB.Const) THEN ORS.Mark("not accessible ") END
END MakeItem;

(* Code generation for Selectors, Variables, Constants *)

PROCEDURE Field*(VAR x: Item; y: ORB.Object);   (* x := x.y *)
BEGIN;
  IF x.mode = ORB.Var THEN
    IF x.r >= 0 THEN x.a := x.a + y.val
    ELSE loadAdr(x); x.mode := RegI; x.a := y.val
    END
  ELSIF x.mode = RegI THEN x.a := x.a + y.val
  ELSIF x.mode = ORB.Par THEN x.b := x.b + y.val
  END
END Field;

PROCEDURE Index*(VAR x, y: Item);   (* x := x[y] *)
  VAR s, lim: INTEGER;
BEGIN s := x.type.base.size; lim := x.type.len;
  IF (y.mode = ORB.Const) & (lim >= 0) THEN
    IF (y.a < 0) OR (y.a >= lim) THEN ORS.Mark("bad index") END;
    IF x.mode IN {ORB.Var, RegI} THEN x.a := y.a * s + x.a
    ELSIF x.mode = ORB.Par THEN x.b := y.a * s + x.b
    END
  ELSE load(y);
    IF check THEN  (* check array bounds *)
      IF lim >= 0 THEN Put1a(Cmp, RH, y.r, lim, "Check array bounds")
                  ELSE (*open array*)
                       IF x.mode IN {ORB.Var, ORB.Par} THEN
                         Put2(Ldr, RH, SP, x.a+4+frame, "Check open array bounds");
                         Put0(Cmp, RH, y.r, RH, "")
                       ELSE ORS.Mark("error in Index")
                       END
      END;
      Trap(10, 1)  (*BCC*)
    END;
    IF s = 4 THEN Put1(Lsl, y.r, y.r, 2, "index") ELSIF s > 1 THEN Put1a(Mul, y.r, y.r, s, "index") END;
    IF x.mode = ORB.Var THEN
      IF x.r > 0 THEN Put0(Add, y.r, SP, y.r, "index local var"); INC(x.a, frame)
      ELSE GetSB(x.r, "index global/imported var");
        IF x.r = 0 THEN Put0(Add, y.r, RH, y.r, "")
        ELSE Put1a(Add, RH, RH, x.a, ""); Put0(Add, y.r, RH, y.r, ""); x.a := 0
        END
      END;
      x.r := y.r; x.mode := RegI
    ELSIF x.mode = ORB.Par THEN
      Put2(Ldr, RH, SP, x.a + frame, "index parameter");
      Put0(Add, y.r, RH, y.r, ""); x.mode := RegI; x.r := y.r; x.a := x.b
    ELSIF x.mode = RegI THEN Put0(Add, x.r, x.r, y.r, "index RegI"); DEC(RH)
    END
  END
END Index;

PROCEDURE DeRef*(VAR x: Item);
BEGIN
  IF x.mode = ORB.Var THEN
    IF x.r > 0 THEN Put2(Ldr, RH, SP, x.a + frame, "deref local var")  (*local*)
               ELSE GetSB(x.r, "deref global/imported var"); Put2(Ldr, RH, RH, x.a, "") END;
    NilCheck; x.r := RH; incR
  ELSIF x.mode = ORB.Par THEN
    Put2(Ldr, RH, SP, x.a + frame, "deref parameter"); Put2(Ldr, RH, RH, x.b, "");
    NilCheck; x.r := RH; incR
  ELSIF x.mode = RegI THEN Put2(Ldr, x.r, x.r, x.a, "deref RegI"); NilCheck
  ELSIF x.mode # Reg THEN ORS.Mark("bad mode in DeRef")
  END;
  x.mode := RegI; x.a := 0; x.b := 0
END DeRef;

PROCEDURE Q(T: ORB.Type; VAR dcw: INTEGER);
BEGIN (*one entry of type descriptor extension table*)
  IF T.base # NIL THEN
    Q(T.base, dcw); data[dcw] := (T.mno*1000H + T.len) * 1000H + dcw - fixorgT;
    fixorgT := dcw; INC(dcw)
  END
END Q;

PROCEDURE FindPtrFlds(typ: ORB.Type; off: INTEGER; VAR dcw: INTEGER);
  VAR fld: ORB.Object; i, s: INTEGER;
BEGIN
  IF (typ.form = ORB.Pointer) OR (typ.form = ORB.NilTyp) THEN data[dcw] := off; INC(dcw)
  ELSIF typ.form = ORB.Record THEN
    fld := typ.dsc;
    WHILE fld # NIL DO FindPtrFlds(fld.type, fld.val + off, dcw); fld := fld.next END
  ELSIF typ.form = ORB.Array THEN
    s := typ.base.size;
    FOR i := 0 TO typ.len-1 DO FindPtrFlds(typ.base, i*s + off, dcw) END
  END
END FindPtrFlds;

PROCEDURE BuildTD*(T: ORB.Type; VAR dc: INTEGER);
  VAR dcw, k, s: INTEGER;  (*dcw = word address*)
BEGIN dcw := dc DIV 4; s := T.size; (*convert size for heap allocation*)
  IF s <= 24 THEN s := 32 ELSIF s <= 56 THEN s := 64 ELSIF s <= 120 THEN s := 128
  ELSE s := (s+263) DIV 256 * 256
  END;
  T.len := dc; data[dcw] := s; INC(dcw);  (*len used as address*)
  k := T.nofpar;   (*extension level!*)
  IF k > 3 THEN ORS.Mark("ext level too large")
  ELSE Q(T, dcw);
    WHILE k < 3 DO data[dcw] := -1; INC(dcw); INC(k) END
  END;
  FindPtrFlds(T, 0, dcw); data[dcw] := -1; INC(dcw); tdx := dcw; dc := dcw*4;
  IF tdx >= maxTD THEN ORS.Mark("too many record types"); tdx := 0 END
END BuildTD;

PROCEDURE TypeTest*(VAR x: Item; T: ORB.Type; varpar, isguard: BOOLEAN);
  VAR pc0: INTEGER;
BEGIN (*fetch tag into RH*)
  IF varpar THEN Put2(Ldr, RH, SP, x.a+4+frame, "type test fetch varpar tag")
            ELSE load(x);
                 pc0 := pc; Put3(BC, EQ, 0, "");  (*NIL belongs to every pointer type*)
                 Put2(Ldr, RH, x.r, -8, "")
  END;
  Put2(Ldr, RH, RH, T.nofpar*4, "type test"); incR;
  loadTypTagAdr(T);  (*tag of T*)
  Put0(Cmp, RH-1, RH-1, RH-2, ""); DEC(RH, 2);
  IF ~varpar THEN fix(pc0, pc - pc0 - 1) END;
  IF isguard THEN
    IF check THEN Trap(NE, 2) END
  ELSE SetCC(x, EQ);
    IF ~varpar THEN DEC(RH) END
  END
END TypeTest;

(* Code generation for Boolean operators *)

PROCEDURE Not*(VAR x: Item);   (* x := ~x *)
  VAR t: INTEGER;
BEGIN
  IF x.mode # Cond THEN loadCond(x) END;
  x.r := negated(x.r); t := x.a; x.a := x.b; x.b := t
END Not;

PROCEDURE And1*(VAR x: Item);   (* x := x & *)
BEGIN
  IF x.mode # Cond THEN loadCond(x) END;
  Put3(BC, negated(x.r), x.a, "And1"); x.a := pc-1; FixLink(x.b); x.b := 0
END And1;

PROCEDURE And2*(VAR x, y: Item);
BEGIN
  IF y.mode # Cond THEN loadCond(y) END;
  x.a := merged(y.a, x.a); x.b := y.b; x.r := y.r
END And2;

PROCEDURE Or1*(VAR x: Item);   (* x := x OR *)
BEGIN
  IF x.mode # Cond THEN loadCond(x) END;
  Put3(BC, x.r, x.b, "Or1");  x.b := pc-1; FixLink(x.a); x.a := 0
END Or1;

PROCEDURE Or2*(VAR x, y: Item);
BEGIN
  IF y.mode # Cond THEN loadCond(y) END;
  x.a := y.a; x.b := merged(y.b, x.b); x.r := y.r
END Or2;

(* Code generation for arithmetic operators *)

PROCEDURE Neg*(VAR x: Item);   (* x := -x *)
BEGIN
  IF x.type.form = ORB.Int THEN
    IF x.mode = ORB.Const THEN x.a := -x.a
    ELSE load(x); Put1(Mov, RH, 0, 0, "Neg int"); Put0(Sub, x.r, RH, x.r, "")
    END
  ELSIF x.type.form = ORB.Real THEN
    IF x.mode = ORB.Const THEN x.a := x.a + 7FFFFFFFH + 1
    ELSE load(x); Put1(Mov, RH, 0, 0, "Neg real"); Put0(Fsb, x.r, RH, x.r, "")
    END
  ELSE (*form = Set*)
    IF x.mode = ORB.Const THEN x.a := -x.a-1
    ELSE load(x); Put1(Xor, x.r, x.r, -1, "Neg set")
    END
  END
END Neg;

PROCEDURE AddOp*(op: INTEGER; VAR x, y: Item);   (* x := x +- y *)
BEGIN
  IF op = ORS.plus THEN
    IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN x.a := x.a + y.a
    ELSIF y.mode = ORB.Const THEN
      load(x); IF y.a # 0 THEN Put1a(Add, x.r, x.r, y.a, "AddOp plus const") END
    ELSE load(x); load(y); Put0(Add, RH-2, x.r, y.r, "AddOp plus"); DEC(RH); x.r := RH-1
    END
  ELSE (*op = ORS.minus*)
    IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN x.a := x.a - y.a
    ELSIF y.mode = ORB.Const THEN
      load(x); IF y.a # 0 THEN Put1a(Sub, x.r, x.r, y.a, "AddOP minus const") END
    ELSE load(x); load(y); Put0(Sub, RH-2, x.r, y.r, "AddOp minus"); DEC(RH); x.r := RH-1
    END
  END
END AddOp;

PROCEDURE log2(m: INTEGER; VAR e: INTEGER): INTEGER;
BEGIN e := 0;
  WHILE ~ODD(m) DO m := m DIV 2; INC(e) END;
  RETURN m
END log2;

PROCEDURE MulOp*(VAR x, y: Item);   (* x := x * y *)
  VAR e: INTEGER;
BEGIN
  IF    (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN
    x.a := x.a * y.a
  ELSIF (y.mode = ORB.Const) & (y.a >= 2) & (log2(y.a, e) = 1) THEN
    load(x); Put1(Lsl, x.r, x.r, e, "MulOp by const power of 2")
  ELSIF  y.mode = ORB.Const THEN
    load(x); Put1a(Mul, x.r, x.r, y.a, "MulOp by const")
  ELSIF (x.mode = ORB.Const) & (x.a >= 2) & (log2(x.a, e) = 1) THEN
    load(y); Put1(Lsl, y.r, y.r, e, "MulOp of const power of 2"); x.mode := Reg; x.r := y.r
  ELSIF  x.mode = ORB.Const THEN
    load(y); Put1a(Mul, y.r, y.r, x.a, "MulOp of const"); x.mode := Reg; x.r := y.r
  ELSE
    load(x); load(y); Put0(Mul, RH-2, x.r, y.r, "MulOp"); DEC(RH); x.r := RH-1
  END
END MulOp;

PROCEDURE DivOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
  VAR e: INTEGER;
BEGIN
  IF op = ORS.div THEN
    IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN
      IF y.a > 0 THEN x.a := x.a DIV y.a ELSE ORS.Mark("bad divisor") END
    ELSIF (y.mode = ORB.Const) & (y.a >= 2) & (log2(y.a, e) = 1) THEN
      load(x); Put1(Asr, x.r, x.r, e, "Div by constant power of 2")
    ELSIF y.mode = ORB.Const THEN
      IF y.a > 0 THEN load(x); Put1a(Div, x.r, x.r, y.a, "Divide by constant") ELSE ORS.Mark("bad divisor") END
    ELSE
      load(y);  IF check THEN Trap(LE, 6) END;
      load(x);  Put0(Div, RH-2, x.r, y.r, "Divide"); DEC(RH); x.r := RH-1
    END
  ELSE (*op = ORS.mod*)
    IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN
      IF y.a > 0 THEN x.a := x.a MOD y.a ELSE ORS.Mark("bad modulus") END
    ELSIF (y.mode = ORB.Const) & (y.a >= 2) & (log2(y.a, e) = 1) THEN
      load(x);
      IF e <= 16 THEN Put1(And, x.r, x.r, y.a-1, "Modulus constant power of 2 <= 16")
                 ELSE Put1(Lsl, x.r, x.r, 32-e, "Modulus constant power of 2 > 16");
                      Put1(Ror, x.r, x.r, 32-e, "")
      END
    ELSIF y.mode = ORB.Const THEN
      IF y.a > 0 THEN load(x); Put1a(Div, x.r, x.r, y.a, "Modulus constant"); Put0(Mov+U, x.r, 0, 0, "")
                 ELSE ORS.Mark("bad modulus") END
    ELSE load(y);
      IF check THEN Trap(LE, 6) END;
      load(x); Put0(Div, RH-2, x.r, y.r, "Modulus"); Put0(Mov+U, RH-2, 0, 0, ""); DEC(RH); x.r := RH-1
    END
  END
END DivOp;

(* Code generation for REAL operators *)

PROCEDURE RealOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
BEGIN load(x); load(y);
  IF    op = ORS.plus  THEN Put0(Fad, RH-2, x.r, y.r, "Real plus")
  ELSIF op = ORS.minus THEN Put0(Fsb, RH-2, x.r, y.r, "Real minus")
  ELSIF op = ORS.times THEN Put0(Fml, RH-2, x.r, y.r, "Real multiply")
  ELSIF op = ORS.rdiv  THEN Put0(Fdv, RH-2, x.r, y.r, "Real divide")
  END;
  DEC(RH); x.r := RH-1
END RealOp;

(* Code generation for set operators *)

PROCEDURE Singleton*(VAR x: Item);  (* x := {x} *)
BEGIN
  IF x.mode = ORB.Const THEN x.a := LSL(1, x.a)
  ELSE load(x); Put1(Mov, RH, 0, 1, "x := {x}"); Put0(Lsl, x.r, RH,  x.r, "")
  END
END Singleton;

PROCEDURE Set*(VAR x, y: Item);   (* x := {x .. y} *)
BEGIN
  IF (x.mode = ORB.Const) & ( y.mode = ORB.Const) THEN
    IF x.a <= y.a THEN x.a := LSL(2, y.a) - LSL(1, x.a) ELSE x.a := 0 END
  ELSE
    IF (x.mode = ORB.Const) & (x.a <= 16) THEN x.a := LSL(-1, x.a)
    ELSE load(x); Put1(Mov, RH, 0, -1, "x := {x..y} prepare x"); Put0(Lsl, x.r, RH, x.r, "")
    END;
    IF (y.mode = ORB.Const) & (y.a < 16) THEN Put1(Mov, RH, 0, LSL(-2, y.a), "x := {x..y} prepare y"); y.mode := Reg; y.r := RH; incR
    ELSE load(y); Put1(Mov, RH, 0, -2, "x := {x..y} prepare y"); Put0(Lsl, y.r, RH, y.r, "")
    END;
    IF x.mode = ORB.Const THEN
      IF x.a # 0 THEN Put1(Xor, y.r, y.r, -1, "x := {x..y}"); Put1a(And, RH-1, y.r, x.a, "") END;
      x.mode := Reg; x.r := RH-1
    ELSE
      DEC(RH); Put0(Ann, RH-1, x.r, y.r, "x := {x..y}")
    END
  END
END Set;

PROCEDURE In*(VAR x, y: Item);  (* x := x IN y *)
BEGIN load(y);
  IF x.mode = ORB.Const THEN Put1(Ror, y.r, y.r, (x.a + 1) MOD 20H, "IN const"); DEC(RH)
  ELSE load(x); Put1(Add, x.r, x.r, 1, "IN"); Put0(Ror, y.r, y.r, x.r, ""); DEC(RH, 2)
  END;
  SetCC(x, MI)
END In;

PROCEDURE SetOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
  VAR xset, yset: SET; (*x.type.form = Set*)
BEGIN
  IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN
    xset := SYSTEM.VAL(SET, x.a); yset := SYSTEM.VAL(SET, y.a);
    IF op = ORS.plus THEN xset := xset + yset
    ELSIF op = ORS.minus THEN xset := xset - yset
    ELSIF op = ORS.times THEN xset := xset * yset
    ELSIF op = ORS.rdiv THEN xset := xset / yset
    END;
    x.a := SYSTEM.VAL(INTEGER, xset)
  ELSIF y.mode = ORB.Const THEN
    load(x);
    IF    op = ORS.plus  THEN Put1a(Ior, x.r, x.r, y.a, "Set plus const")
    ELSIF op = ORS.minus THEN Put1a(Ann, x.r, x.r, y.a, "Set minus const")
    ELSIF op = ORS.times THEN Put1a(And, x.r, x.r, y.a, "Set times const")
    ELSIF op = ORS.rdiv  THEN Put1a(Xor, x.r, x.r, y.a, "Set rdiv const")
    END;
  ELSE load(x); load(y);
    IF    op = ORS.plus  THEN Put0(Ior, RH-2, x.r, y.r, "Set plus")
    ELSIF op = ORS.minus THEN Put0(Ann, RH-2, x.r, y.r, "Set minus")
    ELSIF op = ORS.times THEN Put0(And, RH-2, x.r, y.r, "Set times")
    ELSIF op = ORS.rdiv  THEN Put0(Xor, RH-2, x.r, y.r, "Set rdiv")
    END;
    DEC(RH); x.r := RH-1
  END
END SetOp;

(* Code generation for relations *)

PROCEDURE IntRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
BEGIN
  IF (y.mode = ORB.Const) & (y.type.form # ORB.Proc) THEN
    load(x);
    IF (y.a # 0) OR ~(op IN {ORS.eql, ORS.neq}) OR (code[pc-1] DIV 40000000H # -2) THEN
      Put1a(Cmp, x.r, x.r, y.a, "Int relation const")
    END;
    DEC(RH)
  ELSE
    IF (x.mode = Cond) OR (y.mode = Cond) THEN ORS.Mark("not implemented") END;
    load(x); load(y); Put0(Cmp, x.r, x.r, y.r, "Int relation"); DEC(RH, 2)
  END;
  SetCC(x, relmap[op - ORS.eql])
END IntRelation;

PROCEDURE RealRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
BEGIN load(x);
  IF (y.mode = ORB.Const) & (y.a = 0) THEN DEC(RH)
  ELSE load(y); Put0(Fsb, x.r, x.r, y.r, "Real relation"); DEC(RH, 2)
  END;
  SetCC(x, relmap[op - ORS.eql])
END RealRelation;

PROCEDURE StringRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
  (*x, y are char arrays or strings*)
BEGIN
  IF x.type.form = ORB.String THEN loadStringAdr(x) ELSE loadAdr(x) END;
  IF y.type.form = ORB.String THEN loadStringAdr(y) ELSE loadAdr(y) END;
  Put2(Ldr+1, RH,   x.r, 0,    "String relation");
  Put1(Add,   x.r,  x.r, 1,    "");
  Put2(Ldr+1, RH+1, y.r, 0,    "");
  Put1(Add,   y.r,  y.r, 1,    "");
  Put0(Cmp,   RH+2, RH,  RH+1, "");
  Put3(BC,    NE,   2,         "");
  Put1(Cmp,   RH+2, RH,  0,    "");
  Put3(BC,    NE,   -8,        "");
  DEC(RH, 2); SetCC(x, relmap[op - ORS.eql])
END StringRelation;

(* Code generation of Assignments *)

PROCEDURE StrToChar*(VAR x: Item);
BEGIN x.type := ORB.charType; DEC(strx, 4); x.a := ORD(str[x.a])
END StrToChar;

PROCEDURE Store*(VAR x, y: Item); (* x := y *)
  VAR op: INTEGER;
BEGIN  load(y);
  IF x.type.size = 1 THEN op := Str+1 ELSE op := Str END;
  IF x.mode = ORB.Var THEN
    IF x.r > 0 THEN Put2(op, y.r, SP, x.a + frame, "Store local var") (*local*)
                    ELSE GetSB(x.r, "Store global/imported var"); Put2(op, y.r, RH, x.a, "")
    END
  ELSIF x.mode = ORB.Par THEN Put2(Ldr, RH, SP, x.a + frame, "Store parameter"); Put2(op, y.r, RH, x.b, "");
  ELSIF x.mode = RegI THEN Put2(op, y.r, x.r, x.a, "Store RegI"); DEC(RH);
  ELSE ORS.Mark("bad mode in Store")
  END;
  DEC(RH)
END Store;

PROCEDURE StoreStruct*(VAR x, y: Item); (* x := y, frame = 0 *)
  VAR s, pc0: INTEGER;
BEGIN
  IF y.type.size # 0 THEN
    loadAdr(x); loadAdr(y);
    IF (x.type.form = ORB.Array) &  (x.type.len > 0) THEN
      IF y.type.len >= 0 THEN
        IF x.type.size = y.type.size THEN Put1a(Mov, RH, 0, (y.type.size+3) DIV 4, "Store array")
        ELSE ORS.Mark("different length/size, not implemented")
        END
      ELSE (*y  open array*)
        Put2(Ldr, RH, SP, y.a+4, "Store from open array");
        s := y.type.base.size;  (*element size*)
        pc0 := pc; Put3(BC, EQ, 0, "");
        IF s = 1 THEN Put1(Add, RH, RH, 3, ""); Put1(Asr, RH, RH, 2, "")
        ELSIF s # 4 THEN Put1a(Mul, RH, RH, s DIV 4, "")
        END;
        IF check THEN
          Put1a(Mov, RH+1, 0, (x.type.size+3) DIV 4, "");
          Put0(Cmp, RH+1, RH, RH+1, ""); Trap(GT, 3)
        END;
        fix(pc0, pc + 5 - pc0)
      END
    ELSIF x.type.form = ORB.Record THEN Put1a(Mov, RH, 0, x.type.size DIV 4, "Store record")
    ELSE ORS.Mark("inadmissible assignment")
    END;
    Put2(Ldr, RH+1, y.r, 0, "Copy array/record"); Put1(Add, y.r, y.r, 4, "");
    Put2(Str, RH+1, x.r, 0, ""); Put1(Add, x.r, x.r, 4, "");
    Put1(Sub, RH, RH, 1, "");
    Put3(BC, NE, -6, "")
  END;
  RH := 0
END StoreStruct;

PROCEDURE CopyString*(VAR x, y: Item);  (* x := y *)
   VAR len: INTEGER;
 BEGIN loadAdr(x); len := x.type.len;
  IF len >= 0 THEN
    IF len <  y.b THEN ORS.Mark("string too long") END
  ELSIF check THEN
    Put2(Ldr, RH, SP, x.a+4, "Check open array string length before copy");  (*open array len, frame = 0*)
    Put1(Cmp,RH, RH, y.b, ""); Trap(LT, 3)
  END;
  loadStringAdr(y);
  Put2(Ldr, RH,  y.r, 0, "Copy string");
  Put1(Add, y.r, y.r, 4, "");
  Put2(Str, RH,  x.r, 0, "");
  Put1(Add, x.r, x.r, 4, "");
  Put1(Asr, RH,  RH,  24, "");
  Put3(BC,  NE,  -6, "");  RH := 0
 END CopyString;

(* Code generation for parameters *)

PROCEDURE OpenArrayParam*(VAR x: Item);
BEGIN loadAdr(x);
  IF x.type.len >= 0 THEN Put1a(Mov, RH, 0, x.type.len, "Open array parameter")
                     ELSE Put2(Ldr, RH, SP, x.a+4+frame, "Open array parameter") END;
  incR
END OpenArrayParam;

PROCEDURE VarParam*(VAR x: Item; ftype: ORB.Type);
  VAR xmd: INTEGER;
BEGIN xmd := x.mode; loadAdr(x);
  IF (ftype.form = ORB.Array) & (ftype.len < 0) THEN (*open array*)
    IF x.type.len >= 0 THEN Put1a(Mov, RH, 0, x.type.len,  "open array var param")
                       ELSE Put2(Ldr, RH, SP, x.a+4+frame, "open array var param") END;
    incR
  ELSIF ftype.form = ORB.Record THEN
    IF xmd = ORB.Par THEN Put2(Ldr, RH, SP, x.a+4+frame, "record param"); incR
                     ELSE loadTypTagAdr(x.type) END
  END
END VarParam;

PROCEDURE ValueParam*(VAR x: Item);
BEGIN load(x)
END ValueParam;

PROCEDURE StringParam*(VAR x: Item);
BEGIN loadStringAdr(x); Put1(Mov, RH, 0, x.b, "string param len"); incR  (*len*)
END StringParam;

(*For Statements*)

PROCEDURE For0*(VAR x, y: Item);
BEGIN load(y)
END For0;

PROCEDURE For1*(VAR x, y, z, w: Item; VAR L: INTEGER);
BEGIN
  IF z.mode = ORB.Const THEN Put1a(Cmp, RH, y.r, z.a, "FOR const step")
  ELSE load(z); Put0(Cmp, RH-1, y.r, z.r, "FOR var step"); DEC(RH)
  END;
  L := pc;
  IF w.a > 0 THEN Put3(BC, GT, 0, "")
  ELSIF w.a < 0 THEN Put3(BC, LT, 0, "")
  ELSE ORS.Mark("zero increment"); Put3(BC, MI, 0, "")
  END;
  Store(x, y)
END For1;

PROCEDURE For2*(VAR x, y, w: Item);
BEGIN load(x); DEC(RH); Put1a(Add, x.r, x.r, w.a, "FOR advance step")
END For2;

(* Branches, procedure calls, procedure prolog and epilog *)

PROCEDURE Here*(): INTEGER;
BEGIN RETURN pc
END Here;

PROCEDURE FJump*(VAR L: INTEGER);
BEGIN Put3(BC, 7, L, "FJump"); L := pc-1
END FJump;

PROCEDURE CFJump*(VAR x: Item);
BEGIN
  IF x.mode # Cond THEN loadCond(x) END;
  Put3(BC, negated(x.r), x.a, "CFJUMP"); FixLink(x.b); x.a := pc-1
END CFJump;

PROCEDURE BJump*(L: INTEGER);
BEGIN Put3(BC, 7, L-pc-1, "BJump")
END BJump;

PROCEDURE CBJump*(VAR x: Item; L: INTEGER);
BEGIN
  IF x.mode # Cond THEN loadCond(x) END;
  Put3(BC, negated(x.r), L-pc-1, "CBJump"); FixLink(x.b); FixLinkWith(x.a, L)
END CBJump;

PROCEDURE Fixup*(VAR x: Item);
BEGIN FixLink(x.a)
END Fixup;

PROCEDURE SaveRegs(r: INTEGER);  (* R[0 .. r-1]*)
  VAR r0: INTEGER;
BEGIN (*r > 0*) r0 := 0;
  Put1(Sub, SP, SP, r*4, "SaveRegs"); INC(frame, 4*r);
  REPEAT Put2(Str, r0, SP, (r-r0-1)*4, ""); INC(r0) UNTIL r0 = r
END SaveRegs;

PROCEDURE RestoreRegs(r: INTEGER); (*R[0 .. r-1]*)
  VAR r0: INTEGER;
BEGIN (*r > 0*) r0 := r;
  REPEAT DEC(r0); Put2(Ldr, r0, SP, (r-r0-1)*4, "RestoreRegs") UNTIL r0 = 0;
  Put1(Add, SP, SP, r*4, ""); DEC(frame, 4*r)
END RestoreRegs;

PROCEDURE PrepCall*(VAR x: Item; VAR r: INTEGER);
BEGIN (*x.type.form = ORB.Proc*)
  IF x.mode > ORB.Par THEN load(x) END;
  r := RH;
  IF RH > 0 THEN SaveRegs(RH); RH := 0 END
END PrepCall;

PROCEDURE Call*(VAR x: Item; r: INTEGER);
BEGIN (*x.type.form = ORB.Proc*)
  IF x.mode = ORB.Const THEN
    IF x.r >= 0 THEN Put3(BL, 7, (x.a DIV 4)-pc-1, "Call Local/Global procedure")
    ELSE (*imported*)
      IF pc - fixorgP < 1000H THEN
        Put3(BL, 7, ((-x.r) * 100H + x.a) * 1000H + pc-fixorgP, "Call imported procedure");
        fixorgP := pc-1
      ELSE ORS.Mark("fixup impossible")
      END
    END
  ELSE
    IF x.mode <= ORB.Par THEN load(x); DEC(RH)
    ELSE Put2(Ldr, RH, SP, 0, "Prepare var/param procedure for call"); Put1(Add, SP, SP, 4, ""); DEC(r); DEC(frame, 4)
    END;
    IF check THEN Trap(EQ, 5) END;
    Put3(BLR, 7, RH, "Call var/param procedure")
  END;
  IF x.type.base.form = ORB.NoTyp THEN (*procedure*) RH := 0
  ELSE (*function*)
    IF r > 0 THEN Put0(Mov, r, 0, 0, "Move function result to R0"); RestoreRegs(r) END;
    x.mode := Reg; x.r := r; RH := r+1
  END
END Call;

PROCEDURE Enter*(parblksize, locblksize: INTEGER; int: BOOLEAN);
  VAR a, r: INTEGER;
BEGIN frame := 0;
  IF ~int THEN (*procedure prolog*)
    IF locblksize >= 20000H THEN ORS.Mark("too many locals") END;
    a := 4; r := 0;
    Put1(Sub, SP, SP, locblksize, "procedure prolog");
    Put2(Str, LNK, SP, 0, "");
    WHILE a < parblksize DO Put2(Str, r, SP, a, ""); INC(r); INC(a, 4) END
  ELSE (*interrupt procedure*)
    Put1(Sub, SP, SP, locblksize, "Interrupt procedure prolog");
    Put2(Str, 0, SP, 0, "");
    Put2(Str, 1, SP, 4, "");
    Put2(Str, 2, SP, 8, "")
    (*R0, R1, R2 saved on stack*)
  END
END Enter;

PROCEDURE Return*(form: INTEGER; VAR x: Item; size: INTEGER; int: BOOLEAN);
BEGIN
  IF form # ORB.NoTyp THEN load(x) END;
  IF ~int THEN (*procedure epilog*)
    Put2(Ldr, LNK, SP, 0, "porcedure epilog"); Put1(Add, SP, SP, size, ""); Put3(BR, 7, LNK, "")
  ELSE (*interrupt return, restore R2, R1, R0*)
    Put2(Ldr, 2, SP, 8, "Interrup procedure epilog"); Put2(Ldr, 1, SP, 4, "");
    Put2(Ldr, 0, SP, 0, ""); Put1(Add, SP, SP, size, "");
    Put3(BR, 7, 10H, "") (*RTI*)
  END;
  RH := 0
END Return;

(* In-line code procedures*)

PROCEDURE Increment*(upordown: INTEGER; VAR x, y: Item);
  VAR op, zr, v: INTEGER;
BEGIN (*frame = 0*)
  IF upordown = 0 THEN op := Add ELSE op := Sub END;
  IF x.type = ORB.byteType THEN v := 1 ELSE v := 0 END;
  IF y.type.form = ORB.NoTyp THEN y.mode := ORB.Const; y.a := 1 END;
  IF (x.mode = ORB.Var) & (x.r > 0) THEN
    zr := RH;
    Put2(Ldr+v, zr, SP, x.a, "Inc/Dec local var"); incR;
    IF y.mode = ORB.Const THEN Put1a(op, zr, zr, y.a, "")
                          ELSE load(y); Put0(op, zr, zr, y.r, ""); DEC(RH) END;
    Put2(Str+v, zr, SP, x.a, ""); DEC(RH)
  ELSE loadAdr(x); zr := RH;
    Put2(Ldr+v, RH, x.r, 0, "Inc/Dec global/imported var"); incR;
    IF y.mode = ORB.Const THEN Put1a(op, zr, zr, y.a, "")
                          ELSE load(y); Put0(op, zr, zr, y.r, ""); DEC(RH) END;
    Put2(Str+v, zr, x.r, 0, ""); DEC(RH, 2)
  END
END Increment;

PROCEDURE Include*(inorex: INTEGER; VAR x, y: Item);
  VAR op, zr: INTEGER;
BEGIN loadAdr(x); zr := RH; Put2(Ldr, RH, x.r, 0, "Include"); incR;
  IF inorex = 0 THEN op := Ior ELSE op := Ann END;
  IF y.mode = ORB.Const THEN Put1a(op, zr, zr, LSL(1, y.a), "")
  ELSE load(y); Put1(Mov, RH, 0, 1, ""); Put0(Lsl, y.r, RH, y.r, ""); Put0(op, zr, zr, y.r, ""); DEC(RH)
  END;
  Put2(Str, zr, x.r, 0, ""); DEC(RH, 2)
END Include;

PROCEDURE Assert*(VAR x: Item);
  VAR cond: INTEGER;
BEGIN
  IF x.mode # Cond THEN loadCond(x) END;
  IF x.a = 0 THEN cond := negated(x.r)
  ELSE Put3(BC, x.r, x.b, "Assert"); FixLink(x.a); x.b := pc-1; cond := 7
  END;
  Trap(cond, 7); FixLink(x.b)
END Assert;

PROCEDURE New*(VAR x: Item);
BEGIN loadAdr(x); loadTypTagAdr(x.type.base); Trap(7, 0); RH := 0
END New;

PROCEDURE Pack*(VAR x, y: Item);
  VAR z: Item;
BEGIN z := x; load(x); load(y);
  Put1(Lsl, y.r, y.r, 23, "Pack"); Put0(Add, x.r, x.r, y.r, ""); DEC(RH); Store(z, x)
END Pack;

PROCEDURE Unpk*(VAR x, y: Item);
  VAR z, e0: Item;
BEGIN  z := x; load(x); e0.mode := Reg; e0.r := RH; e0.type := ORB.intType;
  Put1(Asr, RH, x.r, 23, "Unpack"); Put1(Sub, RH, RH, 127, ""); Store(y, e0); incR;
  Put1(Lsl, RH, RH, 23, ""); Put0(Sub, x.r, x.r, RH, ""); Store(z, x)
END Unpk;

PROCEDURE Led*(VAR x: Item);
BEGIN load(x); Put1(Mov, RH, 0, -60, "LED"); Put2(Str, x.r, RH, 0, ""); DEC(RH)
END Led;

PROCEDURE Get*(VAR x, y: Item);
BEGIN load(x); x.type := y.type; x.mode := RegI; x.a := 0; Store(y, x)
END Get;

PROCEDURE Put*(VAR x, y: Item);
BEGIN load(x); x.type := y.type; x.mode := RegI; x.a := 0; Store(x, y)
END Put;

PROCEDURE Copy*(VAR x, y, z: Item);
BEGIN load(x); load(y);
  IF z.mode = ORB.Const THEN
    IF z.a > 0 THEN load(z) ELSE ORS.Mark("bad count") END
  ELSE load(z);
    IF check THEN Trap(LT, 3) END;
    Put3(BC, EQ, 6, "Prepare copy")
  END;
  Put2(Ldr, RH, x.r, 0, "copy");
  Put1(Add, x.r, x.r, 4, "");
  Put2(Str, RH, y.r, 0, "");
  Put1(Add, y.r, y.r, 4, "");
  Put1(Sub, z.r, z.r, 1, "");
  Put3(BC, NE, -6, ""); DEC(RH, 3)
END Copy;

PROCEDURE LDPSR*(VAR x: Item);
BEGIN (*x.mode = Const*)  Put3(0, 15, x.a + 20H, "LDPSR")
END LDPSR;

PROCEDURE LDREG*(VAR x, y: Item);
BEGIN
  IF y.mode = ORB.Const THEN Put1a(Mov, x.a, 0, y.a, "LDREG")
  ELSE load(y); Put0(Mov, x.a, 0, y.r, "LDREG"); DEC(RH)
  END
END LDREG;

(*In-line code functions*)

PROCEDURE Abs*(VAR x: Item);
BEGIN
  IF x.mode = ORB.Const THEN x.a := ABS(x.a)
  ELSE load(x);
    IF x.type.form = ORB.Real THEN Put1(Lsl, x.r, x.r, 1, "Abs real"); Put1(Ror, x.r, x.r, 1, "")
    ELSE Put1(Cmp, x.r, x.r, 0, "Abs int"); Put3(BC, GE, 2, ""); Put1(Mov, RH, 0, 0, ""); Put0(Sub, x.r, RH, x.r, "")
    END
  END
END Abs;

PROCEDURE Odd*(VAR x: Item);
BEGIN load(x); Put1(And, x.r, x.r, 1, "Odd"); SetCC(x, NE); DEC(RH)
END Odd;

PROCEDURE Floor*(VAR x: Item);
BEGIN load(x); Put1(Mov+U, RH, 0, 4B00H, "Floor"); Put0(Fad+V, x.r, x.r, RH, "")
END Floor;

PROCEDURE Float*(VAR x: Item);
BEGIN load(x); Put1(Mov+U, RH, 0, 4B00H, "Float");  Put0(Fad+U, x.r, x.r, RH, "")
END Float;

PROCEDURE Ord*(VAR x: Item);
BEGIN
  IF x.mode IN {ORB.Var, ORB.Par, RegI, Cond} THEN load(x) END
END Ord;

PROCEDURE Len*(VAR x: Item);
BEGIN
  IF x.type.len >= 0 THEN
    IF x.mode = RegI THEN DEC(RH) END;
    x.mode := ORB.Const; x.a := x.type.len
  ELSE (*open array*) Put2(Ldr, RH, SP, x.a + 4 + frame, "Open array LEN"); x.mode := Reg; x.r := RH; incR
  END
END Len;

PROCEDURE Shift*(fct: INTEGER; VAR x, y: Item);
  VAR op: INTEGER;
BEGIN load(x);
  IF fct = 0 THEN op := Lsl ELSIF fct = 1 THEN op := Asr ELSE op := Ror END;
  IF y.mode = ORB.Const THEN Put1(op, x.r, x.r, y.a MOD 20H, "Shift const")
  ELSE load(y); Put0(op, RH-2, x.r, y.r, "Shift by var"); DEC(RH); x.r := RH-1
  END
END Shift;

PROCEDURE ADC*(VAR x, y: Item);
BEGIN load(x); load(y); Put0(Add+2000H, x.r, x.r, y.r, "ADC"); DEC(RH)
END ADC;

PROCEDURE SBC*(VAR x, y: Item);
BEGIN load(x); load(y); Put0(Sub+2000H, x.r, x.r, y.r, "SBC"); DEC(RH)
END SBC;

PROCEDURE UML*(VAR x, y: Item);
BEGIN load(x); load(y); Put0(Mul+2000H, x.r, x.r, y.r, "UML"); DEC(RH)
END UML;

PROCEDURE Bit*(VAR x, y: Item);
BEGIN load(x); Put2(Ldr, x.r, x.r, 0, "Bit ..");
  IF y.mode = ORB.Const THEN Put1(Ror, x.r, x.r, y.a+1, "BIT const"); DEC(RH)
  ELSE load(y); Put1(Add, y.r, y.r, 1, "Bit var"); Put0(Ror, x.r, x.r, y.r, ""); DEC(RH, 2)
  END;
  SetCC(x, MI)
END Bit;

PROCEDURE Register*(VAR x: Item);
BEGIN (*x.mode = Const*)
  Put0(Mov, RH, 0, x.a MOD 10H, "Register"); x.mode := Reg; x.r := RH; incR
END Register;

PROCEDURE H*(VAR x: Item);
BEGIN (*x.mode = Const*)
  Put0(Mov + U + x.a MOD 2 * V, RH, 0, 0, "H"); x.mode := Reg; x.r := RH; incR
END H;

PROCEDURE Adr*(VAR x: Item);
BEGIN
  IF     x.mode IN {ORB.Var, ORB.Par, RegI}               THEN loadAdr(x)
  ELSIF (x.mode = ORB.Const) & (x.type.form = ORB.Proc)   THEN load(x)
  ELSIF (x.mode = ORB.Const) & (x.type.form = ORB.String) THEN loadStringAdr(x)
  ELSE ORS.Mark("not addressable")
  END
END Adr;

PROCEDURE Condition*(VAR x: Item);
BEGIN (*x.mode = Const*) SetCC(x, x.a)
END Condition;

PROCEDURE Open*(v: INTEGER);
BEGIN pc := 0; tdx := 0; strx := 0; RH := 0; fixorgP := 0; fixorgD := 0; fixorgT := 0; check := v # 0; version := v;
  IF v = 0 THEN pc := 1;
    REPEAT code[pc] := 0; INC(pc) UNTIL pc = 8
  END
END Open;

PROCEDURE SetDataSize*(dc: INTEGER);
BEGIN varsize := dc
END SetDataSize;

PROCEDURE Header*;
BEGIN entry := pc*4;
  IF version = 0 THEN code[0] := 0E7000000H-1 + pc;  Put1a(Mov, SP, 0, StkOrg0, "RISC-0 header")  (*RISC-0*)
  ELSE Put1(Sub, SP, SP, 4, "RISC-5 header"); Put2(Str, LNK, SP, 0, "")
  END
END Header;

PROCEDURE NofPtrs(typ: ORB.Type): INTEGER;
  VAR fld: ORB.Object; n: INTEGER;
BEGIN
  IF (typ.form = ORB.Pointer) OR (typ.form = ORB.NilTyp) THEN n := 1
  ELSIF typ.form = ORB.Record THEN
    fld := typ.dsc; n := 0;
    WHILE fld # NIL DO n := NofPtrs(fld.type) + n; fld := fld.next END
  ELSIF typ.form = ORB.Array THEN n := NofPtrs(typ.base) * typ.len
  ELSE n := 0
  END;
  RETURN n
END NofPtrs;

PROCEDURE FindPtrs(VAR R: Files.Rider; typ: ORB.Type; adr: INTEGER);
  VAR fld: ORB.Object; i, s: INTEGER;
BEGIN
  IF (typ.form = ORB.Pointer) OR (typ.form = ORB.NilTyp) THEN Files.WriteInt(R, adr)
  ELSIF typ.form = ORB.Record THEN
    fld := typ.dsc;
    WHILE fld # NIL DO FindPtrs(R, fld.type, fld.val + adr); fld := fld.next END
  ELSIF typ.form = ORB.Array THEN
    s := typ.base.size;
    FOR i := 0 TO typ.len-1 DO FindPtrs(R, typ.base, i*s + adr) END
  END
END FindPtrs;

PROCEDURE Close*(VAR modid: ORS.Ident; key, nofent: INTEGER);
  VAR obj: ORB.Object;
    i, comsize, nofimps, nofptrs, size: INTEGER;
    name: ORS.Ident;
    F: Files.File; R: Files.Rider;
BEGIN  (*exit code*)
  IF version = 0 THEN Put1(Mov, 0, 0, 0, "RISC-0 exit code"); Put3(BR, 7, 0, "")  (*RISC-0*)
  ELSE Put2(Ldr, LNK, SP, 0, "RISC-5 exit code"); Put1(Add, SP, SP, 4, ""); Put3(BR, 7, LNK, "")
  END;
  obj := ORB.topScope.next; nofimps := 0; comsize := 4; nofptrs := 0;
  WHILE obj # NIL DO
    IF (obj.class = ORB.Mod) & (obj.dsc # ORB.system) THEN INC(nofimps) (*count imports*)
    ELSIF (obj.exno # 0) & (obj.class = ORB.Const) & (obj.type.form = ORB.Proc)
        & (obj.type.nofpar = 0) & (obj.type.base = ORB.noType) THEN i := 0; (*count commands*)
      WHILE obj.name[i] # 0X DO INC(i) END;
      i := (i+4) DIV 4 * 4; INC(comsize, i+4)
    ELSIF obj.class = ORB.Var THEN INC(nofptrs, NofPtrs(obj.type))  (*count pointers*)
    END;
    obj := obj.next
  END;
  size := varsize + strx + comsize + (pc + nofimps + nofent + nofptrs + 1)*4;  (*varsize includes type descriptors*)

  ORB.MakeFileName(name, modid, ".rsc"); (*write code file*)
  F := Files.New(name); Files.Set(R, F, 0); Files.WriteString(R, modid); Files.WriteInt(R, key); Files.Write(R, CHR(version));
  Files.WriteInt(R, size);
  obj := ORB.topScope.next;
  WHILE (obj # NIL) & (obj.class = ORB.Mod) DO  (*imports*)
    IF obj.dsc # ORB.system THEN Files.WriteString(R, obj(ORB.Module).orgname); Files.WriteInt(R, obj.val) END;
    obj := obj.next
  END;
  Files.Write(R, 0X);
  Files.WriteInt(R, tdx*4);
  i := 0;
  WHILE i < tdx DO Files.WriteInt(R, data[i]); INC(i) END; (*type descriptors*)
  Files.WriteInt(R, varsize - tdx*4);  (*data*)
  Files.WriteInt(R, strx);
  FOR i := 0 TO strx-1 DO Files.Write(R, str[i]) END;  (*strings*)
  Files.WriteInt(R, pc);  (*code len*)
  FOR i := 0 TO pc-1 DO Files.WriteInt(R, code[i]) END;  (*program*)
  obj := ORB.topScope.next;
  WHILE obj # NIL DO  (*commands*)
    IF (obj.exno # 0) & (obj.class = ORB.Const) & (obj.type.form = ORB.Proc) &
        (obj.type.nofpar = 0) & (obj.type.base = ORB.noType) THEN
      Files.WriteString(R, obj.name); Files.WriteInt(R, obj.val)
    END;
    obj := obj.next
  END;
  Files.Write(R, 0X);
  Files.WriteInt(R, nofent); Files.WriteInt(R, entry);
  obj := ORB.topScope.next;
  WHILE obj # NIL DO  (*entries*)
    IF obj.exno # 0 THEN
      IF (obj.class = ORB.Const) & (obj.type.form = ORB.Proc) OR (obj.class = ORB.Var) THEN
        Files.WriteInt(R, obj.val);
      ELSIF obj.class = ORB.Typ THEN
        IF obj.type.form = ORB.Record THEN Files.WriteInt(R,  obj.type.len MOD 10000H)
        ELSIF (obj.type.form = ORB.Pointer) & ((obj.type.base.typobj = NIL) OR (obj.type.base.typobj.exno = 0)) THEN
          Files.WriteInt(R,  obj.type.base.len MOD 10000H)
        END
      END
    END;
    obj := obj.next
  END;
  obj := ORB.topScope.next;
  WHILE obj # NIL DO  (*pointer variables*)
    IF obj.class = ORB.Var THEN FindPtrs(R, obj.type, obj.val) END;
    obj := obj.next
  END;
  Files.WriteInt(R, -1);
  Files.WriteInt(R, fixorgP); Files.WriteInt(R, fixorgD); Files.WriteInt(R, fixorgT); Files.WriteInt(R, entry);
  Files.Write(R, "O"); Files.Register(F)
END Close;

BEGIN relmap[0] := 1; relmap[1] := 9; relmap[2] := 5; relmap[3] := 6; relmap[4] := 14; relmap[5] := 13;
END ORG.
