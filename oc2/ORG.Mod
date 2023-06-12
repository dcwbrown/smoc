MODULE ORG; (* N.Wirth, 16.4.2016 / 4.4.2017 / 31.5.2019  Oberon compiler; code generator for RISC / AP 13.7.22 Extended Oberon*)

IMPORT SYSTEM, Files, ORS, ORB, w := Writer, K := Kernel;

(* Code generator for Oberon compiler for RISC processor.
   Procedural interface to Parser ORP; result in array "code".
   Procedure Close writes code-files *)

CONST
  WordSize* = 4;

  maxCode = 12000;
  maxStrx = 8192;
  maxTD   = 160;
  maxSet  = WordSize*8;
  ZeroD = 4B00H;  (* denormalized zero used for FLOOR/FLT conversions *)

  Reg = 10; RegI = 11; Cond = 12;  (* internal item modes *)
  TR = 13;  SP = 14;  LNK = 15;  (* dedicated registers *)

  BCT = 0E7000000H; (* instructions *)
  RTI = 10H;
  STI = 20H;
  F2 = -2;

  C4  = 10H;       C6  = 40H;     C8  = 100H;     C10 = 400H;
  C12 = 1000H;     C14 = 4000H;   C16 = 10000H;   C18 = 40000H;
  C20 = 100000H;   C22 = 400000H; C24 = 1000000H; C26 = 4000000H;
  C28 = 10000000H; C30 = 40000000H;

  (* frequently used opcodes *)
  U = 2000H; V = 1000H;
  Mov = 0;  Lsl = 1;  Asr = 2;  Ror= 3;   And = 4; Ann = 5; Ior = 6; Xor = 7;
  Add = 8;  Sub = 9;  Cmp = 9;  Mul = 10; Div = 11;
  Fad = 12; Fsb = 13; Fml = 14; Fdv = 15;
  Ldr = 8;  Str = 10;
  BR = 0;   BLR = 1;  BC = 2;   BL = 3;
  MI = 0;   PL = 8;   EQ = 1;   NE = 9; CS = 2; CC = 10; LT = 5; GE = 13; LE = 6; GT = 14;

  StkOrg0 = -64;  VarOrg0 = 0;  (* for RISC-0 only *)

TYPE
  Item* = RECORD
    mode*:     INTEGER;
    type*:     ORB.Type;
    obj*:      ORB.Object;
    a*, b*, r: INTEGER;
    rdo*:      BOOLEAN;  (* Read only    *)
    deref*:    BOOLEAN;  (* Dereferenced *)
    super:     BOOLEAN   (* Super call   *)
  END;

  LabelRange* = RECORD low*, high*, label*: INTEGER END;

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
  pc*:              INTEGER;  (* program counter                              *)
  varx, strx, tdw:  INTEGER;  (* varx and strx in bytes, tdw in words         *)
  entry, final:     INTEGER;  (* main entry point, finalization sequence      *)
  RH:               INTEGER;  (* available registers R[0] ... R[H-1]          *)
  frame:            INTEGER;  (* frame offset changed in SaveRegs/RestoreRegs *)
  fixorgP, fixorgD: INTEGER;  (* origins of lists of locations                *)
  fixorgT, fixorgM: INTEGER;  (* - to be fixed up by loader                   *)
  check:            BOOLEAN;  (* emit run-time checks                         *)
  version:          INTEGER;  (* 0 = RISC-0, 1 = RISC-5                       *)

  relmap: ARRAY 6       OF INTEGER;  (* condition codes for relations *)
  code:   ARRAY maxCode OF INTEGER;
  str:    ARRAY maxStrx OF CHAR;     (* strings *)
  td:     ARRAY maxTD   OF INTEGER;  (* type descriptors *)

(* -------------------------- instruction listing --------------------------- *)

PROCEDURE ins(src: ARRAY OF CHAR;  VAR x: INTEGER;  VAR dst: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN i := 0;
  WHILE src[i] # 0X DO dst[x] := src[i];  INC(i); INC(x) END
END ins;

PROCEDURE dop(op, u: INTEGER; VAR x: INTEGER; VAR dis: ARRAY OF CHAR);
BEGIN
  CASE op MOD 16 OF
    | 0: ins("mov", x, dis);  IF u # 0  THEN ins("u", x, dis) END;
    | 1: ins("lsl", x, dis)
    | 2: ins("asr", x, dis)
    | 3: ins("ror", x, dis)
    | 4: ins("and", x, dis)
    | 5: ins("ann", x, dis)
    | 6: ins("ior", x, dis)
    | 7: ins("xor", x, dis)
    | 8: ins("add", x, dis);  IF u # 0  THEN ins("c", x, dis) END;
    | 9: ins("sub", x, dis);  IF u # 0  THEN ins("c", x, dis) END;
    |10: ins("mul", x, dis);  IF u # 0  THEN ins("u", x, dis) END;
    |11: ins("div", x, dis)
    |12: ins("fad", x, dis)
    |13: ins("fsb", x, dis)
    |14: ins("fml", x, dis)
    |15: ins("fdv", x, dis)
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
  CASE cond OF
  | 0: ins("mi",x,dis)    |  8: ins("pl",x,dis)
  | 1: ins("eq",x,dis)    |  9: ins("ne",x,dis)
  | 2: ins("cs",x,dis)    | 10: ins("cc",x,dis)
  | 3: ins("vs",x,dis)    | 11: ins("vc",x,dis)
  | 4: ins("ls",x,dis)    | 12: ins("hi",x,dis)
  | 5: ins("lt",x,dis)    | 13: ins("ge",x,dis)
  | 6: ins("le",x,dis)    | 14: ins("gt",x,dis)
  | 7: ins("br",x,dis)    | 15: ins("bn",x,dis)
  END
END dcond;

PROCEDURE dism(pc: INTEGER; comment: ARRAY OF CHAR);
VAR dis: ARRAY 41 OF CHAR;  in, u, v, a, b, op, im, moff, boff, c, x: INTEGER;
BEGIN
  dis := "                                        ";
  in  := code[pc] MOD 100000000H;
  IF    comment = "FIX"   THEN w.s("FIX     ")
  ELSIF comment = "MERGE" THEN w.s("MERGE   ")
  ELSE
    w.in(ORS.linenum, -3);  w.s(":");  w.in(ORS.Pos() - ORS.linebeg, 4)
  END;
  w.hn(pc, -6); w.s(": ");  w.hn(in, -8);  w.s("  ");

  u    := ASR(in, 29) MOD 2;
  v    := ASR(in, 28) MOD 2;
  a    := ASR(in, 24) MOD 10H;
  b    := ASR(in, 20) MOD 10H;
  op   := ASR(in, 16) MOD 10H;
  moff := ASR(LSL(in,44),44);  (* memory offset 20 bits sign extended *)
  boff := ASR(LSL(in,40),40);  (* branch offset 24 bits sign extended *)
  im   := in MOD 10000H;
  c    := in MOD 10H;

  x := 0;
  CASE ASR(in, 30) MOD 4 OF

  |0: (* Register operation *)
      dop(op,u,x,dis);  x := 5;  dreg(a,x,dis);  x := 9;
      IF op # 0 THEN dreg(b,x,dis); ins(",",x,dis) END;
      dreg(c,x,dis)

  |1: (* Immediate operation *)
      dop(op,u,x,dis);  x := 5;  dreg(a,x,dis);  x := 9;
      IF op # 0 THEN dreg(b,x,dis); ins(",",x,dis) END;
      IF v = 0 THEN ini(im,x,dis) ELSE ini(im+0FFFF0000H,x,dis) END

  |2: (* Memory operation *)
      IF u = 0 THEN ins("ld",x,dis) ELSE ins("st",x,dis) END;  x := 5;
      dreg(a,x,dis);  IF v # 0 THEN ins("b",x,dis) END;
      x := 9;  dreg(b,x,dis);
      IF moff # 0 THEN ins("+",x,dis);  ini(moff,x,dis) END;

  |3: (* Branch operation *)
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
END dism;


(* -------------- instruction assemblers according to formats --------------- *)

PROCEDURE incR;
BEGIN
  IF RH < TR-1 THEN INC(RH) ELSE ORS.Mark("register stack overflow") END
END incR;

PROCEDURE Put0(op, a, b, c: INTEGER; comment: ARRAY OF CHAR);
BEGIN (* emit format-0 register instruction *)
  code[pc] := ((a*C4 + b) * C4 + op) * C16 + c;
  dism(pc, comment);  INC(pc)
END Put0;

PROCEDURE Put1(op, a, b, im: INTEGER; comment: ARRAY OF CHAR);
BEGIN (* emit format-1 register instruction, -C16 <= im < C16 *)
  IF im < 0 THEN INC(op, V) END;
  code[pc] := (((a+C6) * C4 + b) * C4 + op) * C16 + im MOD C16;
  dism(pc, comment);  INC(pc)
END Put1;

PROCEDURE Put1a(op, a, b, im: INTEGER; comment: ARRAY OF CHAR);
VAR r: INTEGER;
BEGIN (* same as Put1, but with range test -C16 <= im < C16 *)
  IF (im >= -C16) & (im < C16) THEN Put1(op, a, b, im, comment)
  ELSIF op = Mov THEN
    Put1(Mov+U, a, 0, im DIV C16, comment);
    IF im MOD C16 # 0 THEN Put1(Ior, a, a, im MOD C16, "") END
  ELSE r := RH;
    IF b = RH THEN incR END;
    Put1(Mov+U, RH, 0, im DIV C16, comment);
    IF im MOD C16 # 0 THEN Put1(Ior, RH, RH, im MOD C16, "") END;
    Put0(op, a, b, RH, "");
    IF RH > r THEN DEC(RH) END
  END
END Put1a;

PROCEDURE Put1b(r, mno, off, disp: INTEGER; comment: ARRAY OF CHAR);
(* - emit modified register instruction to be fixed up by loader, - *)
(* 0 <= mno < 64, 0 <= off < 256                                *)
BEGIN
  code[pc] := (((r+16) * C6 + mno) * C8 + off) * C12 + disp MOD C12;
  dism(pc, comment);  INC(pc)
END Put1b;

PROCEDURE Put2(op, a, b, off: INTEGER; comment: ARRAY OF CHAR);
BEGIN (* emit load/store instruction *)
  code[pc] := ((op * C4 + a) * C4 + b) * C20 + off MOD C20;
  dism(pc, comment);  INC(pc)
END Put2;

PROCEDURE Put3(op, cond, off: INTEGER; comment: ARRAY OF CHAR);
BEGIN (* emit branch instruction *)
  code[pc] := ((op+12) * C4 + cond) * C24 + off MOD C24;
  dism(pc, comment);  INC(pc)
END Put3;

PROCEDURE Put3a(op, mno, pno, disp: INTEGER; comment: ARRAY OF CHAR);
(* - emit modified branch instruction to be fixed up by loader, 0 <= mno < 64 - *)
BEGIN
  code[pc] := (((op+12) * C6 + mno) * C8 + pno) * C14 + disp MOD C14;
  dism(pc, comment);  INC(pc)
END Put3a;

PROCEDURE PutPair(base, op, a, b, off, format: INTEGER; comment: ARRAY OF CHAR);
VAR high: INTEGER;
BEGIN (* emit instruction pair to be fixed up by loader, 0 <= off < C24 *)
  IF version = 0 THEN Put1(Mov, RH, 0, VarOrg0, comment)
  ELSIF pc - fixorgD < C12 THEN
    IF base = 0 THEN high := off DIV C16 MOD C8; off := off MOD C16
                ELSE high := 0 END;
    Put1b(RH, -base, high, pc-fixorgD, comment); fixorgD := pc-1
  ELSE ORS.Mark("fixup impossible")
  END;
  IF format = 1 THEN Put1(op, a, b, off, comment) ELSE Put2(op, a, b, off, comment) END
END PutPair;

PROCEDURE CheckRegs*;
BEGIN
  IF RH # 0 THEN ORS.Mark("Reg Stack"); RH := 0 END;
  IF pc >= maxCode - 40 THEN ORS.Mark("program too long") END;
  IF frame # 0 THEN ORS.Mark("frame error"); frame := 0 END
END CheckRegs;

PROCEDURE SetCC(VAR x: Item; n: INTEGER);
BEGIN x.mode := Cond; x.a := 0; x.b := 0; x.r := n
END SetCC;

(* -------------------------------------------------------------------------- *)

PROCEDURE Trap(cond, num: INTEGER);
BEGIN Put3(BLR, cond, ORS.Pos()*C8 + num*C4 + TR, "Trap");
END Trap;

PROCEDURE NilCheck;
BEGIN IF check THEN Trap(EQ, 4) END
END NilCheck;

(* ----- handling of forward reference, fixups of instruction operands ------ *)

PROCEDURE negated(cond: INTEGER): INTEGER;
BEGIN
  IF cond < 8 THEN cond := cond+8 ELSE cond := cond-8 END;
  RETURN cond
END negated;

PROCEDURE fix1(at, with: INTEGER);
VAR v: INTEGER;
BEGIN (* fix format-1 register instruction *)
  IF with < 0 THEN v := C28 (* set v bit *) ELSE v := 0 END;
  code[at] := code[at] DIV C16 * C16 + with MOD C16 + v;
  dism(at, "FIX");
END fix1;

PROCEDURE fix3(at, with: INTEGER);
BEGIN (* fix branch instruction *)
  code[at] := code[at] DIV C24 * C24 + with MOD C24;
  dism(at, "FIX");
END fix3;

PROCEDURE FixOne*(at: INTEGER);
BEGIN fix3(at, pc-at-1)
END FixOne;

PROCEDURE FixLinkWith(L, dst: INTEGER);
VAR L1: INTEGER;
BEGIN (* fix chain of branch instructions *)
  WHILE L # 0 DO L1 := code[L] MOD C24; fix3(L, dst-L-1); L := L1 END
END FixLinkWith;

PROCEDURE FixLink*(L: INTEGER);
BEGIN FixLinkWith(L, pc)
END FixLink;

PROCEDURE FixLinkPair(L, adr: INTEGER);
(* - fix chain of instruction pairs with an address that is spread across - *)
(* both instructions, 0 <= adr < C24                                    *)
VAR L1: INTEGER; p, q: INTEGER;
BEGIN
  WHILE L # 0 DO p := code[L-1]; q := code[L];
    L1 := p DIV C12 MOD C8 + q MOD C16;
    code[L-1] := p DIV C20 * C20 + adr DIV C16 MOD C8 * C12 + p MOD C12;
    code[L] := q DIV C16 * C16 + adr MOD C16;
    dism(L-1, "FIX");  dism(L, "FIX");
    L := L1
  END
END FixLinkPair;

PROCEDURE merged(L0, L1: INTEGER): INTEGER;
VAR L2, L3: INTEGER;
BEGIN (* merge chains of the two operands of AND and OR *)
  IF L0 # 0 THEN L3 := L0;
    REPEAT L2 := L3; L3 := code[L2] MOD C16 UNTIL L3 = 0;
    code[L2] := code[L2] + L1; L1 := L0;
    dism(L2, "MERGE");
  END;
  RETURN L1
END merged;

(* ------------ loading of operands and addresses into registers ------------ *)

PROCEDURE load(VAR x: Item);
VAR op: INTEGER;
BEGIN
  IF x.mode # Reg THEN
    IF x.type.size = 1 THEN op := Ldr+1 ELSE op := Ldr END;
    IF x.mode = ORB.Const THEN
      IF x.type.form = ORB.Proc THEN
        IF    x.r > 0 THEN ORS.Mark("not allowed")                (* local  *)
        ELSIF x.r = 0 THEN Put3(BL, 7, 0, "load global proc");    (* global *)
                           Put1a(Sub, RH, LNK, pc*4 - x.a, "")
                      ELSE (* mark as progbase-relative *)        (* imported*)
                           PutPair(x.r, Add, RH, RH, x.a + C8, 1, "load imported proc")
        END
      ELSE Put1a(Mov, RH, 0, x.a, "load const")
      END;
      x.r := RH; incR
    ELSIF x.mode = ORB.Var THEN
      IF x.r > 0 THEN (* local *)
        Put2(op, RH, SP, x.a + frame, "load local var")
      ELSE
        PutPair(x.r, op, RH, RH, x.a, 2, "load imported var")
      END;
      x.r := RH; incR
    ELSIF x.mode = ORB.Par THEN Put2(Ldr, RH, SP, x.a + frame, "load parameter");
                                Put2(op, RH, RH, x.b, ""); x.r := RH; incR
    ELSIF x.mode = RegI    THEN Put2(op, x.r, x.r, x.a, "load reg indirect")
    ELSIF x.mode = Cond THEN
      Put3(BC, negated(x.r), 2, "load condition");
      FixLink(x.b); Put1(Mov, RH, 0, 1, ""); Put3(BC, 7, 1, "");
      FixLink(x.a); Put1(Mov, RH, 0, 0, ""); x.r := RH; incR
    END;
    x.mode := Reg
  END
END load;

PROCEDURE loadAdr(VAR x: Item);
BEGIN
  IF x.mode = ORB.Var THEN
    IF x.r > 0 THEN Put1a(Add, RH, SP, x.a + frame, "loadadr local var")  (* local *)
               ELSE PutPair(x.r, Add, RH, RH, x.a, 1, "loadadr local var") END;
    x.r := RH; incR
  ELSIF x.mode = ORB.Par THEN Put2(Ldr, RH, SP, x.a + frame, "loadadr parameter");
    IF x.b # 0 THEN Put1a(Add, RH, RH, x.b, "") END;
    x.r := RH; incR
  ELSIF x.mode = RegI THEN
    IF x.a # 0 THEN Put1a(Add, x.r, x.r, x.a, "loadadr reg indirect") END
  ELSE ORS.Mark("address error")
  END;
  x.mode := Reg
END loadAdr;

PROCEDURE loadCond(VAR x: Item);
BEGIN
  IF x.mode # Cond THEN
    IF x.type.form = ORB.Bool THEN
      IF x.mode = ORB.Const THEN x.r := 15 - x.a*8
      ELSE load(x);
        IF code[pc-1] DIV C30 # F2 THEN Put1(Cmp, x.r, x.r, 0, "load condition") END;
        x.r := NE; DEC(RH)
      END;
      x.mode := Cond; x.a := 0; x.b := 0
    ELSE ORS.Mark("not Boolean?")
    END
  END
END loadCond;

PROCEDURE loadTypTagAdr(T: ORB.Type);
VAR x: Item;
BEGIN
  IF T.mno > 0 THEN (* imported *)
    x.mode := ORB.Var; x.a := T.len; x.r := -T.mno; loadAdr(x)
  ELSE
    (* insert into fixorgD chain, fixed up in Close *)
    PutPair(0, Add, RH, RH, T.len, 1, "load typ tag address"); incR; T.len := pc-1
  END
END loadTypTagAdr;

PROCEDURE loadStringAdr(VAR x: Item);
BEGIN
  IF x.r >= 0 THEN PutPair(0, Add, RH, RH, varx+x.a, 1, "load local string address")
              ELSE PutPair(x.r, Add, RH, RH, x.a, 1, "load imported string address") END; (* imported *)
  x.mode := Reg; x.r := RH; incR
END loadStringAdr;


(* ------ Items: Conversion from constants or from Objects on the Heap ------ *)
(*        to Items on the Stack                                               *)

PROCEDURE MakeConstItem*(VAR x: Item; typ: ORB.Type; val: INTEGER);
BEGIN x.mode := ORB.Const; x.type := typ; x.a := val
END MakeConstItem;

PROCEDURE MakeRealItem*(VAR x: Item; val: REAL);
BEGIN x.mode := ORB.Const; x.type := ORB.realType;
  x.a := SYSTEM.VAL(INTEGER, val)
END MakeRealItem;

PROCEDURE MakeStringItem*(VAR x: Item; len: INTEGER);
(* - copies string from ORS-buffer to ORG-string array - *)
VAR i: INTEGER;
BEGIN x.mode := ORB.Const; x.type := ORB.strType;
  x.a := strx; x.b := len; x.r := 0; i := 0;
  IF strx + len + 4 < maxStrx THEN
    WHILE len > 0 DO str[strx] := ORS.str[i]; INC(strx); INC(i); DEC(len) END;
    WHILE strx MOD 4 # 0 DO str[strx] := 0X; INC(strx) END
  ELSE ORS.Mark("too many strings")
  END
END MakeStringItem;

PROCEDURE MakeItem*(VAR x: Item; y: ORB.Object);
BEGIN x.obj := y; x.mode := y.class; x.type := y.type;
  x.a := y.val; x.rdo := y.rdo; x.deref := FALSE;
  IF y.class = ORB.Par THEN x.b := 0
  ELSIF (y.class = ORB.Const) & (y.type.form = ORB.String) THEN x.r := y.lev;
    x.a := y.val MOD C20; (* strx/exno*) x.b := y.val DIV C20 (*len *)
  ELSE x.r := y.lev
  END
END MakeItem;

(* ---------- Code generation for Selectors, Variables, Constants ----------- *)

PROCEDURE Field*(VAR x: Item; y: ORB.Object);   (* x := x.y *)
BEGIN x.deref := FALSE;
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
BEGIN s := x.type.base.size; lim := x.type.len; x.deref := FALSE;
  IF (y.mode = ORB.Const) & (y.a < 0) THEN ORS.Mark("bad index") END;
  IF (y.mode = ORB.Const) & (lim >= 0) THEN
    IF y.a >= lim THEN ORS.Mark("bad index") END;
    IF x.mode = ORB.Var THEN
      IF x.r < 0 THEN (* imported *) loadAdr(x); x.mode := RegI; x.a := y.a * s
                 ELSE x.a := y.a * s + x.a END
    ELSIF x.mode = RegI THEN x.a := y.a * s + x.a
    ELSIF x.mode = ORB.Par THEN x.b := y.a * s + x.b
    END
  ELSE load(y);
    IF check THEN  (* check array bounds *)
      IF lim >= 0 THEN Put1a(Cmp, RH, y.r, lim, "check array bound")
      ELSIF x.mode IN {ORB.Var, ORB.Par} THEN (* open array param *)
        Put2(Ldr, RH, SP, x.a+4+frame, "check open array param bounds");
        Put0(Cmp, RH, y.r, RH, "")
      ELSIF x.mode = RegI THEN (* dynamic open array *)
        Put2(Ldr, RH, x.r, -16, "check dynamic open array bounds"); (* len *)
        Put0(Cmp, RH, y.r, RH, "")
      ELSE ORS.Mark("error in Index")
      END;
      Trap(CC, 1)
    END;
    IF    s = 4 THEN Put1(Lsl, y.r, y.r, 2, "index")
    ELSIF s > 1 THEN Put1a(Mul, y.r, y.r, s, "index")
    END;
    IF x.mode = ORB.Var THEN
      IF    x.r > 0 THEN Put0(Add, y.r, SP, y.r, ".. index local"); INC(x.a, frame)  (* local *)
      ELSIF x.r = 0 THEN PutPair(0, Add, RH, RH, 0, 1, ".. index global");           (* global *)
                         Put0(Add, y.r, RH, y.r, "")
                    ELSE PutPair(x.r, Add, RH, RH, x.a, 1, ".. index imported");     (* imported *)
                         Put0(Add, y.r, RH, y.r, ""); x.a := 0
      END;
      x.r := y.r; x.mode := RegI
    ELSIF x.mode = ORB.Par THEN
      Put2(Ldr, RH, SP, x.a + frame, "index parameter");
      Put0(Add, y.r, RH, y.r, "");
      x.mode := RegI; x.r := y.r; x.a := x.b
    ELSIF x.mode = RegI THEN Put0(Add, x.r, x.r, y.r, "index reg indirect"); DEC(RH)
    END
  END
END Index;

PROCEDURE DeRef*(VAR x: Item);
BEGIN
  IF x.mode = ORB.Var THEN
    IF x.r > 0 THEN Put2(Ldr, RH, SP, x.a + frame, "deref local var") (* local *)
               ELSE PutPair(x.r, Ldr, RH, RH, x.a, 2, "deref var") END;
    NilCheck; x.r := RH; incR
  ELSIF x.mode = ORB.Par THEN Put2(Ldr, RH, SP, x.a + frame, "deref param");
                              Put2(Ldr, RH, RH, x.b, "");
                              NilCheck; x.r := RH; incR
  ELSIF x.mode = RegI    THEN Put2(Ldr, x.r, x.r, x.a, "deref reg indirect"); NilCheck
  ELSIF x.mode # Reg     THEN ORS.Mark("bad mode in DeRef")
  END;
  IF x.type.base.form = ORB.Array THEN Put1(Add, x.r, x.r, 16, "deref array pointer") END; (* pointer to array *)
  x.mode := RegI; x.a := 0; x.b := 0; x.deref := TRUE
END DeRef;

PROCEDURE Method*(VAR x: Item; y: ORB.Object; super: BOOLEAN);
BEGIN loadAdr(x); (* receiver *)
  IF super THEN x.a := y.val; (* mthadr/exno *) x.b := -y.type.mno
  ELSE x.a := y.lev; (* mthno *)
    IF x.deref THEN x.b := ORB.Var ELSE x.b := ORB.Par END
  END;
  x.deref := FALSE; x.super := super
END Method;

PROCEDURE Q(T: ORB.Type; VAR tdw: INTEGER);
BEGIN (* one entry of type descriptor extension table *)
  IF T.base # NIL THEN
    Q(T.base, tdw);
    td[tdw] := (T.mno*C12 + T.len (* TDoff/exno *)) * C12 + tdw - fixorgT;
    fixorgT := tdw; INC(tdw)
  END
END Q;

PROCEDURE FindRefFlds(ftyp: SET; typ: ORB.Type; off: INTEGER; VAR tdw: INTEGER);
VAR fld: ORB.Object; i, s: INTEGER;
BEGIN
  IF typ.form IN ftyp THEN td[tdw] := off; INC(tdw)
  ELSIF typ.form = ORB.Record THEN fld := typ.dsc;
    WHILE fld # NIL DO
      FindRefFlds(ftyp, fld.type, fld.val + off, tdw); fld := fld.next
    END
  ELSIF typ.form = ORB.Array THEN s := typ.base.size;
    FOR i := 0 TO typ.len-1 DO FindRefFlds(ftyp, typ.base, i*s + off, tdw) END
  END
END FindRefFlds;


PROCEDURE BuildTD(T: ORB.Type; VAR tdw: INTEGER);
VAR
  k, s:     INTEGER;
  fld, bot: ORB.Object;
  t:        ORB.Type;
BEGIN (* type descriptors of base types of T are already built *)
  k := ORB.NofMethods(T); td[tdw] := -k-1; INC(tdw); s := tdw;
  WHILE k > 0 DO td[tdw] := -1; INC(tdw); DEC(k) END;
  t := T; fld := NIL;  (* build method table *)
  WHILE t # NIL DO fld := t.dsc;
    IF t.base # NIL THEN bot := t.base.dsc ELSE bot := NIL END;
    WHILE fld # bot DO
      IF (fld.class = ORB.Const) & (td[tdw-fld.lev-1] = -1) & ((t.mno = 0)
      OR (fld.name[0] # 0X)) THEN
        td[tdw-fld.lev-1] := (t.mno*C16 + fld.val (* mthadr/exno *)) * C10
      END;
      fld := fld.next
    END;
    t := t.base
  END;
  FOR k := s TO tdw-1 DO  (* insert displacements in ascending order *)
    IF td[k] # -1 THEN td[k] := td[k] + k - fixorgM; fixorgM := k
                  ELSE td[k] := 0 END
  END;
  s := T.size;  (* convert size for heap allocation *)
  IF    s <= 24  THEN s := 32
  ELSIF s <= 56  THEN s := 64
  ELSIF s <= 120 THEN s := 128
                 ELSE s := (s+263) DIV 256 * 256
  END;
  T.len := tdw*4; (* len used as type descriptor offset in bytes relative to tdx *)
  td[tdw] := s; INC(tdw);
  k := T.nofpar;  (* extension level! *)
  IF k > 3 THEN ORS.Mark("ext level too large")
  ELSE Q(T, tdw);
    WHILE k < 3 DO td[tdw] := -1; INC(tdw); INC(k) END
  END;
  FindRefFlds(ORB.Ptrs, T, 0, tdw);  td[tdw] := -1; INC(tdw);
  FindRefFlds(ORB.Procs, T, 0, tdw); td[tdw] := -1; INC(tdw);
  IF tdw >= maxTD THEN ORS.Mark("too many record types"); tdw := 0 END
END BuildTD;


PROCEDURE TypeTest*(VAR x: Item; T: ORB.Type; varpar, isguard: BOOLEAN);
VAR pc0: INTEGER;
BEGIN
  IF T = NIL THEN
    IF x.mode >= Reg THEN DEC(RH) END;
    SetCC(x, 7)
  ELSE (* fetch tag into RH *)
    IF varpar THEN Put2(Ldr, RH, SP, x.a+4+frame, "varpar type test")
    ELSE load(x);
      pc0 := pc; Put3(BC, EQ, 0, "type test");  (* NIL belongs to every pointer type *)
      Put2(Ldr, RH, x.r, -8, "")
    END;
    Put2(Ldr, RH, RH, T.nofpar*4, ""); incR;
    loadTypTagAdr(T);  (* tag of T *)
    Put0(Cmp, RH-1, RH-1, RH-2, ""); DEC(RH, 2);
    IF ~varpar THEN fix3(pc0, pc - pc0 - 1) END;
    IF isguard THEN
      IF check THEN Trap(NE, 2) END
    ELSE SetCC(x, EQ);
      IF ~varpar THEN DEC(RH) END
    END
  END
END TypeTest;


(* ----------------- Code generation for Boolean operators ------------------ *)

PROCEDURE Not*(VAR x: Item);   (* x := ~x *)
VAR t: INTEGER;
BEGIN loadCond(x); x.r := negated(x.r); t := x.a; x.a := x.b; x.b := t
END Not;

PROCEDURE And1*(VAR x: Item);   (* x := x & *)
BEGIN loadCond(x); Put3(BC, negated(x.r), x.a, "&"); x.a := pc-1; FixLink(x.b); x.b := 0
END And1;

PROCEDURE And2*(VAR x, y: Item);
BEGIN loadCond(y); x.a := merged(y.a, x.a); x.b := y.b; x.r := y.r
END And2;

PROCEDURE Or1*(VAR x: Item);   (* x := x OR *)
BEGIN loadCond(x); Put3(BC, x.r, x.b, "OR");  x.b := pc-1; FixLink(x.a); x.a := 0
END Or1;

PROCEDURE Or2*(VAR x, y: Item);
BEGIN loadCond(y); x.a := y.a; x.b := merged(y.b, x.b); x.r := y.r
END Or2;


(* ---------------- Code generation for arithmetic operators ---------------- *)

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
  ELSE (* form = Set *)
    IF x.mode = ORB.Const THEN x.a := -x.a-1
    ELSE load(x); Put1(Xor, x.r, x.r, -1, "Neg set")
    END
  END
END Neg;

PROCEDURE AddOp*(op: INTEGER; VAR x, y: Item);   (* x := x +- y *)
BEGIN
  IF op = ORS.plus THEN
    IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN x.a := x.a + y.a
    ELSIF y.mode = ORB.Const THEN load(x);
      IF y.a # 0 THEN Put1a(Add, x.r, x.r, y.a, "plus") END
    ELSE load(x); load(y); Put0(Add, RH-2, x.r, y.r, "plus"); DEC(RH); x.r := RH-1
    END
  ELSE (* op = ORS.minus *)
    IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN x.a := x.a - y.a
    ELSIF y.mode = ORB.Const THEN load(x);
      IF y.a # 0 THEN Put1a(Sub, x.r, x.r, y.a, "minus") END
    ELSE load(x); load(y); Put0(Sub, RH-2, x.r, y.r, "minus"); DEC(RH); x.r := RH-1
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
  IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN x.a := x.a * y.a
  ELSIF (y.mode = ORB.Const)
      & (y.a >= 2)
      & (log2(y.a, e) = 1) THEN load(x); Put1(Lsl, x.r, x.r, e, "multiply by const power of 2")
  ELSIF y.mode = ORB.Const THEN load(x); Put1a(Mul, x.r, x.r, y.a, "multiply const")
  ELSIF (x.mode = ORB.Const)
      & (x.a >= 2)
      & (log2(x.a, e) = 1) THEN load(y); Put1(Lsl, y.r, y.r, e, "multiply const power of 2 by y");
                                x.mode := Reg; x.r := y.r
  ELSIF x.mode = ORB.Const THEN load(y); Put1a(Mul, y.r, y.r, x.a, "multiply const");
                                x.mode := Reg; x.r := y.r
                           ELSE load(x); load(y); Put0(Mul, RH-2, x.r, y.r, "multiply");
                                DEC(RH); x.r := RH-1
  END
END MulOp;

PROCEDURE DivOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR e: INTEGER;
BEGIN
  IF op = ORS.div THEN
    IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN
      IF y.a > 0 THEN x.a := x.a DIV y.a ELSE ORS.Mark("bad divisor") END
    ELSIF (y.mode = ORB.Const) & (y.a >= 2) & (log2(y.a, e) = 1) THEN
      load(x); Put1(Asr, x.r, x.r, e, "int divide by const power of 2")
    ELSIF y.mode = ORB.Const THEN
      IF y.a > 0 THEN load(x); Put1a(Div, x.r, x.r, y.a, "divide by const")
                 ELSE ORS.Mark("bad divisor") END
    ELSE load(y);
      IF check THEN Trap(LE, 6) END;
      load(x); Put0(Div, RH-2, x.r, y.r, "divide"); DEC(RH); x.r := RH-1
    END
  ELSE (* op = ORS.mod *)
    IF (x.mode = ORB.Const) & (y.mode = ORB.Const) THEN
      IF y.a > 0 THEN x.a := x.a MOD y.a ELSE ORS.Mark("bad modulus") END
    ELSIF (y.mode = ORB.Const) & (y.a >= 2) & (log2(y.a, e) = 1) THEN load(x);
      IF e <= 16 THEN Put1(And, x.r, x.r, y.a-1, "Modulus const power of 2")
                 ELSE Put1(Lsl, x.r, x.r, 32-e, "Nodulus large const power of 2"); Put1(Ror, x.r, x.r, 32-e, "") END
    ELSIF y.mode = ORB.Const THEN
      IF y.a > 0 THEN load(x); Put1a(Div, x.r, x.r, y.a, "Modulus const"); Put0(Mov+U, x.r, 0, 0, "")
                 ELSE ORS.Mark("bad modulus") END
    ELSE load(y);
      IF check THEN Trap(LE, 6) END;
      load(x); Put0(Div, RH-2, x.r, y.r, "modulus"); Put0(Mov+U, RH-2, 0, 0, "");
      DEC(RH); x.r := RH-1
    END
  END
END DivOp;

(* ------------------- Code generation for REAL operators ------------------- *)

PROCEDURE RealOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
BEGIN load(x); load(y);
  IF    op = ORS.plus  THEN Put0(Fad, RH-2, x.r, y.r, "plus")
  ELSIF op = ORS.minus THEN Put0(Fsb, RH-2, x.r, y.r, "minus")
  ELSIF op = ORS.times THEN Put0(Fml, RH-2, x.r, y.r, "times")
  ELSIF op = ORS.rdiv  THEN Put0(Fdv, RH-2, x.r, y.r, "rdivide")
  END;
  DEC(RH); x.r := RH-1
END RealOp;

(* - Code generation for set operators - *)

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
    ELSE load(x); Put1(Mov, RH, 0, -1, "x := {x..y}"); Put0(Lsl, x.r, RH, x.r, "")
    END;
    IF (y.mode = ORB.Const) & (y.a < 16) THEN Put1(Mov, RH, 0, LSL(-2, y.a), "x := {x..y}");
                                              y.mode := Reg; y.r := RH; incR
    ELSE load(y); Put1(Mov, RH, 0, -2, "x := {x..y}"); Put0(Lsl, y.r, RH, y.r, "")
    END;
    IF x.mode = ORB.Const THEN
      IF x.a # 0 THEN Put1(Xor, y.r, y.r, -1, "x := {x..y}"); Put1a(And, RH-1, y.r, x.a, "") END;
      x.mode := Reg
    ELSE DEC(RH); Put0(Ann, RH-1, x.r, y.r, "x := {x..y}")
    END;
    x.r := RH-1
  END
END Set;

PROCEDURE In*(VAR x, y: Item);  (* x := x IN y *)
BEGIN load(y);
  IF x.mode = ORB.Const THEN Put1(Ror, y.r, y.r, (x.a + 1) MOD maxSet, "IN const"); DEC(RH)
  ELSE load(x); Put1(Add, x.r, x.r, 1, "IN"); Put0(Ror, y.r, y.r, x.r, ""); DEC(RH, 2)
  END;
  SetCC(x, MI)
END In;

PROCEDURE SetOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
VAR xset, yset: SET; (* x.type.form = Set *)
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
    IF    op = ORS.plus  THEN Put1a(Ior, x.r, x.r, y.a, "set plus const")
    ELSIF op = ORS.minus THEN Put1a(Ann, x.r, x.r, y.a, "set minus const")
    ELSIF op = ORS.times THEN Put1a(And, x.r, x.r, y.a, "set times const")
    ELSIF op = ORS.rdiv  THEN Put1a(Xor, x.r, x.r, y.a, "set rdiv const")
    END;
  ELSE load(x); load(y);
    IF    op = ORS.plus  THEN Put0(Ior, RH-2, x.r, y.r, "set plus")
    ELSIF op = ORS.minus THEN Put0(Ann, RH-2, x.r, y.r, "set minus")
    ELSIF op = ORS.times THEN Put0(And, RH-2, x.r, y.r, "set times")
    ELSIF op = ORS.rdiv  THEN Put0(Xor, RH-2, x.r, y.r, "set rdiv")
    END;
    DEC(RH); x.r := RH-1
  END
END SetOp;


(* --------------------- Code generation for relations ---------------------- *)

PROCEDURE IntRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
BEGIN
  IF (y.mode = ORB.Const) & (y.type.form # ORB.Proc) THEN
    load(x);
    IF (y.a # 0) OR ~(op IN {ORS.eql, ORS.neq})
    OR (code[pc-1] DIV C30 # F2) THEN Put1a(Cmp, x.r, x.r, y.a, "int relation const") END;
    DEC(RH)
  ELSE
    IF (x.mode = Cond) OR (y.mode = Cond) THEN ORS.Mark("not implemented") END;
    load(x); load(y); Put0(Cmp, x.r, x.r, y.r, "int relation"); DEC(RH, 2)
  END;
  SetCC(x, relmap[op - ORS.eql])
END IntRelation;

PROCEDURE RealRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
BEGIN load(x);
  IF (y.mode = ORB.Const) & (y.a = 0) THEN DEC(RH)
  ELSE load(y); Put0(Fsb, x.r, x.r, y.r, "real relation"); DEC(RH, 2)
  END;
  SetCC(x, relmap[op - ORS.eql])
END RealRelation;

PROCEDURE StringRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
  (* x, y are char arrays or strings *)
BEGIN
  w.sl("                          ; String comparison");
  IF x.type.form = ORB.String THEN loadStringAdr(x) ELSE loadAdr(x) END;
  IF y.type.form = ORB.String THEN loadStringAdr(y) ELSE loadAdr(y) END;
  Put2(Ldr+1, RH,   x.r, 0,    ""); Put1(Add, x.r, x.r, 1, "");
  Put2(Ldr+1, RH+1, y.r, 0,    ""); Put1(Add, y.r, y.r, 1, "");
  Put0(Cmp,   RH+2, RH,  RH+1, ""); Put3(BC,  NE,  2,      "");
  Put1(Cmp,   RH+2, RH,  0,    ""); Put3(BC,  NE, -8,      "");
  DEC(RH, 2); SetCC(x, relmap[op - ORS.eql])
END StringRelation;


(* --------------------- Code generation of Assignments --------------------- *)

PROCEDURE StrToChar*(VAR x: Item);
BEGIN x.type := ORB.charType; DEC(strx, 4); x.a := ORD(str[x.a])
END StrToChar;

PROCEDURE Store*(VAR x, y: Item); (* x := y *)
VAR op: INTEGER;
BEGIN  load(y);
  IF x.type.size = 1 THEN op := Str+1 ELSE op := Str END;
  IF x.mode = ORB.Var THEN
    IF x.r > 0 THEN Put2(op, y.r, SP, x.a + frame, "store local var") (* local *)
               ELSE PutPair(x.r, op, y.r, RH, x.a, 2, "store var") END
  ELSIF x.mode = ORB.Par THEN Put2(Ldr, RH, SP, x.a + frame, "store parameter");
                              Put2(op, y.r, RH, x.b, "");
  ELSIF x.mode = RegI    THEN Put2(op, y.r, x.r, x.a, "store register indirect"); DEC(RH);
  ELSE ORS.Mark("bad mode in Store")
  END;
  DEC(RH)
END Store;

PROCEDURE StoreStruct*(VAR x, y: Item); (* x := y, frame = 0 *)
VAR s, pc0: INTEGER;
BEGIN loadAdr(x); loadAdr(y);
  IF (x.type.form = ORB.Array) & (x.type.len > 0) THEN
    IF y.type.len >= 0 THEN
      IF x.type.size = y.type.size THEN Put1a(Mov, RH, 0, (y.type.size+3) DIV 4, "store array")
      ELSE ORS.Mark("different length/size, not implemented")
      END
    ELSE (* y open array param or dynamic open array *)
      IF y.type.size > 0 THEN Put2(Ldr, RH, SP, y.a+4, "store array dynamic/open")
                         ELSE Put2(Ldr, RH, y.r, -16, "store array dynamic/open") END; (* len *)
      s := y.type.base.size;  (* element size *)
      pc0 := pc; Put3(BC, EQ, 0, "");
      IF s = 1 THEN Put1(Add, RH, RH, 3, ""); Put1(Asr, RH, RH, 2, "")
      ELSIF s # 4 THEN Put1a(Mul, RH, RH, s DIV 4, "")
      END;
      IF check THEN (* check array lengths *)
        Put1a(Mov, RH+1, 0, (x.type.size+3) DIV 4, "  (check array lengths)");
        Put0(Cmp, RH+1, RH, RH+1, ""); Trap(GT, 3)
      END;
      fix3(pc0, pc + 5 - pc0)
    END
  ELSIF x.type.form = ORB.Record THEN Put1a(Mov, RH, 0, x.type.size DIV 4, "store record")
  ELSE ORS.Mark("inadmissible assignment")
  END;
  Put2(Ldr, RH+1, y.r, 0, ""); Put1(Add, y.r, y.r, 4, "");
  Put2(Str, RH+1, x.r, 0, ""); Put1(Add, x.r, x.r, 4, "");
  Put1(Sub, RH,   RH,  1, ""); Put3(BC,  NE,  -6,     ""); RH := 0
END StoreStruct;

PROCEDURE CopyString*(VAR x, y: Item);  (* x := y, frame = 0 *)
   VAR len: INTEGER;
BEGIN
  w.sl("                          ; String copy");
  loadAdr(x); len := x.type.len;
  IF len >= 0 THEN
    IF len <  y.b THEN ORS.Mark("string too long") END
  ELSIF check THEN (* x open array param or dynamic open array *)
    IF x.type.size > 0 THEN Put2(Ldr, RH, SP, x.a+4, "check open array size")
                       ELSE Put2(Ldr, RH, x.r, -16, "check dynamic array size") END; (* len *)
    Put1(Cmp,RH, RH, y.b, ""); Trap(LT, 3)
  END;
  loadStringAdr(y);
  Put2(Ldr, RH, y.r, 0, ""); Put1(Add, y.r, y.r, 4, "");
  Put2(Str, RH, x.r, 0, ""); Put1(Add, x.r, x.r, 4, "");
  Put1(Asr, RH, RH, 24, ""); Put3(BC,  NE,      -6, "");  RH := 0
END CopyString;

(* --------------------- Code generation for parameters --------------------- *)

PROCEDURE OpenArrayParam*(VAR x: Item);
BEGIN loadAdr(x);
  IF x.type.len >= 0 THEN Put1a(Mov, RH, 0, x.type.len, "OpenArrayParam")
  ELSIF x.type.size > 0 THEN (* open array param *)
                             Put2(Ldr, RH, SP, x.a+4+frame, "open array param")
                        ELSE (* dynamic open array*)
                             Put2(Ldr, RH, x.r, -16, "dynamic open array") (*len *)
  END;
  incR
END OpenArrayParam;

PROCEDURE VarParam*(VAR x: Item; ftype: ORB.Type);
VAR xmd: INTEGER;
BEGIN xmd := x.mode; loadAdr(x);
  IF (ftype.form = ORB.Array) & (ftype.len < 0) THEN (* open array *)
    IF x.type.len >= 0 THEN Put1a(Mov, RH, 0, x.type.len, "var open array param")
    ELSIF x.type.size > 0 THEN (* open array param *) Put2(Ldr, RH, SP, x.a+4+frame, "var open array param")
    ELSE (* dynamic open array*) Put2(Ldr, RH, x.r, -16, "var dynamic array param") (*len *)
    END;
    incR
  ELSIF ftype.form = ORB.Record THEN
    IF x.deref THEN Put2(Ldr, RH, x.r, -8, "var record param"); incR
    ELSIF xmd = ORB.Par THEN Put2(Ldr, RH, SP, x.a+4+frame, "var record param"); incR
    ELSE loadTypTagAdr(x.type)
    END
  END
END VarParam;

PROCEDURE ValueParam*(VAR x: Item);
BEGIN load(x)
END ValueParam;

PROCEDURE StringParam*(VAR x: Item);
BEGIN loadStringAdr(x); Put1(Mov, RH, 0, x.b, "string param"); incR  (* len *)
END StringParam;

PROCEDURE ReceiverParam*(VAR x: Item; par: ORB.Object);
BEGIN
  IF x.r # RH THEN Put0(Mov, RH, 0, x.r, "receiver param -> R0") END;  (* receiver -> R0 *)
  incR;
  IF par.class = ORB.Par THEN (* record*)
    loadTypTagAdr(par.type) (*type tag -> R1 *)
  END
END ReceiverParam;


(* ----------------------------- For Statements ----------------------------- *)

PROCEDURE For0*(VAR x, y: Item);
BEGIN load(y)
END For0;

PROCEDURE For1*(VAR x, y, z, w: Item; VAR L: INTEGER);
BEGIN
  IF z.mode = ORB.Const THEN Put1a(Cmp, RH, y.r, z.a, "For1 const")
  ELSE load(z); Put0(Cmp, RH-1, y.r, z.r, "For1"); DEC(RH)
  END;
  L := pc;
  IF w.a > 0 THEN Put3(BC, GT, 0, "")
  ELSIF w.a < 0 THEN Put3(BC, LT, 0, "")
  ELSE ORS.Mark("zero increment"); Put3(BC, MI, 0, "")
  END;
  Store(x, y)
END For1;

PROCEDURE For2*(VAR x, y, w: Item);
BEGIN load(x); DEC(RH); Put1a(Add, x.r, x.r, w.a, "For2")
END For2;


(* --------- Branches, procedure calls, procedure prolog and epilog --------- *)

PROCEDURE Here*(): INTEGER;
BEGIN RETURN pc
END Here;

PROCEDURE FJump*(VAR L: INTEGER);
BEGIN Put3(BC, 7, L, "FJump"); L := pc-1
END FJump;

PROCEDURE CFJump*(VAR x: Item);
BEGIN loadCond(x); Put3(BC, negated(x.r), x.a, "CFJump"); FixLink(x.b); x.a := pc-1
END CFJump;

PROCEDURE BJump*(L: INTEGER);
BEGIN Put3(BC, 7, L-pc-1, "BJump")
END BJump;

PROCEDURE CBJump*(VAR x: Item; L: INTEGER);
BEGIN loadCond(x); Put3(BC, negated(x.r), L-pc-1, "CBJump"); FixLink(x.b); FixLinkWith(x.a, L)
END CBJump;

PROCEDURE Fixup*(VAR x: Item);
BEGIN FixLink(x.a)
END Fixup;

PROCEDURE SaveRegs(r: INTEGER);  (* R[0 .. r-1]*)
VAR r0: INTEGER;
BEGIN (* r > 0 *) r0 := 0;
  Put1(Sub, SP, SP, r*4, "alloc for save reg"); INC(frame, 4*r);
  REPEAT Put2(Str, r0, SP, (r-r0-1)*4, "save reg"); INC(r0) UNTIL r0 = r
END SaveRegs;

PROCEDURE RestoreRegs(r: INTEGER); (* R[0 .. r-1] *)
VAR r0: INTEGER;
BEGIN (* r > 0 *) r0 := r;
  REPEAT DEC(r0); Put2(Ldr, r0, SP, (r-r0-1)*4, "restore reg") UNTIL r0 = 0;
  Put1(Add, SP, SP, r*4, "free saved regs"); DEC(frame, 4*r)
END RestoreRegs;

PROCEDURE PrepCall*(VAR x: Item; VAR r: INTEGER);
BEGIN (* x.type.form IN {ORB.Proc, ORB.TProc} *)
  IF x.type.form = ORB.TProc THEN DEC(RH) ELSIF x.mode > ORB.Par THEN load(x) END;
  r := RH;
  IF RH > 0 THEN SaveRegs(RH); RH := 0 END
END PrepCall;

PROCEDURE Call*(VAR x: Item; r: INTEGER);
BEGIN (* x.type.form IN {ORB.Proc, ORB.TProc} *)
  w.s("                          ; Call ");
  IF x.type.form = ORB.TProc THEN
    w.s("type bound procedure ");    w.s(x.obj.name)
  ELSE
    CASE x.mode OF
    |1:  w.s("module procedure ");    w.s(x.obj.name)
    |2:  w.s("variable procedure ");  w.s(x.obj.name)
    |10: w.s("record field procedure");
    END
  END;  w.l;
  IF x.type.form = ORB.TProc THEN  (* type-bound procedure *)
    IF x.super THEN  (* super call *)
      IF x.b >= 0 THEN Put3(BL, 7, (x.a DIV 4)-pc-1, "Local super call")
      ELSE (* imported *) Put3a(BL, -x.b, x.a, pc-fixorgP, "Imported super call"); fixorgP := pc-1
      END
    ELSE  (* method call *)
      IF x.b = ORB.Var THEN Put2(Ldr, RH, 0, -8, "var method call") ELSE Put0(Mov, RH, 0, 1, "method call") END;
      Put2(Ldr, RH, RH, -4-x.a*4, ""); Put3(BLR, 7, RH, "")
    END
  ELSIF x.mode = ORB.Const THEN  (* regular procedure *)
    IF x.r >= 0 THEN Put3(BL, 7, (x.a DIV 4)-pc-1, "regular procedure call")
    ELSE (* imported *) Put3a(BL, -x.r, x.a, pc-fixorgP, "imported procedure call"); fixorgP := pc-1
    END
  ELSE  (* installed procedure *)
    IF x.mode <= ORB.Par THEN load(x); DEC(RH)
    ELSE Put2(Ldr, RH, SP, 0, ""); Put1(Add, SP, SP, 4, ""); DEC(r); DEC(frame, 4)
    END;
    IF check THEN Trap(EQ, 5) END;
    Put3(BLR, 7, RH, "installed procedure call")
  END;
  IF x.type.base.form = ORB.NoTyp THEN (* procedure *) RH := 0
  ELSE  (* function *)
    IF r > 0 THEN Put0(Mov, r, 0, 0, "function result"); RestoreRegs(r) END;
    x.mode := Reg; x.r := r; RH := r+1
  END
END Call;

PROCEDURE Enter*(parblksize, locblksize: INTEGER; int: BOOLEAN; proc: ORB.Object);
VAR a, r: INTEGER;
BEGIN frame := 0;
  IF ~int THEN (* procedure prolog *)
    w.l; w.s("PROCEDURE "); w.s(proc.name); w.l;
    IF locblksize >= C18 THEN ORS.Mark("locals too large") END;
    a := 4; r := 0;
    Put1(Sub, SP, SP, locblksize, ""); Put2(Str, LNK, SP, 0, "");
    WHILE a < parblksize DO Put2(Str, r, SP, a, ""); INC(r); INC(a, 4) END
  ELSE (* interrupt procedure *)
    w.sl("Interrupt procedure prolog");
    Put1(Sub, SP, SP, locblksize, ""); Put2(Str, 0, SP, 0, "");
    Put2(Str, 1, SP, 4, ""); Put2(Str, 2, SP, 8, "")
    (* R0, R1, R2 saved on stack *)
  END
END Enter;

PROCEDURE Return*(form: INTEGER; VAR x: Item; size: INTEGER; int: BOOLEAN);
BEGIN
  IF form # ORB.NoTyp THEN
    w.sl("Load return value");
    load(x)
  END;
  IF ~int THEN (* procedure epilog *)
    w.sl("Procedure epilog");
    Put2(Ldr, LNK, SP, 0, ""); Put1(Add, SP, SP, size, ""); Put3(BR, 7, LNK, "")
  ELSE (* interrupt return, restore R2, R1, R0 *)
    w.sl("Interrupt procedure epilog");
    Put2(Ldr, 2, SP, 8, ""); Put2(Ldr, 1, SP, 4, ""); Put2(Ldr, 0, SP, 0, "");
    Put1(Add, SP, SP, size, ""); Put3(BR, 7, RTI, "")
  END;
  RH := 0
END Return;


(* ---------------------------- Case Statements ----------------------------- *)

PROCEDURE CaseHead*(VAR x: Item; VAR L0: INTEGER);
BEGIN load(x);  (* value of case expression *)
  L0 := pc; Put1(Cmp, RH, x.r, 0, "Case");  (* higher bound, fixed up in CaseTail *)
  Put3(BC, CC, 0, "");         (* branch to else, fixed up in CaseTail *)
  Put1(Add, x.r, x.r, 0, "");  (* nof words between BL instruction at L0+4 and
                                  jump table, fixed up in CaseTail *)
  Put1(Lsl, x.r, x.r, 2, "");
  (* L0+4*) Put3(BL, 7, 0, "");  (*LNK := PC+1 *)
  Put0(Add, LNK, LNK, x.r, ""); Put3(BR, 7, LNK, ""); DEC(RH)
END CaseHead;

PROCEDURE CaseTail*(L0, L1: INTEGER; n: INTEGER; VAR tab: ARRAY OF LabelRange);
(* - L1 = label for else - *)
VAR i, j: INTEGER;
BEGIN
  IF    n  > 0 THEN fix1(L0, tab[n-1].high + 1) (* higher bound *)
  ELSIF L1 = 0 THEN ORS.Mark("empty case") END;
  IF L1 = 0 THEN L1 := pc; Trap(7, 1) END;  (* create else *)
  fix3(L0+1, L1-L0-2);  (* branch to else *)
  fix1(L0+2, pc-L0-5);  (* nof words between BL instruction at L0+4 and jump table *)
  j := 0;
  FOR i := 0 TO n-1 DO  (* construct jump table *)
    WHILE j < tab[i].low DO BJump(L1); INC(j) END;  (* else *)
    WHILE j <= tab[i].high DO BJump(tab[i].label); INC(j) END
  END
END CaseTail;


(* ------------------------ In-line code procedures ------------------------- *)

PROCEDURE Increment*(upordown: INTEGER; VAR x, y: Item);
VAR op, zr, v: INTEGER;
BEGIN (* frame = 0 *)
  IF upordown = 0 THEN op := Add ELSE op := Sub END;
  IF x.type = ORB.byteType THEN v := 1 ELSE v := 0 END;
  IF y.type.form = ORB.NoTyp THEN y.mode := ORB.Const; y.a := 1 END;
  IF (x.mode = ORB.Var) & (x.r > 0) THEN
    zr := RH; Put2(Ldr+v, zr, SP, x.a, ""); incR;
    IF y.mode = ORB.Const THEN Put1a(op, zr, zr, y.a, "INC/DEC")
                          ELSE load(y); Put0(op, zr, zr, y.r, "INC/DEC"); DEC(RH) END;
    Put2(Str+v, zr, SP, x.a, ""); DEC(RH)
  ELSE loadAdr(x); zr := RH; Put2(Ldr+v, RH, x.r, 0, ""); incR;
    IF y.mode = ORB.Const THEN Put1a(op, zr, zr, y.a, "INC/DEC")
                          ELSE load(y); Put0(op, zr, zr, y.r, "INC/DEC"); DEC(RH) END;
    Put2(Str+v, zr, x.r, 0, ""); DEC(RH, 2)
  END
END Increment;

PROCEDURE Include*(inorex: INTEGER; VAR x, y: Item);
VAR op, zr: INTEGER;
BEGIN loadAdr(x); zr := RH; Put2(Ldr, RH, x.r, 0, "INCL"); incR;
  IF inorex = 0 THEN op := Ior ELSE op := Ann END;
  IF y.mode = ORB.Const THEN
    Put1a(op, zr, zr, LSL(1, y.a), "")
  ELSE
    load(y); Put1(Mov, RH, 0, 1, "");
    Put0(Lsl, y.r, RH, y.r, ""); Put0(op, zr, zr, y.r, ""); DEC(RH)
  END;
  Put2(Str, zr, x.r, 0, ""); DEC(RH, 2)
END Include;

PROCEDURE Assert*(VAR x: Item);
VAR cond: INTEGER;
BEGIN loadCond(x);
  IF x.a = 0 THEN cond := negated(x.r)
  ELSE Put3(BC, x.r, x.b, "Assert"); FixLink(x.a); x.b := pc-1; cond := 7
  END;
  Trap(cond, 7); FixLink(x.b)
END Assert;

PROCEDURE New*(VAR x, y: Item);
VAR z: Item; xt: ORB.Type; xf: INTEGER;
BEGIN
  w.sl("New");
  loadAdr(x);
  IF y.type = ORB.noType THEN (* record *) loadTypTagAdr(x.type.base)
  ELSE (* array *) xt:= x.type.base.base; xf := xt.form;
    IF    xf = ORB.Record  THEN (* array of record, blktyp = 1 *)
                                loadTypTagAdr(xt);
                                Put1(Add, RH-1, RH-1, 1, "new array of record")
    ELSIF xf = ORB.Pointer THEN (* array of pointer, blktyp = 2 *)
                                Put1(Mov, RH, 0, 2, "new array of pointer"); incR
    ELSIF xf = ORB.Proc    THEN (* array of procedure, blktyp = 3, tag = 7 *)
                                Put1(Mov, RH, 0, 7, "new array of procedure"); incR
    ELSIF xf < ORB.Pointer THEN (* array of basic type, blktyp = 3, tag = 3 *)
                                Put1(Mov, RH, 0, 3, "new array of basic type"); incR
    ELSE ORS.Mark("no valid ptr base type")
    END;
    IF y.mode = ORB.Const THEN
      IF y.a > 0 THEN load(y) (* len *) ELSE ORS.Mark("not a valid dyn array length") END
    ELSE load(y); (* len *)
      IF check THEN Trap(LE, 1) END
    END;
    z.mode := ORB.Const; z.type := ORB.intType; z.a := xt.size; load(z) (* elemsize *)
  END;
  Trap(7, 0); RH := 0
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
BEGIN w.sl("GET"); load(x); x.type := y.type; x.mode := RegI; x.a := 0; Store(y, x)
END Get;

PROCEDURE Put*(VAR x, y: Item);
BEGIN w.sl("PUT"); load(x); x.type := y.type; x.mode := RegI; x.a := 0; Store(x, y)
END Put;

PROCEDURE Copy*(VAR x, y, z: Item);
BEGIN
  w.sl("Copy");
  load(x); load(y);
  IF z.mode = ORB.Const THEN
    IF z.a > 0 THEN load(z) ELSE ORS.Mark("bad count") END
  ELSE load(z);
    IF check THEN Trap(LT, 3) END;
    Put3(BC, EQ, 6, "")
  END;
  Put2(Ldr, RH,  x.r, 0, ""); Put1(Add, x.r, x.r, 4, "");
  Put2(Str, RH,  y.r, 0, ""); Put1(Add, y.r, y.r, 4, "");
  Put1(Sub, z.r, z.r, 1, ""); Put3(BC,  NE,      -6, ""); DEC(RH, 3)
END Copy;

PROCEDURE LDPSR*(VAR x: Item);
BEGIN (* x.mode = Const *)  Put3(0, 15, x.a + STI, "LDPSR")
END LDPSR;

PROCEDURE LDREG*(VAR x, y: Item);
BEGIN
  IF y.mode = ORB.Const THEN Put1a(Mov, x.a, 0, y.a, "LDREG const")
  ELSE load(y); Put0(Mov, x.a, 0, y.r, "LDREG"); DEC(RH)
  END
END LDREG;


(* ------------------------- In-line code functions ------------------------- *)

PROCEDURE Abs*(VAR x: Item);
BEGIN
  IF x.mode = ORB.Const THEN x.a := ABS(x.a)
  ELSE load(x);
    IF x.type.form = ORB.Real THEN Put1(Lsl, x.r, x.r, 1, "real abs");
                                   Put1(Ror, x.r, x.r, 1, "")
                              ELSE Put1(Cmp, x.r, x.r, 0, "int abs");
                                   Put3(BC, GE, 2, "");
                                   Put1(Mov, RH, 0, 0, "");
                                   Put0(Sub, x.r, RH, x.r, "")
    END
  END
END Abs;

PROCEDURE Odd*(VAR x: Item);
BEGIN load(x); Put1(And, x.r, x.r, 1, "Odd"); SetCC(x, NE); DEC(RH)
END Odd;

PROCEDURE Floor*(VAR x: Item);
BEGIN load(x); Put1(Mov+U, RH, 0, ZeroD, "Floor"); Put0(Fad+V, x.r, x.r, RH, "")
END Floor;

PROCEDURE Float*(VAR x: Item);
BEGIN load(x); Put1(Mov+U, RH, 0, ZeroD, "Float");  Put0(Fad+U, x.r, x.r, RH, "")
END Float;

PROCEDURE Ord*(VAR x: Item);
BEGIN
  IF x.mode IN {ORB.Var, ORB.Par, RegI, Cond} THEN load(x);
    IF (x.type.form = ORB.Pointer) & (x.type.base.form = ORB.Array) THEN
      Put1(Add, x.r, x.r, 16, "Ord")
    END
  END
END Ord;

PROCEDURE Len*(VAR x: Item);
BEGIN
  IF x.type.len >= 0 THEN
    IF x.mode = RegI THEN DEC(RH) END;
    x.mode := ORB.Const; x.a := x.type.len
  ELSIF x.type.size > 0 THEN (* open array param *)
    Put2(Ldr, RH, SP, x.a + 4 + frame, "open array LEN"); x.mode := Reg; x.r := RH; incR
  ELSE (* dynamic open array*) Put2(Ldr, x.r, x.r, -16, "dynamic array LEN"); (*len *) x.mode := Reg
  END
END Len;

PROCEDURE Shift*(fct: INTEGER; VAR x, y: Item);
VAR op: INTEGER;
BEGIN load(x);
  IF fct = 0 THEN op := Lsl ELSIF fct = 1 THEN op := Asr ELSE op := Ror END;
  IF y.mode = ORB.Const THEN Put1(op, x.r, x.r, y.a MOD maxSet, "shift")
  ELSE load(y); Put0(op, RH-2, x.r, y.r, "shift"); DEC(RH); x.r := RH-1
  END
END Shift;

PROCEDURE ADC*(VAR x, y: Item);
BEGIN load(x); load(y); Put0(Add+U, x.r, x.r, y.r, "ADC"); DEC(RH)
END ADC;

PROCEDURE SBC*(VAR x, y: Item);
BEGIN load(x); load(y); Put0(Sub+U, x.r, x.r, y.r, "SBC"); DEC(RH)
END SBC;

PROCEDURE UML*(VAR x, y: Item);
BEGIN load(x); load(y); Put0(Mul+U, x.r, x.r, y.r, "UML"); DEC(RH)
END UML;

PROCEDURE Bit*(VAR x, y: Item);
BEGIN load(x); Put2(Ldr, x.r, x.r, 0, "Bit");
  IF y.mode = ORB.Const THEN Put1(Ror, x.r, x.r, y.a+1, ""); DEC(RH)
  ELSE load(y); Put1(Add, y.r, y.r, 1, ""); Put0(Ror, x.r, x.r, y.r, ""); DEC(RH, 2)
  END;
  SetCC(x, MI)
END Bit;

PROCEDURE Register*(VAR x: Item);
BEGIN (* x.mode = Const *)
  Put0(Mov, RH, 0, x.a MOD C4, "Register"); x.mode := Reg; x.r := RH; incR
END Register;

PROCEDURE H*(VAR x: Item);
BEGIN (* x.mode = Const *)
  Put0(Mov + U + x.a MOD 2 * V, RH, 0, 0, "H"); x.mode := Reg; x.r := RH; incR
END H;

PROCEDURE Adr*(VAR x: Item);
BEGIN
  IF x.mode IN {ORB.Var, ORB.Par, RegI} THEN loadAdr(x)
  ELSIF (x.mode = ORB.Const) & (x.type.form = ORB.Proc) THEN load(x)
  ELSIF (x.mode = ORB.Const) & (x.type.form = ORB.String) THEN loadStringAdr(x)
  ELSE ORS.Mark("not addressable")
  END
END Adr;

PROCEDURE Condition*(VAR x: Item);
BEGIN (* x.mode = Const *) SetCC(x, x.a)
END Condition;

PROCEDURE Open*(v: INTEGER);
BEGIN pc := 0; final := -1; strx := 0; tdw := 0; RH := 0; check := v # 0; version := v;
  fixorgP := 0; fixorgD := 0; fixorgT := 0; fixorgM := 0;
  IF v = 0 THEN pc := 1;
    REPEAT code[pc] := 0; INC(pc) UNTIL pc = 8
  END
END Open;

PROCEDURE SetDataSize*(dc: INTEGER);
BEGIN varx := dc
END SetDataSize;

PROCEDURE Header*;
BEGIN
  w.sl("Header ");
  entry := pc*4;
  IF version = 0 THEN code[0] := BCT - 1 + pc;  Put1a(Mov, SP, 0, StkOrg0, "")  (* RISC-0 *)
  ELSE Put1(Sub, SP, SP, 4, ""); Put2(Str, LNK, SP, 0, "")
  END
END Header;

PROCEDURE Exit*;
BEGIN (* exit code *)
  w.sl("Exit ");
  IF version = 0 THEN Put1(Mov, 0, 0, 0, ""); Put3(BR, 7, 0, "")  (* RISC-0 *)
  ELSE Put2(Ldr, LNK, SP, 0, ""); Put1(Add, SP, SP, 4, ""); Put3(BR, 7, LNK, "")
  END
END Exit;

PROCEDURE Final*;
BEGIN final := pc*4;
  IF version # 0 THEN Put1(Sub, SP, SP, 4, "Final"); Put2(Str, LNK, SP, 0, "") END
END Final;

PROCEDURE NofRefs(ftyp: SET; typ: ORB.Type): INTEGER;
VAR fld: ORB.Object; n: INTEGER;
BEGIN
  IF typ.form IN ftyp THEN n := 1
  ELSIF typ.form = ORB.Record THEN fld := typ.dsc; n := 0;
    WHILE fld # NIL DO n := NofRefs(ftyp, fld.type) + n; fld := fld.next END
  ELSIF typ.form = ORB.Array THEN n := NofRefs(ftyp, typ.base) * typ.len
  ELSE n := 0
  END;
  RETURN n
END NofRefs;

PROCEDURE FindRefs(VAR R: Files.Rider; ftyp: SET; typ: ORB.Type; off: INTEGER);
VAR fld: ORB.Object; i, s: INTEGER;
BEGIN
  IF typ.form IN ftyp THEN Files.WriteInt(R, off)
  ELSIF typ.form = ORB.Record THEN fld := typ.dsc;
    WHILE fld # NIL DO FindRefs(R, ftyp, fld.type, fld.val + off); fld := fld.next END
  ELSIF typ.form = ORB.Array THEN s := typ.base.size;
    FOR i := 0 TO typ.len-1 DO FindRefs(R, ftyp, typ.base, i*s + off) END
  END
END FindRefs;


(* -------------------------- object file writing --------------------------- *)

PROCEDURE Close*(VAR modid: ORS.Ident; key, nofent: INTEGER);
VAR
  obj:  ORB.Object;
  name: ORS.Ident;
  F:    Files.File;
  R:    Files.Rider;
  i, comsize, nofimps, nofrefs, size, tdx, base: INTEGER;
BEGIN
  nofimps := 0; comsize := 4; nofrefs := 0; tdx := varx + strx;

  obj := ORB.topScope.next;
  WHILE obj # NIL DO
    IF (obj.class = ORB.Mod) & (obj.dsc # ORB.system) THEN INC(nofimps)  (* count imports *)
    ELSIF (obj.exno # 0) & (obj.class = ORB.Const)
        & (obj.type.form = ORB.Proc) & (obj.type.nofpar = 0)
        & (obj.type.base = ORB.noType) THEN
      i := 0;  (* count commands *)
      WHILE obj.name[i] # 0X DO INC(i) END;
      i := (i+4) DIV 4 * 4; INC(comsize, i+4)
    ELSIF obj.class = ORB.Var THEN
      INC(nofrefs, NofRefs(ORB.Ptrs + ORB.Procs, obj.type))  (* count ptrs and pvrs *)
    ELSIF (obj.class = ORB.Typ) & (obj.type.form = ORB.Record) & (obj.type.typobj = obj) THEN
      (* build type descriptors *)
      i := obj.type.len;  (* heading of fixup chain of instruction pairs
                             inserted into fixorgD chain in loadTypTagAdr *)
      BuildTD(obj.type, tdw);  (* obj.type.len now used as type descriptor
                                  offset in bytes relative to tdx *)
      IF i > 0 THEN (* fix chain of instruction pairs with type descriptor address *)
        FixLinkPair(i, tdx + obj.type.len)
      END
    END;
    obj := obj.next
  END;
  size := tdx + tdw*4 + comsize + (pc + nofimps + nofent + nofrefs + 2)*4;

  ORB.MakeFileName(name, modid, ".rsc");  (* write code file *)
  F := Files.New(name);
  Files.Set(R, F, 0);
  Files.WriteString(R, modid);
  Files.WriteInt   (R, key);
  Files.Write      (R, CHR(version));
  Files.WriteInt   (R, size);

  (* Imports *)
  obj := ORB.topScope.next;
  WHILE (obj # NIL) & (obj.class = ORB.Mod) DO
    IF obj.dsc # ORB.system THEN
      Files.WriteString(R, obj(ORB.Module).orgname); Files.WriteInt(R, obj.val)
    END;
    obj := obj.next
  END;
  Files.Write(R, 0X);

  Files.WriteInt(R, varx);  (* variable space *)

  (* Strings *)
  Files.WriteInt(R, strx);
  FOR i := 0 TO strx-1 DO Files.Write(R, str[i]) END;

  (* Type descriptors *)
  Files.WriteInt(R, tdw*4);
  FOR i := 0 TO tdw-1 DO Files.WriteInt(R, td[i]) END;

  (* Program code *)
  Files.WriteInt(R, pc);  (* code len *)
  FOR i := 0 TO pc-1 DO Files.WriteInt(R, code[i]) END;

  (* Commands *)
  obj := ORB.topScope.next;
  WHILE obj # NIL DO
    IF (obj.exno # 0) & (obj.class = ORB.Const) & (obj.type.form = ORB.Proc) &
        (obj.type.nofpar = 0) & (obj.type.base = ORB.noType) THEN
      Files.WriteString(R, obj.name); Files.WriteInt(R, obj.val)
    END;
    obj := obj.next
  END;
  Files.Write(R, 0X);

  (* Exports *)
  Files.WriteInt(R, nofent); Files.WriteInt(R, entry);
  obj := ORB.topScope.next;
  WHILE obj # NIL DO  (* entries *)
    IF obj.exno # 0 THEN
      IF  obj.class = ORB.Const THEN
        IF    obj.type.form = ORB.String THEN (* convert strx to SB-relative *)
                                              Files.WriteInt(R, varx + obj.val MOD C20)
        ELSIF obj.type.form = ORB.Proc   THEN Files.WriteInt(R, obj.val)
        ELSIF obj.type.form = ORB.TProc  THEN (* dummy to preserve linear order of exno *)
                                              Files.WriteInt(R, obj.dsc.val)
        END
      ELSIF obj.class = ORB.Typ THEN
        IF obj.type.form = ORB.Record THEN Files.WriteInt(R, tdx + obj.type.len MOD C16)
        ELSIF (obj.type.form = ORB.Pointer) & ((obj.type.base.typobj = NIL)
           OR (obj.type.base.typobj.exno = 0)) THEN
          Files.WriteInt(R, tdx + obj.type.base.len MOD C16)
        END
      ELSIF obj.class = ORB.Var THEN Files.WriteInt(R, obj.val)
      END
    END;
    obj := obj.next
  END;

  (* Pointer variables *)
  obj := ORB.topScope.next;
  WHILE obj # NIL DO
    IF obj.class = ORB.Var THEN FindRefs(R, ORB.Ptrs, obj.type, obj.val) END;
    obj := obj.next
  END;
  Files.WriteInt(R, -1);

  (* Procedure variables *)
  obj := ORB.topScope.next;
  WHILE obj # NIL DO
    IF obj.class = ORB.Var THEN FindRefs(R, ORB.Procs, obj.type, obj.val) END;
    obj := obj.next
  END;
  Files.WriteInt(R, -1);

  (* Relocation lists *)
  Files.WriteInt(R, fixorgP);
  Files.WriteInt(R, fixorgD);
  Files.WriteInt(R, fixorgT);
  Files.WriteInt(R, fixorgM);
  Files.WriteInt(R, entry);
  Files.WriteInt(R, final);
  Files.Write(R, "O");
  Files.Register(F)
END Close;

BEGIN
  relmap[0] := 1;  relmap[1] := 9;  relmap[2] := 5;
  relmap[3] := 6;  relmap[4] := 14; relmap[5] := 13
END ORG.
