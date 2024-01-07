MODULE Listing;
IMPORT SYSTEM, ORS, ORB, X64, Files, w := Writer;

CONST
  RCX = X64.RCX;
  RSP = X64.RSP;

  RightCol = 70;     (* Col for code generation trace *)


TYPE
  Buffer* = RECORD
    c*: ARRAY 1024 OF CHAR;
    p*: INTEGER
  END;


VAR
  (* Listing columns *)
  Binary*:  Buffer;
  Inst*:    Buffer;
  Args*:    Buffer;
  Comment*: Buffer;

  Sourcefile: Files.File;
  Source:     Files.Rider;
  SourceBol:  INTEGER;      (* pos at beginiing of line *)
  SourceLine: INTEGER;      (* Line number *)
  Sourcechar: CHAR;


(* ----------------------------- Source display ----------------------------- *)

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
    w.l;  w.b(RightCol)
  END
END DisplaySourceToPos;

PROCEDURE StartRightCol*;
BEGIN
  IF Pos() < ORS.Pos() THEN
    DisplaySourceToPos(ORS.Pos())
  ELSE
    w.b(RightCol)
  END
END StartRightCol;

PROCEDURE CatchupSource*;
BEGIN IF Pos() < ORS.Pos() THEN DisplaySourceToPos(ORS.Pos()); w.l END
END CatchupSource;


(* ---------------------------- Buffered output ----------------------------- *)

PROCEDURE InitBuf*(VAR b: Buffer);
BEGIN b.p := 0 END InitBuf;

PROCEDURE InitBufs;
BEGIN InitBuf(Binary);  InitBuf(Inst);  InitBuf(Args);  InitBuf(Comment)
END InitBufs;

PROCEDURE WriteBuf(VAR b: Buffer; wid: INTEGER);
BEGIN
  IF b.p < LEN(b.c) THEN b.c[b.p] := 0X END;
  w.s(b.c);  w.b(wid - b.p)
END WriteBuf;

PROCEDURE c*(c: CHAR; VAR b: Buffer);
BEGIN IF b.p < LEN(b.c) THEN b.c[b.p] := c;  INC(b.p) END END c;

PROCEDURE s*(s: ARRAY OF CHAR; VAR b: Buffer);
VAR i: INTEGER;
BEGIN  i := 0;
  WHILE s[i] # 0X DO c(s[i], b); INC(i) END
END s;

PROCEDURE l*(VAR b: Buffer);
BEGIN
  IF b.p <= LEN(b.c) THEN
    IF b.p <  LEN(b.c) THEN b.c[b.p] := 0X END;
    w.sl(b.c);
    b.p := 0
  END
END l;

PROCEDURE sl*(str: ARRAY OF CHAR; VAR buf: Buffer);
BEGIN s(str, buf);  l(buf) END sl;

PROCEDURE b*(n: INTEGER; VAR buf: Buffer);
BEGIN WHILE n > 0 DO c(" ", buf);  DEC(n) END END b;

PROCEDURE sn*(s: ARRAY OF CHAR; n: INTEGER; VAR buf: Buffer);
(* n<0: spaces to left; n>0: spaces to right *)
VAR i, l, w: INTEGER;
BEGIN l := 0;
  IF n < 0 THEN w := -n ELSE w := n END;
  WHILE (l < LEN(s)) & (l < w) & (s[l] # 0X) DO INC(l) END;
  IF n < 0 THEN
    b(w-l, buf);  FOR i := 0 TO l-1 DO c(s[i], buf) END
  ELSE
    FOR i := 0 TO l-1 DO c(s[i], buf) END;  b(w-l, buf)
  END
END sn;

PROCEDURE i*(v: INTEGER; VAR b: Buffer);
BEGIN
  IF b.p < LEN(b.c) THEN
    IF v < 0 THEN c("-", b);  v := -v END;
    IF v >= 10 THEN i(v DIV 10, b); v := v MOD 10 END;
    b.c[b.p] := CHR(v + 48);
    INC(b.p)
  END
END i;

PROCEDURE hexdigit(d: INTEGER): CHAR;  VAR ch: CHAR;
BEGIN
  ASSERT(d = d MOD 16);
  IF d < 10 THEN ch := CHR(d + 48) ELSE ch := CHR(d + 55) END
RETURN ch END hexdigit;

PROCEDURE h*(v: INTEGER; VAR b: Buffer);  (* Hex in as many columns as it takes *)
VAR upper: INTEGER;
BEGIN
  IF b.p < LEN(b.c) THEN
    upper := v DIV 16 MOD 1000000000000000H;
    IF upper # 0 THEN h(upper, b); v := v MOD 16 END;
    b.c[b.p] := hexdigit(v);
    INC(b.p)
  END
END h;

PROCEDURE hn*(h, n: INTEGER; VAR b: Buffer);  (* Hex in fixed number of columns *)
BEGIN
  (*w.s("<hn("); w.i(h); w.s(", "); w.i(n); w.sl(", buf)");*)
  IF n > 1 THEN hn(h DIV 16, n-1, b) END;  c(hexdigit(h MOD 16), b)
END hn;

PROCEDURE hb(h, n: INTEGER; VAR b: Buffer);  (* n as byte count of 1, 2, 4 or 8 *)
BEGIN
  ASSERT(n IN {1, 2, 4, 8});
  IF n > 4 THEN hn(h DIV 100000000H MOD 100000000H, 8, b) END;
  IF n > 2 THEN hn(h DIV 10000H     MOD 10000H,     4, b) END;
  IF n > 1 THEN hn(h DIV 100H,                      2, b) END;
  hn(h, 2, b)
END hb;


PROCEDURE Set*(set: SET; VAR buf: Buffer);
VAR n, f, l: INTEGER; ch: CHAR;
BEGIN
  IF set = {} THEN s("{}", buf)
  ELSE
    ch := "{";  n := 0;  f := -1;  l := -1;
    FOR n := 0 TO 63 DO
      IF n IN set THEN
        IF f < 0 THEN
          f := n; l := n
        ELSIF n = l + 1 THEN
          l := n
        ELSE (* n > l + 1 *)
          c(ch, buf);  ch := ",";
          IF f = l THEN i(f, buf) ELSE i(f, buf);  c("-", buf);  i(l, buf) END;
          f := n;  l := n
        END
      END
    END;
    c(ch, buf);
    IF f = l THEN i(f, buf) ELSE i(f, buf);  c("-", buf);  i(l, buf) END;
    c("}", buf)
  END
END Set;


PROCEDURE Reg*(size, r: INTEGER; VAR b: Buffer); (* r is X64 reg num *)
BEGIN
  ASSERT(size IN {1, 2, 4, 8});
  IF size = 8 THEN
    IF    r =  0 THEN s("rax", b)
    ELSIF r =  1 THEN s("rcx", b)
    ELSIF r =  2 THEN s("rdx", b)
    ELSIF r =  3 THEN s("rbx", b)
    ELSIF r =  4 THEN s("rsp", b)
    ELSIF r =  5 THEN s("rbp", b)
    ELSIF r =  6 THEN s("rsi", b)
    ELSIF r =  7 THEN s("rdi", b)
    ELSIF r =  8 THEN s("r8", b)
    ELSIF r =  9 THEN s("r9", b)
    ELSIF r = 10 THEN s("r10", b)
    ELSIF r = 11 THEN s("r11", b)
    ELSIF r = 12 THEN s("r12", b)
    ELSIF r = 13 THEN s("r13", b)
    ELSIF r = 14 THEN s("r14", b)
    ELSIF r = 15 THEN s("r15", b)
    ELSE
      w.s("** r "); w.i(r);  w.sl(" **");
      WriteBuf(Binary,  35);
      WriteBuf(Inst,     7);
      WriteBuf(Args,    26);
      WriteBuf(Comment,  0); w.l;
      ASSERT(FALSE)
    END
  ELSIF size = 4 THEN
    IF    r =  0 THEN s("eax", b)
    ELSIF r =  1 THEN s("ecx", b)
    ELSIF r =  2 THEN s("edx", b)
    ELSIF r =  3 THEN s("ebx", b)
    ELSIF r =  4 THEN s("esp", b)
    ELSIF r =  5 THEN s("ebp", b)
    ELSIF r =  6 THEN s("esi", b)
    ELSIF r =  7 THEN s("edi", b)
    ELSIF r =  8 THEN s("r8d", b)
    ELSIF r =  9 THEN s("r9d", b)
    ELSIF r = 10 THEN s("r10d", b)
    ELSIF r = 11 THEN s("r11d", b)
    ELSIF r = 12 THEN s("r12d", b)
    ELSIF r = 13 THEN s("r13d", b)
    ELSIF r = 14 THEN s("r14d", b)
    ELSIF r = 15 THEN s("r15d", b)
    ELSE ASSERT(FALSE)
    END
  ELSIF size = 2 THEN
    IF    r =  0 THEN s("ax", b)
    ELSIF r =  1 THEN s("cx", b)
    ELSIF r =  2 THEN s("dx", b)
    ELSIF r =  3 THEN s("bx", b)
    ELSIF r =  4 THEN s("sp", b)
    ELSIF r =  5 THEN s("bp", b)
    ELSIF r =  6 THEN s("si", b)
    ELSIF r =  7 THEN s("di", b)
    ELSIF r =  8 THEN s("r8w", b)
    ELSIF r =  9 THEN s("r9w", b)
    ELSIF r = 10 THEN s("r10w", b)
    ELSIF r = 11 THEN s("r11w", b)
    ELSIF r = 12 THEN s("r12w", b)
    ELSIF r = 13 THEN s("r13w", b)
    ELSIF r = 14 THEN s("r14w", b)
    ELSIF r = 15 THEN s("r15w", b)
    ELSE ASSERT(FALSE)
    END
  ELSIF size = 1 THEN
    IF    r =  0 THEN s("al", b)
    ELSIF r =  1 THEN s("cl", b)
    ELSIF r =  2 THEN s("dl", b)
    ELSIF r =  3 THEN s("bl", b)
    ELSIF r =  4 THEN s("spl", b)
    ELSIF r =  5 THEN s("bpl", b)
    ELSIF r =  6 THEN s("sil", b)
    ELSIF r =  7 THEN s("dil", b)
    ELSIF r =  8 THEN s("r8l", b)
    ELSIF r =  9 THEN s("r9l", b)
    ELSIF r = 10 THEN s("r10l", b)
    ELSIF r = 11 THEN s("r11l", b)
    ELSIF r = 12 THEN s("r12l", b)
    ELSIF r = 13 THEN s("r13l", b)
    ELSIF r = 14 THEN s("r14l", b)
    ELSIF r = 15 THEN s("r15l", b)
    ELSIF r = 16 THEN s("ah", b)    ELSIF r = 17 THEN s("ch", b)
    ELSIF r = 18 THEN s("dh", b)    ELSIF r = 19 THEN s("bh", b)
    ELSE ASSERT(FALSE)
    END
  ELSE ASSERT(FALSE)
  END
END Reg;


PROCEDURE AluOp(op: INTEGER; VAR b: Buffer);
BEGIN
  IF    op = 0 THEN s("add", b)  ELSIF op = 1 THEN s("or",  b)
  ELSIF op = 2 THEN s("adc", b)  ELSIF op = 3 THEN s("sbb", b)
  ELSIF op = 4 THEN s("and", b)  ELSIF op = 5 THEN s("sub", b)
  ELSIF op = 6 THEN s("xor", b)  ELSIF op = 7 THEN s("cmp", b)
  END
END AluOp;

PROCEDURE BaseIndexScaleDisp(
    indirect: BOOLEAN;
    otherreg, regsize, base, index, scale, disp, mode: INTEGER;
    VAR b: Buffer
);
BEGIN
  ASSERT(regsize IN {1, 2, 4, 8});
  IF indirect THEN
    IF otherreg < 0 THEN
      IF    regsize = 1 THEN s("byte",  b)  ELSIF regsize = 2 THEN s("word",  b)
      ELSIF regsize = 4 THEN s("dword", b)  ELSIF regsize = 8 THEN s("qword", b) END
    END;
    c("[", b);
    IF    mode = X64.Code   THEN s("Code+", b); h(disp, b);  c("H", b)
    ELSIF mode = X64.String THEN s("Str+", b);  h(disp, b);  c("H", b)
    ELSIF mode = X64.Global THEN s("VAR+", b);  h(disp, b);  c("H", b)
    ELSIF (mode MOD 256) = X64.Import THEN
      s("Imp:", b); i(mode DIV 1000000H MOD 10000H, b);
      c(".", b);    i(mode DIV 100H     MOD 10000H, b)
    ELSE
      IF disp >= 0 THEN h(disp, b) ELSE c("-", b); h(-disp, b) END;
      c("H", b)
    END;
    IF base >= 0    THEN c("+", b); Reg(8, base, b) END;
    IF index >= 0   THEN
      c("+", b);  Reg(8, index, b);
      IF    scale = 1 THEN s("*2", b)
      ELSIF scale = 2 THEN s("*4", b)
      ELSIF scale = 3 THEN s("*8", b)
      END
    END;
    c("]", b)
  ELSE
    ASSERT(base >= 0);  Reg(regsize, base, b)
  END
END BaseIndexScaleDisp;

PROCEDURE CondOp*(cond: INTEGER; VAR b: Buffer);
BEGIN ASSERT(cond > 1);  (* 0/1 = never/always *)
  cond := cond MOD 16;
  IF    cond =  0 THEN s("o ", b)
  ELSIF cond =  1 THEN s("no", b)
  ELSIF cond =  2 THEN s("c ", b)
  ELSIF cond =  3 THEN s("nc", b)
  ELSIF cond =  4 THEN s("z ", b)
  ELSIF cond =  5 THEN s("nz", b)
  ELSIF cond =  6 THEN s("be", b)
  ELSIF cond =  7 THEN s("a ", b)
  ELSIF cond =  8 THEN s("s ", b)
  ELSIF cond =  9 THEN s("ns", b)
  ELSIF cond = 10 THEN s("pe", b)
  ELSIF cond = 11 THEN s("po", b)
  ELSIF cond = 12 THEN s("l ", b)
  ELSIF cond = 13 THEN s("ge", b)
  ELSIF cond = 14 THEN s("le", b)
  ELSIF cond = 15 THEN s("g ", b)
  END
END CondOp;


PROCEDURE AddHex(size, val: INTEGER);
BEGIN
  ASSERT(size IN {1, 2, 4, 8});
  hn(val, size * 2, Binary);
  c(" ", Binary);
END AddHex;

PROCEDURE GetUnsigned(size: INTEGER; VAR adr, val: INTEGER);
BEGIN
  ASSERT(size IN {1, 2, 4, 8});
  val := X64.Peek(adr, size, FALSE);
  AddHex(size, val);
  INC(adr, size)
END GetUnsigned;

PROCEDURE GetSigned(size: INTEGER; VAR adr, val: INTEGER);
BEGIN
  ASSERT(size IN {1, 2, 4, 8});
  val := X64.Peek(adr, size, TRUE);
  AddHex(size, val);
  INC(adr, size)
END GetSigned;

PROCEDURE DisSIB(  (* Returns X64 reg nums in range 0-7 *)
  VAR pc, base, index, scale: INTEGER;
  VAR buf: Buffer);
BEGIN
  AddHex(1, X64.Text[pc]);
  scale := X64.Text[pc] DIV 64;
  base  := X64.Text[pc] MOD 8;
  IF X64.Text[pc] DIV 8 MOD 8 = 4 THEN
    index := -1; scale := 0
  ELSE
    index := X64.Text[pc] DIV 8 MOD 8;
  END;
  INC(pc)
END DisSIB;

PROCEDURE DisModRegRm(
  VAR pc, reg, base, index, disp, scale: INTEGER;
  size: INTEGER;
  VAR indirect: BOOLEAN;
  VAR buf: Buffer);
VAR mode, rm: INTEGER;
BEGIN
  AddHex(1, X64.Text[pc]);
  mode     := X64.Text[pc] DIV 64;
  reg      := X64.Text[pc] DIV 8 MOD 8;
  rm       := X64.Text[pc] MOD 8;
  scale    := 0;
  index    := -1;
  base     := -1;
  disp     := 0;
  indirect := mode < 3;
  INC(pc);
  IF mode = 0 THEN
    IF rm = 4 THEN (* sib *)
      DisSIB(pc, base, index, scale, buf);
      IF base = X64.RBP THEN base := -1; GetSigned(4, pc, disp) END;
    ELSIF rm = 5 THEN (* disp32 *)
      GetSigned(4, pc, disp); INC(disp, pc); (* Code relative *)
    ELSE base := rm
    END
  ELSIF mode = 1 THEN
    IF rm = 4 THEN (* sib + disp8 *)
      DisSIB(pc, base, index, scale, buf)
    ELSE base := rm
    END;
    GetSigned(1, pc, disp)
  ELSIF mode = 2 THEN
    IF rm = 4 THEN (* sib + disp32 *)
      DisSIB(pc, base, index, scale, buf);
      IF base < 0 THEN INC(disp, pc) END; (* Code relative *)
    ELSE base := rm
    END;
    GetSigned(4, pc, disp)
  ELSE  (* Mode = 3 *)
    IF (size = 1) & (rm >= 4) THEN (* ah/bh/ch/dh *)
      IF    rm = 4 THEN w.s(" <ah> ")
      ELSIF rm = 5 THEN w.s(" <ch> ")
      ELSIF rm = 5 THEN w.s(" <dh> ")
      ELSIF rm = 5 THEN w.s(" <bh> ")
      END;
      base := 16 + rm-4
    ELSE
      base := rm
    END
  END
END DisModRegRm;


PROCEDURE DisassembleInstruction*(
  VAR pc:    INTEGER;
  itemmode:  INTEGER;
  comment:   ARRAY OF CHAR);
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
  regsize  := 4;      dispsize  := 0;
  disp     := 0;      mode      := 0;      scale    := 0;
  indirect := FALSE;  tofirst   := FALSE;

  hn(pc, 6, Binary);  s(":  ", Binary);

  IF X64.Text[pc] = 66H THEN
    AddHex(1, X64.Text[pc]);
    regsize := 2;
    INC(pc)
  END;

  IF X64.Text[pc] DIV 16 = 4 THEN (* REX prefix *)
    AddHex(1, X64.Text[pc]);
    IF X64.Text[pc] DIV 8 MOD 2 = 1 THEN regsize := 8 END;
    reghigh   := X64.Text[pc] DIV 4 MOD 2 = 1;
    indexhigh := X64.Text[pc] DIV 2 MOD 2 = 1;
    basehigh  := X64.Text[pc]       MOD 2 = 1;
    INC(pc)
  END;

  opcode := X64.Text[pc];  INC(pc);  AddHex(1, opcode);

  IF (opcode < 64) & (opcode MOD 8 < 6) THEN (* alu op *)

    AluOp(opcode DIV 8, Inst);
    IF ~ODD(opcode) THEN regsize := 1 END;
    tofirst := opcode MOD 4 >= 2;
    IF opcode MOD 8 >= 4 THEN (* rax/al Immediate *)
      Reg(regsize, 0, Args);  (* al/ax/eax/eax *)
      IF regsize < 8 THEN
        GetSigned(regsize, pc, disp)
      ELSE
        GetSigned(4, pc, disp)
      END;
    ELSE (* modregrm *)
      DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect, Args);
      IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
      IF indexhigh & (index < 8) THEN INC(index, 8) END;
      IF basehigh  & (base  < 8) THEN INC(base,  8) END;
      IF tofirst THEN
        Reg(regsize, reg, Args);
        c(",", Args);
        BaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp, itemmode, Args)
      ELSE
        BaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp, itemmode, Args);
        s(",", Args);
        Reg(regsize, reg, Args);
      END
    END

  ELSIF (opcode DIV 32 = 1) & (opcode MOD 8 = 6) THEN

    s("seg", Inst); s("; (ignored) ", Comment);
    opcode := opcode DIV 8;
    IF    opcode = 4 THEN s("es", Args)  (* 26 *)
    ELSIF opcode = 5 THEN s("cs", Args)  (* 2E *)
    ELSIF opcode = 6 THEN s("ss", Args)  (* 36 *)
    ELSIF opcode = 7 THEN s("ds", Args)  (* 2E *)
    END;

  ELSIF (opcode >= 50H) & (opcode <= 57H) THEN

    s("push", Inst);
    reg := opcode MOD 8;
    IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
    Reg(8, reg, Args)

  ELSIF (opcode >= 58H) & (opcode <= 5FH) THEN

    s("pop", Inst);
    reg := opcode MOD 8;
    IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
    Reg(8, reg, Args)

  ELSIF (opcode = 68H) OR (opcode = 6AH) THEN

    IF opcode = 6AH THEN regsize := 1 END;
    s("push", Inst);
    GetSigned(regsize, pc, disp);
    h(disp, Args);  s("H", Args)

  ELSIF (opcode = 69H) OR (opcode = 6BH) THEN
    IF    opcode = 6BH THEN dispsize := 1
    ELSIF regsize >= 4 THEN dispsize := 4
    ELSE                    dispsize := 2
    END;
    s("imul", Inst);
    DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect, Args);
    IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    Reg(regsize, reg, Args);
    s(",", Args);
    BaseIndexScaleDisp(indirect, -1, regsize, base, index, scale, disp, itemmode, Args);
    s(",", Args);
    GetSigned(dispsize, pc, disp);  h(disp, Args); s("H", Args)

  ELSIF (opcode >= 70H) & (opcode <= 7FH) THEN  (* 8 bit conditional jump *)

    s("j", Inst);  CondOp(opcode, Inst);
    GetSigned(1, pc, disp);
    h(pc + disp, Args);
    s(" (disp ", Args);
    IF disp < 0 THEN s("-", Args);  disp := -disp ELSE s("+", Args) END;
    i(disp, Args);
    c(")", Args)

  ELSIF (opcode >= 80H) & (opcode <= 83H) THEN (* alu with immediate op *)

    IF opcode = 81H THEN dispsize := 4 ELSE dispsize := 1 END;
    DisModRegRm(pc, opcode, base, index, disp, scale, regsize, indirect, Args);
    AluOp(opcode, Inst);
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    BaseIndexScaleDisp(indirect, -1, regsize, base, index, scale, disp, itemmode, Args);
    s(",", Args);
    GetSigned(dispsize, pc, disp);  h(disp, Args); s("H", Args)

  ELSIF (opcode = 84H) OR (opcode = 85H) THEN

    s("test", Inst);
    IF opcode = 84H THEN regsize := 1 END;
    DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect, Args);
    IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    Reg(regsize, reg, Args);
    s(",", Args);
    BaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp, itemmode, Args)

  ELSIF (opcode >= 86H) & (opcode <= 87H) THEN (* xchg *)

    s("xchg", Inst);
    IF ~ODD(opcode) THEN regsize := 1 END;
    DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect, Args);
    IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    BaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp, itemmode, Args);
    s(",", Args);
    Reg(regsize, reg, Args);

  ELSIF (opcode >= 88H) & (opcode <= 8BH) THEN (* mov *)

    s("mov", Inst);
    IF ~ODD(opcode) THEN regsize := 1 END;
    tofirst := opcode >= 8AH;
    DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect, Args);
    IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    IF tofirst THEN
      Reg(regsize, reg, Args);
      s(",", Args);
      BaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp, itemmode, Args)
    ELSE
      BaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp, itemmode, Args);
      s(",", Args);
      Reg(regsize, reg, Args);
    END

  ELSIF opcode = 08DH THEN (* load effective address *)

    s("lea", Inst);
    DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect, Args);
    IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    Reg(regsize, reg, Args);
    s(",", Args);
    BaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp, itemmode, Args)

  ELSIF opcode = 0A4H THEN

    s("movsb", Inst);

  ELSIF opcode = 0A5H THEN

    IF    regsize = 2 THEN s("movsw", Inst)
    ELSIF regsize = 8 THEN s("movsq", Inst)
    ELSE                   s("movsd", Inst)
    END

  ELSIF opcode = 0ACH THEN

    s("lodsb", Inst)

  ELSIF opcode = 0ADH THEN

    IF    regsize = 2 THEN s("lodsw", Inst)
    ELSIF regsize = 8 THEN s("lodsq", Inst)
    ELSE                   s("lodsd", Inst)
    END

  ELSIF opcode DIV 16 = 0BH THEN  (* Move immediate to register *)

    s("mov", Inst);
    base := opcode MOD 8;
    IF basehigh THEN INC(base, 8) END;
    IF opcode < 0B8H THEN regsize := 1 END;
    Reg(regsize, base, Args);
    s(",", Args);
    (*IF regsize < 1 THEN GetUnsigned(regsize, pc, disp) ELSE GetUnsigned(4, pc, disp) END;*)
    GetUnsigned(regsize, pc, disp);
    h(disp, Args); s("H", Args)

  ELSIF (opcode >= 0C0H) & (opcode <= 0C1H) THEN (* group 2 alu with immediate op *)

    IF opcode = 80H THEN regsize := 1 END;
    DisModRegRm(pc, opcode, base, index, disp, scale, regsize, indirect, Args);
    IF    opcode = 0 THEN s("rol", Inst)
    ELSIF opcode = 1 THEN s("ror", Inst)
    ELSIF opcode = 2 THEN s("rcl", Inst)
    ELSIF opcode = 3 THEN s("rcr", Inst)
    ELSIF opcode = 4 THEN s("shl", Inst)
    ELSIF opcode = 5 THEN s("shr", Inst)
    ELSIF opcode = 6 THEN s("sal", Inst)
    ELSIF opcode = 7 THEN s("sar", Inst)
    END;
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    BaseIndexScaleDisp(indirect, -1, regsize, base, index, scale, disp, itemmode, Args);
    s(",", Args);
    GetSigned(1, pc, disp);  h(disp, Args); s("H", Args)

  ELSIF opcode = 0C2H THEN

    s("ret", Inst);  GetUnsigned(2, pc, disp);
    h(disp, Args); s("H", Args)

  ELSIF opcode = 0C3H THEN

    s("ret", Inst)

  ELSIF (opcode = 0C6H) OR (opcode = 0C7H) THEN (* Move immediate to modregrm *)

    IF opcode = 0C6H THEN regsize := 1 END;
    s("mov", Inst);
    DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect, Args);
    ASSERT(reg = 0);
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    BaseIndexScaleDisp(indirect, -1, regsize, base, index, scale, disp, itemmode, Args);
    s(",", Args);
    IF regsize < 8 THEN
      GetUnsigned(regsize, pc, disp)
    ELSE
      GetUnsigned(4, pc, disp)
    END;
    h(disp, Args); s("H", Args)

  ELSIF opcode = 0C9H THEN

    s("leave", Inst)

  ELSIF (opcode >= 0D0H) & (opcode <= 0D3H) THEN

    IF opcode MOD 2 = 0 THEN regsize := 1 END;
    IF opcode < 0D2H THEN reg := -1 ELSE reg := RCX END;
    DisModRegRm(pc, opcode, base, index, disp, scale, regsize, indirect, Args);
    IF    opcode = 0 THEN s("rol", Inst)
    ELSIF opcode = 1 THEN s("ror", Inst)
    ELSIF opcode = 2 THEN s("rcl", Inst)
    ELSIF opcode = 3 THEN s("rcr", Inst)
    ELSIF opcode = 4 THEN s("shl", Inst)
    ELSIF opcode = 5 THEN s("shr", Inst)
    ELSIF opcode = 6 THEN s("sal", Inst)
    ELSIF opcode = 7 THEN s("sar", Inst)
    END;
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    BaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp, itemmode, Args);
    IF reg < 0 THEN s(",1", Args) ELSE s(",cl", Args) END;

  ELSIF (opcode >= 0E0H) & (opcode <= 0E2H) THEN

    IF    opcode = 0E0H THEN s("loopne", Inst)
    ELSIF opcode = 0E1H THEN s("loope",  Inst)
    ELSE                     s("loop",   Inst)
    END;
    GetSigned(1, pc, disp);
    h(pc + disp, Args);
    s(" (disp ", Args);
    IF disp < 0 THEN s("-", Args);  disp := -disp ELSE s("+", Args) END;
    i(disp, Args);
    c(")", Args)

  ELSIF opcode = 0E8H THEN

    s("call", Inst);
    s("$", Args);
    GetSigned(4, pc, disp);
    h(pc + disp, Args);
    s(" (disp ", Args);
    IF disp < 0 THEN s("-", Args);  disp := -disp ELSE s("+", Args) END;
    i(disp, Args);
    c(")", Args)

  ELSIF (opcode = 0E9H) OR (opcode = 0EBH) THEN

    s("jmp", Inst);
    IF opcode = 0E9H THEN
      GetSigned(4, pc, disp)
    ELSE
      GetSigned(1, pc, disp)
    END;
    h(pc + disp, Args);
    s(" (disp ", Args);
    IF disp < 0 THEN s("-", Args);  disp := -disp ELSE s("+", Args) END;
    i(disp, Args);
    c(")", Args)

  ELSIF opcode = 0F3H THEN

    s("rep", Inst);

  ELSIF (opcode = 0F6H) OR (opcode = 0F7H) THEN

    IF opcode = 0F6H THEN regsize := 1 END;
    DisModRegRm(pc, opcode, base, index, disp, scale, regsize, indirect, Args);
    IF    opcode < 2 THEN s("test", Inst)
    ELSIF opcode = 2 THEN s("not", Inst)
    ELSIF opcode = 3 THEN s("neg", Inst)
    ELSIF opcode = 4 THEN s("mul", Inst)
    ELSIF opcode = 5 THEN s("imul", Inst)
    ELSIF opcode = 6 THEN s("div", Inst)
    ELSIF opcode = 7 THEN s("idiv", Inst)
    ELSE
      s("??", Args)
    END;
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;
    BaseIndexScaleDisp(indirect, -1, regsize, base, index, scale, disp, itemmode, Args)

  ELSIF opcode = 0FFH THEN  (* Group 5 extensions to primary opcode map *)

    DisModRegRm(pc, opcode, base, index, disp, scale, regsize, indirect, Args);
    IF indexhigh & (index < 8) THEN INC(index, 8) END;
    IF basehigh  & (base  < 8) THEN INC(base,  8) END;

    IF    opcode = 0 THEN s("inc", Inst)
    ELSIF opcode = 1 THEN s("dec", Inst)
    ELSIF opcode = 2 THEN s("call", Inst); regsize := 8
    ELSIF opcode = 6 THEN s("push", Inst); IF regsize < 4 THEN regsize := 2 ELSE regsize := 8 END;
    ELSE                  s("??5", Inst); END;
    BaseIndexScaleDisp(indirect, -1, regsize, base, index, scale, disp, itemmode, Args)

  ELSIF opcode = 0FH THEN  (* Secondary opcode map *)

    opcode := X64.Text[pc];  INC(pc);  AddHex(1, opcode);

    IF (opcode = 0B6H) OR (opcode = 0B7H)
    OR (opcode = 0BEH) OR (opcode = 0BFH) THEN  (* movsx/movzx *)
      IF    opcode = 0B6H THEN s("movzx", Inst);  regsize := 1
      ELSIF opcode = 0B7H THEN s("movzx", Inst);  regsize := 2
      ELSIF opcode = 0BEH THEN s("movsx", Inst);  regsize := 1
      ELSIF opcode = 0BFH THEN s("movsx", Inst);  regsize := 2
      END;
      DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect, Args);
      IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
      IF indexhigh & (index < 8) THEN INC(index, 8) END;
      IF basehigh  & (base  < 8) THEN INC(base,  8) END;
      Reg(8, reg, Args);  s(",", Args);
      BaseIndexScaleDisp(indirect, -1, regsize, base, index, scale, disp, itemmode, Args)

    ELSIF (opcode >= 40H) & (opcode <= 4FH) THEN  (* Conditional move *)

      s("cmov", Inst);  CondOp(opcode, Inst);
      DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect, Args);
      IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
      IF indexhigh & (index < 8) THEN INC(index, 8) END;
      IF basehigh  & (base  < 8) THEN INC(base,  8) END;
      Reg(8, reg, Args);  s(",", Args);
      IF (base >= 0) & (base <= 15) THEN
        BaseIndexScaleDisp(indirect, -1, regsize, base, index, scale, disp, itemmode, Args)
      ELSE
        c("*", Args); i(base, Args); c("*", Args)
      END

    ELSIF (opcode >= 80H) & (opcode <= 8FH) THEN  (* 32 bit conditional jump *)

      c("j", Inst);  CondOp(opcode, Inst);
      GetSigned(4, pc, disp);
      h(pc + disp, Args);
      s(" (disp ", Args);
      IF disp < 0 THEN s("-", Args);  disp := -disp ELSE s("+", Args) END;
      i(disp, Args);
      c(")", Args)

    ELSIF (opcode >= 90H) & (opcode <= 9FH) THEN  (* SetCC *)

      s("set", Inst);  CondOp(opcode, Inst);  s(" ", Inst);
      DisModRegRm(pc, reg, base, index, disp, scale, 1, indirect, Args);
      IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
      IF indexhigh & (index < 8) THEN INC(index, 8) END;
      IF basehigh  & (base  < 8) THEN INC(base,  8) END;
      BaseIndexScaleDisp(indirect, reg, 1, base, index, scale, disp, itemmode, Args)

    ELSIF (opcode = 0A3H) OR (opcode = 0ABH)
    OR    (opcode = 0B3H) OR (opcode = 0BBH) THEN
      IF    opcode = 0A3H THEN s("bt",  Inst)
      ELSIF opcode = 0ABH THEN s("bts", Inst)
      ELSIF opcode = 0B3H THEN s("btr", Inst)
      ELSIF opcode = 0BBH THEN s("btc", Inst)
      END;
      DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect, Args);
      IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
      IF indexhigh & (index < 8) THEN INC(index, 8) END;
      IF basehigh  & (base  < 8) THEN INC(base,  8) END;
      BaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp, itemmode, Args);
      s(",", Args);  Reg(8, reg, Args)

    ELSIF opcode = 0AFH THEN

      s("imul", Inst);
      DisModRegRm(pc, reg, base, index, disp, scale, regsize, indirect, Args);
      IF reghigh   & (reg   < 8) THEN INC(reg,   8) END;
      IF indexhigh & (index < 8) THEN INC(index, 8) END;
      IF basehigh  & (base  < 8) THEN INC(base,  8) END;
      Reg(8, reg, Args);  s(",", Args);
      BaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp, itemmode, Args)

    ELSIF opcode = 0BAH THEN

      DisModRegRm(pc, opcode, base, index, disp, scale, regsize, indirect, Args);
      IF indexhigh & (index < 8) THEN INC(index, 8) END;
      IF basehigh  & (base  < 8) THEN INC(base,  8) END;
      ASSERT(opcode > 3);
      IF    opcode = 4 THEN s("bt",  Inst)
      ELSIF opcode = 5 THEN s("bts", Inst)
      ELSIF opcode = 6 THEN s("btr", Inst)
      ELSIF opcode = 7 THEN s("btc", Inst)
      END;
      BaseIndexScaleDisp(indirect, reg, regsize, base, index, scale, disp, itemmode, Args);
      c(",", Args);
      GetUnsigned(8, pc, disp);
      h(disp, Args); c("H", Args)

    ELSE
      s("??", Inst)
    END

  ELSE
    s("unknown", Inst)
  END;

  IF comment # "" THEN s("; ", Comment);  s(comment, Comment) END;

  IF ~Source.eof & (Pos() < ORS.Pos()) THEN
    DisplaySourceToPos(ORS.Pos());  w.l
  END;
  WriteBuf(Binary,  37);
  WriteBuf(Inst,     7);
  WriteBuf(Args,    26);
  WriteBuf(Comment,  0); w.l;
  InitBufs
END DisassembleInstruction;


PROCEDURE DisassembleString*(VAR pc: INTEGER);
(* Dissassemble 'db' for up to 8 bytes of string per line *)
VAR lim, ch, state: INTEGER;  (* state: 0 init, 1 string, 2 numeric *)
BEGIN
  IF ~Source.eof & (Pos() < ORS.Pos()) THEN
    DisplaySourceToPos(ORS.Pos());  w.l
  END;
  ASSERT(pc < X64.PC);
  WHILE pc < X64.PC DO
    hn(pc, 6, Binary);  s(":  ", Binary);
    lim := pc + 8;  IF lim > X64.PC THEN lim := X64.PC END;
    s("db", Inst);
    state := 0;  (* init *)
    WHILE pc < lim DO
      ch := X64.Text[pc];  INC(pc);  AddHex(1, ch);
      IF (ch >= 20H) & (ch < 7FH) THEN
        IF state # 1 THEN
          IF state # 0 THEN c(",", Args) END;
          c(22X, Args);
          state := 1
        END;
        c(CHR(ch), Args)
      ELSE
        IF state # 2 THEN
          IF state = 1 THEN c(22X, Args) END;
          IF state > 0 THEN c(",", Args) END
        END;
        i(ch, Args);
        state := 2
      END;
    END;
    IF state = 1 THEN c(22X, Args) END;
    WriteBuf(Binary,  35);
    WriteBuf(Inst,     7);
    WriteBuf(Args,    26); w.l;
    InitBufs
  END
END DisassembleString;


PROCEDURE DisassembleInt*(VAR pc: INTEGER; comment: ARRAY OF CHAR);
VAR v: INTEGER;
BEGIN
  IF ~Source.eof & (Pos() < ORS.Pos()) THEN
    DisplaySourceToPos(ORS.Pos());  w.l
  END;
  ASSERT(pc + 8 <= X64.PC);
  hn(pc, 6, Binary);  s(":  ", Binary);
  s("dq", Inst);
  SYSTEM.GET(SYSTEM.ADR(X64.Text[pc]), v);  INC(pc, 8);
  hn(v, 16, Binary);
  i(v, Args);
  IF comment # "" THEN s("; ", Comment);  s(comment, Comment) END;
  WriteBuf(Binary,  35);
  WriteBuf(Inst,     7);
  WriteBuf(Args,    26);
  WriteBuf(Comment,  0); w.l;
  InitBufs
END DisassembleInt;


(* -------------------------------------------------------------------------- *)


PROCEDURE Bool*(b: BOOLEAN; VAR buf: Buffer);
BEGIN IF b THEN s("TRUE", buf) ELSE s("FALSE", buf) END END Bool;

PROCEDURE Form*(form: INTEGER; VAR buf: Buffer);
BEGIN
  IF    form = ORB.Byte    THEN s("Byte",    buf)
  ELSIF form = ORB.Bool    THEN s("Bool",    buf)
  ELSIF form = ORB.Char    THEN s("Char",    buf)
  ELSIF form = ORB.Int8    THEN s("Int8",    buf)
  ELSIF form = ORB.Int16   THEN s("Int16",   buf)
  ELSIF form = ORB.Int32   THEN s("Int32",   buf)
  ELSIF form = ORB.Int64   THEN s("Int64",   buf)
  ELSIF form = ORB.Card16  THEN s("Card16",  buf)
  ELSIF form = ORB.Card32  THEN s("Card32",  buf)
  ELSIF form = ORB.Real    THEN s("Real",    buf)
  ELSIF form = ORB.Set     THEN s("Set",     buf)
  ELSIF form = ORB.Pointer THEN s("Pointer", buf)
  ELSIF form = ORB.NilTyp  THEN s("NilTyp",  buf)
  ELSIF form = ORB.NoTyp   THEN s("NoTyp",   buf)
  ELSIF form = ORB.Proc    THEN s("Proc",    buf)
  ELSIF form = ORB.String  THEN s("String",  buf)
  ELSIF form = ORB.Array   THEN s("Array",   buf)
  ELSIF form = ORB.Record  THEN s("Record",  buf)
  ELSE ASSERT(FALSE)
  END
END Form;

PROCEDURE Class*(class: INTEGER; VAR buf: Buffer);
BEGIN
  IF    class = ORB.Head  THEN s("Head",  buf)
  ELSIF class = ORB.Const THEN s("Const", buf)
  ELSIF class = ORB.Var   THEN s("Var",   buf)
  ELSIF class = ORB.Par   THEN s("Par",   buf)
  ELSIF class = ORB.Fld   THEN s("Fld",   buf)
  ELSIF class = ORB.Typ   THEN s("Typ",   buf)
  ELSIF class = ORB.SProc THEN s("SProc", buf)
  ELSIF class = ORB.SFunc THEN s("SFunc", buf)
  ELSIF class = ORB.Mod   THEN s("Mod",   buf)
  ELSE ASSERT(FALSE)
  END
END Class;

PROCEDURE Sym*(sym: INTEGER; VAR buf: Buffer);
BEGIN
  IF    sym = ORS.times     THEN s("*",         buf)
  ELSIF sym = ORS.rdiv      THEN s("/",         buf)
  ELSIF sym = ORS.div       THEN s("DIV",       buf)
  ELSIF sym = ORS.mod       THEN s("MOD",       buf)
  ELSIF sym = ORS.and       THEN s("&",         buf)
  ELSIF sym = ORS.plus      THEN s("+",         buf)
  ELSIF sym = ORS.minus     THEN s("-",         buf)
  ELSIF sym = ORS.or        THEN s("OR",        buf)
  ELSIF sym = ORS.eql       THEN s("=",         buf)
  ELSIF sym = ORS.neq       THEN s("#",         buf)
  ELSIF sym = ORS.lss       THEN s("<",         buf)
  ELSIF sym = ORS.leq       THEN s("<=",        buf)
  ELSIF sym = ORS.gtr       THEN s(">",         buf)
  ELSIF sym = ORS.geq       THEN s(">=",        buf)
  ELSIF sym = ORS.in        THEN s("IN",        buf)
  ELSIF sym = ORS.is        THEN s("IS",        buf)
  ELSIF sym = ORS.arrow     THEN s("^",         buf)
  ELSIF sym = ORS.period    THEN s(".",         buf)
  ELSIF sym = ORS.char      THEN s("CHAR",      buf)
  ELSIF sym = ORS.int       THEN s("int",       buf)
  ELSIF sym = ORS.real      THEN s("real",      buf)
  ELSIF sym = ORS.false     THEN s("FALSE",     buf)
  ELSIF sym = ORS.true      THEN s("TRUE",      buf)
  ELSIF sym = ORS.nil       THEN s("NIL",       buf)
  ELSIF sym = ORS.string    THEN c(22X,         buf)
  ELSIF sym = ORS.not       THEN s("~",         buf)
  ELSIF sym = ORS.lparen    THEN s("(",         buf)
  ELSIF sym = ORS.lbrak     THEN s("[",         buf)
  ELSIF sym = ORS.lbrace    THEN s("{",         buf)
  ELSIF sym = ORS.ident     THEN s("ident",     buf)
  ELSIF sym = ORS.if        THEN s("IF",        buf)
  ELSIF sym = ORS.while     THEN s("WHILE",     buf)
  ELSIF sym = ORS.repeat    THEN s("REPEAT",    buf)
  ELSIF sym = ORS.case      THEN s("CASE",      buf)
  ELSIF sym = ORS.for       THEN s("FOR",       buf)
  ELSIF sym = ORS.comma     THEN s(",",         buf)
  ELSIF sym = ORS.colon     THEN s(":",         buf)
  ELSIF sym = ORS.becomes   THEN s(":=",        buf)
  ELSIF sym = ORS.upto      THEN s("..",        buf)
  ELSIF sym = ORS.rparen    THEN s(")",         buf)
  ELSIF sym = ORS.rbrak     THEN s("]",         buf)
  ELSIF sym = ORS.rbrace    THEN s("}",         buf)
  ELSIF sym = ORS.then      THEN s("THN",       buf)
  ELSIF sym = ORS.of        THEN s("OF",        buf)
  ELSIF sym = ORS.do        THEN s("DO",        buf)
  ELSIF sym = ORS.to        THEN s("TO",        buf)
  ELSIF sym = ORS.by        THEN s("BY",        buf)
  ELSIF sym = ORS.semicolon THEN s(";",         buf)
  ELSIF sym = ORS.end       THEN s("END",       buf)
  ELSIF sym = ORS.bar       THEN s("|",         buf)
  ELSIF sym = ORS.else      THEN s("ELSE",      buf)
  ELSIF sym = ORS.elsif     THEN s("ELSIF",     buf)
  ELSIF sym = ORS.until     THEN s("UNTIL",     buf)
  ELSIF sym = ORS.return    THEN s("RETURN",    buf)
  ELSIF sym = ORS.array     THEN s("ARRAY",     buf)
  ELSIF sym = ORS.record    THEN s("RECORD",    buf)
  ELSIF sym = ORS.pointer   THEN s("POINTER",   buf)
  ELSIF sym = ORS.const     THEN s("CONST",     buf)
  ELSIF sym = ORS.type      THEN s("TYPE",      buf)
  ELSIF sym = ORS.var       THEN s("VAR",       buf)
  ELSIF sym = ORS.procedure THEN s("PROCEDURE", buf)
  ELSIF sym = ORS.begin     THEN s("BEGIN",     buf)
  ELSIF sym = ORS.import    THEN s("IMPORT",    buf)
  ELSIF sym = ORS.module    THEN s("MODULE",    buf)
  ELSE ASSERT(FALSE)
  END
END Sym;

PROCEDURE Cond*(cond: INTEGER; VAR buf: Buffer);
BEGIN
  IF    cond = 0   THEN s("false",            buf)
  ELSIF cond = 1   THEN s("true",             buf)
  ELSIF cond = 80H THEN s("overflow",         buf)
  ELSIF cond = 81H THEN s("no overflow",      buf)
  ELSIF cond = 82H THEN s("carry",            buf)
  ELSIF cond = 83H THEN s("no carry",         buf)
  ELSIF cond = 84H THEN s("equal",            buf)
  ELSIF cond = 85H THEN s("not equal",        buf)
  ELSIF cond = 86H THEN s("not above",        buf)
  ELSIF cond = 87H THEN s("above",            buf)
  ELSIF cond = 88H THEN s("negative",         buf)
  ELSIF cond = 89H THEN s("positive",         buf)
  ELSIF cond = 8AH THEN s("parity 1",         buf)
  ELSIF cond = 8BH THEN s("parity 0",         buf)
  ELSIF cond = 8CH THEN s("less than",        buf)
  ELSIF cond = 8DH THEN s("greater or equal", buf)
  ELSIF cond = 8EH THEN s("lesser or equal",  buf)
  ELSIF cond = 8FH THEN s("greater than",     buf)
  ELSE  ASSERT(FALSE)
  END
END Cond;

PROCEDURE Mode*(mode: INTEGER; VAR buf: Buffer);
BEGIN
  IF    mode = ORB.Head   THEN s("Head",   buf)
  ELSIF mode = ORB.SProc  THEN s("SProc",  buf)
  ELSIF mode = ORB.Typ    THEN s("Typ",    buf)
  ELSIF mode = X64.Cond   THEN s("Cond",   buf)
  ELSIF mode = X64.Const  THEN s("Const",  buf)
  ELSIF mode = X64.Reg    THEN s("Reg",    buf)
  ELSIF mode = X64.Stkind THEN s("Stkind", buf)
  ELSIF mode = X64.Eadr   THEN s("Eadr",   buf)
  ELSIF mode = X64.Code   THEN s("Code",   buf)
  ELSIF mode = X64.String THEN s("String", buf)
  ELSIF mode = X64.Global THEN s("Global", buf)
  ELSIF mode = X64.Import THEN s("Import", buf)
  ELSE  ASSERT(FALSE)
  END
END Mode;

PROCEDURE Op*(op: INTEGER; VAR buf: Buffer);
BEGIN
  IF    op = X64.Plus  THEN s("Plus",  buf)
  ELSIF op = X64.Or    THEN s("Or",    buf)
  ELSIF op = X64.And   THEN s("And",   buf)
  ELSIF op = X64.Minus THEN s("Minus", buf)
  ELSIF op = X64.Xor   THEN s("Xor",   buf)
  ELSIF op = X64.Cmp   THEN s("Cmp",   buf)
  ELSE ASSERT(FALSE)
  END
END Op;


PROCEDURE Type*(t: ORB.Type; VAR buf: Buffer);
BEGIN
  IF t = NIL THEN
    s("(NIL)", buf)
  ELSE
    c("(", buf);
    IF t.ref # 0 THEN Form(t.ref, buf) ELSE Form(t.form, buf) END;
    s(", mno ", buf);  i(t.mno, buf);
    IF t.len # 0 THEN s(", len ", buf);  i(t.len, buf) END;
    s(", size ", buf);  i(t.size, buf);
    (*
    IF t.form IN {ORB.Array, ORB.Record, ORB.Pointer} THEN
      s(", base ", buf); Type(t.base)
    END;
    *)
    c(")", buf)
  END
END Type;

PROCEDURE Object*(o: ORB.Object; VAR buf: Buffer);
BEGIN
  IF o = NIL THEN s("NIL", buf) ELSE
    Class(o.class, buf);
    s(": ",  buf);  Type(o.type, buf);
    s(" '",  buf);  s(o.name, buf);
    s("' v", buf);  i(o.val,  buf);
    s(" l",  buf);  i(o.lev,  buf);
    IF o.rdo THEN s(" ro", buf) END
  END
END Object;

PROCEDURE Item*(x: X64.Item; VAR buf: Buffer);
BEGIN
  c("(", buf);  Mode(x.mode, buf);  c(" ",   buf);
  IF    x.mode = X64.Const  THEN i(x.n, buf)
  ELSIF x.mode = X64.Reg    THEN Reg(8, x.n, buf)
  ELSIF x.mode = X64.Cond   THEN Cond(x.n, buf)
  ELSIF x.mode IN {X64.Eadr..X64.Import} THEN
    IF x.type.size # 8 THEN
      IF X64.IsSigned(x.type) THEN c("s", buf) ELSE c("u", buf) END;
      IF    x.type.size = 1  THEN s("byte",  buf)
      ELSIF x.type.size = 2  THEN s("word",  buf)
      ELSIF x.type.size = 4  THEN s("dword", buf)
      ELSIF x.type.size # 8  THEN c("<", buf); i(x.type.size, buf); c(">", buf)
      END
    END;
    s(" [",  buf);
    IF    x.mode = X64.Eadr   THEN Reg(8, x.n, buf)
    ELSIF x.mode = X64.Stkind THEN s("[rsp", buf); i(x.n, buf); c("]", buf)
    ELSIF x.mode = X64.Code   THEN s("text", buf); ASSERT(x.n = 0)
    ELSIF x.mode = X64.String THEN s("string l", buf); i(x.n, buf)
    ELSIF x.mode = X64.Global THEN s("VAR",  buf);
    ELSIF x.mode = X64.Import THEN s("import(",  buf); i(x.n DIV 10000H, buf);
                                   c(",", buf); i(x.n MOD 10000H, buf); c(")", buf)
    END;
    IF (x.index >= 0) OR (x.offset # 0) THEN
      IF    x.index >= 0 THEN c("+", buf);  Reg(8, x.index, buf) END;
      IF    x.offset > 0 THEN c("+", buf);  i(x.offset, buf)
      ELSIF x.offset < 0 THEN c("-", buf);  i(-x.offset, buf)
      END
    END;
    c("]", buf);
    IF (x.mode = X64.Stkind) OR ((x.mode = X64.Eadr) & (x.n = RSP)) THEN
      s(" SPO ", buf); i(X64.SPO, buf)
    END
  END;
  IF x.readonly THEN s(", readonly", buf) END;
  IF x.type # NIL THEN s(", ", buf);  Type(x.type, buf) END;
  c(")", buf)
END Item;

(*---
PROCEDURE Disassemble*(comment: ARRAY OF CHAR);
BEGIN
(*---
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
    IF disasmpc < X64.PC THEN
---*)
  IF ORB.ListAssembly & (disasmpc < X64.PC) THEN
    DisassembleInstruction(disasmpc, comment);
    WHILE disasmpc < X64.PC DO
      DisassembleInstruction(disasmpc, "");
    END
  END
END Disassemble;
---*)


PROCEDURE Init*(sourcefn: ARRAY OF CHAR);
BEGIN
  Sourcefile := Files.Old(sourcefn);  Files.Set(Source, Sourcefile, 0);
  SourceBol  := 0;  SourceLine := 1;  (*disasmpc := 0;*)
  w.l;
  GetChar
END Init;


BEGIN
END Listing.
