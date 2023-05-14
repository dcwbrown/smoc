MODULE Texts; (*JG 21.11.90 / NW 11.7.90 / 24.12.95 / 22.11.10 / 18.11.2014 / 10.1.2019 / AP 15.9.20 Extended Oberon / DCWB 7May23 *)

IMPORT Files, Fonts, w := Writer;

CONST (*scanner symbol classes*)
  Inval*  = 0;  (*invalid symbol*)
  Name*   = 1;  (*name s (length length)*)
  String* = 2;  (*literal string s (length length)*)
  Int*    = 3;  (*integer i (decimal or hexadecimal)*)
  Real*   = 4;  (*real number x*)
  Char*   = 6;  (*special character c*)

  (* TextBlock = TextTag offset run {run} "0" length {AsciiCode}. *)
  (* run       = fnt [name] col voff length.                      *)

  TAB      = 9X;
  CR       = 0DX;
  TextTag  = 0F1X;
  replace* = 0;  (*op-codes*)
  insert*  = 1;
  delete*  = 2;
  unmark*  = 3;

TYPE
  Piece = POINTER TO PieceDesc;
  PieceDesc = RECORD
    f:      Files.File;
    offset: INTEGER;
    length: INTEGER;
    fnt:    Fonts.Font;
    col:    INTEGER;
    voff:   INTEGER;
    prev:   Piece;
    next:   Piece
  END;

  Text* = POINTER TO TextDesc;
  Notifier* = PROCEDURE (T: Text; op: INTEGER; beg, end: INTEGER);
  TextDesc* = RECORD
    length*:  INTEGER;
    changed*: BOOLEAN;
    notify*:  Notifier;
    trailer:  Piece;
    pce:      Piece;  (*cache*)
    org:      INTEGER (*cache*)
  END;

  Reader* = RECORD
    eot*:   BOOLEAN;
    fnt*:   Fonts.Font;
    col*:   INTEGER;
    voff*:  INTEGER;
    ref:    Piece;
    org:    INTEGER;
    offset: INTEGER;
    rider:  Files.Rider
  END;

  Scanner* = RECORD (Reader)
    nextCh*: CHAR;
    line*:   INTEGER;
    class*:  INTEGER;
    i*:      INTEGER;
    x*:      REAL;
    c*:      CHAR;
    length*: INTEGER;
    s*:      ARRAY 32 OF CHAR
  END;

  Buffer* = POINTER TO BufDesc;
  BufDesc* = RECORD
    length*: INTEGER;
    header:  Piece;
    last:    Piece
  END;

  Writer* = RECORD
    buf*:  Buffer;
    fnt*:  Fonts.Font;
    col*:  INTEGER;
    voff*: INTEGER;
    rider: Files.Rider
  END;

VAR
  TrailerFile: Files.File;


(* -------------------- Filing ------------------------*)

PROCEDURE Trailer(): Piece;
VAR Q: Piece;
BEGIN NEW(Q);
  Q.f := TrailerFile; Q.offset := -1; Q.length := 1; Q.fnt := NIL; Q.col := 0; Q.voff := 0; RETURN Q
END Trailer;

PROCEDURE Load*(VAR R: Files.Rider; T: Text);
VAR
  Q, q, p: Piece;
  offset:     INTEGER;
  N, fno:  INTEGER;
  bt:      BYTE;
  f:       Files.File;
  FName:   ARRAY 32 OF CHAR;
  Dict:    ARRAY 32 OF Fonts.Font;
BEGIN f := Files.Base(R); N := 1; Q := Trailer(); p := Q;
  Files.ReadInt(R, offset); Files.ReadByte(R, bt); fno := bt;
  WHILE fno # 0 DO
    IF fno = N THEN
      Files.ReadString(R, FName);
      Dict[N] := Fonts.Load(FName); INC(N)
    END;
    NEW(q); q.fnt := Dict[fno];
    Files.ReadByte(R, bt); q.col := bt;
    Files.ReadByte(R, bt); q.voff := ASR(LSL(bt, -24), 24);
    Files.ReadInt(R, q.length);
    Files.ReadByte(R, bt); fno := bt;
    q.f := f; q.offset := offset; offset := offset + q.length;
    p.next := q; q.prev := p; p := q
  END;
  p.next := Q; Q.prev := p;
  T.trailer := Q; Files.ReadInt(R, T.length); (*Files.Set(R, f, Files.Pos(R) + T.length)*)
END Load;

PROCEDURE Open*(T: Text; name: ARRAY OF CHAR);
VAR
  f:    Files.File;
  R:    Files.Rider;
  Q, q: Piece;
  tag:  CHAR;
  length:  INTEGER;
BEGIN f := Files.Old(name);
  IF f # NIL THEN
    Files.Set(R, f, 0); Files.Read(R, tag);
    IF tag = TextTag THEN Load(R, T)
    ELSE (*Ascii file*)
      length := Files.Length(f); Q := Trailer();
      NEW(q); q.fnt := Fonts.Default; q.col := 1; q.voff := 0; q.f := f; q.offset := 0; q.length := length;
      Q.next := q; q.prev := Q; q.next := Q; Q.prev := q; T.trailer := Q; T.length := length
    END
  ELSE (*create new text*)
    Q := Trailer(); Q.next := Q; Q.prev := Q; T.trailer := Q; T.length := 0
  END ;
  T.changed := FALSE; T.org := -1; T.pce := T.trailer (*init cache*)
END Open;

PROCEDURE Store*(VAR W: Files.Rider; T: Text);
VAR p, q: Piece;
    R: Files.Rider;
    offset, rlen, pos: INTEGER;
    N, n: INTEGER;
    ch: CHAR;
    Dict: ARRAY 32, 32 OF CHAR;
BEGIN pos := Files.Pos(W); Files.WriteInt(W, 0); (*place holder*)
  N := 1; p := T.trailer.next;
  WHILE p # T.trailer DO
    rlen := p.length; q := p.next;
    WHILE (q # T.trailer) & (q.fnt = p.fnt) & (q.col = p.col) & (q.voff = p.voff) DO
      rlen := rlen + q.length; q := q.next
    END;
    Dict[N] := p.fnt.name;
    n := 1;
    WHILE Dict[n] # p.fnt.name DO INC(n) END;
    Files.WriteByte(W, n);
    IF n = N THEN Files.WriteString(W, p.fnt.name); INC(N) END;
    Files.WriteByte(W, p.col); Files.WriteByte(W, p.voff); Files.WriteInt(W, rlen);
    p := q
  END;
  Files.WriteByte(W, 0); Files.WriteInt(W, T.length);
  offset := Files.Pos(W); p := T.trailer.next;
  WHILE p # T.trailer DO
    rlen := p.length; Files.Set(R, p.f, p.offset);
    WHILE rlen > 0 DO Files.Read(R, ch); Files.Write(W, ch); DEC(rlen) END ;
    p := p.next
  END ;
  Files.Set(W, Files.Base(W), pos); Files.WriteInt(W, offset); (*fixup*)
  T.changed := FALSE;
  IF T.notify # NIL THEN T.notify(T, unmark, 0, 0) END
END Store;

PROCEDURE Close*(T: Text; name: ARRAY OF CHAR);
VAR f: Files.File; w: Files.Rider;
BEGIN f := Files.New(name); Files.Set(w, f, 0);
  Files.Write(w, TextTag); Store(w, T); Files.Register(f)
END Close;

(* -------------------- Editing ----------------------- *)

PROCEDURE OpenBuf*(B: Buffer);
BEGIN NEW(B.header); (*null piece*)
  B.last := B.header; B.length := 0
END OpenBuf;

PROCEDURE FindPiece (T: Text; pos: INTEGER; VAR org: INTEGER; VAR pce: Piece);
VAR p: Piece; porg: INTEGER;
BEGIN p := T.pce; porg := T.org;
  IF pos >= porg THEN
    WHILE pos >= porg + p.length DO INC(porg, p.length); p := p.next END
  ELSE p := p.prev; DEC(porg, p.length);
    WHILE pos < porg DO p := p.prev; DEC(porg, p.length) END
  END ;
  T.pce := p; T.org := porg;  (*update cache*)
  pce := p; org := porg
END FindPiece;

PROCEDURE SplitPiece (p: Piece; offset: INTEGER; VAR pr: Piece);
VAR q: Piece;
BEGIN
  IF offset > 0 THEN NEW(q);
    q.fnt := p.fnt; q.col := p.col; q.voff := p.voff;
    q.length := p.length - offset;
    q.f := p.f; q.offset := p.offset + offset;
    p.length := offset;
    q.next := p.next; p.next := q;
    q.prev := p; q.next.prev := q;
    pr := q
  ELSE pr := p
  END
END SplitPiece;

PROCEDURE Save*(T: Text; beg, end: INTEGER; B: Buffer);
VAR p, q, qb, qe: Piece; org: INTEGER;
BEGIN
  IF end > T.length THEN end := T.length END;
  FindPiece(T, beg, org, p);
  NEW(qb); qb^ := p^;
  qb.length := qb.length - (beg - org);
  qb.offset := qb.offset + (beg - org);
  qe := qb;
  WHILE end > org + p.length DO
    org := org + p.length; p := p.next;
    NEW(q); q^ := p^; qe.next := q; q.prev := qe; qe := q
  END;
  qe.next := NIL; qe.length := qe.length - (org + p.length - end);
  B.last.next := qb; qb.prev := B.last; B.last := qe;
  B.length := B.length + (end - beg)
END Save;

PROCEDURE Copy*(SB, DB: Buffer);
VAR Q, q, p: Piece;
BEGIN p := SB.header; Q := DB.last;
  WHILE p # SB.last DO p := p.next;
    NEW(q); q^ := p^; Q.next := q; q.prev := Q; Q := q
  END;
  DB.last := Q; DB.length := DB.length + SB.length
END Copy;

PROCEDURE Insert*(T: Text; pos: INTEGER; B: Buffer);
VAR pl, pr, p, qb, qe: Piece; org, end: INTEGER;
BEGIN
  FindPiece(T, pos, org, p); SplitPiece(p, pos - org, pr);
  IF T.org >= org THEN T.org := org - p.prev.length; T.pce := p.prev END ;
  pl := pr.prev; qb := B.header.next;
  IF (qb # NIL) & (qb.f = pl.f) & (qb.offset = pl.offset + pl.length)
      & (qb.fnt = pl.fnt) & (qb.col = pl.col) & (qb.voff = pl.voff) THEN
    pl.length := pl.length + qb.length; qb := qb.next
  END;
  IF qb # NIL THEN qe := B.last;
    qb.prev := pl; pl.next := qb; qe.next := pr; pr.prev := qe
  END;
  T.length := T.length + B.length; end := pos + B.length;
  B.last := B.header; B.last.next := NIL; B.length := 0;
  T.changed := TRUE;
  IF T.notify # NIL THEN  T.notify(T, insert, pos, end) END
END Insert;

PROCEDURE Append*(T: Text; B: Buffer);
BEGIN Insert(T, T.length, B)
END Append;

PROCEDURE Delete*(T: Text; beg, end: INTEGER; B: Buffer);
VAR pb, pe, pbr, per: Piece; orgb, orge: INTEGER;
BEGIN
  IF end > T.length THEN end := T.length END;
  FindPiece(T, beg, orgb, pb); SplitPiece(pb, beg - orgb, pbr);
  FindPiece(T, end, orge, pe);
  SplitPiece(pe, end - orge, per);
  IF T.org >= orgb THEN (*adjust cache*)
    T.org := orgb - pb.prev.length; T.pce := pb.prev
  END;
  B.header.next := pbr; B.last := per.prev;
  B.last.next := NIL; B.length := end - beg;
  per.prev := pbr.prev; pbr.prev.next := per;
  T.length := T.length - B.length;
  T.changed := TRUE;
  IF T.notify # NIL THEN T.notify(T, delete, beg, end) END
END Delete;

PROCEDURE ChangeLooks*(T: Text; beg, end: INTEGER; sel: SET; fnt: Fonts.Font; col, voff: INTEGER);
VAR pb, pe, p: Piece; org: INTEGER;
BEGIN
  IF end > T.length THEN end := T.length END;
  FindPiece(T, beg, org, p); SplitPiece(p, beg - org, pb);
  FindPiece(T, end, org, p); SplitPiece(p, end - org, pe);
  p := pb;
  REPEAT
    IF 0 IN sel THEN p.fnt := fnt END;
    IF 1 IN sel THEN p.col := col END;
    IF 2 IN sel THEN p.voff := voff END;
    p := p.next
  UNTIL p = pe;
  T.changed := TRUE;
  IF T.notify # NIL THEN T.notify(T, replace, beg, end) END
END ChangeLooks;

PROCEDURE Attributes*(T: Text; pos: INTEGER; VAR fnt: Fonts.Font; VAR col, voff: INTEGER);
VAR p: Piece; org: INTEGER;
BEGIN FindPiece(T, pos, org, p); fnt := p.fnt; col := p.col; voff := p.voff
END Attributes;

(* ------------------ Access: Readers ------------------------- *)

PROCEDURE OpenReader*(VAR R: Reader; T: Text; pos: INTEGER);
VAR p: Piece; org: INTEGER;
BEGIN FindPiece(T, pos, org, p);
  R.ref := p; R.org := org; R.offset := pos - org;
  Files.Set(R.rider, p.f, p.offset + R.offset); R.eot := FALSE
END OpenReader;

PROCEDURE Read*(VAR R: Reader; VAR ch: CHAR);
BEGIN Files.Read(R.rider, ch);
  R.fnt := R.ref.fnt; R.col := R.ref.col; R.voff := R.ref.voff;
  INC(R.offset);
  IF R.offset = R.ref.length THEN
    IF R.ref.f = TrailerFile THEN R.eot := TRUE END;
    R.ref := R.ref.next; R.org := R.org + R.offset; R.offset := 0;
    Files.Set(R.rider, R.ref.f, R.ref.offset)
  END
END Read;

(*
PROCEDURE ReadUTF8*(VAR R: Reader; VAR codepoint: INTEGER);
VAR ch: CHAR;  n: INTEGER;
BEGIN
  Read(R, ch);  codepoint := ORD(ch);
  IF codepoint >= 0C0H THEN
    IF    codepoint >= 0FCH THEN codepoint := codepoint MOD 2;  n := 5
    ELSIF codepoint >= 0F8H THEN codepoint := codepoint MOD 4;  n := 4
    ELSIF codepoint >= 0F0H THEN codepoint := codepoint MOD 8;  n := 3
    ELSIF codepoint >= 0E0H THEN codepoint := codepoint MOD 16; n := 2
    ELSE                         codepoint := codepoint MOD 32; n := 1
    END;
    WHILE n > 0 DO
      codepoint := LSL(codepoint,6);  DEC(n);
      IF ~R.eot THEN Read(R, ch);
        IF ORD(ch) DIV 40H = 2 THEN INC(codepoint, ORD(ch) MOD 40H) END
      END
    END
  END
END ReadUTF8;
*)

PROCEDURE Pos*(VAR R: Reader): INTEGER;
BEGIN RETURN R.org + R.offset
END Pos;

(* ------------------ Access: Scanners (NW) ------------------------- *)

PROCEDURE OpenScanner*(VAR S: Scanner; T: Text; pos: INTEGER);
BEGIN OpenReader(S, T, pos); S.line := 0; S.nextCh := " "
END OpenScanner;

(*floating point formats:
  x = 1.m * 2^(e-127)   bit 0: sign, bits 1- 8: e, bits  9-31: m
  x = 1.m * 2^(e-1023)  bit 0: sign, bits 1-11: e, bits 12-63: m *)

PROCEDURE Ten(n: INTEGER): REAL;
VAR t, p: REAL;
BEGIN t := 1.0; p := 10.0;   (*compute 10^n *)
  WHILE n > 0 DO
    IF ODD(n) THEN t := p * t END ;
    p := p*p; n := n DIV 2
  END ;
  RETURN t
END Ten;

PROCEDURE Scan*(VAR S: Scanner);
CONST maxExp = 38; maxM = 16777216; (*2^24*)
VAR ch: CHAR;
    neg, negE, hex: BOOLEAN;
    i, j, h, d, e, n, s: INTEGER;
    x: REAL;
BEGIN ch := S.nextCh; i := 0;
  WHILE (ch = " ") OR (ch = TAB) OR (ch = CR) DO
    IF ch = CR THEN INC(S.line) END ;
    Read(S, ch)
  END ;
  IF ("A" <= ch) & (ch <= "Z") OR ("a" <= ch) & (ch <= "z") THEN (*name*)
    REPEAT S.s[i] := ch; INC(i); Read(S, ch)
    UNTIL ((ch < "0") & (ch # ".") OR ("9" < ch) & (ch < "A") OR ("Z" < ch) & (ch < "a") OR ("z" < ch)) OR (i = 31);
    S.s[i] := 0X; S.length := i; S.class := Name
  ELSIF ch = 22X THEN (*string*)
    Read(S, ch);
    WHILE (ch # 22X) & (ch >= " ") & (i # 31) DO S.s[i] := ch; INC(i); Read(S, ch) END;
    S.s[i] := 0X; S.length := i+1; Read(S, ch); S.class := String
  ELSE hex := FALSE;
    IF ch = "-" THEN neg := TRUE; Read(S, ch) ELSE neg := FALSE END ;
    IF ("0" <= ch) & (ch <= "9") THEN (*number*)
      n := ORD(ch) - 30H; h := n; Read(S, ch);
      WHILE ("0" <= ch) & (ch <= "9") OR ("A" <= ch) & (ch <= "F") DO
        IF ch <= "9" THEN d := ORD(ch) - 30H ELSE d := ORD(ch) - 37H; hex := TRUE END ;
        n := 10*n + d; h := 10H*h + d; Read(S, ch)
      END ;
      IF ch = "H" THEN (*hex integer*) Read(S, ch); S.i := h; S.class := Int  (*neg?*)
      ELSIF ch = "." THEN (*real number*)
        Read(S, ch); x := 0.0; e := 0; j := 0;
        WHILE ("0" <= ch) & (ch <= "9") DO  (*fraction*)
          h := 10*n + (ORD(ch) - 30H);
          IF h < maxM THEN n := h; INC(j) END ;
          Read(S, ch)
        END ;
        IF ch = "E" THEN (*scale factor*)
          s := 0; Read(S, ch);
          IF ch = "-" THEN negE := TRUE; Read(S, ch)
          ELSE negE := FALSE;
            IF ch = "+" THEN Read(S, ch) END
          END ;
          WHILE ("0" <= ch) & (ch <= "9") DO
            s := s*10 + ORD(ch) - 30H; Read(S, ch)
          END ;
          IF negE THEN DEC(e, s) ELSE INC(e, s) END ;
        END ;
        x := FLT(n); DEC(e, j);
        IF e < 0 THEN
          IF e >= -maxExp THEN x := x / Ten(-e) ELSE x := 0.0 END
        ELSIF e > 0 THEN
          IF e <= maxExp THEN x := Ten(e) * x ELSE x := 0.0 END
        END ;
        IF neg THEN S.x := -x ELSE S.x := x END ;
        IF hex THEN S.class := 0 ELSE S.class := Real END
      ELSE (*decimal integer*)
        IF neg THEN S.i := -n ELSE S.i := n END;
        IF hex THEN S.class := Inval ELSE S.class := Int END
      END
    ELSE (*spectal character*) S.class := Char;
      IF neg THEN S.c := "-" ELSE S.c := ch; Read(S, ch) END
    END
  END ;
  S.nextCh := ch
END Scan;

(* --------------- Access: Writers (NW) ------------------ *)

PROCEDURE OpenWriter*(VAR W: Writer);
BEGIN NEW(W.buf);
  OpenBuf(W.buf); W.fnt := Fonts.Default; W.col := 0FFFFFFH; W.voff := 0;
  Files.Set(W.rider, Files.New(""), 0)
END OpenWriter;

PROCEDURE SetFont*(VAR W: Writer; fnt: Fonts.Font);
BEGIN W.fnt := fnt
END SetFont;

PROCEDURE SetColor*(VAR W: Writer; col: INTEGER);
BEGIN W.col := col
END SetColor;

PROCEDURE SetOffset*(VAR W: Writer; voff: INTEGER);
BEGIN W.voff := voff
END SetOffset;

PROCEDURE Write*(VAR W: Writer; ch: CHAR);
VAR p: Piece;
BEGIN
  IF (W.buf.last.fnt # W.fnt) OR (W.buf.last.col # W.col) OR (W.buf.last.voff # W.voff) THEN
    NEW(p); p.f := Files.Base(W.rider); p.offset := Files.Pos(W.rider); p.length := 0;
    p.fnt := W.fnt; p.col := W.col; p.voff:= W.voff;
    p.next := NIL; W.buf.last.next := p;
    p.prev := W.buf.last; W.buf.last := p
  END;
  Files.Write(W.rider, ch);
  INC(W.buf.last.length); INC(W.buf.length)
END Write;

PROCEDURE WriteLn*(VAR W: Writer);
BEGIN Write(W, CR)
END WriteLn;

PROCEDURE WriteString*(VAR W: Writer; s: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN
  w.s("Texts.WriteString, col $"); w.h(W.col); w.s(": '"); w.s(s); w.sl("'.");
  i := 0;
  WHILE s[i] >= " " DO Write(W, s[i]); INC(i) END
END WriteString;

PROCEDURE WriteInt*(VAR W: Writer; x, n: INTEGER);
VAR i: INTEGER; x0: INTEGER;
    a: ARRAY 10 OF CHAR;
BEGIN
  IF ROR(x, 31) = 1 THEN WriteString(W, " -2147483648")
  ELSE i := 0;
    IF x < 0 THEN DEC(n); x0 := -x ELSE x0 := x END;
    REPEAT
      a[i] := CHR(x0 MOD 10 + 30H); x0 := x0 DIV 10; INC(i)
    UNTIL x0 = 0;
    WHILE n > i DO Write(W, " "); DEC(n) END;
    IF x < 0 THEN Write(W, "-") END;
    REPEAT DEC(i); Write(W, a[i]) UNTIL i = 0
  END
END WriteInt;

PROCEDURE WriteHex*(VAR W: Writer; x: INTEGER);
VAR i: INTEGER; y: INTEGER;
    a: ARRAY 10 OF CHAR;
BEGIN i := 0; Write(W, " ");
  REPEAT y := x MOD 10H;
    IF y < 10 THEN a[i] := CHR(y + 30H) ELSE a[i] := CHR(y + 37H) END;
    x := x DIV 10H; INC(i)
  UNTIL i = 8;
  REPEAT DEC(i); Write(W, a[i]) UNTIL i = 0
END WriteHex;

(*
PROCEDURE WriteReal*(VAR W: Writer; x: REAL; n: INTEGER);
VAR e, i, k, m: INTEGER;
    d: ARRAY 16 OF CHAR;
BEGIN e := ASR(ORD(x), 23) MOD 100H;  (*binary exponent*)
  IF e = 0 THEN
    WriteString(W, "  0 ");
    WHILE n >= 3 DO Write(W, " "); DEC(n) END
  ELSIF e = 255 THEN WriteString(W, " NaN ")
  ELSE Write(W, " ");
    WHILE n >= 15 DO DEC(n); Write(W, " ") END ;
    (* 2 < n < 9 digits to be written*)
    IF x < 0.0 THEN Write(W, "-"); x := -x ELSE Write(W, " ") END ;
    e := (e - 127) * 77 DIV 256 - 6;  (*decimal exponent*)
    IF e >= 0 THEN x := x / Ten(e) ELSE x := Ten(-e) * x END ;
    m := FLOOR(x + 0.5);
    IF m >= 10000000 THEN INC(e); m := m DIV 10 END ;
    i := 0; k := 13-n;
    REPEAT
      IF i = k THEN INC(m, 5) END ;    (*rounding*)
      d[i] := CHR(m MOD 10 + 30H); m := m DIV 10; INC(i)
    UNTIL m = 0;
    DEC(i); Write(W, d[i]); Write(W, ".");
    IF i < n-7 THEN n := 0 ELSE n := 14 - n END ;
    WHILE i > n DO DEC(i); Write(W, d[i]) END ;
    Write(W, "E"); INC(e, 6);
     IF e < 0 THEN Write(W, "-"); e := -e ELSE Write(W, "+") END ;
    Write(W, CHR(e DIV 10 + 30H)); Write(W, CHR(e MOD 10 + 30H))
  END
END WriteReal;
*)

PROCEDURE WriteRealFix*(VAR W: Writer; x: REAL; n, k: INTEGER);
VAR i, m: INTEGER; neg: BOOLEAN;
    d: ARRAY 12 OF CHAR;
BEGIN
  IF x = 0.0 THEN WriteString(W, "  0")
  ELSE
    IF x < 0.0 THEN x := -x; neg := TRUE ELSE neg := FALSE END ;
    IF k > 7 THEN k := 7 END ;
    x := Ten(k) * x; m := FLOOR(x + 0.5);
    i := 0;
    REPEAT d[i] := CHR(m MOD 10 + 30H); m := m DIV 10; INC(i) UNTIL m = 0;
    Write(W, " ");
    WHILE n > i+3 DO  Write(W, " "); DEC(n) END ;
    IF neg THEN Write(W, "-"); DEC(n) ELSE Write(W, " ") END ;
    WHILE i > k DO DEC(i); Write(W, d[i]) END ;
    Write(W, ".");
    WHILE k > i DO DEC(k); Write(W, "0") END ;
    WHILE i > 0 DO DEC(i); Write(W, d[i]) END
  END
END WriteRealFix;

(*
PROCEDURE WritePair(VAR W: Writer; ch: CHAR; x: INTEGER);
BEGIN Write(W, ch);
  Write(W, CHR(x DIV 10 + 30H)); Write(W, CHR(x MOD 10 + 30H))
END WritePair;

PROCEDURE WriteClock*(VAR W: Writer; d: INTEGER);
BEGIN
  WritePair(W, " ", d DIV 20000H   MOD 20H);  (* day   *)
  WritePair(W, ".", d DIV 400000H  MOD 10H);  (* month *)
  WritePair(W, ".", d DIV 4000000H MOD 40H);  (* year  *)
  WritePair(W, " ", d DIV 1000H    MOD 20H);  (* hour  *)
  WritePair(W, ":", d DIV 40H      MOD 40H);  (* min   *)
  WritePair(W, ":", d MOD 40H)                (* sec   *)
END WriteClock;
*)

BEGIN
  w.sl("Texts initialising.");
  TrailerFile := Files.New("");
  w.sl("Texts initialised.");
END Texts.
