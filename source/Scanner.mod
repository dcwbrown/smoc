(*
  Project Oberon, Revised Edition 2013

  Book copyright (C)2013 Niklaus Wirth and Juerg Gutknecht;
  software copyright (C)2013 Niklaus Wirth (NW), Juerg Gutknecht (JG), Paul
  Reed (PR/PDR).

  Permission to use, copy, modify, and/or distribute this software and its
  accompanying documentation (the "Software") for any purpose with or
  without fee is hereby granted, provided that the above copyright notice
  and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHORS DISCLAIM ALL WARRANTIES
  WITH REGARD TO THE SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY, FITNESS AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
  AUTHORS BE LIABLE FOR ANY CLAIM, SPECIAL, DIRECT, INDIRECT, OR
  CONSEQUENTIAL DAMAGES OR ANY DAMAGES OR LIABILITY WHATSOEVER, WHETHER IN
  AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  CONNECTION WITH THE DEALINGS IN OR USE OR PERFORMANCE OF THE SOFTWARE.
*)

MODULE Scanner;  (* Modified from ORS module in Project Oberon *)

IMPORT SYSTEM, K := Kernel, Files, BigNums, w := Writer;

CONST
  TAB = 9;
  LF  = 10;
  CR  = 13;
  SP  = 32;

  MaxIdLen*     = 63;
  MaxStrLen*    = 2048;
  MaxInt        = 9223372036854775807;
  MinInt        = -MaxInt - 1;
  NKW           = 35;  (* Number of keywords *)
  maxExp        = 308;
  stringBufSize = 256;

  (* Symbols *)
  null*   = 0;   times*  = 1;   rdiv*  = 2;   div*    = 3;   mod* = 4;
  and*    = 5;   plus*   = 6;   minus* = 7;   or*     = 8;   eql* = 9;
  neq*    = 10;  lss*    = 11;  leq*   = 12;  gtr*    = 13;  geq* = 14;
  in*     = 15;  is*     = 16;  arrow* = 17;  period* = 18;
  char*   = 20;  int*    = 21;  real*  = 22;  false*  = 23;  true*  = 24;
  nil*    = 25;  string* = 26;  not*   = 27;  lparen* = 28;  lbrak* = 29;
  lbrace* = 30;  ident*  = 31;

  if*     = 32;  while*  = 34;  repeat*    = 35;  case*   = 36;  for*    = 37;
  comma*  = 40;  colon*  = 41;  becomes*   = 42;  upto*   = 43;  rparen* = 44;
  rbrak*  = 45;  rbrace* = 46;  then*      = 47;  of*     = 48;  do*     = 49;
  to*     = 50;  by*     = 51;  semicolon* = 52;  end*    = 53;  bar*    = 54;
  else*   = 55;  elsif*  = 56;  until*     = 57;  return* = 58;
  array*  = 60;  record* = 61;  pointer*   = 62;
  const*  = 70;  type*   = 71;  var*       = 72;  procedure* = 73;  begin* = 74;
  import* = 76;  module* = 77;

  call* = 100;  par* = 101;  sproc* = 102;  bitset* = 104;

  begSf*   = 110;
  sfABS*   = 110;  sfODD* = 111;  sfLEN* = 112;
  sfLSL*   = 113;  sfASR* = 114;  sfROR* = 115;
  sfFLOOR* = 116;  sfFLT* = 117;  sfORD* = 118;  sfCHR8* = 119;

  sfADR*          = 120;  sfBIT* = 121;  sfVAL* = 122;  sfSIZE* = 123;
  sfNtCurrentTeb* = 124;  sfCAS* = 125;
  endSf*          = 129;

  begSp* = 130;
  spINC* = 130;  spDEC*    = 131;  spINCL* = 132;  spEXCL* = 133;
  spNEW* = 134;  spASSERT* = 135;  spPACK* = 136;  spUNPK* = 137;
  spGET* = 138;  spPUT*    = 139;  spCOPY* = 140;

  spINT3* = 154;
  spPAUSE*        = 155;
  endSp*          = 159;

TYPE
  IdStr* = ARRAY MaxIdLen+1  OF CHAR;
  Str*   = ARRAY MaxStrLen+1 OF CHAR;

  SetCompilerFlagProc* = PROCEDURE(pragma: ARRAY OF CHAR);
  NotifyErrorProc*     = PROCEDURE(line, column: INTEGER;  msg: ARRAY OF CHAR);

VAR
  ival*, slen*: INTEGER;
  rval*:        REAL;
  id*:          IdStr;
  str*:         Str;
  ansiStr*:     BOOLEAN;
  errCnt*:      INTEGER;

  ch:     INTEGER;  (* Unicode codepoint 0 .. 17*65536-1 *)
  eof:    BOOLEAN;
  k:      INTEGER;
  KWX:    ARRAY 11 OF INTEGER;
  keyTab: ARRAY NKW OF RECORD sym: INTEGER;  id: IdStr END;

  buffer: ARRAY 80000H OF CHAR;
  bufPos, bufSize: INTEGER;

  errPos:                  INTEGER;
  lineNumber, linePos:     INTEGER;
  LastLine*,  LastColumn*: INTEGER;

  SetCompilerFlag: SetCompilerFlagProc;
  NotifyError:     NotifyErrorProc;

PROCEDURE Mark*(msg: ARRAY OF CHAR);
BEGIN
  IF (bufPos > errPos) & (errCnt < 25) & (NotifyError # NIL) THEN
    NotifyError(LastLine, LastColumn, msg)
  END;
  INC(errCnt);  errPos := bufPos + 1
END Mark;

PROCEDURE Read;
VAR n: INTEGER;
BEGIN
  IF bufPos < bufSize THEN
    ch := K.GetUtf8(buffer, bufPos);
    IF ch = LF THEN linePos := bufPos;  INC(lineNumber) END;
  ELSE eof := TRUE;  ch := 0
  END
END Read;

PROCEDURE SourcePos*(): INTEGER;
RETURN LSL(LastLine, 10) + (LastColumn MOD 400H) END SourcePos;

PROCEDURE Identifier(c1: CHAR): INTEGER;
VAR i, j, k, sym: INTEGER;
BEGIN sym := ident;  id[0] := c1;  i := 1;
  WHILE (i < MaxIdLen) & (   (ch >= ORD("0")) & (ch <= ORD("9"))
                          OR (ch >= ORD("A")) & (ch <= ORD("Z"))
                          OR (ch >= ORD("a")) & (ch <= ORD("z"))
                          OR (ch =  ORD("_"))) DO
    K.PutUtf8 (ch, id, i);
    Read
  END;
  id[i] := 0X;
  IF i >= MaxIdLen THEN Mark("identifier too long") END;
  (* search for keyword *)
  IF i < LEN(KWX) THEN
    j := KWX[i-1];  k := KWX[i];
    WHILE (keyTab[j].id # id) & (j < k) DO INC(j) END;
    IF j < k THEN sym := keyTab[j].sym END
  END
RETURN sym END Identifier;

PROCEDURE String(quoteCh: CHAR): INTEGER;
BEGIN
  slen := 0;
  WHILE ~eof & (slen < MaxStrLen) & (ch # ORD(quoteCh)) DO
    K.PutUtf8(ch, str, slen); Read
  END;
  Read;  str[slen] := 0X;  INC(slen);
  IF slen >= MaxStrLen THEN Mark("String too long") END;
RETURN string END String;

PROCEDURE HexString(): INTEGER;
VAR i, m, n, o, p: INTEGER;

  PROCEDURE hexdigit(): INTEGER;
  VAR n: INTEGER;
  BEGIN
    IF    eof THEN n := -1
    ELSIF (ch >= ORD("0")) & (ch <= ORD("9")) THEN n := ch - 30H
    ELSIF (ch >= ORD("A")) & (ch <= ORD("F")) THEN n := ch - 37H
    ELSE  n := -1 END
  RETURN n END hexdigit;
BEGIN
  i := 0;  slen := 0;
  WHILE ~eof & (ch # ORD("$")) DO

    WHILE ~eof & ((ch = SP) OR (ch = TAB) OR (ch = CR) OR (ch = LF) OR (ch = ORD("#"))) DO
      IF ch = ORD("#") THEN Read; WHILE ~eof & (ch #LF) DO Read END; Read
      ELSE Read END
    END;

    IF ~eof & (ch # ORD("$")) THEN
      m := hexdigit(); Read;  IF m >= 0 THEN n := hexdigit(); Read END;
      IF (m >= 0) & (n >= 0) THEN
        IF slen < MaxStrLen THEN str[slen] := CHR(m * 10H + n); INC(slen) END
      ELSE
        Mark("Hex digit expected")
      END
    END
  END;
  IF slen > MaxStrLen THEN Mark("Hex string too long") END;
  Read;
  str[slen] := 0X;  (* Guaranteed terminator, not included in string length *)
  RETURN string
END HexString;

PROCEDURE Real(VAR sym: INTEGER;  d: ARRAY OF INTEGER;  n: INTEGER);
VAR x, f, max, min, half: BigNums.BigNum;
    i, k, e, float, last: INTEGER;  negE: BOOLEAN;
BEGIN i := n-1;  k := 0;  x := BigNums.Zero;  f := BigNums.Zero;
  REPEAT
    IF d[i] > 10 THEN Mark("Bad number")
    ELSE BigNums.SetDecimalDigit(x, k, d[i])
    END;
    DEC(i);  INC(k)
  UNTIL i < 0;
  i := BigNums.MaxDecimalDigits-1;
  WHILE (ch >= ORD("0")) & (ch <= ORD("9")) DO (* fraction *)
    IF i > BigNums.MaxDecimalDigits-19 THEN
      BigNums.SetDecimalDigit(f, i, ch-30H)
    ELSIF i = BigNums.MaxDecimalDigits-19 THEN Mark("Fraction too long")
    END;
    DEC(i);  Read
  END;
  IF (ch = ORD("E")) OR (ch = ORD("D")) THEN (* scale factor *)
    Read;  e := 0;
    IF ch = ORD("-") THEN negE := TRUE;  Read
    ELSE negE := FALSE;  IF ch = ORD("+") THEN Read END
    END;
    IF (ch >= ORD("0")) & (ch <= ORD("9")) THEN
      REPEAT e := e*10 + ch-30H;  Read
      UNTIL (ch < ORD("0")) OR (ch > ORD("9")) OR (e > maxExp);
      IF e > maxExp THEN Mark("Exponent too large");
        WHILE (ch < ORD("0")) OR (ch > ORD("9")) DO Read END
      END;
      IF negE THEN e := -e END
    ELSE Mark("Digit?")
    END;
    i := BigNums.MaxDecimalDigits-1;
    WHILE e > 0 DO BigNums.MultiplyByTen(x, x);
      BigNums.SetDecimalDigit(x, 0, BigNums.DecimalDigit(f, i));
      BigNums.SetDecimalDigit(f, i, 0);  BigNums.MultiplyByTen(f, f);
      DEC(e)
    END;
    WHILE e < 0 DO
      last := BigNums.DecimalDigit(f, 0);  BigNums.DivideByTen(f, f);
      BigNums.SetDecimalDigit(f, i, BigNums.DecimalDigit(x, 0));
      BigNums.DivideByTen(x, x);
      IF (last > 5) OR (last = 5) & ODD(BigNums.DecimalDigit(f, 0)) THEN
        IF BigNums.Compare(f, BigNums.MaxNum) = 0 THEN
          f := BigNums.Zero;  BigNums.Add(x, x, BigNums.One)
        ELSE BigNums.Add(f, f, BigNums.One)
        END
      END;
      INC(e)
    END
  END;
  e := 52;  half := BigNums.Zero;
  i := BigNums.MaxDecimalDigits-1;  BigNums.SetDecimalDigit(half, i, 5);
  BigNums.Set0(max, 1FFFFFFFFFFFFFH);  BigNums.Set0(min, 10000000000000H);
  IF (BigNums.Compare(x, BigNums.Zero) # 0)
  OR (BigNums.Compare(x, BigNums.Zero) # 0) THEN
    WHILE BigNums.Compare(x, min) < 0 DO BigNums.Add(x, x, x);
      IF BigNums.Compare(f, half) >= 0 THEN
        BigNums.Subtract(f, f, half);  BigNums.Add(x, x, BigNums.One)
      END;
      BigNums.Add(f, f, f);  DEC(e)
    END;
    WHILE BigNums.Compare(x, max) > 0 DO BigNums.DivideByTwo(f, f);
      IF BigNums.ModuloTwo(x) = 1 THEN BigNums.Add(f, f, half) END;
      BigNums.DivideByTwo(x, x);  INC(e)
    END;
    float := BigNums.Get0(x);  i := BigNums.Compare(f, half);
    IF (i > 0) OR (i = 0) & ODD(float) THEN INC(float);
      IF float > 1FFFFFFFFFFFFFH THEN float := float DIV 2;  INC(e) END
    END;
    float := float - 10000000000000H + (e+1023)*10000000000000H;
  ELSE float := 0
  END;
  sym := real;  rval := SYSTEM.VAL(REAL, float);  ival := float
END Real;

PROCEDURE Number(c1: CHAR): INTEGER;
CONST max = MaxInt;
VAR i, k2, e, n, s, h: INTEGER;
    x:    REAL;
    d:    ARRAY 21 OF INTEGER;
    negE: BOOLEAN;
    sym:  INTEGER;
BEGIN
  ival := 0;  i := 0;  k2 := 0;
  d[0] := ORD(c1) - 30H;  n := 1;
  WHILE (ch >= ORD("0")) & (ch <= ORD("9"))
  OR    (ch >= ORD("A")) & (ch <= ORD("F")) DO
    IF n < LEN(d) THEN d[n] := ch - 30H;  INC(n)
    ELSE Mark("Too many digits");  n := 0
    END;
    Read
  END;
  IF (ch = ORD("H")) OR (ch = ORD("R")) OR (ch = ORD("X")) OR (ch = ORD("Y")) THEN  (* hex *)
    REPEAT h := d[i];
      IF h >= 10 THEN h := h-7 END;
      k2 := k2*10H + h;  INC(i) (* no overflow check *)
    UNTIL i = n;
    IF ch = ORD("X") THEN sym := string;
      IF k2 < 100H THEN ival := k2 ELSE Mark("Illegal value");  ival := 0  END;
      IF k2 = 0 THEN str[0] := 0X;  slen := 1
      ELSE str[0] := CHR(k2);  str[1] := 0X;  slen := 2
      END
    ELSIF ch = ORD("R") THEN sym := real;  rval := SYSTEM.VAL(REAL, k2)
    ELSE sym := int;  ival := k2
    END;
    Read
  ELSIF ch = ORD(".") THEN Read;
    IF ch = ORD(".") THEN (* double dot *) ch := 7FH;  (* decimal integer *)
      REPEAT
        IF d[i] < 10 THEN
          IF k2 <= (max-d[i]) DIV 10 THEN k2 := k2 * 10 + d[i]
          ELSE Mark("Too large");  k2 := 0
          END
        ELSE Mark("Bad integer")
        END;
        INC(i)
      UNTIL i = n;
      sym := int;  ival := k2
    ELSE (* real number *)
      Real(sym, d, n)
    END
  ELSE (* decimal integer *)
    REPEAT
      IF d[i] < 10 THEN
        IF k2 <= (max-d[i]) DIV 10 THEN k2 := k2*10 + d[i]
        ELSE Mark("Too large");  k2 := 0
        END
      ELSE Mark("Bad integer")
      END;
      INC(i)
    UNTIL i = n;
    sym := int;  ival := k2
  END
RETURN sym END Number;

PROCEDURE SkipComment(lev: INTEGER);
VAR exit: BOOLEAN;

  PROCEDURE SetPragma;
  VAR pragma: Str;  i: INTEGER;
  BEGIN Read;  i := 0;
    WHILE (i < LEN(pragma) - 1) & (ch # ORD("*")) & ~eof DO
      K.PutUtf8(ch, pragma, i);  Read
    END;
    pragma[i] := 0X;
    IF ch = ORD("*") THEN SetCompilerFlag(pragma)
    ELSE Mark("Incorrect compiler directive")
    END
  END SetPragma;

BEGIN
  IF (ch = ORD("$")) & (lev = 0) THEN SetPragma END;
  exit := FALSE;
  WHILE ~eof & ~exit DO
    IF ch = ORD("(") THEN Read;
      IF ch = ORD("*") THEN Read;  SkipComment(lev + 1) END
    ELSIF ch = ORD("*") THEN Read;
      IF ch = ORD(")") THEN Read;  exit := TRUE END
    ELSE Read
    END
  END
END SkipComment;

(*       ASCII == first 128 characters of Unicode:       *)
(*                                                       *)
(*            0 1 2 3 4 5 6 7 8 9 a b c d e f            *)
(*          +---------------------------------+          *)
(*   32   20|   ! " # $ % & ' ( ) * + , - . / |20    32  *)
(*   48   30| 0 1 2 3 4 5 6 7 8 9 : ; < = > ? |30    48  *)
(*   64   40| @ A B C D E F G H I J K L M N O |40    64  *)
(*   80   50| P Q R S T U V W X Y Z [ \ ] ^ _ |50    80  *)
(*   96   60| ` a b c d e f g h i j k l m n o |60    96  *)
(*  112   70| p q r s t u v w x u z { | } ~   |70   112  *)
(*          +---------------------------------+          *)
(*            0 1 2 3 4 5 6 7 8 9 a b c d e f            *)

PROCEDURE Get*(VAR sym: INTEGER);
VAR c1: CHAR;
BEGIN
  REPEAT
    WHILE ~eof & (ch <= ORD(" ")) DO Read END;
    (* Record potential error position *)
    LastLine   := lineNumber;
    LastColumn := bufPos - linePos;
    sym := null;
    IF (ch > 32) & (ch < 128) THEN
      c1 := CHR(ch);  Read;
      CASE c1 OF
      | "A".."Z": sym := Identifier(c1)
      | "a".."z": sym := Identifier(c1)
      | ";":      sym := semicolon
      | "(":      IF ch # ORD("*") THEN sym := lparen ELSE Read; SkipComment(0) END
      | ")":      sym := rparen
      | ".":      IF ch # ORD(".") THEN sym := period ELSE Read; sym := upto    END
      | ":":      IF ch # ORD("=") THEN sym := colon  ELSE Read; sym := becomes END
      | ",":      sym := comma
      | "0".."9": sym := Number(c1)
      | "=":      sym := eql
      | "*":      sym := times
      | '"', "'": sym := String(c1)
      | "[":      sym := lbrak
      | "]":      sym := rbrak
      | "#":      sym := neq
      | "+":      sym := plus
      | "&":      sym := and
      | "-":      sym := minus
      | "<":      IF ch # ORD("=") THEN sym := lss ELSE Read; sym := leq END
      | ">":      IF ch # ORD("=") THEN sym := gtr ELSE Read; sym := geq END
      | "{":      sym := lbrace
      | "}":      sym := rbrace
      | "~":      sym := not
      | "|":      sym := bar
      | "^":      sym := arrow
      | "/":      sym := rdiv
      | "$":      sym := HexString()
      | "_":      sym := Identifier(c1)
      | 7FX:      sym := upto
      | "!", "%",
        "@", '\',
        "|", "`": Mark("Unexpected character")
      END
    ELSE
      Read;
    END
  UNTIL (sym # null) OR eof
END Get;


PROCEDURE Init*(f: Files.File);
VAR r: Files.Rider;
BEGIN
  Files.Set(r, f, 0);
  Files.ReadBytes(r, buffer, LEN(buffer));
  bufSize := LEN(buffer)-r.res;  eof := FALSE;
  bufPos     := 0;
  errCnt     := 0;  errPos     := -10;
  lineNumber := 1;  linePos    := 0;
  LastLine   := 1;  LastColumn := 1;
  Read
END Init;

PROCEDURE InstallSetCompilerFlag*(proc: SetCompilerFlagProc);
BEGIN SetCompilerFlag := proc
END InstallSetCompilerFlag;

PROCEDURE InstallNotifyError*(proc: NotifyErrorProc);
BEGIN NotifyError := proc
END InstallNotifyError;

PROCEDURE EnterKW(sym: INTEGER;  name: IdStr);
BEGIN keyTab[k].id := name;  keyTab[k].sym := sym;  INC(k)
END EnterKW;

BEGIN
  k := 0;  KWX[0] := 0;  KWX[1] := 0;
  EnterKW(if, "IF");
  EnterKW(do, "DO");
  EnterKW(of, "OF");
  EnterKW(or, "OR");
  EnterKW(to, "TO");
  EnterKW(in, "IN");
  EnterKW(is, "IS");
  EnterKW(by, "BX");
  KWX[2] := k;
  EnterKW(end, "END");
  EnterKW(nil, "NIL");
  EnterKW(var, "VAR");
  EnterKW(div, "DIV");
  EnterKW(mod, "MOD");
  EnterKW(for, "FOR");
  KWX[3] := k;
  EnterKW(else, "ELSE");
  EnterKW(then, "THEN");
  EnterKW(true, "TRUE");
  EnterKW(type, "TYPE");
  EnterKW(case, "CASE");
  KWX[4] := k;
  EnterKW(elsif, "ELSIF");
  EnterKW(false, "FALSE");
  EnterKW(array, "ARRAY");
  EnterKW(begin, "BEGIN");
  EnterKW(const, "CONST");
  EnterKW(until, "UNTIL");
  EnterKW(while, "WHILE");
  KWX[5] := k;
  EnterKW(record, "RECORD");
  EnterKW(repeat, "REPEAT");
  EnterKW(return, "RETURN");
  EnterKW(import, "IMPORT");
  EnterKW(module, "MODULE");
  KWX[6] := k;
  EnterKW(pointer, "POINTER");
  KWX[7] := k;
  KWX[8] := k;
  EnterKW(procedure, "PROCEDURE");
  KWX[9] := k;
  EnterKW(null, "EXTENSIBLE");
  KWX[10] := k
END Scanner.