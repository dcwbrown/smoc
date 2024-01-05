MODULE X64;  (* DCWB August 2023; X64 constants *)

IMPORT SYSTEM, ORS, ORB, w := Writer;

CONST
  MaxPC* = 10000H;  (* Text generation buffer size *)

  (* Named registers *)
  RAX* = 0;
  RCX* = 1;
  RDX* = 2;
  RBX* = 3;
  RSP* = 4;     (* X64 stack pointer *)
  RBP* = 5;
  RSI* = 6;
  RDI* = 7;

  (* Conditions *)
  CF*  = 0;     (* False, Never                         *)
  CT*  = 1;     (* True, always                         *)
  CO*  = 80H;   (* Overflow                             *)
  CNO* = 81H;   (* No overflow                          *)
  CC*  = 82H;   (* Carry, Below                         *)
  CNC* = 83H;   (* No carry, Above or equal             *)
  CZ*  = 84H;   (* Zero, Equal                          *)
  CNZ* = 85H;   (* Nonzero, Not equal                   *)
  CNA* = 86H;   (* Not above, Below or equal            *)
  CA*  = 87H;   (* Above, Not below or equal            *)
  CS*  = 88H;   (* Sign, Negative                       *)
  CNS* = 89H;   (* No sign, Positive or zero            *)
  CP*  = 8AH;   (* Parity                               *)
  CNP* = 8BH;   (* No parity                            *)
  CL*  = 8CH;   (* Less than, Not greater or equal      *)
  CNL* = 8DH;   (* Not less than, Greater or equal      *)
  CNG* = 8EH;   (* Not greater than, Less than or equal *)
  CG*  = 8FH;   (* Greater than, Not less than or equal *)

  (* Dyadic ALU operations, values correspond to x86 instruction set *)
  Plus*  = 00H;
  Or*    = 08H;
  And*   = 20H;
  Minus* = 28H;
  Xor*   = 30H;
  Cmp*   = 38H;

  (* X64 processor item representation *)

  (* Mode           Description        n                   offset                      *)
  (*                -----------------  ------------------  --------------------------- *)
  Cond*   = 20;  (* Condition          CF, CT, CO..CG      0                           *)
  Const*  = 21;  (* Constant           value               0                           *)
  Reg*    = 22;  (* Register           register (0-15)     0                           *)
  Stkind* = 23;  (* Stack indirect     stack offset        0                           *)
  Eadr*   = 24;  (* Effective address  base register       relative to base            *)
  Code*   = 25;  (* Code offset        0                   code offset                 *)
  String* = 26;  (* String offs & len  string length       current string block offset *)
  Global* = 27;  (* VAR offset         0                   VAR block offset            *)
  Import* = 28;  (* Import             16/modno, 16/impno  0                           *)


TYPE
  Item* = RECORD
    mode*:     INTEGER;
    type*:     ORB.Type;
    readonly*: BOOLEAN;
    n*:        INTEGER;
    offset*:   INTEGER;
    index*:    INTEGER;
    scale*:    INTEGER
  END;

  CodeHeaderPtr = POINTER TO CodeHeader;
  CodeHeader* = RECORD
    length*:   SYSTEM.CARD32;  (* File length *)
    initcode*: SYSTEM.CARD32;
    pointers*: SYSTEM.CARD32;
    commands*: SYSTEM.CARD32;
    exports*:  SYSTEM.CARD32;
    imports*:  SYSTEM.CARD32;  (* VARs start here following import resolution *)
    varsize*:  SYSTEM.CARD32;
    key*:      INTEGER;
  END;


VAR
  PC*:     INTEGER;
  SPO*:    INTEGER;
  Text*:   ARRAY MaxPC OF BYTE;
  Header*: CodeHeaderPtr;


(* -------------------------------- Assembly -------------------------------- *)

PROCEDURE ClearMode*(VAR x: Item);
BEGIN
  x.mode     := Const;
  x.n        := -1;
  x.offset   := 0;
  x.index    := -1;
  x.scale    := 0;
END ClearMode;

PROCEDURE ClearStack*; BEGIN SPO := 0 END ClearStack;

PROCEDURE AdjustStack*(delta: INTEGER);  (* delta is a count of quadwords *)
BEGIN INC(SPO, delta) END AdjustStack;

PROCEDURE Emit*(value: INTEGER);
BEGIN
  IF PC < LEN(Text) THEN
    Text[PC] := value;  INC(PC)
  ELSE
    ORS.Mark("Program too long")
  END
END Emit;

PROCEDURE Align*(size: INTEGER);
BEGIN PC := (PC + size - 1) DIV size * size END Align;

PROCEDURE EmitBytes*(size, value: INTEGER);
BEGIN
  ASSERT(size IN {1, 2, 4, 8});
  IF PC + size <= LEN(Text) THEN
    SYSTEM.COPY(SYSTEM.ADR(value), SYSTEM.ADR(Text) + PC, size);
    INC(PC, size)
  ELSE
    ORS.Mark("Program too long")
  END
END EmitBytes;

PROCEDURE EmitString*(s: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN i := 0;
  WHILE (i < LEN(s)) & (s[i] # 0X) DO Emit(ORD(s[i])); INC(i) END;
  Emit(0)
END EmitString;

PROCEDURE IsSigned*(x: ORB.Type): BOOLEAN;  (* returns whether x represents a signed integer value *)
BEGIN RETURN (x.ref >= ORB.Int8) & (x.ref <= ORB.Int64) END IsSigned;

PROCEDURE Peek*(adr, size: INTEGER; signed: BOOLEAN): INTEGER;
VAR result: INTEGER;
BEGIN
  IF signed & (Text[adr+size-1] >= 128) THEN result := -1 ELSE result := 0 END;
  SYSTEM.COPY(SYSTEM.ADR(Text) + adr, SYSTEM.ADR(result), size)
RETURN result END Peek;

PROCEDURE Patch*(adr, size, value: INTEGER);
BEGIN
  IF (adr < 0) OR (adr + size >= MaxPC) THEN
    w.s("** Patch address $"); w.h(adr); w.sl(" out of range **");
    ASSERT(FALSE)
  END;
  ASSERT(size IN {1, 2, 4, 8});
  SYSTEM.COPY(SYSTEM.ADR(value), SYSTEM.ADR(Text) + adr, size)
END Patch;


PROCEDURE Init*;
BEGIN PC := 0;  SPO := 0;  Header := SYSTEM.VAL(CodeHeaderPtr, SYSTEM.ADR(Text)) END Init;

BEGIN  Init
END X64.
