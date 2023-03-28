MODULE Dumper;

(* Include this module to get a trace of every garbage collection. *)

IMPORT Out, SYSTEM, Rtl;

VAR memstart, memlimit: INTEGER;


(* -------------------------------------------------------------------------- *)
(* Character output convenience functions                                     *)

PROCEDURE wl();                  BEGIN Out.Ln        END wl;
PROCEDURE wc(c: CHAR);           BEGIN Out.Char(c)   END wc;
PROCEDURE ws(s: ARRAY OF CHAR);  BEGIN Out.String(s) END ws;
PROCEDURE wsl(s: ARRAY OF CHAR); BEGIN ws(s); wl     END wsl;
PROCEDURE wb(i: INTEGER);        BEGIN WHILE i > 0 DO wc(' ');  DEC(i) END END wb;

PROCEDURE wh1 (n: INTEGER); BEGIN IF n<10 THEN wc(CHR(n + 48)) ELSE wc(CHR(n + 87)) END END wh1;
PROCEDURE wh2 (n: INTEGER); BEGIN wh1(ASR(n,4)  MOD        10H);  wh1(n MOD        10H) END wh2;
PROCEDURE wh4 (n: INTEGER); BEGIN wh2(ASR(n,8)  MOD       100H);  wh2(n MOD       100H) END wh4;
PROCEDURE wh8 (n: INTEGER); BEGIN wh4(ASR(n,16) MOD     10000H);  wh4(n MOD     10000H) END wh8;
PROCEDURE wh12(n: INTEGER); BEGIN wh4(ASR(n,32) MOD     10000H);  wh8(n MOD 100000000H) END wh12;
PROCEDURE wh16(n: INTEGER); BEGIN wh8(ASR(n,32) MOD 100000000H);  wh8(n MOD 100000000H) END wh16;

PROCEDURE wh(n: INTEGER);
BEGIN
  IF (n < 0) OR (n > 15) THEN wh((n DIV 16) MOD 1000000000000000H) END;
  wh1(n MOD 16);
END wh;

PROCEDURE whs(n: INTEGER); BEGIN IF n < 0 THEN wc("-");  n := -n END;  wh(n) END whs;

PROCEDURE wi(n: INTEGER);
BEGIN
  IF n < 0 THEN wc('-'); n := -n END;
  IF n > 9 THEN wi(n DIV 10) END;
  wc(CHR(n MOD 10 + 48))
END wi;

(* -------------------------------------------------------------------------- *)
(* Memory access wrapper functions *)

PROCEDURE InitialiseMemoryRange;
VAR stackvar: INTEGER;
BEGIN
  memstart := SYSTEM.ADR(stackvar) - 1000H;  (* 1000H is stack reserve in PE header *)
  memlimit := Rtl.heapBase + Rtl.heapSize;
END InitialiseMemoryRange;

PROCEDURE CheckAddress(adr: INTEGER);
BEGIN
  IF (adr < memstart) OR (adr >= memlimit) THEN
    ws("SYSTEM.GET address $"); wh(adr);
    IF adr < memstart THEN ws(" below") ELSE ws(" above") END;
    wsl(" program memory.");
    Rtl.Halt(4)
  END
END CheckAddress;

PROCEDURE getbyte(adr: INTEGER): BYTE;
VAR result: BYTE;
BEGIN CheckAddress(adr);  SYSTEM.GET(adr, result)
RETURN result END getbyte;

PROCEDURE getword(adr: INTEGER): SYSTEM.CARD16;
VAR result: SYSTEM.CARD16;
BEGIN CheckAddress(adr);  SYSTEM.GET(adr, result)
RETURN result END getword;

PROCEDURE getdword(adr: INTEGER): SYSTEM.CARD32;
VAR result: SYSTEM.CARD32;
BEGIN CheckAddress(adr);  SYSTEM.GET(adr, result)
RETURN result END getdword;

PROCEDURE getint(adr: INTEGER): INTEGER;
VAR result: INTEGER;
BEGIN CheckAddress(adr);  SYSTEM.GET(adr, result)
RETURN result END getint;

(* -------------------------------------------------------------------------- *)

PROCEDURE WriteAnsiName(adr: INTEGER);
VAR ch: INTEGER;
BEGIN
  ch := getbyte(adr);
  WHILE ch # 0 DO
    wc(CHR(ch));
    INC(adr);
    ch := getbyte(adr)
  END
END WriteAnsiName;

(* -------------------------------------------------------------------------- *)

PROCEDURE Dump*(indent, adr, len: INTEGER);
VAR
  rowadr, i, dumplimit: INTEGER;
  bytes: ARRAY 16 OF INTEGER;
BEGIN
  rowadr    := (       adr       DIV 16) * 16;
  dumplimit := ((adr + len + 15) DIV 16) * 16;
  WHILE rowadr < dumplimit DO
    wb(indent); wh12(rowadr); ws("  ");

    (* Load a row of bytes *)
    FOR i := 0 TO 15 DO
      IF (rowadr+i >= adr) & (rowadr+i < adr+len) THEN
        bytes[i] := getbyte(rowadr+i)
      ELSE
        bytes[i] := -1
      END
    END;

    (* One row of hex Dump *)
    FOR i := 0 TO 15 DO
      IF i MOD 8 = 0 THEN wc(" ") END;
      IF bytes[i] >= 0 THEN wh2(bytes[i]);  wc(" ") ELSE ws("   ") END;
    END;
    ws("  ");

    (* One row of character Dump *)
    FOR i := 0 TO 15 DO
      IF bytes[i] >= 0 THEN
        IF (bytes[i] < 32) OR (bytes[i] >= 127) THEN
          wc('.')
        ELSE
          wc(CHR(bytes[i]))
        END
      ELSE
        wc(" ")
      END
    END;

    wl;
    INC(rowadr, 16);
  END
END Dump;

PROCEDURE DumpVar*(title: ARRAY OF CHAR; VAR var: ARRAY OF BYTE);
BEGIN
  ws("Variable ");  ws(title);  ws(" at $");  wh(SYSTEM.ADR(var));  wsl(":");
  Dump(2, SYSTEM.ADR(var), LEN(var))
END DumpVar;

(* -------------------------------------------------------------------------- *)

(* NOTE: Dumper is called during (at the start of) collection so           *)
(*       MUST NOT cause any heap allocation.                                  *)

PROCEDURE DumpPointerTable(title: ARRAY OF CHAR; indent, base, table: INTEGER);
VAR offset, ptr, descriptor, size: INTEGER;
BEGIN
  offset := getint(table);
  IF offset # -1 THEN
    wb(indent);  ws(title);  wl;

    Dump(indent, table, 256);

    WHILE offset # -1 DO
      wb(indent+2); ws("Offset $");  whs(offset);
      ws(" = $"); wh(base+offset);
      ptr := getint(base + offset) - 16;
      ws(", target metadata at $");  wh(ptr);
      IF ptr < Rtl.heapBase THEN wsl(": not in heap.")
      ELSE
        descriptor := getint(ptr);  ws(", type $");  wh(descriptor);
        ws(", size $");  wh(getint(descriptor));
        ws(", mark $");  wh(getint(ptr + 8));
        wsl(".")
      END;
      INC(table, 8);  offset := getint(table)
    END
  END
END DumpPointerTable;


PROCEDURE WriteModuleName(baseadr: INTEGER);
VAR loadadr, exports, nameadr: INTEGER;
BEGIN
  loadadr := baseadr - getint(baseadr + 80);
  exports := loadadr + getint(loadadr + 3D8H);
  nameadr := loadadr + getdword(exports + 12);
  WriteAnsiName(nameadr)
END WriteModuleName;


PROCEDURE HeapTrace*(reason: INTEGER);
VAR i, modBase, ptrTable, offset, ptr, typedesc, stkDesc, stkBase: INTEGER;
BEGIN
  IF Rtl.nMod # 0 THEN
    wl; ws("Heap trace callback, reason "); wi(reason);
    ws(". modList: $"); wh(Rtl.modList);
    ws(", nMod: "); wi(Rtl.nMod); wsl('.');

    FOR i := 0 TO Rtl.nMod-1 DO
      modBase := getint(Rtl.modList+8*i);
      ws("  module [");    wi(i);
      ws("] loadaddr $");  wh(modBase - getint(modBase+80));
      ws(", baseaddr $");  wh(modBase);
      ws(" '");            WriteModuleName(modBase);
      wsl("'.");

      ptrTable := getint(modBase + 112);
      IF getint(ptrTable) # -1 THEN DumpPointerTable("Pointers in global VARs:", 4, modBase, ptrTable) END;

      stkDesc := getint(modBase+104);
      WHILE stkDesc # 0 DO
        ws("    Stack descriptor at $"); wh(stkDesc);
        stkBase  := getint(stkDesc);   ws(", base at $");     wh(stkBase);
        ptrTable := getint(stkDesc+8); ws(", ptrtable at $"); wh(ptrTable);
        wsl(".");
        DumpPointerTable("Pointers in stack frame:", 6, stkBase, ptrTable);

        stkDesc := getint(stkDesc+16)
      END;
      wl
    END
  END
END HeapTrace;

PROCEDURE EnableHeapTrace*();
BEGIN Rtl.InstallHeapTraceHandler(HeapTrace) END EnableHeapTrace;

(* -------------------------------------------------------------------------- *)

BEGIN InitialiseMemoryRange;
END Dumper.
