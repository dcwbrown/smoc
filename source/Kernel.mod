MODULE Kernel;  (*$RTL-*)

IMPORT SYSTEM, Boot;

CONST
  MarkedListSentinel = 2;  (* Used to mark end of list of marked heap blocks during GC *)

TYPE
  ExceptionHandlerProc = PROCEDURE(p: INTEGER): INTEGER;

  (* Finalisation during garbage collection *)
  Finalised*     = POINTER [untraced] TO FinalisedDesc;
  FinaliseProc*  = PROCEDURE(ptr: Finalised);
  FinalisedDesc* = RECORD
                     Finalise: FinaliseProc;
                     next:     Finalised
                   END;

VAR
  oneByteBeforeBase: CHAR; (* MUST BE THE FIRST GLOBAL VARIABLE       *)
                           (* - its address locates the kernel's base *)

  Kernel*:       INTEGER;
  User*:         INTEGER;
  Shell*:        INTEGER;

  MessageBoxW:   PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER): INTEGER;
  AddVectoredExceptionHandler: PROCEDURE(first: INTEGER; filter: ExceptionHandlerProc);
  GetSystemTimePreciseAsFileTime: PROCEDURE(tickAdr: INTEGER);

  (* Heap allocation *)
  VirtualAlloc:  PROCEDURE(lpAddress, dwSize, flAllocationType, flProtect: INTEGER): INTEGER;
  HeapBase:      INTEGER;
  HeapSize:      INTEGER;            (* Committed heap memory                        *)
  HeapMax:       INTEGER;            (* HeapSize to HeapMax reserved, not committed  *)
  FreeList:      ARRAY 4 OF INTEGER; (* Free lists for 32, 64, 128 & 256 byte blocks *)
  LargeFreeList: INTEGER;            (* Free list for 512 byte and larger blocks     *)
  Allocated:     INTEGER;            (* Includes 16 byte header but excludes padding *)

  (* Garbage collection *)
  MarkedList:    INTEGER;
  Collect0:      PROCEDURE;
  JustCollected: BOOLEAN;
  FinalisedList: Finalised;

  (* Windows command line *)
  GetCommandLineW:    PROCEDURE(): INTEGER;
  CommandLineToArgvW: PROCEDURE(lpCmdLine, pNumArgs: INTEGER): INTEGER;
  ArgV:               INTEGER;
  NumArgs*:           INTEGER;
  CommandAdr:         INTEGER;



(* -------------------------------------------------------------------------- *)
(* ------------ Unicode Transformation Formats UTF-8 and UTF-16 ------------- *)
(* -------------------------------------------------------------------------- *)

(* UTF8:                                                                                           *)
(* -------------- codepoint --------------    ----------------------- bytes ----------------------- *)
(* 0000 0000 0000 0000 0000 0000 0zzz zzzz    0zzzzzzz                                              *)
(* 0000 0000 0000 0000 0000 0yyy yyzz zzzz    110yyyyy 10zzzzzz                                     *)
(* 0000 0000 0000 0000 xxxx yyyy yyzz zzzz    1110xxxx 10yyyyyy 10zzzzzz                            *)
(* 0000 0000 000w wwxx xxxx yyyy yyzz zzzz    11110www 10xxxxxx 10yyyyyy 10zzzzzz                   *)
(* 0000 00vv wwww wwxx xxxx yyyy yyzz zzzz    111110vv 10wwwwww 10xxxxxx 10yyyyyy 10zzzzzz          *)
(* 0uvv vvvv wwww wwxx xxxx yyyy yyzz zzzz    1111110u 10vvvvvv 10wwwwww 10xxxxxx 10yyyyyy 10zzzzzz *)

PROCEDURE GetUtf8*(src: ARRAY OF BYTE; VAR i: INTEGER): INTEGER;
VAR n, result: INTEGER;
BEGIN ASSERT(i < LEN(src)); result := src[i];  INC(i);
  IF result >= 0C0H THEN
    IF    result >= 0FCH THEN result := result MOD 2;  n := 5
    ELSIF result >= 0F8H THEN result := result MOD 4;  n := 4
    ELSIF result >= 0F0H THEN result := result MOD 8;  n := 3
    ELSIF result >= 0E0H THEN result := result MOD 16; n := 2
    ELSE                      result := result MOD 32; n := 1
    END;
    WHILE n > 0 DO
      result := LSL(result,6);  DEC(n);
      IF (i < LEN(src)) & (src[i] DIV 40H = 2) THEN
        INC(result, src[i] MOD 40H);  INC(i)
      END
    END
  END;
RETURN result END GetUtf8;

PROCEDURE PutUtf8*(c: INTEGER; VAR dst: ARRAY OF BYTE; VAR i: INTEGER);
VAR n: INTEGER;
BEGIN
  ASSERT(i < LEN(dst));
  ASSERT(c > 0);  ASSERT(c < 80000000H);
  IF i < LEN(dst) THEN
    IF c < 80H THEN dst[i] := c;  INC(i)
    ELSE
      IF    c < 800H     THEN  dst[i] := 0C0H + ASR(c, 6);    n := 1;
      ELSIF c < 10000H   THEN  dst[i] := 0E0H + ASR(c, 12);   n := 2;
      ELSIF c < 200000H  THEN  dst[i] := 0F0H + ASR(c, 18);   n := 3;
      ELSIF c < 4000000H THEN  dst[i] := 0F8H + ASR(c, 24);   n := 4;
      ELSE                     dst[i] := 0FCH + ASR(c, 30);   n := 5;
      END;
      INC(i);
      WHILE (n > 0) & (i < LEN(dst)) DO
        DEC(n);  dst[i] := 80H + ASR(c, n*6) MOD 40H;  INC(i)
      END;
    END
  END
END PutUtf8;


PROCEDURE GetUtf16*(src: ARRAY OF SYSTEM.CARD16; VAR i: INTEGER): INTEGER;
VAR result: INTEGER;
BEGIN
  ASSERT(i < LEN(src));
  result := src[i];  INC(i);
  IF result DIV 400H = 36H THEN    (* High surrogate *)
    result := LSL(result MOD 400H, 10) + 10000H;
    IF (i < LEN(src)) & (src[i] DIV 400H = 37H) THEN  (* Low surrogate *)
      INC(result, src[i] MOD 400H);  INC(i)
    END
  END
RETURN result END GetUtf16;

PROCEDURE PutUtf16*(ch: INTEGER; VAR dst: ARRAY OF SYSTEM.CARD16; VAR i: INTEGER);
BEGIN
  ASSERT(i < LEN(dst));
  IF (ch < 10000H) & (i < LEN(dst)) THEN
    dst[i] := ch;  INC(i)
  ELSIF i+1 < LEN(dst) THEN
    DEC(ch, 10000H);
    dst[i] := 0D800H + ch DIV 400H;  INC(i);
    dst[i] := 0DC00H + ch MOD 400H;  INC(i);
  END
END PutUtf16;


PROCEDURE Utf8ToUtf16*(src: ARRAY OF BYTE;  VAR dst: ARRAY OF SYSTEM.CARD16): INTEGER;
VAR i, j: INTEGER;
BEGIN  i := 0;  j := 0;
  WHILE (i < LEN(src)) & (src[i] # 0) DO PutUtf16(GetUtf8(src, i), dst, j) END;
  IF j < LEN(dst) THEN dst[j] := 0;  INC(j) END
RETURN j END Utf8ToUtf16;

PROCEDURE Utf16ToUtf8*(src: ARRAY OF SYSTEM.CARD16;  VAR dst: ARRAY OF BYTE): INTEGER;
VAR i, j: INTEGER;
BEGIN  i := 0;  j := 0;
  WHILE (i < LEN(src)) & (src[i] # 0) DO PutUtf8(GetUtf16(src, i), dst, j) END;
  IF j < LEN(dst) THEN dst[j] := 0;  INC(j) END
RETURN j END Utf16ToUtf8;


(* -------------------------------------------------------------------------- *)
(* ---------------- Last resort error reporting - MessageBox ---------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE MessageBox*(title, msg: ARRAY OF CHAR);
VAR
  res:     INTEGER;
  title16: ARRAY 256 OF SYSTEM.CARD16;
  msg16:   ARRAY 256 OF SYSTEM.CARD16;
BEGIN
  res := Utf8ToUtf16(title, title16);
  res := Utf8ToUtf16(msg,   msg16);
  res := MessageBoxW(0, SYSTEM.ADR(msg16), SYSTEM.ADR(title16), 0)
END MessageBox;


(* -------------------------------------------------------------------------- *)
(* ----------------------- Simple Integer formatting ------------------------ *)
(* -------------------------------------------------------------------------- *)

PROCEDURE IntToDecimal*(n: INTEGER; VAR s: ARRAY OF CHAR);
VAR i, j: INTEGER;  ch: CHAR;
BEGIN
  IF n = 8000000000000000H THEN s := "-9223372036854775808"
  ELSE i := 0;
    IF n < 0 THEN s[0] := '-';  i := 1; END;
    j := i;
    REPEAT s[j] := CHR(n MOD 10 + 48);  INC(j);  n := n DIV 10 UNTIL n = 0;
    s[j] := 0X;  DEC(j);
    WHILE i < j DO ch:=s[i]; s[i]:=s[j]; s[j]:=ch; INC(i); DEC(j) END
  END
END IntToDecimal;

PROCEDURE IntToHex*(n: INTEGER; VAR s: ARRAY OF CHAR);
VAR d, i, j: INTEGER;  ch: CHAR;
BEGIN
  i := 0;  j := 0;
  REPEAT
    d := n MOD 16;  n := n DIV 16;
    IF d <= 9 THEN s[j] := CHR(d + 48) ELSE s[j] := CHR(d + 55) END;
    INC(j)
  UNTIL n = 0;
  s[j] := 0X;  DEC(j);
  WHILE i < j DO ch:=s[i]; s[i]:=s[j]; s[j]:=ch; INC(i); DEC(j) END
END IntToHex;


(* -------------------------------------------------------------------------- *)
(* ---------------------- Very basic string functions ----------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Length*(s: ARRAY OF CHAR): INTEGER;
VAR l: INTEGER;
BEGIN  l := 0;  WHILE (l < LEN(s)) & (s[l] # 0X) DO INC(l) END
RETURN l END Length;

PROCEDURE Append*(s: ARRAY OF CHAR; VAR d: ARRAY OF CHAR);
VAR i, j: INTEGER;
BEGIN
  j := Length(d);
  i := 0; WHILE (i < LEN(s)) & (j < LEN(d)) & (s[i] # 0X) DO
    d[j] := s[i];  INC(i);  INC(j)
  END;
  IF j >= LEN( d) THEN DEC(j) END;  d[j] := 0X
END Append;


(* -------------------------------------------------------------------------- *)
(* --------------------------- Exception handling --------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE WriteException(code: INTEGER; VAR s: ARRAY OF CHAR);
VAR number:  ARRAY 25 OF CHAR;
BEGIN
  IF    code = 0C0000005H THEN s := "Access violation"
  ELSIF code = 0C0000006H THEN s := "In-page error"
  ELSIF code = 0C000001DH THEN s := "Illegal instruction"
  ELSIF code = 0C000008EH THEN s := "Divide by zero"
  ELSIF code = 0C0000094H THEN s := "Integer divide by zero"
                          ELSE s := "Exception. Code: $";
                               IntToHex(code, number);  Append(number, s)
  END
END WriteException;

PROCEDURE ExceptionHandler(p: INTEGER): INTEGER;
TYPE
  Exception = POINTER [untraced] TO RECORD
    code:      SYSTEM.CARD32;
    flags:     SYSTEM.CARD32;
    nested:    Exception;
    address:   INTEGER;
    NumParams: SYSTEM.CARD32;
    Params:    ARRAY 15 OF INTEGER
  END;
  Context           = POINTER [untraced] TO RECORD END;
  ExceptionPointers = POINTER [untraced] TO RECORD
    exception: Exception;  context: Context
  END;

VAR
  ep:      ExceptionPointers;
  module:  Boot.ModuleHeader;
  address: INTEGER;
  trapadr: INTEGER;
  detail:  INTEGER;
  trap:    INTEGER;
  line:    INTEGER;
  col:     INTEGER;
  adr:     INTEGER;
  report:  ARRAY 256 OF CHAR;
  number:  ARRAY 25 OF CHAR;
BEGIN
  ep := SYSTEM.VAL(ExceptionPointers, p);
  address := ep.exception.address;
  module  := Boot.FirstModule;
  WHILE (module # NIL) & (module.length # 0) & ((address < module.code) OR (address > module.trap)) DO
    module := module.next
  END;

  IF module = NIL THEN
    WriteException(ep.exception.code, report);
    Append(" at address $", report);
    IntToHex(address, number);  Append(number, report);
    Append(" (not in loaded module code).", report);
  ELSE
    trapadr := module.trap;
    DEC(address, module.code);  (* Make address relative to modules code *)
    SYSTEM.GET(trapadr, detail);
    WHILE detail # -1 DO
      trap := ASR(detail, 60) MOD 10H;
      line := ASR(detail, 40) MOD 100000H;
      col  := ASR(detail, 30) MOD 400H;
      adr  := detail MOD 40000000H;
      IF adr = address THEN
        detail := -1  (* End loop *)
      ELSE
        INC(trapadr, 8);  SYSTEM.GET(trapadr, detail);
      END
    END;
    IF (adr = address) & (trap <= 10) THEN
      CASE trap OF
      |  1: report := "Array index out of range in module "
      |  2: report := "Type trap in module "
      |  3: report := "String size error in module "
      |  4: report := "NIL reference in module "
      |  5: report := "NIL procedure call in module "
      |  6: report := "Divide by zero in module "
      |  7: report := "Assertion FALSE in module "
      |  9: report := "SYSTEM.GET access violation in module "
      | 10: report := "SYSTEM.PUT access violation in module "
      END;
      Append(module.name, report);
      Append(" at ", report);
      IntToDecimal(line, number); Append(number, report);
      Append(":", report);
      IntToDecimal(col,  number); Append(number, report);
    ELSE
      WriteException(ep.exception.code, report);
      Append(" in module ", report);  Append(module.name, report);
      Append(" at code offset $", report);
      IntToHex(address, number);  Append(number, report)
    END
  END;

  MessageBox("Exception", report);
  Boot.PEImports.ExitProcess(99)  (* Immediate exit - bypass GC finalisation *)
RETURN 0 END ExceptionHandler;



(* -------------------------------------------------------------------------- *)
(* --------------------------- Memory allocation ---------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Align(a: INTEGER;  align: INTEGER): INTEGER;
VAR result: INTEGER;
BEGIN
  IF    a > 0 THEN result := (a + align - 1) DIV align * align
  ELSIF a < 0 THEN result :=        a        DIV align * align
  END
RETURN result END Align;

(*
PROCEDURE InstallHeapTraceHandler*(tracer: HeapTraceHandler);
BEGIN HeapTracer := tracer END InstallHeapTraceHandler;
*)

PROCEDURE InitHeap(reserve, commit: INTEGER);
CONST MEM_RESERVE = 2000H;  MEM_COMMIT = 1000H;  PAGE_READWRITE = 4;
VAR i, p: INTEGER;
BEGIN
  (* Reserve address space for later use as heap space *)
  HeapBase := VirtualAlloc(0, reserve, MEM_RESERVE, PAGE_READWRITE);
  ASSERT(HeapBase # 0);
  HeapMax  := reserve;
  HeapSize := commit;
  HeapBase := VirtualAlloc(HeapBase, HeapSize, MEM_COMMIT, PAGE_READWRITE);
  ASSERT(HeapBase # 0);

  FOR i := 0 TO LEN(FreeList)-1 DO FreeList[i] := 0 END;

  p             := HeapBase;
  LargeFreeList := HeapBase;
  Allocated     := 0;

  SYSTEM.PUT(p,    HeapSize);
  SYSTEM.PUT(p+8,  -1);
  SYSTEM.PUT(p+16, 0);

  MarkedList := MarkedListSentinel;
END InitHeap;


PROCEDURE ExtendHeap;
CONST MEM_COMMIT = 1000H;  PAGE_READWRITE = 4;
VAR p, mark, size, prev, p2: INTEGER;
BEGIN
  p := VirtualAlloc(HeapBase + HeapSize, HeapSize, MEM_COMMIT, PAGE_READWRITE);
  ASSERT(HeapSize < HeapMax);  ASSERT(p # 0);
  SYSTEM.PUT(p, HeapSize);  SYSTEM.PUT(p+8, -1);  SYSTEM.PUT(p+16, 0);
  IF LargeFreeList = 0 THEN
    LargeFreeList := p
  ELSE
    prev := LargeFreeList;  SYSTEM.GET(LargeFreeList+16, p2);
    WHILE p2 # 0 DO prev := p2;  SYSTEM.GET(p2+16, p2) END;
    SYSTEM.PUT(prev+16, p)
  END;
  HeapSize := HeapSize * 2
END ExtendHeap;


PROCEDURE GetLargeBlock(need: INTEGER): INTEGER;
  (* need is multiple of 512 *)
VAR p, q0, q1, q2, size: INTEGER;  done: BOOLEAN;
BEGIN q0 := 0;  q1 := LargeFreeList;  done := FALSE;
  WHILE ~done & (q1 # 0) DO
    SYSTEM.GET(q1, size);  SYSTEM.GET(q1+16, q2);
    IF    size < need THEN (* no fit *) q0 := q1;  q1 := q2
    ELSIF size = need THEN (* extract -> p *)
      done := TRUE;  p := q1;
      IF q0 # 0 THEN SYSTEM.PUT(q0 + 16, q2) ELSE LargeFreeList := q2 END
    ELSE (* reduce size *)
      done := TRUE;  p := q1;  q1 := q1 + need;
      SYSTEM.PUT(q1,     size-need);
      SYSTEM.PUT(q1 + 8,  -1);
      SYSTEM.PUT(q1 + 16, q2);
      IF q0 # 0 THEN SYSTEM.PUT(q0 + 16, q1) ELSE LargeFreeList := q1 END
    END
  END;
  IF ~done THEN
    IF ~JustCollected THEN Collect0 ELSE ExtendHeap;  JustCollected := FALSE END;
    p := GetLargeBlock(need)
  END;
RETURN p END GetLargeBlock;


PROCEDURE GetBlock256(): INTEGER;
VAR p, q: INTEGER;
BEGIN
  IF FreeList[3] # 0 THEN p := FreeList[3];  SYSTEM.GET(FreeList[3]+16, FreeList[3])
  ELSE q := GetLargeBlock(512);  SYSTEM.PUT(q+256, 256);  SYSTEM.PUT(q+(256+8), -1);
    SYSTEM.PUT(q+(256+16), 0);  FreeList[3] := q + 256;  p := q
  END;
RETURN p END GetBlock256;


PROCEDURE GetBlock128(): INTEGER;
VAR p, q: INTEGER;
BEGIN
  IF FreeList[2] # 0 THEN p := FreeList[2];  SYSTEM.GET(FreeList[2]+16, FreeList[2])
  ELSE q := GetBlock256();  SYSTEM.PUT(q+128, 128);  SYSTEM.PUT(q+(128+8), -1);
    SYSTEM.PUT(q+(128+16), 0);  FreeList[2] := q + 128;  p := q
  END;
RETURN p END GetBlock128;


PROCEDURE GetBlock64(): INTEGER;
VAR p, q: INTEGER;
BEGIN
  IF FreeList[1] # 0 THEN p := FreeList[1];  SYSTEM.GET(FreeList[1]+16, FreeList[1])
  ELSE q := GetBlock128();  SYSTEM.PUT(q+64, 64);  SYSTEM.PUT(q+(64+8), -1);
    SYSTEM.PUT(q+(64+16), 0);  FreeList[1] := q + 64;  p := q
  END;
RETURN p END GetBlock64;


PROCEDURE GetBlock32(): INTEGER;
VAR p, q: INTEGER;
BEGIN
  IF FreeList[0] # 0 THEN p := FreeList[0];  SYSTEM.GET(FreeList[0]+16, FreeList[0])
  ELSE q := GetBlock64();  SYSTEM.PUT(q+32, 32);  SYSTEM.PUT(q+(32+8), -1);
    SYSTEM.PUT(q+(32+16), 0);  FreeList[0] := q + 32;  p := q
  END;
RETURN p END GetBlock32;


PROCEDURE RoundUp(VAR size: INTEGER);
BEGIN
  IF    size < 32  THEN size := 32
  ELSIF size < 64  THEN size := 64
  ELSIF size < 128 THEN size := 128
  ELSIF size < 256 THEN size := 256
                   ELSE size := Align(size, 512)
  END
END RoundUp;


PROCEDURE New*(VAR ptr: INTEGER;  tdAdr: INTEGER);
VAR p, size, need, lim: INTEGER;
BEGIN
  SYSTEM.GET(tdAdr, size);  need := size+16;  RoundUp(need);
  IF    need = 32  THEN p := GetBlock32()
  ELSIF need = 64  THEN p := GetBlock64()
  ELSIF need = 128 THEN p := GetBlock128()
  ELSIF need = 256 THEN p := GetBlock256()
                   ELSE p := GetLargeBlock(need)
  END;
  SYSTEM.PUT(p,   tdAdr);
  SYSTEM.PUT(p+8, 0);
  INC(p, 16);  ptr := p;
  lim := Align(p + size, 8);
  WHILE p < lim DO SYSTEM.PUT(p, 0); INC(p, 8) END;
  INC(Allocated, need);
END New;


(* -------------------------------------------------------------------------- *)
(* --------------------------- Garbage collection --------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Mark and Sweep *)
(* Combined ideas from both N. Wirth's and F. Negele's Garbage Collectors *)

(* Block metadata is 16 bytes for Allocated block *)
(* First word: Type desc address *)
(* Second word: Mark word - 0 is not marked - 1 is marked - otherwise *)
(*              pointer to the next element in marked list            *)

(* For free block is 32 bytes *)
(* First word: Size *)
(* Second word: Mark = -1 *)
(* Third word: Next element in free list *)
(* Fourth word: Unused *)

PROCEDURE Mark(blk: INTEGER);
VAR mark: INTEGER;
BEGIN
  SYSTEM.GET(blk+8, mark);
  IF mark # 0 THEN (* already marked *)
  ELSE SYSTEM.PUT(blk+8, MarkedList);  MarkedList := blk
  END
END Mark;

PROCEDURE TraceMarked;
VAR list, tdAdr, off, ptr, next: INTEGER;
BEGIN list := MarkedList;  MarkedList := MarkedListSentinel;
  WHILE list # MarkedListSentinel DO
    SYSTEM.GET(list, tdAdr);  INC(tdAdr, 64);  SYSTEM.GET(tdAdr, off);
    WHILE off # -1 DO
      SYSTEM.GET(list + 16 + off, ptr);
      DEC(ptr, 16);
      IF ptr >= HeapBase THEN Mark(ptr) END;
      INC(tdAdr, 8);  SYSTEM.GET(tdAdr, off)
    END;
    SYSTEM.GET(list+8, next);  SYSTEM.PUT(list+8, 1);  list := next
  END
END TraceMarked;

PROCEDURE Scan;
VAR p, q, mark, tag, size, heapLimit: INTEGER;
BEGIN
  p := HeapBase;  heapLimit := HeapBase + HeapSize;
  REPEAT SYSTEM.GET(p+8, mark);  q := p;
    WHILE mark = 0 DO
      SYSTEM.GET(p, tag);  SYSTEM.GET(tag, size);
      INC(size, 16);  RoundUp(size);  INC(p, size);
      IF p < heapLimit THEN SYSTEM.GET(p+8, mark) ELSE mark := -1 END
    END;
    size := p - q;  DEC(Allocated, size);  (* size of free block *)
    IF size > 0 THEN
      IF size MOD 64 # 0 THEN
        SYSTEM.PUT(q, 32);  SYSTEM.PUT(q+8, -1);
        SYSTEM.PUT(q+16, FreeList[0]);  FreeList[0] := q;
        INC(q, 32);  DEC(size, 32)
      END;
      IF size MOD 128 # 0 THEN
        SYSTEM.PUT(q, 64);  SYSTEM.PUT(q+8, -1);
        SYSTEM.PUT(q+16, FreeList[1]);  FreeList[1] := q;
        INC(q, 64);  DEC(size, 64)
      END;
      IF size MOD 256 # 0 THEN
        SYSTEM.PUT(q, 128);  SYSTEM.PUT(q+8, -1);
        SYSTEM.PUT(q+16, FreeList[2]);  FreeList[2] := q;
        INC(q, 128);  DEC(size, 128)
      END;
      IF size MOD 512 # 0 THEN
        SYSTEM.PUT(q, 256);  SYSTEM.PUT(q+8, -1);
        SYSTEM.PUT(q+16, FreeList[3]);  FreeList[3] := q;
        INC(q, 256);  DEC(size, 256)
      END;
      IF size > 0 THEN
        SYSTEM.PUT(q, size);  SYSTEM.PUT(q+8, -1);
        SYSTEM.PUT(q+16, LargeFreeList);  LargeFreeList := q;  INC(q, size)
      END
    END;
    IF mark > 0 THEN
      SYSTEM.GET(p, tag);  SYSTEM.GET(tag, size);
      SYSTEM.PUT(p+8, 0);  INC(size, 16);  RoundUp(size);  INC(p, size)
    ELSIF p < heapLimit THEN (*free*) SYSTEM.GET(p, size);  INC(p, size)
    END
  UNTIL p >= heapLimit
END Scan;


PROCEDURE Finalise;
VAR prev, ptr, next: Finalised;  p, mark: INTEGER;
BEGIN ptr := FinalisedList;
  WHILE ptr # NIL DO
    p := SYSTEM.VAL(INTEGER, ptr) - 16;  SYSTEM.GET(p+8, mark);
    IF mark = 0 (* released *) THEN
      next := ptr.next;
      IF prev # NIL THEN prev.next := next
      ELSE FinalisedList := SYSTEM.VAL(Finalised, next)
      END;
      ptr.Finalise(ptr);  ptr := SYSTEM.VAL(Finalised, next)
    ELSE prev := ptr;  ptr := SYSTEM.VAL(Finalised, ptr.next)
    END
  END
END Finalise;


PROCEDURE Collect*;
VAR
  module:  Boot.ModuleHeader;
  modBase: INTEGER;  (* modBase is both the start of the initialised .data    *)
                     (* section and also the limit of the uninitialised (.bss)*)
                     (* data section. Global VARs are Allocated backward from *)
                     (* modBase.                                              *)
  stkDesc, stkBase, ptrTable, off, ptr: INTEGER;
BEGIN
(*IF HeapTracer # NIL THEN HeapTracer(0) END;*)  (* Trace collect call *)
  module := Boot.FirstModule;
  WHILE module # NIL DO
    modBase := module.base;
    (* Loop through list of traced data items.                                *)

    (* At modBase+112 is ptrTable, a list of pointer offsets relative to      *)
    (* modBase. Each non-nil pointer addresses a dynamically Allocated block  *)
    (* which is preceeded by two 64 bit integers of block metadata, being the *)
    (* type descriptor address and the mark word.                             *)
    SYSTEM.GET(modBase + 112, ptrTable);
    SYSTEM.GET(ptrTable, off);
    WHILE off # -1 DO
      SYSTEM.GET(modBase + off, ptr);
      DEC(ptr, 16);    (* Address block metadata (or -16 if ptr was NIL) *)
      IF ptr >= HeapBase THEN Mark(ptr) END;
      INC(ptrTable, 8);  SYSTEM.GET(ptrTable, off)
    END;

    (* At modBase+104 is stkDesc, a linked list of stack descriptions:        *)
    (*   64/stack base                                                        *)
    (*   64/offset relative to stack base of table of pointers                *)
    (*   64/next stack base                                                   *)
    (* Each pointer table works much as the pointer table for the module base *)
    (* except that the pointers are offsets relative to the current stack base*)
    SYSTEM.GET(modBase + 104, stkDesc);
    WHILE stkDesc # 0 DO SYSTEM.GET(stkDesc, stkBase);
      SYSTEM.GET(stkDesc + 8, ptrTable);  SYSTEM.GET(ptrTable, off);
      WHILE off # -1 DO
        SYSTEM.GET(stkBase + off, ptr);  DEC(ptr, 16);
        IF ptr >= HeapBase THEN Mark(ptr) END;
        INC(ptrTable, 8);  SYSTEM.GET(ptrTable, off)
      END;
      SYSTEM.GET(stkDesc + 16, stkDesc)
    END;
    module := module.next
  END;

  WHILE MarkedList # MarkedListSentinel DO TraceMarked END;

  Finalise;
  Scan;
  JustCollected := TRUE
END Collect;


PROCEDURE RegisterFinalised*(ptr: Finalised;  finalise: FinaliseProc);
BEGIN
  ASSERT(finalise # NIL);  ptr.Finalise := finalise;
  ptr.next := SYSTEM.VAL(Finalised, FinalisedList);  FinalisedList := ptr
END RegisterFinalised;


(* -------------------------------------------------------------------------- *)
(* -------------------------- Windows command line -------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE GetArg*(n: INTEGER; VAR str: ARRAY OF CHAR);
VAR i, argAdr: INTEGER;  str16: ARRAY 1024 OF SYSTEM.CARD16;
BEGIN
  IF (n < 0) OR (n >= NumArgs) THEN
    str := ""
  ELSE
    SYSTEM.GET(ArgV + n * 8, argAdr);
    i := 0;  SYSTEM.GET(argAdr, str16[i]);
    WHILE str16[i] # 0 DO
      INC(argAdr, 2);  INC(i);  SYSTEM.GET(argAdr, str16[i])
    END;
    i := Utf16ToUtf8(str16, str)
  END;
END GetArg;


(* -------------------------------------------------------------------------- *)
(* --------- Time in 100nS ticks since since January 1, 1601 (UTC) ---------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Time*(): INTEGER;
VAR tick: INTEGER;
BEGIN GetSystemTimePreciseAsFileTime(SYSTEM.ADR(tick));
RETURN tick END Time;

PROCEDURE TimeToMSecs*(time: INTEGER): INTEGER;
  RETURN time DIV 10000
END TimeToMSecs;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Halt*(exitCode: INTEGER);
BEGIN  Finalise;  Boot.PEImports.ExitProcess(exitCode)
END Halt;

(* -------------------------------------------------------------------------- *)
(* ------- Kernel initialisation code - called following kernel link -------- *)
(* -------------------------------------------------------------------------- *)

BEGIN
  (* Set up some useful exports from standard procedures. *)
  Kernel := Boot.PEImports.LoadLibraryA(SYSTEM.ADR("kernel32.dll"));
  User   := Boot.PEImports.LoadLibraryA(SYSTEM.ADR("user32.dll"));
  Shell  := Boot.PEImports.LoadLibraryA(SYSTEM.ADR("shell32.dll"));

  (* Initialise exception/trap handling *)
  SYSTEM.PUT(SYSTEM.ADR(MessageBoxW),                 Boot.PEImports.GetProcAddress(User,   SYSTEM.ADR("MessageBoxW")));
  SYSTEM.PUT(SYSTEM.ADR(AddVectoredExceptionHandler), Boot.PEImports.GetProcAddress(Kernel, SYSTEM.ADR("AddVectoredExceptionHandler")));

  AddVectoredExceptionHandler(1, ExceptionHandler);

  (* Initialise Heap and GC *)
  SYSTEM.PUT(SYSTEM.ADR(VirtualAlloc), Boot.PEImports.GetProcAddress(Kernel, SYSTEM.ADR("VirtualAlloc")));

(*InitHeap(80000000H, 80000H);*)(* Reserve 2GB, commit 512KB *)
  InitHeap(80000000H, 2000000H);  (* Reserve 2GB, commit 32MB *)
  Collect0 := Collect;

  (* Initialise command line access *)
  SYSTEM.PUT(SYSTEM.ADR(GetCommandLineW), Boot.PEImports.GetProcAddress(Kernel, SYSTEM.ADR("GetCommandLineW")));

  SYSTEM.PUT(SYSTEM.ADR(CommandLineToArgvW), Boot.PEImports.GetProcAddress(Shell, SYSTEM.ADR("CommandLineToArgvW")));

  CommandAdr := GetCommandLineW();
  NumArgs    := 0;
  ArgV       := CommandLineToArgvW(CommandAdr, SYSTEM.ADR(NumArgs));

  (* System time (precise) *)
  SYSTEM.PUT(SYSTEM.ADR(GetSystemTimePreciseAsFileTime), Boot.PEImports.GetProcAddress(Kernel, SYSTEM.ADR("GetSystemTimePreciseAsFileTime")));

  (* Install New *)
  SYSTEM.PUT(SYSTEM.ADR(Boot.PEImports.New), New)
END Kernel.
