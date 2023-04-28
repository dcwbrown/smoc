MODULE Rtl;  (* multi-threaded application NOT SUPPORTED *)
(*$RTL-*)
IMPORT SYSTEM;

CONST
  MinInt = 8000000000000000H;  MaxInt = 7FFFFFFFFFFFFFFFH;

  HeapMax = 80000000H;  blkMeta = 16;  MarkedListSentinel = 2;

  GENERIC_READ = {31};  GENERIC_WRITE = {30};
  MEM_RESERVE = 2000H;  MEM_COMMIT = 1000H;  PAGE_READWRITE = 4;

TYPE
  Handle  = INTEGER;
  Pointer = INTEGER;
  Bool    = SYSTEM.CARD32;
  Int     = SYSTEM.CARD32;
  Dword   = SYSTEM.CARD32;
  Ulong   = SYSTEM.CARD32;
  Uint    = SYSTEM.CARD32;

  Finalised*     = POINTER [untraced] TO FinalisedDesc;
  FinaliseProc*  = PROCEDURE(ptr: Finalised);
  FinalisedDesc* = RECORD
                     Finalise: FinaliseProc;
                     next:     Finalised
                   END;

  HeapTraceHandler = PROCEDURE(action: INTEGER);

VAR
  modList*, nMod*:    INTEGER;
  argv,     NumArgs*: INTEGER;
  finalisedList:      Finalised;

  (* Utility *)
  ExitProcess:             PROCEDURE(uExitCode: Uint);
  MessageBoxW:             PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER): Int;
  GetSystemTimeAsFileTime: PROCEDURE(lpSystemTimeAsFileTime: Pointer);
  GetCommandLineW:         PROCEDURE(): Pointer;
  CommandLineToArgvW:      PROCEDURE(lpCmdLine, pNumArgs: Pointer): Pointer;

  AddVectoredExceptionHandler: PROCEDURE(FirstHandler:    Ulong;
                                         VectoredHandler: Pointer);

  Kernel*: INTEGER;
  User*:   INTEGER;
  Shell*:  INTEGER;

  (* Heap *)
  VirtualAlloc: PROCEDURE(lpAddress, dwSize, flAllocationType, flProtect: INTEGER): Pointer;

  HeapBase*:     INTEGER;
  HeapSize*:     INTEGER;
  MarkedList:    INTEGER;
  Allocated*:    INTEGER;
  FreeList:      ARRAY 4 OF INTEGER;
  LargeFreeList: INTEGER;
  Collect0:      PROCEDURE;
  JustCollected: BOOLEAN;

  (* Tracing *)
  HeapTracer:  HeapTraceHandler;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Unicode *)

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
(* -------------------------------------------------------------------------- *)
(* Utility procedures *)

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


PROCEDURE GetArg*(n: INTEGER; VAR out: ARRAY OF CHAR);
VAR i, arg: INTEGER;  str16: ARRAY 1024 OF SYSTEM.CARD16;
BEGIN (* GetArg *)
  ASSERT(argv # 0);
  IF (n < NumArgs) & (n >= 0) THEN
    i := 0;
    SYSTEM.GET(argv+n*8, arg);  ASSERT(arg # 0);
    SYSTEM.GET(arg, str16[i]);
    WHILE str16[i] # 0 DO INC(arg, 2);  INC(i);  SYSTEM.GET(arg, str16[i]) END
  ELSE str16[0] := 0
  END;
  i := Utf16ToUtf8(str16, out)
END GetArg;


PROCEDURE Time*(): INTEGER;
VAR tick: INTEGER;
BEGIN
  GetSystemTimeAsFileTime(SYSTEM.ADR(tick));
  RETURN tick
END Time;


PROCEDURE TimeToMSecs*(time: INTEGER): INTEGER;
  RETURN time DIV 10000
END TimeToMSecs;


(* -------------------------------------------------------------------------- *)

PROCEDURE LowerCase*(VAR str: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN
  WHILE (i < LEN(str)) & (str[i] # 0X) DO
    IF (ORD(str[i]) >= ORD("a"))  &  (ORD(str[i]) <= ORD("z")) THEN
      str[i] := CHR(ORD(str[i]) - 20H)
    END;
    INC(i)
  END
END LowerCase;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Module management *)

PROCEDURE Register*(modAdr: INTEGER);
BEGIN
  ASSERT(nMod < 512);
  IF modList = 0 THEN
    modList := VirtualAlloc(0, 4096, MEM_COMMIT, PAGE_READWRITE);
    nMod := 0;  ASSERT(modList # 0)
  END;
  INC(nMod);  SYSTEM.PUT(modList+nMod*8-8, modAdr)
END Register;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Heap management *)
(* Based on Kernel.mod in Project Oberon 2013 *)

PROCEDURE Align(a: INTEGER;  align: INTEGER): INTEGER;
VAR result: INTEGER;
BEGIN
  IF    a > 0 THEN result := (a + align - 1) DIV align * align
  ELSIF a < 0 THEN result :=        a        DIV align * align
  END
RETURN result END Align;

PROCEDURE InstallHeapTraceHandler*(tracer: HeapTraceHandler);
BEGIN HeapTracer := tracer END InstallHeapTraceHandler;

PROCEDURE ExtendHeap;
VAR p, mark, size, prev, p2: INTEGER;
BEGIN
  p := VirtualAlloc(HeapBase + HeapSize, HeapSize, MEM_COMMIT, PAGE_READWRITE);
  ASSERT(HeapSize < HeapMax);  ASSERT(p # 0);
  SYSTEM.PUT(p, HeapSize);  SYSTEM.PUT(p+8, -1);  SYSTEM.PUT(p+16, 0);
  IF LargeFreeList = 0 THEN LargeFreeList := p
  ELSE prev := LargeFreeList;  SYSTEM.GET(LargeFreeList+16, p2);
    WHILE p2 # 0 DO prev := p2;  SYSTEM.GET(p2+16, p2) END;
    SYSTEM.PUT(prev+16, p)
  END;
  HeapSize := HeapSize*2
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
BEGIN SYSTEM.GET(blk+8, mark);
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
      SYSTEM.GET(list+blkMeta+off, ptr);  DEC(ptr, blkMeta);
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
BEGIN ptr := finalisedList;
  WHILE ptr # NIL DO
    p := SYSTEM.VAL(INTEGER, ptr) - 16;  SYSTEM.GET(p+8, mark);
    IF mark = 0 (* released *) THEN
      next := ptr.next;
      IF prev # NIL THEN prev.next := next
      ELSE finalisedList := SYSTEM.VAL(Finalised, next)
      END;
      ptr.Finalise(ptr);  ptr := SYSTEM.VAL(Finalised, next)
    ELSE prev := ptr;  ptr := SYSTEM.VAL(Finalised, ptr.next)
    END
  END
END Finalise;

PROCEDURE Collect*;
VAR
  i: INTEGER;
  modBase: INTEGER;  (* modBase is both the start of the initialised .data    *)
                     (* section and also the limit of the uninitialised (.bss)*)
                     (* data section. Global VARs are Allocated backward from *)
                     (* modBase.                                              *)
  stkDesc, stkBase, ptrTable, off, ptr: INTEGER;
BEGIN
  IF HeapTracer # NIL THEN HeapTracer(0) END;  (* Trace collect call *)
  i := 0;
  WHILE i < nMod DO SYSTEM.GET(modList+i*8, modBase);
    (* Loop through list of traced data items.                                *)

    (* At modBase+112 is ptrTable, a list of pointer offsets relative to      *)
    (* modBase. Each non-nil pointer addresses a dynamically Allocated block  *)
    (* which is preceeded by two 64 bit integers of block metadata, being the *)
    (* type descriptor address and the mark word.                             *)
    SYSTEM.GET(modBase+112, ptrTable);
    SYSTEM.GET(ptrTable, off);
    WHILE off # -1 DO
      SYSTEM.GET(modBase+off, ptr);
      DEC(ptr, blkMeta);    (* Address block metadata (or -16 if ptr was NIL) *)
      IF ptr >= HeapBase THEN Mark(ptr) END;
      INC(ptrTable, 8);  SYSTEM.GET(ptrTable, off)
    END;

    (* At modBase+114 is stkDesc, a linked list of stack descriptions:        *)
    (*   64/stack base                                                        *)
    (*   64/offset relative to stack base of table of pointers                *)
    (*   64/next stack base                                                   *)
    (* Each pointer table works much as the pointer table for the module base *)
    (* except that the pointers are offsets relative to the current stack base*)
    SYSTEM.GET(modBase+104, stkDesc);
    WHILE stkDesc # 0 DO SYSTEM.GET(stkDesc, stkBase);
      SYSTEM.GET(stkDesc+8, ptrTable);  SYSTEM.GET(ptrTable, off);
      WHILE off # -1 DO
        SYSTEM.GET(stkBase+off, ptr);  DEC(ptr, blkMeta);
        IF ptr >= HeapBase THEN Mark(ptr) END;
        INC(ptrTable, 8);  SYSTEM.GET(ptrTable, off)
      END;
      SYSTEM.GET(stkDesc+16, stkDesc)
    END;
    INC(i)
  END;
  WHILE MarkedList # MarkedListSentinel DO TraceMarked END;
  Finalise;  Scan;  JustCollected := TRUE
END Collect;

PROCEDURE RegisterFinalised*(ptr: Finalised;  finalise: FinaliseProc);
BEGIN
  ASSERT(finalise # NIL);  ptr.Finalise := finalise;
  ptr.next := SYSTEM.VAL(Finalised, finalisedList);  finalisedList := ptr
END RegisterFinalised;

(* -------------------------------------------------------------------------- *)

PROCEDURE InitHeap;
VAR i, p: INTEGER;
BEGIN
  (* Reserve 2GB of address space for later use as heap space *)
  HeapBase := VirtualAlloc(0, HeapMax, MEM_RESERVE, PAGE_READWRITE);
  ASSERT(HeapBase # 0);

  (* Allocate first 512KB for initial heap *)
(*HeapSize := 80000H;*)
  HeapSize := 2000000H;  (* 32MB *)
  HeapBase := VirtualAlloc(HeapBase, HeapSize, MEM_COMMIT, PAGE_READWRITE);
  ASSERT(HeapBase # 0);

  FOR i := 0 TO LEN(FreeList)-1 DO FreeList[i] := 0 END;
  p := HeapBase;  LargeFreeList := HeapBase;  Allocated := 0;
  SYSTEM.PUT(p, HeapSize);  SYSTEM.PUT(p+8, -1);  SYSTEM.PUT(p+16, 0);
  MarkedList := MarkedListSentinel;  Collect0 := Collect
END InitHeap;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Halt*(exitCode: INTEGER);
BEGIN
  nMod := 0;  Collect;
  ExitProcess(exitCode)
END Halt;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Init *)

PROCEDURE GetArgv;
VAR pCmdLine: Pointer;
BEGIN
  pCmdLine := GetCommandLineW();
  argv := CommandLineToArgvW(pCmdLine, SYSTEM.ADR(NumArgs))
END GetArgv;

PROCEDURE InitWin32;
BEGIN
  SYSTEM.LoadLibraryA(Kernel, "kernel32.dll");
  SYSTEM.LoadLibraryA(User,   "user32.dll");
  SYSTEM.LoadLibraryA(Shell,  "shell32.dll");

  SYSTEM.GetProcAddress(ExitProcess,                 Kernel, SYSTEM.ADR("ExitProcess"));                 ASSERT(ExitProcess                 # NIL);
  SYSTEM.GetProcAddress(AddVectoredExceptionHandler, Kernel, SYSTEM.ADR("AddVectoredExceptionHandler")); ASSERT(AddVectoredExceptionHandler # NIL);
  SYSTEM.GetProcAddress(MessageBoxW,                 User,   SYSTEM.ADR("MessageBoxW"));                 ASSERT(MessageBoxW                 # NIL);
  SYSTEM.GetProcAddress(GetSystemTimeAsFileTime,     Kernel, SYSTEM.ADR("GetSystemTimeAsFileTime"));     ASSERT(GetSystemTimeAsFileTime     # NIL);
  SYSTEM.GetProcAddress(GetCommandLineW,             Kernel, SYSTEM.ADR("GetCommandLineW"));             ASSERT(GetCommandLineW             # NIL);
  SYSTEM.GetProcAddress(CommandLineToArgvW,          Shell,  SYSTEM.ADR("CommandLineToArgvW"));          ASSERT(CommandLineToArgvW          # NIL);
  SYSTEM.GetProcAddress(VirtualAlloc,                Kernel, SYSTEM.ADR("VirtualAlloc"));                ASSERT(VirtualAlloc                # NIL);
END InitWin32;

BEGIN
  InitWin32;  GetArgv;  InitHeap
END Rtl.
