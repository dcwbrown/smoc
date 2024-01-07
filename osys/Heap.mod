MODULE Heap;  (*$RTL-*)

IMPORT SYSTEM, Boot;


CONST
  MarkedListSentinel = 2;  (* Used to mark end of list of marked heap blocks during GC *)
  HeapReserve = 80000000H; (* 2GB   *)
(*HeapCommit  = 80000H;    (* 512KB *) *)
  HeapCommit  = 2000000H;  (* 32MB *)


TYPE
  ExceptionHandlerProc = PROCEDURE(p: INTEGER): INTEGER;

  (* Finalisation during garbage collection *)
  Finalised*       = POINTER TO FinalisedDesc;
  FinaliseProc*    = PROCEDURE(ptr: Finalised);
  FinalisedDesc*   = RECORD
                       Finalise: FinaliseProc;
                       next:     Finalised
                     END;
  HeapTraceHandler = PROCEDURE(reason: INTEGER);

  WindowsAllocator* = PROCEDURE(lpAddress, dwSize,
                               flAllocationType, flProtect: INTEGER): INTEGER;

VAR
  VirtualAlloc:  WindowsAllocator;
  HeapBase*:     INTEGER;
  HeapSize*:     INTEGER;            (* Committed heap memory                        *)
  HeapMax:       INTEGER;            (* HeapSize to HeapMax reserved, not committed  *)
  FreeList:      ARRAY 4 OF INTEGER; (* Free lists for 32, 64, 128 & 256 byte blocks *)
  LargeFreeList: INTEGER;            (* Free list for 512 byte and larger blocks     *)
  Allocated:     INTEGER;            (* Includes 16 byte header but excludes padding *)

  (* Garbage collection *)
  MarkedList:    INTEGER;
  Collect0:      PROCEDURE;
  JustCollected: BOOLEAN;
  FinalisedList: Finalised;
  HeapTracer:    HeapTraceHandler;


(* -------------------------------------------------------------------------- *)
(* --------------------------- Memory allocation ---------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Align(a: INTEGER;  align: INTEGER): INTEGER;
BEGIN ASSERT(a >= 0);
RETURN (a + align - 1) DIV align * align END Align;


PROCEDURE InstallHeapTraceHandler*(tracer: HeapTraceHandler);
BEGIN HeapTracer := tracer END InstallHeapTraceHandler;


PROCEDURE InitHeap*(allocator: WindowsAllocator);
CONST MEMRESERVE = 2000H;  MEMCOMMIT = 1000H;  PAGEREADWRITE = 4;
VAR i, p: INTEGER;
BEGIN
  VirtualAlloc := allocator;

  (* Reserve address space for later use as heap space *)
  HeapMax  := HeapReserve;
  HeapBase := VirtualAlloc(0, HeapMax, MEMRESERVE, PAGEREADWRITE);
  ASSERT(HeapBase # 0);

  HeapSize := HeapCommit;
  HeapBase := VirtualAlloc(HeapBase, HeapSize, MEMCOMMIT, PAGEREADWRITE);
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
CONST MEMCOMMIT = 1000H;  PAGEREADWRITE = 4;
VAR p, mark, size, prev, p2: INTEGER;
BEGIN
  p := VirtualAlloc(HeapBase + HeapSize, HeapSize, MEMCOMMIT, PAGEREADWRITE);
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
(* ------------------------------ Finalisation ------------------------------ *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Finalise*;
VAR prev, ptr, next: Finalised;  p, mark: INTEGER;
BEGIN ptr := FinalisedList;
  WHILE ptr # NIL DO
    p := SYSTEM.ADR(ptr^) (*SYSTEM.VAL(INTEGER, ptr)*) - 16;
    SYSTEM.GET(p+8, mark);
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


PROCEDURE RegisterFinalised*(ptr: Finalised;  finalise: FinaliseProc);
BEGIN
  ASSERT(finalise # NIL);
  ptr.Finalise := finalise;
  ptr.next     := FinalisedList;
  FinalisedList := ptr
END RegisterFinalised;




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
  ASSERT(blk # 0);
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


PROCEDURE Collect*;
VAR
  (*module:  Boot.ModuleHeader;*)
  modhdr:  INTEGER;
  modBase: INTEGER;  (* modBase is both the start of the initialised .data    *)
                     (* section and also the limit of the uninitialised (.bss)*)
                     (* data section. Global VARs are Allocated backward from *)
                     (* modBase.                                              *)
  stkDesc, stkBase, ptrTable, off, ptr: INTEGER;
BEGIN
  IF HeapTracer # NIL THEN HeapTracer(0) END;  (* Trace collect call *)

  (*
  module := SYSTEM.VAL(Boot.ModuleHeader, Boot.BootHeader);
  WHILE module # NIL DO
    modBase := module.base;
  *)

  modhdr := Boot.BootHeader;
  WHILE modhdr # 0 DO
    SYSTEM.GET(modhdr + Boot.OffModBase, modBase);


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
    (*module := module.next*)
    SYSTEM.GET(modhdr + Boot.OffModNext, modhdr)
  END;

  WHILE MarkedList # MarkedListSentinel DO TraceMarked END;

  Finalise;
  Scan;
  JustCollected := TRUE
END Collect;




(* -------------------------------------------------------------------------- *)
(* ------- Heap initialisation code - called following kernel link -------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE GetProc*(dll: INTEGER; name: ARRAY OF CHAR; VAR proc: ARRAY OF BYTE);
BEGIN
  SYSTEM.PUT(SYSTEM.ADR(proc), Boot.GetProcAddress(dll, SYSTEM.ADR(name)))
END GetProc;

BEGIN Collect0 := Collect;
END Heap.
