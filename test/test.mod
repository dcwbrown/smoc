MODULE test;  (*$CONSOLE*)

IMPORT Out, SYSTEM, Rtl;

TYPE
  intrec = POINTER TO intrecdesc;
  intrecdesc = RECORD
                 i: INTEGER;
                 n: intrec
               END;
VAR
  i: INTEGER;
  j: intrec;
  k: intrecdesc;
  s: ARRAY 10 OF CHAR;
  c: CHAR;
  memstart: INTEGER;
  memlimit: INTEGER; (* Keep this as the last global variable *)

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

PROCEDURE Do*; BEGIN wsl("Teapots greeted.") END Do;


(* -------------------------------------------------------------------------- *)

PROCEDURE InitialiseMemoryRange;
VAR stackvar: INTEGER;
BEGIN
  memstart := SYSTEM.ADR(stackvar) - 1000H;  (* 1000H is stack reserve in PE header *)
  memlimit := Rtl.heapBase + Rtl.heapSize;
  ws("memstart: $"); wh(memstart); ws(", memlimit $"); wh(memlimit); wsl(".");
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

PROCEDURE dump(indent, adr, len: INTEGER);
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

    (* One row of hex dump *)
    FOR i := 0 TO 15 DO
      IF i MOD 8 = 0 THEN wc(" ") END;
      IF bytes[i] >= 0 THEN wh2(bytes[i]);  wc(" ") ELSE ws("   ") END;
    END;
    ws("  ");

    (* One row of character dump *)
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
END dump;

(* -------------------------------------------------------------------------- *)

PROCEDURE FillAnsi(s: ARRAY OF CHAR; VAR ansi: ARRAY OF BYTE);
VAR i: INTEGER;
BEGIN
  i := 0;
  WHILE (i < LEN(s)) & (i < LEN(ansi)) & (s[i] # 0X) DO
    ansi[i] := ORD(s[i]);  INC(i)
  END;
  IF i < LEN(ansi) THEN ansi[i] := 0 END
END FillAnsi;

PROCEDURE GetModuleInfo;
VAR
  kernel32: INTEGER;
  psapi:    INTEGER;
  result:   INTEGER;
  nModules: INTEGER;
  hProcess: INTEGER;
  hModule:  INTEGER;

  ExportsRva: INTEGER;
  CodeRva:    INTEGER;
  TrapsRva:   INTEGER;
  ModKey0:    INTEGER;
  ModKey1:    INTEGER;

  cbneeded: SYSTEM.CARD32;
  i:        INTEGER;
  ansi:     ARRAY 256 OF BYTE;
  modules:  ARRAY 100 OF INTEGER;
  name:     ARRAY 256 OF CHAR;

  GetCurrentProcess: PROCEDURE(): INTEGER;
  GetModuleHandleW:  PROCEDURE(name: INTEGER): INTEGER;

  EnumProcessModulesEx: PROCEDURE(
        hProcess: INTEGER;
        hModules: INTEGER;
        cb:       SYSTEM.CARD32;
    VAR cbNeeded: SYSTEM.CARD32;
        filter:   SYSTEM.CARD32
  ): INTEGER;

  GetModuleFileNameExW: PROCEDURE(
        hProcess:    INTEGER;
        hModule:     INTEGER;
        adrfilename: INTEGER;
        size:        INTEGER
  ): INTEGER;

BEGIN
  SYSTEM.LoadLibraryW(kernel32, "KERNEL32.DLL");  ASSERT(kernel32 # 0);

  FillAnsi("GetModuleHandleW", ansi);
  SYSTEM.GetProcAddress(GetModuleHandleW, kernel32, SYSTEM.ADR(ansi));
  ASSERT(GetModuleHandleW # NIL);

  hModule := GetModuleHandleW(0);
  ws("Main program module handle: $"); wh(hModule); wsl(".");

  (* Oberon specific fields *)
  wsl("Main program Oberon specific header fields:");
  ExportsRva := getint(hModule+3D8H); ws("  ExportsRva $"); wh(ExportsRva); wsl(".");
  CodeRva    := getint(hModule+3E0H); ws("  CodeRva    $"); wh(CodeRva);    wsl(".");
  TrapsRva   := getint(hModule+3E8H); ws("  TrapsRva   $"); wh(TrapsRva);   wsl(".");
  ModKey0    := getint(hModule+3F0H); ws("  ModKey0    $"); wh(ModKey0);    wsl(".");
  ModKey1    := getint(hModule+3F8H); ws("  ModKey1    $"); wh(ModKey1);    wsl(".");

  FillAnsi("GetCurrentProcess", ansi);
  SYSTEM.GetProcAddress(GetCurrentProcess, kernel32, SYSTEM.ADR(ansi));
  ASSERT(GetCurrentProcess # NIL);

  hProcess := GetCurrentProcess();

  SYSTEM.LoadLibraryW(psapi, "PSAPI.DLL");  ASSERT(psapi # 0);

  FillAnsi("EnumProcessModulesEx", ansi);
  SYSTEM.GetProcAddress(EnumProcessModulesEx, psapi, SYSTEM.ADR(ansi));
  ASSERT(EnumProcessModulesEx # NIL);

  FillAnsi("GetModuleFileNameExW", ansi);
  SYSTEM.GetProcAddress(GetModuleFileNameExW, psapi, SYSTEM.ADR(ansi));
  ASSERT(GetModuleFileNameExW # NIL);

  ASSERT(EnumProcessModulesEx(
    hProcess, SYSTEM.ADR(modules), LEN(modules)*8,
    cbneeded, 3
  ) # 0);
  nModules := cbneeded DIV 8;
  ASSERT(nModules <= LEN(modules));
  wsl("Module addresses:");
  FOR i := 0 TO nModules-1 DO
    ws("  $"); wh12(modules[i]);
    ws(", ");
    ASSERT(GetModuleFileNameExW(-1, modules[i], SYSTEM.ADR(name), LEN(name)) # 0);
    ws(name);
    wsl(".")
  END
END GetModuleInfo;


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

PROCEDURE ShowExports(indent, imageBase, edata: INTEGER);
VAR
  i, nameAdr: INTEGER;
  dir: POINTER [untraced] TO RECORD
         flags:       SYSTEM.CARD32;
         timeDate:    SYSTEM.CARD32;
         majorVer:    SYSTEM.CARD16;
         minorVer:    SYSTEM.CARD16;
         nameRva:     SYSTEM.CARD32;
         ordinalBase: SYSTEM.CARD32;
         nAddresses:  SYSTEM.CARD32;
         nNames:      SYSTEM.CARD32;
         addresses:   SYSTEM.CARD32;
         names:       SYSTEM.CARD32;
         ordinals:    SYSTEM.CARD32
       END;
BEGIN
  SYSTEM.PUT(SYSTEM.ADR(dir), edata);
  (*
  ws("  nameRva:     $");  wh(dir.nameRva);      wsl(".");
  ws("  ordinalBase: $");  wh(dir.ordinalBase);  wsl(".");
  ws("  nAddresses:  $");  wh(dir.nAddresses);   wsl(".");
  ws("  nNames:      $");  wh(dir.nNames);       wsl(".");
  ws("  addresses:   $");  wh(dir.addresses);    wsl(".");
  ws("  names:       $");  wh(dir.names);        wsl(".");
  ws("  ordinals:    $");  wh(dir.ordinals);     wsl(".");
  *)

  wb(indent); ws("Module file name: '"); WriteAnsiName(imageBase + dir.nameRva); wsl("'.");

  wb(indent); wsl("Addresses:");
  i := 0;  WHILE i < dir.nAddresses DO
    wb(indent+2); ws("["); wi(i); ws("]: $");
    wh(getdword(imageBase + dir.addresses + i*4)); wsl(".");
    INC(i)
  END;

  wb(indent); wsl("Names:");
  i := 0;  WHILE i < dir.nNames DO
    nameAdr := getdword(imageBase + dir.names + i*4);
    wb(indent+2); ws("["); wi(i); ws("]: $"); wh(nameAdr); ws(", '");
    WriteAnsiName(imageBase + nameAdr); ws("' ordinal ");
    wi(getword(imageBase + dir.ordinals + i*2));
    wsl(".");
    INC(i)
  END;
END ShowExports;

(* -------------------------------------------------------------------------- *)

PROCEDURE ShowPEtables;
TYPE
VAR
  imageBase:           INTEGER;
  PEAddress:           INTEGER;
  BaseAddress:         INTEGER;
  nSections:           SYSTEM.CARD16;
  sizeOptional:        SYSTEM.CARD16;
  SectionTableAddress: INTEGER;
  adr:                 INTEGER;
  i, j, a:             INTEGER;
  name:                ARRAY 8 OF CHAR;
  ch:                  CHAR;
  ExportTableAddress:  INTEGER;
  ExportTableSize:     INTEGER;
BEGIN
  (* The .exe is usually loaded at $400000 with the first section at $401000 *)
  (* A global data var will normally be in this first section, so find the   *)
  (* address of the lowest global var and mask off the bottom 16 bits and    *)
  (* we should be left with the .exe load address.                           *)
  (* The PE header is $80 bytes further on, after the MSDOS compatibility    *)
  (* heasder.                                                                *)
  imageBase := (SYSTEM.ADR(memlimit) DIV 10000H) * 10000H;
  ws("Load address: $"); wh(imageBase); wsl(".");
  IF imageBase # 400000H THEN wsl("  -- unexpected load address!") END;
  wsl("MS-DOS Stub:");
  dump(2, imageBase, 64);

  PEAddress := imageBase + getdword(imageBase + 3CH);
  ws("PE address $"); wh(PEAddress); wsl(".");
  wsl("PE header:");
  dump(2, PEAddress, 24);
  nSections    := getword(PEAddress + 6);
  ws("Number of sections: "); wi(nSections); wsl(".");

  sizeOptional := getword(PEAddress + 20);
  IF sizeOptional > 0 THEN
    ws("Optional header size "); wi(sizeOptional); wsl(" bytes:");
    dump(2, PEAddress + 24, sizeOptional);
  END;

  ExportTableAddress := 0;
  BaseAddress        := 0;
  SectionTableAddress := PEAddress + 24 + sizeOptional;
  wsl("Section table:");
  dump(2, SectionTableAddress, 40 * nSections);
  i := 0;
  WHILE i < nSections DO
    ws("  ");
    a := SectionTableAddress + 40 * i;
    FOR j := 0 TO 7 DO
      ch := CHR(getbyte(a+j));  name[j] := ch;
      IF ch = 0X THEN ch := " " END;
      wc(ch)
    END;
    ws(' at $');            wh(getdword(a+12));
    ws(', virtual size $'); wh(getdword(a+8));
    wl;
    IF name = ".edata" THEN
      ExportTableAddress := imageBase + getdword(a+12);
      ExportTableSize    := getdword(a+8);
    ELSIF name = '.data' THEN
      BaseAddress := imageBase + getdword(a+12);
    END;
    INC(i)
  END;

  IF BaseAddress # 0 THEN
    wsl("Initialised data (at base address) starts:");
    dump(2, BaseAddress, 128);

    wsl("  Fixed address values at start of initialised data:");
      ws(" [0]   GetProcAddress              $"); wh(getint(BaseAddress + 0));   wsl(".");
      ws(" [8]   LoadLibraryW                $"); wh(getint(BaseAddress + 8));   wsl(".");
      ws(" [16]  ExitProcess                 $"); wh(getint(BaseAddress + 16));  wsl(".");
      ws(" [24]  GetModuleHandleExW          $"); wh(getint(BaseAddress + 24));  wsl(".");
      ws(" [32]  AddVectoredExceptionHandler $"); wh(getint(BaseAddress + 32));  wsl(".");
      ws(" [80]  InitGlobals RVA             $"); wh(getint(BaseAddress + 80));  wsl(".");
      ws(" [88]  MessageBoxW                 $"); wh(getint(BaseAddress + 88));  wsl(".");
      ws(" [96]  wsprintfW                   $"); wh(getint(BaseAddress + 96));  wsl(".");
      ws(" [104] adrOfStackPtrList           $"); wh(getint(BaseAddress + 104)); wsl(".");
      ws(" [112] adrOfPtrTable               $"); wh(getint(BaseAddress + 112)); wsl(".");
      ws(" [120] adrOfNEW                    $"); wh(getint(BaseAddress + 120)); wsl(".");
  END;

  IF ExportTableAddress # 0 THEN
    wsl("  Exports:");
    ShowExports(4, imageBase, ExportTableAddress)
  END;

END ShowPEtables;

(* -------------------------------------------------------------------------- *)

PROCEDURE ShowOberonModules;
VAR module, baseadr, loadadr, export, i: INTEGER;
BEGIN
  module := Rtl.modList;
  wsl("Oberon registered module list (excludes modules copiled with $RTL-):");
  ws("  Rtl.modList: $"); wh(Rtl.modList); ws(", Rtl.nMod: "); wi(Rtl.nMod); wsl(".");
  FOR i := 0 TO Rtl.nMod-1 DO
    baseadr := getint(module + 8*i);
    ws("  base address $"); wh12(baseadr);
    loadadr := baseadr - getint(baseadr+80);
    ws(", load address $"); wh12(loadadr);
    export := loadadr + getint(loadadr + 3D8H);
    wsl(", exports:");
    ShowExports(4, loadadr, export);
  END
END ShowOberonModules;


(* -------------------------------------------------------------------------- *)

(* NOTE: HeapTrace is called during (at the start of) collection so           *)
(*       MUST NOT cause any heap allocation.                                  *)

PROCEDURE DumpPointerTable(title: ARRAY OF CHAR; indent, base, table: INTEGER);
VAR offset, ptr, descriptor, size: INTEGER;
BEGIN
  offset := getint(table);
  IF offset # -1 THEN
    wb(indent);  ws(title);  wl;
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


PROCEDURE HeapTrace(action: INTEGER);
VAR i, modBase, ptrTable, offset, ptr, typedesc, stkDesc, stkBase: INTEGER;
BEGIN
  IF Rtl.nMod # 0 THEN
    wl; ws("Heap trace callback. modList: $"); wh(Rtl.modList);
    ws(", nMod: "); wi(Rtl.nMod); wsl('.');

    FOR i := 0 TO Rtl.nMod-1 DO
      modBase := getint(Rtl.modList+8*i);
      ws("  module ["); wi(i);
      ws("] at $");     wh(modBase);
      ws(" '");         WriteModuleName(modBase);
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


PROCEDURE NestedCollect;
  VAR p: intrec;
BEGIN
  NEW(p);
  Rtl.Collect()
END NestedCollect;


BEGIN
  InitialiseMemoryRange;

  (*
  ShowOberonModules;
  GetModuleInfo;
  ShowPEtables;
  *)

  Rtl.InstallHeapTraceHandler(HeapTrace);
  wsl("Hello teapots.");
  ws("Address of i: "); wh(SYSTEM.ADR(i)); wl;
  ws("Address of j: "); wh(SYSTEM.ADR(j)); wl;
  ws("Address of s: "); wh(SYSTEM.ADR(s)); wl;
  ws("Address of c: "); wh(SYSTEM.ADR(c)); wl;

  s := 'Hello';
  i := 20;
  NEW(j);
  c := 'X';


  wsl("VAR dump:");
  dump(2, SYSTEM.ADR(c), 38);

  NestedCollect;
  Rtl.Halt(0);
  ASSERT(FALSE);

  (* c := s[20];*)
  c := s[i];

  i := 0;
  i := 5 DIV i;

  i := j.i;

END test.



10000000
   8fea0