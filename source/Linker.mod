MODULE Linker;
IMPORT SYSTEM, Files, B := Base;

CONST
  HeaderSize       =  400H;
  SectionAlignment = 1000H;  (* Sections are a multiple of this size in memory *)
  FileAlignment    =  200H;  (* Sections are a multiple of this size on file *)

  (* Sections generated *)
  SecUninitGlobals = 0;
  SecInitGlobals   = 1;
  SecCode          = 2;
  SecImports       = 3;
  SecTraps         = 4;
  SecExports       = 5;

  (* Import section - Kernel32 import table constants *)
  K32TableOffset = 40;  (* Relative to start of .idata (imports) section *)
  K32TableLen    = 6;   (* 5 imports and a zero entry *)
  K32TableSize   = K32TableLen * 8;
  K32NameOffset  = K32TableOffset + K32TableSize;
  K32HintOffset  = K32NameOffset + 16;

  (* Section flags *)
  SWriteable     = 80000000H;
  SReadable      = 40000000H;
  SExecutable    = 20000000H;
  SUninitialised =       80H;
  SInitialised   =       40H;
  SCode          =       20H;
TYPE
  SectionMetrics = RECORD
    fadr:  INTEGER;  (* Address in file                                      *)
    rva:   INTEGER;  (* Relative address in memory                           *)
    size:  INTEGER   (* Raw data size, not rounded up to file or memory size *)
  END;

VAR
  FileName:           ARRAY 512 OF CHAR;
  Out:                Files.File;
  Rider:              Files.Rider;
  ImageBase:          INTEGER;
  EntryPoint:         INTEGER;
  ModulePointerTable: INTEGER;  (* Where to store module pointers within initialised data *)

  Kernel32Table: ARRAY K32TableLen OF INTEGER; (* Fixed Kernel32 imports *)

  (* Section layout

     All sections are aligned on SectionAlignment (4096) size boundaries.
     The first three sections must be:

        Uninitialised globals (Module's VAR block)
        Initialised global (Literal strings and type descriptors)
        Code

     The position independent code relies on this ordering to generate the
     relative address of the base of the data section.

     The uninitialised and initialised global section lengths are passed to the
     Link function as parameters varSize and staticSize respectively (and are
     then rounded up to a whole multiple of the section size).

     The rva of the first section (uninitialised globals) is always 4096. (The
     first 4096 bytes contain the PE file headers.)
  *)

  Section: ARRAY 6 OF SectionMetrics;
  Rva: INTEGER;  (* current section rva, updated as each section is written. *)
  (* Predetermined rva's - calculated from Link parameters, and checked as   *)
  (* the sections are generated.                                             *)
  RvaInitGlobals, RvaCode, RvaImport: INTEGER;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Align(a: INTEGER;  align: INTEGER): INTEGER;
VAR result: INTEGER;
BEGIN
  IF    a > 0 THEN result := (a + align - 1) DIV align * align
  ELSIF a < 0 THEN result :=        a        DIV align * align
  END
RETURN result END Align;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE MakeUninitGlobals(VAR metrics: SectionMetrics; size: INTEGER);
BEGIN size := Align(size, SectionAlignment);
  metrics.size  := size;
  metrics.fadr  := 0;
  metrics.rva   := Rva;
  INC(Rva, size)
END MakeUninitGlobals;


PROCEDURE Write_pointer_offset(offset: INTEGER;  type: B.Type);
VAR ident: B.Ident;       field: B.Field;
    size, k, n: INTEGER;  base:  B.Type;
BEGIN
  IF type.form = B.tRec THEN ident := type.fields;
    IF (type.base # NIL) & (type.base.nTraced > 0) THEN
      Write_pointer_offset(offset, type.base)
    END;
    WHILE ident # NIL DO field := ident.obj(B.Field);
      IF field.type.nTraced > 0 THEN n := offset + field.off;
        IF field.type.form = B.tPtr THEN Files.WriteInt(Rider, n)
        ELSE Write_pointer_offset(n, field.type)
        END
      END;
      ident := ident.next
    END
  ELSIF type.form = B.tArray THEN
    base := type.base;  k := 0;
    IF base.form = B.tPtr THEN n := offset;
      WHILE k < type.len DO
        Files.WriteInt(Rider, n);  INC(k);  INC(n, 8)
      END
    ELSE size := base.size;
      WHILE k < type.len DO
        Write_pointer_offset(offset, base);
        INC(k);  INC(offset, size)
      END
    END
  ELSE ASSERT(FALSE)
  END
END Write_pointer_offset;


PROCEDURE Write_proc_pointer_offset(ident: B.Ident);
VAR x: B.Proc;

  PROCEDURE Write(x: B.Proc);
  VAR ident: B.Ident;  adr: INTEGER;  y: B.Object;
  BEGIN
    Files.Set(Rider, Out, Section[SecInitGlobals].fadr + x.descAdr);
    ident := x.decl;
    WHILE ident # NIL DO y := ident.obj;
      IF (y IS B.Var) & ~(y IS B.Str) & ~(y IS B.Par)
      OR (y IS B.Par) & ~y(B.Par).varpar & (y.type.size = 8) THEN
        IF y.type.nTraced > 0 THEN adr := y(B.Var).adr;
          IF y.type.form = B.tPtr THEN Files.WriteInt(Rider, adr)
          ELSE Write_pointer_offset(adr, y.type)
          END
        END
      END;
      ident := ident.next
    END;
    Files.WriteInt(Rider, -1)
  END Write;

BEGIN (* Write_proc_pointer_offset *)
  WHILE ident # NIL DO
    IF ident.obj IS B.Proc THEN
      x := ident.obj(B.Proc);  Write_proc_pointer_offset(x.decl);
      IF x.nTraced > 0 THEN Write(x) END
    END;
    ident := ident.next
  END
END Write_proc_pointer_offset;


PROCEDURE WriteInitGlobals(VAR metrics: SectionMetrics; size: INTEGER);
VAR
  i, adr: INTEGER;  b: BYTE;
  imod: B.Module;  slist: B.StrList;  ident: B.Ident;
  x: B.Str;  t: B.TypeList;  obj: B.Object;
  str: ARRAY 512 OF CHAR;
BEGIN
  ASSERT(Rva MOD SectionAlignment = 0);
  ASSERT(Rva = RvaInitGlobals);

  metrics.fadr  := Align(Files.Pos(Rider), FileAlignment);
  metrics.rva   := Rva;
  metrics.size  := size;

  (* Write initialised data *)

  (* Start with addresses of 5 standard kernel32 imports *)
  Files.Set(Rider, Out, metrics.fadr);  i := 0;
  WHILE i < LEN(Kernel32Table) DO
    Files.WriteInt(Rider, Kernel32Table[i]);  INC(i)
  END;

  (* For debugging support write the RVA of initialised data allowing debug   *)
  (* code to find the Windows load address of the exe or dll.                 *)
  Files.Set(Rider, Out, metrics.fadr + 80);
  Files.WriteInt(Rider, RvaInitGlobals);

  (* Write names of imported modules at offsets specified by the .adr field *)
  (* of each module object.                                                 *)
  imod := B.modList;
  WHILE imod # NIL DO
    IF imod.import OR (imod.impList # NIL) THEN
      Files.Set(Rider, Out, metrics.fadr + imod.adr);
      i := 0;  B.Insert(imod.id, str, i);
      B.Insert('.dll', str, i);  Files.WriteString(Rider, str)
    END;
    imod := imod.next
  END;

  slist := B.strList;
  WHILE slist # NIL DO x := slist.obj;
    Files.Set(Rider, Out, metrics.fadr + x.adr);  i := 0;
    WHILE i < x.len DO
      Files.WriteChar(Rider, B.strbuf[x.bufpos+i]);  INC(i)
    END;
    slist := slist.next
  END;
  t := B.recList;
  WHILE t # NIL DO
    Files.Set(Rider, Out, metrics.fadr + t.type.adr);
    Files.WriteInt(Rider, t.type.size);
    Files.Set(Rider, Out, metrics.fadr + t.type.adr + 8 + B.MaxExt*8);
    IF t.type.nTraced > 0 THEN Write_pointer_offset(0, t.type) END;
    Files.WriteInt(Rider, -1);  t := t.next
  END;

  (* Write module pointers into initialised data *)
  Files.Set(Rider, Out, metrics.fadr + ModulePointerTable);
  ident := B.universe.first;
  WHILE ident # NIL DO obj := ident.obj;
    IF (obj IS B.Var) & ~(obj IS B.Str) THEN
      IF obj.type.nTraced > 0 THEN adr := obj(B.Var).adr;
        IF obj.type.form = B.tPtr THEN Files.WriteInt(Rider, adr)
        ELSE Write_pointer_offset(adr, obj.type)
        END
      END
    END;
    ident := ident.next
  END;
  Files.WriteInt(Rider, -1);

  Write_proc_pointer_offset(B.universe.first);

  Files.Set(Rider, Out, metrics.fadr + size);
  Rva := Align(Rva + size, SectionAlignment);
END WriteInitGlobals;


PROCEDURE WriteCode(VAR metrics: SectionMetrics; code: ARRAY OF BYTE; size: INTEGER);
BEGIN
  ASSERT(Rva MOD SectionAlignment = 0);
  ASSERT(Rva = RvaCode);

  metrics.fadr  := Align(Files.Pos(Rider), FileAlignment);
  metrics.rva   := Rva;
  metrics.size  := size;

  Files.Set(Rider, Out, metrics.fadr);
  Files.WriteBytes(Rider, code, size);

  Rva := Align(Rva + size, SectionAlignment);
END WriteCode;


PROCEDURE MakeKernel32ImportTable;
VAR
  i: INTEGER;
BEGIN i := 0;
  WHILE i < K32TableLen - 1 DO
    Kernel32Table[i] := RvaImport + K32HintOffset + 32 * i;  (* Lookup by name given in hint table *)
    INC(i)
  END;
  Kernel32Table[i] := 0
END MakeKernel32ImportTable;


PROCEDURE WriteImports(VAR metrics: SectionMetrics);
VAR
  i, fpos: INTEGER;

BEGIN (* WriteImports *)
  ASSERT(Rva MOD SectionAlignment = 0);
  ASSERT(Rva = RvaImport);

  metrics.fadr  := Align(Files.Pos(Rider), FileAlignment);
  metrics.rva   := Rva;

  Files.Set(Rider, Out, metrics.fadr);

  (* Write import Directory Entry - Kernel32.dll *)
  Files.WriteCard32(Rider, metrics.rva + K32TableOffset); (* RVA of import lookup table      *)
  Files.WriteCard32(Rider, 0);                            (* Time/Date stamp (none)          *)
  Files.WriteCard32(Rider, 0);                            (* Forwarder chain (none)          *)
  Files.WriteCard32(Rider, metrics.rva + K32NameOffset);  (* Name of DLL being imported from *)
  Files.WriteCard32(Rider, RvaInitGlobals);               (* RVA of imported addresses       *)

  (* Write import lookup table (also written to start of data section) *)
  Files.Set(Rider, Out, metrics.fadr + K32TableOffset);   (* Leaves 20 zero bytes as end of import directory *)
  FOR i := 0 TO K32TableLen - 1 DO Files.WriteInt(Rider, Kernel32Table[i]) END;

  (* Write DLL name being imported from *)
  Files.Set(Rider, Out, metrics.fadr + K32NameOffset);  Files.WriteByteStr(Rider, 'KERNEL32.DLL');

  (* Write hint table of imported entry point names *)
  Files.Set(Rider, Out, metrics.fadr + K32HintOffset + 2);   Files.WriteByteStr(Rider, 'GetProcAddress');
  Files.Set(Rider, Out, metrics.fadr + K32HintOffset + 34);  Files.WriteByteStr(Rider, 'LoadLibraryW');
  Files.Set(Rider, Out, metrics.fadr + K32HintOffset + 66);  Files.WriteByteStr(Rider, 'ExitProcess');
  Files.Set(Rider, Out, metrics.fadr + K32HintOffset + 98);  Files.WriteByteStr(Rider, 'GetModuleHandleExW');
  Files.Set(Rider, Out, metrics.fadr + K32HintOffset + 130); Files.WriteByteStr(Rider, 'AddVectoredExceptionHandler');

  metrics.size := K32HintOffset + 160;

  Rva := Align(Rva + metrics.size, SectionAlignment);

END WriteImports;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Trap lookup debug info *)

PROCEDURE WriteTraps(VAR metrics: SectionMetrics; debug: Files.File);
VAR
  data: ARRAY 200H OF BYTE;
  len, cnt: INTEGER;  r: Files.Rider;
  fpos: INTEGER;
BEGIN
  ASSERT(Rva MOD SectionAlignment = 0);

  metrics.fadr  := Align(Files.Pos(Rider), FileAlignment);
  metrics.rva   := Rva;

  Files.Set(Rider, Out, metrics.fadr + 32);
  Files.Set(r, debug, 0);  len := Files.Length(debug);
  Files.ReadBytes(r, data, LEN(data));  cnt := LEN(data) - r.res;
  WHILE len > 0 DO
    IF cnt > len THEN cnt := len END;  DEC(len, cnt);
    Files.WriteBytes(Rider, data, cnt);
    Files.ReadBytes(r, data, LEN(data));  cnt := LEN(data) - r.res
  END;
  Files.WriteInt(Rider, -1);

  fpos := Files.Pos(Rider);

  metrics.size := fpos - metrics.fadr;

  Rva := Align(Rva + metrics.size, SectionAlignment);

END WriteTraps;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* .edata *)

PROCEDURE WriteExports(VAR metrics: SectionMetrics);
CONST
  exportDirectorySize = 40;  (* Fixed length export section header *)
TYPE
  namedExport = POINTER TO RECORD
    ident:  B.Ident;
    number: INTEGER;
    next:   namedExport
  END;
VAR
  moduleNameSize:  INTEGER;
  exportCount:     INTEGER;
  export:          B.ObjList;
  nameCount:       INTEGER;  (* Names are provided only for procedures *)
  names, name:     namedExport;
  ordinal:         INTEGER;
  addressTableRva: INTEGER;
  namePointersRva: INTEGER;
  ordinalsRva:     INTEGER;
  namesRva:        INTEGER;
  exportAdr:       INTEGER;
  obj:             B.Object;
  nameRva:         INTEGER;
  fpos:            INTEGER;

  PROCEDURE addname(ident: B.Ident; number: INTEGER; VAR names: namedExport);
  VAR name, p, q: namedExport;
  BEGIN
    p := NIL; q := names;
    WHILE (q # NIL) & (q.ident.name < ident.name) DO p := q;  q := q.next END;
    NEW(name);  name.next := q;  name.ident := ident;  name.number := number;
    IF p # NIL THEN p.next := name ELSE names := name END
  END addname;

BEGIN (* WriteExports *)
  ASSERT(Rva MOD SectionAlignment = 0);
  metrics.fadr := Align(Files.Pos(Rider), FileAlignment);
  metrics.rva  := Rva;

  moduleNameSize := B.StrLen(FileName)+1;
  exportCount    := B.expno;

  export    := B.expList;
  nameCount := 0;
  ordinal   := 1;  (* export index (1 based) *)
  WHILE export # NIL DO
    IF export.obj IS B.Proc THEN
      INC(nameCount);  addname(export.obj.ident, ordinal, names);
    END;
    INC(ordinal);
    export := export.next
  END;

  addressTableRva := metrics.rva + exportDirectorySize;
  namePointersRva := addressTableRva + exportCount * 4; (* 32 bits per export *)
  ordinalsRva     := namePointersRva + nameCount * 4;   (* One 32 bit rva per name *)
  namesRva        := ordinalsRva + nameCount * 2;       (* 16 bits per ordinal *)

  (* Export directory *)
  Files.Set(Rider, Out, metrics.fadr + 12);  (* Skip datestamp and version fields *)
  Files.WriteCard32(Rider, namesRva);        (* Name RVA                          *)
  Files.WriteCard32(Rider, 1);               (* Ordinal base                      *)
  Files.WriteCard32(Rider, exportCount);     (* Address table entries             *)
  Files.WriteCard32(Rider, nameCount);       (* Number of name pointers           *)
  Files.WriteCard32(Rider, addressTableRva);
  Files.WriteCard32(Rider, namePointersRva);
  Files.WriteCard32(Rider, ordinalsRva);

  (* Export address table *)
  Files.Set(Rider, Out, metrics.fadr + exportDirectorySize);
  export := B.expList;
  WHILE export # NIL DO obj := export.obj;
    IF obj.class = B.cType THEN exportAdr := Section[SecInitGlobals].rva + obj.type.adr
    ELSIF obj IS B.Var     THEN exportAdr := Section[SecInitGlobals].rva + obj(B.Var).adr
    ELSIF obj IS B.Proc    THEN exportAdr := Section[SecCode].rva        + obj(B.Proc).adr
    END;
    Files.WriteCard32(Rider, exportAdr);
    export := export.next
  END;

  (* Export Name Pointer Table *)
  name := names;  nameRva := namesRva + moduleNameSize;
  WHILE name # NIL DO
    Files.WriteCard32(Rider, nameRva);
    INC(nameRva, B.StrLen(name.ident.name)+1);
    name := name.next
  END;

  (* Export Ordinal Table *)
  name := names;
  WHILE name # NIL DO
    Files.WriteCard16(Rider, name.number-1);
    name := name.next
  END;

  (* Name strings *)
  Files.WriteByteStr(Rider, FileName);
  name := names;
  WHILE name # NIL DO
    Files.WriteByteStr(Rider, name.ident.name);
    name := name.next
  END;

  (* metrics.size := Files.Pos(Rider) - metrics.fadr; *)
  fpos := Files.Pos(Rider) - metrics.fadr;
  metrics.size := fpos;

  Rva := Align(Rva + metrics.size, SectionAlignment);
END WriteExports;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* PE Header *)

PROCEDURE WriteSectionHeader(sec: INTEGER; name: ARRAY OF CHAR; flags: INTEGER);
VAR i, l, filesize, virtualsize: INTEGER;
BEGIN
  l := LEN(name);  i := 0;  IF l > 8 THEN l := 8 END;
  WHILE (i < l) & (name[i] # 0X) DO Files.Write(Rider, ORD(name[i]));  INC(i) END;
  WHILE (i < 8) DO Files.Write(Rider, 0); INC(i) END;

  virtualsize := Align(Section[sec].size, SectionAlignment);
  filesize    := Align(Section[sec].size, FileAlignment);
  IF sec = SecUninitGlobals THEN filesize := 0 END;

  Files.WriteCard32(Rider, virtualsize);       (* VirtualSize                     *)
  Files.WriteCard32(Rider, Section[sec].rva);  (* VirtualAddress                  *)
  Files.WriteCard32(Rider, filesize);          (* SizeOfRawData                   *)
  Files.WriteCard32(Rider, Section[sec].fadr); (* PointerToRawData                *)
  Files.WriteCard32(Rider, 0);                 (* PointerToRelocations            *)
  Files.WriteCard32(Rider, 0);                 (* PointerToLinenumbers            *)
  Files.WriteCard32(Rider, 0);                 (* NumberOfRelocations/linenumbers *)
  Files.WriteCard32(Rider, flags)              (* Characteristics                 *)
END WriteSectionHeader;

PROCEDURE WritePEHeader;
CONST
  dosProgram = $0E1FBA0E00B409CD21B8014CCD21546869732070726F6772616D2063616E6E6F74202072756E20696E20444F53206D6F64652E0D0A24$;
VAR
  i, imageSize, uninitialisedSize, initialisedSize: INTEGER;
BEGIN
  Files.Set(Rider, Out, 0);            (* e_magic   *)
  Files.WriteCard16(Rider, 5A4DH);
  Files.Set(Rider, Out, 3CH);          (* e_lfanew  *)
  Files.WriteCard32(Rider, 128);
  Files.WriteBytes(Rider, dosProgram, 40H);
  Files.Set(Rider, Out, 80H);
  Files.WriteCard32(Rider, 4550H);     (* signature *)

  (* Windows COFF header *)
  Files.WriteCard16(Rider, 8664H);        (* Machine = AMD64/Intel 64 *)
  Files.WriteCard16(Rider, LEN(Section)); (* NumberOfSections         *)
  Files.WriteCard32(Rider, 0);            (* TimeDateStamp            *)
  Files.WriteCard32(Rider, 0);            (* PointerToSymbolTable     *)
  Files.WriteCard32(Rider, 0);            (* NumberOfSymbols          *)
  Files.WriteCard16(Rider, 240);          (* Size of optional header  *)

  (* Characteristics *)
  (* 2000H - This is adynamic link library                   *)
  (*  200H - Windows debug information stripped              *)
  (*   20H - Large address aware                             *)
  (*    8  - Coff symbol tables removed (should really be 0) *)
  (*    4  - Coff linenumbers removed   (should really be 0) *)
  (*    2  - Executable image                                *)
  (*    1  - Relocs stripped                                 *)
  IF B.Flag.main THEN
    Files.WriteCard16(Rider,         200H + 20H + 8 + 4 + 2 + 1)
  ELSE
    Files.WriteCard16(Rider, 2000H + 200H + 20H + 8 + 4 + 2)
  END;

  (* PE32+ optional header *)
  Files.WriteCard16(Rider, 20BH);       (* Magic number for PE32+           *)
  Files.Write(Rider, 1);                (* Linker major version             *)
  Files.Write(Rider, 49H);              (* Linker minor version             *)
  Files.WriteCard32(Rider, Align(Section[SecCode].size, FileAlignment));  (* SizeOfCode *)

  (* Optional header section sizes *)
  uninitialisedSize := Align(Section[SecUninitGlobals].size, SectionAlignment);
  initialisedSize   := Align(Section[SecInitGlobals].size,   SectionAlignment)
                     + Align(Section[SecImports].size,       SectionAlignment)
                     + Align(Section[SecTraps].size,         SectionAlignment)
                     + Align(Section[SecExports].size,       SectionAlignment);

  Files.WriteCard32(Rider, initialisedSize);                   (* SizeOfInitializedData   *)
  Files.WriteCard32(Rider, uninitialisedSize);                 (* SizeOfUninitializedData *)
  Files.WriteCard32(Rider, Section[SecCode].rva + EntryPoint); (* Address of entry point  *)
  Files.WriteCard32(Rider, Section[SecCode].rva);              (* Base of code            *)

  (* Windows specific PE32+ fields *)

  Files.WriteInt(Rider, ImageBase);           (* Preferred addres of first byte of image *)
  Files.WriteCard32(Rider, SectionAlignment); (* Section alignment when loaded           *)
  Files.WriteCard32(Rider, FileAlignment);    (* File alignment                          *)
  Files.WriteCard16(Rider, 1);                (* Major OS version                        *)
  Files.WriteCard16(Rider, 0);                (* Minor OS version                        *)
  Files.WriteCard16(Rider, 0);                (* Major image version                     *)
  Files.WriteCard16(Rider, 0);                (* Minor image version                     *)
  Files.WriteCard16(Rider, 5);                (* Major subsytem version                  *)
  Files.WriteCard16(Rider, 0);                (* Minor subsytem version                  *)
  Files.WriteCard32(Rider, 0);                (* Win32VerionValue (must be 0)            *)

  (* Image size including headers *)
  imageSize := Align(HeaderSize, SectionAlignment)
             + Align(Section[SecCode].size, SectionAlignment)
             + uninitialisedSize + initialisedSize;
  Files.WriteCard32(Rider, imageSize);  (* SizeOfImage                             *)
  Files.WriteCard32(Rider, HeaderSize); (* SizeOfHeaders                           *)
  Files.WriteCard32(Rider, 0);          (* Checksum                                *)
  IF B.Flag.console THEN
    Files.WriteCard16(Rider, 3)         (* Subsystem = Console                     *)
  ELSE
    Files.WriteCard16(Rider, 2)         (* Subsystem = GUI                         *)
  END;
                                        (* DLL Characteristics                     *)
  IF B.Flag.main THEN Files.WriteCard16(Rider, 400H) (* No SEH *)
  ELSE Files.WriteCard16(Rider, 400H + 100H + 40H)   (* No SEH, NX compat, relocatable *)
  END;

  Files.WriteInt(Rider, 1000H);         (* Size of stack reserve    *)
  Files.WriteInt(Rider, 1000H);         (* Size of stack commit     *)
  Files.WriteInt(Rider, 10000H);        (* Size of heap reserve     *)
  Files.WriteInt(Rider, 0);             (* Size of heap commit      *)
  Files.WriteCard32(Rider, 0);          (* Loader flags (must be 0) *)
  Files.WriteCard32(Rider, 16);         (* Number of RVAs/Sizes in data directories *)

  (* Optional header data directories *)
  Files.WriteCard32(Rider, Section[SecExports].rva); (* Export table *)
  Files.WriteCard32(Rider, Section[SecExports].size);
  Files.WriteCard32(Rider, Section[SecImports].rva); (* Import table *)
  Files.WriteCard32(Rider, Section[SecImports].size);
  Files.WriteInt(Rider, 0);             (* Resource table           *)
  Files.WriteInt(Rider, 0);             (* Exception table          *)
  Files.WriteInt(Rider, 0);             (* Certificate table        *)
  Files.WriteInt(Rider, 0);             (* Relocations              *)
  Files.WriteInt(Rider, 0);             (* Debug data               *)
  Files.WriteInt(Rider, 0);             (* Architecture (must be 0) *)
  Files.WriteInt(Rider, 0);             (* Global ptr               *)
  Files.WriteInt(Rider, 0);             (* TLS table                *)
  Files.WriteInt(Rider, 0);             (* Load config table        *)
  Files.WriteInt(Rider, 0);             (* Bound import             *)
  Files.WriteInt(Rider, 0);             (* Import address table ?   *)
  Files.WriteInt(Rider, 0);             (* Delay import descriptor  *)
  Files.WriteInt(Rider, 0);             (* CLR runtime header       *)
  Files.WriteInt(Rider, 0);             (* Reserved (must be 0)     *)

  (* Write section headers *)
  WriteSectionHeader(SecUninitGlobals, ".bss",   SReadable + SWriteable  + SUninitialised);
  WriteSectionHeader(SecInitGlobals,   ".data",  SReadable + SWriteable  + SInitialised);
  WriteSectionHeader(SecCode,          ".text",  SReadable + SExecutable + SCode);
  WriteSectionHeader(SecImports,       ".idata", SReadable + SWriteable  + SInitialised);
  WriteSectionHeader(SecTraps,         ".traps", SReadable +               SInitialised);
  WriteSectionHeader(SecExports,       ".edata", SReadable +               SInitialised);

  (* Oberon compiler specific data *)
  Files.Set(Rider, Out, 400H - 40);  (* Insert before end of header *)
  Files.WriteInt(Rider, Section[SecExports].rva);
  Files.WriteInt(Rider, Section[SecCode].rva);
  Files.WriteInt(Rider, Section[SecTraps].rva+32);
  Files.WriteInt(Rider, B.modkey[0]);
  Files.WriteInt(Rider, B.modkey[1])
END WritePEHeader;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)


PROCEDURE Link*(debug: Files.File;
                code: ARRAY OF BYTE;
                codesize, entry, staticSize, varSize, modPtrTable: INTEGER);
VAR fpos: INTEGER;
BEGIN
  FileName           := B.modid;
  EntryPoint         := entry;
  ModulePointerTable := modPtrTable;

  IF B.Flag.main THEN ImageBase := 400000H;    B.Append('.exe', FileName)
  ELSE                ImageBase := 10000000H;  B.Append('.dll', FileName)
  END;

  Out := Files.New(FileName);

  (* First section starts after headers *)
  Rva            := Align(HeaderSize,                  SectionAlignment);
  RvaInitGlobals := Align(Rva + varSize,               SectionAlignment);
  RvaCode        := Align(RvaInitGlobals + staticSize, SectionAlignment);
  RvaImport      := Align(RvaCode + codesize,          SectionAlignment);

  (* The kernel32 import table is used in both the data and import sections *)
  MakeKernel32ImportTable;  (* so build it first here *)

  Files.Set(Rider, Out, HeaderSize);

  (* Note: The uninitialised data, initialialised data and code sections must
           follow in sequence as the position independent code is generated to
           use this sequence to locate the uninitialised and initialised
           data sections at run time.                                         *)

  MakeUninitGlobals(Section[SecUninitGlobals], varSize);
  WriteInitGlobals (Section[SecInitGlobals],   staticSize);
  WriteCode        (Section[SecCode],          code, codesize);
  WriteImports     (Section[SecImports]);
  WriteTraps       (Section[SecTraps],         debug);
  WriteExports     (Section[SecExports]);

  (* Make sure file size is a whole multiple of FileAlignment *)
  fpos := Files.Pos(Rider);
  IF fpos MOD FileAlignment # 0 THEN
    Files.Set(Rider, Out, Align(fpos, FileAlignment) - 1);
    Files.Write(Rider, 1)
  END;

  WritePEHeader;

  Files.Register(Out)
END Link;

END Linker.