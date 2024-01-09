MODULE WritePE;  (* Create exe from a list of compiled Oberon modules *)
IMPORT SYSTEM, K := Kernel, Files, B := Base, w := Writer;


CONST
  HeaderSize       =    400H;
  SectionAlignment =   1000H;  (* Sections are a multiple of this size in memory *)
  FileAlignment    =    200H;  (* Sections are a multiple of this size on file *)
  RvaImport        =   1000H;
  FadrImport       =    400H;
  RvaOberon        =   2000H;
  FadrOberon       =    600H;

TYPE
  ObjectFile = POINTER TO ObjectFileDesc;
  ObjectFileDesc = RECORD
    next: ObjectFile;
    name: ARRAY 1204 OF CHAR
  END;


VAR
  FileName:   ARRAY 512 OF CHAR;
  ExeFile:    Files.File;
  Exe:        Files.Rider;
  OberonSize: INTEGER;
  ImportSize: INTEGER;
  EntryPoint: INTEGER;
  Objects:    ObjectFile;
  LastObject: ObjectFile;

  (* Section layout - generates 2 sections:
     1. Imports section requesting standard system functions
     2. Oberon section containing concatenated modules in link sequence
  *)


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

PROCEDURE WriteImports;
VAR
  i, fpos: INTEGER;
BEGIN (* WriteImports *)
  Files.Set(Exe, ExeFile, FadrImport);

  (* Write import Directory table entry - Kernel32.dll *)
  Files.WriteCard32(Exe, RvaImport + 40);   (*  0: RVA of import lookup table      *)
  Files.WriteCard32(Exe, 0);                (*  4: Time/Date stamp (none)          *)
  Files.WriteCard32(Exe, 0);                (*  8: Forwarder chain (none)          *)
  Files.WriteCard32(Exe, RvaImport + 72);   (* 12: Name of DLL being imported from *)
  Files.WriteCard32(Exe, RvaOberon);        (* 16: RVA of imported addresses       *)
  FOR i := 1 TO 5 DO Files.WriteCard32(Exe,0) END; (* Empty directory table entry - end of direvtory table *)

  (* Write import lookup table for kernel32.dll (also written to start of data section) *)
  Files.WriteInt(Exe, RvaImport +  86);     (* 40: Address of 1st entry point hint *)
  Files.WriteInt(Exe, RvaImport + 104);     (* 48: Address of 2nd entry point hint *)
  Files.WriteInt(Exe, RvaImport + 118);     (* 56: Address of 3rd entry point hint *)
  Files.WriteInt(Exe, 0);                   (* 64: End of import lookup table      *)

  (* Write DLL name being imported from *)
  Files.WriteString(Exe, "KERNEL32.DLL");   (* 72: Name of library being imported from *)

  (* Write hint table of imported entry point names *)
  Files.Set(Exe, ExeFile, FadrImport + 86);
  Files.WriteCard16(Exe, 0);                (* 86: import number (not specified) *)
  Files.WriteString(Exe, "GetProcAddress"); (* 88: import name *)

  Files.Set(Exe, ExeFile, FadrImport + 104);
  Files.WriteCard16(Exe, 0);                (* 104: import number (not specified) *)
  Files.WriteString(Exe, "LoadLibraryA");   (* 106: import name *)

  Files.Set(Exe, ExeFile, FadrImport + 118);
  Files.WriteCard16(Exe, 0);                (* 118: import number (not specified) *)
  Files.WriteString(Exe, "ExitProcess");    (* 120: import name *)

  ImportSize := Align(Files.Pos(Exe), 16) - FadrImport;
END WriteImports;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE CopyFile(name: ARRAY OF CHAR);
VAR  f: Files.File;  r: Files.Rider;  buf: ARRAY 1000H OF BYTE;
BEGIN
  f := Files.Old(name);
  IF f = NIL THEN
    w.s("Couldn't copy '"); w.s(name); w.sl("'."); K.Halt(99)
  END;
  Files.Set(r, f, 0);
  WHILE ~r.eof DO
    Files.ReadBytes(r, buf, LEN(buf));
    Files.WriteBytes(Exe, buf, LEN(buf) - r.res);
  END
END CopyFile;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE WriteOberon;
VAR
   FirstModule: INTEGER;  object: ObjectFile;
BEGIN
  (* Oberon section starts with addresses of kernel functions GetProcAddress and LoadLibraryA *)
  Files.Set(Exe, ExeFile, FadrOberon);
  Files.WriteInt(Exe, RvaImport +  86);  (*  0: Address of GetProcAddress *)
  Files.WriteInt(Exe, RvaImport + 104);  (*  8: Address of LoadLibraryA   *)
  Files.WriteInt(Exe, RvaImport + 118);  (* 16: Address of ExitProcess    *)
  Files.WriteInt(Exe, 0);                (* 24: Zero terminator           *)

  FirstModule := Files.Pos(Exe);

  object := Objects;
  WHILE object # NIL DO CopyFile(object.name);  object := object.next END;

  Files.WriteInt(Exe, 0);  (* Mark end of modules - appears as header.length = 0 *)
  Files.WriteInt(Exe, 0);  (* Mark end of modules - appears as header.next = NIL *)

  (* Fill Oberon section to a whole multiple of section alignment *)
  IF Files.Pos(Exe) - FadrOberon MOD FileAlignment # 0 THEN
    Files.Set(Exe, ExeFile, Align(Files.Pos(Exe), FileAlignment)-1);
    Files.WriteByte(Exe, 0);
  END;

  OberonSize := Files.Pos(Exe) - FadrOberon;

  (* Extract and clear the first modules entry point and set it in the PE header. *)
  Files.Set(Exe, ExeFile, FirstModule + 64);
  Files.ReadInt(Exe, EntryPoint);  (* Entry point relative to module *)
  Files.Set(Exe, ExeFile, FirstModule + 64);
  Files.WriteInt(Exe, 0);

  INC(EntryPoint, RvaOberon + FirstModule - FadrOberon);
END WriteOberon;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* PE Header *)

PROCEDURE WriteSectionHeader(name: ARRAY OF CHAR;
                             vsize, fsize, rva, fadr: INTEGER;
                             flags: INTEGER);
VAR i, l: INTEGER;
BEGIN
  l := LEN(name);  i := 0;  IF l > 8 THEN l := 8 END;
  WHILE (i < l) & (name[i] # 0X) DO Files.WriteByte(Exe, ORD(name[i]));  INC(i) END;
  WHILE (i < 8) DO Files.WriteByte(Exe, 0); INC(i) END;
  Files.WriteCard32(Exe, vsize);  (* VirtualSize                     *)
  Files.WriteCard32(Exe, rva);    (* VirtualAddress                  *)
  Files.WriteCard32(Exe, fsize);  (* SizeOfRawData                   *)
  Files.WriteCard32(Exe, fadr);   (* PointerToRawData                *)
  Files.WriteCard32(Exe, 0);      (* PointerToRelocations            *)
  Files.WriteCard32(Exe, 0);      (* PointerToLinenumbers            *)
  Files.WriteCard32(Exe, 0);      (* NumberOfRelocations/linenumbers *)
  Files.WriteCard32(Exe, flags)   (* Characteristics                 *)
END WriteSectionHeader;

PROCEDURE WritePEHeader;
CONST
  (* Section flags *)
  SWriteable     = 80000000H;
  SReadable      = 40000000H;
  SExecutable    = 20000000H;
  SUninitialised =       80H;
  SInitialised   =       40H;
  SCode          =       20H;
TYPE
  U8  = BYTE;         U16 = SYSTEM.CARD16;  U32 = SYSTEM.CARD32;
  I8  = SYSTEM.INT8;  I16 = SYSTEM.INT16;   I32 = SYSTEM.INT32;   I64 = INTEGER;
  PEHDR = RECORD
    eMagic:     U16;  (* 5AD4 *)
    zeroes:     ARRAY 3AH OF BYTE;
    eLfanew:    U32;
    dosProgram: ARRAY 40H OF CHAR;
    signature:  U32;

    (* COFF file header*)
    machine:              U16;
    numberOfSections:     U16;
    timeDateStamp:        U32;
    pointerToSymbolTable: U32;
    numberOfSymbols:      U32;
    sizeOfOptionalHeader: U16;
    characteristics:      U16;

    (* PE32+ optional header *)
    pe32magic:               U16;
    majorLinkerVersion:      U8;  minorLinkerVersion:  U8;
    sizeOfCode:              U32;
    sizeOfInitializedData:   U32;
    sizeOfUninitializedData: U32;
    addressOfEntryPoint:     U32;
    baseOfCode:              U32;

    (* Windows specific PE32+ fields *)
    imageBase:             I64;
    sectionAlignment:      U32;  fileAlignment:         U32;
    majorOSVersion:        U16;  minorOSVersion:        U16;
    majorImageVersion:     U16;  minorImageVersion:     U16;
    majorSubsystemVersion: U16;  minorSubsystemVersion: U16;
    win32VersionValue:     U32;
    sizeOfImage:           U32;  sizeOfHeaders:         U32;
    checksum:              U32;
    subsystem:             U16;
    dllCharacteristics:    U16;
    sizeOfStackReserve:    I64;  sizeOfStackCommit:     I64;
    sizeOfHeapReserve:     I64;  sizeOfHeapCommit:      I64;
    loaderFlags:           U32;
    numberOfRvaAndSizes:   U32;

    (* Optional header data directories *)
    exportTableRVA:           U32;  exportTableSize:           U32;
    importTableRVA:           U32;  importTableSize:           U32;
    resourceTableRVA:         U32;  resourceTableSize:         U32;
    exceptionTableRVA:        U32;  exceptionTableSize:        U32;
    certificateTableRVA:      U32;  certificateTableSize:      U32;
    baseRelocationTableRVA:   U32;  baseRelocationTableSize:   U32;
    debugRVA:                 U32;  debugSize:                 U32;
    architectureRVA:          U32;  architectureSize:          U32;
    globalPtrRVA:             U32;  globalPtrSize:             U32;
    tlsTableRVA:              U32;  tlsTableSize:              U32;
    loadConfigTableRVA:       U32;  loadConfigTableSize:       U32;
    boundImportRVA:           U32;  boundImportSize:           U32;
    IATRVA:                   U32;  IATSize:                   U32;
    delayImportDescriptorRVA: U32;  delayImportDescriptorSize: U32;
    CLRRuntimeHeaderRVA:      U32;  CLRRuntimeHeaderSize:      U32;
    reservedZeroRVA:          U32;  reservedZeroSize:          U32
  END;
VAR
  hdr: PEHDR;

  PROCEDURE ZeroFill(VAR buf: ARRAY OF BYTE);  VAR i: INTEGER;
  BEGIN FOR i := 0 TO LEN(buf)-1 DO buf[i] := 0 END END ZeroFill;

BEGIN
  ZeroFill(hdr);

  (* MSDOS stub *)
  hdr.eMagic               := 5A4DH;
  hdr.eLfanew              := 128;
  hdr.dosProgram           := $ 0E 1F BA 0E 00 B4 09 CD  21 B8 01 4C CD 21 54 68
                                69 73 20 70 72 6F 67 72  61 6D 20 63 61 6E 6E 6F
                                74 20 20 72 75 6E 20 69  6E 20 44 4F 53 20 6D 6F
                                64 65 2E 0D 0A 24 $;
  hdr.signature            := 4550H;

  (* COFF file header*)
  hdr.machine              := 8664H;  (* AMD64/Intel 64 *)
  hdr.numberOfSections     := 2;
  hdr.sizeOfOptionalHeader := 240;
  hdr.characteristics      := 200H  (* Windows debug information stripped               *)
                            + 20H   (* Large address aware                              *)
                            + 8     (* Coff symbol tables removed (should really be 0?) *)
                            + 4     (* Coff linenumbers removed   (should really be 0?) *)
                            + 2     (* Executable image                                 *)
                            + 1;    (* Relocs stripped *)

  (* PE32+ optional header *)
  hdr.pe32magic               := 20BH;  (* PE32+ *)
  hdr.majorLinkerVersion      := 1;
  hdr.minorLinkerVersion      := 49H;
  hdr.sizeOfCode              := Align(OberonSize, FileAlignment);
  hdr.sizeOfInitializedData   := Align(ImportSize, SectionAlignment);
  hdr.sizeOfUninitializedData := 0;
  hdr.addressOfEntryPoint     := EntryPoint;
  hdr.baseOfCode              := RvaOberon;

  (* Windows specific PE32+ fields *)
  hdr.imageBase               := 400000H;
  hdr.sectionAlignment        := SectionAlignment;
  hdr.fileAlignment           := FileAlignment;
  hdr.majorOSVersion          := 1;
  hdr.majorSubsystemVersion   := 5;
  hdr.sizeOfImage             := Align(HeaderSize, SectionAlignment)
                               + Align(OberonSize, SectionAlignment)
                               + 4096;   (* import section *)
  hdr.sizeOfHeaders           := HeaderSize;
  IF B.Flag.console THEN
    hdr.subsystem             := 3
  ELSE
    hdr.subsystem             := 2
  END;
  hdr.dllCharacteristics      := 400H;   (* No SEH *)
  hdr.sizeOfStackReserve      := 1000H;
  hdr.sizeOfStackCommit       := 1000H;
  hdr.sizeOfHeapReserve       := 1000H;  (* Minimal heap - Windows may use it, we don't *)
  hdr.numberOfRvaAndSizes     := 16;

  (* Optional header data directories *)
  hdr.importTableRVA          := RvaImport;
  hdr.importTableSize         := ImportSize;

  Files.Set(Exe, ExeFile, 0);
  Files.WriteBytes(Exe, hdr, SYSTEM.SIZE(PEHDR));

  (* Write section headers *)
  WriteSectionHeader(".idata",
                     Align(ImportSize, SectionAlignment),  (* Size in memory *)
                     Align(ImportSize, FileAlignment),     (* Size on disk *)
                     RvaImport, FadrImport,
                     SReadable + SWriteable + SInitialised);
  WriteSectionHeader("Oberon",
                     Align(OberonSize, SectionAlignment),   (* Size in memory *)
                     Align(OberonSize, FileAlignment),      (* Size on disk *)
                     RvaOberon, FadrOberon,
                     SReadable + SWriteable + SExecutable + SCode);
END WritePEHeader;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE AddObject*(fn: ARRAY OF CHAR);
BEGIN
  IF LastObject = NIL THEN
    NEW(Objects);  Objects.name := fn;  LastObject := Objects
  ELSE
    NEW(LastObject.next);  LastObject := LastObject.next;
    LastObject.name := fn
  END
END AddObject;


PROCEDURE Generate*(filename: ARRAY OF CHAR);
VAR fpos: INTEGER;
BEGIN
  ExeFile := Files.New(filename);

  WriteImports;
  WriteOberon;
  WritePEHeader;

  Files.Register(ExeFile)
END Generate;

BEGIN (* w.sl("WritePe loaded.") *)
END WritePE.