MODULE Loader;  (* Test code for loading OC2 code *)  (*$CONSOLE*)

IMPORT SYSTEM, Boot, Heap, X64, K := Kernel, w := Writer, l := Listing;

CONST
  MEMRESERVE           = 2000H;
  MEMCOMMIT            = 1000H;
  PAGEEXECUTEREADWRITE = 40H;

VAR
  VirtualAlloc:       PROCEDURE(lpAddress, dwSize, flAllocationType, flProtect: INTEGER): INTEGER;
  CreateFileA:        PROCEDURE(lpFileName, dwDesiredAccess, dwShareMode,
                                lpSecurityAttributes, dwCreationDisposition,
                                dwFlagsAndAttributes, hTemplateFile: INTEGER): INTEGER;
  CloseHandle:        PROCEDURE(hObject: INTEGER): INTEGER;
  GetFileSizeEx:      PROCEDURE(hFile, lpFileSize: INTEGER): INTEGER;
  GetLastError:       PROCEDURE(): INTEGER;
  CreateFileMappingA: PROCEDURE(hfile, attributes, protection,
                                maxsizehigh, maxsizelow, name: INTEGER): INTEGER;
  MapViewOfFile:      PROCEDURE(hmapping, access, offsethigh, offsetlow, length: INTEGER): INTEGER;
  UnmapViewOfFile:    PROCEDURE(baseaddress: INTEGER): INTEGER;

  (* Windows APIs passed directly to loaded module *)
  LoadLibraryA:       INTEGER;
  GetProcAddress:     INTEGER;
  MessageBoxA:        INTEGER;

  Base:       INTEGER;   (* code/VAR memory base *)
  Procedures: ARRAY 1024 OF INTEGER;  (* Adresses of procedures to disassemble *)
  Nprocs:     INTEGER;


PROCEDURE OpenFileMap(fn: ARRAY OF CHAR): INTEGER;
CONST
  GENERICREAD     = 80000000H;
  SHAREREADWRITE  = 3;
  OPENEXISTING    = 3;
  ATTRIBUTENORMAL = 80H;
  PAGEREAD        = 2H;
  MAPREAD         = 4H;
VAR
  hfile:    INTEGER;
  hmapping: INTEGER;
  address:  INTEGER;
BEGIN
  hfile := CreateFileA(SYSTEM.ADR(fn), GENERICREAD, SHAREREADWRITE, 0, OPENEXISTING, ATTRIBUTENORMAL, 0);
  IF hfile < 0 THEN
    w.s("Couldn't open '"); w.s(fn); w.s("', hfile "); w.i(hfile);
    w.s(", last error "); w.h(GetLastError()); w.sl("H.");
  END;
  ASSERT(hfile >= 0);
  hmapping := CreateFileMappingA(hfile, 0, PAGEREAD, 0, 0, 0);
  ASSERT(hmapping # 0);
  address := MapViewOfFile(hmapping, MAPREAD, 0, 0, 0);  (* Map whole file for read *)
  ASSERT(address # 0);
  ASSERT(CloseHandle(hmapping) # 0);
  ASSERT(CloseHandle(hfile) # 0);
RETURN address END OpenFileMap;


PROCEDURE Peek*(adr, size: INTEGER; signed: BOOLEAN): INTEGER;
VAR result: INTEGER; firstbyte: BYTE;
BEGIN
  SYSTEM.GET(adr, firstbyte);
  IF signed & (firstbyte >= 128) THEN result := -1 ELSE result := 0 END;
  SYSTEM.COPY(adr, SYSTEM.ADR(result), size)
RETURN result END Peek;

PROCEDURE InitDisassembly(addr, length: INTEGER);
BEGIN
  ASSERT(length <= LEN(X64.Text));
  SYSTEM.COPY(addr, SYSTEM.ADR(X64.Text), length);
END InitDisassembly;

PROCEDURE AddProc(entry: INTEGER);
VAR i: INTEGER;
BEGIN
  i := 0;
  WHILE (i < Nprocs) & (entry # 0) DO
    IF Procedures[i] = entry THEN entry := 0 ELSE INC(i) END
  END;
  IF entry # 0 THEN
    Procedures[Nprocs] := entry;
    INC(Nprocs)
  END
END AddProc;

PROCEDURE DisassembleSubroutine(start, limit: INTEGER);
VAR pc: INTEGER;
BEGIN
  (* Patch loaded code into X64 text array *)
  pc := start;
  WHILE (pc < limit) & (X64.Text[pc] # 2EH) DO
    IF X64.Text[pc] = 0E8H THEN  (* Add procedure to list needing disassembly *)
      AddProc(pc + 5 + X64.Peek(pc + 1, 4, TRUE))
    END;
    l.DisassembleInstruction(pc, 0, "");
  END;
END DisassembleSubroutine;

PROCEDURE LoadModule(addr: INTEGER);
VAR
  header:       X64.CodeHeader;
  c:            CHAR;
  base, commit: INTEGER;
  pc, i, adr:   INTEGER;
  init:         PROCEDURE;
  i32:          SYSTEM.INT32;
BEGIN
  header := SYSTEM.VAL(X64.CodeHeader, addr);
  w.sl("Module: ");

  w.s("  Length:        ");  w.h(header.length);    w.sl("H.");
  w.s("  Init code:     ");  w.h(header.initcode);  w.sl("H.");
  w.s("  Pointers:      ");  w.h(header.pointers);  w.sl("H.");
  w.s("  Commands:      ");  w.h(header.commands);  w.sl("H.");
  w.s("  Exports:       ");  w.h(header.exports);   w.sl("H.");
  w.s("  Imports:       ");  w.h(header.imports);   w.sl("H.");
  w.s("  VAR size:      ");  w.h(header.varsize);   w.sl("H.");
  w.s("  Module name:   ");

  pc := SYSTEM.SIZE(X64.CodeHeaderItem);  (* Module name follows header *)
  REPEAT SYSTEM.GET(addr + pc, c);  INC(pc);
    IF c # 0X THEN w.c(c) END
  UNTIL c = 0X;
  pc := (pc + 15) DIV 16 * 16;
  w.sl(".");

  Nprocs := 0;
  InitDisassembly(addr, header.pointers);  (* header.pointers is first item after code *)

  w.sl("Init code:");
  DisassembleSubroutine(header.initcode, header.pointers);

  (* Disassemble statically called procedures *)
  i := 0;
  WHILE i < Nprocs DO
    w.l;
    DisassembleSubroutine(Procedures[i], header.pointers);
    INC(i);
  END;

  (* Commit sufficent memory *)
  commit := (header.imports + header.varsize + 4095) DIV 4096 * 4096;
  w.s("Committing "); w.h(commit); w.sl("H bytes.");
  base := VirtualAlloc(Base, commit, MEMCOMMIT, PAGEEXECUTEREADWRITE);
  ASSERT(base = Base);

  (* Copy code into executable memory *)
  SYSTEM.COPY(addr, base, header.residentsize);

  (* Hack: preset the first 3 INTEGERs in the module VARs to Windows APIs *)
  SYSTEM.PUT(base + header.residentsize,    LoadLibraryA);
  SYSTEM.PUT(base + header.residentsize+8,  GetProcAddress);
  SYSTEM.PUT(base + header.residentsize+16, MessageBoxA);


  SYSTEM.PUT(SYSTEM.ADR(init), base + header.initcode);

  (* Dump command table *)
  adr := base + header.commands;
  SYSTEM.GET(adr, c);
  IF c # 0X THEN  (* Imported module name table *)
    w.sl("Commands:");
    REPEAT
      w.s("  "); REPEAT w.c(c); INC(adr);  SYSTEM.GET(adr, c)
      UNTIL c = 0X;
      INC(adr); SYSTEM.GET(adr, i32);  w.s(": ");  w.h(i32);  w.sl("H");
      INC(adr, 8); SYSTEM.GET(adr, c)
    UNTIL c = 0X;
  END;

  (* Dump global variable pointer list *)
  adr := base + header.pointers;
  SYSTEM.GET(adr, i);
  IF i >= 0 THEN
    w.sl("Pointers in module VARs:");
    REPEAT
      w.s("  ");  w.h(i);  w.sl("H");
      INC(adr, 8); SYSTEM.GET(adr, i);
    UNTIL i < 0;
  END;

  (* Dump export table *)
  adr := base + header.exports;  i := 0;
  SYSTEM.GET(adr, i32);
  IF i32 >= 0 THEN
    w.sl("Exports: ");
    REPEAT
      w.s("  ");  w.i(i);  w.s(": ");  w.h(i32);  w.sl("H");
      INC(i);  INC(adr, 4); SYSTEM.GET(adr, i32);
    UNTIL i32 < 0
  END;

  (* List imported module names *)
  DEC(adr, base);  adr := (adr + 15) DIV 16 * 16;  INC(adr, addr);
  SYSTEM.GET(adr, c);
  IF c # 0X THEN  (* Imported module name table *)
    w.sl("Imported modules:");
  END;


  w.sl("Calling init.");
  (*K.MessageBox("Loader", "Calling init.");*)
  init;
  (*K.MessageBox("Loader", "init returned.");*)
  w.sl("Init complete.");

END LoadModule;

PROCEDURE Load*(fn: ARRAY OF CHAR);
VAR mapaddress, commit: INTEGER;
BEGIN
  w.s("Loading "); w.sl(fn);

  (* Reserve 2Gb address space for Oberon code and global VARs *)
  Base := VirtualAlloc(0, 80000000H, MEMRESERVE, PAGEEXECUTEREADWRITE);
  ASSERT(Base # 0);
  w.s("SMOC heap base at "); w.h(Heap.HeapBase); w.sl("H.");
  w.s("Oberon base at    "); w.h(Base); w.sl("H.");

  (* Map code file *)
  mapaddress := OpenFileMap(fn);
  LoadModule(mapaddress);

  ASSERT(UnmapViewOfFile(mapaddress) # 0);
END Load;


BEGIN
  K.GetProc(K.Kernel, "VirtualAlloc",       VirtualAlloc);       ASSERT(VirtualAlloc       # NIL);
  K.GetProc(K.Kernel, "CreateFileA",        CreateFileA);        ASSERT(CreateFileA        # NIL);
  K.GetProc(K.Kernel, "CloseHandle",        CloseHandle);        ASSERT(CloseHandle        # NIL);
  K.GetProc(K.Kernel, "GetFileSizeEx",      GetFileSizeEx);      ASSERT(GetFileSizeEx      # NIL);
  K.GetProc(K.Kernel, "GetLastError",       GetLastError);       ASSERT(GetLastError       # NIL);
  K.GetProc(K.Kernel, "CreateFileMappingA", CreateFileMappingA); ASSERT(CreateFileMappingA # NIL);
  K.GetProc(K.Kernel, "MapViewOfFile",      MapViewOfFile);      ASSERT(MapViewOfFile      # NIL);
  K.GetProc(K.Kernel, "UnmapViewOfFile",    UnmapViewOfFile);    ASSERT(UnmapViewOfFile    # NIL);
  K.GetProc(K.Kernel, "LoadLibraryA",       LoadLibraryA);       ASSERT(LoadLibraryA       # 0);
  K.GetProc(K.Kernel, "GetProcAddress",     GetProcAddress);     ASSERT(GetProcAddress     # 0);
  K.GetProc(K.User,   "MessageBoxA",        MessageBoxA);        ASSERT(MessageBoxA        # 0);
END Loader.

