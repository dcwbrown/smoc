MODULE ObjDump;  (*$CONSOLE*)  (* Dump content od .x64 object file *)
IMPORT SYSTEM, Files, Rtl,w := Writer;

TYPE
  ModuleHeader = POINTER TO ModuleHeaderDesc;
  ModuleHeaderDesc = RECORD
    length*:     INTEGER;          (*   0                                *)
    next*:       ModuleHeader;     (*   8                                *)
    name:        ARRAY 32 OF CHAR; (*  16                                *)
    base:        INTEGER;          (*  48                                *)
    code*:       INTEGER;          (*  56                                *)
    init:        INTEGER;          (*  64                                *)
    trap*:       INTEGER;          (*  72                                *)
    key0, key1:  INTEGER;          (*  80                                *)
    imports:     INTEGER;          (*  88 list of import names and keys  *)
    importCount: INTEGER;          (*  96 number of imports at base+128  *)
    exports:     INTEGER           (* 104 array of export addresses      *)
  END;

  VAR
  X64file: Files.File;  (* Input file *)
  X64:     Files.Rider;
  Buf:     ARRAY 80000H OF BYTE;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)


PROCEDURE DumpX64;
VAR
  filename:     ARRAY 256 OF CHAR;
  header:       ModuleHeaderDesc;
  name:         ARRAY 1024 OF CHAR;
  nameOffset:   INTEGER;
  i:            INTEGER;
  key:          INTEGER;
  export:       INTEGER;
  adr:          INTEGER;
  trap:         INTEGER;
  line, col:    INTEGER;
  modno, expno: INTEGER;
BEGIN
  Rtl.GetArg(1, filename);
  IF filename[0] = 0X THEN
    w.sl("Expected filename.")
  ELSE
    X64file := Files.Old(filename);
    IF X64file = NIL THEN
      w.s("File '"); w.s(filename); w.sl("' not found.");
    ELSE
      w.s("Dumping '"); w.s(filename); w.sl("'.");

      (* Load header *)
      Files.Set(X64, X64file, 0);
      Files.ReadBytes(X64, header, SYSTEM.SIZE(ModuleHeaderDesc));
      w.s("header.length:      $"); w.h(header.length);          w.sl(".");
      w.s("header.base:        $"); w.h(header.base);            w.sl(".");
      w.s("header.code:        $"); w.h(header.code);            w.sl(".");
      w.s("header.init:        $"); w.h(header.init);            w.sl(".");
      w.s("header.trap:        $"); w.h(header.trap);            w.sl(".");
      w.s("header.name:        '"); w.s(header.name);            w.sl("'.");
      w.s("header.key0:        $"); w.h(header.key0);            w.sl(".");
      w.s("header.key1:        $"); w.h(header.key1);            w.sl(".");
      w.s("header.imports:     $"); w.h(header.imports);         w.sl(".");
      w.s("header.importCount: $"); w.h(header.importCount);     w.sl(".");
      w.s("header.exports:     $"); w.h(header.exports);         w.sl(".");
      w.l;

      (* Module name and key *)
      w.s("MODULE '"); w.s(header.name); w.s("' key $");
      w.hn(header.key0, 16); w.s(', $'); w.hn(header.key1, 16); w.sl(".");

      (* Imported module names and keys *)
      IF header.imports = 0 THEN
        w.sl("No imported modules.")
      ELSE
        w.sl("Imported modules:");
        Files.Set(X64, X64file, header.imports);
        Files.ReadString(X64, name);
        i := 0;
        WHILE name[0] # 0X DO
          w.s("  ");  w.in(i, 3);  w.s(": "); w.s(name);
          Files.ReadInt(X64, key); w.s(" $"); w.h(key);
          Files.ReadInt(X64, key); w.s(" $"); w.h(key);
          w.sl(".");
          Files.ReadString(X64, name);
          INC(i)
        END
      END;

      (* Exported addresses *)
      IF header.exports = 0 THEN
        w.sl("No exports.")
      ELSE
        w.sl("Exported addresses:");
        Files.Set(X64, X64file, header.exports);
        Files.ReadInt(X64, export);  i := 0;
        WHILE export # 8000000000000000H DO
          w.s("  "); w.in(i, 3); w.s(": $"); w.h(export); w.sl(".");
          Files.ReadInt(X64, export);  INC(i);
        END
      END;

      IF header.importCount > 0 THEN
        w.sl("Imports.");
        Files.Set(X64, X64file, header.base + 128);
        FOR i := 0 TO header.importCount-1 DO
          Files.ReadInt(X64, expno);
          modno := expno DIV 100000000H;  expno := expno MOD 100000000H;
          w.s(" modno "); w.i(modno); w.s(", expno "); w.i(expno); w.sl(".");
        END
      END;

      (* Traps *)
      w.sl("Trap table:");
      Files.Set(X64, X64file, header.trap);
      Files.ReadInt(X64, adr);
      WHILE adr # -1 DO
        trap := ASR(adr, 60) MOD 10H;
        line := ASR(adr, 40) MOD 100000H;
        col  := ASR(adr, 30) MOD 400H;
        adr  := adr MOD 40000000H;
        CASE trap OF
        | 0: w.s("  modkey trap  at $");
        | 1: w.s("  array trap   at $");
        | 2: w.s("  type trap    at $");
        | 3: w.s("  string trap  at $");
        | 4: w.s("  nil trap     at $");
        | 5: w.s("  nilProc trap at $");
        | 6: w.s("  divide trap  at $");
        | 7: w.s("  assert trap  at $");
        | 8: w.s("  rtl trap     at $");
        | 9: w.s("  GET trap     at $");
        |10: w.s("  PUT trap     at $");
        END;
        w.hn(adr,12); w.s(" "); w.i(line); w.s(":"); w.i(col); w.sl(".");
        Files.ReadInt(X64, adr)
      END;


      (* Load and dump static data *)
      Files.Set(X64, X64file, header.base);
      Files.ReadBytes(X64, Buf, header.code - header.base);
      w.sl("Static data:");
      w.DumpMem(2, SYSTEM.ADR(Buf), 0, header.code - header.base);

      (* Load and dump code *)
      Files.Set(X64, X64file, header.code);
      Files.ReadBytes(X64, Buf, header.trap - header.code);
      w.sl("Code:");
      w.DumpMem(2, SYSTEM.ADR(Buf), 0, header.trap - header.code);


(*

      (* 5b. Module ptr table address *)
      Files.ReadInt(X64, modPtrTable);
      w.s("Module ptr table at $");  w.h(modPtrTable);  w.sl(".");

      (* 4. Initialised static data *)
      Files.ReadInt(X64, i);
      w.s("Static data size "); w.i(i); w.sl(".");
      IF i > 0 THEN
        Files.ReadBytes(X64, buf, i);
        w.DumpMem(2, SYSTEM.ADR(buf), 0, i);
      END;

      (* 6. Code *)
      Files.ReadInt(X64, i);
      w.s("Code size "); w.i(i); w.sl(".");
      IF i > 0 THEN
        Files.ReadBytes(X64, buf, i);
        w.DumpMem(2, SYSTEM.ADR(buf), 0, i);
      END;

      (* 8. Trap table *)
      Files.ReadInt(X64, i);
      IF i <= 0 THEN
        w.sl("No trap table.");
      ELSE
        w.s("Trap table length "); w.i(i); w.sl(" bytes:");
        WHILE i > 0 DO
          Files.ReadInt(X64, adr);
          trap := ASR(adr, 60) MOD 10H;
          line := ASR(adr, 40) MOD 100000H;
          col  := ASR(adr, 30) MOD 400H;
          adr  := adr MOD 40000000H;
          CASE trap OF
          | 0: w.s("  modkey trap  at $");
          | 1: w.s("  array trap   at $");
          | 2: w.s("  type trap    at $");
          | 3: w.s("  string trap  at $");
          | 4: w.s("  nil trap     at $");
          | 5: w.s("  nilProc trap at $");
          | 6: w.s("  divide trap  at $");
          | 7: w.s("  assert trap  at $");
          | 8: w.s("  rtl trap     at $");
          END;
          w.hn(adr,12); w.s(" "); w.i(line); w.s(":"); w.i(col); w.sl(".");
          DEC(i, 8);
        END
      END;

      (* 9. List of export addresses in export number order *)
      Files.ReadInt(X64, adr);
      IF adr = 0 THEN
        w.sl("No export addresses.");
      ELSE
        w.sl("Export addresses:");
        n := 0;
        WHILE adr # 0  DO
          w.s("  "); w.i(n);
          i :=  ASR(adr, 62) MOD 4;
          adr := adr MOD 4000000000000000H;
          CASE i OF
          | 1: w.s(": type at $")
          | 2: w.s(": var  at $")
          | 3: w.s(": proc at $")
          END;
          w.h(adr); w.sl(".");
          Files.ReadInt(X64, adr);  INC(n);
        END
      END
*)
    END
  END
END DumpX64;

BEGIN DumpX64
END ObjDump.