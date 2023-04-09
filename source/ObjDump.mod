MODULE ObjDump;  (*$CONSOLE*)  (* Dump content od .x64 object file *)
IMPORT SYSTEM, Files, Rtl,w := Writer;

VAR
  X64: Files.Rider;  (* Input file *)
  buf: ARRAY 80000H OF BYTE;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(*
PROCEDURE WriteImportReferences;
VAR
  impmod:     B.Module;
  impobj:     B.Object;
  import:     B.Ident;
  modno:      INTEGER;
  adr, expno: INTEGER;
BEGIN
  impmod := B.modList;
  modno := 0;
  WHILE impmod # NIL DO
    import := impmod.impList;
    WHILE import # NIL DO
      impobj := import.obj;
      IF impobj.class = B.cType THEN ASSERT(impobj.type.form = B.tRec);
                                  adr := impobj.type.adr;    expno := impobj.type.expno
      ELSIF impobj IS B.Var  THEN adr := impobj(B.Var).adr;  expno := impobj(B.Var).expno
      ELSIF impobj IS B.Proc THEN adr := impobj(B.Proc).adr; expno := impobj(B.Proc).expno
      END;
      ASSERT(adr >= 128);
      Files.WriteCard32(X64, adr);
      Files.WriteCard16(X64, modno);
      Files.WriteCard16(X64, expno);
      import := import.next
    END;
    impmod := impmod.next;  INC(modno)
  END;
  Files.WriteCard(X64, 0);
END WriteImportReferences;
*)

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)


PROCEDURE DumpX64;
VAR
  filename:     ARRAY 256 OF CHAR;
  x64file:      Files.File;
  modid:        ARRAY 64 OF CHAR;
  key0, key1:   INTEGER;
  i, n:         INTEGER;
  modno, impno: SYSTEM.CARD16;
  adr:          INTEGER;
  line, col,
  trap:         INTEGER;
  modPtrTable:  INTEGER;
  importCount:  INTEGER;
BEGIN
  Rtl.GetArg(filename, 1);
  IF filename[0] = 0X THEN
    w.sl("Expected filename.")
  ELSE
    x64file := Files.Old(filename);
    IF x64file = NIL THEN
      w.s("File '"); w.s(filename); w.sl("' not found.");
    ELSE
      w.s("Dumping '"); w.s(filename); w.sl("'.");
      Files.Set(X64, x64file, 0);

      (* 1. Module name and key *)
      Files.ReadString(X64, modid);
      Files.ReadInt(X64, key0);  Files.ReadInt(X64, key1);

      w.s("MODULE '"); w.s(modid); w.s("' key $");
      w.hn(key0, 16); w.s(', $'); w.hn(key1, 16); w.sl(".");

      (* Import count *)
      Files.ReadInt(X64, importCount);
      w.s("Import count: ");  w.i(importCount);  w.sl(".");

      (* 5b. Module ptr table address *)
      Files.ReadInt(X64, modPtrTable);
      w.s("Module ptr table at $");  w.h(modPtrTable);  w.sl(".");

      (* 7. Code offset of initialisation entry point, if any *)
      Files.ReadInt(X64, i);
      IF i < 0 THEN
        w.sl("No initialisation entry point.");
      ELSE
        w.s("Initialisation entry point $"); w.h(i); w.sl(".");
      END;

      (* 2. List of referenced modules *)
      Files.ReadString(X64, modid);
      IF modid[0] = 0X THEN
        w.sl("No imported modules.")
      ELSE
        w.sl("Imported modules:");
        WHILE modid[0] # 0X DO
          Files.ReadInt(X64, key0);  Files.ReadInt(X64, key1);
          w.s("  '"); w.s(modid); w.s("' key $");
          w.hn(key0, 16); w.s(', $'); w.hn(key1, 16); w.sl(".");
          Files.ReadString(X64, modid);
        END
      END;

      Files.ReadInt(X64, i);
      w.s("Module global VAR size "); w.i(i); w.sl(".");

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
    END
  END
END DumpX64;

BEGIN DumpX64
END ObjDump.