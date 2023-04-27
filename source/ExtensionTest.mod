MODULE ExtensionTest;  (*$CONSOLE*)

IMPORT SYSTEM, w := Writer;

TYPE 
  root   = POINTER TO RECORD        thing:  INTEGER END;
  branch = POINTER TO RECORD (root) thing1: INTEGER END;

VAR
  r: root;
  b: branch;

PROCEDURE dump(r: root);
VAR
  radr: INTEGER;
  tadr: INTEGER;
BEGIN
  SYSTEM.GET(SYSTEM.ADR(r), radr);
  w.s("r points to  $"); w.h(radr); w.sl(".");
  SYSTEM.GET(radr-10H, tadr);
  w.s("type info at $"); w.h(tadr); w.sl(" contains:");
  w.DumpMem(2, tadr, 0, 80);
END dump;

BEGIN
  NEW(b);  b.thing := 0;  b.thing1 := 1;

  r := b;

  CASE r OF
  | branch:  w.sl("r IS branch.");
  | root:    w.sl("r IS root.");
  END;

  dump(r)

END ExtensionTest.
