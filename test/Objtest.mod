MODULE Objtest;  (*$CONSOLE*)

IMPORT SYSTEM, Boot, Kernel, w := Writer, Classy;

TYPE
  branch3* = POINTER TO branch3Desc; branch3Desc* = RECORD (Classy.branch2) thing3*: INTEGER END;


VAR
  MessageBoxA: PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER): INTEGER;
  User:   INTEGER;
  i, j:   INTEGER;
  result: INTEGER;
  p:      POINTER TO RECORD i: INTEGER END;
  s:      ARRAY 10 OF CHAR;
  module: Boot.ModuleHeader;
  cr:     Classy.root;
  cb1:    Classy.branch1;
  cb3:    branch3;


PROCEDURE dump(r: Classy.root);
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
  w.sl("Objtest starting.");

  w.s("CR = $");   w.hn(w.crlf[0], 2);
  w.s(", LF = $"); w.hn(w.crlf[1], 2); w.sl(".");

(*Kernel.MessageBox("Objtest", "Starting.");*)

(*SYSTEM.LoadLibraryA(User, "user32.dll");
  SYSTEM.GetProcAddress(MessageBoxA, User, SYSTEM.ADR("MessageBoxA"));
  result := MessageBoxA(0, SYSTEM.ADR("Hurrah"), SYSTEM.ADR("Object load test"), 0);
  w.s("User handle: $"); w.h(User); w.sl(".");
*)

  Kernel.Collect;

  (* List loaded module info *)
  module := Boot.FirstModule;
  WHILE module # NIL DO
    w.s("Module ");    w.sn(module.name,   12);
    w.s("base at $");  w.hn(module.base,   12);
    w.s(", length $"); w.hn(module.length, 12);
    w.sl(".");
    module := module.next
  END;

  w.s("Classy.b1.thing1 = "); w.i(Classy.b1.thing1); w.sl(".");
  w.s("Classy.b2.thing2 = "); w.i(Classy.b2.thing2); w.sl(".");

  CASE Classy.r OF
  | Classy.branch1:  w.sl("Classy.r IS Classy.branch1.");
  | Classy.branch2:  w.sl("Classy.r IS Classy.branch2.");
  | Classy.root:     w.sl("Classy.r IS Classy.root.");
  END;

  cr := Classy.b2;      w.s("cr should be Classy.b2. ");
  CASE cr OF
  | Classy.branch1:  w.sl("cr IS Classy.branch1.");
  | Classy.branch2:  w.sl("cr IS Classy.branch2.");
  | Classy.root:     w.sl("cr IS Classy.root.");
  END;

  NEW(cb1);  cr := cb1; w.s("cr should be cb1. ");
  CASE cr OF
  | Classy.branch1:  w.sl("cr IS Classy.branch1.");
  | Classy.branch2:  w.sl("cr IS Classy.branch2.");
  | Classy.root:     w.sl("cr IS Classy.root.");
  END;

  NEW(cb3);  cr := cb3; w.s("cr should be cb3. ");
  CASE cr OF
  | branch3:         w.sl("cr IS branch3.");
  | Classy.branch1:  w.sl("cr IS Classy.branch1.");
  | Classy.branch2:  w.sl("cr IS Classy.branch2.");
  | Classy.root:     w.sl("cr IS Classy.root.");
  END;

  dump(cr);

(*
  i := 20;
  s[i] := "a";

  ASSERT(FALSE);

  p := NIL;  i := p.i;

  SYSTEM.PUT(SYSTEM.ADR(p), 5);  i := p.i;

  j := 0;  i := 5 DIV j;
*)
  w.sl("Objtest complete.");
END Objtest.
