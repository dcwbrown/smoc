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

  cr := Classy.b2;
  CASE cr OF
  | Classy.branch1:  w.sl("cr IS Classy.branch1.");
  | Classy.branch2:  w.sl("cr IS Classy.branch2.");
  | Classy.root:     w.sl("cr IS Classy.root.");
  END;

  NEW(cb1);  cr := cb1;
  CASE cr OF
  | Classy.branch1:  w.sl("cr IS Classy.branch1.");
  | Classy.branch2:  w.sl("cr IS Classy.branch2.");
  | Classy.root:     w.sl("cr IS Classy.root.");
  END;

  NEW(cb3);  cr := cb3;
  CASE cr OF
  | Classy.branch1:  w.sl("cr IS Classy.branch1.");
  | Classy.branch2:  w.sl("cr IS Classy.branch2.");
  | Classy.root:     w.sl("cr IS Classy.root.");
  END;



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
