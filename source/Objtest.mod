MODULE Objtest;  (*$OBJECT*)

IMPORT SYSTEM, Boot, Kernel, w := ObjWriter;

VAR
  MessageBoxA: PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER): INTEGER;
  User:   INTEGER;
  i, j:   INTEGER;
  result: INTEGER;
  p:      POINTER TO RECORD i: INTEGER END;
  s:      ARRAY 10 OF CHAR;
  module: Boot.ModuleHeader;
BEGIN
  w.sl("Objtest starting.");
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
