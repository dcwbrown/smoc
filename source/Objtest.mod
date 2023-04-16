MODULE Objtest;  (*$OBJECT*)

IMPORT SYSTEM, Kernel, w := ObjWriter;

VAR
  MessageBoxA: PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER): INTEGER;
  User:   INTEGER;
  i, j:   INTEGER;
  result: INTEGER;
  p:      POINTER TO RECORD i: INTEGER END;
BEGIN
  Kernel.MessageBox("Objtest", "Starting.");
  SYSTEM.LoadLibraryA(User, "user32.dll");
  SYSTEM.GetProcAddress(MessageBoxA, User, SYSTEM.ADR("MessageBoxA"));
  result := MessageBoxA(0, SYSTEM.ADR("Hurrah"), SYSTEM.ADR("Object load test"), 0);
  w.s("User handle: $"); w.h(User); w.sl(".");


  p := NIL;  i := p.i;

  SYSTEM.PUT(SYSTEM.ADR(p), 5);  i := p.i;

  ASSERT(FALSE);

  j := 0;  i := 5 DIV j;

  w.sl("Objtest complete.");
END Objtest.
