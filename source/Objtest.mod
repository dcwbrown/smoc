MODULE Objtest;  (*$OBJECT*)

IMPORT SYSTEM, w := ObjWriter;

VAR
  MessageBoxA: PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER): INTEGER;
  User:   INTEGER;
  result: INTEGER;
BEGIN
  SYSTEM.LoadLibraryA(User, "user32.dll");
  SYSTEM.GetProcAddress(MessageBoxA, User, SYSTEM.ADR("MessageBoxA"));
  result := MessageBoxA(0, SYSTEM.ADR("Hurrah"), SYSTEM.ADR("Object load test"), 0);
  w.s("User handle: $"); w.h(User); w.sl(".");
  w.sl("Objtest complete.");
  (*ASSERT(FALSE);*)
END Objtest.
