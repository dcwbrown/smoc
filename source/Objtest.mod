MODULE Objtest;  (*$OBJECT*)

IMPORT SYSTEM, w := ObjWriter;

VAR
  MessageBoxA: PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER): INTEGER;
  HUser: INTEGER;
  result: INTEGER;
BEGIN
  SYSTEM.LoadLibraryA(HUser, "user32.dll");
  SYSTEM.GetProcAddress(MessageBoxA, HUser, SYSTEM.ADR("MessageBoxA"));
  result := MessageBoxA(0, SYSTEM.ADR("Hurrah"), SYSTEM.ADR("Object load test"), 0);
  w.sl("Objtest complete.");
  (*ASSERT(FALSE);*)
END Objtest.
