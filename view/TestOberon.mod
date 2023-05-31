MODULE TestOberon;
IMPORT SYSTEM, Display, Viewers, System, Oberon, Texts,
       MenuViewers, TextFrames, Edit, w := Writer;

PROCEDURE wpair(x, y: INTEGER);
BEGIN w.i(x);  w.c(",");  w.i(y) END wpair;

PROCEDURE DumpState(v: Viewers.Viewer);
BEGIN
  IF    v.state < -1 THEN w.s("(suspended) ");
  ELSIF v.state = -1 THEN w.s("(track/suspended filler) ");
  ELSIF v.state =  0 THEN w.s("(closed) ");
  ELSIF v.state =  1 THEN w.s("(filler) ");
                     ELSE w.s("(displayed) ")
  END;
END DumpState;

PROCEDURE DumpFrame(f: Display.Frame);
BEGIN
  w.c("$");          w.h(SYSTEM.ADR(f^));
  w.s(" handler $"); w.h(SYSTEM.VAL(INTEGER, f.handle));
  CASE f OF
  | MenuViewers.Viewer:  w.s(" MenuViewers.Viewer  "); DumpState(f)
  | TextFrames.Frame:    w.s(" TextFrames.Frame    ")
  | Viewers.DisplayArea: w.s(" Viewers.DisplayArea "); DumpState(f)
  | Viewers.Viewer:      w.s(" Viewers.Viewer      "); DumpState(f)
  | Display.Frame:       w.s(" Display.Frame       ")
  END;
  wpair(f.X, f.Y); w.c(" "); wpair(f.W, f.H)
END DumpFrame;

PROCEDURE DumpFrameList(indent: INTEGER; f: Display.Frame);
VAR i: INTEGER;  lf: Display.Frame;
BEGIN
  lf := f.next;
  w.b(indent);   w.s("[s] ");  DumpFrame(f);  w.sl(".");
  IF f.child # NIL THEN DumpFrameList(indent+4, f.child) END;
  i := 1;
  WHILE (lf # NIL) & (lf # f) DO
    w.b(indent);   w.c("[");  w.i(i);  w.s("] ");   DumpFrame(lf);  w.sl(".");
    IF lf.child # NIL THEN DumpFrameList(indent+4, lf.child) END;
    INC(i);  lf := lf.next
  END
END DumpFrameList;

PROCEDURE DumpDisplay;
BEGIN
  w.sl("Display frame dump:");
  DumpFrameList(2, Viewers.root)
END DumpDisplay;

BEGIN
  w.sl("TestOberon starting.");
  Oberon.SetDumpDisplay(DumpDisplay);
  Oberon.Loop
END TestOberon.
