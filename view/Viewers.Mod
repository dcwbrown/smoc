MODULE Viewers; (*JG 14.9.90 / NW 15.9.13 / AP 13.2.20 Extended Oberon / DCWB 8May23 *)
IMPORT Display, w := Writer, SYSTEM, Windows;

CONST
  restore*  = 0; (*message ids*)
  modify*   = 1;
  suspend*  = 2;
  inf       = 65535;
  DnLength* = 32;

  (* Viewer states *)
  Suspended = -2; (* or less *)
  (*          -1 ..  track or suspended filler *)
  Closed    = 0;
  IsFiller  = 1;
  Displayed = 2;  (* or more *)

TYPE
  Viewer* = POINTER TO ViewerDesc;
  ViewerDesc* = RECORD (Display.FrameDesc)
    prev:   Viewer;
    parent: Viewer;
    minH*:  INTEGER;
    state*: INTEGER
  END;

  ViewerMsg* = RECORD (Display.FrameMsg)
    id*:    INTEGER;  (*restore, modify, suspend*)
    X*, Y*: INTEGER;
    W*, H*: INTEGER;
    state*: INTEGER
  END;

  Track = POINTER TO TrackDesc;
  TrackDesc = RECORD (ViewerDesc)
    under: Display.Frame
  END;

  DisplayArea* = POINTER TO DisplayDesc;  (*logical display area*)
  DisplayDesc* = RECORD (ViewerDesc)
    curW*:  INTEGER;
    id*:    INTEGER;
    name*:  ARRAY DnLength OF CHAR;
    focus:  Viewer; (*focus viewer*)
    backup: Viewer  (*last closed viewer*)
  END;

VAR
  CurDisplay*:  DisplayArea;
  root*:        DisplayArea;
  FocusViewer*: Viewer;
  nextId:       INTEGER;


PROCEDURE ThisViewer* (D: DisplayArea; X, Y: INTEGER): Viewer;
VAR T, V: Display.Frame;
BEGIN
  IF (X < inf) & (Y < D.H) THEN
    T := D.child;
    REPEAT T := T.next UNTIL X < T.X + T.W;
    V := T.child;
    REPEAT V := V.next UNTIL Y < V.Y + V.H
  ELSE V := NIL
  END;
  RETURN V(Viewer)
END ThisViewer;


PROCEDURE This* (X, Y: INTEGER): Viewer;  (*for backward compatibility*)
VAR V: Viewer;
BEGIN
  IF CurDisplay # NIL THEN V := ThisViewer(CurDisplay, X, Y) ELSE V := NIL END;
  RETURN V
END This;


PROCEDURE ThisDisplay* (V: Viewer): DisplayArea;
BEGIN
  WHILE (V # NIL) & (V.parent # NIL) (*~display*) DO V := V.parent END;
  RETURN V(DisplayArea)
END ThisDisplay;


PROCEDURE Next* (V: Viewer): Viewer;
BEGIN
  IF (V IS DisplayArea) & (V.next = NIL) THEN
    V := root
  ELSE V := V.next(Viewer)
  END;
  RETURN V
END Next;


PROCEDURE Prev* (V: Viewer): Viewer;
BEGIN RETURN V.prev(Viewer)
END Prev;


PROCEDURE SetFocus* (D: DisplayArea; V: Viewer);
BEGIN
  w.s("Viewers.SetFocus to viewer $"); w.h(SYSTEM.ADR(V^)); w.sl(".");
  D.focus := V;
  w.s("  D $");  w.h(SYSTEM.ADR(D^));  w.s(", CurDisplay $");  w.h(SYSTEM.ADR(CurDisplay^)); w.sl(".");
  IF D = CurDisplay THEN FocusViewer := V END
END SetFocus;


PROCEDURE GetFocus* (D: DisplayArea): Viewer;
BEGIN RETURN D.focus
END GetFocus;


PROCEDURE Adjust* (F: Display.Frame; id, Y, H: INTEGER);
VAR M: ViewerMsg;
BEGIN
  IF Y+H > Display.Height THEN
    w.s("Viewers.Adjust Y "); w.i(Y); w.s(", H "); w.i(H);
    IF F IS Viewer THEN w.s(", viewer state "); w.i(F(Viewer).state) END;
    w.s(", viewer adr $"); w.i(SYSTEM.ADR(F)); w.sl(".");
    ASSERT(FALSE)
  END;
  M.Y := Y;  M.H := H;  M.id := id;  F.handle(F, M);
  F.Y := Y;  F.H := H
END Adjust;


PROCEDURE WVS(f: Display.Frame);  (* Write viewer state *)
BEGIN
  IF f IS Viewer THEN
    w.s(" state ");
    IF    f(Viewer).state < -1 THEN w.s("suspended")
    ELSIF f(Viewer).state = -1 THEN w.s("track, or suspended filler")
    ELSIF f(Viewer).state =  0 THEN w.s("closed")
    ELSIF f(Viewer).state =  1 THEN w.s("filler")
    ELSIF f(Viewer).state >  1 THEN w.s("displayed")
    END
  ELSE w.s(" not viewer") END
END WVS;


PROCEDURE Open* (D: DisplayArea; V: Viewer; X, Y: INTEGER); (* Y is requested top for new view *)
VAR T, v: Display.Frame; M: ViewerMsg;
BEGIN
  w.s("Viewers.Open at "); Display.wxy(X, Y); w.s(" min height "); w.i(V.minH); w.sl(".");
  IF (V.state = Closed) & (X < inf) THEN
    IF Y > D.H THEN Y := D.H END;

    w.sl("a");
    T := D.child.next; (* first track *)
    WHILE X >= T.X + T.W DO T := T.next END;

    w.sl("b");
    (* Find viewer containing Y *)
    v := T.child.next;
    WHILE Y >= v.Y + v.H DO v := v.next END;

    w.sl("c");
    ASSERT(Y >= v.Y);  ASSERT(Y < v.Y + v.H);

    IF Y + V.minH > v.Y + v.H THEN
      Y := v.Y + v.H - V.minH;
      w.s("  adjust Y to "); w.i(Y); w.sl(" to ensure minimal height.");
    END;

    w.s("  found existing viewer at "); Display.wfpos(v); WVS(v); w.sl(".");
    w.s("  next viewer at           "); Display.wfpos(v.next); WVS(v.next); w.sl(".");
        IF v = v.next THEN w.sl("  (v and v.next are the same viewer).")
                  ELSE w.sl("  (v and v.next are different viewers).") END;

    w.s("  Y "); w.i(Y);
    w.s(", v.Y "); w.i(v.Y);
    w.s(", v(Viewer).minH "); w.i(v(Viewer).minH);
    w.s(", v.next.Y "); w.i(v.next.Y);
    w.s(", v.next.H "); w.i(v.next.H);
    w.s(", T.H "); w.i(T.H);
    w.sl(".");

    (*IF (v.next.Y + v.next.H # T.H) & (Y <= v.Y + v(Viewer).minH) THEN*)
    IF (v(Viewer).state > 1) & (Y <= v.Y + v(Viewer).minH) THEN
      w.sl("  replace existing viewer.");
      V.X := T.X;
      V.W := T.W;
      V.Y := v.Y;
      V.H := v.H;
      M.id := suspend; M.state := Closed; v.handle(v, M); v(Viewer).state := Closed;
      V.next              := v.next;
      V.prev              := v(Viewer).prev;
      v(Viewer).prev.next := V;
      v.next(Viewer).prev := V;
      IF T.child = v THEN T.child := V END;
    ELSE
      (* Split - v gets space above Y, V is added below *)
      w.s("  split existing viewer at Y "); w.i(Y); w.sl(".");
      w.s("  v was ");  Display.wfpos(v);  w.sl(".");
      V.X := T.X;
      V.W := T.W;
      V.Y := Y;
      V.H := v.Y + v.H - Y;
      w.s("  Generated V ");  Display.wfpos(V);  w.sl(".");
      Adjust(v, modify, v.Y, Y - v.Y);
      V.next              := v.next;
      V.prev              := v(Viewer);
      v.next              := V;
      V.next(Viewer).prev := V;
    END;
    V(Viewer).parent    := T(Viewer);
    V.state             := Displayed
  END;
  w.s("  opened viewer at "); Display.wfpos(V); w.sl(".");
  ASSERT(V.Y + V.H <= Display.Height);
END Open;


PROCEDURE Validate* (V: Viewer; VAR Y, H: INTEGER);
VAR D: DisplayArea; v: Display.Frame; s: INTEGER;
BEGIN
  IF V.state > 1 THEN
    D := ThisDisplay(V);
    IF H < V.minH THEN H := V.minH END;
    v := V.prev;
    s := 0;
    WHILE v(Viewer).state > 1 DO s := s + v(Viewer).minH; v := v(Viewer).prev END;
    IF Y < s THEN Y := s END;
    v := V.next;
    s := 0;
    WHILE v(Viewer).state > 1 DO s := s + v(Viewer).minH; v := v.next END;
    IF    Y > D.H - s - V.minH THEN Y := D.H - s - V.minH; H := V.minH
    ELSIF Y + H > D.H - s      THEN H := D.H - s - Y
    END
  END
END Validate;


PROCEDURE Change* (V: Viewer; Y, H: INTEGER);
CONST none = 0; self = 1; above = 2; below = 3;
VAR v: Viewer; i, y, dY: INTEGER; m: ARRAY 3 OF INTEGER; up, down: BOOLEAN;
BEGIN (*Y, H validated*)
  w.sl("Viewers.Change called - not yet integrated.");  ASSERT(FALSE);
  IF V.state > 1 THEN
    m[0] := none; m[1] := self; m[2] := none;
    up   := Y + H > V.Y + V.H;
    down := Y + H < V.Y + V.H;
    IF Y < V.Y THEN m[0] := below;
      IF up THEN m[1] := above; m[2] := self ELSIF down THEN m[2] := above END
    ELSIF Y > V.Y THEN m[2] := below;
      IF up THEN m[0] := above ELSIF down THEN m[0] := self; m[1] := above END
    ELSE
      IF up THEN m[0] := above ELSIF down THEN m[2] := above ELSE m[1] := none END
    END;
    FOR i := 0 TO 2 DO
      IF m[i] = self THEN Adjust(V, modify, Y, H)
      ELSIF m[i] = above THEN v := V.next(Viewer);
        IF up THEN dY := Y + H - V.Y - V.H;
          WHILE (v.state > 1) & (dY > v.H - v.minH) DO dY := dY - v.H + v.minH; v := v.next(Viewer) END;
          y := v.Y + dY;
          Adjust(v, modify, y, v.H - dY); v := v.prev; y := y - v.minH;
          WHILE v # V DO Adjust(v, modify, y, v.minH); v := v.prev; y := y - v.minH END
        ELSE Adjust(v, modify, Y + H, v.Y + v.H - Y - H)
        END
      ELSIF m[i] = below THEN v := V.prev;
        IF v.state > 1 THEN
          IF Y > V.Y THEN Adjust(v, modify, v.Y, Y - v.Y)
          ELSE dY := V.Y - Y;
            WHILE (v.state > 1) & (dY > v.H - v.minH) DO dY := dY - v.H + v.minH; v := v.prev END;
            IF v.state > 1 THEN Adjust(v, modify, v.Y, v.H - dY); y := v.Y + v.H ELSE y := v.Y END;
            v := v.next(Viewer);
            WHILE v # V DO Adjust(v, modify, y, v.minH); y := y + v.H; v := v.next(Viewer) END
          END
        END
      END
    END
  END
END Change;


(*
PROCEDURE Spread* (D: DisplayArea; X, W: INTEGER); (*redistribute viewers vertically*)
VAR T, t, v, fil: Display.Frame; M: ViewerMsg; Y, H: INTEGER;
BEGIN
  IF X < inf THEN
    T := D.child.next;
    WHILE X >= T.X + T.W DO T := T.next END;
    t := T(Viewer).prev;
    WHILE X + W > T.X + T.W DO T := T.next END;
    M.id := restore;
    REPEAT t := t.next; fil := t.child; v := fil.next;
      IF v # fil THEN H := 0;
        REPEAT INC(H); v := v.next UNTIL v = fil;
        H := D.H DIV H; v := fil; Y := 0;
        REPEAT v := v.next; v.Y := Y;
          IF v.next = fil THEN v.H := D.H - Y; H := 0 ELSE v.H := H END;
          v.handle(v, M); Y := Y + v.H
        UNTIL v = fil
      END
    UNTIL t = T
  END
END Spread;
*)


PROCEDURE RestoreTrack (S: Display.Frame);
VAR T, t, v: Display.Frame; M: ViewerMsg;
BEGIN
  w.sl("Viewers.RestoreTrack called - not yet integrated.");  ASSERT(FALSE);
  t := S(Viewer).prev; T := S(Track).under;
  WHILE T.next # NIL DO T := T.next END;
  t.next := S(Track).under; t.next(Viewer).prev := t(Viewer); T.next := S.next; T.next(Viewer).prev := T(Viewer);
  M.id := restore;
  REPEAT t := t.next; v := t.child;
    REPEAT v := v.next; v.handle(v, M); v(Viewer).state := - v(Viewer).state
    UNTIL v = t.child
  UNTIL t = T
END RestoreTrack;


PROCEDURE Close* (V: Viewer);
VAR D: DisplayArea; T, U: Display.Frame; M: ViewerMsg;
BEGIN
  w.sl("Viewers.Close.");
  IF V.state > 1 THEN
    D := ThisDisplay(V);
    U := V.next;
    T := D.child;
    REPEAT T := T.next UNTIL V.X < T.X + T.W;
    IF (T(Track).under = NIL) OR (U.next # V) THEN
      M.id := suspend;  M.state := Closed;  V.handle(V, M);
      V.state := Closed;
      Adjust(U, modify, V.Y, V.H + U.H);
      V.prev.next := V.next;
      V.next(Viewer).prev := V.prev;
      D.backup := V
    ELSE (*close track*)
      M.id := suspend;  M.state := Closed;  V.handle(V, M);
      V.state := Closed;
      U.handle(U, M);
      U(Viewer).state := Closed;
      D.backup := V;
      RestoreTrack(T)
    END;
    V.next := NIL;  (*make other viewers unreachable from V*)
    V.prev := NIL;
    IF D.focus = V THEN SetFocus(D, ThisViewer(D, 0, 0)) END
  END
END Close;


(*
PROCEDURE Recall* (D: DisplayArea; VAR V: Viewer); (*last closed viewer*)
BEGIN V := D.backup
END Recall;
*)


PROCEDURE Flush* (D: DisplayArea); (*last closed viewer*)
BEGIN D.backup := NIL
END Flush;


PROCEDURE Locate* (D: DisplayArea; X, H: INTEGER;
                   VAR fil, top, bot, alt, max: Display.Frame);
VAR track, V: Display.Frame;
BEGIN
  w.s("Viewers.Locate starting with params X "); w.i(X); w.s(", H"); w.i(H); w.sl(".");
  IF X < inf THEN
    track := D.child;  REPEAT track := track.next UNTIL X < track.X + track.W;
    fil := track.child;
    top := fil.next;           ASSERT(top(Viewer).prev = fil);
    bot := fil(Viewer).prev;   ASSERT(bot.next = fil);
    IF top.next # fil THEN
      (* There's more than one viewer in the track *)
      alt := bot(Viewer).prev;
      V   := alt(Viewer).prev;
      WHILE (V # fil) & (alt.H < H) DO
        IF V.H > alt.H THEN alt := V END;
        V := V(Viewer).prev
      END
    ELSE alt := top
    END;
    max := track.child;
    V := max.next;
    WHILE V # fil DO
      IF V.H > max.H THEN max := V END;
      V := V.next
    END
  END;
  w.sl("Viewers.Locate complete, frames returned:");
  w.s("  fil $"); w.h(SYSTEM.ADR(fil^)); w.s(" at "); w.i(fil.X); w.c(","); w.i(fil.Y); w.s(" size ");w.i(fil.W); w.c(","); w.i(fil.H); w.sl(".");
  w.s("  top $"); w.h(SYSTEM.ADR(top^)); w.s(" at "); w.i(top.X); w.c(","); w.i(top.Y); w.s(" size ");w.i(top.W); w.c(","); w.i(top.H); w.sl(".");
  w.s("  bot $"); w.h(SYSTEM.ADR(bot^)); w.s(" at "); w.i(bot.X); w.c(","); w.i(bot.Y); w.s(" size ");w.i(bot.W); w.c(","); w.i(bot.H); w.sl(".");
  w.s("  alt $"); w.h(SYSTEM.ADR(alt^)); w.s(" at "); w.i(alt.X); w.c(","); w.i(alt.Y); w.s(" size ");w.i(alt.W); w.c(","); w.i(alt.H); w.sl(".");
  w.s("  max $"); w.h(SYSTEM.ADR(max^)); w.s(" at "); w.i(max.X); w.c(","); w.i(max.Y); w.s(" size ");w.i(max.W); w.c(","); w.i(max.H); w.sl(".");
END Locate;


PROCEDURE Install* (parent, F: Display.Frame); (*frame F as a subframe into parent frame*)
VAR f: Display.Frame;
BEGIN F.next := NIL;
  IF parent.child = NIL THEN parent.child := F
  ELSE f := parent.child;
    WHILE f.next # NIL DO f := f.next END;
    f.next := F
  END
END Install;


PROCEDURE Remove* (parent, F: Display.Frame); (*subframe F from its parent frame*)
VAR f, f0: Display.Frame;
BEGIN
  IF parent.child = F THEN parent.child := F.next
  ELSE f0 := parent.child; f := f0.next;
    WHILE (f # NIL) & (f # F) DO f0 := f; f := f.next END;
    IF f # NIL THEN f0.next := f.next END
  END
END Remove;


PROCEDURE InitFiller (X, Y, W, H, minH: INTEGER; Filler: Viewer); (*except fields handle, parent, dsc*)
BEGIN
  Filler.X     := X;
  Filler.Y     := Y;
  Filler.W     := W;
  Filler.H     := H;
  Filler.minH  := minH;
  Filler.state := 1;
  Filler.next  := Filler;
  Filler.prev  := Filler
END InitFiller;


PROCEDURE InitTrack* (D: DisplayArea; W, H: INTEGER; Filler: Viewer);
VAR T, fillerTrack: Track; fillerViewer: Viewer;
BEGIN
  InitFiller(D.curW, 0, W, H, 0, Filler); Filler.child := NIL;
  NEW(T);  T.child := Filler;  Filler.parent := T;
  T.parent := D;
  T.under  := NIL;
  T.X      := D.curW;
  T.Y      := 0;
  T.W      := W;
  T.H      := H;
  T.minH   := 0;
  T.state  := -1;
  fillerTrack      := D.child(Track);
  fillerViewer     := fillerTrack.child(Viewer);
  fillerViewer.X   := D.curW + W;
  fillerViewer.W   := inf - fillerViewer.X;
  fillerTrack.X    := fillerViewer.X;
  fillerTrack.W    := fillerViewer.W;
  T.next           := fillerTrack;
  T.prev           := fillerTrack.prev;
  T.prev.next      := T;
  fillerTrack.prev := T;
  D.curW           := D.curW + W
END InitTrack;


PROCEDURE OpenTrack* (D: DisplayArea; X, W: INTEGER; Filler: Viewer);
VAR newT: Track; S, T, t, v: Display.Frame; M: ViewerMsg; v0: Viewer;
BEGIN
  IF (X < inf) & (Filler.state = Closed) THEN
    T := D.child.next;
    WHILE X >= T.X + T.W DO T := T.next END;
    S := T(Viewer).prev;
    WHILE X + W > T.X + T.W DO T := T.next END;
    M.id := suspend;
    t    := S;
    REPEAT t := t.next; v := t.child;
      REPEAT
        v       := v.next;
        M.state := -v(Viewer).state;
        v.handle(v, M);
        v(Viewer).state := M.state
      UNTIL v = t.child
    UNTIL t = T;
    InitFiller(S.next.X, 0, T.X + T.W - S.next.X, D.H, 0, Filler);
    Filler.child := NIL;
    NEW(newT);
    newT.child    := Filler;
    Filler.parent := newT;
    newT.parent   := D;
    newT.X        := Filler.X;
    newT.Y        := 0;
    newT.W        := Filler.W;
    newT.H        := D.H;
    newT.minH     := 0;
    newT.state    := -1; (*track*)
    newT.under    := S.next;
    S.next(Viewer).prev := NIL;
    newT.next     := T.next;
    newT.prev     := S(Viewer);
    S.next        := newT;
    T.next(Viewer).prev := newT;
    T.next := NIL
  END
END OpenTrack;


(*
PROCEDURE CloseTrack* (D: DisplayArea; X: INTEGER);
VAR T, V: Display.Frame; M: ViewerMsg; hasFocus: BOOLEAN;
BEGIN
  IF X < inf THEN
    T := D.child;
    REPEAT T := T.next UNTIL X < T.X + T.W;
    IF T(Track).under # NIL THEN hasFocus := FALSE;
      M.id := suspend; M.state := Closed; V := T.child;
      REPEAT V := V.next; V.handle(V, M); V(Viewer).state := Closed;
        IF D.focus = V THEN hasFocus := TRUE END
      UNTIL V = T.child;
      RestoreTrack(T);
      IF hasFocus THEN SetFocus(D, ThisViewer(D, 0, 0)) END
    END
  END
END CloseTrack;
*)


PROCEDURE Broadcast* (VAR M: Display.FrameMsg); (*to current display*)
VAR T, V: Display.Frame;
BEGIN
  IF CurDisplay # NIL THEN
    T := CurDisplay.child.next;
    WHILE T # CurDisplay.child DO
      V := T.child;
      REPEAT V := V.next;  V.handle(V, M) UNTIL V = T.child;
      T := T.next
    END
  END
END Broadcast;


PROCEDURE InitDisplay* (D: DisplayArea; W, H: INTEGER; name: ARRAY OF CHAR);
VAR fillerTrack: Track; fillerViewer: Viewer;
BEGIN
  D.id     := nextId;
  D.name   := name;
  D.curW   := 0;
  D.focus  := NIL;
  D.backup := NIL;
  D.X      := 0;
  D.Y      := 0;
  D.W      := W;
  D.H      := H;
  D.minH   := 1;
  D.state  := Closed;
  D.parent := NIL;
  NEW(fillerTrack);  InitFiller(0, 0, inf, H, 0, fillerTrack);
  D.child := fillerTrack;
  NEW(fillerViewer);  InitFiller(0, 0, inf, H, 0, fillerViewer);
  fillerTrack.child   := fillerViewer;
  fillerTrack.parent  := D;
  fillerViewer.parent := fillerTrack;
  fillerViewer.child  := NIL;
  D.focus             := fillerViewer;
  IF root = NIL THEN root := D ELSE D.prev := root.prev; D.prev.next := D END;
  root.prev := D;
  D.next    := NIL;
  nextId    := nextId + 1
END InitDisplay;


PROCEDURE SetDisplay* (D: DisplayArea);
VAR M: ViewerMsg;
BEGIN
  IF CurDisplay # NIL THEN
    M.id  := suspend; M.state := Closed; Broadcast(M);
    CurDisplay.state := Closed
  END;
  CurDisplay := D;
  D.state := Displayed;
  SetFocus(D, ThisViewer(D, 0, 0));
  M.id := restore; Broadcast(M)
END SetDisplay;


PROCEDURE GetDisplay* (id: INTEGER): DisplayArea;
VAR D: DisplayArea;
BEGIN D := root;
  WHILE (D # NIL) & (id # D.id) DO D := D.next(DisplayArea) END;
  RETURN D
END GetDisplay;


PROCEDURE CloseDisplay* (D: DisplayArea; hint: INTEGER); (*except last display*)
BEGIN
  IF root.next # NIL THEN D.state := Closed;
    IF    D = root      (*first*) THEN  D.next(Viewer).prev := D.prev;
                                        root := D.next(DisplayArea)
    ELSIF D = root.prev (*last*)  THEN  root.prev := D.prev;
                                        D.prev.next := NIL
                                  ELSE  D.next(Viewer).prev := D.prev;
                                        D.prev.next := D.next
    END;
    IF D = CurDisplay THEN D := GetDisplay(hint);
      IF D = NIL THEN SetDisplay(root) ELSE SetDisplay(D) END
    END
  END
END CloseDisplay;

BEGIN
  w.sl("Viewers initialising.");
  root        := NIL;
  CurDisplay  := NIL;
  FocusViewer := NIL;
  nextId      := 0;
  w.sl("Viewers initialised.");
END Viewers.
