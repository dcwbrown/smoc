MODULE MenuViewers; (*JG 26.8.90 / 16.9.93 / NW 10.3.13 / AP 15.8.22 Extended Oberon*)
IMPORT Input, Windows, Display, Viewers, Oberon, w := Writer, SYSTEM;

TYPE
  Viewer* = POINTER TO ViewerDesc;
  ViewerDesc* = RECORD (Viewers.ViewerDesc) menuH*: INTEGER END;

PROCEDURE DrawBorders (V: Viewer; Y, H: INTEGER);
BEGIN
  IF Y+H > Display.Height THEN
    w.s("MenuViewers.DrawBorders Y "); w.i(Y);
    w.s(", H ");                       w.i(H);
    w.s(", viewer state ");            w.i(V.state);
    w.s(", viewer adr $");             w.i(SYSTEM.ADR(V));
    w.sl(".");
    ASSERT(FALSE);
  END;
  w.s("MenuViewer.DrawBorders viewer at "); Display.wfpos(V); w.s("; Y "); w.i(Y); w.s(", H "); w.i(H); w.sl(".");
  Display.FillRectangle(V.X,           Y,         1,       H, Display.white);
  Display.FillRectangle(V.X + V.W - 1, Y,         1,       H, Display.white);
  Display.FillRectangle(V.X + 1,       Y,         V.W - 2, 1, Display.white);
  Display.FillRectangle(V.X + 1,       Y + H - 1, V.W - 2, 1, Display.white);
END DrawBorders;

PROCEDURE Restore (V: Viewer);
VAR Menu, Main: Display.Frame;
BEGIN
  w.s("MenuViewer.Restore viewer at "); Display.wfpos(V); w.sl(".");
  Menu := V.child;
  Main := V.child.next;
  Oberon.RemoveMarks(V.X, V.Y, V.W, V.H);
  DrawBorders(V, V.Y, V.H);
  IF V.H > V.menuH + 1 THEN
    (* There is sufficient room for the whole menu *)
    Menu.X := V.X + 1;
    Menu.Y := V.Y + 1;
    Menu.W := V.W - 2;
    Menu.H := V.menuH - 1;

    Main.X := V.X + 1;
    Main.Y := V.Y + V.menuH + 1;
    Main.W := V.W - 2;
    Main.H := V.H - V.menuH - 1;

    w.s(" Menu at "); Display.wfpos(Menu); w.sl(".");
    w.s(" Main at "); Display.wfpos(Main); w.sl(".");

    Viewers.Adjust(Menu, Viewers.restore, Menu.Y, Menu.H);
    Viewers.Adjust(Main, Viewers.restore, Main.Y, Main.H)
  ELSE
    Menu.X := V.X + 1;
    Menu.Y := V.Y + 1;
    Menu.W := V.W - 2;
    Menu.H := V.H - 2;
    Viewers.Adjust(Menu, Viewers.restore, Menu.Y, Menu.H)
  END
END Restore;


PROCEDURE Modify (V: Viewer; Y, H: INTEGER);
VAR Menu, Main: Display.Frame;
BEGIN
  Menu := V.child;
  Main := V.child.next;
  Oberon.RemoveMarks(V.X, V.Y, V.W, V.H);

  IF H > V.menuH + 1 THEN
    (* Sufficient room for menu *)
    IF Y < V.Y THEN
      (* Top moving up *)
      (* Put menu at top *)
      Viewers.Adjust(Menu, Viewers.modify, Y,  V.menuH - 1);
      (* Fill remainder with main *)
      Viewers.Adjust(Main, Viewers.modify, Y + V.menuH + 1, H - V.menuH - 1)
    ELSE
      (* Top not changing or moving down *)
      (* Put main at bottom *)
      Viewers.Adjust(Main, Viewers.modify, Y + V.menuH + 1, H - V.menuH - 1);
      (* Put menu at top *)
      Viewers.Adjust(Menu, Viewers.modify, Y, V.menuH - 1)
    END
  ELSIF Y + H > V.Y + V.H THEN
    (* Insufficient room for menu and top moving up *)
    Viewers.Adjust(Menu, Viewers.modify, Y, H - 2);
    Viewers.Adjust(Main, Viewers.modify, Y + V.menuH + 1, 0)
  ELSE
    (* Insufficient room for menu and top not changing, or moving down *)
    Viewers.Adjust(Main, Viewers.modify, Y + V.menuH + 1, 0);
    Viewers.Adjust(Menu, Viewers.modify, Y, H - 2)
  END;

  DrawBorders(V, Y, H)
END Modify;

PROCEDURE Suspend (V: Viewer);
VAR Menu, Main: Display.Frame;
BEGIN
  w.sl("MenuViewers.Suspend.");
  Menu := V.child; Main := V.child.next;
  Viewers.Adjust(Main, Viewers.modify, V.Y + V.menuH, 0);
  Viewers.Adjust(Menu, Viewers.modify, V.Y + 1, 0)
END Suspend;


PROCEDURE Move (V: Viewer; X, Y: INTEGER; Keys: SET);
VAR Y0, H: INTEGER; keysum: SET;
    D: Viewers.DisplayArea; v: Viewers.Viewer;
BEGIN
  w.sl("MenuViewers.Move called.");
  ASSERT(Keys # {});
  keysum := Keys;  Oberon.SetStarPos(X, Y);  Y0 := Y;
  WHILE Windows.GetMouseMessage(X, Y, Keys) & (Keys # {}) DO
    keysum := keysum + Keys;  Oberon.SetStarPos(X, Y)
  END;
  IF (Y # Y0) & ~(0 IN keysum) THEN (*~MR*)

(* TODO - needs rewrite for top-down measurement *)

    v := Viewers.This(X, Y);
    IF v = V.next THEN               (* mouse in next window (above) *)
      Y0 := V.Y;
      IF Y > v.Y + v.H - v.minH THEN (* mouse above main part of window *)
        H := v.Y + v.H - Y0;
        Viewers.Close(v)
      ELSE
        H := Y - Y0
      END;
      Viewers.Validate(V, Y0, H);
      Viewers.Change(V, Y0, H)
    ELSIF v = V THEN                 (* mouse in same window *)
      Y0 := V.Y;
      IF Y < V.Y + V.minH THEN
        H := V.minH
      ELSE
        H := Y - Y0
      END;
      Viewers.Validate(V, Y0, H);
      Viewers.Change(V, Y0, H)
    ELSIF V.minH <= v.H THEN         (* mouse window will fit in lower window *)
      IF Y > v.Y + v.H - v.minH THEN Y := v.Y + v.H    END;
      IF Y < v.Y + V.minH       THEN Y := v.Y + V.minH END;
      D := Viewers.ThisDisplay(V);
      Viewers.Close(V);
      Viewers.Open(D, V, X, Y);
      Restore(V)
    END
  END
END Move;


PROCEDURE Change (V: Viewer; X, Y: INTEGER; Keys: SET);
VAR Y0, H: INTEGER;
BEGIN
  w.sl("MenuViewers.Change called - not yet integrated.");
  ASSERT(Keys # {}); (*Input.Mouse(Keys, X, Y);*)
  Y0 := Y;
  WHILE Windows.GetMouseMessage(X, Y, Keys) & (Keys # {}) DO

(* TODO - needs rewrite for top-down measurement *)

    IF (Y # Y0) & (Keys = {2}) THEN (*ML*)
      IF Y > Y0 THEN                     (* moving up with left button down *)
        H  := V.H + Y - Y0;
        Y0 := V.Y
      ELSIF Y0 - Y > V.H - V.minH THEN   (* moving down by more than extent of main textframe *)
        Y0 := V.Y + V.H - V.minH - Y0 + Y;
        H  := V.minH
      ELSE
        H  := V.H - Y0 + Y;
        Y0 := V.Y
      END;
      Viewers.Validate(V, Y0, H); Viewers.Change(V, Y0, H)
    END;
    Y0 := Y;
  END
END Change;


PROCEDURE Expand (V: Viewers.Viewer);
VAR D: Viewers.DisplayArea; Y, H: INTEGER;
BEGIN
  w.sl("MenuViewers.Expand called - not yet integrated.");  ASSERT(FALSE);
  D := Viewers.ThisDisplay(V); Y := 0; H := D.H;
  Viewers.Validate(V, Y, H); Viewers.Change(V, Y, H)
END Expand;


PROCEDURE Copy (V: Viewer; VAR V1: Viewer);
VAR Menu, Main: Display.Frame; M: Oberon.CopyMsg;
BEGIN Menu := V.child; Main := V.child.next;
  NEW(V1); V1^ := V^; V1.state := 0; V1.child := NIL;
  M.F := NIL; Menu.handle(Menu, M); Viewers.Install(V1, M.F);
  M.F := NIL; Main.handle(Main, M); Viewers.Install(V1, M.F)
END Copy;

PROCEDURE Handle* (V: Display.Frame; VAR M: Display.FrameMsg);
VAR X, Y: INTEGER;
    Menu, Main: Display.Frame; V1: Viewer;
BEGIN
  w.sl("MenuViewers.Handle.");
  Menu := V.child;
  Main := V.child.next;
  CASE V OF Viewer:
    CASE M OF
    Oberon.InputMsg:
      w.s("MenuViewers.Handle Oberon.InputMsg, M.id "); w.i(M.id); w.sl(".");
      IF M.id = Oberon.track THEN X := M.X; Y := M.Y;
        IF    Y >  V.Y + V.H                 THEN (* mouse arrow shown by default *)
        ELSIF Y >= V.Y + V(Viewer).menuH     THEN Main.handle(Main, M)
        ELSIF Y >= V.Y + V(Viewer).menuH - 2 THEN Menu.handle(Menu, M)
        ELSIF Y > V.Y THEN
          IF X < V.X + V.W - 30 THEN
            IF 2 IN M.keys THEN Change(V(Viewer), X, Y, M.keys)
                           ELSE Menu.handle(Menu, M) END
          ELSIF 1 IN M.keys  THEN Menu.handle(Menu, M)
          ELSIF 2 IN M.keys  THEN Move(V(Viewer), X, Y, M.keys)
          ELSIF M.keys = {0} THEN Expand(V)
          ELSE Oberon.SetSpritePos(Oberon.Star, X, Y)
          END
        (* ELSE Oberon.SetMousePos(X, Y) mouse arrow shown by default *)
        END
      ELSIF M.id = Oberon.consume THEN Menu.handle(Menu, M); Main.handle(Main, M)
      END |
    Oberon.ControlMsg:
      w.s("MenuViewers.Handle Oberon.ControlMsg, M.id "); w.i(M.id); w.sl(".");
      IF    M.id = Oberon.mark       THEN X := M.X; Y := M.Y;
                                          (* Oberon.SetMousePos(X, Y); mouse arrow shown by default *)
                                          Oberon.SetStarPos(X, Y)
      ELSIF M.id = Oberon.defocus    THEN Menu.handle(Menu, M); Main.handle(Main, M)
      ELSIF M.id = Oberon.neutralize THEN Menu.handle(Menu, M); Main.handle(Main, M)
      END |
    Oberon.CopyMsg:
      w.sl("MenuViewers.Handle Oberon.CopyMsg.");
      Copy(V(Viewer), V1); M.F := V1 |
    Viewers.ViewerMsg:
      w.s("MenuViewers.Handle Viewers.ViewerMsg, M.id "); w.i(M.id); w.sl(".");
      IF    M.id = Viewers.restore THEN Restore(V(Viewer))
      ELSIF M.id = Viewers.suspend THEN Suspend(V(Viewer))
      ELSIF M.id = Viewers.modify  THEN Modify(V(Viewer), M.Y, M.H)
      END |
    Display.FrameMsg: Menu.handle(Menu, M); Main.handle(Main, M)
    END
  END
END Handle;


PROCEDURE New* (Menu, Main: Display.Frame; menuH, X, Y: INTEGER): Viewer;
VAR V: Viewer;
BEGIN
  NEW(V);
  V.handle := Handle;
  V.menuH  := menuH;
  V.minH   := menuH + 2;
  V.child  := NIL;
  Viewers.Install(V, Menu); (*V.child*)
  Viewers.Install(V, Main); (*V.child.next*)
  Viewers.Open(Viewers.CurDisplay, V, X, Y);
  Restore(V);
  RETURN V
END New;

BEGIN w.sl("MenuViewers loaded.")
END MenuViewers.
