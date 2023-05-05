MODULE TestWindows;
IMPORT SYSTEM, Windows, Rendering, w := Writer;

CONST
  (* States *)
  sNormal = 0;
  sMoving = 1;
  sSizing = 2;

  GrabWidth = 10;

VAR
  window:         Windows.Window;
  LeftButtonDown: BOOLEAN;
  State:          INTEGER;
  MoveStartX:     INTEGER;  (* Move start position relative to window origin *)
  MoveStartY:     INTEGER;  (* Move start position relative to window origin *)
  SizeStartX:     INTEGER;  (* Move start position relative to window right edge *)
  SizeStartY:     INTEGER;  (* Move start position relative to window bottom edge *)

PROCEDURE DoCharacter(ch: INTEGER);
BEGIN
  w.s("UTF-32: $"); w.h(ch); w.sl(".");
  IF ch = 1BH THEN Windows.Quit END
END DoCharacter;

PROCEDURE DrawFrame(bitmap: Windows.Bitmap; width, height: INTEGER);
BEGIN
  Windows.FillRectangle(bitmap, 0,       0,        width,   1,         0FF808080H);
  Windows.FillRectangle(bitmap, 1,       1,        width-2, 1,         0FFC0C0C0H);
  Windows.FillRectangle(bitmap, 2,       2,        width-4, 1,         0FF808080H);

  Windows.FillRectangle(bitmap, 0,       0,        1,       height,    0FF808080H);
  Windows.FillRectangle(bitmap, 1,       1,        1,       height-2,  0FFC0C0C0H);
  Windows.FillRectangle(bitmap, 2,       2,        1,       height-4,  0FF808080H);

  Windows.FillRectangle(bitmap, width-1, 0,        1,       height,    0FF808080H);
  Windows.FillRectangle(bitmap, width-2, 1,        1,       height-2,  0FFC0C0C0H);
  Windows.FillRectangle(bitmap, width-3, 2,        1,       height-4,  0FF808080H);

  Windows.FillRectangle(bitmap, 0,       height-1, width,   1,         0FF808080H);
  Windows.FillRectangle(bitmap, 1,       height-2, width-2, 1,         0FFC0C0C0H);
  Windows.FillRectangle(bitmap, 2,       height-3, width-4, 1,         0FF808080H);
END DrawFrame;

PROCEDURE DoDraw(width, height: INTEGER; bitmap: Windows.Bitmap);
BEGIN
  w.s("dodraw. Address $"); w.h(bitmap.address);
  w.s(", width ");  w.i(width);
  w.s(", height "); w.i(height);
  w.sl(".");
  DrawFrame(bitmap, width, height);
  Windows.FillRectangle(bitmap, 50, 50, 100, 100, 0E0FFH);
  Rendering.TestRenderLowerCaseA(200, 100, "Hello teapots.", bitmap.address, bitmap.width)
END DoDraw;

PROCEDURE DoMouse(x, y: INTEGER; flags: SET);
VAR
  leftbuttondown: BOOLEAN;
BEGIN
  leftbuttondown := 0 IN flags;
  IF leftbuttondown # LeftButtonDown THEN
    IF leftbuttondown THEN
      w.s("Going down at "); w.i(x); w.c(","); w.i(y); w.sl(".");
      IF y < GrabWidth THEN
        State := sMoving;  MoveStartX := x;  MoveStartY := y
      ELSIF (x >= window.width - GrabWidth) & (y >= window.height - GrabWidth) THEN
        State := sSizing;   SizeStartX := x - window.width;  SizeStartY := y - window.height;
      END
    ELSE
      w.s("Returning up at "); w.i(x); w.c(","); w.i(y); w.sl(".");
      State := sNormal;
    END
  END;
  LeftButtonDown := leftbuttondown;
  IF State = sMoving THEN
    Windows.Move(window, window.x + x - MoveStartX, window.y + y - MoveStartY)
  ELSIF State = sSizing THEN
    Windows.Resize(window, x - SizeStartX, y - SizeStartY)
  ELSE
    IF leftbuttondown THEN
      w.s("Mouse at "); w.i(x); w.c(","); w.i(y); w.sl(".")
    END
  END
END DoMouse;

BEGIN
  LeftButtonDown := FALSE;  State := sNormal;
  window := Windows.NewWindow(50, 50, 800, 600, DoCharacter, DoDraw, DoMouse);
  Windows.MessagePump
END TestWindows.
