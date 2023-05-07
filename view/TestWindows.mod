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
VAR x: INTEGER;
BEGIN
  w.s("dodraw. Address $"); w.h(bitmap.address);
  w.s(", width ");  w.i(width);
  w.s(", height "); w.i(height);
  w.sl(".");

  Windows.FillRectangle(bitmap, 0, 0, width, height, 0FFFFFFH);

  DrawFrame(bitmap, width, height);
  Windows.FillRectangle(bitmap, 50, 50, 100, 100, 0E0FFH);

  x := 200 * 256;
  Rendering.RenderString(x,  70, "garamond",         24, 0, "Hello teapots.", bitmap.address, bitmap.width);
  x := 200 * 256;
  Rendering.RenderString(x, 100, "arial",            18, 0, "the quick brown fox jumps over the lazy dog.", bitmap.address, bitmap.width);
  x := 200 * 256;
  Rendering.RenderString(x, 140, "times new roman italic",       36, 0, "fluffy fluff for jaffa piffle.", bitmap.address, bitmap.width);
  x := 20 * 256;
  Rendering.RenderString(x, 210, "edwardian script itc",   64, 0, "Hello Teapots.", bitmap.address, bitmap.width);

  x := 20 * 256;
  Rendering.RenderString(x, 250, "fira code retina italic",  16, 0C695C6H, "PROCEDURE ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 250, "fira code retina",         16, 0F9AE57H, "RenderAlphaMap ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 250, "fira code retina",         16, 0A6ACB9H, "fira code retina", bitmap.address, bitmap.width);

  x := 20 * 256;
  Rendering.RenderString(x, 275, "times new roman italic",  19, 0C695C6H, "PROCEDURE ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 275, "times new roman",         19, 0F9AE57H, "RenderAlphaMap ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 275, "times new roman",         19, 0A6ACB9H, "times new roman", bitmap.address, bitmap.width);

  x := 20 * 256;
  Rendering.RenderString(x, 300, "sitka subheading italic",  19, 0C695C6H, "PROCEDURE ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 300, "sitka subheading",         19, 0F9AE57H, "RenderAlphaMap ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 300, "sitka subheading",         19, 0A6ACB9H, "sitka subheading", bitmap.address, bitmap.width);

  x := 20 * 256;
  Rendering.RenderString(x, 325, "Avrile Serif Italic",  17, 0C695C6H, "PROCEDURE ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 325, "Avrile Serif",         17, 0F9AE57H, "RenderAlphaMap ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 325, "Avrile Serif",         17, 0A6ACB9H, "Avrile Serif", bitmap.address, bitmap.width);

  x := 20 * 256;
  Rendering.RenderString(x, 350, "Bookerly Italic",  17, 0C695C6H, "PROCEDURE ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 350, "Bookerly",         17, 0F9AE57H, "RenderAlphaMap ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 350, "Bookerly",         17, 0A6ACB9H, "Bookerly", bitmap.address, bitmap.width);

  x := 20 * 256;
  Rendering.RenderString(x, 375, "bookman old style Italic",  17, 0C695C6H, "PROCEDURE ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 375, "bookman old style",         17, 0F9AE57H, "RenderAlphaMap ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 375, "bookman old style",         17, 0A6ACB9H, "bookman old style", bitmap.address, bitmap.width);

  x := 20 * 256;
  Rendering.RenderString(x, 400, "californian fb Italic",  19, 0C695C6H, "PROCEDURE ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 400, "californian fb",         19, 0F9AE57H, "RenderAlphaMap ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 400, "californian fb",         19, 0A6ACB9H, "californian fb", bitmap.address, bitmap.width);

  x := 20 * 256;
  Rendering.RenderString(x, 425, "cambria Italic",  19, 0C695C6H, "PROCEDURE ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 425, "cambria",         19, 0F9AE57H, "RenderAlphaMap ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 425, "cambria",         19, 0A6ACB9H, "cambria", bitmap.address, bitmap.width);

  x := 20 * 256;
  Rendering.RenderString(x, 450, "constantia Italic",  18, 0C695C6H, "PROCEDURE ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 450, "constantia",         18, 0F9AE57H, "RenderAlphaMap ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 450, "constantia",         18, 0A6ACB9H, "constantia", bitmap.address, bitmap.width);

  x := 20 * 256;
  Rendering.RenderString(x, 475, "garamond Italic",  19, 0C695C6H, "PROCEDURE ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 475, "garamond",         19, 0F9AE57H, "RenderAlphaMap ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 475, "garamond",         19, 0A6ACB9H, "garamond", bitmap.address, bitmap.width);

  x := 20 * 256;
  Rendering.RenderString(x, 500, "liberation serif Italic",  19, 0C695C6H, "PROCEDURE ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 500, "liberation serif",         19, 0F9AE57H, "RenderAlphaMap ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 500, "liberation serif",         19, 0A6ACB9H, "liberation serif", bitmap.address, bitmap.width);

  x := 20 * 256;
  Rendering.RenderString(x, 525, "lucida bright Italic",  17, 0C695C6H, "PROCEDURE ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 525, "lucida bright",         17, 0F9AE57H, "RenderAlphaMap ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 525, "lucida bright",         17, 0A6ACB9H, "lucida bright", bitmap.address, bitmap.width);

  x := 20 * 256;
  Rendering.RenderString(x, 550, "palatino linotype Italic",  18, 0C695C6H, "PROCEDURE ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 550, "palatino linotype",         18, 0F9AE57H, "RenderAlphaMap ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 550, "palatino linotype",         18, 0A6ACB9H, "palatino linotype", bitmap.address, bitmap.width);

  x := 20 * 256;
  Rendering.RenderString(x, 575, "sitka small Italic",  17, 0C695C6H, "PROCEDURE ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 575, "sitka small",         17, 0F9AE57H, "RenderAlphaMap ", bitmap.address, bitmap.width);
  Rendering.RenderString(x, 575, "sitka small",         17, 0A6ACB9H, "sitka small", bitmap.address, bitmap.width);

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
