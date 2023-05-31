MODULE Display;  (*NW 5.11.2013 / 17.1.2019 / AP 15.9.20 Extended Oberon / DCWB 8May23 *)
IMPORT SYSTEM, Windows, Fonts, w := Writer;

CONST
  Black* = 0FF000000H;        (*black = background*)
  White* = 0FFFFFFFFH;

  replace* = 0; (*modes*)
  paint*   = 1;
  invert*  = 2;

TYPE
  ARGB*     = SYSTEM.CARD32;
  Frame*    = POINTER TO FrameDesc;
  FrameMsg* = RECORD END ;
  Handler*  = PROCEDURE (F: Frame; VAR M: FrameMsg);
  FrameDesc* = RECORD
    X*, Y*, W*, H*: INTEGER;
    handle*:        Handler;
    next*, child*:  Frame
  END;

VAR
  Width*:  INTEGER;
  Height*: INTEGER;
  arrow*:  INTEGER;
  star*:   INTEGER;
  hook*:   INTEGER;
  updown*: INTEGER;
  block*:  INTEGER;
  cross*:  INTEGER;
  grey*:   INTEGER;
  (* a pattern is an array of bytes; the first is its width (< 32), *)
  (* the second its height, the rest the raster.                    *)

  Window*: Windows.Window;

PROCEDURE Handle*(F: Frame; VAR M: FrameMsg);
BEGIN
  IF (F # NIL) & (F.handle # NIL) THEN F.handle(F, M) END
END Handle;

(* raster ops *)

PROCEDURE DrawPixel*(x, y: INTEGER; colour: ARGB);
BEGIN Windows.DrawPixel(Window.bmp, x, y, colour) END DrawPixel;

PROCEDURE FillRectangle*(x, y, w, h: INTEGER; colour: ARGB);
BEGIN
  Windows.FillRectangle(Window.bmp, x, y, w, h, colour);
  Windows.InvalidateRect(Window, x, y, w, h)
END FillRectangle;

PROCEDURE RenderAlphaMap*(
  x:         INTEGER;  (* In 1/4 pixels   *)
  y:         INTEGER;  (* In whole pixels *)
  width:     INTEGER;  (* In 1/4 pixels   *)
  height:    INTEGER;  (* In whole pixels *)
  mapadr:    INTEGER;
  paint:     ARGB
);
BEGIN
  Windows.RenderAlphaMapToBitmap(
    x, y, width, height,
    mapadr, paint,
    Window.bmp.address, Window.bmp.width,
    Window.bmp.address + 4 * Window.bmp.width * Window.bmp.height
  )
END RenderAlphaMap;




(* ------------------------------ Debug/Trace ------------------------------- *)


PROCEDURE wxy*(x, y: INTEGER);
BEGIN w.i(x);  w.c(",");  w.i(y); END wxy;

PROCEDURE wfpos*(f: Frame);
BEGIN wxy(f.X, f.Y);  w.s(" size ");  wxy(f.W, f.H); END wfpos;


(* ------------------------- Display initialisation ------------------------- *)

BEGIN
  w.sl("Display initialising.");
  Width  := 1800;
  Height := 1280;
  Window := Windows.NewWindow(700, 50, Width, Height);
  w.sl("Display initialised.");
END Display.
