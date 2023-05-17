MODULE Display;  (*NW 5.11.2013 / 17.1.2019 / AP 15.9.20 Extended Oberon / DCWB 8May23 *)
IMPORT SYSTEM, Windows, w := Writer;

CONST
  black* = 0;        (*black = background*)
  white* = 0FFFFFFH;

  replace* = 0; (*modes*)
  paint*   = 1;
  invert*  = 2;

TYPE
  ARGB*     = Windows.ARGB;
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
BEGIN Windows.FillRectangle(Window.bmp, x, y, w, h, colour) END FillRectangle;


(* ------------------------------ Debug/Trace ------------------------------- *)


PROCEDURE wxy*(x, y: INTEGER);
BEGIN w.i(x);  w.c(",");  w.i(y); END wxy;

PROCEDURE wfpos*(f: Frame);
BEGIN wxy(f.X, f.Y);  w.s(" size ");  wxy(f.W, f.H); END wfpos;


(* -------------------------- Rendering primitives -------------------------- *)


PROCEDURE u8sqrt(x: INTEGER): INTEGER;
VAR c, d: INTEGER;
BEGIN
  c := 0;  d := 16384;
  WHILE d # 0 DO
    IF x >= c + d THEN
      DEC(x, c + d);  c := c DIV 2 + d
    ELSE
      c := c DIV 2
    END;
    d := d DIV 4
  END
RETURN c END u8sqrt;


PROCEDURE AlphaMultiplyChannel(p, a: BYTE): INTEGER;
RETURN (p * p * a) DIV 256 END AlphaMultiplyChannel;


PROCEDURE AlphaMultiplyPixel(pixel: INTEGER; alpha: BYTE): INTEGER;
VAR result: INTEGER;
BEGIN
  IF    alpha = 0   THEN result := 0
  ELSIF alpha = 255 THEN result := pixel
  ELSE
    result := (u8sqrt(AlphaMultiplyChannel((pixel DIV 10000H) MOD 100H, alpha)) * 10000H)
            + (u8sqrt(AlphaMultiplyChannel((pixel DIV   100H) MOD 100H, alpha)) *   100H)
            + (u8sqrt(AlphaMultiplyChannel( pixel             MOD 100H, alpha))         )
  END
RETURN result END AlphaMultiplyPixel;


(*   BlendChannel - Blend alpha * foreground with 1-alpha * background *)
(*   entry  fg    - 8 bit gamma encoded foreground intensity           *)
(*          bg    - 8 bit gamma encoded background intensity           *)
(*          alpha - 8 bit linear alpha                                 *)
PROCEDURE BlendChannel(fg, bg, alpha: BYTE): BYTE;
BEGIN
  RETURN u8sqrt(  AlphaMultiplyChannel(fg, alpha)
                + AlphaMultiplyChannel(bg, 255 - alpha))
END BlendChannel;


PROCEDURE BlendPixel*(fg, bg: ARGB; alpha: BYTE): ARGB;
VAR result: ARGB;
BEGIN
  IF    bg    = 0   THEN result := AlphaMultiplyPixel(fg, alpha)
  ELSIF alpha = 255 THEN result := fg
  ELSIF alpha = 0   THEN result := bg
  ELSE
    result := (BlendChannel((fg DIV 10000H) MOD 100H, (bg DIV 10000H) MOD 100H, alpha) * 10000H)
            + (BlendChannel((fg DIV   100H) MOD 100H, (bg DIV   100H) MOD 100H, alpha) *   100H)
            + (BlendChannel( fg             MOD 100H,  bg             MOD 100H, alpha)         );
  END
RETURN result END BlendPixel;



PROCEDURE RenderAlphaMap*(
  x:         INTEGER;  (* In 1/4 pixels   *)
  y:         INTEGER;  (* In whole pixels *)
  width:     INTEGER;  (* In 1/4 pixels   *)
  height:    INTEGER;  (* In whole pixels *)
  mapadr:    INTEGER;
  paint:     ARGB
);
VAR
  alpha, len: BYTE;
  sp,    mp:  INTEGER;
  pixel:      ARGB;
  subpixel:   INTEGER;
  alphasum:   INTEGER;
  bitmapadr:  INTEGER;
  stride:     INTEGER;
BEGIN
  (*
  w.s("RenderAlphaMap, x "); w.i(x);
  w.s(", width ");           w.i(width);
  w.s(", height ");          w.i(height);
  w.s(", mapadr $");         w.h(mapadr);  w.sl(".");

  w.DumpMem(2, mapadr, 0, 323);
  *)

  bitmapadr := Window.bmp.address;
  stride    := Window.bmp.width;

  mp       := bitmapadr + 4 * (stride * y + x DIV 4);
  subpixel := x MOD 4;
  alphasum := 0;
  sp       := 0;

  SYSTEM.GET(mapadr, len);  INC(mapadr);
  WHILE len # 0 DO
    CASE len DIV 64 OF
    | 0: alpha := len;        len := 1
    | 1: len := len MOD 40H;  alpha := 0;
    | 2: len := len MOD 40H;  alpha := 40H;
    | 3: len := len MOD 40H;  SYSTEM.GET(mapadr, alpha);  INC(mapadr);
    END;

    WHILE len > 0 DO
      INC(alphasum, alpha); INC(subpixel);
      IF subpixel > 3 THEN
        IF alphasum > 0 THEN
          IF alphasum >= 255 THEN
            SYSTEM.PUT(mp, paint);
          ELSE
            SYSTEM.GET(mp, pixel);
            SYSTEM.PUT(mp, BlendPixel(paint, pixel, alphasum));
          END
        END;
        subpixel := 0;
        alphasum := 0;
        INC(mp, 4);
      END;
      INC(sp);
      IF sp >= width THEN
        IF alphasum > 0 THEN  (* write remaining partial pixel *)
          SYSTEM.GET(mp, pixel);
          SYSTEM.PUT(mp, BlendPixel(paint, pixel, alphasum));
        END;
        INC(y);
        mp := bitmapadr + 4 * (stride * y + x DIV 4);
        sp := 0;
        alphasum := 0;
        subpixel := x MOD 4;
      END;
      DEC(len)
    END;
    SYSTEM.GET(mapadr, len);  INC(mapadr);
  END
END RenderAlphaMap;


(* ----------------------- Windows specific handlers ------------------------ *)

BEGIN
  w.sl("Display initialising.");
  Width  := 1920;
  Height := 1280;
  Window := Windows.NewWindow(50, 50, Width, Height);
  w.sl("Display initialised.");
END Display.
