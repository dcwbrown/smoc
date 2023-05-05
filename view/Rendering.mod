MODULE Rendering;

IMPORT SYSTEM, Fonts, w := Writer;

TYPE UINT32 = SYSTEM.CARD32;

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


PROCEDURE BlendPixel(fg, bg: UINT32; alpha: BYTE): UINT32;
VAR result: UINT32;
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



PROCEDURE RenderAlphaMap(x, y, width, height, mapadr, paint: INTEGER; bitmapadr, stride: INTEGER);
VAR
  alpha, len:    BYTE;
  sp,    lp:     INTEGER;
  pixel:         SYSTEM.CARD32;
BEGIN
  (*
  w.s("RenderAlphaMap width "); w.i(width);
  w.s(", height ");             w.i(height);
  w.s(", mapadr $");            w.h(mapadr);  w.sl(".");
  w.DumpMem(2, mapadr, 0, 323);
  *)

  sp := bitmapadr + 4 * (stride * y + x);
  lp := sp + 4 * width;

  SYSTEM.GET(mapadr, len);  INC(mapadr);
  WHILE len # 0 DO
    CASE len DIV 64 OF
    | 0: alpha := len * 4;    len := 1
    | 1: len := len MOD 40H;  alpha := 0;
    | 2: len := len MOD 40H;  alpha := 0FFH;
    | 3: len := len MOD 40H;  SYSTEM.GET(mapadr, alpha);  INC(mapadr);
                              IF alpha < 40H THEN alpha := alpha * 4 ELSE alpha := 0FFH END;
    END;
    WHILE len > 0 DO
      SYSTEM.GET(sp, pixel);
      SYSTEM.PUT(sp, BlendPixel(paint, pixel, alpha));
      INC(sp, 4);
      IF sp >= lp THEN
        INC(y);
        sp := bitmapadr + 4 * (stride * y + x);
        lp := sp + 4 * width;
      END;
      DEC(len)
    END;

    SYSTEM.GET(mapadr, len);  INC(mapadr);
  END
END RenderAlphaMap;


PROCEDURE TestRenderLowerCaseA*(x, y: INTEGER; str: ARRAY OF CHAR; address, stride: INTEGER);
VAR glyph: Fonts.Glyph;  i: INTEGER;
BEGIN
  x := x * 4;  (* work horizontally in 1/4 pixels *)
  i := 0;
  WHILE (i < LEN(str)) & (str[i] # 0X)  DO
    glyph := Fonts.GetGlyph(ORD(str[i]), 24, x MOD 4);
    IF glyph.map[x MOD 4] # 0 THEN
      RenderAlphaMap(
        x DIV 4,
        y - glyph.baseline,
        glyph.mapWidth, glyph.mapHeight,
        glyph.map[x MOD 4],
        0FFFFFFH,  (* Paint *)
        address, stride
      )
    END;
    INC(x, glyph.advance);
    INC(i)
  END
END TestRenderLowerCaseA;


BEGIN
END Rendering.

