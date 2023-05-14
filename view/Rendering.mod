MODULE Rendering;

IMPORT SYSTEM, Fonts, w := Writer;

TYPE ARGB = SYSTEM.CARD32;

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



PROCEDURE RenderAlphaMap(
  x:         INTEGER;  (* In 1/4 pixels   *)
  y:         INTEGER;  (* In whole pixels *)
  width:     INTEGER;  (* In 1/4 pixels   *)
  height:    INTEGER;  (* In whole pixels *)
  mapadr:    INTEGER;
  paint:     ARGB;
  bitmapadr: INTEGER;
  stride:    INTEGER
);
VAR
  alpha, len: BYTE;
  sp,    mp:  INTEGER;
  pixel:      ARGB;
  subpixel:   INTEGER;
  alphasum:   INTEGER;
BEGIN
  (*
  w.s("RenderAlphaMap, x "); w.i(x);
  w.s(", width ");           w.i(width);
  w.s(", height ");          w.i(height);
  w.s(", mapadr $");         w.h(mapadr);  w.sl(".");

  w.DumpMem(2, mapadr, 0, 323);
  *)

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


PROCEDURE RenderString*(
  VAR x:       INTEGER;        (* In 1/256ths of a pixel *)
      y:       INTEGER;        (* In whole pixels        *)
      name:    ARRAY OF CHAR;
      size:    INTEGER;
      colour:  INTEGER;
      str:     ARRAY OF CHAR;
      address: INTEGER;
      stride:  INTEGER);
VAR font: Fonts.Font;  glyph: Fonts.Glyph;  i: INTEGER;
BEGIN
  (*font := Fonts.GetFont("Fira Code Retina", size);*)
  (*font := Fonts.GetFont("blackadder itc", size);*)
  (*font := Fonts.GetFont("sitka text", size);*)
  (*font := Fonts.GetFont("arial", size);*)
  font := Fonts.GetFont(name, size);
  i := 0;
  WHILE (i < LEN(str)) & (str[i] # 0X)  DO
    glyph := Fonts.GetGlyph(font, ORD(str[i]));
    IF glyph.map # 0 THEN
      RenderAlphaMap(
        x DIV 64 + glyph.originX,
        y - glyph.baseline,
        glyph.mapWidth, glyph.mapHeight,
        glyph.map,
        colour,
        address, stride
      )
    END;
    INC(x, Fonts.GetAdvance(font, ORD(str[i])));
    INC(i)
  END
END RenderString;


BEGIN
END Rendering.

