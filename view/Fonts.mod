MODULE Fonts;

IMPORT SYSTEM, K := Kernel, w := Writer;

(* TODO: all advance widths should be determined from the font at EM size.    *)
(* Currently the code uses the whole pixel advance from GetCharABCWidths or   *)
(* or the subpixel advance from GetGlyphOutline.                              *)

TYPE
  INT16 = SYSTEM.INT16;  UINT16 = SYSTEM.CARD16;
  INT32 = SYSTEM.INT32;  UINT32 = SYSTEM.CARD32;

  Glyph* = POINTER TO GlyphDesc;

  GlyphDesc* = RECORD
    map*:       INTEGER;  (* Indices in AlphaMap                                                *)
    mapWidth*:  INTEGER;  (* Alpha width                                                        *)
    mapHeight*: INTEGER;  (* Alpha heght                                                        *)
    originX*:   INTEGER;  (* Offset of left alignment edge rightwards from left of alpha map    *)
    baseline*:  INTEGER   (* Offset of baseline downwards from top of alpha map                 *)
   END;

   Face  = POINTER TO FaceDesc;
   Font* = POINTER TO FontDesc;

   FontDesc = RECORD
     hfont:  INTEGER;
     face:   Face;
     em:     INTEGER;
     glyphs: ARRAY 95 OF Glyph;
     next:   Font
   END;

   FaceDesc = RECORD
     name:   ARRAY 256 OF CHAR;
     sizes:  Font;
     emsize: INTEGER;
     widths: ARRAY 95 OF INTEGER; (* at emsize in 56.8 *)
     next:   Face
   END;

VAR
  CreateDCA:              PROCEDURE(drive, device, port, devmode: INTEGER): INTEGER;
  CreateFontA:            PROCEDURE(cHeight,  cWidth,        cEscapement,    cOrientation,
                                    cWeight,  bItalic,       bUnderline,     bStrikeOut,
                                    iCharSet, iOutPrecision, iClipPrecision, iQuality,
                                    iPitchAndFamily,         pszFaceName: INTEGER): INTEGER;
  SelectObject:           PROCEDURE(hdc, hobject: INTEGER): INTEGER;
  DeleteObject:           PROCEDURE(hobject: INTEGER): INTEGER;
  GetGlyphOutlineW:       PROCEDURE(hdc, uChar, fuFormat, lpgm, cjBuffer, pvBuffer, lpmat2: INTEGER): INTEGER;
  GetCharABCWidthsW:      PROCEDURE(hdc, first, last, abcadr: INTEGER): INTEGER;
  GetDeviceCaps:          PROCEDURE(hdc, index: INTEGER): INTEGER;
  GetOutlineTextMetricsW: PROCEDURE(hdc, bufsize, buffer: INTEGER): INTEGER;

  DisplayDC: INTEGER;

  AlphaMap:  ARRAY 100000 OF BYTE;
  MapLen:    INTEGER;

  Faces:     Face;


PROCEDURE GetFace(name: ARRAY OF CHAR): Face;
TYPE
  TextMetrics = RECORD
    height:              INT32;
    ascent:              INT32;
    descent:             INT32;
    internalLeading:     INT32;
    externalLeading:     INT32;
    aveCharWidth:        INT32;
    maxCharWidth:        INT32;
    weight:              INT32;
    overhang:            INT32;
    digitizedAspectX:    INT32;
    digitizedAspectY:    INT32;
    firstChar:           UINT16;
    lastChar:            UINT16;
    defaultChar:         UINT16;
    breakChar:           UINT16;
    italic:              BYTE;
    underlined:          BYTE;
    struckOut:           BYTE;
    pitchAndFamily:      BYTE;
    charSet:             BYTE
  END;
  OutlineTextMetrics = RECORD
    size:                  UINT32;
    textmetrics:           TextMetrics;
    filler:                BYTE;
    panoseFamilyType:      BYTE;
    panoseSerifStyle:      BYTE;
    panoseWeight:          BYTE;
    panoseProportion:      BYTE;
    panoseContrast:        BYTE;
    panoseStrokeVariation: BYTE;
    panoseArmStyle:        BYTE;
    panoseLetterform:      BYTE;
    panoseMidline:         BYTE;
    panoseXHeight:         BYTE;
    selection:             UINT32;
    type:                  UINT32;
    charSlopeRise:         INT32;
    charSlopeRun:          INT32;
    italicAngle:           INT32;
    emSquare:              UINT32;
    ascent:                INT32;
    descent:               INT32;
    lineGap:               UINT32;
    capEmHeight:           UINT32;
    xHeight:               UINT32;
    fontBoxLeft:           INT32;
    fontBoxTop:            INT32;
    fontBoxRight:          INT32;
    fontBoxBottom:         INT32;
    macAscent:             INT32;
    macDescent:            INT32;
    macLineGap:            UINT32;
    minimumPPEM:           UINT32;
    subscriptSizeX:        INT32;
    subscriptSizeY:        INT32;
    subscriptOffsetX:      INT32;
    subscriptOffsetY:      INT32;
    superscriptSizeX:      INT32;
    superscriptSizeY:      INT32;
    superscriptOffsetX:    INT32;
    superscriptOffsetY:    INT32;
    strikeoutSize:         UINT32;
    strikeoutPosition:     INT32;
    underscoreSize:        INT32;
    underscorePosition:    INT32;
    pFamilyName:           INTEGER;
    pFaceName:             INTEGER;
    pStyleName:            INTEGER;
    pFullName:             INTEGER;
    buffer:                ARRAY 4096 OF UINT16  (* Contains face names *)
  END;
  ABC = RECORD a: INT32; b: UINT32; c: INT32 END;

VAR
  face:     Face;
  hFont:    INTEGER;
  hOldFont: INTEGER;
  tm:       TextMetrics;
  otm:      OutlineTextMetrics;
  bufsize:  INTEGER;
  i:        INTEGER;
  abcs:     ARRAY 95 OF ABC;
BEGIN
  face := Faces;
  WHILE (face # NIL) & (face.name # name) DO face := face.next END;
  IF face = NIL THEN
    w.s("Create new Fonts.Face '"); w.s(name); w.sl("'.");
    NEW(face);
    face.name := name;
    hFont     := CreateFontA(-2048, 0,0,0,0,0,0,0,0,7,0,0,0, SYSTEM.ADR(name));  (* 7: OUT_TT_ONLY_PRECIS *)
    ASSERT(hFont # 0);
    hOldFont  := SelectObject(DisplayDC, hFont);
    bufsize   := GetOutlineTextMetricsW(DisplayDC, 0, 0);
    w.s("GetOutlineTextMetricsW buffer size required: "); w.i(bufsize); w.sl(".");
    ASSERT(GetOutlineTextMetricsW(DisplayDC, SYSTEM.SIZE(OutlineTextMetrics), SYSTEM.ADR(otm)) # 0);
    face.emsize := otm.emSquare;
    w.s("Create face '");        w.s(name);
    w.s(", emSquare ");          w.i(otm.emSquare);
    w.s(", tmHeight ");          w.i(otm.textmetrics.height);
    w.s(", tmInternalLeading "); w.i(otm.textmetrics.internalLeading);
    w.s(", height - leading ");  w.i(otm.textmetrics.height - otm.textmetrics.internalLeading);
    w.sl(".");
    IF otm.emSquare # otm.textmetrics.height - otm.textmetrics.internalLeading THEN
      (* Reselect the font at its desing em size. *)
    END;
    (* fill in character width at design em square size *)
    ASSERT(GetCharABCWidthsW(DisplayDC, 32, 126, SYSTEM.ADR(abcs)) # 0);
    FOR i := 0 TO 94 DO
      face.widths[i] := (abcs[i].a + abcs[i].b + abcs[i].c) * otm.emSquare
                    DIV (otm.textmetrics.height - otm.textmetrics.internalLeading)
    END;
    face.next := Faces;  Faces := face;
    ASSERT(DeleteObject(SelectObject(DisplayDC, hOldFont)) # 0);
  END
RETURN face END GetFace;


PROCEDURE GetSize(face: Face; em: INTEGER): Font;
VAR size: Font;
BEGIN
  size := face.sizes;
  WHILE (size # NIL) & (size.em # em) DO size := size.next END;
  IF size = NIL THEN
    NEW(size);
    size.face  := face;
    size.em    := em;
    size.hfont := CreateFontA(-em, 0,0,0,0,0,0,0,0,7,0,0,0, SYSTEM.ADR(face.name));
    size.next  := face.sizes;  face.sizes := size;
  END
RETURN size END GetSize;

PROCEDURE GetFont*(name: ARRAY OF CHAR; em: INTEGER): Font;
RETURN GetSize(GetFace(name), em) END GetFont;

PROCEDURE WriteMap(alpha: BYTE; len: INTEGER);
BEGIN
  WHILE len > 3FH DO WriteMap(alpha, 3FH);  DEC(len, 3FH)  END;
  IF    alpha =  0   THEN AlphaMap[MapLen] :=  40H + len;  INC(MapLen)
  ELSIF alpha >= 40H THEN AlphaMap[MapLen] :=  80H + len;  INC(MapLen)
  ELSIF len   >= 3   THEN AlphaMap[MapLen] := 0C0H + len;  INC(MapLen);
                          AlphaMap[MapLen] := alpha;       INC(MapLen)
  ELSE
    ASSERT(alpha > 0);  ASSERT(alpha <= 3FH);
    WHILE len > 0 DO AlphaMap[MapLen] := alpha;  INC(MapLen);  DEC(len) END
  END
END WriteMap;


PROCEDURE MakeAlphaMap(width, height: INTEGER; buf: ARRAY OF BYTE): INTEGER;
VAR
  stride, len:   INTEGER;
  last,   alpha: INTEGER;
  y:             INTEGER;
  result:        INTEGER;
  pos,    lim:   INTEGER;
BEGIN
  result := MapLen;
  ASSERT(width < 10000H);
  ASSERT(height < 10000H);

  (*
  w.s("MakeAlphaMap width "); w.i(width);
  w.s(", height "); w.i(height);
  w.s(", stride "); w.i(stride);
  w.sl(".");
  *)

  stride := (width + 3) DIV 4 * 4;
  last   := buf[0];
  pos    := 1;
  lim    := width;
  len    := 1;
  y      := 0;
  WHILE y < height DO
    WHILE pos < lim DO
      alpha := buf[pos];  INC(pos);
      IF alpha = last THEN INC(len)
      ELSE WriteMap(last, len);  last := alpha;  len := 1 END;
    END;
    INC(y);  pos := y * stride;  lim := pos + width;
  END;
  WriteMap(last, len);
  AlphaMap[MapLen] := 0H;
  INC(MapLen);

  (*
  w.s("MakeAlphaMap. Original size "); w.i(width * stride);
  w.s(", compressed size ");  w.i(MapLen - result);
  w.s(", reduction to "); w.i((100 * (MapLen - result)) DIV (width * stride));
  w.sl("%.");
  w.DumpMem(2, SYSTEM.ADR(AlphaMap[result]), 0, MapLen - result);
  *)

RETURN result END MakeAlphaMap;


PROCEDURE GetAlphaMap(font: Font; ch: INTEGER; VAR glyph: Glyph);
VAR
  oldfont: INTEGER;
  res:     INTEGER;
  buffer:  ARRAY 16384 OF BYTE;

  glyphmetrics: RECORD
    blackBoxX:    UINT32;
    blackBoxY:    UINT32;
    glyphOriginX: INT32;
    glyphOriginY: INT32;
    cellIncX:     INT16;
    cellIncY:     INT16
  END;

  matrix: RECORD
    em11, em12, em21, em22: UINT32  (* in 16.16 whole.fraction format *)
  END;
BEGIN
  ASSERT(font.hfont # 0);  ASSERT(DisplayDC # 0);

  matrix.em11 := 40000H;
  matrix.em12 := 0;
  matrix.em21 := 0;
  matrix.em22 := 10000H;

  oldfont := SelectObject(DisplayDC, font.hfont);
  res := GetGlyphOutlineW(
    DisplayDC,
    ch,
    6,                               (* GGO_GRAY8_BITMAP *)
    SYSTEM.ADR(glyphmetrics),
    LEN(buffer), SYSTEM.ADR(buffer),
    SYSTEM.ADR(matrix)
  );
  ASSERT(res > 0);

  glyph.mapWidth  := glyphmetrics.blackBoxX;
  glyph.mapHeight := glyphmetrics.blackBoxY;
  glyph.originX   := glyphmetrics.glyphOriginX;
  glyph.baseline  := glyphmetrics.glyphOriginY;

  (*
  w.c("'");               w.c(CHR(ch));
  w.s("' bitmap size ");  w.i(res); w.sl(".");
  w.s("Black box ");      w.i(glyphmetrics.blackBoxX);     w.c(",");  w.i(glyphmetrics.blackBoxY);
  w.s(", glyph origin "); w.i(glyphmetrics.glyphOriginX);  w.c(",");  w.i(glyphmetrics.glyphOriginY);
  w.s(", advance ");      w.i(glyphmetrics.cellIncX);      w.c(",");  w.i(glyphmetrics.cellIncY);
  w.sl(".");
  w.DumpMem(2, SYSTEM.ADR(buffer), 0, res);
  *)

  glyph.map := SYSTEM.ADR(AlphaMap[MakeAlphaMap(glyphmetrics.blackBoxX, glyphmetrics.blackBoxY, buffer)])
END GetAlphaMap;


PROCEDURE GetGlyph*(font: Font; ch: INTEGER): Glyph;
VAR glyph: Glyph;
BEGIN
  ASSERT((ch >= 32) & (ch <= 126));
  glyph := font.glyphs[ch-32];
  IF (glyph = NIL) OR (glyph.map = -1) THEN
    IF glyph = NIL THEN
      NEW(glyph);
      glyph.map := -1;
      font.glyphs[ch-32] := glyph
    END;
    IF ch = 32 THEN
      glyph.mapWidth  := 0;
      glyph.mapHeight := 0;
      glyph.originX   := 0;
      glyph.baseline  := 0;
      glyph.map       := 0
    ELSE
      GetAlphaMap(font, ch, glyph)
    END
  END
RETURN glyph END GetGlyph;

PROCEDURE GetAdvance*(font: Font; ch: INTEGER): INTEGER;
RETURN font.face.widths[ch-32] * font.em * 256 DIV font.face.emsize END GetAdvance;


BEGIN
  K.GetProc(K.Gdi, "CreateDCA",               CreateDCA);               ASSERT(CreateDCA              # NIL);
  K.GetProc(K.Gdi, "CreateFontA",             CreateFontA);             ASSERT(CreateFontA            # NIL);
  K.GetProc(K.Gdi, "SelectObject",            SelectObject);            ASSERT(SelectObject           # NIL);
  K.GetProc(K.Gdi, "DeleteObject",            DeleteObject);            ASSERT(DeleteObject           # NIL);
  K.GetProc(K.Gdi, "GetGlyphOutlineW",        GetGlyphOutlineW);        ASSERT(GetGlyphOutlineW       # NIL);
  K.GetProc(K.Gdi, "GetCharABCWidthsW",       GetCharABCWidthsW);       ASSERT(GetCharABCWidthsW      # NIL);
  K.GetProc(K.Gdi, "GetOutlineTextMetricsW",  GetOutlineTextMetricsW);  ASSERT(GetOutlineTextMetricsW # NIL);
  MapLen := 0;
  DisplayDC := CreateDCA(SYSTEM.ADR("DISPLAY"),0,0,0);  ASSERT(DisplayDC # 0);
END Fonts.
