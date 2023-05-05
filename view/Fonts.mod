MODULE Fonts;

IMPORT SYSTEM, K := Kernel, w := Writer;

TYPE
  UINT32 = SYSTEM.CARD32;
  UINT16 = SYSTEM.CARD16;
  Glyph* = POINTER TO GlyphDesc;

  GlyphDesc* = RECORD
    map*:       ARRAY 4 OF INTEGER;  (* Indices in AlphaMap                                     *)
    mapWidth*:  INTEGER;  (* Alpha width                                                        *)
    mapHeight*: INTEGER;  (* Alpha heght                                                        *)
    originX*:   INTEGER;  (* Offset of left alignment edge rightwards from left of alpha map    *)
    baseline*:  INTEGER;  (* Offset of baseline downwards from top of alpha map                 *)
    advance*:   INTEGER   (* Offset of right alignment edge rightwards from left alignment edge *)
   END;

VAR
  CreateDCA:        PROCEDURE(drive, device, port, devmode: INTEGER): INTEGER;
  CreateFontA:      PROCEDURE(cHeight,  cWidth,        cEscapement,    cOrientation,
                              cWeight,  bItalic,       bUnderline,     bStrikeOut,
                              iCharSet, iOutPrecision, iClipPrecision, iQuality,
                              iPitchAndFamily,         pszFaceName: INTEGER): INTEGER;
  SelectObject:     PROCEDURE(hdc, hobject: INTEGER): INTEGER;
  DeleteObject:     PROCEDURE(hobject: INTEGER): INTEGER;
  GetGlyphOutlineW: PROCEDURE(hdc, uChar, fuFormat, lpgm, cjBuffer, pvBuffer, lpmat2: INTEGER): INTEGER;

  AlphaMap: ARRAY 32768 OF BYTE;
  MapLen:   INTEGER;
  Glyphs*:  ARRAY 95 OF Glyph;  (* Representing characters 32 (space) to 126 ('~') *)



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


PROCEDURE MakeAlphaPixel(buf: ARRAY OF BYTE; VAR pos: INTEGER; subpixels: INTEGER): INTEGER;
VAR alpha: INTEGER;
BEGIN alpha := 0;
  WHILE subpixels > 0 DO INC(alpha, buf[pos]);  INC(pos);  DEC(subpixels) END
RETURN (alpha + 2) DIV 4 END MakeAlphaPixel;

PROCEDURE MakeAlphaMap(width, mapwidth, height, offset: INTEGER; buf: ARRAY OF BYTE): INTEGER;
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
  stride := (width + 3) DIV 4 * 4;

  (*
  w.s("MakeAlphaMap width "); w.i(width);
  w.s(", height "); w.i(height);
  w.s(", stride "); w.i(stride);
  w.sl(".");
  *)

  pos  := 0;
  last := MakeAlphaPixel(buf, pos, 4 - offset);
  len  := 1;
  y    := 0;
  WHILE y < height DO
    lim := pos + 4 * (mapwidth - 1);
    WHILE pos < lim DO (* Remaining pixels *)
      alpha := (buf[pos] + buf[pos+1] + buf[pos+2] + buf[pos+3]) DIV 4;  INC(pos, 4);
      IF alpha = last THEN INC(len)
      ELSE WriteMap(last, len);  last := alpha;  len := 1 END;
    END;
    INC(y);  pos := y * stride;
    IF y < height THEN
      (* First pixel on row *)
      alpha := MakeAlphaPixel(buf, pos, 4 - offset);
      IF alpha = last THEN INC(len)
      ELSE WriteMap(last, len);  last := alpha;  len := 1 END;
    END
  END;
  WriteMap(last, len);
  AlphaMap[MapLen] := 0H;
  INC(MapLen)
RETURN result END MakeAlphaMap;


PROCEDURE GetAlphaMap(ch, size, offset: INTEGER; VAR glyph: Glyph);
VAR
  dc:      INTEGER;
  font:    INTEGER;
  oldfont: INTEGER;
  res:     INTEGER;
  buffer:  ARRAY 16384 OF BYTE;

  glyphmetrics: RECORD
    blackBoxX:    UINT32;
    blackBoxY:    UINT32;
    glyphOriginX: UINT32;
    glyphOriginY: UINT32;
    cellIncX:     UINT16;
    cellIncY:     UINT16
  END;

  matrix: RECORD
    em11, em12, em21, em22: UINT32  (* in 16.16 whole.fraction format *)
  END;
BEGIN
  font := CreateFontA(size, 0,0,0,0,0,0,0,0,0,0,0,0, SYSTEM.ADR("Arial"));
  dc   := CreateDCA(SYSTEM.ADR("DISPLAY"), 0,0,0);

  ASSERT(font # 0);  ASSERT(dc # 0);

  matrix.em11 := 40000H;
  matrix.em12 := 0;
  matrix.em21 := 0;
  matrix.em22 := 10000H;

  oldfont := SelectObject(dc, font);
  res := GetGlyphOutlineW(
    dc,
    ch,
    6,                               (* GGO_GRAY8_BITMAP *)
    SYSTEM.ADR(glyphmetrics),
    LEN(buffer), SYSTEM.ADR(buffer),
    SYSTEM.ADR(matrix)
  );
  ASSERT(res > 0);

  glyph.mapWidth  := (glyphmetrics.blackBoxX + 3) DIV 4;
  glyph.mapHeight := glyphmetrics.blackBoxY;
  glyph.originX   := glyphmetrics.glyphOriginX;
  glyph.baseline  := glyphmetrics.glyphOriginY;
  glyph.advance   := glyphmetrics.cellIncX;

  (*
  w.c("'");               w.c(CHR(ch));
  w.s("' bitmap size ");  w.i(res); w.sl(".");
  w.s("Black box ");      w.i(glyphmetrics.blackBoxX);     w.c(",");  w.i(glyphmetrics.blackBoxY);
  w.s(", glyph origin "); w.i(glyphmetrics.glyphOriginX);  w.c(",");  w.i(glyphmetrics.glyphOriginY);
  w.s(", advance ");      w.i(glyphmetrics.cellIncX);      w.c(",");  w.i(glyphmetrics.cellIncY);
  w.sl(".");
  w.DumpMem(2, SYSTEM.ADR(buffer), 0, res);
  *)

  glyph.map[offset] := SYSTEM.ADR(AlphaMap[MakeAlphaMap(glyphmetrics.blackBoxX, glyph.mapWidth, glyphmetrics.blackBoxY, offset, buffer)])
END GetAlphaMap;


PROCEDURE GetSpaceGlyph(VAR glyph: Glyph; size, offset: INTEGER);
BEGIN
  glyph.mapWidth    := 0;
  glyph.mapHeight   := 0;
  glyph.originX     := 0;
  glyph.baseline    := 0;
  glyph.advance     := size * 4 DIV 3;
  glyph.map[offset] := 0
END GetSpaceGlyph;


PROCEDURE GetGlyph*(ch, size, offset: INTEGER): Glyph;
VAR glyph: Glyph;
BEGIN
  ASSERT((ch >= 32) & (ch <= 126));
  ASSERT(offset >= 0);  ASSERT(offset < 4);
  glyph := Glyphs[ch-32];
  IF (glyph = NIL) OR (glyph.map[offset] = -1) THEN
    IF glyph = NIL THEN
      NEW(glyph);
      glyph.map[0] := -1;  glyph.map[1] := -1;
      glyph.map[2] := -1;  glyph.map[3] := -1;
      Glyphs[ch-32] := glyph
    END;
    IF ch = 32 THEN
      GetSpaceGlyph(glyph, size, offset)
    ELSE
      GetAlphaMap(ch, size, offset, glyph)
    END
  END
RETURN glyph END GetGlyph;

BEGIN
  K.GetProc(K.Gdi, "CreateDCA",        CreateDCA);         ASSERT(CreateDCA        # NIL);
  K.GetProc(K.Gdi, "CreateFontA",      CreateFontA);       ASSERT(CreateFontA      # NIL);
  K.GetProc(K.Gdi, "SelectObject",     SelectObject);      ASSERT(SelectObject     # NIL);
  K.GetProc(K.Gdi, "DeleteObject",     DeleteObject);      ASSERT(DeleteObject     # NIL);
  K.GetProc(K.Gdi, "GetGlyphOutlineW", GetGlyphOutlineW);  ASSERT(GetGlyphOutlineW # NIL);
  MapLen := 0;
END Fonts.
