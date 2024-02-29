MODULE Windows;

IMPORT SYSTEM, w := Writer, K := Kernel, Boot, WindowsMessageNames, Fonts;

CONST
  Lin12Table = $
    00 00  01 00  02 00  04 00  05 00  06 00  07 00  09 00  0A 00  0B 00  0C 00  0E 00  0F 00  10 00  12 00  14 00
    15 00  17 00  19 00  1B 00  1D 00  1F 00  21 00  23 00  25 00  28 00  2A 00  2D 00  30 00  32 00  35 00  38 00
    3B 00  3E 00  42 00  45 00  48 00  4C 00  4F 00  53 00  57 00  5B 00  5F 00  63 00  67 00  6B 00  70 00  74 00
    79 00  7E 00  83 00  88 00  8D 00  92 00  97 00  9C 00  A2 00  A8 00  AD 00  B3 00  B9 00  BF 00  C5 00  CC 00
    D2 00  D8 00  DF 00  E6 00  ED 00  F4 00  FB 00  02 01  09 01  11 01  18 01  20 01  28 01  30 01  38 01  40 01
    49 01  51 01  5A 01  62 01  6B 01  74 01  7D 01  86 01  90 01  99 01  A3 01  AC 01  B6 01  C0 01  CA 01  D5 01
    DF 01  EA 01  F4 01  FF 01  0A 02  15 02  20 02  2B 02  37 02  42 02  4E 02  5A 02  66 02  72 02  7F 02  8B 02
    98 02  A4 02  B1 02  BE 02  CB 02  D8 02  E6 02  F3 02  01 03  0F 03  1D 03  2B 03  39 03  48 03  56 03  65 03
    74 03  83 03  92 03  A1 03  B1 03  C0 03  D0 03  E0 03  F0 03  00 04  11 04  21 04  32 04  43 04  54 04  65 04
    76 04  87 04  99 04  AB 04  BD 04  CF 04  E1 04  F3 04  06 05  18 05  2B 05  3E 05  51 05  65 05  78 05  8C 05
    A0 05  B3 05  C8 05  DC 05  F0 05  05 06  1A 06  2E 06  43 06  59 06  6E 06  84 06  99 06  AF 06  C5 06  DB 06
    F2 06  08 07  1F 07  36 07  4D 07  64 07  7C 07  93 07  AB 07  C3 07  DB 07  F3 07  0B 08  24 08  3D 08  55 08
    6F 08  88 08  A1 08  BB 08  D4 08  EE 08  08 09  23 09  3D 09  58 09  73 09  8E 09  A9 09  C4 09  DF 09  FB 09
    17 0A  33 0A  4F 0A  6C 0A  88 0A  A5 0A  C2 0A  DF 0A  FC 0A  19 0B  37 0B  55 0B  73 0B  91 0B  AF 0B  CE 0B
    EC 0B  0B 0C  2A 0C  4A 0C  69 0C  89 0C  A8 0C  C8 0C  E8 0C  09 0D  29 0D  4A 0D  6B 0D  8C 0D  AD 0D  CF 0D
    F0 0D  12 0E  34 0E  56 0E  79 0E  9B 0E  BE 0E  E1 0E  04 0F  27 0F  4B 0F  6E 0F  92 0F  B6 0F  DB 0F  FF 0F
  $;

  SRGBtable = $
    21 00  A7 00  D4 00  F5 00  0F 00  27 00  3B 00  4E 00  60 00  70 00  7F 00  8D 00  9A 00  A7 00  B4 00  BF 00
    CB 00  D6 00  E0 00  EA 00  F4 00  FE 00  07 01  10 01  19 01  22 01  2A 01  33 01  3B 01  43 01  4B 01  52 01
    5A 01  61 01  68 01  6F 01  76 01  7D 01  84 01  8B 01  91 01  98 01  9E 01  A4 01  AB 01  B1 01  B7 01  BD 01
    C3 01  C8 01  CE 01  D4 01  DA 01  DF 01  E5 01  EA 01  EF 01  F5 01  FA 01  FF 01  04 02  0A 02  0F 02  14 02
    19 02  1E 02  22 02  27 02  2C 02  31 02  36 02  3A 02  3F 02  43 02  48 02  4D 02  51 02  56 02  5A 02  5E 02
    63 02  67 02  6B 02  70 02  74 02  78 02  7C 02  80 02  85 02  89 02  8D 02  91 02  95 02  99 02  9D 02  A1 02
    A5 02  A9 02  AC 02  B0 02  B4 02  B8 02  BC 02  BF 02  C3 02  C7 02  CB 02  CE 02  D2 02  D6 02  D9 02  DD 02
    E0 02  E4 02  E8 02  EB 02  EF 02  F2 02  F6 02  F9 02  FC 02  00 03  03 03  07 03  0A 03  0D 03  11 03  14 03
    17 03  1B 03  1E 03  21 03  25 03  28 03  2B 03  2E 03  31 03  35 03  38 03  3B 03  3E 03  41 03  44 03  47 03
    4B 03  4E 03  51 03  54 03  57 03  5A 03  5D 03  60 03  63 03  66 03  69 03  6C 03  6F 03  72 03  75 03  77 03
    7A 03  7D 03  80 03  83 03  86 03  89 03  8C 03  8E 03  91 03  94 03  97 03  9A 03  9C 03  9F 03  A2 03  A5 03
    A8 03  AA 03  AD 03  B0 03  B2 03  B5 03  B8 03  BB 03  BD 03  C0 03  C3 03  C5 03  C8 03  CB 03  CD 03  D0 03
    D2 03  D5 03  D8 03  DA 03  DD 03  DF 03  E2 03  E4 03  E7 03  EA 03  EC 03  EF 03  F1 03  F4 03  F6 03  F9 03
    FB 03  FE 03  00 04  03 04  05 04  08 04  0A 04  0C 04  0F 04  11 04  14 04  16 04  19 04  1B 04  1D 04  20 04
    22 04  25 04  27 04  29 04  2C 04  2E 04  30 04  33 04  35 04  37 04  3A 04  3C 04  3E 04  41 04  43 04  45 04
    48 04  4A 04  4C 04  4E 04  51 04  53 04  55 04  57 04  5A 04  5C 04  5E 04  60 04  63 04  65 04  67 04  69 04
    6C 04  6E 04  70 04  72 04  74 04  77 04  79 04  7B 04  7D 04  7F 04  81 04  84 04  86 04  88 04  8A 04  8C 04
    8E 04  90 04  92 04  95 04  97 04  99 04  9B 04  9D 04  9F 04  A1 04  A3 04  A5 04  A7 04  AA 04  AC 04  AE 04
    B0 04  B2 04  B4 04  B6 04  B8 04  BA 04  BC 04  BE 04  C0 04  C2 04  C4 04  C6 04  C8 04  CA 04  CC 04  CE 04
    D0 04  D2 04  D4 04  D6 04  D8 04  DA 04  DC 04  DE 04  E0 04  E2 04  E4 04  E6 04  E8 04  EA 04  EC 04  EE 04
    F0 04  F2 04  F4 04  F5 04  F7 04  F9 04  FB 04  FD 04  FF 04  01 05  03 05  05 05  07 05  09 05  0A 05  0C 05
    0E 05  10 05  12 05  14 05  16 05  18 05  19 05  1B 05  1D 05  1F 05  21 05  23 05  25 05  26 05  28 05  2A 05
    2C 05  2E 05  30 05  31 05  33 05  35 05  37 05  39 05  3B 05  3C 05  3E 05  40 05  42 05  44 05  45 05  47 05
    49 05  4B 05  4C 05  4E 05  50 05  52 05  54 05  55 05  57 05  59 05  5B 05  5C 05  5E 05  60 05  62 05  63 05
    65 05  67 05  69 05  6A 05  6C 05  6E 05  70 05  71 05  73 05  75 05  76 05  78 05  7A 05  7C 05  7D 05  7F 05
    81 05  82 05  84 05  86 05  87 05  89 05  8B 05  8C 05  8E 05  90 05  92 05  93 05  95 05  97 05  98 05  9A 05
    9C 05  9D 05  9F 05  A1 05  A2 05  A4 05  A5 05  A7 05  A9 05  AA 05  AC 05  AE 05  AF 05  B1 05  B3 05  B4 05
    B6 05  B7 05  B9 05  BB 05  BC 05  BE 05  C0 05  C1 05  C3 05  C4 05  C6 05  C8 05  C9 05  CB 05  CC 05  CE 05
    D0 05  D1 05  D3 05  D4 05  D6 05  D8 05  D9 05  DB 05  DC 05  DE 05  DF 05  E1 05  E3 05  E4 05  E6 05  E7 05
    E9 05  EA 05  EC 05  ED 05  EF 05  F1 05  F2 05  F4 05  F5 05  F7 05  F8 05  FA 05  FB 05  FD 05  FE 05  00 06
    02 06  03 06  05 06  06 06  08 06  09 06  0B 06  0C 06  0E 06  0F 06  11 06  12 06  14 06  15 06  17 06  18 06
    1A 06  1B 06  1D 06  1E 06  20 06  21 06  23 06  24 06  26 06  27 06  29 06  2A 06  2C 06  2D 06  2F 06  30 06
  $;

TYPE
  UINT16 = SYSTEM.CARD16;
  UINT32 = SYSTEM.CARD32;  INT32 = SYSTEM.INT32;

  ARGB* = SYSTEM.CARD32;

  Bitmap* = POINTER TO BitmapDesc;
  BitmapDesc* = RECORD
    width*:   INTEGER;
    height*:  INTEGER;
    address*: INTEGER;    (* Address of bitmap data                                  *)
    hdib:     INTEGER;    (* Device independant bitmap handle                        *)
    context:  INTEGER;    (* Device context that currently contains the bitmap       *)
    oldh:     INTEGER     (* Old handle for restoration following context operations *)
  END;

  CharacterHandler = PROCEDURE(ch: INTEGER);  (* Receives full UTF-32 codepoint *)
  DrawHandler      = PROCEDURE(x, y, width, height: INTEGER;  bitmap: Bitmap);
  MouseHandler     = PROCEDURE(x, y: INTEGER; flags: SET);

  Window* = POINTER TO WindowDesc;
  WindowDesc = RECORD
    hwnd:     INTEGER;
    bmp*:     Bitmap;
    x*:       INTEGER;
    y*:       INTEGER;
    width*:   INTEGER;
    height*:  INTEGER;
    DPI*:     INTEGER;
    highch:   INTEGER;  (* Leading/high surrogate codepoint of a pair *)
    dochar:   CharacterHandler;
    predraw:  DrawHandler;
    postdraw: DrawHandler;
    domouse:  MouseHandler;
    next:     Window
  END;

  MSG = RECORD
    hwnd:     INTEGER;
    message:  UINT32;
    pad1:     UINT32;
    wParam:   INTEGER;
    lParam:   INTEGER;
    time:     UINT32;
    x, y:     UINT32;
    lPrivate: UINT32
  END;

  Card16x256Array = POINTER TO RECORD lookup: ARRAY 256 OF SYSTEM.CARD16 END;
  Card16x512Array = POINTER TO RECORD lookup: ARRAY 512 OF SYSTEM.CARD16 END;

VAR
  GetLastError:       PROCEDURE(): INTEGER;
  Sleep:              PROCEDURE(ms: INTEGER);

  CreateDIBSection:   PROCEDURE(hdc, pbmi, usage, ppvbits, hsection, offset: INTEGER): INTEGER;
  SelectObject:       PROCEDURE(hdc, hobject: INTEGER): INTEGER;
  DeleteObject:       PROCEDURE(hobject: INTEGER): INTEGER;
  CreateCompatibleDC: PROCEDURE(hdc: INTEGER): INTEGER;
  CreateBitmap:       PROCEDURE(width, height, planes, depth, bits: INTEGER): INTEGER;
  BitBlt:             PROCEDURE(hdc, x, y, cx, cy, hdcSrc, x1, y1, rop: INTEGER): INTEGER;

  LoadCursorW:        PROCEDURE(hinstance, lpcursorname: INTEGER): INTEGER;
  RegisterClassExW:   PROCEDURE(wndclassexw: INTEGER): INTEGER;
  CreateWindowExW:    PROCEDURE(dwExStyle, lpClassName, lpWindowName, dwStyle, X, Y, nWidth, nHeight, hWndParent, hMenu, hInstance, lpParam: INTEGER): INTEGER;
  GetMessageW:        PROCEDURE(lpmsg, hwnd, filtermin, filtermax: INTEGER): INTEGER;
  PeekMessageW:       PROCEDURE(lpmsg, hwnd, filtermin, filtermax, remove: INTEGER): INTEGER;
  GetQueueStatus:     PROCEDURE(flags: INTEGER): INTEGER;
  TranslateMessage:   PROCEDURE(msg: INTEGER): INTEGER;
  DispatchMessageW:   PROCEDURE(msg: INTEGER): INTEGER;
  DefWindowProcW:     PROCEDURE(hwnd, umsg, wparam, lparam: INTEGER): INTEGER;
  PostQuitMessage:    PROCEDURE(retcode: INTEGER);
  BeginPaint:         PROCEDURE(hwnd, paintstruct: INTEGER): INTEGER;
  EndPaint:           PROCEDURE(hwnd, paintstruct: INTEGER): INTEGER;
  SetCapture:         PROCEDURE(hwnd: INTEGER);
  ReleaseCapture:     PROCEDURE;
  MoveWindow:         PROCEDURE(hwnd, x, y, w, h, repaint: INTEGER);
  GetDpiForWindow:    PROCEDURE(hwnd: INTEGER): INTEGER;
  ShowWindow:         PROCEDURE(hwnd, cmd: INTEGER);
  WInvalidateRect:    PROCEDURE(hwnd, rect, bErase: INTEGER): INTEGER;
  ShowCursor:         PROCEDURE(show: INTEGER);
  CreateIconIndirect: PROCEDURE(iconinfo: INTEGER): INTEGER;

  MsgWaitForMultipleObjects:     PROCEDURE(count, handles, waitall, ms, wakemask: INTEGER);
  SetProcessDpiAwarenessContext: PROCEDURE(context: INTEGER): INTEGER;

  FirstWindow: Window;

  Lin12: Card16x256Array;
  SRGB:  Card16x512Array;


PROCEDURE LastError;
BEGIN w.s("Last error: $"); w.h(GetLastError()); w.sl(".");
END LastError;

PROCEDURE ZeroFill(VAR x: ARRAY OF BYTE);
VAR i: INTEGER;
BEGIN FOR i := 0 TO LEN(x)-1 DO x[i] := 0 END END ZeroFill;


(* ----------------------------- Window bitmap ------------------------------ *)


PROCEDURE EnsureBitmap(w, h: INTEGER; VAR bitmap: Bitmap);
TYPE
  BITMAPINFOHEADER = RECORD
    size:          UINT32;
    width:         UINT32;
    height:        UINT32;
    planes:        UINT16;
    bitCount:      UINT16;
    compression:   UINT32;
    sizeImage:     UINT32;
    xPelsPerMeter: UINT32;
    yPelsPerMeter: UINT32;
    clrUsed:       UINT32;
    clrImportant:  UINT32
  END;
VAR
  res: INTEGER;
  bmi: BITMAPINFOHEADER;
BEGIN
  IF bitmap = NIL THEN NEW(bitmap); ZeroFill(bitmap^) END;

  IF (bitmap.width < w) OR (bitmap.height < h) THEN
    IF bitmap.hdib # 0 THEN
      res := SelectObject(bitmap.context, bitmap.oldh);
      res := DeleteObject(bitmap.hdib)
    END;
    bitmap.width   := 0;  bitmap.height := 0;
    bitmap.address := 0;  bitmap.hdib   := 0;
    bitmap.oldh    := 0;

    ZeroFill(bmi);
    bmi.size     := SYSTEM.SIZE(BITMAPINFOHEADER);
    bmi.width    := w;
    bmi.height   := -h;  (* Negative height => y=0 at top *)
    bmi.planes   := 1;
    bmi.bitCount := 32;  (* 4 bytes per pixel: RGBA *)
    bitmap.hdib  := CreateDIBSection(0, SYSTEM.ADR(bmi), 0, SYSTEM.ADR(bitmap.address), 0, 0);

    ASSERT(bitmap.hdib # 0);
    bitmap.width  := w;
    bitmap.height := h;

    IF bitmap.context = 0 THEN  (* If no context to reuse *)
      bitmap.context := CreateCompatibleDC(0);  ASSERT(bitmap.context # 0)
    END;

    bitmap.oldh := SelectObject(bitmap.context, bitmap.hdib)
  END
END EnsureBitmap;


PROCEDURE Clip(v, max: INTEGER): INTEGER;
BEGIN IF v > max THEN v := max END
RETURN v END Clip;


PROCEDURE DrawPixel*(bitmap: Bitmap; x, y: INTEGER; colour: ARGB);
BEGIN
  x := Clip(x, bitmap.width);
  y := Clip(y, bitmap.height);
  SYSTEM.PUT(bitmap.address + y * bitmap.width * 4 + x * 4, colour)
END DrawPixel;


PROCEDURE FillRectangle*(bitmap: Bitmap; x, y, wi, h: INTEGER; colour: ARGB);
VAR xs, ys, xl, yl: INTEGER;
BEGIN
  w.s("Fill rectangle. x "); w.i(x);
  w.s(", y ");               w.i(y);
  w.s(", w ");               w.i(wi);
  w.s(", h ");               w.i(h);
  w.s(", colour $");         w.h(colour);
  w.sl(".");
  xs := Clip(x, bitmap.width-1);   xl := Clip(x+wi-1, bitmap.width-1);
  ys := Clip(y, bitmap.height-1);  yl := Clip(y+h-1, bitmap.height-1);
  FOR y := ys TO yl DO
    FOR x := xs TO xl DO
      SYSTEM.PUT(bitmap.address + y * bitmap.width * 4 + x * 4, colour)
    END
  END
END FillRectangle;

PROCEDURE Quit*;
BEGIN PostQuitMessage(0) END Quit;

PROCEDURE Move*(window: Window; x, y: INTEGER);
BEGIN
  MoveWindow(window.hwnd, x, y, window.width, window.height, 0);
  window.x := x;  window.y := y
END Move;

PROCEDURE Resize*(window: Window; w, h: INTEGER);
BEGIN
  MoveWindow(window.hwnd, window.x, window.y, w, h, 1);
  window.width := w;  window.height := h
END Resize;


(* -------------------------- Rendering primitives -------------------------- *)


PROCEDURE ToSRGB8(lin: INTEGER): BYTE;
VAR result: BYTE;
BEGIN
  IF    lin <= 0    THEN result := 0
  ELSIF lin >= 4095 THEN result := 255
  ELSIF lin <  10   THEN result := (lin * 13 + 10) DIV 16
  ELSIF lin <  512  THEN result := (SRGB.lookup[lin] + 8) DIV 16
                    ELSE result := 21 + ((9718 * SRGB.lookup[lin DIV 8] - 35000) DIV 65536)
  END
RETURN result END ToSRGB8;


PROCEDURE TestSRGB;
VAR l: INTEGER;  b: BYTE;
BEGIN
  w.sl("SRGB test.");
  b := 0;
  REPEAT
    w.in(b,3);
    l := Lin12.lookup[b];
    w.s(" lin ");   w.in(l, 4);
    w.s(" srgb ");  w.in(ToSRGB8(l), 3);
    IF ToSRGB8(l) # b THEN w.s(" *****") END;
    w.l;
    INC(b)
  UNTIL b = 0
END TestSRGB;



PROCEDURE AlphaMultiplyPixel(pixel: INTEGER; alpha: BYTE): INTEGER;
VAR result: INTEGER;
BEGIN
  IF    alpha = 0   THEN result := 0
  ELSIF alpha = 255 THEN result := pixel
  ELSE
    result := (ToSRGB8(Lin12.lookup[(pixel DIV 10000H) MOD 100H] * alpha DIV 256) * 10000H)
            + (ToSRGB8(Lin12.lookup[(pixel DIV   100H) MOD 100H] * alpha DIV 256) *   100H)
            + (ToSRGB8(Lin12.lookup[ pixel             MOD 100H] * alpha DIV 256)         )
            + 0FF000000H;
  END
RETURN result END AlphaMultiplyPixel;


(*   BlendChannel - Blend alpha * foreground with 1-alpha * background *)
(*   entry  fg    - 8 bit gamma encoded foreground intensity           *)
(*          bg    - 8 bit gamma encoded background intensity           *)
(*          alpha - 8 bit linear alpha                                 *)
PROCEDURE BlendChannel(fg, bg, alpha: BYTE): BYTE;
BEGIN RETURN ToSRGB8((Lin12.lookup[fg] * alpha + Lin12.lookup[bg] * (255 - alpha)) DIV 256)
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
            + (BlendChannel( fg             MOD 100H,  bg             MOD 100H, alpha)         )
            + 0FF000000H;
  END
RETURN result END BlendPixel;


PROCEDURE RenderAlphaMapToBitmap*(
  x:         INTEGER;  (* In 1/4 pixels   *)
  y:         INTEGER;  (* In whole pixels *)
  width:     INTEGER;  (* In 1/4 pixels   *)
  height:    INTEGER;  (* In whole pixels *)
  mapadr:    INTEGER;
  paint:     ARGB;
  bitmapadr: INTEGER;
  stride:    INTEGER;
  bitmaplim: INTEGER
);
VAR
  alpha, len: BYTE;
  sp,    mp:  INTEGER;
  pixel:      ARGB;
  subpixel:   INTEGER;
  alphasum:   INTEGER;
BEGIN
  (*
  w.s("RenderAlphaMapToBitmap, x "); w.i(x);
  w.s(", width ");           w.i(width);
  w.s(", height ");          w.i(height);
  w.s(", mapadr $");         w.h(mapadr);  w.sl(".");

  w.DumpMem(2, mapadr, 0, 323);
  *)

  (*ASSERT(x >= 0);  ASSERT(y >= 0);*)

  mp       := bitmapadr + 4 * (stride * y + x DIV 4);
  subpixel := x MOD 4;
  alphasum := 0;
  sp       := 0;

  SYSTEM.GET(mapadr, len);  INC(mapadr);
  WHILE (mp < bitmaplim) & (len # 0) DO
    CASE len DIV 64 OF
    | 0: alpha := len;        len := 1
    | 1: len := len MOD 40H;  alpha := 0;
    | 2: len := len MOD 40H;  alpha := 40H;
    | 3: len := len MOD 40H;  SYSTEM.GET(mapadr, alpha);  INC(mapadr);
    END;

    WHILE len > 0 DO
      INC(alphasum, alpha); INC(subpixel);
      IF subpixel > 3 THEN
        IF (alphasum > 0) & (mp >= bitmapadr) & (mp+3 < bitmaplim) THEN
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
        IF (alphasum > 0) & (mp >= bitmapadr) & (mp+3 < bitmaplim) THEN  (* write remaining partial pixel *)
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
END RenderAlphaMapToBitmap;


(* ---------------- Make Windows application icon for Oberon ---------------- *)
(* - Obtains icon from character U+058D (Armenian eternity sign) in Calibri - *)

PROCEDURE MakeIcon(): INTEGER;
VAR
  font:       Fonts.Font;
  glyph:      Fonts.Glyph;
  maskbits:   ARRAY  128 OF BYTE;  (* 32 x 32 x 1 bit *)
  colourbits: ARRAY 1024 OF ARGB;  (* 32 x 32 x 4 bytes *)
  icon, i, j: INTEGER;
  IconInfo:   RECORD
    fIcon:    UINT32;
    xHotspot: UINT32;
    yHotspot: UINT32;
    hbmMask:  INTEGER;
    hbmColor: INTEGER
  END;
BEGIN
  NEW(glyph);
  font := Fonts.GetFont("segoe ui symbol", 40);
  Fonts.GetAlphaMap(font, 2707H (*58DH*), glyph);
  w.s("Icon dimensions w "); w.i((glyph.mapWidth+3) DIV 4);
  w.s(", h "); w.i(glyph.mapHeight); w.sl(".");
  FOR i := 0 TO LEN(maskbits)-1 DO maskbits[i] := 0FFH END;
  ZeroFill(colourbits);
  RenderAlphaMapToBitmap(
    0, 0,
    glyph.mapWidth, glyph.mapHeight,
    glyph.map, 0FFFFFFFFH,
    SYSTEM.ADR(colourbits), 32,
    SYSTEM.ADR(colourbits) + 4 * LEN(colourbits)
  );
  IconInfo.fIcon    := 1;
  IconInfo.xHotspot := 0;
  IconInfo.yHotspot := 0;
  IconInfo.hbmMask  := CreateBitmap(32, 32, 1,  1, SYSTEM.ADR(maskbits));
  IconInfo.hbmColor := CreateBitmap(32, 32, 1, 32, SYSTEM.ADR(colourbits));
  icon := CreateIconIndirect(SYSTEM.ADR(IconInfo));
  ASSERT(icon # 0);
  ASSERT(DeleteObject(IconInfo.hbmMask)  # 0);
  ASSERT(DeleteObject(IconInfo.hbmColor) # 0);
RETURN icon END MakeIcon;




(* -------------------------- Window hwnd handling -------------------------- *)


PROCEDURE FindWindow(hwnd: INTEGER): Window;
VAR window: Window;
BEGIN window := FirstWindow;
  (*
  w.s("Find window $"); w.h(hwnd);
  IF window = NIL THEN w.sl(", no windows!")
  ELSE w.s(", starting with $"); w.h(window.hwnd); w.sl(".") END;
  *)
  WHILE (window # NIL) & (hwnd # window.hwnd) DO window := window.next END
RETURN window END FindWindow;


PROCEDURE Paint(hwnd: INTEGER);
TYPE
  PAINTSTRUCT = RECORD
    hdc:         INTEGER;
    fErase:      UINT32;
    left:        UINT32;
    top:         UINT32;
    right:       UINT32;
    bottom:      UINT32;
    fRestore:    UINT32;
    fIncUpdate:  UINT32;
    rgbReserved: ARRAY 36 OF BYTE
  END;
VAR
  res:    INTEGER;
  ps:     PAINTSTRUCT;
  x, y:   INTEGER;
  width:  INTEGER;
  height: INTEGER;
  window: Window;
  hdc:    INTEGER;
BEGIN
  window := FindWindow(hwnd);
  ASSERT(window # NIL);

  (*EnsureBitmap(window.width, window.height, window.bmp);*)

  res    := BeginPaint(hwnd, SYSTEM.ADR(ps));
  x      := ps.left;
  y      := ps.top;
  width  := ps.right  - ps.left + 1;
  height := ps.bottom - ps.top  + 1;

  IF window.predraw # NIL THEN window.predraw(x, y, width, height, window.bmp) END;

  (*
  w.s("BitBlt x "); w.i(x);      w.s(", y ");      w.i(y);
  w.s(", width ");  w.i(width);  w.s(", height "); w.i(height);  w.sl(".");
  *)

  IF TRUE THEN
    res := BitBlt(ps.hdc, x, y, width, height, window.bmp.context, x, y, 0CC0020H);  (* SRCCPY *)
  ELSE (* Testing: paint the whole bitmap *)
    res := BitBlt(ps.hdc, 0, 0, window.width, window.height, window.bmp.context, 0, 0, 0CC0020H);  (* SRCCPY *)
  END;

  IF res = 0 THEN LastError END;
  ASSERT(res # 0);

  IF window.postdraw # NIL THEN window.postdraw(x, y, width, height, window.bmp) END;

  res := EndPaint(hwnd, SYSTEM.ADR(ps));
END Paint;


PROCEDURE Size(wi, h: INTEGER);
BEGIN w.s("size := "); w.i(wi); w.c(","); w.i(h); w.sl(".");
END Size;


PROCEDURE Char(hwnd, ch: INTEGER);  (* UTF-16 value *)
VAR window: Window;
BEGIN w.s("char $"); w.h(ch); w.sl(".");
  window := FindWindow(hwnd);  ASSERT(window # NIL);
  IF window.dochar # NIL THEN
    IF (ch >= 0D800H) & (ch <= 0DBFFH) THEN
      window.highch := ch
    ELSIF (ch >= 0DC00H) & (ch <= 0DFFFH) THEN
      window.dochar(LSL(window.highch MOD 400H, 10) + 10000H + ch MOD 400H)
    ELSE
      window.dochar(ch)
    END
  END
END Char;


PROCEDURE Key(hwnd, key: INTEGER);
VAR window: Window;
BEGIN
  window := FindWindow(hwnd);  ASSERT(window # NIL);
  IF window.dochar # NIL THEN
    CASE key OF
    | 25H: (* VK_LEFT  *) window.dochar(11H)
    | 26H: (* VK_UP    *) window.dochar(13H)
    | 27H: (* VK_RIGHT *) window.dochar(12H)
    | 28H: (* VK_DOWN  *) window.dochar(14H)
    END
  END
END Key;


PROCEDURE Mouse(hwnd, msg, x, y, flags: INTEGER);
VAR window: Window;
BEGIN
  IF x > 32767 THEN x := x - 10000H END; (* Sign extend 16 bit value *)
  IF y > 32767 THEN y := y - 10000H END; (* Sign extend 16 bit value *)
  window := FindWindow(hwnd);  ASSERT(window # NIL);
  IF    msg = 201H (* WM_LBUTTONDOWN *) THEN SetCapture(hwnd)
  ELSIF msg = 202H (* WM_LBUTTONUP   *) THEN ReleaseCapture
  END;
  IF window.domouse # NIL THEN window.domouse(x, y, SYSTEM.VAL(SET, flags)) END;
END Mouse;


(* -------------------------------------------------------------------------- *)


PROCEDURE WndProc(hwnd, msg, wp, lp: INTEGER): INTEGER;
VAR res: INTEGER;
BEGIN
  (*w.s("    WndProc: hwnd $"); w.h(hwnd); w.s(", msg "); WindowsMessageNames.Write(msg); w.l;*)
  IF FirstWindow.hwnd = 0 THEN FirstWindow.hwnd := hwnd END;
  res := 0;
  IF     msg =   02H  (* WM_DESTROY       *) THEN PostQuitMessage(0)
  ELSIF  msg =   0FH  (* WM_PAINT         *) THEN Paint(hwnd)
  ELSIF  msg =   14H  (* WM_ERASEBKGND    *) THEN
  ELSIF  msg =   05H  (* WM_SIZE          *) THEN Size(lp MOD 10000H, lp DIV 10000H MOD 10000H)
  ELSIF  msg =  100H  (* WM_KEYDOWN       *) THEN Key(hwnd, wp)
  ELSIF  msg =  102H  (* WM_CHAR          *) THEN Char(hwnd, wp)
  ELSIF (msg >= 200H) (* WM_MOUSEMOVE     *)
     &  (msg <= 209H) (* WM_MBUTTONDBLCLK *) THEN Mouse(hwnd, msg, lp MOD 10000H, lp DIV 10000H MOD 10000H, wp)
  ELSE  res := DefWindowProcW(hwnd, msg, wp, lp)
  END
RETURN res END WndProc;


(* -------------------------------------------------------------------------- *)

PROCEDURE NewWindow*(x, y, width, height: INTEGER): Window;
TYPE
  wndclassexw = RECORD
    cbsize:        UINT32;
    style:         UINT32;
    wndproc:       INTEGER;
    cbClsExtra:    UINT32;
    cbWndExtra:    UINT32;
    hInstance:     INTEGER;
    hIcon:         INTEGER;
    hCursor:       INTEGER;
    hbrBackground: INTEGER;
    lpszMenuName:  INTEGER;
    className:     INTEGER;
    hIconSm:       INTEGER
  END;

VAR
  class:      wndclassexw;
  classAtom:  INTEGER;
  classname:  ARRAY 16  OF SYSTEM.CARD16;
  windowname: ARRAY 256 OF SYSTEM.CARD16;
  hwnd:       INTEGER;
  i:          INTEGER;
  window:     Window;
BEGIN
  i := K.Utf8ToUtf16("Oberon", classname);
  i := K.Utf8ToUtf16("Oberon", windowname);
  ZeroFill(class);
  class.cbsize    := SYSTEM.SIZE(wndclassexw);
  class.style     := 3;                      (* CS_HREDRAW|CS_VREDRAW *)
  class.wndproc   := SYSTEM.VAL(INTEGER, WndProc);
  class.className := SYSTEM.ADR(classname);
  class.hIcon     := MakeIcon();
  classAtom       := RegisterClassExW(SYSTEM.ADR(class));
  ASSERT(classAtom # 0);

  NEW(window);
  window.hwnd      := 0;    (* Will be filled in by first WndProc callback during CreateWindow *)
  window.x         := x;
  window.y         := y;
  window.width     := width;
  window.height    := height;
  window.dochar    := NIL;
  window.predraw   := NIL;
  window.postdraw  := NIL;
  window.domouse   := NIL;
  window.next      := FirstWindow;
  FirstWindow      := window;

  hwnd := CreateWindowExW(
    0,                       (* Extended window style *)
    SYSTEM.ADR(classname),
    SYSTEM.ADR(windowname),
    80000000H,               (* WS_POPUP *)
    x, y, width, height,     (* Initial position *)
    0, 0, 0, 0               (* hWndParent, hMenu, hInstance, lpParam *)
  );
  ASSERT(hwnd # 0);

  w.s("Created window. hwnd $");  w.h(hwnd);  w.sl(".");

  K.SetHWnd(hwnd);  (* Make sure kernel error message boxes stop the message pump *)

  EnsureBitmap(width, height, window.bmp);

  window.DPI := GetDpiForWindow(hwnd);

  w.s("GetDpiForWindow -> ");  w.i(window.DPI);  w.sl(".");

  ShowCursor(0);
  ShowWindow(hwnd, 1);

  w.sl("ShowWindow complete.");

RETURN window END NewWindow;

PROCEDURE SetCharacterHandler* (w: Window; h: CharacterHandler);
BEGIN w.dochar := h END SetCharacterHandler;

PROCEDURE SetMouseHandler* (w: Window; h: MouseHandler);
BEGIN w.domouse := h END SetMouseHandler;

PROCEDURE SetDrawHandlers* (w: Window; pre, post: DrawHandler);
BEGIN w.predraw := pre;  w.postdraw := post END SetDrawHandlers;


(* ------------- Windows message pump - pump until queue empty -------------- *)

PROCEDURE MessagePump*(): BOOLEAN;  (* Returns FALSE only on receipt of WM_QUIT *)
VAR
  msg:  MSG;
  res:  INTEGER;
  quit: BOOLEAN;
BEGIN quit := FALSE;
  res := PeekMessageW(SYSTEM.ADR(msg), 0,0,0,1); (* Get and remove message if available *)
  WHILE ~quit & (res # 0) DO
    ASSERT(res # -1);
    w.s("PeekMessageW: "); WindowsMessageNames.Write(msg.message); w.l;
    IF msg.message = 0FH THEN w.sl("** PeekMessage returned WM_PAINT **") END;
    IF msg.message = 12H THEN quit := TRUE  (* 12H = WM_QUIT *)
    ELSE
      res := TranslateMessage(SYSTEM.ADR(msg));
      res := DispatchMessageW(SYSTEM.ADR(msg));
      res := PeekMessageW(SYSTEM.ADR(msg), 0,0,0,1); (* Get and remove messgae if available *)
    END
  END;
  (*
  (* There may still be a WM_PAINT in the queue, but only GetMessage can return that. *)
  IF ~quit & (GetQueueStatus(20H) # 0) THEN  (* Check for pending WM_PAINT *)
    w.sl("** GetQueueStatus reported WM_PAINT available **");
    res := GetMessageW(SYSTEM.ADR(msg), 0,0,0);
    w.s("GetMessageW: "); WindowsMessageNames.Write(msg.message); w.l;
    IF  res # 0 THEN
      res := TranslateMessage(SYSTEM.ADR(msg));
      res := DispatchMessageW(SYSTEM.ADR(msg))
    END
  END
  *)
RETURN ~quit END MessagePump;


PROCEDURE ProcessOneMessage* (): INTEGER;  (* 0 - none available, 1 - processed, 2 - WM_QUIT received *)
VAR
  msg:  MSG;
  res:  INTEGER;
BEGIN
  res := PeekMessageW(SYSTEM.ADR(msg), 0,0,0,1); (* Get and remove message if available *)
  IF res # 0  THEN
    IF msg.message = 12H THEN
      res :=  2  (* 12H = WM_QUIT *)
    ELSE
      res := TranslateMessage(SYSTEM.ADR(msg));
      res := DispatchMessageW(SYSTEM.ADR(msg));
      res := 1;
    END
  END
RETURN res END ProcessOneMessage;

PROCEDURE WaitMsgOrTime*(time: INTEGER);  (* Waits for time (ms) OR message in queue *)
BEGIN MsgWaitForMultipleObjects(0, 0, 0, time, 1DFFH);
END WaitMsgOrTime;


(* Shortcut to get the next message if and only if it is a mouse movement *)
PROCEDURE GetMouseMessage* (VAR x, y: INTEGER;  VAR flags: SET): BOOLEAN;
VAR
  msg:  MSG;
  res:  INTEGER;
BEGIN
  res := PeekMessageW(SYSTEM.ADR(msg), 0, 200H, 209H, 1); (* Any mouse move w/wout click *)
  IF res # 0 THEN
    x := msg.lParam MOD 10000H;
    y := msg.lParam DIV 10000H MOD 10000H;
    flags := {};
    IF ODD(msg.wParam DIV 2)  THEN INCL(flags, 0) END;  (* (1) -> MR *)
    IF ODD(msg.wParam DIV 16) THEN INCL(flags, 1) END;  (* (4) -> MM *)
    IF ODD(msg.wParam)        THEN INCL(flags, 2) END;  (* (0) -> ML *)
  END
RETURN res # 0 END GetMouseMessage;


PROCEDURE InvalidateRect* (w: Window; x, y, width, height: INTEGER);
VAR rect: RECORD left, top, right, bottom: INT32 END;
BEGIN
  rect.left   := x;
  rect.top    := y;
  rect.right  := x + width;
  rect.bottom := y + height;
  ASSERT(WInvalidateRect(w.hwnd, SYSTEM.ADR(rect), 0) # 0)
END InvalidateRect;

PROCEDURE Invalidate* (w: Window);
BEGIN InvalidateRect(w, 0, 0, w.bmp.width, w.bmp.height) END Invalidate;


(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)



(* -------------------------------------------------------------------------- *)


BEGIN
  w.sl("Hello teapots.");

  Lin12 := SYSTEM.VAL(Card16x256Array, SYSTEM.ADR(Lin12Table));
  SRGB  := SYSTEM.VAL(Card16x512Array, SYSTEM.ADR(SRGBtable));
  TestSRGB;

  K.GetProc(K.Kernel, "GetLastError",       GetLastError);        ASSERT(GetLastError       # NIL);
  K.GetProc(K.Kernel, "Sleep",              Sleep);               ASSERT(Sleep              # NIL);
  K.GetProc(K.Gdi,    "CreateDIBSection",   CreateDIBSection);    ASSERT(CreateDIBSection   # NIL);
  K.GetProc(K.Gdi,    "SelectObject",       SelectObject);        ASSERT(SelectObject       # NIL);
  K.GetProc(K.Gdi,    "DeleteObject",       DeleteObject);        ASSERT(DeleteObject       # NIL);
  K.GetProc(K.Gdi,    "CreateCompatibleDC", CreateCompatibleDC);  ASSERT(CreateCompatibleDC # NIL);
  K.GetProc(K.Gdi,    "CreateBitmap",       CreateBitmap);        ASSERT(CreateBitmap       # NIL);
  K.GetProc(K.Gdi,    "BitBlt",             BitBlt);              ASSERT(BitBlt             # NIL);
  K.GetProc(K.User,   "LoadCursorW",        LoadCursorW);         ASSERT(LoadCursorW        # NIL);
  K.GetProc(K.User,   "RegisterClassExW",   RegisterClassExW);    ASSERT(RegisterClassExW   # NIL);
  K.GetProc(K.User,   "CreateWindowExW",    CreateWindowExW);     ASSERT(CreateWindowExW    # NIL);
  K.GetProc(K.User,   "GetMessageW",        GetMessageW);         ASSERT(GetMessageW        # NIL);
  K.GetProc(K.User,   "PeekMessageW",       PeekMessageW);        ASSERT(PeekMessageW       # NIL);
  K.GetProc(K.User,   "GetQueueStatus",     GetQueueStatus);      ASSERT(GetQueueStatus     # NIL);
  K.GetProc(K.User,   "TranslateMessage",   TranslateMessage);    ASSERT(TranslateMessage   # NIL);
  K.GetProc(K.User,   "DispatchMessageW",   DispatchMessageW);    ASSERT(DispatchMessageW   # NIL);
  K.GetProc(K.User,   "DefWindowProcW",     DefWindowProcW);      ASSERT(DefWindowProcW     # NIL);
  K.GetProc(K.User,   "PostQuitMessage",    PostQuitMessage);     ASSERT(PostQuitMessage    # NIL);
  K.GetProc(K.User,   "BeginPaint",         BeginPaint);          ASSERT(BeginPaint         # NIL);
  K.GetProc(K.User,   "EndPaint",           EndPaint);            ASSERT(EndPaint           # NIL);
  K.GetProc(K.User,   "SetCapture",         SetCapture);          ASSERT(SetCapture         # NIL);
  K.GetProc(K.User,   "ReleaseCapture",     ReleaseCapture);      ASSERT(ReleaseCapture     # NIL);
  K.GetProc(K.User,   "MoveWindow",         MoveWindow);          ASSERT(MoveWindow         # NIL);
  K.GetProc(K.User,   "GetDpiForWindow",    GetDpiForWindow);     ASSERT(GetDpiForWindow    # NIL);
  K.GetProc(K.User,   "ShowWindow",         ShowWindow);          ASSERT(ShowWindow         # NIL);
  K.GetProc(K.User,   "InvalidateRect",     WInvalidateRect);     ASSERT(WInvalidateRect    # NIL);
  K.GetProc(K.User,   "ShowCursor",         ShowCursor);          ASSERT(ShowCursor         # NIL);
  K.GetProc(K.User,   "CreateIconIndirect", CreateIconIndirect);  ASSERT(CreateIconIndirect # NIL);

  K.GetProc(K.User, "MsgWaitForMultipleObjects", MsgWaitForMultipleObjects);
  ASSERT(MsgWaitForMultipleObjects # NIL);

  K.GetProc(K.User, "SetProcessDpiAwarenessContext", SetProcessDpiAwarenessContext);
  ASSERT(SetProcessDpiAwarenessContext # NIL);
  IF SetProcessDpiAwarenessContext # NIL THEN
    ASSERT(SetProcessDpiAwarenessContext (-3) # 0)  (* DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE *)
  END
END Windows.
