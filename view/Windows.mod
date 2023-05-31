MODULE Windows;

IMPORT SYSTEM, w := Writer, K := Kernel, Boot, WindowsMessageNames, Fonts;

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

VAR
  CreateDIBSection:   PROCEDURE(hdc, pbmi, usage, ppvbits, hsection, offset: INTEGER): INTEGER;
  SelectObject:       PROCEDURE(hdc, hobject: INTEGER): INTEGER;
  DeleteObject:       PROCEDURE(hobject: INTEGER): INTEGER;
  CreateCompatibleDC: PROCEDURE(hdc: INTEGER): INTEGER;
  CreateBitmap:       PROCEDURE(width, height, planes, depth, bits: INTEGER): INTEGER;
  LoadCursorW:        PROCEDURE(hinstance, lpcursorname: INTEGER): INTEGER;
  RegisterClassExW:   PROCEDURE(wndclassexw: INTEGER): INTEGER;
  CreateWindowExW:    PROCEDURE(dwExStyle, lpClassName, lpWindowName,
                                dwStyle, X, Y, nWidth, nHeight,
                                hWndParent, hMenu, hInstance, lpParam: INTEGER): INTEGER;
  GetMessageW:        PROCEDURE(lpmsg, hwnd, filtermin, filtermax: INTEGER): INTEGER;
  PeekMessageW:       PROCEDURE(lpmsg, hwnd, filtermin, filtermax, remove: INTEGER): INTEGER;
  GetQueueStatus:     PROCEDURE(flags: INTEGER): INTEGER;
  TranslateMessage:   PROCEDURE(msg: INTEGER): INTEGER;
  DispatchMessageW:   PROCEDURE(msg: INTEGER): INTEGER;
  DefWindowProcW:     PROCEDURE(hwnd, umsg, wparam, lparam: INTEGER): INTEGER;
  GetLastError:       PROCEDURE(): INTEGER;
  PostQuitMessage:    PROCEDURE(retcode: INTEGER);
  BeginPaint:         PROCEDURE(hwnd, paintstruct: INTEGER): INTEGER;
  EndPaint:           PROCEDURE(hwnd, paintstruct: INTEGER): INTEGER;
  BitBlt:             PROCEDURE(hdc, x, y, cx, cy, hdcSrc, x1, y1, rop: INTEGER): INTEGER;
  SetCapture:         PROCEDURE(hwnd: INTEGER);
  ReleaseCapture:     PROCEDURE;
  MoveWindow:         PROCEDURE(hwnd, x, y, w, h, repaint: INTEGER);
  GetDpiForWindow:    PROCEDURE(hwnd: INTEGER): INTEGER;
  ShowWindow:         PROCEDURE(hwnd, cmd: INTEGER);
  WInvalidateRect:    PROCEDURE(hwnd, rect, bErase: INTEGER): INTEGER;
  ShowCursor:         PROCEDURE(show: INTEGER);
  CreateIconIndirect: PROCEDURE(iconinfo: INTEGER): INTEGER;
  Sleep:              PROCEDURE(ms: INTEGER);

  MsgWaitForMultipleObjects:     PROCEDURE(count, handles, waitall, ms, wakemask: INTEGER);
  SetProcessDpiAwarenessContext: PROCEDURE(context: INTEGER): INTEGER;

  FirstWindow: Window;


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
            + 0FF000000H;
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
      IF (mp < bitmaplim) & (sp >= width) THEN
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
