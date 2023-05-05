MODULE Windows;

IMPORT SYSTEM, w := Writer, K := Kernel, Boot, WindowsMessageNames;

TYPE
  UINT16 = SYSTEM.CARD16;
  UINT32 = SYSTEM.CARD32;

  Colour* = SYSTEM.CARD32;

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
  DrawHandler      = PROCEDURE(width, height: INTEGER; bitmap: Bitmap);
  MouseHandler     = PROCEDURE(x, y: INTEGER; flags: SET);

  Window* = POINTER TO WindowDesc;
  WindowDesc = RECORD
    hwnd:    INTEGER;
    bmp:     Bitmap;
    x*:      INTEGER;
    y*:      INTEGER;
    width*:  INTEGER;
    height*: INTEGER;
    highch:  INTEGER;  (* Leading/high surrogate codepoint of a pair *)
    dochar:  CharacterHandler;
    dodraw:  DrawHandler;
    domouse: MouseHandler;
    next:    Window
  END;

VAR
  CreateDIBSection:       PROCEDURE(hdc, pbmi, usage, ppvbits, hsection, offset: INTEGER): INTEGER;
  SelectObject:           PROCEDURE(hdc, hobject: INTEGER): INTEGER;
  DeleteObject:           PROCEDURE(hobject: INTEGER): INTEGER;
  CreateCompatibleDC:     PROCEDURE(hdc: INTEGER): INTEGER;
  LoadCursorW:            PROCEDURE(hinstance, lpcursorname: INTEGER): INTEGER;
  RegisterClassExW:       PROCEDURE(wndclassexw: INTEGER): INTEGER;
  CreateWindowExW:        PROCEDURE(dwExStyle, lpClassName, lpWindowName,
                                    dwStyle, X, Y, nWidth, nHeight,
                                    hWndParent, hMenu, hInstance, lpParam: INTEGER): INTEGER;
  GetMessageW:            PROCEDURE(lpmsg, hwnd, filtermin, filtermax: INTEGER): INTEGER;
  TranslateMessage:       PROCEDURE(msg: INTEGER): INTEGER;
  DispatchMessageW:       PROCEDURE(msg: INTEGER): INTEGER;
  DefWindowProcW:         PROCEDURE(hwnd, umsg, wparam, lparam: INTEGER): INTEGER;
  GetLastError:           PROCEDURE(): INTEGER;
  PostQuitMessage:        PROCEDURE(retcode: INTEGER);
  BeginPaint:             PROCEDURE(hwnd, paintstruct: INTEGER): INTEGER;
  EndPaint:               PROCEDURE(hwnd, paintstruct: INTEGER): INTEGER;
  BitBlt:                 PROCEDURE(hdc, x, y, cx, cy, hdcSrc, x1, y1, rop: INTEGER): INTEGER;
  SetProcessDpiAwareness: PROCEDURE(awareness: INTEGER);
  SetCapture:             PROCEDURE(hwnd: INTEGER);
  ReleaseCapture:         PROCEDURE;
  MoveWindow:             PROCEDURE(hwnd, x, y, w, h, repaint: INTEGER);

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
    bitmap.width := 0;  bitmap.height := 0;
    bitmap.address   := 0;  bitmap.hdib   := 0;
    bitmap.oldh  := 0;
    ZeroFill(bmi);
    bmi.size     := SYSTEM.SIZE(BITMAPINFOHEADER);
    bmi.width    := w;
    bmi.height   := -h;  (* Negative height => y=0 at top *)
    bmi.planes   := 1;
    bmi.bitCount := 32;  (* 4 bytes per pixel: RGBA *)
    bitmap.hdib := CreateDIBSection(0, SYSTEM.ADR(bmi), 0, SYSTEM.ADR(bitmap.address), 0, 0);
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

PROCEDURE DrawPixel*(bitmap: Bitmap; x, y: INTEGER; colour: Colour);
BEGIN
  x := Clip(x, bitmap.width);
  y := Clip(y, bitmap.height);
  SYSTEM.PUT(bitmap.address + y * bitmap.width * 4 + x * 4, colour)
END DrawPixel;

PROCEDURE FillRectangle*(bitmap: Bitmap; x, y, wi, h: INTEGER; colour: Colour);
VAR xs, ys, xl, yl: INTEGER;
BEGIN
  (*
  w.s("Fill rectangle. x "); w.i(x);
  w.s(", y ");               w.i(y);
  w.s(", w ");               w.i(wi);
  w.s(", h ");               w.i(h);
  w.sl(".");
  *)
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


(* -------------------------- Window hwnd handling -------------------------- *)


PROCEDURE FindWindow(hwnd: INTEGER): Window;
VAR window: Window;
BEGIN window := FirstWindow;
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
  ps:     PAINTSTRUCT;
  res:    INTEGER;
  window: Window;
  hdc:    INTEGER;
BEGIN
  res := BeginPaint(hwnd, SYSTEM.ADR(ps));
  window := FindWindow(hwnd);
  ASSERT(window # NIL);

  EnsureBitmap(window.width, window.height, window.bmp);

  IF window.dodraw # NIL THEN window.dodraw(window.width, window.height, window.bmp) END;

  res := BitBlt(ps.hdc,
                0, 0,
                window.width, window.height,
                window.bmp.context,
                0, 0,
                0CC0020H);  (* SRCCPY *)
  IF res = 0 THEN LastError END;
  ASSERT(res # 0);

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
BEGIN (*w.s("    WndProc: "); WindowsMessageNames.Write(msg); w.l;*)
  res := 0;
  IF     msg =   02H  (* WM_DESTROY       *) THEN PostQuitMessage(0)
  ELSIF  msg =   0FH  (* WM_PAINT         *) THEN Paint(hwnd)
  ELSIF  msg =   14H  (* WM_ERASEBKGND    *) THEN
  ELSIF  msg =   05H  (* WM_SIZE          *) THEN Size(lp MOD 10000H, lp DIV 10000H MOD 10000H)
  ELSIF  msg =  102H  (* WM_CHAR          *) THEN Char(hwnd, wp)
  ELSIF (msg >= 200H) (* WM_MOUSEMOVE     *)
     &  (msg <= 209H) (* WM_MBUTTONDBLCLK *) THEN Mouse(hwnd, msg, lp MOD 10000H, lp DIV 10000H MOD 10000H, wp)
  ELSE  res := DefWindowProcW(hwnd, msg, wp, lp)
  END
RETURN res END WndProc;


(* -------------------------------------------------------------------------- *)


PROCEDURE NewWindow*(x, y, w, h: INTEGER;
                     dochar:  CharacterHandler;
                     dodraw:  DrawHandler;
                     domouse: MouseHandler): Window;
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
  i := K.Utf8ToUtf16("Oberon",        classname);
  i := K.Utf8ToUtf16("Oberon window", windowname);
  ZeroFill(class);
  class.cbsize    := SYSTEM.SIZE(wndclassexw);
  class.style     := 3;                      (* CS_HREDRAW|CS_VREDRAW *)
  class.wndproc   := SYSTEM.VAL(INTEGER, WndProc);
  class.className := SYSTEM.ADR(classname);
  class.hCursor   := LoadCursorW(0, 32512);  (* IDC_ARROW *)  ASSERT(class.hCursor # 0);
  classAtom       := RegisterClassExW(SYSTEM.ADR(class));
  ASSERT(classAtom # 0);
  hwnd := CreateWindowExW(
    0,                       (* Extended window style *)
    SYSTEM.ADR(classname),
    SYSTEM.ADR(windowname),
  (*10CF0000H,               (* WS_OVERLAPPEDWINDOW|WS_VISIBLE *) *)
    90000000H,               (* WS_POPUP|WS_VISIBLE *)
    x, y, w, h,              (* Initial position *)
    0, 0, 0, 0               (* hWndParent, hMenu, hInstance, lpParam *)
  );
  ASSERT(hwnd # 0);

  NEW(window);
  window.hwnd    := hwnd;
  window.x       := x;
  window.y       := y;
  window.width   := w;
  window.height  := h;
  window.dochar  := dochar;
  window.dodraw  := dodraw;
  window.domouse := domouse;
  window.next    := FirstWindow;
  FirstWindow    := window;
RETURN window END NewWindow;


(* -------------------------------------------------------------------------- *)

PROCEDURE MessagePump*;
TYPE
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
  msg: MSG;
  res: INTEGER;
BEGIN
  (*w.sl("MessagePump starting.");*)
  res := GetMessageW(SYSTEM.ADR(msg), 0,0,0);
  WHILE res # 0 DO
    ASSERT(res # -1);
    (*w.s("GetMessageW: "); WindowsMessageNames.Write(msg.message); w.l;*)
    res := TranslateMessage(SYSTEM.ADR(msg));
    res := DispatchMessageW(SYSTEM.ADR(msg));

    res := GetMessageW(SYSTEM.ADR(msg), 0,0,0);
  END
END MessagePump;



(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)



(* -------------------------------------------------------------------------- *)


BEGIN
  w.sl("Hello teapots.");

  K.GetProc(K.Kernel, "GetLastError",           GetLastError);           ASSERT(GetLastError            # NIL);
  K.GetProc(K.Gdi,    "CreateDIBSection",       CreateDIBSection);       ASSERT(CreateDIBSection        # NIL);
  K.GetProc(K.Gdi,    "SelectObject",           SelectObject);           ASSERT(SelectObject            # NIL);
  K.GetProc(K.Gdi,    "DeleteObject",           DeleteObject);           ASSERT(DeleteObject            # NIL);
  K.GetProc(K.Gdi,    "CreateCompatibleDC",     CreateCompatibleDC);     ASSERT(CreateCompatibleDC      # NIL);
  K.GetProc(K.Gdi,    "BitBlt",                 BitBlt);                 ASSERT(BitBlt                  # NIL);
  K.GetProc(K.User,   "LoadCursorW",            LoadCursorW);            ASSERT(LoadCursorW             # NIL);
  K.GetProc(K.User,   "RegisterClassExW",       RegisterClassExW);       ASSERT(RegisterClassExW        # NIL);
  K.GetProc(K.User,   "CreateWindowExW",        CreateWindowExW);        ASSERT(CreateWindowExW         # NIL);
  K.GetProc(K.User,   "GetMessageW",            GetMessageW);            ASSERT(GetMessageW             # NIL);
  K.GetProc(K.User,   "TranslateMessage",       TranslateMessage);       ASSERT(TranslateMessage        # NIL);
  K.GetProc(K.User,   "DispatchMessageW",       DispatchMessageW);       ASSERT(DispatchMessageW        # NIL);
  K.GetProc(K.User,   "DefWindowProcW",         DefWindowProcW);         ASSERT(DefWindowProcW          # NIL);
  K.GetProc(K.User,   "PostQuitMessage",        PostQuitMessage);        ASSERT(PostQuitMessage         # NIL);
  K.GetProc(K.User,   "BeginPaint",             BeginPaint);             ASSERT(BeginPaint              # NIL);
  K.GetProc(K.User,   "EndPaint",               EndPaint);               ASSERT(EndPaint                # NIL);
  K.GetProc(K.User,   "SetCapture",             SetCapture);             ASSERT(SetCapture              # NIL);
  K.GetProc(K.User,   "ReleaseCapture",         ReleaseCapture);         ASSERT(ReleaseCapture          # NIL);
  K.GetProc(K.User,   "MoveWindow",             MoveWindow);             ASSERT(MoveWindow              # NIL);
  K.GetProc(K.ShCore, "SetProcessDpiAwareness", SetProcessDpiAwareness); ASSERT(SetProcessDpiAwareness  # NIL);

  SetProcessDpiAwareness(2);  (* Disable DPI scaling *)
END Windows.
