MODULE TextFrames; (*JG 8.10.90 / NW 16.11.2015 / AP 30.12.20 Extended Oberon / DCWB 8May23 *)

IMPORT Windows, Display, Viewers, Fonts, Texts, Oberon, w := Writer, Input, SYSTEM;

CONST
  replace* = Texts.replace; (*message ids*)
  insert*  = Texts.insert;
  delete*  = Texts.delete;
  unmark*  = Texts.unmark;

  NUL*         = 00X;
  CtrlA*       = 01X;
  CtrlC*       = 03X;
  BS*          = 08X;
  TAB*         = 09X;
  LF*          = 0AX;
  CR*          = 0DX;
  CursorLeft*  = 11X;
  CursorRight* = 12X;
  CursorUp*    = 13X;
  CursorDown*  = 14X;
  CtrlV*       = 16X;
  CtrlX*       = 18X;
  SUB*         = 1AX;
  ESC*         = 1BX;
  SPC*         = 20X;
  DEL*         = 7FX;

TYPE
  ARGB = Display.ARGB;

  Line = POINTER TO LineDesc;
  LineDesc = RECORD
    length:   INTEGER;  (* Number of (UTF-8) CHAR positions          *)
    width:    INTEGER;  (* Line width in whole pixels                *)
    ascent:   INTEGER;  (* Largest font ascent in line               *)
    descent:  INTEGER;  (* largest font descent in line              *)
    height:   INTEGER;  (* largest ascent+descent+lead in line       *)
    baseline: INTEGER;  (* Y offset in frame determined by PlaceLine *)
    eot:      BOOLEAN;  (* If eof at end of line                     *)
    next:     Line
  END;

  Location* = RECORD
    org*:  INTEGER;  (* Set by LocateString, LocateChar, LocatePos, LocateLine *)
    line:  Line;     (* Set by LocateString, LocateChar, LocatePos, LocateLine *)
    y*:    INTEGER;  (* Set by LocateString, LocateChar, LocatePos, LocateLine *)
    pos*:  INTEGER;  (* Set by LocateString, LocateChar, LocatePos             *)
    x*:    INTEGER;  (* Set by LocateString, LocateChar, LocatePos             *)
    dx*:   INTEGER;  (* Set by LocateString, LocateChar                        *)
    rpos*: INTEGER   (* Set by LocateString                                    *)
  END;


  Frame* = POINTER TO FrameDesc;
  FrameDesc* = RECORD (Display.FrameDesc)
    text*:      Texts.Text;
    org*:       INTEGER;  (* pos in text for first character in current view  *)
    colour*:    ARGB;     (* background colour                                *)
    voff:       INTEGER;  (* current character vertical offset                *)

    lineHeight: INTEGER;  (* Overall vertical advance from line to line       *)
    baseline:   INTEGER;  (* Distance of baseline down from top of line space *)

    top*:       INTEGER;  (* Top margin                                       *)
    left*:      INTEGER;  (* Left margin                                      *)
    right*:     INTEGER;  (* Right margin                                     *)
    bottom*:    INTEGER;  (* Offset from top to start of bottom margin        *)

    headLine:   Line;     (* Sentinel for circular list of rendered lines     *)

    hasCaret*:  BOOLEAN;
    caretLoc*:  Location; (* Caret location                                   *)

    selStart:   INTEGER;  (* First selected character                         *)
    selLimit:   INTEGER;  (* First character beyond selection                 *)
    time*:      INTEGER;  (* Time of last selection                           *)

    trkStart:   INTEGER;  (* First tracked character                          *)
    trkLimit:   INTEGER;  (* First character beyond track                     *)

    tracktext:  BOOLEAN;  (* FALSE: tracking scroll op, TRUE: text            *)
    firstkey:   INTEGER;  (* initial mouse key press                          *)
    keysum:     SET;

    changeMark: Oberon.Sprite
  END;

  UpdateMsg* = RECORD (Display.FrameMsg)
    id*:   INTEGER;       (*replace, insert, delete, unmark*)
    text*: Texts.Text;
    beg*:  INTEGER;
    end*:  INTEGER
  END;

  CopyOverMsg* = RECORD (Display.FrameMsg)
    text*: Texts.Text;
    beg*:  INTEGER;
    end*:  INTEGER
  END;

VAR
  NextCh:     CHAR;

  (* Default frame metrics *)
  LineHeight:  INTEGER;
  Baseline:    INTEGER;
  EolWidth:    INTEGER;
  BarWidth:    INTEGER;  (* Size of scroll bar *)
  ThumbWidth:  INTEGER;
  MenuHeight*: INTEGER;
  Left*:       INTEGER;
  Right*:      INTEGER;
  Top*:        INTEGER;
  Bottom*:     INTEGER;

  TBuf*:  Texts.Buffer;
  DelBuf: Texts.Buffer;  (* Deleted text buffer *)
  KW:     Texts.Writer;  (* Keyboard writer used to prepare text to be inserted *)
  W:      Texts.Writer;  (* Writer for composing menus *)


PROCEDURE Min (i, j: INTEGER): INTEGER;
BEGIN IF i < j THEN j := i END;  RETURN j END Min;

PROCEDURE Max (i, j: INTEGER): INTEGER;
BEGIN IF i > j THEN j := i END;  RETURN j END Max;


(*------------------display support------------------------*)

PROCEDURE DrawThumb(F: Frame);
BEGIN
  IF (F.H > 0) & (F.left >= BarWidth) THEN
    Windows.FillRectangle(
      Display.Window.bmp,
      F.X + 3,  F.Y + Max(F.org * (F.H - 24) DIV (F.text.length + 1), 0) + 2,
      ThumbWidth,  20,
      0FFFFFFFFH
    );
  END
END DrawThumb;

PROCEDURE SetChangeMark (F: Frame;  on: BOOLEAN);  (*in corner*)
BEGIN
  IF F.H > MenuHeight THEN
    ASSERT(F.changeMark # NIL);
    IF on THEN
      Oberon.SetSpritePos(F.changeMark, F.X+F.W-12, F.Y+4);
      Oberon.EnableSprite(F.changeMark)
    ELSE
      Oberon.DisableSprite(F.changeMark)
    END
  END
END SetChangeMark;


PROCEDURE Width (VAR R: Texts.Reader;  length: INTEGER): INTEGER;
VAR
  pos: INTEGER;
  ox:  INTEGER;
BEGIN pos := 0;  ox := 0;
  WHILE pos < length DO
    INC(ox, Fonts.GetAdvance(R.fnt, ORD(NextCh)));
    INC(pos);
    Texts.Read(R, NextCh)
  END;
RETURN ox END Width;


PROCEDURE MeasureLine (L: Line;  VAR R: Texts.Reader);
VAR ascent, descent, below, length: INTEGER;
BEGIN
  ascent := 1;  descent := 0;  below := 0;  length := 0;
  WHILE (NextCh # CR) & (NextCh # LF) & (R.fnt # NIL) DO
    IF R.fnt.ascent  > ascent  THEN ascent  := R.fnt.ascent  END;
    IF R.fnt.descent > descent THEN descent := R.fnt.descent END;
    IF R.fnt.descent + R.fnt.lead > below THEN
      below := R.fnt.descent + R.fnt.lead
    END;
    INC(length);  Texts.Read(R, NextCh)
  END;
  L.length  := length + 1;
  L.ascent  := ascent;
  L.descent := descent;
  L.height  := ascent + below;
  L.eot     := R.fnt = NIL;
  IF NextCh = CR THEN
    Texts.Read(R, NextCh);
    IF NextCh = LF THEN Texts.Read(R, NextCh);  INC(L.length) END
  ELSE
    Texts.Read(R, NextCh)
  END
END MeasureLine;


PROCEDURE PlaceLine(f: Frame;  L: Line;  top: INTEGER);
(* Determines the first available baseline in the frame at which to place *)
(* line L such that the line's ascent does not reach higher than top.     *)
VAR linenum: INTEGER;
BEGIN
  linenum    := (top - f.top - 1) DIV f.lineHeight;
  L.baseline := f.top + (linenum + 1) * f.lineHeight + f.baseline
END PlaceLine;


PROCEDURE DisplayLine(  (*pass 2*)
  F:      Frame;
  L:      Line;
  VAR R:  Texts.Reader;
  X:      INTEGER;  (* In whole pixels relative to screen *)
  Y:      INTEGER;  (* In whole pixels, baseline will be placed at Y *)
  length: INTEGER   (* 0 unless displaying trailing part of line *)
);
VAR
  dx, xLim: INTEGER;
  glyph:    Fonts.Glyph;
BEGIN
  w.s("DisplayLine, X "); w.i(X);
  w.s(" Y "); w.i(Y);
  w.s(" R.col $"); w.h(R.col); w.s(" '");
  X    := X * 256;                      (* switch to subpixels *)
  xLim := (F.X + F.W - F.right) * 256;  (* x Limit in pixels   *)
  WHILE (NextCh # CR) & (NextCh # LF) & (R.fnt # NIL) DO
    w.c(NextCh);
    dx := Fonts.GetAdvance(R.fnt, ORD(NextCh));
    IF (NextCh >= 20X) & (NextCh <= 7EX) & ((X + dx) < xLim) THEN
      glyph := Fonts.GetGlyph(R.fnt, ORD(NextCh));
      IF glyph.map # 0 THEN
        Display.RenderAlphaMap(
          X DIV 64 + glyph.originX,  Y - glyph.baseline,
          glyph.mapWidth,            glyph.mapHeight,
          glyph.map,                 R.col)
      END
    END;
    INC(X, dx);
    INC(length);
    Texts.Read(R, NextCh)
  END;
  w.sl("'.");
  L.length := length + 1;
  L.width  := (X+255) DIV 256 + EolWidth - (F.X + F.left);
  L.eot    := R.fnt = NIL;
  IF NextCh = CR THEN
    Texts.Read(R, NextCh);
    IF NextCh = LF THEN Texts.Read(R, NextCh);  INC(L.length) END
  ELSE
    Texts.Read(R, NextCh)
  END
END DisplayLine;


PROCEDURE DisplaySelectionBackground(  (*pass 2*)
  F:       Frame;
  L:       Line;
  VAR R:   Texts.Reader;
  X:       INTEGER;  (* In whole pixels relative to screen            *)
  Y:       INTEGER;  (* In whole pixels, baseline will be placed at Y *)
  length:  INTEGER   (* 0 unless displaying trailing part of line     *)
);
VAR
  pos:   INTEGER;
  xLim:  INTEGER;
  selx:  INTEGER;  (* Left edge of selection *)
  seldx: INTEGER;  (* Width of selection     *)
  trkx:  INTEGER;  (* Left edge of tracking  *)
  trkdx: INTEGER;  (* Width of tracking      *)
BEGIN
  w.s("DisplaySelectionBackground, X "); w.i(X);
  w.s(" Y ");          w.i(Y);
  w.s(" F.X ");        w.i(F.X);
  w.s(" F.Y ");        w.i(F.Y);
  w.s(" L.baseline "); w.i(L.baseline);
  w.s(" L.ascent ");   w.i(L.ascent);
  w.s(" L.height ");   w.i(L.height);
  w.s(" L.width ");    w.i(L.width);
  w.l;
  w.s("  on line from "); w.i(Texts.Pos(R)-1);
  w.s(" to ");            w.i(Texts.Pos(R)-1 + L.length);
  w.s(" selStart ");      w.i(F.selStart);
  w.s(" selLimit ");      w.i(F.selLimit);
  w.sl(".");

  pos  := Texts.Pos(R) - 1;
  X    := X * 256;                      (* switch to subpixels *)
  xLim := (F.X + F.W - F.right) * 256;  (* x Limit in pixels   *)
  selx := X;  seldx := 0;
  trkx := X;  trkdx := 0;

  WHILE (NextCh # CR) & (NextCh # LF) & (R.fnt # NIL) DO
    IF (pos = F.selStart) THEN selx  := X        END;
    IF (pos = F.selLimit) THEN seldx := X - selx END;
    IF (pos = F.trkStart) THEN trkx  := X        END;
    IF (pos = F.trkLimit) THEN trkdx := X - trkx END;
    INC(X, Fonts.GetAdvance(R.fnt, ORD(NextCh)));
    INC(pos); INC(length);
    Texts.Read(R, NextCh)
  END;

  IF F.selLimit > pos THEN seldx := X - selx + 256 * EolWidth END;
  IF seldx > 0 THEN
    Display.FillRectangle(
      selx DIV 256,  F.Y + L.baseline - L.ascent,
      (seldx + 255) DIV 256, L.height,
      0FF606060H)
  END;

  IF F.trkLimit > pos THEN trkdx := X - trkx + 256 * EolWidth END;
  IF trkdx > 0 THEN
    Display.FillRectangle(
      trkx DIV 256,  F.Y + L.baseline + 1,
      (trkdx + 255) DIV 256, 4,
      0FFE0E0E0H)
  END;

  L.length := length + 1;
  L.width  := (X+255) DIV 256 + EolWidth - (F.X + F.left);
  L.eot    := R.fnt = NIL;
  IF NextCh = CR THEN
    Texts.Read(R, NextCh);
    IF NextCh = LF THEN Texts.Read(R, NextCh);  INC(L.length) END
  ELSE
    Texts.Read(R, NextCh)
  END
END DisplaySelectionBackground;


PROCEDURE EnsureSOL(T: Texts.Text;  VAR pos: INTEGER);
(* Ensure pos at start of line *)
VAR R: Texts.Reader;
BEGIN
  w.s("EnsureSOL. pos "); w.i(pos); w.s(" T.length "); w.i(T.length); w.sl(".");
  IF pos > T.length THEN pos := T.length
  ELSIF pos > 0 THEN
    DEC(pos);
    Texts.OpenReader(R, T, pos);
    REPEAT Texts.Read(R, NextCh);  INC(pos)
    UNTIL R.eot OR (NextCh = CR) OR (NextCh = LF);
    w.s("EnsureSOL found NextCh = $"); w.h(ORD(NextCh)); w.sl(".");
    IF NextCh = CR THEN
      Texts.Read(R, NextCh); w.sl("-- found CR --.");
      IF NextCh = LF THEN w.sl("-- Skip LF --.");  INC(pos) END
    END
  ELSE pos := 0
  END
END EnsureSOL;


(*------------------frame modifiers------------------------*)

PROCEDURE Render* (F: Frame);
VAR
  R:        Texts.Reader;
  line:     Line;
  prevl:    Line;
  org:      INTEGER;
  top:      INTEGER;
  bottom:   INTEGER;
  y:        INTEGER;  (* current top of available space for next line    *)
  baseline: INTEGER;  (* evaluated baseline position for line            *)
BEGIN
  w.s("Render frame at "); Display.wfpos(F);
  w.s(" top "); w.i(F.top); w.s(" bottom "); w.i(F.bottom); w.sl(".");

  Display.FillRectangle(F.X, F.Y, F.W, F.H, F.colour);
  Windows.InvalidateRect(Display.Window, F.X, F.Y, F.W, F.H);

  IF F.left >= BarWidth THEN  (* white line between left side bar and content *)
    Display.FillRectangle(F.X + BarWidth - 1, F.Y, 1, F.H, Display.White)
  END;

  EnsureSOL(F.text, F.org);
  top     := F.top;
  bottom  := F.H - F.bottom;
  prevl   := F.headLine;

  IF top < bottom THEN
    y   := top;
    org := F.org;
    Texts.OpenReader(R, F.text, org);
    Texts.Read(R, NextCh);
    WHILE ~prevl.eot & (y < bottom) DO
      w.s("  restore line at org "); w.i(org); w.sl(".");
      NEW(line);
      MeasureLine(line, R);
      PlaceLine(F, line, y);
      y := line.baseline + line.descent;
      w.s("  y "); w.i(y); w.s(", bottom "); w.i(bottom);
      w.s(", selStart "); w.i(F.selStart);
      w.s(", selLimit "); w.i(F.selLimit);
      w.sl(".");
      IF y <= bottom THEN
        IF (F.selStart < org + line.length) & (F.selLimit > org)
        OR (F.trkStart < org + line.length) & (F.trkLimit > org) THEN
          Texts.OpenReader(R, F.text, org);  (* Reset reader to start of line for display *)
          Texts.Read(R, NextCh);
          DisplaySelectionBackground(F, line, R, F.X + F.left, F.Y + line.baseline, 0)
        END;
        Texts.OpenReader(R, F.text, org);  (* Reset reader to start of line for display *)
        Texts.Read(R, NextCh);
        DisplayLine(F, line, R, F.X + F.left, F.Y + line.baseline, 0)
      END;
      prevl.next := line;
      prevl      := line;
      org        := org + line.length;
    END
  END;
  prevl.next := F.headLine;
  DrawThumb(F)
END Render;


PROCEDURE Show* (F: Frame;  pos: INTEGER);
BEGIN F.org := pos;  Render(F) END Show;



(* ------------------- Location finders within line cache ------------------- *)

PROCEDURE LocateLine (F: Frame;  y: INTEGER;  VAR loc: Location);
VAR
  line: Line;
  org:  INTEGER;
BEGIN
  org  := F.org;
  line := F.headLine.next;
  WHILE (line.next # F.headLine) & (line.baseline - line.ascent + line.height < y) DO
    org := org + line.length;  line := line.next
  END;
  loc.org  := org;
  loc.line := line;
  loc.y    := line.baseline
END LocateLine;


(* Locate a blank delimited string *)
PROCEDURE LocateString (F: Frame;  x, y: INTEGER;  VAR loc: Location);
VAR
  R:     Texts.Reader;
  lim:   INTEGER;
  glyph: Fonts.Glyph;
  pos:   INTEGER;
  lpos:  INTEGER;     (* char position at left end of non-blank run *)
  rpos:  INTEGER;     (* char position at right end of non-blank run *)
  lx:    INTEGER;     (* subpixel position at left end of non-blank run *)
  dx:    INTEGER;     (* subpixel count of non-blank run *)
  rx:    INTEGER;     (* subpixel position at right end of run *)
BEGIN
  LocateLine(F, y, loc);
  lim  := loc.org + loc.line.length - 1;
  lx   := F.left * 256;  (* horizontal offsets are in 256ths os a pixel *)
  pos  := loc.org;
  rx   := lx;
  lpos := pos;
  dx   := 0;
  Texts.OpenReader(R, F.text, loc.org);
  Texts.Read(R, NextCh);
  REPEAT
    WHILE (pos < lim) & (NextCh > " ") DO  (* Scan to next space *)
      INC(rx, Fonts.GetAdvance(R.fnt, ORD(NextCh)));
      INC(pos);
      Texts.Read(R, NextCh)
    END;
    dx := rx - lx;  rpos := pos;
    WHILE (pos < lim) & (NextCh <= " ") DO  (* Scan spaces *)
      INC(rx, Fonts.GetAdvance(R.fnt, ORD(NextCh)));
      INC(pos);
      Texts.Read(R, NextCh)
    END;
    (* lx - left edge of non-blank run                      *)
    (* dx - width of non-blank run                          *)
    (* rx - right edge of blank run following non-blank run *)
    IF (rx DIV 256 < x) & (pos < lim) THEN
      w.sl(".. next token.");
      lpos := pos;  lx := rx
    END
  UNTIL (rx DIV 256 >= x) OR (pos >= lim);
  loc.pos  := lpos;
  loc.rpos := rpos;
  loc.x    := (lx + 255) DIV 256;
  loc.dx   := (dx + 255) DIV 256;
  w.s("LocateString line org "); w.i(loc.org);
  w.s(", pos ");  w.i(loc.pos);
  w.s(", rpos "); w.i(loc.rpos);
  w.s(", x ");    w.i(loc.x);
  w.s(", dx ");   w.i(loc.dx); w.sl(".");
END LocateString;


PROCEDURE LocateChar (F: Frame;  x, y: INTEGER;  VAR loc: Location);
VAR
  R:   Texts.Reader;
  pos: INTEGER;  (* current position *)
  lim: INTEGER;  (* limit position   *)
  ox:  INTEGER;  (* character origin *)
  dx:  INTEGER;  (* character width  *)
BEGIN
  w.s("TextFrames.LocateChar at "); w.i(x); w.c(","); w.i(y); w.sl(".");
  LocateLine(F, y, loc);
  lim := loc.org + loc.line.length - 1;
  pos := loc.org;
  ox  := F.left * 256;
  dx  := EolWidth * 256;
  w.s("  loc.org "); w.i(loc.org); w.s(", line length "); w.i(loc.line.length); w.sl(".");
  Texts.OpenReader(R, F.text, loc.org);
  WHILE pos < lim DO
    Texts.Read(R, NextCh);
    dx := Fonts.GetAdvance(R.fnt, ORD(NextCh));
    w.s("  ox "); w.i(ox); w.s(", dx "); w.i(dx); w.s(", x"); w.i(x); w.sl(".");
    IF (ox + dx) DIV 256 <= x THEN
      INC(pos);  INC(ox, dx);
      IF pos = lim THEN dx := EolWidth END
    ELSE lim := pos
    END
  END;
  w.s("  found at pos "); w.i(pos); w.s(", ox "); w.i(ox); w.sl(".");
  loc.pos := pos;
  loc.x   := ox DIV 256;
  loc.dx  := dx DIV 256
END LocateChar;


PROCEDURE LocatePos (F: Frame;  pos: INTEGER;  VAR loc: Location);
VAR
  T:   Texts.Text;
  R:   Texts.Reader;
  L:   Line;
  org: INTEGER;
BEGIN
  T    := F.text;
  org  := F.org;
  L    := F.headLine.next;
  IF pos < org THEN pos := org END;
  WHILE (L.next # F.headLine) & (pos >= org + L.length) DO
    org  := org + L.length;
    L    := L.next
  END;
  IF pos >= org + L.length THEN pos := org + L.length - 1 END;
  loc.org  := org;
  loc.pos  := pos;
  loc.line := L;
  Texts.OpenReader(R, T, org);  Texts.Read(R, NextCh);
  loc.x  := F.left + Width(R, pos - org) DIV 256;
  loc.y  := L.baseline
END LocatePos;


PROCEDURE Pos* (F: Frame;  X, Y: INTEGER): INTEGER;
VAR loc: Location;
BEGIN LocateChar(F,  X - F.X,  Y - F.Y,  loc);
RETURN loc.pos END Pos;


(*------------------caret, selection, mouse tracking------------------------*)

PROCEDURE ShowCaret (F: Frame);
BEGIN
  w.s("TextFrames.ShowCaret for frame $"); w.h(SYSTEM.ADR(F^));
  w.s(", caretLoc "); w.i(F.caretLoc.x); w.c(","); w.i(F.caretLoc.y);
  w.s(", frame width "); w.i(F.W);
  w.sl(".");
  IF (F.caretLoc.x < F.W) & (F.caretLoc.y >= 10) & (F.caretLoc.x + 12 < F.W) THEN
    w.s("  SetHookPos "); w.i(F.X + F.caretLoc.x); w.c(","); w.i(F.Y + F.caretLoc.y - 10); w.sl(".");
    Oberon.SetHookPos(F.X + F.caretLoc.x,  F.Y + F.caretLoc.y)
  END;
  F.hasCaret := TRUE
END ShowCaret;

PROCEDURE SetCaretPos* (F: Frame;  pos: INTEGER);
BEGIN LocatePos(F, pos, F.caretLoc);  ShowCaret(F);
END SetCaretPos;


(*------------------text modifiers------------------------*)


PROCEDURE Recall*(VAR B: Texts.Buffer);
BEGIN B := TBuf;  NEW(TBuf);  Texts.OpenBuf(TBuf)
END Recall;


(*------------------message handling------------------------*)


PROCEDURE NotifyDisplay* (T: Texts.Text;  op: INTEGER;  beg, end: INTEGER);
VAR M: UpdateMsg;
BEGIN
  w.sl("NotifyDisplay.");
  M.id := op;  M.text := T;  M.beg := beg;  M.end := end;  Viewers.Broadcast(M)
END NotifyDisplay;


PROCEDURE Call* (F: Frame;  pos: INTEGER);
BEGIN Oberon.Call(F, F.text, pos) END Call;


PROCEDURE Write* (F: Frame;  ch: CHAR;  fnt: Fonts.Font;  col, voff: INTEGER);
VAR buf: Texts.Buffer;
BEGIN ASSERT(F.hasCaret); w.sl("TextFrames.Write ch $"); w.h(ORD(ch)); w.sl(".");
  CASE ch OF
  |BS:          IF F.caretLoc.pos > F.org THEN                               (* Backspace *)
                  Texts.Delete(F.text, F.caretLoc.pos - 1, F.caretLoc.pos, DelBuf);
                  SetCaretPos(F, F.caretLoc.pos - 1)
                END
  |CtrlA:       F.selStart := 0;  F.selLimit := F.text.length;               (* Select all *)
                F.time := Oberon.Time();
                Render(F);
  |CtrlC:       IF F.selStart < F.selLimit THEN                              (* Copy *)
                  NEW(TBuf);  Texts.OpenBuf(TBuf);
                  Texts.Save(F.text, F.selStart, F.selLimit, TBuf)
                END
  |CtrlX:       IF F.selStart < F.selLimit THEN                              (* Cut *)
                  NEW(TBuf);  Texts.OpenBuf(TBuf);
                  w.sl("Ctrl/X delete.");
                  Texts.Delete(F.text, F.selStart, F.selLimit, TBuf);
                  Render(F)
                END
  |CtrlV:       NEW(buf);  Texts.OpenBuf(buf);  Texts.Copy(TBuf, buf);       (* Paste *)
                Texts.Insert(F.text, F.caretLoc.pos, buf);
                SetCaretPos(F, F.caretLoc.pos + TBuf.length);
                Render(F)
  |CursorLeft:  IF F.caretLoc.pos > 0 THEN
                  SetCaretPos(F, F.caretLoc.pos - 1)
                END
  |CursorRight: IF F.caretLoc.pos < F.text.length THEN
                  SetCaretPos(F, F.caretLoc.pos + 1)
                END
  |CursorUp:    SetCaretPos(F, Pos(F, F.X + F.caretLoc.x,
                                   F.Y + F.caretLoc.y - F.lineHeight))
  |CursorDown:  SetCaretPos(F, Pos(F, F.X + F.caretLoc.x,
                                   F.Y + F.caretLoc.y + F.lineHeight))
  |CR, TAB,
   " "..DEL:    KW.fnt := fnt;  KW.col := col;  KW.voff := voff;
                Texts.Write(KW, ch);
                Texts.Insert(F.text, F.caretLoc.pos, KW.buf);
                SetCaretPos(F, F.caretLoc.pos + 1)
  END
END Write;


PROCEDURE Modify* (F: Frame;  id, Y, H: INTEGER);
VAR dY: INTEGER;
BEGIN
  SetChangeMark(F, FALSE);
  IF    id = Viewers.restore THEN Render(F)
  ELSIF id = Viewers.modify  THEN Render(F)
  END;
END Modify;


PROCEDURE Open* (F: Frame;  H: Display.Handler;  T: Texts.Text;
                 org,  col,   voff:        INTEGER;
                 left, right, top, bottom: INTEGER;
                 lineHeight,  baseline:    INTEGER);
VAR l: Line;
BEGIN
  w.s("TextFrames.Open l "); w.i(left);
  w.s(", r "); w.i(right);
  w.s(", t "); w.i(top);
  w.s(", b "); w.i(bottom);
  w.sl(".");


  NEW(l);  (* Create a headLine line for a closed list of lines *)
  l.length     := 0;        l.width      := 0;
  l.eot        := FALSE;    l.next       := l;
  l.ascent     := 1;        l.descent    := 0;
  l.height     := 1;

  F.headLine   := l;        F.org        := org;
  F.handle     := H;        F.text       := T;
  F.baseline   := baseline; F.lineHeight := lineHeight;
  F.colour     := col;      F.voff       := voff;
  F.top        := top;      F.left       := left;
  F.right      := right;    F.bottom     := bottom;
  F.hasCaret   := FALSE;    F.time       := 0;
  F.selStart   := 0;        F.selLimit   := 0;
  F.trkStart   := 0;        F.trkLimit   := 0;

  F.tracktext  := FALSE;
  F.firstkey   := -1;        F.keysum   := {};

  NEW(F.changeMark); Oberon.SetSpriteGlyph(F.changeMark, Oberon.BlockGlyph);
END Open;


PROCEDURE Copy* (F: Frame;  VAR F1: Frame);
BEGIN NEW(F1);
  Open(F1, F.handle, F.text,    F.org, F.colour, F.voff,
           F.left,   F.right,   F.top, F.bottom,
           F.lineHeight,        F.baseline)
END Copy;


PROCEDURE CopyOver(F: Frame;  text: Texts.Text;  beg, end: INTEGER);
VAR buf: Texts.Buffer;
BEGIN
  IF F.hasCaret THEN
    NEW(buf);  Texts.OpenBuf(buf);
    Texts.Save(text, beg, end, buf);  Texts.Insert(F.text, F.caretLoc.pos, buf);
    SetCaretPos(F, F.caretLoc.pos + (end - beg))
  END
END CopyOver;


PROCEDURE GetSelection* (F: Frame;  VAR text: Texts.Text;  VAR beg, end, time: INTEGER);
BEGIN
  IF F.selStart < F.selLimit THEN
    IF F.text = text THEN
      IF F.selStart < beg THEN beg := F.selStart END;  (*leftmost*)
      IF F.time > time THEN end := F.selLimit;  time := F.time END  (*last selected*)
    ELSIF F.time > time THEN
      text := F.text;  beg := F.selStart;  end := F.selLimit;  time := F.time
    END
  END
END GetSelection;

PROCEDURE RemoveSelection*(F: Frame);
BEGIN F.selStart := 0;  F.selLimit := 0;  F.trkStart := 0;  F.trkLimit := 0
END RemoveSelection;


PROCEDURE UpdatePosition(anchor, deleted, inserted: INTEGER; VAR pos: INTEGER);
BEGIN  (* Update pos corresponding to insertion and/or deletion at anchor *)
  IF anchor < pos THEN
    IF pos < anchor + deleted THEN pos := anchor ELSE DEC(pos, deleted) END;
    INC(pos, inserted)
  END
END UpdatePosition;


PROCEDURE UpdatePositions(F: Frame;  anchor, deleted, inserted: INTEGER);
BEGIN
  UpdatePosition(anchor, deleted, inserted, F.selStart);
  UpdatePosition(anchor, deleted, inserted, F.selLimit);
  UpdatePosition(anchor, deleted, inserted, F.org);
END UpdatePositions;


PROCEDURE Update* (F: Frame;  VAR M: UpdateMsg);
BEGIN
  w.sl("TextFrames.Update.");
  SetChangeMark(F, FALSE);
  Oberon.RemoveMarks(F.X, F.Y, F.W, F.H);
  IF    M.id = replace THEN UpdatePositions(F, M.beg, M.end - M.beg, M.end - M.beg)
  ELSIF M.id = insert  THEN UpdatePositions(F, M.beg, 0,             M.end - M.beg)
  ELSIF M.id = delete  THEN UpdatePositions(F, M.beg, M.end - M.beg, 0)
  END;
  SetChangeMark(F, F.text.changed);
  F.headLine.next := F.headLine;  (* Free all cached lines *)
  IF M.end > F.org THEN Render(F) END
END Update;

(* -------------------------------------------------------------------------- *)


(*  Scroll handling                                                           *)
(*                                                                            *)
(*  First key  Added key  tracking             complete                       *)
(*  ---------  ---------  --------------       --------------                 *)
(*  left       none       position thumb                                      *)
(*  middle     none                            go to top                      *)
(*  middle     right                           go to bottom                   *)
(*  right      none       highlight line       line to top                    *)
(*  right      left       highlight line       line downwards                 *)

PROCEDURE TrackMouseUnderway(F: Frame; X, Y: INTEGER);
VAR loc: Location;
BEGIN
  IF F.tracktext THEN
    CASE F.firstkey OF
    |0: (* Right mouse  *)
        LocateChar(F, X - F.X, Y - F.Y, loc);
        IF    loc.pos <  F.selStart THEN F.selStart := loc.pos
        ELSIF loc.pos >= F.selLimit THEN F.selLimit := loc.pos + 1
        END;
        Render(F)
    |1: (* Middle mouse *)
        LocateString(F, X - F.X, Y - F.Y, loc);
        F.trkStart := loc.pos;  F.trkLimit := loc.rpos;
        Render(F)
    |2: (* Left mouse   *)
        LocateChar(F, X - F.X, Y - F.Y, F.caretLoc);
        ShowCaret(F)
    END
  ELSE
    Oberon.SetMouseGlyph(Oberon.UpdownGlyph);
    CASE F.firstkey OF
    |0: (* Right mouse - show tracked line *)
        LocateLine(F, Y - F.Y, loc);
        F.trkStart := loc.org;  F.trkLimit := loc.org + loc.line.length;
        Render(F)
    |2: (* Left mouse - scroll immediately to thumb proportion *)
        Show(F, (Y - F.Y) * F.text.length DIV F.H);
    END
  END
END TrackMouseUnderway;


PROCEDURE TrackMouseStart(F: Frame; X, Y: INTEGER);
VAR loc: Location;
BEGIN
  IF F.tracktext THEN
    CASE F.firstkey OF (* Record initial selection character *)
    |0: LocateChar(F, X - F.X, Y - F.Y, loc);
        F.selStart := loc.pos;  F.selLimit := loc.pos + 1;
        Render(F)
    |2: Oberon.PassFocus(Viewers.This(F.X, F.Y));
    END
  END;
  TrackMouseUnderway(F, X, Y)
END TrackMouseStart;


PROCEDURE TrackMouseComplete(F: Frame; X, Y: INTEGER);
VAR
  text: Texts.Text;
  buf:  Texts.Buffer;
  beg:  INTEGER;
  end:  INTEGER;
  time: INTEGER;
  fnt:  Fonts.Font;
  col:  INTEGER;
  voff: INTEGER;
BEGIN
  IF F.tracktext THEN
    CASE F.firstkey OF
      |1: (* Middle mouse - execute selected command *)
          IF F.keysum = {1} THEN Call(F, F.trkStart) END;
          F.trkStart := 0;  F.trkLimit := 0;
      |2: (* Left mouse - paste selection or looks if middle or right added *)
          IF F.keysum = {1,2} THEN (* left+middle: copy selection to caret *)
            Oberon.GetSelection(text, beg, end, time);
            IF time >= 0 THEN
              NEW(TBuf); Texts.OpenBuf(TBuf);
              Texts.Save(text, beg, end, TBuf);
              Texts.Insert(F.text, F.caretLoc.pos, TBuf);
              F.selStart := F.caretLoc.pos;  F.selLimit := F.caretLoc.pos + (end  - beg);
              SetCaretPos(F, F.caretLoc.pos + (end - beg))
            ELSIF TBuf # NIL THEN
              NEW(buf); Texts.OpenBuf(buf);
              Texts.Copy(TBuf, buf);
              Texts.Insert(F.text, F.caretLoc.pos, buf);
              SetCaretPos(F, F.caretLoc.pos + buf.length)
            END
          ELSIF F.keysum = {0,2} THEN (* left+right: copy looks to caret *)
            Oberon.GetSelection(text, beg, end, time);
            IF time >= 0 THEN
              Texts.Attributes(F.text, F.caretLoc.pos, fnt, col, voff);
              IF fnt # NIL THEN Texts.ChangeLooks(text, beg, end, {0,1,2}, fnt, col, voff) END
            END
          END
    END
  ELSE
    CASE F.firstkey OF
    |0: (* Right mouse - scroll tracked line up of down *)
        IF ~(2 IN F.keysum) THEN  (* Scroll line to top *)
          F.org := F.trkStart - 1
        ELSE                      (* Scroll line downwards *)
          F.org := F.org - (F.trkStart - F.org) - 100
        END;
        F.trkStart := 0;  F.trkLimit := 0;
        Render(F)
    |1: (* Middle mouse - scroll to top or bottom of file *)
        IF F.keysum = {1} THEN
          F.org := 0
        ELSE
          F.org := F.text.length - 100
        END;
        Render(F)
    END
  END
END TrackMouseComplete;


PROCEDURE Edit* (F: Frame;  X, Y: INTEGER;  Keys: SET);
BEGIN
  IF X < F.X + Min(F.left, BarWidth) THEN Oberon.SetMouseGlyph(Oberon.UpdownGlyph) END;
  IF Keys # {} THEN
    IF F.firstkey < 0 THEN (* Begin tracking *)
      F.tracktext := X >= F.X + Min(F.left, BarWidth);
      IF    0 IN Keys THEN F.firstkey := 0
      ELSIF 1 IN Keys THEN F.firstkey := 1
      ELSE F.firstkey := 2 END;
      F.keysum    := Keys;
      TrackMouseStart(F, X, Y)
    ELSE (* Continue tracking *)
      F.keysum := F.keysum + Keys;
      TrackMouseUnderway(F, X, Y)
    END;
  ELSIF F.keysum # {} THEN (* End tracking *)
    TrackMouseComplete(F, X, Y);
    F.firstkey := -1;  F.keysum := {};
  END
END Edit;


PROCEDURE Handle* (F: Display.Frame;  VAR M: Display.FrameMsg);
VAR F1: Frame;
BEGIN
  w.sl("TextFrames.Handle:");
  CASE F OF Frame:
    CASE M OF
    Oberon.InputMsg:     w.s("  Oberon.InputMsg id "); w.i(M.id); w.sl(".");
                         IF    M.id = Oberon.track THEN Edit(F, M.X, M.Y, M.keys)
                         ELSIF M.id = Oberon.consume THEN
                           IF F.hasCaret THEN Write(F, M.ch, M.fnt, M.col, M.voff) END
                         END |
    Oberon.ControlMsg:   w.sl("  Oberon.ControlMsg");
                         IF    M.id = Oberon.defocus    THEN (*Defocus(F)   *)
                         ELSIF M.id = Oberon.neutralize THEN (*Neutralize(F)*)
                         END |
    Oberon.SelectionMsg: w.sl("  Oberon.SelectionMsg");
                         GetSelection(F, M.text, M.beg, M.end, M.time) |
    Oberon.CopyMsg:      w.sl("  Oberon.CopyMsg");
                         Copy(F, F1);  M.F := F1 |
    Viewers.ViewerMsg:   w.sl("  Viewers.ViewerMsg");
                         Modify(F, M.id, M.Y, M.H) |
    CopyOverMsg:         w.sl("  CopyOverMsg");
                         CopyOver(F, M.text, M.beg, M.end) |
    UpdateMsg:           w.sl("  UpdateMsg");
                         IF F.text = M.text THEN Update(F, M) END
    END
  END;
END Handle;


(* --------------------------- TextFrame creation --------------------------- *)

PROCEDURE Menu (name, commands: ARRAY OF CHAR): Texts.Text;
VAR T: Texts.Text;
BEGIN NEW(T);
  T.notify := NotifyDisplay;
  Texts.Open(T, "");
  Texts.WriteString(W, name);
  Texts.WriteString(W, " | ");
  Texts.WriteString(W, commands);
  Texts.Append(T, W.buf);
RETURN T END Menu;

PROCEDURE Text* (name: ARRAY OF CHAR): Texts.Text;
VAR T: Texts.Text;
BEGIN NEW(T);  T.notify := NotifyDisplay;  Texts.Open(T, name);
RETURN T END Text;

PROCEDURE NewMenu* (name, commands: ARRAY OF CHAR): Frame;
VAR F: Frame;  T: Texts.Text;
BEGIN NEW(F);  T := Menu(name, commands);
  Open(F, Handle, T,
       0, Display.White, 0,
       Left DIV 4, 0, 0, 0,
       LineHeight, Baseline);
RETURN F END NewMenu;

PROCEDURE NewText* (text: Texts.Text;  pos: INTEGER): Frame;
VAR F: Frame;
BEGIN NEW(F);
  Open(F, Handle, text,
       pos, Display.Black, 0,
       Left, Right, Top, Bottom,
       LineHeight, Baseline);
RETURN F END NewText;

BEGIN
  w.sl("TextFrames initialising.");
  NEW(TBuf);    Texts.OpenBuf(TBuf);
  NEW(DelBuf);  Texts.OpenBuf(DelBuf);

  (* Set default frame metrics *)
  LineHeight := Fonts.Default.ascent + Fonts.Default.descent + Fonts.Default.lead;
  Baseline   := Fonts.Default.ascent;
  EolWidth   := (Fonts.Default.ascent + Fonts.Default.descent) DIV 2;
  BarWidth   := Fonts.Default.ascent + Fonts.Default.descent;
  ThumbWidth := BarWidth - 7;
  MenuHeight := Fonts.Default.ascent + Fonts.Default.descent + 1;
  Left       := BarWidth + (Fonts.Default.ascent + Fonts.Default.descent) DIV 2;
  Right      := (Fonts.Default.ascent + Fonts.Default.descent) DIV 2;
  Top        := (Fonts.Default.ascent + Fonts.Default.descent) DIV 2;
  Bottom     := (Fonts.Default.ascent + Fonts.Default.descent) DIV 2;

  Texts.OpenWriter(W);  Texts.SetColor(W,  Display.Black);  (* For composing menus *)
  Texts.OpenWriter(KW); Texts.SetColor(KW, Display.White);  (* For keyboard entry  *)
  w.sl("TextFrames initialised.");
  w.s("BarWidth "); w.i(BarWidth);
  w.s(", EolWidth "); w.i(EolWidth);
  w.sl(".");
END TextFrames.


