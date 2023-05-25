MODULE TextFrames; (*JG 8.10.90 / NW 16.11.2015 / AP 30.12.20 Extended Oberon / DCWB 8May23 *)

IMPORT Input, Display, Viewers, Fonts, Texts, Oberon, w := Writer, SYSTEM;

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
    lead:     INTEGER;  (* largest font lead in line                 *)
    baseline: INTEGER;  (* Y offset in frame determined by PlaceLine *)
    eot:      BOOLEAN;  (* If eof at end of line                     *)
    next:     Line
  END;

  Location* = RECORD
    org*:  INTEGER;  (* Set by LocateChar, LocateString, LocatePos, LocateLine *)
    line:  Line;     (* Set by LocateChar, LocateString, LocatePos, LocateLine *)
    y*:    INTEGER;  (* Set by LocateChar, LocateString, LocatePos, LocateLine *)
    pos*:  INTEGER;  (* Set by LocateChar, LocateString, LocatePos             *)
    x*:    INTEGER;  (* Set by LocateChar, LocateString, LocatePos             *)
    dx*:   INTEGER   (* Set by LocateChar, LocateString                        *)
  END;

  Frame* = POINTER TO FrameDesc;
  FrameDesc* = RECORD (Display.FrameDesc)
    text*:      Texts.Text;
    org*:       INTEGER;  (* pos in text for first character in current view *)
    colour*:    ARGB;     (* background colour                               *)
    voff:       INTEGER;  (* current character vertical offset               *)

    (* The line height is represented by three components                    *)
    ascent:     INTEGER;  (* Baseline offset within line space               *)
    descent:    INTEGER;  (* Common extension of characters below baseline   *)
    lead:       INTEGER;  (* Recommended extra space between lines           *)

    top*:       INTEGER;  (* Top margin                                      *)
    left*:      INTEGER;  (* Left margin                                     *)
    right*:     INTEGER;  (* Right margin                                    *)
    bottom*:    INTEGER;  (* Offset from top to start of bottom margin       *)

    thumb*:     INTEGER;  (* Scroll thumb vertical offset                    *)
    sentinel:   Line;     (* Sentinel for circular list of rendered lines    *)

    hasCaret*:  BOOLEAN;
    caretLoc*:  Location; (* Caret location                                  *)

    hasSel*:    BOOLEAN;
    selbeg*:    Location;
    selend*:    Location;
    time*:      INTEGER;  (* Time of last selection                          *)

    changeMark: Oberon.Sprite;
    thumbMark:  Oberon.Sprite

    (*
    markH*:        INTEGER;  (* thumb                                           *)
    hasMark:       BOOLEAN;  (*                                                 *)
    hasChangeMark: BOOLEAN;  (*                                                 *)
    pool:          Line      (* line pool to minimize heap space consumption during continuous scrolling *)
    *)
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
  Ascent:      INTEGER;  (* default ascent = position of baseline *)
  Descent:     INTEGER;
  Lead:        INTEGER;
  EolWidth:    INTEGER;
  BarWidth:    INTEGER;  (* Size of scroll bar *)
  MenuHeight*: INTEGER;
  Left*:       INTEGER;
  Right*:      INTEGER;
  Top*:        INTEGER;
  Bottom*:     INTEGER;

  TBuf*:  Texts.Buffer;
  DelBuf: Texts.Buffer;  (* Deleted text buffer *)
  KW:     Texts.Writer;  (* Keyboard writer used to prepare text to be inserted *)
  W:      Texts.Writer;  (* Writer for composing menus *)


(*
  selH:         INTEGER;
  markW:        INTEGER;
  ScrollMarker: Oberon.Marker;
*)

PROCEDURE Min (i, j: INTEGER): INTEGER;
BEGIN IF i < j THEN j := i END;  RETURN j END Min;

PROCEDURE Max (i, j: INTEGER): INTEGER;
BEGIN IF i > j THEN j := i END;  RETURN j END Max;

PROCEDURE NewLine(F: Frame;  VAR L: Line);  (*reuse line from line pool if possible*)
BEGIN
  (*IF F.pool # NIL THEN L := F.pool;  F.pool := L.next ELSE*) NEW(L) (*END*)
END NewLine;

(*
PROCEDURE PoolLines (F: Frame;  L, L1: Line);  (*move lines from L.next to L1 to line pool*)
VAR l: Line;
BEGIN IF L # L1 THEN l := L1.next;  L1.next := F.pool;  F.pool := L.next;  L.next := l END
END PoolLines;

PROCEDURE LastLine (F: Frame;  L: Line);  (*move all lines after L to line pool*)
VAR l: Line;
BEGIN (*L in closed F.sentinel ring*) l := L;
  WHILE l.next # F.sentinel DO l := l.next END;
  IF l # L THEN l.next := F.pool;  F.pool := L.next;  L.next := F.sentinel END
END LastLine;
*)

(*------------------display support------------------------*)

PROCEDURE MarkThumb* (F: Frame;  on: BOOLEAN);  (*in scroll bar*)
BEGIN
  IF (F.H > 0) & (F.left >= BarWidth) THEN
    IF on THEN
      Oberon.SetSpritePos(F.thumbMark, F.X + 3, F.Y + F.thumb + 2);
      Oberon.EnableSprite(F.thumbMark)
    ELSE
      Oberon.DisableSprite(F.thumbMark)
    END
  END
END MarkThumb;


PROCEDURE UpdateThumb (F: Frame);  (*in scroll bar*)
BEGIN
  F.thumb := Max(F.org * (F.H - 24) DIV (F.text.length + 1), 0);
  w.s("UpdateThumb: F.org "); w.i(F.org);
  w.s(", F.text.length ");    w.i(F.H);
  w.s(", F.H ");              w.i(F.H);
  w.s("-> F.thumb ");         w.i(F.thumb); w.sl(".");
  MarkThumb(F, TRUE)
END UpdateThumb;


PROCEDURE SetChangeMark (F: Frame;  on: BOOLEAN);  (*in corner*)
BEGIN
  w.s("TextFrames.SetChangeMark $"); w.h(SYSTEM.ADR(F^));
  IF on THEN w.sl(" on.") ELSE w.sl(" off.") END;
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


PROCEDURE MeasureLine (l: Line;  VAR R: Texts.Reader);
VAR ascent, descent, lead, length: INTEGER;
BEGIN
  ascent := 1;  descent := 1;  lead := 1;  length := 0;
  WHILE (NextCh # CR) & (R.fnt # NIL) DO
    IF R.fnt.ascent  > ascent  THEN ascent  := R.fnt.ascent  END;
    IF R.fnt.descent > descent THEN descent := R.fnt.descent END;
    IF R.fnt.lead    > lead    THEN lead    := R.fnt.lead    END;
    INC(length);  Texts.Read(R, NextCh)
  END;
  l.length  := length + 1;
  l.ascent  := ascent;
  l.descent := descent;
  l.lead    := lead;
  l.eot     := R.fnt = NIL;
  Texts.Read(R, NextCh)
END MeasureLine;


PROCEDURE PlaceLine(f: Frame;  l: Line;  top: INTEGER);
(* Determines the first available baseline in the frame at which to place *)
(* line l such that the line's ascent does not reach higher than top.     *)
VAR linenum, lineheight: INTEGER;
BEGIN
  lineheight := f.ascent + f.descent + f.lead;
  linenum    := (top - f.top - 1) DIV lineheight;
  l.baseline := f.top + (linenum + 1) * lineheight + f.ascent
END PlaceLine;


PROCEDURE DisplayLine(  (*pass 2*)
  F:       Frame;
  L:       Line;
  VAR R:   Texts.Reader;
  X:       INTEGER;  (* In whole pixels relative to screen *)
  Y:       INTEGER;  (* In whole pixels, baseline will be placed at Y *)
  length:  INTEGER   (* 0 unless displaying trailing part of line *)
);
VAR
  dx, xLim: INTEGER;
  glyph:    Fonts.Glyph;
BEGIN
  w.s("DisplayLine, R.col = $"); w.h(R.col); w.s(" '");
  X      := X * 256;                     (* switch to subpixels *)
  xLim   := (F.X + F.W - F.right) * 256; (* x Limit in pixels   *)
  WHILE (NextCh # CR) & (R.fnt # NIL) DO
    w.c(NextCh);
    glyph := Fonts.GetGlyph  (R.fnt, ORD(NextCh));
    dx    := Fonts.GetAdvance(R.fnt, ORD(NextCh));
    IF (glyph.map # 0) & ((X + dx) < xLim) THEN
      Display.RenderAlphaMap(
        X DIV 64 + glyph.originX,  Y - glyph.baseline,
        glyph.mapWidth,            glyph.mapHeight,
        glyph.map,                 R.col)
    END;
    X := X + dx;
    INC(length);
    Texts.Read(R, NextCh)
  END;
  w.sl("'.");
  L.length := length + 1;
  L.width  := (X+255) DIV 256 + EolWidth - (F.X + F.left);
  L.eot    := R.fnt = NIL;
  Texts.Read(R, NextCh)
END DisplayLine;


PROCEDURE Validate (T: Texts.Text;  VAR pos: INTEGER);
VAR R: Texts.Reader;
BEGIN
  IF pos > T.length THEN pos := T.length
  ELSIF pos > 0 THEN
    DEC(pos);
    Texts.OpenReader(R, T, pos);
    REPEAT Texts.Read(R, NextCh);  INC(pos) UNTIL R.eot OR (NextCh = CR)
  ELSE pos := 0
  END
END Validate;

(*------------------frame modifiers------------------------*)

PROCEDURE Restore* (F: Frame);
CONST K = 100;  (*assumed number of lines*)
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
  w.s("Restore frame at "); Display.wfpos(F); w.sl(".");
  Display.FillRectangle(F.X, F.Y, F.W, F.H, F.colour);
  IF F.left >= BarWidth THEN  (* white line between left side bar and content *)
    Display.FillRectangle(F.X + BarWidth - 1, F.Y, 1, F.H, Display.white)
  END;

  Validate(F.text, F.org);
  top     := F.top;
  bottom  := F.H - F.bottom;
  prevl   := F.sentinel;

  IF top < bottom THEN
    y   := top;
    org := F.org;
    Texts.OpenReader(R, F.text, org);
    Texts.Read(R, NextCh);
    WHILE ~prevl.eot & (y < bottom) DO
      w.s("  restore line at org "); w.i(org); w.sl(".");
      NewLine(F, line);
      MeasureLine(line, R);
      PlaceLine(F, line, y);
      Texts.OpenReader(R, F.text, org);  (* Reset reader to start of line for display *)
      Texts.Read(R, NextCh);
      DisplayLine(F, line, R, F.X + F.left, F.Y + line.baseline, 0);
      y          := line.baseline + line.descent;
      prevl.next := line;
      prevl      := line;
      org        := org + line.length;
    END
  END;
  prevl.next := F.sentinel;
  F.thumb    := F.H * F.org DIV (F.text.length + 1);
END Restore;

PROCEDURE Suspend* (F: Frame);
BEGIN F.sentinel.next := F.sentinel;  (*F.pool := NIL*)
END Suspend;

PROCEDURE Extend* (F: Frame;  newY: INTEGER);
VAR R: Texts.Reader;  L, l: Line;
    org: INTEGER;  curY, topY, botY, botY0: INTEGER;
BEGIN
  (*
  Display.ReplConst(F.col, F.X, newY, F.W, F.Y - newY, Display.replace);
  IF F.left >= BarWidth THEN
    Display.ReplConst(Display.white, F.X + BarWidth - 1, newY, 1, F.Y - newY, Display.invert)
  END;
  topY := F.Y + F.H - F.top;  botY0 := F.Y + F.bottom;
  F.H := F.H + F.Y - newY;  F.Y := newY;  botY := F.Y + F.bottom;
  IF F.sentinel.next = F.sentinel THEN Validate(F.text, F.org) END;
  L := F.sentinel;  org := F.org;  curY := topY + F.voff;
  WHILE (L.next # F.sentinel) & (curY > botY0) DO
    L := L.next;  org := org + L.length;  curY := curY - L.lineHeight
  END;
  IF (L # F.sentinel) & (curY < botY0) THEN
    Texts.OpenReader(R, F.text, org - L.length);  Texts.Read(R, NextCh);
    DisplayLine(F, L, R, F.X + F.left, curY + L.descent, botY0, botY, 0)  (*old fractional bottom line*)
  ELSE Texts.OpenReader(R, F.text, org);  Texts.Read(R, NextCh)
  END;
  IF topY > botY THEN
    WHILE ~L.eot & (curY > botY) DO
      NewLine(F, l);
      PrepareLine(l, R);
      Texts.OpenReader(R, F.text, org);  Texts.Read(R, NextCh);
      DisplayLine(F, l, R, F.X + F.left, curY - l.ascent, topY, botY, 0);
      L.next := l;  L := l;  org := org + L.length;  curY := curY - L.lineHeight
    END
  END;
  L.next := F.sentinel;
  F.thumb := F.org * F.H DIV (F.text.length + 1)
  *)
END Extend;

PROCEDURE Reduce* (F: Frame;  newY: INTEGER);
VAR L: Line;  curY, topY, botY: INTEGER;
BEGIN
  (*
  F.H := F.H + F.Y - newY;  F.Y := newY;
  topY := F.Y + F.H - F.top;  botY := F.Y + F.bottom;  L := F.sentinel;
  IF topY > botY THEN curY := topY + F.voff;
    WHILE (L.next # F.sentinel) & (curY > botY) DO
      L := L.next;  curY := curY - L.lineHeight
    END
  END;
  LastLine(F, L);
  IF F.H >= F.top + F.bottom THEN Display.ReplConst(F.col, F.X + F.left, F.Y, F.W - F.left,
                                                 F.bottom, Display.replace)
  ELSIF F.H > F.top THEN Display.ReplConst(F.col, F.X + F.left, F.Y, F.W - F.left,
                                           F.H - F.top, Display.replace)
  END;
  F.thumb := F.org * F.H DIV (F.text.length + 1);  MarkThumb(F, TRUE)
  *)
END Reduce;

(*------------------fractional line scrolling------------------------*)

PROCEDURE ScrollDown (F: Frame;  org: INTEGER;  voff, dY: INTEGER;
                      lines: ARRAY OF LineDesc;  beg, end: INTEGER);
VAR R: Texts.Reader;  L, L0, l: Line;
    curY, topY, botY, Y0, Y1, Y2, h, k: INTEGER;
BEGIN
  (*
  topY := F.Y + F.H - F.top;  botY := F.Y + F.bottom;
  curY := topY + F.voff - dY;  Y0 := Max(curY, botY);
  F.org := org;  F.voff := voff;  L := F.sentinel;
  WHILE (L.next # F.sentinel) & (curY > botY) DO
    L := L.next;  curY := curY - L.lineHeight
  END;
  LastLine(F, L);
  Y1 := Max(topY - dY, botY);  Y2 := Max(curY, botY);  h := Y1 - Y2;
  IF h > 0 THEN Display.CopyBlock(F.X + F.left, topY - h, F.W - F.left,
                                  h, F.X + F.left, Y2, 0) END;
  ReplConst(F.col, F, F.X + F.left, Y1, F.W - F.left,
            topY - Y1, Display.replace);
  L := F.sentinel;  L0 := L.next;  curY := topY + voff;  k := beg;
  Texts.OpenReader(R, F.text, org);  Texts.Read(R, NextCh);
  WHILE ~L.eot & (curY > Y0) DO
    NewLine(F, l);
    IF k < end THEN l^ := lines[k];  INC(k) ELSE PrepareLine(l, R) END;
    Texts.OpenReader(R, F.text, org);  Texts.Read(R, NextCh);
    DisplayLine(F, l, R, F.X + F.left, curY - l.ascent, topY, Y0, 0);
    L.next := l;  L := l;  org := org + L.length;  curY := curY - L.lineHeight
  END;
  L.next := L0;
  IF (L0 # F.sentinel) & (curY > Y1) THEN
    DisplayLine(F, L0, R, F.X + F.left, curY - L0.ascent, topY, Y1, 0)  (*old fractional top line*)
  END;
  UpdateThumb(F)
  *)
END ScrollDown;

PROCEDURE ScrollUp (F: Frame;  org: INTEGER;  voff, dY: INTEGER;  skipto: Line);
VAR R: Texts.Reader;  L, l: Line;
    curY, topY, botY, Y0, Y1, h: INTEGER;
BEGIN
  (*
  topY := F.Y + F.H - F.top;  botY := F.Y + F.bottom;  curY := topY + voff - dY;
  F.org := org;  F.voff := voff;  L := F.sentinel;  PoolLines(F, L, skipto);
  WHILE (L.next # F.sentinel) & (curY > botY) DO
    L := L.next;  org := org + L.length;  curY := curY - L.lineHeight
  END;
  Y1 := Max(curY, botY);  Y0 := Min(Y1 + dY, topY);  h := topY - Y0;
  IF h > 0 THEN
    Display.CopyBlock(F.X + F.left, Y1, F.W - F.left, h, F.X + F.left, Y0, 0)
  END;
  ReplConst(F.col, F, F.X + F.left, Y1, F.W - F.left, Y0 - Y1, Display.replace);
  curY := curY + dY;
  IF (L # F.sentinel) & (curY - dY < botY) THEN
    Texts.OpenReader(R, F.text, org - L.length);  Texts.Read(R, NextCh);
    DisplayLine(F, L, R, F.X + F.left, curY + L.descent, Y0, botY, 0)  (*old fractional bottom line*)
  ELSE Texts.OpenReader(R, F.text, org);  Texts.Read(R, NextCh)
  END;
  WHILE ~L.eot & (curY > botY) DO
    NewLine(F, l);
    PrepareLine(l, R);
    Texts.OpenReader(R, F.text, org);  Texts.Read(R, NextCh);
    DisplayLine(F, l, R, F.X + F.left, curY - l.ascent, topY, botY, 0);
    L.next := l;  L := l;  org := org + L.length;  curY := curY - L.lineHeight
  END;
  L.next := F.sentinel;  UpdateThumb(F)
  *)
END ScrollUp;

PROCEDURE Scroll* (F: Frame;  dY: INTEGER);  (*scroll displayed text dY pixels up or down*)
CONST K = 100;  length = 75;  (*assumed number of lines and average line length*)
VAR R: Texts.Reader;  L, L0: Line;  l: LineDesc;
    org, org0, q: INTEGER;  dy, i, k: INTEGER;
    lines: ARRAY K OF LineDesc;  (*cache to prevent re-reading of lines*)
BEGIN (*dY # 0*)
  (*
  IF F.sentinel.next # F.sentinel THEN
    IF dY < 0 THEN dY := -dY;
      IF dY <= F.voff THEN ScrollDown(F, F.org, F.voff - dY, dY, lines, 0, 0)
      ELSIF F.org = 0 THEN ScrollDown(F, 0, 0, F.voff, lines, 0, 0)
      ELSE (*F.org > 0 & dY > F.voff*) k := F.sentinel.next.lineHeight;
        q := F.org - length*((dY - F.voff + k - 1) DIV k - 1);  (*first guess backward*)
        REPEAT DEC(q, length);  org := q;  Validate(F.text, org) UNTIL org < F.org;
        REPEAT org0 := org;  k := 0;  dy := 0;
          Texts.OpenReader(R, F.text, org);  Texts.Read(R, NextCh);
          WHILE org < F.org DO
            MeasureLine(l, R);
            org := org + l.length;  dy := dy + l.lineHeight;
            IF k < K THEN lines[k] := l;  INC(k) END
          END;
          IF dy + F.voff >= dY THEN org := org0;  i := 0;
            WHILE (i < k) & (dy + F.voff - lines[i].lineHeight >= dY) DO
              org := org + lines[i].length;  dy := dy - lines[i].lineHeight;  INC(i)
            END;
            IF (i < K) OR (org = F.org) THEN
              ScrollDown(F, org, F.voff + dy - dY, dY, lines, i, k);  org := -1
            END
          ELSIF org0 > 0 THEN  (*next guess backward*)
            REPEAT DEC(q, length);  org := q;  Validate(F.text, org) UNTIL org < org0
          ELSE (*reached beginning of text*) org := -1;
            IF dy + F.voff > 0 THEN ScrollDown(F, 0, 0, dy + F.voff, lines, 0, k) END
          END
        UNTIL org < 0
      END
    ELSIF dY > 0 THEN
      org := F.org;  L0 := F.sentinel;  L := L0.next;  dy := 0;
      WHILE (L # F.sentinel) & (dy + L.lineHeight <= dY + F.voff) DO
        org := org + L.length;  dy := dy + L.lineHeight;  L0 := L;  L := L.next
      END;
      IF L # F.sentinel THEN ScrollUp(F, org, F.voff + dY - dy, dY, L0)
      ELSIF org < F.text.length THEN
        Texts.OpenReader(R, F.text, org);  Texts.Read(R, NextCh);  MeasureLine(l, R);
        WHILE ~l.eot & (org < F.text.length) & (dy + l.lineHeight <= dY + F.voff) DO
          org := org + l.length;  dy := dy + l.lineHeight;  MeasureLine(l, R)
        END;
        IF (org < F.text.length) & (dy + l.lineHeight > dY + F.voff) THEN MarkThumb(F, FALSE);
          F.org := org;  F.voff := F.voff + dY - dy;
          LastLine(F, F.sentinel);  Restore(F);  MarkThumb(F, TRUE)
        END
      END
    END
  END
  *)
END Scroll;

PROCEDURE Show* (F: Frame; pos: INTEGER);  (*scroll specified text position to the top*)
BEGIN
  F.org := pos;
  Restore(F)
END Show;

(*
CONST K = 100;  (*assumed number of lines*)
VAR R: Texts.Reader; L, L0: Line; l: LineDesc;
  org: INTEGER; dy, h, k: INTEGER;
  lines: ARRAY K OF LineDesc;  (*cache to prevent re-reading of lines*)
BEGIN
  IF F.sentinel.next # F.sentinel THEN
    Validate(F.text, pos);
    IF pos < F.org THEN
      org := pos; h := F.H - F.top - F.bottom + F.voff; k := 0; dy := 0;
      Texts.OpenReader(R, F.text, org); Texts.Read(R, nextCh);
      WHILE (org < F.org) & (dy < h) DO
        MeasureLine(l, R);
        org := org + l.len; dy := dy + l.lsp;
        IF k < K THEN lines[k] := l; INC(k) END
      END ;
      IF (org = F.org) & (dy < h) THEN ScrollDown(F, pos, 0, dy + F.voff, lines, 0, k)
      ELSE MarkThumb(F, FALSE); F.org := pos; F.voff := 0; LastLine(F, F.sentinel); Restore(F); MarkThumb(F, TRUE)
      END
    ELSIF pos > F.org THEN
      org := F.org; L0 := F.sentinel; L := L0.next; dy := 0;
      WHILE (L # F.sentinel) & (org < pos) DO
        org := org + L.len; dy := dy + L.lsp; L0 := L; L := L.next
      END ;
      IF L # F.sentinel THEN ScrollUp(F, org, 0, dy - F.voff, L0)
      ELSIF pos < F.text.len THEN
        MarkThumb(F, FALSE); F.org := pos; F.voff := 0; LastLine(F, F.sentinel); Restore(F); MarkThumb(F, TRUE)
      END
    ELSIF F.voff > 0 THEN ScrollDown(F, pos, 0, F.voff, lines, 0, 0)
    END
  END
*)

(*------------------locators------------------------*)

PROCEDURE LocateLine (F: Frame;  y: INTEGER;  VAR loc: Location);
VAR
  line: Line;
  org:  INTEGER;
BEGIN
  org  := F.org;
  line := F.sentinel.next;
  (*w.s("TextFrames.LocateLine y "); w.i(y); w.sl(".");*)
  WHILE (line.next # F.sentinel) & (line.baseline + line.descent + line.lead < y) DO
    (*
    w.s("  baseline "); w.i(line.baseline);
    w.s(", ascent ");   w.i(line.ascent);
    w.s(", length ");   w.i(line.length);
    w.sl(".");
    *)
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
  lx:    INTEGER;     (* subpixel position at left end of non-blank run *)
  dx:    INTEGER;     (* subpixel count of non-blank run *)
  rpos:  INTEGER;     (* char position at right end of run *)
  rx:    INTEGER;     (* subpixel position at right end of run *)
BEGIN
  LocateLine(F, y, loc);
  lim  := loc.org + loc.line.length - 1;
  lx   := F.left * 256;
  pos  := loc.org;
  rx   := lx;
  lpos := pos;
  rpos := pos;
  dx   := 0;
  Texts.OpenReader(R, F.text, loc.org);
  Texts.Read(R, NextCh);
  REPEAT
    WHILE (pos < lim) & (NextCh > " ") DO  (* Scan to next space *)
      INC(rx, Fonts.GetAdvance(R.fnt, ORD(NextCh)));
      INC(pos);
      Texts.Read(R, NextCh)
    END;
    dx := rx - lx;
    WHILE (pos < lim) & (NextCh <= " ") DO  (* Scan spaces *)
      INC(rx, Fonts.GetAdvance(R.fnt, ORD(NextCh)));
      INC(pos);
      Texts.Read(R, NextCh)
    END;

(* Was:
    IF (rx >= x) OR (pos >= lim) THEN  (* x is in this run *)
      pos := lim  (* Trigger exit of repeat loop *)
    ELSE
      lpos := pos;  lx := rx;  (* Start again with this non-blank run *)
      INC(rx, Fonts.GetAdvance(R.fnt, ORD(NextCh)));
      INC(pos);
      Texts.Read(R, NextCh)
    END
  UNTIL pos >= lim;
changed to:*)

    IF (rx DIV 256 < x) & (pos < lim) THEN
      lpos := pos;  lx := rx
    END
  UNTIL (rx >= x) OR (pos >= lim);
(**)

  loc.pos := lpos;
  loc.x   := (lx + 255) DIV 256;
  loc.dx  := (dx + 255) DIV 256;
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
  L    := F.sentinel.next;
  IF pos < org THEN pos := org END;
  WHILE (L.next # F.sentinel) & (pos >= org + L.length) DO
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
END  LocatePos;


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
  END
END ShowCaret;

PROCEDURE SetCaretPos* (F: Frame;  pos: INTEGER);
BEGIN LocatePos(F, pos, F.caretLoc);  ShowCaret(F);  F.hasCaret := TRUE
END SetCaretPos;

PROCEDURE TrackCaret* (F: Frame;  X, Y: INTEGER;  VAR keysum: SET);
VAR loc: Location;  keys: SET;
BEGIN
  w.s("TextFrames.SetCaretPos for frame $"); w.h(SYSTEM.ADR(F^)); w.sl(".");
  IF F.sentinel.next # F.sentinel THEN
    LocateChar(F, X - F.X, Y - F.Y, F.caretLoc);
    ShowCaret(F);
    keysum := {};
    (*
    REPEAT
      Input.Mouse(keys, X, Y);
      keysum := keysum + keys;
      Oberon.DrawMouseArrow(X, Y);
      LocateChar(F, X - F.X, Y - F.Y, loc);
      IF loc.pos # F.caretLoc.pos THEN ShowCaret(F);  F.caretLoc := loc;  ShowCaret(F) END
    UNTIL keys = {};
    *)
    F.hasCaret := TRUE
  END
END TrackCaret;

PROCEDURE RemoveCaret* (F: Frame);
BEGIN (*IF F.hasCar THEN ShowCaret(F);  F.hasCar := FALSE END*)
END RemoveCaret;

PROCEDURE FlipSelection (F: Frame;  VAR beg, end: Location);
VAR L: Line;  Y: INTEGER;  org: INTEGER;  beg0: Location;
BEGIN
  (*
  IF end.org >= F.org THEN
    org := F.org;  L := F.sentinel.next;  Y := F.Y + F.H - F.top + F.voff - L.lineHeight;
    WHILE (L # F.sentinel) & (org < beg.org) DO
      org := org + L.length;  L := L.next;  Y := Y - L.lineHeight
    END;
    IF L # F.sentinel THEN
      IF beg.org < F.org THEN beg0.org := F.org;  beg0.x := F.left ELSE beg0 := beg END;
      IF beg0.org = end.org THEN
        ReplConst(Display.white, F, F.X + beg0.x, Y, end.x - beg0.x, L.lineHeight, Display.invert)
      ELSE
        ReplConst(Display.white, F, F.X + beg0.x, Y, F.left + L.width - beg0.x,
                  L.lineHeight, Display.invert);
        org := org + L.length;  L := L.next;  Y := Y - L.lineHeight;
        WHILE (L # F.sentinel) & (org < end.org) DO
          ReplConst(Display.white, F, F.X + F.left, Y, L.width, L.lineHeight, Display.invert);
          org := org + L.length;  L := L.next;  Y := Y - L.lineHeight
        END;
        IF (L # F.sentinel) & (org = end.org) THEN
          ReplConst(Display.white, F, F.X + F.left, Y, end.x - F.left, L.lineHeight, Display.invert)
        END
      END
    END
  END
  *)
END FlipSelection;

PROCEDURE SetSelection* (F: Frame;  beg, end: INTEGER);
BEGIN
  (*
  IF F.hasSel THEN FlipSelection(F, F.selbeg, F.selend) END;
  LocatePos(F, beg, F.selbeg);  LocatePos(F, end, F.selend);
  IF F.selbeg.pos < F.selend.pos THEN
    FlipSelection(F, F.selbeg, F.selend);  F.time := Oberon.Time();  F.hasSel := TRUE
  END
  *)
END SetSelection;

PROCEDURE TrackSelection* (F: Frame;  X, Y: INTEGER;  VAR keysum: SET);
VAR loc, L, R: Location;  keys: SET;  start, prev: INTEGER;  topY, botY: INTEGER;
BEGIN
  (*
  IF F.sentinel.next # F.sentinel THEN topY := F.Y + F.H - F.top;  botY := F.Y + F.bottom;
    IF F.hasSel THEN FlipSelection(F, F.selbeg, F.selend) END;
    IF F.hasCar THEN ShowCaret(F) END;
    LocateChar(F, X - F.X, Y - F.Y, loc);
    IF F.hasSel & (loc.pos = F.selbeg.pos) & (F.selend.pos = F.selbeg.pos + 1) THEN
      LocateChar(F, F.left, Y - F.Y, F.selbeg)
    ELSE F.selbeg := loc
    END;
    INC(loc.pos);  loc.x := loc.x + loc.dx;  F.selend := loc;
    FlipSelection(F, F.selbeg, F.selend);
    SetChangeMark(F, FALSE);  Oberon.RemoveMarks(F.X, F.Y, F.W, F.H);
    keysum := {};  start := F.selbeg.pos;
    REPEAT prev := loc.pos;
      Input.Mouse(keys, X, Y);
      keysum := keysum + keys;
      IF Y < botY THEN  (*scroll text up*)
        IF (F.selbeg.org = F.selend.org) OR (prev <= start) THEN
          L := F.selbeg;  R := F.selend
        ELSE
          R := F.selend;  L.org := F.selend.org;  L.pos := L.org;  L.x := F.left
        END;
        FlipSelection(F, L, R);  Oberon.FadeMouse;
        Scroll(F, botY - Y);  Y := botY;
        FlipSelection(F, L, R)
      ELSIF (Y > topY - 1) & ((F.org > 0) OR (F.voff > 0)) THEN  (*scroll text down*)
        IF (F.selbeg.org = F.selend.org) OR (prev > start) THEN
          L := F.selbeg;  R := F.selend
        ELSE
          L := F.selbeg;  R.org := F.selbeg.org;  R.pos := F.selbeg.lin.length;
          R.x := F.left + F.selbeg.lin.width
        END;
        FlipSelection(F, L, R);  Oberon.FadeMouse;
        Scroll(F, topY - 1 - Y);  Y := topY - 1;
        FlipSelection(F, L, R)
      END;
      Oberon.DrawMouseArrow(X, Y);
      LocateChar(F, X - F.X, Y - F.Y, loc);
      IF prev > start THEN INC(loc.pos);  loc.x := loc.x + loc.dx;
        IF loc.pos > start THEN
          IF loc.pos < F.selend.pos THEN FlipSelection(F, loc, F.selend);  F.selend := loc
          ELSIF loc.pos > F.selend.pos THEN FlipSelection(F, F.selend, loc);  F.selend := loc
          END
        ELSE (*switch to left of start*) FlipSelection(F, F.selbeg, F.selend);
          F.selend := F.selbeg;  F.selbeg := loc;  FlipSelection(F, F.selbeg, F.selend)
        END
      ELSIF loc.pos > start THEN (*switch to right of start*) FlipSelection(F, F.selbeg, F.selend);
        F.selbeg := F.selend;  F.selend := loc;  FlipSelection(F, F.selbeg, F.selend)
      ELSIF loc.pos > F.selbeg.pos THEN FlipSelection(F, F.selbeg, loc);  F.selbeg := loc
      ELSIF loc.pos < F.selbeg.pos THEN FlipSelection(F, loc, F.selbeg);  F.selbeg := loc
      END
    UNTIL keys = {};
    SetChangeMark(F, F.text.changed);  F.time := Oberon.Time();  F.hasSel := TRUE;
    IF F.hasCar THEN SetCaretPos(F, F.caretLoc.pos) END
  END
  *)
END TrackSelection;

PROCEDURE RemoveSelection* (F: Frame);
BEGIN (*IF F.hasSel THEN FlipSelection(F, F.selbeg, F.selend);  F.hasSel := FALSE END*)
END RemoveSelection;

PROCEDURE TrackLine* (F: Frame;  X, Y: INTEGER;  VAR org: INTEGER;  VAR keysum: SET);
VAR old, new: Location;  keys: SET;
BEGIN
  (*
  IF F.sentinel.next # F.sentinel THEN
    LocateLine(F, Y - F.Y, old);
    ReplConst(Display.white, F, F.X + F.left,
              F.Y + old.y - old.lin.descent, old.lin.width, 2, Display.invert);
    keysum := {};
    REPEAT Input.Mouse(keys, X, Y);
      keysum := keysum + keys;
      Oberon.DrawMouse(ScrollMarker, X, Y);
      LocateLine(F, Y - F.Y, new);
      IF new.org # old.org THEN
        ReplConst(Display.white, F, F.X + F.left,
                  F.Y + old.y - old.lin.descent, old.lin.width, 2, Display.invert);
        ReplConst(Display.white, F, F.X + F.left,
                  F.Y + new.y - new.lin.descent, new.lin.width, 2, Display.invert);
        old := new
      END
    UNTIL keys = {};
    ReplConst(Display.white, F, F.X + F.left,
              F.Y + new.y - new.lin.descent, new.lin.width, 2, Display.invert);
    org := new.org
  ELSE org := 0
  END
  *)
END TrackLine;

PROCEDURE TrackWord* (F: Frame;  X, Y: INTEGER;  VAR pos: INTEGER;  VAR keysum: SET);
VAR old, new: Location;  keys: SET;
BEGIN
  (*
  IF F.sentinel.next # F.sentinel THEN
    LocateString(F, X - F.X, Y - F.Y, old);
    ReplConst(Display.white, F, F.X + old.x,
              F.Y + old.y - old.lin.descent, old.dx, 2, Display.invert);
    keysum := {};
    REPEAT
      Input.Mouse(keys, X, Y);  keysum := keysum + keys;
      Oberon.DrawMouseArrow(X, Y);
      LocateString(F, X - F.X, Y - F.Y, new);
      IF new.pos # old.pos THEN
        ReplConst(Display.white, F, F.X + old.x,
                  F.Y + old.y - old.lin.descent, old.dx, 2, Display.invert);
        ReplConst(Display.white, F, F.X + new.x,
                  F.Y + new.y - new.lin.descent, new.dx, 2, Display.invert);
        old := new
      END
    UNTIL keys = {};
    ReplConst(Display.white, F, F.X + new.x,
              F.Y + new.y - new.lin.descent, new.dx, 2, Display.invert);
    pos := new.pos
  ELSE pos := 0
  END
  *)
END TrackWord;

(*------------------text modifiers------------------------*)

PROCEDURE Replace* (F: Frame;  beg, end: INTEGER);
(* Redisplay frame following change from beg to end *)
VAR
  R: Texts.Reader;
  L, L0, l: Line;
  org: INTEGER;
  curY, topY, botY, dy: INTEGER;
BEGIN
  IF end > F.org THEN
    Restore(F)
    (*
    topY := F.Y + F.H - F.top;
    IF beg < F.org THEN beg := F.org END;
    org  := F.org;
    L0   := F.sentinel;
    L    := L0.next;
    curY := topY + F.voff;
    WHILE (L # F.sentinel) & (org + L.length <= beg) DO
      org  := org + L.length;
      curY := L.baseline;
      L0   := L;
      L    := L.next
    END;
    IF L # F.sentinel THEN
      botY := F.Y + F.bottom;
      dy := 0;
      Texts.OpenReader(R, F.text, org);  Texts.Read(R, NextCh);
      WHILE (L # F.sentinel) & (org <= end) & (dy = 0) DO
        dy := L.lineHeight;  PrepareLine(L, R);  dy := L.lineHeight - dy;
        Texts.OpenReader(R, F.text, org);  Texts.Read(R, NextCh);
        ReplConst(F.col, F, F.X + F.left, curY - L.lineHeight, F.W - F.left, L.lineHeight, Display.replace);
        DisplayLine(F, L, R, F.X + F.left, curY - L.ascent, topY, botY, 0);
        org := org + L.length;  curY := curY - L.lineHeight;  L0 := L;  L := L.next
      END;
      IF ~L0.eot & (curY > botY) & (dy # 0) THEN
        ReplConst(F.col, F, F.X + F.left, botY, F.W - F.left, curY - botY, Display.replace);
        LastLine(F, L0);
        WHILE ~L0.eot & (curY > botY) DO
          NewLine(F, l);
          PrepareLine(l, R);
          Texts.OpenReader(R, F.text, org);  Texts.Read(R, NextCh);
          DisplayLine(F, l, R, F.X + F.left, curY - l.ascent, topY, botY, 0);
          L0.next := l;  L0:= l;  org := org + L0.length;  curY := curY - L0.lineHeight
        END;
        L0.next := F.sentinel
      END
    END
    *)
  END;
  UpdateThumb(F)
END Replace;


PROCEDURE Insert* (F: Frame;  beg, end: INTEGER);
(* Redisplay frame following insertion at beg to end *)
VAR R: Texts.Reader;  L, L0, l: Line;
    org, length: INTEGER;  curY, topY, botY, dy, width: INTEGER;
BEGIN
  w.sl("TextFrames.Insert.");
  Restore(F);
  (*
  IF beg < F.org THEN F.org := F.org + (end - beg)
  ELSE topY := F.Y + F.H - F.top;
    org := F.org;  L := F.sentinel.next;  curY := topY + F.voff;
    WHILE (L # F.sentinel) & (org + L.length <= beg) DO
      org := org + L.length;  curY := curY - L.lineHeight;  L := L.next
    END;
    IF L # F.sentinel THEN botY := F.Y + F.bottom;
      Texts.OpenReader(R, F.text, org);  Texts.Read(R, NextCh);
      dy := L.lineHeight;  PrepareLine(L, R);  dy := L.lineHeight - dy;
      Texts.OpenReader(R, F.text, org);  Texts.Read(R, NextCh);
      IF dy = 0 THEN length := beg - org;  width := Width(R, length);
        ReplConst (F.col, F, F.X + F.left + width, curY
            - L: INTEGER;
            ineH eight, L.width - width, L.lineHeight, Display.replace);
        DisplayLine(F, L, R, F.X + F.left + width, curY - L.ascent, topY, botY, length)
      ELSE
        ReplConst(F.col, F, F.X + F.left, curY - L.lineHeight, F.W - F.left, L.lineHeight, Display.replace);
        DisplayLine(F, L,
         R, F.X + F.left, curY - L.ascent, topY, botY, 0)
      END;
      org := org + L.length;  curY := curY - L.lineHeight;  L0 := L.next;
      WHILE (org <= end) & (curY > botY) DO
        NewLine(F, l);
        PrepareLine(l, R);
        Texts.OpenReader(R, F.text, org);  Texts.Read(R, NextCh);
        ReplConst(F.col, F, F.X + F.left, curY - l.lineHeight, F.W - F.left, l.lineHeight, Display.replace);
        DisplayLine(F, l, R, F.X + F.left, curY - l.ascent, topY, botY, 0);
        L.next := l;  L := l;  org := org + L.length;  curY := curY - L.lineHeight
      END;
      IF (dy # 0) OR (L0 # L.next) THEN
        ReplConst(F.col, F, F.X + F.left, botY, F.W - F.left, curY - botY, Display.replace);
        WHILE ~L.eot & (curY > botY) DO
          NewLine(F, l);
          PrepareLine(l, R);
          Texts.OpenReader(R, F.text, org);  Texts.Read(R, NextCh);
          DisplayLine(F, l, R, F.X + F.left, curY - l.ascent, topY, botY, 0);
          L.next := l;  L := l;  org := org + L.length;  curY := curY - L.lineHeight
        END;
        L.next := F.sentinel
      END
    END
  END;
  *)
  UpdateThumb(F)
END Insert;

PROCEDURE Delete* (F: Frame;  beg, end: INTEGER);
VAR R: Texts.Reader;  L, L0, l: Line;
    org, org0, length: INTEGER;  curY, topY, botY, Y0, Y1, dy, width: INTEGER;
BEGIN
  IF end <= F.org THEN F.org := F.org - (end - beg)
  ELSE
    Restore(F);
    (*
    topY := F.Y + F.H - F.top;
    IF beg < F.org THEN
      F.sentinel.next.length := F.sentinel.next.length + (F.org - beg);  F.org := beg
    END;
    org := F.org;  L := F.sentinel.next;  curY := topY + F.voff;
    WHILE (L # F.sentinel) & (org + L.length <= beg) DO
      org := org + L.length;  curY := curY - L.lineHeight;  L := L.next
    END;
    IF L # F.sentinel THEN botY := F.Y + F.bottom;
      org0 := org;  L0 := L;  l := L;  Y0 := curY;
      WHILE (L # F.sentinel) & (org <= end) DO
        org := org + L.length;  curY := curY - L.lineHeight;  l := L;  L := L.next
      END;
      Y1 := curY;
      Texts.OpenReader(R, F.text, org0);  Texts.Read(R, NextCh);
      dy := L0.lineHeight;  PrepareLine(L0, R);  dy := L0.lineHeight - dy;
      Texts.OpenReader(R, F.text, org0);  Texts.Read(R, NextCh);
      IF dy = 0 THEN length := beg - org0;  width := Width(R, length);
        ReplConst (F.col, F, F.X + F.left + width,

        : INTEGER;
                    Y0 - L0.lineHeight, L0.width - width, L0.lineHeight, Display.replace);
        DisplayLine(F, L0, R, F.X + F.left + width,
                    Y0 - L0.ascent, topY, botY, length)
      ELSE
        ReplConst(F.c
          ol, F, F.X + F.left, Y0 - L0.lineHeight, F.W - F.left, L0.lineHeight, Display.replace);
        DisplayLine(F, L0, R, F.X + F.left, Y0 - L0.ascent, topY, botY, 0)
      END;
      IF (dy # 0) OR (L0.next # L) THEN PoolLines(F, L0, l);
        org := org0 + L0.length;  Y0 := Y0 - L0.lineHeight;  L := L0;
        WHILE L.next # F.sentinel DO
          L := L.next;  org := org + L.length;  curY := curY - L.lineHeight
        END;
        IF curY < botY THEN
          IF Y1 > botY THEN
            Display.CopyBlock(F.X + F.left, botY, F.W - F.left,
                              Y1 - botY, F.X + F.left, botY + Y0 - Y1, 0)
          END;
          dy := L.lineHeight - botY + curY;  curY := curY + Y0 - Y1 + L.lineHeight;
          ReplConst(F.col, F, F.X + F.left, botY, F.W - F.left,
                    curY - dy - botY, Display.replace);
          Texts.OpenReader(R, F.text, org - L.length);  Texts.Read(R, NextCh);
          DisplayLine(F, L, R, F.X + F.left, curY - L.ascent, curY - dy, botY, 0);
          curY := curY - L.lineHeight
        ELSE
          Display.CopyBlock(F.X + F.left, curY, F.W - F.left,
                            Y1 - curY, F.X + F.left, curY + Y0 - Y1, 0);
          curY := curY + Y0 - Y1;
          ReplConst(F.col, F, F.X + F.left, botY, F.W - F.left, curY - botY, Display.replace)
        END;
        Texts.OpenReader(R, F.text, org);  Texts.Read(R, NextCh);
        WHILE ~L.eot & (curY > botY) DO
          NewLine(F, l);
          PrepareLine(l, R);
          Texts.OpenReader(R, F.text, org);  Texts.Read(R, NextCh);
          DisplayLine(F, l, R, F.X + F.left, curY - l.ascent, topY, botY, 0);
          L.next := l;  L := l;  org := org + L.length;  curY := curY - L.lineHeight
        END;
        L.next := F.sentinel
      END
    END
    *)
  END;
  UpdateThumb(F)
END Delete;

PROCEDURE Recall*(VAR B: Texts.Buffer);
BEGIN B := TBuf;  NEW(TBuf);  Texts.OpenBuf(TBuf)
END Recall;

(*------------------message handling------------------------*)

PROCEDURE RemoveMarks (F: Frame);
BEGIN RemoveCaret(F);  RemoveSelection(F) END RemoveMarks;

PROCEDURE NotifyDisplay* (T: Texts.Text;  op: INTEGER;  beg, end: INTEGER);
VAR M: UpdateMsg;
BEGIN
  w.sl("NotifyDisplay.");
  M.id := op;  M.text := T;  M.beg := beg;  M.end := end;  Viewers.Broadcast(M)
END NotifyDisplay;

PROCEDURE Call* (F: Frame;  pos: INTEGER;  new: BOOLEAN);
BEGIN  (*Oberon.Call(F, F.text, pos, new)*)
END Call;

PROCEDURE Write* (F: Frame;  ch: CHAR;  fnt: Fonts.Font;  col, voff: INTEGER);
VAR buf: Texts.Buffer;
BEGIN ASSERT(F.hasCaret);
  CASE ch OF
  |BS:          IF F.caretLoc.pos > F.org THEN                               (*backspace*)
                  Texts.Delete(F.text, F.caretLoc.pos - 1, F.caretLoc.pos, DelBuf);
                  SetCaretPos(F, F.caretLoc.pos - 1)
                END
  |CtrlA:       IF F.hasSel THEN FlipSelection(F, F.selbeg, F.selend) END;   (*select all*)
                F.selbeg.org := 0;  F.selbeg.pos := 0;  F.selbeg.x := F.left;
                F.selend.org := F.text.length;  F.selend.pos := F.text.length;
                F.selend.x := F.left;
                FlipSelection(F, F.selbeg, F.selend);
                F.time := Oberon.Time();  F.hasSel := TRUE
  |CtrlC:       IF F.hasSel THEN                                             (*copy*)
                  NEW(TBuf);  Texts.OpenBuf(TBuf);
                  Texts.Save(F.text, F.selbeg.pos, F.selend.pos, TBuf)
                END
  |CtrlV:       NEW(buf);  Texts.OpenBuf(buf);  Texts.Copy(TBuf, buf);       (*paste*)
                Texts.Insert(F.text, F.caretLoc.pos, buf);
                SetCaretPos(F, F.caretLoc.pos + TBuf.length)
  |CtrlX:       IF F.hasSel THEN                                             (*cut*)
                  NEW(TBuf);  Texts.OpenBuf(TBuf);
                  Texts.Delete(F.text, F.selbeg.pos, F.selend.pos, TBuf)
                END
  |CursorLeft:  IF F.caretLoc.pos > 0 THEN
                  RemoveCaret(F);  SetCaretPos(F, F.caretLoc.pos - 1)
                END
  |CursorRight: IF F.caretLoc.pos < F.text.length THEN
                  RemoveCaret(F);  SetCaretPos(F, F.caretLoc.pos + 1)
                END
  |CursorUp:    RemoveCaret(F);
                SetCaretPos(F, Pos(F, F.X + F.caretLoc.x,
                                   F.Y + F.caretLoc.y - (F.ascent + F.descent + F.lead)))
  |CursorDown:  RemoveCaret(F);
                SetCaretPos(F, Pos(F, F.X + F.caretLoc.x,
                                   F.Y + F.caretLoc.y + F.ascent + F.descent + F.lead))
  |CR, TAB,
   " "..DEL:    KW.fnt := fnt;  KW.col := col;  KW.voff := voff;
                Texts.Write(KW, ch);
                Texts.Insert(F.text, F.caretLoc.pos, KW.buf);
                SetCaretPos(F, F.caretLoc.pos + 1)
  END
END Write;

PROCEDURE Defocus* (F: Frame);
BEGIN RemoveCaret(F)
END Defocus;

PROCEDURE Neutralize* (F: Frame);
BEGIN RemoveMarks(F)
END Neutralize;

PROCEDURE Modify* (F: Frame;  id, Y, H: INTEGER);
VAR dY: INTEGER;
BEGIN
  w.sl("TextFrames Modify handler.");
  MarkThumb(F, FALSE);  RemoveMarks(F);  SetChangeMark(F, FALSE);
  IF id = Viewers.restore THEN Restore(F)
  ELSIF id = Viewers.modify THEN
    Restore(F);
    (*
    dY := Y + H - F.Y - F.H;
    IF H > F.H THEN (*extend*)
      IF dY # 0 THEN Display.CopyBlock(F.X, F.Y, F.W, F.H, F.X,
                                       F.Y + dY, 0);  F.Y := F.Y + dY END;
      Extend(F, Y)
    ELSIF H < F.H THEN (*reduce*)
      Reduce(F, F.Y + F.H - H);
      IF dY # 0 THEN Display.CopyBlock(F.X, F.Y, F.W, F.H, F.X, Y, 0);  F.Y := Y END
    ELSIF dY # 0 THEN (*move*) Display.CopyBlock(F.X, F.Y, F.W, F.H, F.X, Y, 0);  F.Y := Y
    END
    *)
  END;
  IF F.H > 0 THEN MarkThumb(F, TRUE);  SetChangeMark(F, F.text.changed) END
END Modify;

PROCEDURE Open* (F: Frame;  H: Display.Handler;  T: Texts.Text;
                 org,  col,   voff:        INTEGER;
                 left, right, top, bottom: INTEGER;
                 ascent, descent, lead:    INTEGER);
VAR l: Line;
BEGIN
  w.s("TextFrames.Open l "); w.i(left);
  w.s(", r "); w.i(right);
  w.s(", t "); w.i(top);
  w.s(", b "); w.i(bottom);
  w.sl(".");


  NEW(l);  (* Create a sentinel line for a closed  list of lines *)
  l.length     := 0;
  l.width      := 0;
  l.eot        := FALSE;
  l.next       := l;
  l.ascent     := 1;
  l.descent    := 1;
  l.lead       := 1;
  F.sentinel   := l;

  F.handle     := H;        F.text    := T;
  F.org        := org;      F.ascent  := ascent;
  F.descent    := descent;  F.lead    := lead;
  F.colour     := col;      F.voff    := voff;
  F.top        := top;      F.left    := left;
  F.right      := right;    F.bottom  := bottom;
  F.thumb      := 0;        F.time    := 0;
  F.hasCaret   := FALSE;    F.hasSel  := FALSE;

  NEW(F.changeMark); Oberon.SetSpriteGlyph(F.changeMark, Oberon.BlockGlyph);
  NEW(F.thumbMark);  Oberon.SetSpriteGlyph(F.thumbMark,  Oberon.ThumbGlyph);

  (*
  F.pool    := NIL;
  F.hasMark := FALSE;
  F.hasChangeMark := FALSE
  *)
END Open;

PROCEDURE Copy* (F: Frame;  VAR F1: Frame);
BEGIN NEW(F1);
  Open(F1, F.handle, F.text,    F.org, F.colour, F.voff,
           F.left,   F.right,   F.top, F.bottom,
           F.ascent, F.descent, F.lead)
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
  IF F.hasSel THEN
    IF F.text = text THEN
      IF F.selbeg.pos < beg THEN beg := F.selbeg.pos END;  (*leftmost*)
      IF F.time > time THEN end := F.selend.pos;  time := F.time END  (*last selected*)
    ELSIF F.time > time THEN
      text := F.text;  beg := F.selbeg.pos;  end := F.selend.pos;  time := F.time
    END
  END
END GetSelection;

PROCEDURE Update* (F: Frame;  VAR M: UpdateMsg);
BEGIN
  w.sl("TextFrames.Update.");
  (*F.text = M.text*) SetChangeMark(F, FALSE);
  RemoveMarks(F);  Oberon.RemoveMarks(F.X, F.Y, F.W, F.H);
  IF    M.id = replace THEN Replace(F, M.beg, M.end)
  ELSIF M.id = insert  THEN Insert (F, M.beg, M.end)
  ELSIF M.id = delete  THEN Delete (F, M.beg, M.end)
  END;
  SetChangeMark(F, F.text.changed)
END Update;

PROCEDURE Edit* (F: Frame;  X, Y: INTEGER;  Keys: SET);
VAR M:            CopyOverMsg;
    text:         Texts.Text;
    buf:          Texts.Buffer;
    v:            Viewers.Viewer;
    beg, end:     INTEGER;
    time, pos:    INTEGER;
    keysum, keys: SET;
    fnt:          Fonts.Font;
    col, voff:    INTEGER;
    Y0, SL, SR:   INTEGER;
BEGIN
  w.sl("TextFrames.Edit.");
  SL := F.X + Min(F.left, BarWidth);  keys := Keys;
  IF X < SL THEN  (*cursor starts in scroll bar*)
    Oberon.SetMouseGlyph(Oberon.UpdownGlyph);
    (*
    Oberon.DrawMouse(ScrollMarker, X, Y);
    keysum := Keys;
    IF Keys = {2} THEN  (*ML: continuous scroll*)
      Y0 := F.Y + F.H - 1 - F.thumb;  SR := SL + Min(100, F.W DIV 2);
      SetChangeMark(F, FALSE);  RemoveMarks(F);
      WHILE Keys # {} DO Oberon.DrawMouse(ScrollMarker, X, Y);
        IF Y # Y0 THEN Oberon.RemoveMarks(F.X, F.Y, F.W, F.H);
          IF X < SR THEN Show(F, (F.Y + F.H - Y) * (F.text.length) DIV F.H) ELSE Scroll(F, Y0 - Y) END;
          Y0 := Y
        END;
        Input.Mouse(Keys, X, Y)
      END;
      SetChangeMark(F, F.text.changed)
    ELSIF Keys = {1} THEN  (*MM: positional scrolling*) keysum := Keys;
      REPEAT Input.Mouse(Keys, X, Y);  keysum := keysum + Keys;
        Oberon.DrawMouse(ScrollMarker, X, Y)
      UNTIL Keys = {};
      IF keysum # {0, 1, 2} THEN
        IF 0 IN keysum THEN pos := 0
        ELSIF 2 IN keysum THEN pos := Max(F.text.length - 40, 0)
        ELSE pos := (F.Y + F.H - Y) * (F.text.length) DIV F.H
        END;
        SetChangeMark(F, FALSE);
        RemoveMarks(F);  Oberon.RemoveMarks(F.X, F.Y, F.W, F.H);
        Show(F, pos);  SetChangeMark(F, F.text.changed)
      END
    ELSIF Keys = {0} THEN  (*MR: track line and scroll up or down*)
      TrackLine(F, X, Y, pos, keysum);
      IF keysum # {0, 1, 2} THEN
        IF (pos >= 0) & (keysum = {0}) THEN  (*MR, scroll up*)
          SetChangeMark(F, FALSE);
          RemoveMarks(F);  Oberon.RemoveMarks(F.X, F.Y, F.W, F.H);
          Show(F, pos);  SetChangeMark(F, F.text.changed)
        ELSIF (keysum = {0,1}) THEN  (*MR and MM, scroll down*)
          SetChangeMark(F, FALSE);
          RemoveMarks(F);  Oberon.RemoveMarks(F.X, F.Y, F.W, F.H);
          Show(F, F.org*2 - pos - 100);  SetChangeMark(F, F.text.changed)
        END
      END
    END
    *)
  ELSE  (*cursor is in text area*)
    (* Oberon.DrawMouseArrow(X, Y); (Mouse arrow is drawn by default *)
    w.s("TextFrames.Edit cursor in text area, keys $"); w.h(SYSTEM.VAL(INTEGER, Keys)); w.sl(".");
    IF 0 IN Keys THEN  (*MR: select*)
      TrackSelection(F, X, Y, keysum);
      IF F.hasSel THEN
        IF keysum = {0, 2} THEN  (*MR, ML: delete text*)
          Oberon.GetSelection(text, beg, end, time);
          Texts.Delete(text, beg, end, TBuf);
          Oberon.PassFocus(Viewers.This(F.X, F.Y));  SetCaretPos(F, beg)
        ELSIF keysum = {0, 1} THEN  (*MR, MM: copy to caret*)
          Oberon.GetSelection(text, beg, end, time);
          M.text := text;  M.beg := beg;  M.end := end;
          Viewers.FocusViewer.handle(Viewers.FocusViewer, M)
        END
      END
    ELSIF 1 IN Keys THEN  (*MM: call*)
      TrackWord(F, X, Y, pos, keysum);
      IF (pos >= 0) & ~(0 IN keysum) THEN Call(F, pos, 2 IN keysum) END
    ELSIF 2 IN Keys THEN  (*ML: set caret*)
      w.sl("  ML: set caret");
      Oberon.PassFocus(Viewers.This(F.X, F.Y));
      TrackCaret(F, X, Y, keysum);
      IF keysum = {2, 1} THEN  (*ML, MM: copy from selection to caret*)
        Oberon.GetSelection(text, beg, end, time);
        IF time >= 0 THEN
          NEW(TBuf);  Texts.OpenBuf(TBuf);
          Texts.Save(text, beg, end, TBuf);  Texts.Insert(F.text, F.caretLoc.pos, TBuf);
          SetSelection(F, F.caretLoc.pos, F.caretLoc.pos + (end  - beg));
          SetCaretPos(F, F.caretLoc.pos + (end - beg))
        ELSIF TBuf # NIL THEN
          NEW(buf);  Texts.OpenBuf(buf);
          Texts.Copy(TBuf, buf);  Texts.Insert(F.text, F.caretLoc.pos, buf);
          SetCaretPos(F, F.caretLoc.pos + buf.length)
        END
      ELSIF keysum = {2, 0} THEN  (*ML, MR: copy looks*)
        Oberon.GetSelection(text, beg, end, time);
        IF time >= 0 THEN
          Texts.Attributes(F.text, F.caretLoc.pos, fnt, col, voff);
          IF fnt # NIL THEN Texts.ChangeLooks(text, beg, end, {0,1,2}, fnt, col, voff) END
        END
      END
    END
  END;
  (*
  IF keys # {} THEN F.pool := NIL END
  *)
END Edit;

PROCEDURE Handle* (F: Display.Frame;  VAR M: Display.FrameMsg);
VAR F1: Frame;
BEGIN
  w.sl("TextFrames.Handle:");
  CASE F OF Frame:
    CASE M OF
    Oberon.InputMsg:     w.sl("  Oberon.InputMsg");
                         IF    M.id = Oberon.track THEN Edit(F, M.X, M.Y, M.keys)
                         ELSIF M.id = Oberon.consume THEN
                           IF F.hasCaret THEN Write(F, M.ch, M.fnt, M.col, M.voff) END
                         END |
    Oberon.ControlMsg:   w.sl("  Oberon.ControlMsg");
                         IF    M.id = Oberon.defocus THEN Defocus(F)
                         ELSIF M.id = Oberon.neutralize THEN Neutralize(F)
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

(*creation*)

PROCEDURE Menu (name, commands: ARRAY OF CHAR): Texts.Text;
VAR T: Texts.Text;
BEGIN NEW(T);
  T.notify := NotifyDisplay;
  Texts.Open(T, "");
  Texts.SetColor(W, Display.black);
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
       0, Display.white, 0,
       Left DIV 4, 0, 0, 0,
       Ascent, Descent, Lead);
RETURN F END NewMenu;

PROCEDURE NewText* (text: Texts.Text;  pos: INTEGER): Frame;
VAR F: Frame;
BEGIN NEW(F);
  Open(F, Handle, text,
       pos, Display.black, 0,
       Left, Right, Top, Bottom,
       Ascent, Descent, Lead);
RETURN F END NewText;

BEGIN NEW(TBuf);  NEW(DelBuf);
  w.sl("TextFrames initialising.");
  Texts.OpenBuf(TBuf);
  Texts.OpenBuf(DelBuf);

  (* Set default frame metrics *)
  Ascent     := Fonts.Default.ascent;
  Descent    := Fonts.Default.descent;
  Lead       := Fonts.Default.lead;
  EolWidth   := (Ascent + Descent) DIV 2;
  BarWidth   := Ascent + Descent;
  MenuHeight := Ascent + Descent + 1;
  Left       := BarWidth + (Ascent + Descent) DIV 2;
  Right      := (Ascent + Descent) DIV 2;
  Top        := (Ascent + Descent) DIV 2;
  Bottom     := (Ascent + Descent) DIV 2;

  (*
  selH  := lsp;
  markW := BarWidth - 5;
  EolWidth  := lsp DIV 2;

  ScrollMarker.Fade := FlipUpDown;
  ScrollMarker.Draw := FlipUpDown;
  *)

  Texts.OpenWriter(W);  Texts.SetColor(W,  Display.white);
  Texts.OpenWriter(KW); Texts.SetColor(KW, Display.white);
  w.sl("TextFrames initialised.");
  w.s("BarWidth "); w.i(BarWidth); w.sl(".");
END TextFrames.
