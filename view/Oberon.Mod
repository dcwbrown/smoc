MODULE Oberon; (*JG 6.9.90 / 23.9.93 / 13.8.94 / NW 14.4.2013 / 22.12.2015 / AP 1.9.22 Extended Oberon / DCWB 8May23 *)
IMPORT SYSTEM, Kernel,  Windows, Files, Modules, Input,   Display,
               Viewers, Fonts, Texts,   o := Writer;

CONST
  consume*    = 0;  (*message ids*)
  track*      = 1;
  defocus*    = 0;
  neutralize* = 1;
  mark*       = 2;

  off    = 0;       (*task states*)
  idle   = 1;
  active = 2;

  BasicCycle = 20;
  TaskPeriod = 500;  (*milliseconds*)

  TAB     = 09X;
  LF      = 0AX;
  CR      = 0DX;
  ESC     = 1BH;
  SETSTAR = 1AH;

TYPE
  ARGB = SYSTEM.CARD32;

  Sprite* = POINTER TO SpriteDesc;  (* Used for curors and other markers *)
  SpriteDesc = RECORD
    glyph:  Fonts.Glyph;        (* Determines render image and save size *)
    x:      INTEGER;            (* In whole pixels                       *)
    y:      INTEGER;            (* In whole pixels                       *)
    save:   ARRAY 256 OF ARGB;  (* Saved background - enough for 16x16   *)
    next:   Sprite              (* Next in list of enabled sprites       *)
  END;

  InputMsg* = RECORD (Display.FrameMsg)
    id*:    INTEGER;                      (*consume, track*)
    keys*:  SET;
    X*, Y*: INTEGER;
    ch*:    CHAR;
    fnt*:   Fonts.Font;
    col*:   INTEGER;
    voff*:  INTEGER
  END;

  SelectionMsg* = RECORD (Display.FrameMsg)
    time*: INTEGER;
    text*: Texts.Text;
    beg*:  INTEGER;
    end*:  INTEGER
  END;

  ControlMsg* = RECORD (Display.FrameMsg)
    id*, X*, Y*: INTEGER               (*defocus, neutralize, mark*)
  END;

  CopyMsg* = RECORD (Display.FrameMsg)
    F*: Display.Frame
  END;

  Task* = POINTER TO TaskDesc;

  Handler* = PROCEDURE;

  TaskDesc* = RECORD
    state, nextTime, period*: INTEGER;
    next: Task;
    handle: Handler
  END;

  DisplayDumper = PROCEDURE;


VAR
  User*:     ARRAY 8 OF CHAR;
  Password*: INTEGER;
  Log*:      Texts.Text;

  Par*: RECORD
    vwr*:   Viewers.Viewer;
    frame*: Display.Frame;
    text*:  Texts.Text;
    cmd*:   INTEGER;
    pos*:   INTEGER;
    res*:   INTEGER
  END;

  CurFnt*:   Fonts.Font;
  CurCol*:   INTEGER;
  CurOff*:   INTEGER;
  NofTasks*: INTEGER;

  CurTask:    Task;
  DeferTask:  Task;
  DW, DH, CL: INTEGER;
  ActCnt:     INTEGER; (*action count for garbage collector*)
  (*
  Mod:        Modules.Module;
  *)

  defer: BOOLEAN;
  last:  INTEGER;
  W:     Texts.Writer;

  DumpDisplay: DisplayDumper;

  (* Sprite glyphs *)
  ArrowGlyph*:  Fonts.Glyph;
  StarGlyph*:   Fonts.Glyph;
  HookGlyph*:   Fonts.Glyph;
  UpdownGlyph*: Fonts.Glyph;
  BlockGlyph*:  Fonts.Glyph;
  CrossGlyph*:  Fonts.Glyph;
  GreyGlyph*:   Fonts.Glyph;
  ThumbGlyph*:  Fonts.Glyph;

  PrevX: INTEGER;
  PrevY: INTEGER;

  (* There are at most one of each of these 3 sprites                    *)
  Mouse*: Sprite;  (* Mouse pointer represented by Arrow/UpdownGlyph     *)
  Star*:  Sprite;  (* Viewer mark represented by StarGlyph               *)
  Hook*:  Sprite;  (* Text position mark represented by HookGlyph        *)

  EnabledSprites: Sprite;  (* List of sprites showing *)

(*user identification*)

PROCEDURE Code(VAR s: ARRAY OF CHAR): INTEGER;
VAR i: INTEGER;  a, b, c: INTEGER;
BEGIN
  a := 0;  b := 0;  i := 0;
  WHILE s[i] # 0X DO
    c := b;  b := a;  a := (c MOD 509 + 1) * 127 + ORD(s[i]);
    INC(i)
  END;
  IF b >= 32768 THEN b := b - 65536 END;
  RETURN b * 65536 + a
END Code;

PROCEDURE SetUser*(VAR user, password: ARRAY OF CHAR);
BEGIN User := user;  Password := Code(password)
END SetUser;

(*
PROCEDURE Clock*(): INTEGER;
BEGIN RETURN Kernel.Clock()
END Clock;

PROCEDURE SetClock*(d: INTEGER);
BEGIN Kernel.SetClock(d)
END SetClock;
*)

PROCEDURE Time*(): INTEGER;
BEGIN RETURN Kernel.Time()
END Time;


(* --------------------------- Marks and cursors ---------------------------- *)

(* Marks and cursors are represented by 'sprites'. Each sprite is a location  *)
(* on the screen at which to render a glyph.                                  *)
(*                                                                            *)
(* During normal bitmap update by viewers/frames, the sprites are not present *)
(* in the window bitmap.                                                      *)
(*                                                                            *)
(* Sprites are only rendered to the screen bitmap just before the BitBlt to   *)
(* the screen device, and are removed immediately afterwards. (Only sprites   *)
(* that affect the BitBlt area will be rendered and removed.)                 *)
(*                                                                            *)
(* Clients (frames and viewers) add and remove sprites to the sprite list by  *)
(* enabling/disabling them.                                                   *)


PROCEDURE GetPixelGlyphBox(g: Fonts.Glyph; gx, gy: INTEGER; VAR x, y, w, h: INTEGER);
BEGIN  (* Return pixel box relative to origin that contains all glyph subpixels *)
  x := (gx * 4 + g.originX) DIV 4;
  w := (gx * 4 + g.originX + g.mapWidth + 3) DIV 4 - x;
  y := gy - g.baseline;
  h := g.mapHeight;
END GetPixelGlyphBox;


PROCEDURE InvalidateSpriteRect(s: Sprite);
VAR x, y, w, h: INTEGER;
BEGIN
  GetPixelGlyphBox(s.glyph, s.x, s.y, x, y, w, h);
  Windows.InvalidateRect(Display.Window, x, y, w, h);
END InvalidateSpriteRect;


PROCEDURE Enabled(s: Sprite): BOOLEAN;
VAR p: Sprite;
BEGIN p := EnabledSprites;  WHILE (p # NIL) & (p # s) DO p := p.next END;
RETURN p = s END Enabled;


PROCEDURE EnableSprite*(s: Sprite);  (* Does nothing if already enabled *)
BEGIN
  IF ~Enabled(s) THEN
    s.next := EnabledSprites;  EnabledSprites := s;  InvalidateSpriteRect(s)
  END
END EnableSprite;


PROCEDURE DisableSprite*(s: Sprite);  (* Does nothing if already disabled *)
VAR p: Sprite;
BEGIN
  IF EnabledSprites # NIL THEN
    InvalidateSpriteRect(s);
    IF s = EnabledSprites THEN
      EnabledSprites := EnabledSprites.next
    ELSE
      p := EnabledSprites;
      WHILE (p.next # NIL) & (p.next # s) DO p := p.next END;
      IF p.next = s THEN p.next := s.next END
    END
  END;
  s.next := NIL;
END DisableSprite;


PROCEDURE SetSpritePos* (s: Sprite; x, y: INTEGER);
BEGIN
  InvalidateSpriteRect(s);  s.x := x;  s.y := y;  InvalidateSpriteRect(s)
END SetSpritePos;

PROCEDURE SetSpriteGlyph*(s: Sprite; g: Fonts.Glyph);
BEGIN s.glyph := g END SetSpriteGlyph;


(* Convenience functions *)


PROCEDURE SetMouseGlyph* (g: Fonts.Glyph);
BEGIN Mouse.glyph := g END SetMouseGlyph;


PROCEDURE SetStarPos* (x, y: INTEGER);
BEGIN SetSpritePos(Star, x, y);  EnableSprite(Star) END SetStarPos;

PROCEDURE SetHookPos* (x, y: INTEGER);
BEGIN o.s("Set hook pos "); o.i(x); o.c(","); o.i(y); o.sl(".");
SetSpritePos(Hook, x, y);  EnableSprite(Hook) END SetHookPos;

PROCEDURE RemoveMarks*(x, y, w, h: INTEGER);
BEGIN
  IF (Hook.x > x - 8) & (Hook.x < x + w + 8)
   & (Hook.y > y - 8) & (Hook.y < y + h + 8) THEN DisableSprite(Hook) END;

  IF (Star.x > x - 8) & (Star.x < x + w + 8)
   & (Star.y > y - 8) & (Star.y < y + h + 8) THEN DisableSprite(Star) END
END RemoveMarks;


(* ---------------------------- Sprite rendering ---------------------------- *)

PROCEDURE SaveRect(bmp: Windows.Bitmap; x, y, wi, h: INTEGER; VAR buf: ARRAY OF ARGB);
VAR sadr, i, ilim, ylim: INTEGER;
BEGIN ASSERT(wi * h <= LEN(buf));
  (*o.s("SaveRect "); Display.wxy(x,y); o.s(" size "); Display.wxy(wi,h); o.sl(".");*)
  i := 0;  ylim := y + h;
  WHILE y < ylim DO
    sadr := bmp.address + 4 * (bmp.width * y + x);  ilim := i + wi;
    WHILE i < ilim DO SYSTEM.GET(sadr, buf[i]);  INC(i);  INC(sadr, 4); END;
    INC(y);
  END
END SaveRect;

PROCEDURE RestoreRect(buf: ARRAY OF ARGB; bmp: Windows.Bitmap; x, y, wi, h: INTEGER);
VAR tadr, i, ilim, ylim: INTEGER;
BEGIN ASSERT(wi * h <= LEN(buf));
  (*o.s("RestoreRect "); Display.wxy(x,y); o.s(" size "); Display.wxy(wi,h); o.sl(".");*)
  i := 0;  ylim := y + h;
  WHILE y < ylim DO
    tadr := bmp.address + 4 * (bmp.width * y + x);  ilim := i + wi;
    WHILE i < ilim DO SYSTEM.PUT(tadr, buf[i]);  INC(i);  INC(tadr, 4); END;
    INC(y)
  END
END RestoreRect;


(* TODO - only add/remove sprites that affect the paint rectangle *)

PROCEDURE PreRenderSprites;  (* Add enabled sprites to window bitmap *)
VAR p, q: Sprite;  x, y, w, h: INTEGER;
BEGIN
  p := EnabledSprites;  EnabledSprites := NIL;
  WHILE p # NIL DO
    (*o.s("Pre-render sprite $"); o.h(SYSTEM.ADR(p^)); o.s(" at ");  Display.wxy(p.x, p.y); o.sl(".");*)
    GetPixelGlyphBox(p.glyph, p.x, p.y,  x, y, w, h);
    SaveRect(Display.Window.bmp,  x, y, w, h,  p.save);
    Display.RenderAlphaMap(
      p.x * 4 + p.glyph.originX, p.y - p.glyph.baseline,
      p.glyph.mapWidth,          p.glyph.mapHeight,
      p.glyph.map,               0FFFFFFH);
    (* Put sprites back on the EnabledSprites list in reverse order *)
    q := p.next;
    p.next := EnabledSprites;
    EnabledSprites := p;
    p := q
  END
END PreRenderSprites;


PROCEDURE PostRenderSprites;  (* Erase enabled sprites from the window bitmap *)
VAR p, q: Sprite;  x, y, w, h: INTEGER;
BEGIN
  p := EnabledSprites;  EnabledSprites := NIL;
  WHILE p # NIL DO
    (*o.s("Post-render sprite $"); o.h(SYSTEM.ADR(p^)); o.s(" at ");  Display.wxy(p.x, p.y); o.sl(".");*)
    GetPixelGlyphBox(p.glyph, p.x, p.y, x, y, w, h);
    RestoreRect(p.save, Display.Window.bmp, x, y, w, h);
    (* Put sprites back on the EnabledSprites list in reverse order *)
    q := p.next;
    p.next := EnabledSprites;
    EnabledSprites := p;
    p := q
  END
END PostRenderSprites;

(* -------------------------------------------------------------------------- *)




(* PROCEDURE DrawMouse*(s: Sprite;  x, y: INTEGER);                             *)
(* BEGIN DrawSprite(Mouse, s, x, y)                                             *)
(* END DrawMouse;                                                               *)
(*                                                                              *)
(* PROCEDURE DrawMouseArrow*(x, y: INTEGER);                                    *)
(* BEGIN                                                                        *)
(*   o.s("Oberon.DrawMouseArrow("); o.i(x); o.c(","); o.i(y); o.sl(").");       *)
(*   DrawSprite(Mouse, Arrow, x, y)                                             *)
(* END DrawMouseArrow;                                                          *)
(*                                                                              *)
(* PROCEDURE DrawMouseStar*(x, y: INTEGER);                                     *)
(* BEGIN DrawSprite(Mouse, Star, x, y)                                          *)
(* END DrawMouseStar;                                                           *)
(*                                                                              *)
(* PROCEDURE FadeMouse*;                                                        *)
(* BEGIN FadeSprite(Mouse)                                                      *)
(* END FadeMouse;                                                               *)
(*                                                                              *)
(* PROCEDURE MouseOn*(): BOOLEAN;                                               *)
(* BEGIN RETURN Mouse.on                                                        *)
(* END MouseOn;                                                                 *)
(*                                                                              *)
(* PROCEDURE DrawPointer*(s: Sprite;  x, y: INTEGER);                           *)
(* BEGIN DrawSprite(Pointer, s, x, y)                                           *)
(* END DrawPointer;                                                             *)
(*                                                                              *)
(* PROCEDURE DrawPointerArrow*(x, y: INTEGER);                                  *)
(* BEGIN DrawSprite(Pointer, Arrow, x, y)                                       *)
(* END DrawPointerArrow;                                                        *)
(*                                                                              *)
(* PROCEDURE DrawPointerStar*(x, y: INTEGER);                                   *)
(* BEGIN DrawSprite(Pointer, Star, x, y)                                        *)
(* END DrawPointerStar;                                                         *)
(*                                                                              *)
(* PROCEDURE FadePointer*;                                                      *)
(* BEGIN FadeSprite(Pointer)                                                    *)
(* END FadePointer;                                                             *)
(*                                                                              *)
(*                                                                              *)
(* PROCEDURE PointerOn*(): BOOLEAN;                                             *)
(* BEGIN o.sl("PointerOn.");                                                    *)
(*   o.s("Pointer at $"); o.h(SYSTEM.ADR(Pointer)); o.sl(".");                  *)
(*   o.s("Pointer is ");  IF Pointer.on THEN o.sl("on.") ELSE o.sl("off.") END; *)
(*   RETURN Pointer.on                                                          *)
(* END PointerOn;                                                               *)
(*                                                                              *)
(* PROCEDURE RemoveMarks*(X, Y, W, H: INTEGER);                                 *)
(* BEGIN                                                                        *)
(*   (*                                                                         *)
(*   IF (Mouse.x > X     - 16)                                                  *)
(*    & (Mouse.x < X + W + 16)                                                  *)
(*    & (Mouse.y > Y     - 16)                                                  *)
(*    & (Mouse.y < Y + H + 16) THEN                                             *)
(*     FadeSprite(Mouse)                                                        *)
(*   END;                                                                       *)
(*   IF (Pointer.X > X     - 8)                                                 *)
(*    & (Pointer.X < X + W + 8)                                                 *)
(*    & (Pointer.Y > Y     - 8)                                                 *)
(*    & (Pointer.Y < Y + H + 8) THEN                                            *)
(*     FadeSprite(Pointer)                                                      *)
(*   END                                                                        *)
(*   *)                                                                         *)
(* END RemoveMarks;                                                             *)


(* ----------------------------- Current looks ------------------------------ *)

PROCEDURE SetFont*(fnt: Fonts.Font);
BEGIN CurFnt := fnt
END SetFont;

PROCEDURE SetColor*(col: INTEGER);
BEGIN CurCol := col
END SetColor;

PROCEDURE SetOffset*(voff: INTEGER);
BEGIN CurOff := voff
END SetOffset;

(*logs and text selections*)

PROCEDURE OpenLog*(T: Texts.Text);
BEGIN Log := T
END OpenLog;

PROCEDURE GetSelection*(VAR text: Texts.Text;  VAR beg, end, time: INTEGER); (*from current display*)
VAR M: SelectionMsg;
BEGIN M.time := -1;  Viewers.Broadcast(M);  time := M.time;
  IF time >= 0 THEN text := M.text;  beg := M.beg;  end := M.end END
END GetSelection;

(*display management*)

PROCEDURE HandleFiller (V: Display.Frame;  VAR M: Display.FrameMsg);
BEGIN
  o.sl("Oberon.HandleFiller.");
  CASE M OF
  | InputMsg:   IF M.id = track THEN (* Mouse position is updated by top level handler *)END
  | ControlMsg: IF M.id = mark  THEN SetSpritePos(Star,  M.X, M.Y);  EnableSprite(Star) END
  | Viewers.ViewerMsg:
    IF (M.id = Viewers.restore) & (V.W > 0) & (V.H > 0) THEN
      RemoveMarks(V.X, V.Y, V.W, V.H);
      Windows.FillRectangle(Display.Window.bmp, V.X, V.Y, V.W, V.H, 0)
    ELSIF M.id = Viewers.modify THEN
      IF M.Y < V.Y THEN
        RemoveMarks(V.X, M.Y, V.W, V.Y - M.Y);
        Windows.FillRectangle(Display.Window.bmp, V.X, M.Y, V.W, V.Y - M.Y, 0)
      ELSIF M.Y > V.Y THEN RemoveMarks(V.X, V.Y, V.W, M.Y - V.Y)
      END
    END
  END
END HandleFiller;

PROCEDURE InitTrack*(D: Viewers.DisplayArea;  W, H: INTEGER); (*add new track to the right*)
VAR Filler: Viewers.Viewer;
BEGIN NEW(Filler);  Filler.handle := HandleFiller;  Viewers.InitTrack(D, W, H, Filler)
END InitTrack;

PROCEDURE OpenTrack*(D: Viewers.DisplayArea;  X, W: INTEGER); (*create overlay track at X*)
VAR Filler: Viewers.Viewer;
BEGIN NEW(Filler);  Filler.handle := HandleFiller;  Viewers.OpenTrack(D, X, W, Filler)
END OpenTrack;

PROCEDURE InitDisplay*(D: Viewers.DisplayArea;  name: ARRAY OF CHAR);
BEGIN Viewers.InitDisplay(D, DW, DH, name);
  InitTrack(D, DW DIV 8 * 5, DH);
  InitTrack(D, DW DIV 8 * 3, DH)
END InitDisplay;

PROCEDURE SetDisplay*(D: Viewers.DisplayArea);
BEGIN Viewers.SetDisplay(D);  Input.SetMouseLimits(D.curW, D.H)
END SetDisplay;

PROCEDURE CloseDisplay*(D: Viewers.DisplayArea;  hint: INTEGER);
VAR prev: Viewers.DisplayArea;
BEGIN prev := Viewers.CurDisplay;  Viewers.CloseDisplay(D, hint);
  IF Viewers.CurDisplay # prev THEN
    Input.SetMouseLimits(Viewers.CurDisplay.curW, Viewers.CurDisplay.H)
  END
END CloseDisplay;

PROCEDURE DisplayWidth*(X: INTEGER): INTEGER;
BEGIN RETURN DW
END DisplayWidth;

PROCEDURE DisplayHeight*(X: INTEGER): INTEGER;
BEGIN RETURN DH
END DisplayHeight;

PROCEDURE MarkedViewer*(): Viewers.Viewer;
BEGIN RETURN Viewers.This(Star.x, Star.y)
END MarkedViewer;

PROCEDURE PassFocus*(V: Viewers.Viewer);
VAR M: ControlMsg;
BEGIN
  IF Viewers.FocusViewer # NIL  THEN
    o.sl("  defocus.");
    M.id := defocus;  Viewers.FocusViewer.handle(Viewers.FocusViewer, M)
  END;
  Viewers.SetFocus(Viewers.ThisDisplay(V), V)
END PassFocus;

PROCEDURE FocusViewer(): Viewers.Viewer;
BEGIN RETURN Viewers.FocusViewer
END FocusViewer;

(*standard Oberon display with user track and system track, and filler viewer*)

PROCEDURE UserTrack*(X: INTEGER): INTEGER;
BEGIN RETURN X DIV DW * DW
END UserTrack;

PROCEDURE SystemTrack*(X: INTEGER): INTEGER;
BEGIN RETURN X DIV DW * DW + DW DIV 8 * 5
END SystemTrack;

PROCEDURE UY (X: INTEGER): INTEGER;
VAR y: INTEGER;
    fil, top, bot, alt, max: Display.Frame;
BEGIN Viewers.Locate(Viewers.CurDisplay, X, 0, fil, top, bot, alt, max);
  IF fil.H >= DH DIV 8 THEN y := DH ELSE y := max.Y + max.H DIV 2 END;
  RETURN y
END UY;

PROCEDURE AllocateUserViewer*(DX: INTEGER;  VAR X, Y: INTEGER);
BEGIN
  IF Enabled(Star) THEN X := Star.x;  Y := Star.y
  ELSE X := DX DIV DW * DW;  Y := UY(X)
  END
END AllocateUserViewer;

PROCEDURE SplitY (X: INTEGER): INTEGER;
VAR
  H0,  H1,  H2,  H3,  y:   INTEGER;
  fil, top, bot, alt, max: Display.Frame;
BEGIN
  o.s("Oberon.SplitY(X "); o.i(X); o.s(") starting.");
  Viewers.Locate(Viewers.CurDisplay, X, DH, fil, top, bot, alt, max);
  H3 := DH - DH DIV 3;  (* H3 = 2/3  DH *)  (* DH = Display.Height *)
  H2 := H3 - H3 DIV 2;  (* H2 = 1/3  DH *)
  H1 := DH DIV 5;       (* H1 = 1/5  DH *)
  H0 := DH DIV 10;      (* H0 = 1/10 DH *)
  IF    fil.H >= DH DIV 8 THEN y := fil.Y
  ELSIF max.H >= DH - H0  THEN y := max.Y + max.H - H3
  ELSIF max.H >= H3 - H0  THEN y := max.Y + max.H - H2
  ELSIF max.H >= H2 - H0  THEN y := max.Y + max.H - H1
                          ELSE y := max.Y + max.H DIV 2
  END;
  o.s("Oberon.SplitY returning "); o.i(y); o.sl(".");
  RETURN y
END SplitY;

PROCEDURE AllocateSystemViewer*(DX: INTEGER;  VAR X, Y: INTEGER);
BEGIN
  o.sl("Oberon.AllocateSystemViewer starting.");
  IF Enabled(Star) THEN
    X := Star.x;  Y := Star.y
  ELSE
    (*X := DX DIV DW * DW + DW DIV 8 * 5;*)

    o.sl("1.");
    X := DX DIV DW * DW;
    o.sl("2.");
    INC(X, DW DIV 8 * 5);
    o.sl("3.");

    Y := SplitY(X)
  END;
  o.s("Oberon.AllocateSystemViewer returning X ");  o.i(X);
  o.s(", Y ");  o.i(Y);  o.sl(".");
END AllocateSystemViewer;

(*garbage collection*)


PROCEDURE CollectGarbage;
(*
VAR mod: Modules.Module;
*)
BEGIN
  (*o.sl(".. collect garbage ..");*)
  (*
  IF (ActCnt <= 0) OR (Kernel.allocated >= Kernel.heapLim - Kernel.heapOrg - 10000H) THEN
    mod := Modules.root;  LED(21H);
    WHILE mod # NIL DO
      IF mod.name[0] # 0X THEN Kernel.Mark(mod.ptr) END;
      mod := mod.next
    END;
    LED(23H);
    Files.RestoreList;  LED(27H);
    Kernel.Collect;  LED(20H);
    Modules.Collect;
    ActCnt := BasicCycle
  END
  *)
END CollectGarbage;


(*command interpretation*)

PROCEDURE SetPar*(F: Display.Frame;  T: Texts.Text;  cmd, pos: INTEGER;  res: INTEGER);
BEGIN
  Par.vwr   := Viewers.This(F.X, F.Y);
  Par.frame := F;
  Par.text  := T;
  Par.cmd   := cmd;
  Par.pos   := pos;
  Par.res   := res
END SetPar;

PROCEDURE ClearPar*;
BEGIN
  Par.vwr   := NIL;
  Par.frame := NIL;
  Par.text  := NIL;
  Par.cmd   := 0;
  Par.pos   := 0;
  Par.res   := 0
END ClearPar;

PROCEDURE Return*(n: INTEGER);
BEGIN Par.res := n
END Return;

PROCEDURE Skip(VAR S: Texts.Scanner;  VAR len: INTEGER);  (*skip and count white spaces*)
VAR ch: CHAR;  i: INTEGER;
BEGIN Texts.Read(S, ch);  i := 0;
  WHILE (ch = " ") OR (ch = TAB) OR (ch = CR) OR (ch = LF) DO
    IF ch = CR THEN INC(S.line) END;
    Texts.Read(S, ch);  INC(i)
  END;
  S.nextCh := ch;  len := i
END Skip;

PROCEDURE Scan(VAR S: Texts.Scanner);
VAR ch: CHAR;  i: INTEGER;
BEGIN ch := S.nextCh;
  IF ("A" <= ch) & (ch <= "Z")
  OR ("a" <= ch) & (ch <= "z")
  OR (ch = "*")
  OR ("0" <= ch) & (ch <= "9") THEN i := 0;
    REPEAT S.s[i] := ch;  INC(i);  Texts.Read(S, ch)
    UNTIL ((ch < "0") & (ch # ".") & (ch # "*")
       OR ("9" < ch) & (ch < "A")
       OR ("Z" < ch) & (ch < "a")
       OR ("z" < ch)) OR (i = 31);
    S.s[i] := 0X;  S.length := i;  S.class := Texts.Name;  S.nextCh := ch
  ELSE S.class := Texts.Inval
  END
END Scan;

(*
PROCEDURE Call*(F: Display.Frame;  T: Texts.Text;  pos: INTEGER;  new: BOOLEAN);
VAR S: Texts.Scanner;  res, len: INTEGER;
BEGIN Texts.OpenScanner(S, T, pos);  Skip(S, len);  Scan(S);
  IF (S.class = Texts.Name) & (S.line = 0) THEN
    SetPar(F, T, pos, pos + len + S.len, 0);  Modules.Call(S.s, res);
    IF (res > Modules.noerr) & (res < Modules.clients) & (Log # NIL) THEN
      Texts.WriteString(W, "Call error: ");
      CASE res OF
         Modules.nofile:     Texts.WriteString(W, Modules.importing);
                             Texts.WriteString(W, " module not found")
       | Modules.badversion: Texts.WriteString(W, Modules.importing);
                             Texts.WriteString(W, " bad version")
       | Modules.badkey:     Texts.WriteString(W, Modules.importing);
                             Texts.WriteString(W, " imports ");
                             Texts.WriteString(W, Modules.imported);
                             Texts.WriteString(W, " with bad key")
       | Modules.badfile:    Texts.WriteString(W, Modules.importing);
                             Texts.WriteString(W, " corrupted obj file")
       | Modules.nospace:    Texts.WriteString(W, Modules.importing);
                             Texts.WriteString(W, " insufficient space")
       | Modules.nocmd:      Texts.WriteString(W, Modules.importing);
                             Texts.WriteString(W, " command not found")
       | Modules.badcmd:     Texts.WriteString(W, S.s);
                             Texts.WriteString(W, " invalid command")
       | Modules.nomod:      Texts.WriteString(W, Modules.importing);
                             Texts.WriteString(W, " module not found")
       | Modules.badfin:     Texts.WriteString(W, S.s);
                             Texts.WriteString(W, " invalid finalization")
      END;
      Par.res := res;  Texts.WriteLn(W);  Texts.Append(Log, W.buf)
    END
  END
END Call;

PROCEDURE Execute(F: Display.Frame;  T: Texts.Text;  pos: INTEGER;  collect: BOOLEAN);  (*execute multiple commands separated by ~*)
VAR S: Texts.Scanner;  len: INTEGER;  continue: BOOLEAN;
BEGIN continue := pos >= 0;
  WHILE continue DO
    Texts.OpenScanner(S, T, pos);  Skip(S, len);  Scan(S);  continue := FALSE;
    IF (S.class = Texts.Name) & (S.s # "Oberon.Batch") THEN S.line := 0;
      Call(F, T, pos + len, FALSE);
      IF collect THEN ActCnt := 0;  CollectGarbage END;
      IF Par.res = 0 THEN
        REPEAT Texts.Scan(S) UNTIL S.eot OR (S.class = Texts.Char) & (S.c = "~");
        IF ~S.eot THEN pos := Texts.Pos(S);  continue := TRUE END
      END
    END
  END
END Execute;

PROCEDURE Run*(F: Display.Frame;  T: Texts.Text;  pos: INTEGER);  (*does not invoke garbage collector*)
BEGIN Execute(F, T, pos, FALSE)
END Run;

PROCEDURE Batch*;  (*invokes garbage collector after each executed command*)
VAR S: Texts.Scanner;
BEGIN Texts.OpenScanner(S, Par.text, Par.cmd);  Texts.Scan(S);
  Execute(Par.frame, Par.text, Par.pos, (S.class = Texts.Name) & (S.s = "Oberon.Batch"))
END Batch;
*)

(*task management*)

PROCEDURE NewTask*(h: Handler;  period: INTEGER): Task;
VAR t: Task;
BEGIN NEW(t);  t.state := off;  t.next := t;  t.handle := h;  t.period := period;
RETURN t END NewTask;

PROCEDURE Install*(T: Task);
BEGIN
  IF T.state = off THEN
    T.next := CurTask.next;  CurTask.next := T;  T.state := idle;  T.nextTime := 0;
    INC(NofTasks)
  END
END Install;

PROCEDURE Remove*(T: Task);
VAR t: Task;
BEGIN
  IF T.state # off THEN t := T;
    WHILE t.next # T DO t := t.next END;
    t.next := T.next;  T.state := off;  T.next := NIL;  CurTask := t;  DEC(NofTasks)
  END
END Remove;

PROCEDURE Collect*(count: INTEGER);
BEGIN ActCnt := count
END Collect;

(* --------------------- Windows event handler mapping ---------------------- *)

PROCEDURE ws(s: ARRAY OF CHAR);
BEGIN Texts.WriteString(W, s) END ws;

PROCEDURE wl;
BEGIN Texts.WriteLn(W);  Texts.Append(Log, W.buf); END wl;

PROCEDURE SetDumpDisplay* (proc: DisplayDumper);
BEGIN DumpDisplay := proc END SetDumpDisplay;

PROCEDURE DoCharacter(ch: INTEGER);
VAR N: ControlMsg;  V: Viewers.Viewer;  M: InputMsg;
BEGIN

  Windows.Invalidate(Display.Window);  (* Testing *)

  IF ch = 1BH THEN (* ESC - TestOberon specific ... *)
    Windows.Quit
  ELSIF ch = 19H THEN  (* Ctrl/Y - originally ESC *)
    N.id := neutralize;  Viewers.Broadcast(N);
    DisableSprite(Hook);  DisableSprite(Star);
    (*LED(0)*)
  ELSIF ch = SETSTAR THEN  (* Ctrl/Z *)
    N.id := mark;  N.X := Mouse.x;  N.Y := Mouse.y;
    V := Viewers.This(Mouse.x, Mouse.y);  V.handle(V, N)
  ELSE
    M.id  := consume;  M.ch   := CHR(ch);  M.fnt := CurFnt;
    M.col := CurCol;   M.voff := CurOff;
    V := Viewers.FocusViewer;
    o.s("Oberon.DoCharacter focus viewer is $"); o.h(SYSTEM.ADR(V^)); o.sl(".");
    V.handle(V, M);
    DEC(ActCnt)
  END
END DoCharacter;


PROCEDURE PreDraw(x, y, width, height: INTEGER;  bitmap: Windows.Bitmap);
BEGIN PreRenderSprites END PreDraw;


PROCEDURE PostDraw(x, y, width, height: INTEGER;  bitmap: Windows.Bitmap);
BEGIN PostRenderSprites END PostDraw;


(* --- flags values: from Windows WM_MOUSEMOVE wParam --- *)
(* MK_LBUTTON   0x0001  The left mouse button is down.    *)
(* MK_RBUTTON   0x0002  The right mouse button is down.   *)
(* MK_SHIFT     0x0004  The SHIFT key is down.            *)
(* MK_CONTROL   0x0008  The CTRL key is down.             *)
(* MK_MBUTTON   0x0010  The middle mouse button is down.  *)
(* MK_XBUTTON1  0x0020  The first X button is down.       *)
(* MK_XBUTTON2  0x0040  The second X button is down.      *)

PROCEDURE DoMouse(x, y: INTEGER;  flags: SET);  (* {0} MR, {1} MM, {2} ML *)
VAR keys: SET;  M: InputMsg;  V: Viewers.Viewer;
BEGIN
  o.s("DoMouse("); o.i(x); o.c(","); o.i(y);
  o.s(",$"); o.h(SYSTEM.VAL(INTEGER, flags)); o.sl(").");

  InvalidateSpriteRect(Mouse);  (* Make sure erasure of old mouse position gets displayed *)

  Mouse.glyph := ArrowGlyph;  (* Default shape - viewers may override this *)
  Mouse.x := x;
  Mouse.y := y;

  M.id := track;  M.X := x;  M.Y := y;  M.keys := {};
  IF 1 IN flags THEN INCL(M.keys, 0) END;  (* MR *)
  IF 4 IN flags THEN INCL(M.keys, 1) END;  (* MM *)
  IF 0 IN flags THEN INCL(M.keys, 2) END;  (* ML *)

  IF (M.keys # {}) THEN
    V := Viewers.This(x, y);  V.handle(V, M);
    DEC(ActCnt)
  ELSE
    IF (x # PrevX) OR (y # PrevY) THEN
      M.id := track;
      IF y >= DH THEN y := DH END;
      M.Y := y;
      V := Viewers.This(x, y);  V.handle(V, M);
      PrevX := x;  PrevY := y
    END
  END;

  InvalidateSpriteRect(Mouse);  (* Make sure any updated mouse position gets displayed *)
END DoMouse;


PROCEDURE Loop*;
VAR res: INTEGER;  time, wait: INTEGER;  prev: Task;
(*
VAR V: Viewers.Viewer;  M: InputMsg;  N: ControlMsg;
    prevX, prevY, X, Y, t: INTEGER;  keys: SET;  ch: CHAR;
*)
BEGIN
  (*
  REPEAT
    Input.Mouse(keys, X, Y);
    IF Input.Available() > 0 THEN
      Input.Read(ch);
      IF    ch = ESC     THEN  N.id := neutralize;  Viewers.Broadcast(N);
                               FadePointer;  LED(0)
      ELSIF ch = SETSTAR THEN  N.id := mark;  N.X := X;  N.Y := Y;
                               V := Viewers.This(X, Y);  V.handle(V, N)
      ELSE
        M.id  := consume;  M.ch   := ch;  M.fnt := CurFnt;
        M.col := CurCol;   M.voff := CurOff;
        V := Viewers.FocusViewer;  V.handle(V, M);
        DEC(ActCnt)
      END
    ELSIF keys # {} THEN
      M.id := track;  M.X := X;  M.Y := Y;  M.keys := keys;
      REPEAT
        V := Viewers.This(M.X, M.Y);  V.handle(V, M);
        Input.Mouse(M.keys, M.X, M.Y)
      UNTIL M.keys = {};
      DEC(ActCnt)
    ELSE
      IF (X # prevX) OR (Y # prevY) OR ~MouseOn() THEN
        M.id := track;  M.X := X;
        IF Y >= DH THEN Y := DH END;
        M.Y := Y;  M.keys := keys;
        V := Viewers.This(X, Y);  V.handle(V, M);
        prevX := X;  prevY := Y
      END;
      CurTask := CurTask.next;  t := Kernel.Time();
      IF t >= CurTask.nextTime THEN
        CurTask.nextTime := t + CurTask.period;
        CurTask.state := active;  CurTask.handle;  CurTask.state := idle
      END
    END
  UNTIL FALSE
  *)

  REPEAT
    res := Windows.ProcessOneMessage();
    IF res = 0 THEN  (* Empty queue *)
      (* Find first ready task, or soonest ready if none ready now. *)
      time    := Kernel.Time();
      prev    := CurTask;
      wait    := CurTask.nextTime - time;
      CurTask := CurTask.next;
      WHILE (CurTask # prev) & (CurTask.nextTime > time) DO
        IF CurTask.nextTime - time < wait THEN wait := CurTask.nextTime - time END;
        CurTask := CurTask.next
      END;
      IF CurTask.nextTime <= time THEN  (* There is a task ready to run *)
        CurTask.nextTime := time + CurTask.period;
        CurTask.state := active;  CurTask.handle;  CurTask.state := idle
      ELSE (* There is nothing to do but wait for time or message *)
        CurTask := CurTask.next;
        (*o.s("Waiting "); o.i(wait); o.sl("ms.");*)
        Windows.WaitMsgOrTime(wait)
      END
    END
  UNTIL res > 1  (* => WM_QUIT *)
END Loop;

PROCEDURE Reset*;
BEGIN
  IF CurTask.state = active THEN Remove(CurTask) END;
  (*
  SYSTEM.LDREG(14, Kernel.stackOrg); (*reset stack pointer*)
  *)
  Loop
END Reset;

(*
PROCEDURE Trap(VAR a: INTEGER;  b, c, d: INTEGER);
VAR u, v, w: INTEGER;  mod: Modules.Module;
BEGIN u := SYSTEM.REG(LNK);  (*return address, deposited in register LNK by the trap (BLR TR) instruction*)
  SYSTEM.GET(u - 4, v);  (*trap instruction, contains code position and trap number*)
  w := v DIV 10H MOD 10H;  (*trap number*)
  IF w = 0 THEN Kernel.New(a, b, c, d)
  ELSE (*trap*)
    IF Log # NIL THEN
      IF defer THEN (*defer trap handling*) last := u;  Install(DeferTask)
      ELSE defer := TRUE;
        Texts.WriteLn(W);  Texts.WriteString(W, "  pos ");
        Texts.WriteInt(W, v DIV 100H MOD 10000H, 4);  (*code position*)
        Texts.WriteString(W, "  TRAP");  Texts.WriteInt(W, w, 4);  mod := Modules.root;
        WHILE (mod # NIL) & ((u < mod.prg) OR (u >= mod.imp)) DO mod := mod.next END;
        IF mod # NIL THEN Texts.WriteString(W, " in ");  Texts.WriteString(W, mod.name) END;
        Texts.WriteString(W, " at");  Texts.WriteHex(W, u);  Texts.WriteLn(W);
        Texts.Append(Log, W.buf);
        defer := FALSE
      END
    END;
    Collect(0);  Reset
  END
END Trap;
*)

PROCEDURE Abort;
VAR u: INTEGER;  (*mod: Modules.Module;*)
BEGIN
  IF Log # NIL THEN
    (*
    u := SYSTEM.REG(LNK);  (*return address, deposited in register LNK by the abort (BL 0) instruction*)
    *)
    IF defer THEN (*defer abort handling*) last := u;  Install(DeferTask)
    ELSE defer := TRUE;
      Texts.WriteLn(W);  Texts.WriteString(W, "  ABORT  ");
      (*
      mod := Modules.root;
      WHILE (mod # NIL) & ((u < mod.prg) OR (u >= mod.imp)) DO
        mod := mod.next
      END;
      IF mod # NIL THEN
        Texts.WriteString(W, " in ");  Texts.WriteString(W, mod.name)
      END;
      Texts.WriteString(W, " at");  Texts.WriteHex(W, u);
      *)
      Texts.WriteLn(W);
      Texts.Append(Log, W.buf);
      defer := FALSE
    END
  END;
  Collect(0);  Reset
END Abort;

PROCEDURE Deferred;  (*handle trap/abort as soon as the garbage collector has freed up enough heap space*)
VAR v, w, pos: INTEGER;  (*mod: Modules.Module;*)
BEGIN
  (*
  IF (Kernel.allocated < Kernel.heapLim - Kernel.heapOrg - 10000H) & (Log # NIL) THEN
    Remove(DeferTask);
    SYSTEM.GET(last - 4, v);  (*trap instruction, contains code position and trap number*)
    pos := v DIV 100H MOD 10000H;  (*code position*)
    Texts.WriteLn(W);
    IF pos # 0 THEN
      w := v DIV 10H MOD 10H;  (*trap number*)
      Texts.WriteString(W, "  pos ");  Texts.WriteInt(W, pos, 4);
      Texts.WriteString(W, "  RECURSIVE TRAP");  Texts.WriteInt(W, w, 4)
    ELSE Texts.WriteString(W, "  RECURSIVE ABORT  ")
    END;
    mod := Modules.root;
    WHILE (mod # NIL) & ((last < mod.prg) OR (last >= mod.imp)) DO mod := mod.next END;
    IF mod # NIL THEN Texts.WriteString(W, " in ");  Texts.WriteString(W, mod.name) END;
    Texts.WriteString(W, " at");  Texts.WriteHex(W, last);  Texts.WriteLn(W);
    Texts.Append(Log, W.buf);
    defer := FALSE
  END
  *)
END Deferred;

BEGIN
  o.sl("Oberon initialising.");
  User[0] := 0X;

  ArrowGlyph  := Fonts.ConvertOberonPattern(SYSTEM.ADR($ 0F0F 0060 0070 0038 001C 000E 0007 8003 C101 E300 7700 3F00 1F00 3F00 7F00 FF00 $));
  HookGlyph   := Fonts.ConvertOberonPattern(SYSTEM.ADR($ 0C0C 070F 8707 C703 E701 F700 7F00 3F00 1F00 0F00 0700 0300 0100                $));
  UpdownGlyph := Fonts.ConvertOberonPattern(SYSTEM.ADR($ 080E 183C 7EFF 1818 1818 1818 FF7E 3C18                                         $));
  BlockGlyph  := Fonts.ConvertOberonPattern(SYSTEM.ADR($ 0808 FFFF C3C3 C3C3 FFFF                                                        $));
  CrossGlyph  := Fonts.ConvertOberonPattern(SYSTEM.ADR($ 0F0F 0140 0220 0410 0808 1004 2002 4001 0000 4001 2002 1004 0808 0410 0220 0140 $));
  GreyGlyph   := Fonts.ConvertOberonPattern(SYSTEM.ADR($ 2002 5555 5555 AAAA AAAA                                                        $));
  ThumbGlyph  := Fonts.ConvertOberonPattern(SYSTEM.ADR($ 0B14 FF07 FF07 FF07 FF07 FF07 FF07 FF07 FF07 FF07 FF07 FF07 FF07 FF07 FF07 FF07
                                                                                                                FF07 FF07 FF07 FF07 FF07 $));
  StarGlyph   := Fonts.ConvertOberonPattern(SYSTEM.ADR($ 0F0F 8000 8220 8410 8808 9004 A002 C001 7F7F C001 A002 9004 8808 8410 8220 8000 $));
  StarGlyph.originX := -23;  StarGlyph.baseline := 7;

  NEW(Hook);  SetSpriteGlyph(Hook,  HookGlyph);   (* Text position marker *)
  NEW(Star);  SetSpriteGlyph(Star,  StarGlyph);   (* Viewer marker        *)
  NEW(Mouse); SetSpriteGlyph(Mouse, ArrowGlyph);  (* Mouse pointer        *)
  Mouse.x := 10;  PrevX := 10;
  Mouse.y := 10;  PrevY := 10;
  EnableSprite(Mouse);

  DW     := Display.Width;
  DH     := Display.Height;
  CL     := DW;
  CurFnt := Fonts.Default;
  CurCol := Display.white;
  CurOff := 0;
  ActCnt := 0;

  CurTask := NewTask(CollectGarbage, 2000);  Install(CurTask);

  Texts.OpenWriter(W);  Texts.SetColor(W, Display.white);

  Log := NIL;

  defer := FALSE;
  DeferTask := NewTask(Deferred, TaskPeriod);

  (*
  Kernel.Install(SYSTEM.ADR(Trap), Kernel.TrapAdr);
  Kernel.Install(SYSTEM.ADR(Abort), 0);
  *)

  Windows.SetCharacterHandler(Display.Window, DoCharacter);
  Windows.SetMouseHandler(Display.Window, DoMouse);
  Windows.SetDrawHandlers(Display.Window, PreDraw, PostDraw);

  o.sl("Oberon initialised.");
END Oberon.
