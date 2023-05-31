MODULE Edit; (*JG 2.11.90 / NW 28.11.15 / AP 15.9.20 Extended Oberon*)
  IMPORT Files, Display, Viewers, Fonts, Texts, Oberon, MenuViewers, TextFrames, w := Writer;

  CONST
    CR           = 0DX;
    maxlen       = 32;
    StandardMenu = "System.Close System.Copy System.Grow Edit.Search Edit.Store | System.Expand System.Spread System.Clone";

  VAR
    W:    Texts.Writer;
    time: INTEGER;
    M:    INTEGER;
    pat:  ARRAY maxlen OF CHAR;
    d:    ARRAY 256 OF INTEGER;

  PROCEDURE Max(i, j: INTEGER): INTEGER;
    VAR m: INTEGER;
  BEGIN IF i >= j THEN m := i ELSE m := j END ;
    RETURN m
  END Max;

  PROCEDURE FocusViewer(): Viewers.Viewer;  (*for Extended Oberon*)
  BEGIN RETURN Viewers.FocusViewer
  END FocusViewer;

  PROCEDURE Open*;
    VAR T: Texts.Text;
    S: Texts.Scanner;
    V: Viewers.Viewer;
    X, Y: INTEGER;
    beg, end, time: INTEGER;
  BEGIN Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S);
    IF (S.class = Texts.Char) & (S.c = "^") OR (S.line # 0) THEN
      Oberon.GetSelection(T, beg, end, time);
      IF time >= 0 THEN Texts.OpenScanner(S, T, beg); Texts.Scan(S) END
    END;
    IF S.class = Texts.Name THEN
      w.s("Edit.Open "); w.s(S.s); w.sl(".");
      Oberon.AllocateUserViewer(Oberon.Par.vwr.X, X, Y);
      w.s("1. X "); w.i(X); w.s(" Y "); w.i(Y); w.sl(".");
      V := MenuViewers.New(
        TextFrames.NewMenu(S.s, StandardMenu),
        TextFrames.NewText(TextFrames.Text(S.s), 0),
        TextFrames.MenuHeight, X, Y);
      w.sl("2")
    END
  END Open;

  PROCEDURE Store*;
    VAR V: Viewers.Viewer;
      Text: TextFrames.Frame;
      T: Texts.Text;
      S: Texts.Scanner;
      beg, end, time, len: INTEGER;

    PROCEDURE Backup (VAR name: ARRAY OF CHAR);
      VAR res, i: INTEGER; bak: ARRAY 32 OF CHAR;
    BEGIN i := 0;
      WHILE (i < 27) & (name[i] # 0X) DO bak[i] := name[i]; INC(i) END;
      bak[i] := "."; bak[i+1] := "B"; bak[i+2] := "a"; bak[i+3] := "k"; bak[i+4] := 0X;
      Files.Rename(name, bak, res)
    END Backup;

  BEGIN Texts.WriteString(W, "Edit.Store ");
    IF Oberon.Par.frame = Oberon.Par.vwr.child THEN
      V := Oberon.Par.vwr; Texts.OpenScanner(S, V.child(TextFrames.Frame).text, 0)
    ELSE V := Oberon.MarkedViewer(); Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos)
    END;
    Texts.Scan(S);
    IF (S.class = Texts.Char) & (S.c = "^") THEN
      Oberon.GetSelection(T, beg, end, time);
      IF time >= 0 THEN Texts.OpenScanner(S, T, beg); Texts.Scan(S) END
    END;
    IF (S.class = Texts.Name) & (V.child # NIL) & (V.child.next IS TextFrames.Frame) THEN
      Text := V.child.next(TextFrames.Frame);
      Texts.WriteString(W, S.s); Texts.WriteInt(W, Text.text.length, 8);
      Texts.WriteLn(W); Texts.Append(Oberon.Log, W.buf);
      Backup(S.s); Texts.Close(Text.text, S.s)
    END
  END Store;

  PROCEDURE CopyLooks*;
    VAR T: Texts.Text;
      F: TextFrames.Frame;
      v: Viewers.Viewer;
      beg, end, time: INTEGER;
      fnt: Fonts.Font; col, voff: INTEGER;
  BEGIN Oberon.GetSelection(T, beg, end, time);
    IF time >= 0 THEN
      v := FocusViewer();
      IF (v # NIL) & (v.child # NIL) & (v.child.next IS TextFrames.Frame) THEN
        F := v.child.next(TextFrames.Frame);
        Texts.Attributes(F.text, F.caretLoc.pos, fnt, col, voff);
        Texts.ChangeLooks(T, beg, end, {0,1,2}, fnt, col, voff)
      END
    END
  END CopyLooks; 

  PROCEDURE ChangeFont*;
    VAR S: Texts.Scanner; T: Texts.Text; beg, end: INTEGER;
  BEGIN
    Oberon.GetSelection(T, beg, end, time);
    IF time >= 0 THEN
      Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S);
      IF (S.class = Texts.Name) OR (S.class = Texts.String) THEN
        Texts.ChangeLooks(T, beg, end, {0}, Fonts.Load(S.s), 0, 0)
      END
    END
  END ChangeFont;

  PROCEDURE ChangeColor*;
    VAR S: Texts.Scanner;
      T: Texts.Text;
      col: INTEGER;
      beg, end, time: INTEGER;
  BEGIN Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S);
    IF S.class = Texts.Int THEN
      col := S.i; Oberon.GetSelection(T, beg, end, time);
      IF time >= 0 THEN Texts.ChangeLooks(T, beg, end, {1}, NIL, col, 0) END
    END
  END ChangeColor;

  PROCEDURE ChangeOffset*;
    VAR S: Texts.Scanner;
      T: Texts.Text;
      voff: INTEGER; ch: CHAR;
      beg, end, time: INTEGER;
  BEGIN Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S);
    IF S.class = Texts.Int THEN
      voff := S.i; Oberon.GetSelection(T, beg, end, time);
      IF time >= 0 THEN Texts.ChangeLooks(T, beg, end, {2}, NIL, voff, 0) END
    END
  END ChangeOffset;

  PROCEDURE Search*;  (*uses global variables M, pat, d for Boyer-Moore search*)
    VAR Text: TextFrames.Frame;
      V: Viewers.Viewer;
      R: Texts.Reader;
      T: Texts.Text;
      pos, beg, end, prevTime, len: INTEGER; n, i, j: INTEGER;
      buf: ARRAY 32 OF CHAR;

    PROCEDURE Forward(n: INTEGER; VAR R: Texts.Reader; VAR buf: ARRAY OF CHAR);
      VAR m: INTEGER; j: INTEGER;
    BEGIN m := M - n; j := 0;
      WHILE j # m DO buf[j] := buf[n + j]; INC(j) END;
      WHILE j # M DO Texts.Read(R, buf[j]); INC(j) END
    END Forward;

  BEGIN V := Oberon.Par.vwr;
    IF Oberon.Par.frame # V.child THEN V := FocusViewer() END;
    IF (V.child # NIL) & (V.child.next IS TextFrames.Frame) THEN
      Text := V.child.next(TextFrames.Frame);
      prevTime := time; Oberon.GetSelection(T, beg, end, time);
      IF time > prevTime THEN
        Texts.OpenReader(R, T, beg);
        i := 0; pos := beg;
        REPEAT Texts.Read(R, pat[i]); INC(i); INC(pos)
        UNTIL (i = maxlen) OR (pos = end);
        M := i; j := 0;
        WHILE j # 256 DO d[j] := M; INC(j) END;
        j := 0;
        WHILE j # M - 1 DO d[ORD(pat[j])] := M - 1 - j; INC(j) END
      END;
      IF Text.hasCaret THEN pos := Text.caretLoc.pos ELSE pos := 0 END;
      len := Text.text.length;
      Texts.OpenReader(R, Text.text, pos);
      Forward(M, R, buf); pos := pos + M;
      j := M;
      REPEAT DEC(j) UNTIL (j < 0) OR (buf[j] # pat[j]);
      WHILE (j >= 0) & (pos < len) DO
        n := d[ORD(buf[M-1])]; Forward(n, R, buf); INC(pos, n); j := M;
        REPEAT DEC(j) UNTIL (j < 0) OR (buf[j] # pat[j])
      END ;
      IF j < 0 THEN
        TextFrames.RemoveSelection(Text);
        Oberon.RemoveMarks(Text.X, Text.Y, Text.W, Text.H);
        TextFrames.Show(Text, pos - 300); Oberon.PassFocus(V);
        TextFrames.SetCaretPos(Text, pos)
      END
    END
  END Search;

  PROCEDURE Locate*;
    VAR Text: TextFrames.Frame;
      T: Texts.Text; S: Texts.Scanner;
      V: Viewers.Viewer;
      beg, end, time: INTEGER;
  BEGIN
    V := FocusViewer();
    IF (V.child # NIL) & (V.child.next IS TextFrames.Frame) THEN
      Text := V.child.next(TextFrames.Frame);
      Oberon.GetSelection(T, beg, end, time);
      IF time >= 0 THEN
        Texts.OpenScanner(S, T, beg);
        REPEAT Texts.Scan(S) UNTIL (S.class >= Texts.Int); (*skip names*)
        IF S.class = Texts.Int THEN
          TextFrames.RemoveSelection(Text);
          Oberon.RemoveMarks(Text.X, Text.Y, Text.W, Text.H);
          TextFrames.Show(Text, Max(0, S.i - 200));
          Oberon.PassFocus(V);
          TextFrames.SetCaretPos(Text, S.i)
        END
      END
    END
  END Locate;

  PROCEDURE Recall*;
    VAR Menu, Main: Display.Frame;
      buf: Texts.Buffer;
      V: Viewers.Viewer;
      pos: INTEGER;
      M: TextFrames.Frame;
  BEGIN V := FocusViewer();
    IF (V # NIL) & (V IS MenuViewers.Viewer) THEN
      Menu := V.child; Main := V.child.next;
      IF Main IS TextFrames.Frame THEN
        M := Main(TextFrames.Frame);
        IF M.hasCaret THEN
          TextFrames.Recall(buf);
          pos := M.caretLoc.pos + buf.length;
          Texts.Insert(M.text, M.caretLoc.pos, buf);
          TextFrames.SetCaretPos(M, pos)
        END
      ELSIF Menu IS TextFrames.Frame THEN
        M := Menu(TextFrames.Frame);
        IF M.hasCaret THEN
          TextFrames.Recall(buf);
          pos := M.caretLoc.pos + buf.length;
          Texts.Insert(M.text, M.caretLoc.pos, buf);
          TextFrames.SetCaretPos(M, pos)
        END
      END
    END
  END Recall;

BEGIN Texts.OpenWriter(W)
END Edit.
