MODULE Input; (*NW 5.10.86 / 15.11.90 Ceres-2; PDR 21.4.12 / NW 15.5.2013 Ceres-4 / AP 15.9.20 Extended Oberon* / DCWB 8May23 *)
IMPORT SYSTEM, w := Writer;

CONST
  msAdr        = -40;
  kbdAdr       = -36;
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

VAR
  kbdCode:  BYTE;     (*last keyboard code read*)
  Recd:     BOOLEAN;
  Up:       BOOLEAN;
  Shift:    BOOLEAN;
  Ctrl:     BOOLEAN;
  Ext:      BOOLEAN;
  KTabAdr:  INTEGER;  (*keyboard code translation table*)
  MW:       INTEGER;
  MH:       INTEGER;
  MX:       INTEGER;
  MY:       INTEGER;  (*mouse limits and coords*)
  MK:       SET;      (*mouse keys*)

PROCEDURE Peek();
BEGIN
  IF SYSTEM.BIT(msAdr, 28) THEN
    SYSTEM.GET(kbdAdr, kbdCode);
    IF kbdCode = 0F0H THEN Up := TRUE
    ELSIF kbdCode = 0E0H THEN Ext := TRUE
    ELSE
      IF (kbdCode = 12H) OR (kbdCode = 59H) THEN (*shift*) Shift := ~Up
      ELSIF kbdCode = 14H THEN (*ctrl*) Ctrl := ~Up
      ELSIF ~Up THEN Recd := TRUE (*real key going down*)
      END;
      Up := FALSE;  Ext := FALSE
    END
  END;
END Peek;

PROCEDURE Available*(): INTEGER;
BEGIN Peek();
RETURN ORD(Recd) END Available;

PROCEDURE Read*(VAR ch: CHAR);
BEGIN
  WHILE ~Recd DO Peek() END;
  IF Shift OR Ctrl THEN INC(kbdCode, 80H) END; (*ctrl implies shift*)
  SYSTEM.GET(KTabAdr + kbdCode, ch);  (* ch := kbdTab[kbdCode]; *)
  IF Ctrl THEN ch := CHR(ORD(ch) MOD 20H) END;
  Recd := FALSE
END Read;

PROCEDURE Mouse*(VAR keys: SET;  VAR x, y: INTEGER);
VAR w: INTEGER;
BEGIN SYSTEM.GET(msAdr, w);
  keys := SYSTEM.VAL(SET, w DIV 1000000H MOD 8);
  x    := w MOD 400H;
  y    := (w DIV 1000H) MOD 400H;
  IF y >= MH THEN y := MH-1 END
END Mouse;

PROCEDURE SetMouseLimits*(w, h: INTEGER);
BEGIN MW := w;  MH := h
END SetMouseLimits;

PROCEDURE Init*;
BEGIN
  Up    := FALSE;
  Shift := FALSE;
  Ctrl  := FALSE;
  Recd  := FALSE;
END Init;

BEGIN Init;  w.sl("Input loaded.");
END Input.
