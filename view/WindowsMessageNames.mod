MODULE WindowsMessageNames;
IMPORT SYSTEM, Boot, K := Kernel, w := Writer;

VAR
  GetClipboardFormatNameW: PROCEDURE(format, name, maxcount: INTEGER): INTEGER;

PROCEDURE Write*(msg: INTEGER);
VAR
  str:  ARRAY 1024 OF CHAR;
  name: ARRAY 1024 OF SYSTEM.CARD16;
  res:  INTEGER;
BEGIN str := "";
  IF msg < 100H THEN
    CASE msg OF
    | 000H: str := "_NULL"
    | 001H: str := "_CREATE"
    | 002H: str := "_DESTROY"
    | 003H: str := "_MOVE"
    | 005H: str := "_SIZE"
    | 006H: str := "_ACTIVATE"
    | 007H: str := "_SETFOCUS"
    | 008H: str := "_KILLFOCUS"
    | 00AH: str := "_ENABLE"
    | 00BH: str := "_SETREDRAW"
    | 00CH: str := "_SETTEXT"
    | 00DH: str := "_GETTEXT"
    | 00EH: str := "_GETTEXTLENGTH"
    | 00FH: str := "_PAINT"
    | 010H: str := "_CLOSE"
    | 011H: str := "_QUERYENDSESSION"
    | 012H: str := "_QUIT"
    | 013H: str := "_QUERYOPEN"
    | 014H: str := "_ERASEBKGND"
    | 015H: str := "_SYSCOLORCHANGE"
    | 016H: str := "_ENDSESSION"
    | 017H: str := "_SYSTEMERROR"
    | 018H: str := "_SHOWWINDOW"
    | 019H: str := "_CTLCOLOR"
    | 01AH: str := "_WININICHANGE"
    | 01BH: str := "_DEVMODECHANGE"
    | 01CH: str := "_ACTIVATEAPP"
    | 01DH: str := "_FONTCHANGE"
    | 01EH: str := "_TIMECHANGE"
    | 01FH: str := "_CANCELMODE"
    | 020H: str := "_SETCURSOR"
    | 021H: str := "_MOUSEACTIVATE"
    | 022H: str := "_CHILDACTIVATE"
    | 023H: str := "_QUEUESYNC"
    | 024H: str := "_GETMINMAXINFO"
    | 026H: str := "_PAINTICON"
    | 027H: str := "_ICONERASEBKGND"
    | 028H: str := "_NEXTDLGCTL"
    | 02AH: str := "_SPOOLERSTATUS"
    | 02BH: str := "_DRAWITEM"
    | 02CH: str := "_MEASUREITEM"
    | 02DH: str := "_DELETEITEM"
    | 02EH: str := "_VKEYTOITEM"
    | 02FH: str := "_CHARTOITEM"
    | 030H: str := "_SETFONT"
    | 031H: str := "_GETFONT"
    | 032H: str := "_SETHOTKEY"
    | 037H: str := "_QUERYDRAGICON"
    | 039H: str := "_COMPAREITEM"
    | 03DH: str := "_GETOBJECT"
    | 041H: str := "_COMPACTING"
    | 044H: str := "_COMMNOTIFY"
    | 046H: str := "_WINDOWPOSCHANGING"
    | 047H: str := "_WINDOWPOSCHANGED"
    | 048H: str := "_POWER"
    | 04AH: str := "_COPYDATA"
    | 04BH: str := "_CANCELJOURNAL"
    | 04EH: str := "_NOTIFY"
    | 050H: str := "_INPUTLANGCHANGEREQUEST"
    | 051H: str := "_INPUTLANGCHANGE"
    | 052H: str := "_TCARD"
    | 053H: str := "_HELP"
    | 054H: str := "_USERCHANGED"
    | 055H: str := "_NOTIFYFORMAT"
    | 07BH: str := "_CONTEXTMENU"
    | 07CH: str := "_STYLECHANGING"
    | 07DH: str := "_STYLECHANGED"
    | 07EH: str := "_DISPLAYCHANGE"
    | 07FH: str := "_GETICON"
    | 080H: str := "_SETICON"
    | 081H: str := "_NCCREATE"
    | 082H: str := "_NCDESTROY"
    | 083H: str := "_NCCALCSIZE"
    | 084H: str := "_NCHITTEST"
    | 085H: str := "_NCPAINT"
    | 086H: str := "_NCACTIVATE"
    | 087H: str := "_GETDLGCODE"
    | 090H: str := "_UAHDESTROYWINDOW"
    | 091H: str := "_UAHDRAWMENU"
    | 092H: str := "_UAHDRAWMENUITEM"
    | 093H: str := "_UAHINITMENU"
    | 094H: str := "_UAHMEASUREMENUITEM"
    | 095H: str := "_UAHNCPAINTMENUPOPUP"
    | 0A0H: str := "_NCMOUSEMOVE"
    | 0A1H: str := "_NCLBUTTONDOWN"
    | 0A2H: str := "_NCLBUTTONUP"
    | 0A3H: str := "_NCLBUTTONDBLCLK"
    | 0A4H: str := "_NCRBUTTONDOWN"
    | 0A5H: str := "_NCRBUTTONUP"
    | 0A6H: str := "_NCRBUTTONDBLCLK"
    | 0A7H: str := "_NCMBUTTONDOWN"
    | 0A8H: str := "_NCMBUTTONUP"
    | 0A9H: str := "_NCMBUTTONDBLCLK"
    END
  ELSIF msg < 200H THEN
    CASE msg OF
    | 100H: str := "_KEYDOWN"
    | 101H: str := "_KEYUP"
    | 102H: str := "_CHAR"
    | 103H: str := "_DEADCHAR"
    | 104H: str := "_SYSKEYDOWN"
    | 105H: str := "_SYSKEYUP"
    | 106H: str := "_SYSCHAR"
    | 107H: str := "_SYSDEADCHAR"
    | 108H: str := "_KEYLAST"
    | 109H: str := "_UNICHAR"
    | 110H: str := "_INITDIALOG"
    | 111H: str := "_COMMAND"
    | 112H: str := "_SYSCOMMAND"
    | 113H: str := "_TIMER"
    | 114H: str := "_HSCROLL"
    | 115H: str := "_VSCROLL"
    | 116H: str := "_INITMENU"
    | 117H: str := "_INITMENUPOPUP"
    | 119H: str := "_GESTURE"
    | 11AH: str := "_GESTURENOTIFY"
    | 11FH: str := "_MENUSELECT"
    | 120H: str := "_MENUCHAR"
    | 121H: str := "_ENTERIDLE"
    | 122H: str := "_MENURBUTTONUP"
    | 123H: str := "_MENUDRAG"
    | 124H: str := "_MENUGETOBJECT"
    | 125H: str := "_UNINITMENUPOPUP"
    | 126H: str := "_MENUCOMMAND"
    | 132H: str := "_CTLCOLORMSGBOX"
    | 133H: str := "_CTLCOLOREDIT"
    | 134H: str := "_CTLCOLORLISTBOX"
    | 135H: str := "_CTLCOLORBTN"
    | 136H: str := "_CTLCOLORDLG"
    | 137H: str := "_CTLCOLORSCROLLBAR"
    | 138H: str := "_CTLCOLORSTATIC"
    END
  ELSIF msg < 300H THEN
    CASE msg OF
    | 200H: str := "_MOUSEMOVE"
    | 201H: str := "_LBUTTONDOWN"
    | 202H: str := "_LBUTTONUP"
    | 203H: str := "_LBUTTONDBLCLK"
    | 204H: str := "_RBUTTONDOWN"
    | 205H: str := "_RBUTTONUP"
    | 206H: str := "_RBUTTONDBLCLK"
    | 207H: str := "_MBUTTONDOWN"
    | 208H: str := "_MBUTTONUP"
    | 209H: str := "_MBUTTONDBLCLK"
    | 20AH: str := "_MOUSEWHEEL"
    | 210H: str := "_PARENTNOTIFY"
    | 211H: str := "_ENTERMENULOOP"
    | 212H: str := "_EXITMENULOOP"
    | 213H: str := "_NEXTMENU"
    | 214H: str := "_SIZING"
    | 215H: str := "_CAPTURECHANGED"
    | 216H: str := "_MOVING"
    | 218H: str := "_POWERBROADCAST"
    | 219H: str := "_DEVICECHANGE"
    | 220H: str := "_MDICREATE"
    | 221H: str := "_MDIDESTROY"
    | 222H: str := "_MDIACTIVATE"
    | 223H: str := "_MDIRESTORE"
    | 224H: str := "_MDINEXT"
    | 225H: str := "_MDIMAXIMIZE"
    | 226H: str := "_MDITILE"
    | 227H: str := "_MDICASCADE"
    | 228H: str := "_MDIICONARRANGE"
    | 229H: str := "_MDIGETACTIVE"
    | 230H: str := "_MDISETMENU"
    | 231H: str := "_ENTERSIZEMOVE"
    | 232H: str := "_EXITSIZEMOVE"
    | 233H: str := "_DROPFILES"
    | 234H: str := "_MDIREFRESHMENU"
    | 281H: str := "_IME_SETCONTEXT"
    | 282H: str := "_IME_NOTIFY"
    | 283H: str := "_IME_CONTROL"
    | 284H: str := "_IME_COMPOSITIONFULL"
    | 285H: str := "_IME_SELECT"
    | 286H: str := "_IME_CHAR"
    | 290H: str := "_IME_KEYDOWN"
    | 291H: str := "_IME_KEYUP"
    | 2A1H: str := "_MOUSEHOVER"
    | 2A3H: str := "_MOUSELEAVE"
    END
  ELSIF msg < 400H THEN
      CASE msg OF
    | 300H: str := "_CUT"
    | 301H: str := "_COPY"
    | 302H: str := "_PASTE"
    | 303H: str := "_CLEAR"
    | 304H: str := "_UNDO"
    | 305H: str := "_RENDERFORMAT"
    | 306H: str := "_RENDERALLFORMATS"
    | 307H: str := "_DESTROYCLIPBOARD"
    | 308H: str := "_DRAWCLIPBOARD"
    | 309H: str := "_PAINTCLIPBOARD"
    | 30AH: str := "_VSCROLLCLIPBOARD"
    | 30BH: str := "_SIZECLIPBOARD"
    | 30CH: str := "_ASKCBFORMATNAME"
    | 30DH: str := "_CHANGECBCHAIN"
    | 30EH: str := "_HSCROLLCLIPBOARD"
    | 30FH: str := "_QUERYNEWPALETTE"
    | 310H: str := "_PALETTEISCHANGING"
    | 311H: str := "_PALETTECHANGED"
    | 312H: str := "_HOTKEY"
    | 317H: str := "_PRINT"
    | 318H: str := "_PRINTCLIENT"
    | 31FH: str := "_DWMNCRENDERINGCHANGED"
    | 358H: str := "_HANDHELDFIRST"
    | 35FH: str := "_HANDHELDLAST"
    | 360H: str := "_AFXFIRST"
    | 37FH: str := "_AFXLAST"
    | 380H: str := "_PENWINFIRST"
    | 38FH: str := "_PENWINLAST"
    | 390H: str := "_COALESCE_FIRST"
    | 39FH: str := "_COALESCE_LAST"
    END
  ELSIF msg = 400H THEN str := "USER"
  ELSIF (msg >= 0C000H) & (msg <= 0FFFFH) THEN
    (* registered cross-application name *)
    name[0] := 23H;  (* '#' *)
    IF GetClipboardFormatNameW(msg, SYSTEM.ADR(name)+2, LEN(name)-1) > 0 THEN
      res := K.Utf16ToUtf8(name, str)
    END
  END;
  IF str = "" THEN w.s("WM $"); w.h(msg) ELSE w.s("WM"); w.s(str) END
END Write;

(*
PROCEDURE GetProc(dll: INTEGER; name: ARRAY [untagged] OF CHAR; VAR proc: ARRAY OF BYTE);
BEGIN
  SYSTEM.PUT(SYSTEM.ADR(proc), Boot.PEImports.GetProcAddress(dll, SYSTEM.ADR(name)))
END GetProc;
*)

BEGIN
  K.GetProc(K.User, "GetClipboardFormatNameW", GetClipboardFormatNameW); ASSERT(GetClipboardFormatNameW # NIL);
END WindowsMessageNames.