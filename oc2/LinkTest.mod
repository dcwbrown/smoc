MODULE Linktest;

IMPORT SYSTEM, Winboot;

PROCEDURE ltw*(s: ARRAY OF CHAR);
BEGIN
  Winboot.wsl(s);
END ltw;

BEGIN
  Winboot.wsl("Linktest starting.");
  Winboot.ws("crlf at "); Winboot.wh(SYSTEM.ADR(Winboot.crlf)); Winboot.wsl("H.");
  Winboot.ws("crlf ...");
  Winboot.ws(Winboot.crlf);
  ltw("ltw called from LinkTest");
  Winboot.wsl("Linktest complete.")
END Linktest.
