MODULE Linktest;

IMPORT SYSTEM, Winshim;

PROCEDURE ltw*(s: ARRAY OF CHAR);
BEGIN
  Winshim.wsl(s);
END ltw;

BEGIN
  Winshim.wsl("Linktest starting.");
  Winshim.ws("crlf at "); Winshim.wh(SYSTEM.ADR(Winshim.crlf)); Winshim.wsl("H.");
  Winshim.ws("crlf ...");
  Winshim.ws(Winshim.crlf);
  ltw("ltw called from LinkTest");
  Winshim.wsl("Linktest complete.")
END Linktest.
