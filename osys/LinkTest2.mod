MODULE Linktest2;

IMPORT SYSTEM, Linktest, Winshim;

BEGIN
  Linktest.ltw("Linktest2 running via Linktest.");
  Winshim.wsl("LinkTest2 running to Winshim");
END Linktest2.
