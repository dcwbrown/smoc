MODULE OC2;  (* Command line driver for ORP *)  (*$CONSOLE*)

IMPORT ORP, ORS;

BEGIN
  ORP.CompileFile("Compiletests.mod");
  IF TRUE THEN
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Boot.mod")    END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Heap.mod")    END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Kernel.mod")  END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Writer.mod")  END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Display.mod") END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Fonts.mod")   END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Files.mod")   END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Texts.mod")   END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Oberon.mod")  END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("ORS.mod")     END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("ORB.mod")     END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("ORG.mod")     END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("ORP.mod")     END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("OC2.mod")     END
  END
END OC2.
