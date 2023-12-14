MODULE OC2;  (* Command line driver for ORP *)  (*$CONSOLE*)

IMPORT ORP, ORS, Loader;

CONST BuildCompiler = FALSE;

BEGIN
  ORP.CompileFile("Compiletests.mod");
  IF BuildCompiler THEN
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Boot.mod")    END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Heap.mod")    END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Kernel.mod")  END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Fonts.mod")   END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Writer.mod")  END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Files.mod")   END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Texts.mod")   END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Oberon.mod")  END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("ORS.mod")     END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("ORB.mod")     END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("X64.mod")     END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("Listing.mod") END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("ORG.mod")     END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("ORP.mod")     END;
    IF ORS.errcnt = 0 THEN ORP.CompileFile("OC2.mod")     END
  END;

  IF ORS.errcnt = 0 THEN Loader.Load("CompileTests.code") END
END OC2.
