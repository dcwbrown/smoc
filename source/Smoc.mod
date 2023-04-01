MODULE Smoc;
(*$CONSOLE*)

IMPORT
  SYSTEM, Rtl, Files, S := Scanner, B := Base, G := Generator, P := Parser, w := Writer;

VAR
  arg, fname: ARRAY 1024 OF CHAR;
  argIdx:     INTEGER;
  buildfile:  Files.File;
  buildMode,
  errFlag:    BOOLEAN;

PROCEDURE outSep(i, n: INTEGER);
BEGIN
   IF n < 0 THEN n := 0 END;
   IF i < 1000 THEN w.in(i, n)
   ELSE
      outSep(i DIV 1000, n-4);  w.c(",");
      i := i MOD 1000;
      w.c(CHR(ORD("0") + i DIV 100));  i := i MOD 100;
      w.c(CHR(ORD("0") + i DIV 10));   i := i MOD 10;
      w.c(CHR(ORD("0") + i));
   END
END outSep;

(* Write filename part of path in a fixed 20 column field *)
PROCEDURE outFname(fname: ARRAY OF CHAR);
VAR i, j: INTEGER;
BEGIN
  i := 0;
  WHILE fname[i] # 0Y DO INC(i) END;
  WHILE (i > 0) & (fname[i-1] # "\") DO DEC(i) END;
  j := i;  WHILE fname[i] # 0Y DO w.c(fname[i]); INC(i) END;
  WHILE i-j < 20 DO w.c(" "); INC(i) END
END outFname;


PROCEDURE Compile(fname: ARRAY OF CHAR);
VAR srcfile: Files.File;  modinit: B.Node;
    i, sym, startTime, endTime: INTEGER;
BEGIN
  outFname(fname);
  B.SetSrcPath(fname);  srcfile := Files.Old(fname);
  S.Init(srcfile);  S.Get(sym);

  startTime := Rtl.Time();
  IF sym = S.module THEN modinit := P.Module() ELSE S.Mark("Expected 'MODULE'") END;
  IF S.errCnt = 0 THEN
    B.WriteSymfile;  G.Generate(modinit);
    B.Cleanup;  G.Cleanup;  endTime := Rtl.Time();
    outSep(G.pc,          10);   outSep(G.staticSize,  10);
    outSep(G.varSize,     10);   outSep(Rtl.TimeToMSecs(endTime - startTime), 5);
    w.s("ms");  w.l
  END
END Compile;


PROCEDURE ErrorNotFound(fname: ARRAY OF CHAR);
BEGIN w.s("File ");  w.s(fname);  w.sl(" not found") END ErrorNotFound;


PROCEDURE Build(fname: ARRAY OF CHAR);
VAR
  r:          Files.Rider;
  i:          INTEGER;
  x:          BYTE;
  start, end: INTEGER;
  srcfname:   ARRAY 1024 OF CHAR;
  codesize:   INTEGER;
  staticsize: INTEGER;
  varsize:    INTEGER;
BEGIN
  w.sl("File name                 code      data    global   time");
  start := Rtl.Time();  buildfile := Files.Old(fname);
  codesize := 0;  staticsize := 0;  varsize := 0;
  Files.Set(r, buildfile, 0);  i := 0;
  Files.Read(r, x);
  WHILE ~r.eof DO
    WHILE (x <= 32) & ~r.eof DO Files.Read(r, x) END;
    WHILE (x > 32) & ~r.eof DO
      srcfname[i] := CHR(x);  Files.Read(r, x);  INC(i)
    END;
    IF i > 0 THEN
      srcfname[i] := 0Y;
      IF Files.Old(srcfname) # NIL THEN
        Compile(srcfname);
        INC(codesize,   G.pc);
        INC(staticsize, G.staticSize);
        INC(varsize,    G.varSize);
      ELSE ErrorNotFound(srcfname)
      END;
      IF S.errCnt # 0 THEN Rtl.Halt(1) END;
      i := 0
    END
  END;
  end := Rtl.Time();
  w.s("Total               ");
  outSep(codesize, 10);   outSep(staticsize,  10);
  outSep(varsize,  10);   outSep(Rtl.TimeToMSecs(end-start), 5);
  w.s("ms");       w.l
END Build;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Get;
BEGIN INC(argIdx);  Rtl.GetArg(arg, argIdx)
END Get;

PROCEDURE Mark(msg: ARRAY OF CHAR);
BEGIN
  w.s("arg ");  w.i(argIdx);  w.s(": ");
  w.s(msg);  w.l;  errFlag := TRUE
END Mark;

PROCEDURE Arguments;
  PROCEDURE Option;
  BEGIN (*Rtl.LowerCase(arg);*)
    IF arg = "/b" THEN buildMode := TRUE;  Get;  Arguments
    ELSIF arg = "/sym" THEN Get;
      IF arg[0] = "/" THEN Mark("path to symbols?");  Option
      ELSE B.SetSymPath(arg);  Get;  Arguments
      END
    ELSE (* unhandled *) Get;  Arguments
    END
  END Option;
BEGIN (* Arguments *)
  IF arg[0] = 0Y THEN (* end parsing *)
  ELSIF arg[0] # "/" THEN
    IF fname[0] = 0Y THEN fname := arg
    ELSE Mark("expecting another filename")
    END;
    Get;  Arguments
  ELSIF arg[0] = "/" THEN Option
  END
END Arguments;

PROCEDURE NotifyError8(line, column: INTEGER;  msg: ARRAY OF CHAR);
BEGIN
  IF S.errCnt = 0 THEN w.l END;
  w.s("  [");  w.i(line);
  w.c(":");    w.i(column);
  w.s("] ");   w.sl(msg);
END NotifyError8;

BEGIN
  S.InstallNotifyError(NotifyError8);  Get;  Arguments;
  IF fname[0] # 0Y THEN
    IF Files.Old(fname) # NIL THEN
      IF ~buildMode THEN Compile(fname) ELSE Build(fname) END
    ELSE ErrorNotFound(fname)
    END
  ELSE
    w.sl("Small Oberon-07 Compiler");
    w.sl("Usage: Smoc <inputfile>");
  END
END Smoc.