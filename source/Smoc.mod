MODULE Smoc;
(*$CONSOLE*)

IMPORT
  SYSTEM, Rtl, Out, Files, S := Scanner, B := Base, G := Generator, P := Parser;

VAR
  arg, fname: ARRAY 1024 OF CHAR;
  buildfile: Files.File;  argIdx: INTEGER;
  buildMode, errFlag: BOOLEAN;

PROCEDURE outSep(i, w: INTEGER);
BEGIN
   IF w < 0 THEN w := 0 END;
   IF i < 1000 THEN Out.Int(i, w)
   ELSE
      outSep(i DIV 1000, w-4);  Out.Char(',');
      i := i MOD 1000;
      Out.Char(CHR(ORD('0') + i DIV 100));  i := i MOD 100;
      Out.Char(CHR(ORD('0') + i DIV 10));   i := i MOD 10;
      Out.Char(CHR(ORD('0') + i));
   END
END outSep;

PROCEDURE outFname(fname: ARRAY OF CHAR);
VAR basename: ARRAY 1024 OF CHAR;  i, j: INTEGER;
BEGIN
  i := 0;
  WHILE fname[i] # 0X DO INC(i) END;
  WHILE (i > 0) & (fname[i-1] # '\') DO DEC(i) END;
  j := i;  WHILE fname[i] # 0X DO Out.Char(fname[i]); INC(i) END;
  WHILE i-j < 25 DO Out.Char(' '); INC(i) END
END outFname;


PROCEDURE Compile(fname: ARRAY OF CHAR);
VAR srcfile: Files.File;  modinit: B.Node;
    i, sym, startTime, endTime: INTEGER;
BEGIN
  outFname(fname);
  B.SetSrcPath(fname);  srcfile := Files.Old(fname);
  S.Init(srcfile, 0);  S.Get(sym);

  startTime := Rtl.Time();
  IF sym = S.module THEN modinit := P.Module() ELSE S.Mark("Expected 'MODULE'") END;
  IF S.errCnt = 0 THEN
    B.WriteSymfile;  G.Generate(modinit);
    B.Cleanup;  G.Cleanup;  endTime := Rtl.Time();
    outSep(G.pc,          10);   outSep(G.staticSize,  10);
    outSep(G.varSize,     10);   outSep(Rtl.TimeToMSecs(endTime - startTime), 5);
    Out.String('ms');  Out.Ln
  END
END Compile;

PROCEDURE ErrorNotFound(fname: ARRAY OF CHAR);
BEGIN
  Out.String('File ');  Out.String(fname);
  Out.String(' not found');  Out.Ln
END ErrorNotFound;

PROCEDURE Build(fname: ARRAY OF CHAR);
VAR r: Files.Rider;  i: INTEGER;  x: BYTE;  start, end: INTEGER;
    byteStr: ARRAY 1024 OF BYTE;  srcfname: ARRAY 1024 OF CHAR;
    codesize, staticsize, varsize: INTEGER;
BEGIN
  Out.String("File name                      code      data    global   time"); Out.Ln;
  start := Rtl.Time();  buildfile := Files.Old(fname);
  codesize := 0;  staticsize := 0;  varsize := 0;
  Files.Set(r, buildfile, 0);  i := 0;  Files.Read(r, x);
  WHILE ~r.eof DO
    WHILE (x <= 32) & ~r.eof DO Files.Read(r, x) END;
    WHILE (x > 32) & ~r.eof DO
      byteStr[i] := x;  Files.Read(r, x);  INC(i)
    END;
    IF i > 0 THEN
      byteStr[i] := 0;  i := Rtl.Utf8ToUnicode(byteStr, srcfname);
      IF Files.Old(srcfname) # NIL THEN
        Compile(srcfname);  INC(codesize, G.pc);
        INC(staticsize, G.staticSize);  INC(varsize, G.varSize);
      ELSE ErrorNotFound(srcfname)
      END;
      IF S.errCnt # 0 THEN Rtl.Halt(1) END;
      i := 0
    END
  END;
  end := Rtl.Time();
  Out.String('Total                    ');
  outSep(codesize, 10);   outSep(staticsize,  10);
  outSep(varsize,  10);   outSep(Rtl.TimeToMSecs(end-start), 5);
  Out.String('ms');       Out.Ln
END Build;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Get;
BEGIN INC(argIdx);  Rtl.GetArg(arg, argIdx)
END Get;

PROCEDURE Mark(msg: ARRAY OF CHAR);
BEGIN
  Out.String('arg ');  Out.Int(argIdx, 0);  Out.String(': ');
  Out.String(msg);  Out.Ln;  errFlag := TRUE
END Mark;

PROCEDURE Arguments;
  PROCEDURE Option;
  BEGIN (*Rtl.LowerCase(arg);*)
    IF arg = '/b' THEN buildMode := TRUE;  Get;  Arguments
    ELSIF arg = '/sym' THEN Get;
      IF arg[0] = '/' THEN Mark('path to symbols?');  Option
      ELSE B.SetSymPath(arg);  Get;  Arguments
      END
    ELSE (* unhandled *) Get;  Arguments
    END
  END Option;
BEGIN (* Arguments *)
  IF arg = 0X THEN (* end parsing *)
  ELSIF arg[0] # '/' THEN
    IF fname[0] = 0X THEN fname := arg
    ELSE Mark('another filename?')
    END;
    Get;  Arguments
  ELSIF arg[0] = '/' THEN Option
  END
END Arguments;

PROCEDURE NotifyError(line, column: INTEGER;  msg: ARRAY OF CHAR);
BEGIN
  IF S.errCnt = 0 THEN Out.Ln END;
  Out.String('  [');  Out.Int(line, 1);
  Out.Char(':');      Out.Int(column, 1);
  Out.String("] ");   Out.String(msg);  Out.Ln
END NotifyError;

BEGIN
  S.InstallNotifyError(NotifyError);  Get;  Arguments;
  IF fname[0] # 0X THEN
    IF Files.Old(fname) # NIL THEN
      IF ~buildMode THEN Compile(fname) ELSE Build(fname) END
    ELSE ErrorNotFound(fname)
    END
  ELSE
    Out.String('Small Oberon-07 Compiler');  Out.Ln;
    Out.String('Usage: Smoc <inputfile>');   Out.Ln
  END
END Smoc.