MODULE ORB;  (*$CONSOLE*)

(* Oberon Recursive Build                                                     *)
(*                                                                            *)
(* Given a single module to build, works out what to compile to satisfy its   *)
(* imports, compiles them, and combines all objects into a single executable. *)


IMPORT
  SYSTEM, Rtl, Files, S := Scanner, B := Base, G := Generator, P := Parser, w := Writer;

TYPE
  ModuleName = ARRAY   64 OF CHAR;
  FileName   = ARRAY 1024 OF CHAR;
  PathName   = ARRAY 4096 OF CHAR;

  Module     = POINTER TO ModuleDesc;
  Dependency = POINTER TO DependencyDesc;

  ModuleDesc = RECORD
    next:         Module;
    modname:      ModuleName;
    filename:     FileName;
    file:         Files.File;
    dependencies: Dependency;
    scanned:      BOOLEAN
  END;

  DependencyDesc = RECORD
    next: Dependency;
    mod:  Module
  END;

VAR
  Modulename:      ModuleName;
  SourcePath:      PathName;
  BuildPath:       PathName;
  OutputPath:      PathName;
  Object:          BOOLEAN;
  Modules:         Module;
  LongestModname:  INTEGER;
  LongestFilename: INTEGER;


(* -------------------------------------------------------------------------- *)
(* ---------------- File open with ';' delimited searchpath ----------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE FindFile(    name:       ARRAY OF CHAR;
                       searchpath: ARRAY OF CHAR;
                   VAR file:       Files.File;
                   VAR filename:   ARRAY OF CHAR);
VAR
  path: ARRAY 2048 OF CHAR;
  i, j: INTEGER;
BEGIN
  i := 0;  file := NIL;
  WHILE (file = NIL) & (searchpath[i] # 0X) DO
    j := 0;
    WHILE (i < LEN(searchpath)) & (searchpath[i] # 0X) & (searchpath[i] # ";") DO
      path[j] := searchpath[i];  INC(i);  INC(j);
    END;
    IF (path[j-1] # "/") & (path[j-1] # '\') THEN path[j] := '\'; INC(j) END;
    path[j] := 0X;  Rtl.Append(name, path);
    file := Files.Old(path);
    WHILE searchpath[i] = ";" DO INC(i) END;
  END;
  IF file # NIL THEN filename := path END
END FindFile;


(* -------------------------------------------------------------------------- *)
(* ------------------ Scan module recording dependencies -------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE FindModule(modname: ModuleName): Module;
VAR module: Module;
BEGIN module := Modules;
  WHILE (module # NIL) & (module.modname # modname) DO
    module := module.next
  END
RETURN module END FindModule;

PROCEDURE AddModule(modname: ARRAY OF CHAR);
VAR mod: Module;  filename: ModuleName;
BEGIN NEW(mod);
  mod.modname := modname;  mod.scanned := FALSE;
  filename := modname;  Rtl.Append(".mod", filename);
  FindFile(filename, SourcePath, mod.file, mod.filename);
  IF mod.file = NIL THEN
    w.s("Could not find source file for module '"); w.s(modname); w.sl("'.");
    Rtl.Halt(99)
  END;
  IF Rtl.Length(modname)      > LongestModname  THEN LongestModname  := Rtl.Length(modname) END;
  IF Rtl.Length(mod.filename) > LongestFilename THEN LongestFilename := Rtl.Length(mod.filename) END;
  mod.next := Modules;  Modules := mod
END AddModule;

PROCEDURE AddImport(from: Module; modname: ARRAY OF CHAR);
VAR dependentModule: Module;  dependency: Dependency;
BEGIN
  dependentModule := FindModule(modname);
  IF dependentModule = NIL THEN AddModule(modname);  dependentModule := Modules END;
  NEW(dependency);  dependency.mod := dependentModule;
  dependency.next := from.dependencies;  from.dependencies := dependency;
END AddImport;

PROCEDURE WriteFilepos;
BEGIN
  w.c("["); w.i(S.LastLine); w.c(":"); w.i(S.LastColumn); w.c("]")
END WriteFilepos;

PROCEDURE Expected(filename, message: ARRAY OF CHAR);
BEGIN
  w.s("File '"); w.s(filename); WriteFilepos; w.s("': "); w.sl(message); Rtl.Halt(99)
END Expected;

PROCEDURE ScanModuleImports(module: Module);
VAR sym: INTEGER;  impname: ARRAY 64 OF CHAR;
BEGIN
  S.Init(module.file);
  S.Get(sym);
  IF sym # S.module THEN Expected(module.filename, "does not start with MODULE.") END;
  S.Get(sym);
  IF sym # S.ident THEN Expected(module.filename, "expected module id."); END;
  IF S.id # module.modname THEN
    w.s("File "); w.s(module.filename); WriteFilepos; w.s(" module id '");
    w.s(S.id); w.s("' does not match expected id '");
    w.s(module.modname); w.sl("'.");
    Rtl.Halt(99)
  END;
  B.Init(S.id);  S.Get(sym);
  IF sym # S.semicolon THEN Expected(module.filename, "expected semicolon from module id.") END;
  S.Get(sym);
  IF sym = S.import THEN
    REPEAT
      S.Get(sym);
      IF sym # S.ident THEN Expected(module.filename, "expected id (1).") END;
      impname := S.id; S.Get(sym);
      IF sym = S.becomes THEN
        S.Get(sym);
        IF sym # S.ident THEN Expected(module.filename, "expected id (2).") END;
        impname := S.id; S.Get(sym)
      END;
      IF (impname # "SYSTEM") & (impname # "Rtl") THEN AddImport(module, impname) END
    UNTIL sym # S.comma
  END;
  IF B.Flag.rtl THEN AddImport(module, "Rtl") END;
  module.scanned := TRUE
END ScanModuleImports;


(* -------------------------------------------------------------------------- *)
(* ----------------------------- Compile module ----------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE NotifyError(line, column: INTEGER;  msg: ARRAY OF CHAR);
BEGIN
  IF S.errCnt = 0 THEN w.l END;
  w.s("  [");  w.i(line);  w.c(":");  w.i(column);  w.s("] ");  w.sl(msg);
END NotifyError;


PROCEDURE intSep(i, n: INTEGER);
BEGIN
   IF n < 0 THEN n := 0 END;
   IF i < 1000 THEN w.in(i, n)
   ELSE
      intSep(i DIV 1000, n-4);  w.c(",");
      i := i MOD 1000;
      w.c(CHR(ORD("0") + i DIV 100));  i := i MOD 100;
      w.c(CHR(ORD("0") + i DIV 10));   i := i MOD 10;
      w.c(CHR(ORD("0") + i));
   END
END intSep;

PROCEDURE Compile(module: Module);
VAR sym, startTime, endTime: INTEGER;  modinit: B.Node;
BEGIN
  w.sn(module.modname, LongestModname+1); w.sn(module.filename, LongestFilename+1);
  B.SetSourcePath(SourcePath);
  B.SetSymPath(BuildPath);
  B.SetBuildPath(BuildPath);
  B.SetOutputPath(OutputPath);
  S.Init(module.file);  S.Get(sym);
  B.SetObject(Object);  (* Temp hack during transition to objects *)
  startTime := Rtl.Time();
  IF sym = S.module THEN modinit := P.Module() ELSE S.Mark("Expected 'MODULE'") END;
  IF S.errCnt = 0 THEN
    B.WriteSymfile;
    G.Generate(modinit);
    B.Cleanup;
    G.Cleanup;  endTime := Rtl.Time();
    intSep(G.pc,      10);   intSep(G.staticSize,  10);
    intSep(G.varSize, 10);   intSep(Rtl.TimeToMSecs(endTime - startTime), 5);
    w.s("ms");  w.l
  END
END Compile;


(* -------------------------------------------------------------------------- *)
(* ------------------------ Build all needed modules ------------------------ *)
(* -------------------------------------------------------------------------- *)

PROCEDURE RemoveDependencies(remove: Module);
VAR mod: Module;  dep, prev: Dependency;
BEGIN
  mod := Modules;
  WHILE mod # NIL DO
    dep := mod.dependencies;  prev := NIL;
    WHILE dep # NIL DO
      IF dep.mod = remove THEN
        IF prev = NIL THEN
          mod.dependencies := dep.next;  dep := dep.next
        ELSE
          prev.next := dep.next;  dep := dep.next
        END
      ELSE
        prev := dep;  dep := dep.next
      END
    END;
    mod := mod.next
  END
END RemoveDependencies;


PROCEDURE ReportDependencies;
VAR mod: Module;  dep: Dependency;
BEGIN
  mod := Modules;
  WHILE mod # NIL DO
    w.s("Module "); w.sn(mod.modname, LongestModname);
    dep := mod.dependencies;
    IF dep = NIL THEN
      w.sl(" (none).")
    ELSE
      WHILE dep # NIL DO w.c(" ");  w.s(dep.mod.modname);  dep := dep.next END;
      w.sl(".")
    END;
    mod := mod.next
  END
END ReportDependencies;


PROCEDURE Build();
VAR mod, prev: Module;  allscanned: BOOLEAN;
BEGIN
  AddModule(Modulename);

  (* Keep scanning and adding modules until all dependencies have been scanned *)
  REPEAT
    mod := Modules;  allscanned := TRUE;
    WHILE mod # NIL DO
      IF ~mod.scanned THEN
        allscanned := FALSE;  ScanModuleImports(mod)
      END;
      mod := mod.next
    END;
  UNTIL allscanned;

  w.sl("Modules and dependencies:");  ReportDependencies;

  (* Compile dependentless modules until all modules compiled. *)
  w.sn("Module", LongestModname+1);  w.sn("File", LongestFilename+1);
  w.sl("      code    static       VAR   time");
  REPEAT
    mod := Modules;  prev := NIL;
    WHILE (mod # NIL) & (mod.dependencies # NIL) DO
      prev := mod;  mod := mod.next
    END;
    IF mod # NIL THEN
      Compile(mod);
      IF S.errCnt # 0 THEN Rtl.Halt(99) END;
      RemoveDependencies(mod);
      IF prev = NIL THEN Modules := mod.next ELSE prev.next := mod.next END
    ELSE
      w.sl("Cannot resolve circular dependency order in:");
      ReportDependencies; Rtl.Halt(99)
    END
  UNTIL Modules = NIL
END Build;


(* -------------------------------------------------------------------------- *)
(* ---------------------------- Argument parsing ---------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE ArgError(n: INTEGER; arg, msg: ARRAY OF CHAR);
BEGIN
  w.s("Argument "); w.i(n); w.s(" '"); w.s(arg); w.s("': "); w.sl(msg);
  Rtl.Halt(99);
END ArgError;

PROCEDURE ScanArguments;
VAR i: INTEGER;  arg: ARRAY 1024 OF CHAR;
BEGIN
  SourcePath := "./";
  BuildPath  := "";
  OutputPath := "";
  Modulename := "";

  i := 1;
  WHILE i < Rtl.NumArgs DO
    Rtl.GetArg(i, arg);
    IF arg[0] = "/" THEN
      IF    arg = "/source" THEN INC(i);  Rtl.GetArg(i, SourcePath)
      ELSIF arg = "/build"  THEN INC(i);  Rtl.GetArg(i, BuildPath)
      ELSIF arg = "/output" THEN INC(i);  Rtl.GetArg(i, OutputPath)
      ELSIF arg = "/object" THEN Object := TRUE
      ELSE
        ArgError(i, arg, "unrecognised option.")
      END
    ELSE
      IF Modulename = "" THEN
        Modulename := arg
      ELSE
        ArgError(i, arg, "filename already specified.")
      END
    END;
    INC(i)
  END;

  IF Modulename = "" THEN
    w.sl("ORB - small Oberon-07 executable builder.");
    w.sl("Expected name of module to build.");
    Rtl.Halt(99);
  END
END ScanArguments;

BEGIN
  Object := FALSE;
  LongestModname  := 0;
  LongestFilename := 0;
  S.InstallNotifyError(NotifyError);
  ScanArguments;
  Build
END ORB.
