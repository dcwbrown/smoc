MODULE Build;  (*$CONSOLE*)

(* Oberon Recursive Build                                                     *)
(*                                                                            *)
(* Given a single module to build, works out what to compile to satisfy its   *)
(* imports, compiles them, and combines all objects into a single executable. *)


IMPORT
  SYSTEM, K := Kernel, Files, S := Scanner, B := Base, G := Generator, P := Parser, w := Writer, WritePE;

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
  Verbose:         BOOLEAN;
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
    path[j] := 0X;  K.Append(name, path);
    file := Files.Old(path);
    WHILE searchpath[i] = ";" DO INC(i) END;
  END;
  IF file # NIL THEN
    filename := path;
    (*w.s("Found "); w.s(name); w.s(" at "); w.s(path); w.sl(".")*)
  END
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
  filename := modname;  K.Append(".mod", filename);
  FindFile(filename, SourcePath, mod.file, mod.filename);
  IF mod.file = NIL THEN
    w.s("Could not find source file for module '"); w.s(modname); w.sl("'.");
    K.Halt(99)
  END;
  IF K.Length(modname)      > LongestModname  THEN LongestModname  := K.Length(modname) END;
  IF K.Length(mod.filename) > LongestFilename THEN LongestFilename := K.Length(mod.filename) END;
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
  w.s("File '"); w.s(filename); WriteFilepos; w.s("': "); w.sl(message); K.Halt(99)
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
    K.Halt(99)
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
      IF (impname # "SYSTEM") & (impname # "Boot") & (impname # "Kernel") THEN
        AddImport(module, impname)
      END
    UNTIL sym # S.comma
  END;

  (* All modules need boot, excepting boot itself *)
  IF module.modname # "Boot" THEN AddImport(module, "Boot") END;

  (* All modules need kernel excepting those marked RTL- *)
  IF B.Flag.rtl THEN AddImport(module, "Kernel") END;

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
   IF i < 1000 THEN w.in(i, -n)
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
  B.SetBuildPath(BuildPath);
  S.Init(module.file);  S.Get(sym);
  startTime := K.Time();
  IF sym = S.module THEN modinit := P.Module() ELSE S.Mark("Expected 'MODULE'") END;
  IF S.errCnt = 0 THEN
    B.WriteSymfile;
    G.Generate(modinit);
    B.Cleanup;
    G.Cleanup;  endTime := K.Time();
    intSep(G.pc,      10);   intSep(G.staticSize,  10);
    intSep(G.varSize, 10);   intSep(endTime - startTime, 5);
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
VAR
  mod, prev:  Module;
  allscanned: BOOLEAN;
  PEname:     ARRAY 1024 OF CHAR;
  codesize:   INTEGER;
  staticsize: INTEGER;
  varsize:    INTEGER;
  start, end: INTEGER;  (* Times *)

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

  IF Verbose THEN w.sl("Modules and dependencies:");  ReportDependencies END;

  (* Compile dependentless modules until all modules compiled. *)
  w.sn("Module", LongestModname+1);  w.sn("File", LongestFilename+1);
  w.sl("      code    static       VAR   time");
  codesize := 0;  staticsize := 0;  varsize := 0;  start := K.Time();
  REPEAT
    mod := Modules;  prev := NIL;
    WHILE (mod # NIL) & (mod.dependencies # NIL) DO
      prev := mod;  mod := mod.next
    END;
    IF mod # NIL THEN
      Compile(mod);
      IF S.errCnt # 0 THEN K.Halt(99) END;
      INC(codesize, G.pc);  INC(staticsize, G.staticSize);  INC(varsize, G.varSize);
      RemoveDependencies(mod);
      IF prev = NIL THEN Modules := mod.next ELSE prev.next := mod.next END
    ELSE
      w.sl("Cannot resolve circular dependency order in:");
      ReportDependencies; K.Halt(99)
    END
  UNTIL Modules = NIL;

  PEname := B.BuildPath;  K.Append(Modulename, PEname);  K.Append(".exe", PEname);
  WritePE.Generate(PEname);

  end := K.Time();
  w.sn("Total", LongestModname + LongestFilename + 2);
  intSep(codesize, 10);   intSep(staticsize,  10);
  intSep(varsize,  10);   intSep(end - start,  5);
  w.sl("ms")
END Build;


PROCEDURE AddExecutableDirToSourceSearchpath;
VAR i, j, k: INTEGER;
BEGIN
  (*
  w.s("Initial directory: '"); w.s(K.InitialDirectory); w.sl("'.");
  w.s("Executable file:   '"); w.s(K.ExecutablePath);   w.sl("'.");
  *)
  (* See if executable path starts with current working directory and omit if so *)
  i := 0;
  WHILE (i < LEN(K.InitialDirectory))
      & (i < LEN(K.ExecutablePath))
      & (K.InitialDirectory[i] # 0X)
      & (K.InitialDirectory[i] = K.ExecutablePath[i]) DO INC(i) END;
  IF (i > 0) & (K.InitialDirectory[i] = 0X)
   & ((K.ExecutablePath[i] = '/') OR (K.ExecutablePath[i] = '\')) THEN INC(i) ELSE i := 0 END;

  (* 'i' is start of executable directory *)
  (* Find end of path componenet of executable filename *)
  j := -1;  k := i;
  WHILE (k < LEN(K.ExecutablePath)) & (K.ExecutablePath[k] # 0X) DO
    IF (K.ExecutablePath[k] = "/") OR (K.ExecutablePath[k] = '\') THEN j := k END;
    INC(k)
  END;

  IF j >= 0 THEN
    (* 'i'and 'j' are first and limit character of executable directory *)
    (* Find end of source searchpath *)
    k := 0;
    WHILE (k < LEN(SourcePath)) & (SourcePath[k] # 0X) DO INC(k) END;

    (* Add path separator to source searchpath if none already present *)
    IF k < LEN(SourcePath) - 1 THEN
      IF (k > 0) & (SourcePath[k-1] # "/") & (SourcePath[k-1] # '\') THEN
        SourcePath[k] := "/";  INC(k);
      END
    END;

    (* Add executable path to source search path *)
    IF k + j-i + 3 < LEN(SourcePath) THEN
      SourcePath[k] := ";";  INC(k);
      WHILE i < j DO SourcePath[k] := K.ExecutablePath[i];  INC(i);  INC(k) END;
      SourcePath[k] := '/';  INC(k);
      SourcePath[k] := 0X;
    END
  END
END AddExecutableDirToSourceSearchpath;


(* -------------------------------------------------------------------------- *)
(* ---------------------------- Argument parsing ---------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE ArgError(n: INTEGER; arg, msg: ARRAY OF CHAR);
BEGIN
  w.s("Argument "); w.i(n); w.s(" '"); w.s(arg); w.s("': "); w.sl(msg);
  K.Halt(99);
END ArgError;

PROCEDURE ScanArguments;
VAR i: INTEGER;  arg: ARRAY 1024 OF CHAR;
BEGIN
  SourcePath := "./";
  BuildPath  := "";
  Modulename := "";

  i := 1;
  WHILE i < K.NumArgs DO
    K.GetArg(i, arg);
    IF arg[0] = "/" THEN
      IF    arg = "/source" THEN INC(i);  K.GetArg(i, SourcePath)
      ELSIF arg = "/s"      THEN INC(i);  K.GetArg(i, SourcePath)
      ELSIF arg = "/build"  THEN INC(i);  K.GetArg(i, BuildPath)
      ELSIF arg = "/b"      THEN INC(i);  K.GetArg(i, BuildPath)
      ELSIF arg = "/v"      THEN Verbose := TRUE
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
    w.sl("Build - Oberon recursive builder.");
    w.sl("No parameters. Expected name of module to build.");
    K.Halt(99);
  END
END ScanArguments;

BEGIN
  Verbose         := FALSE;
  LongestModname  := 0;
  LongestFilename := 0;
  S.InstallNotifyError(NotifyError);
  ScanArguments;
  AddExecutableDirToSourceSearchpath;
  w.s("SourcePath: '");    w.s(SourcePath);
  w.s("', BuildPath: '");  w.s(BuildPath);  w.sl("'.");
  Build
END Build.
