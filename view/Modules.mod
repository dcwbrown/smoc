MODULE Modules;

IMPORT SYSTEM, Boot, w := Writer;

CONST
  noref*      = 0;  (* no client, type, pointer or procedure variable references *)
  noerr*      = 0;  (* module loaded, command found and executed                 *)
  nofile*     = 1;  (* file not available                                        *)
  badversion* = 2;  (* bad file version                                          *)
  badkey*     = 3;  (* key conflict                                              *)
  badfile*    = 4;  (* corrupted file                                            *)
  nospace*    = 5;  (* insufficient space                                        *)
  nocmd*      = 6;  (* command not found                                         *)
  badcmd*     = 7;  (* invalid command                                           *)
  nomod*      = 8;  (* module not found in module list                           *)
  badfin*     = 9;  (* bad finalization                                          *)

  (*
  clients*    = 10; (* clients exist                                             *)
  dyntypes*   = 11; (* types in use in dynamically allocated objects             *)
  dynptrs*    = 12; (* static module data refd by pointers in dyn allocd objects *)
  dynpvrs*    = 13; (* procedures in use in dynamically allocated objects        *)
  statptrs*   = 14; (* static module data referenced by global pointer variables *)
  statpvrs*   = 15; (* procedures in use in global procedure variables           *)
  *)

TYPE
  Command*    = PROCEDURE;
  ModuleName* = ARRAY 32 OF CHAR;
  Module*     = POINTER TO ModuleDesc;
  ModuleDesc* = RECORD
    header*:  INTEGER;  (* Address of loaded image header *)
    name*:    ModuleName;
    (* finalize: Command; *)
    next*:  Module
  END;

VAR
  root*:      Module;          (* First loaded module              *)
  res*:       INTEGER;         (* error code                       *)
  importing*: ModuleName; (* module name that errors refer to *)
  imported*:  ModuleName; (* module name that errors refer to *)


PROCEDURE error(n: INTEGER; name: ARRAY OF CHAR);
BEGIN res := n;  importing := name END error;


PROCEDURE Load(name: ARRAY OF CHAR;  VAR newmod: Module);
VAR
  mod: Module;
BEGIN
  error(noerr, name);
  mod := root;
  WHILE (mod # NIL) & (mod.name # name) DO mod := mod.next END;
  IF mod = NIL THEN
    error(nomod, name)
  END;
  newmod := mod
END Load;


PROCEDURE GetInt(adr: INTEGER): INTEGER;
VAR result: INTEGER;
BEGIN SYSTEM.GET(adr, result)
RETURN result END GetInt;


PROCEDURE ThisCommand(mod: Module;  command: ARRAY OF CHAR): Command;
VAR
  cmd:  INTEGER;
  adr:  INTEGER;
  name: ModuleName;
  i:    INTEGER;
BEGIN
  error(nocmd, mod.name);  cmd := 0;
  w.s("Lookup command '"); w.s(command); w.s("' in module "); w.s(mod.name); w.sl(".");
  adr := GetInt(mod.header + Boot.OffModCommands);
  IF adr # 0 THEN
    SYSTEM.GET(adr, name[0]);  INC(adr);
    WHILE (name[0] # 0X) & (cmd = 0) DO
      i := 0;  REPEAT INC(i); SYSTEM.GET(adr, name[i]);  INC(adr) UNTIL name[i] = 0X;
      w.s("  consider '"); w.s(name); w.sl("'.");
      IF command = name THEN SYSTEM.GET(adr, cmd);  res := noerr END;
      INC(adr, 8);
      SYSTEM.GET(adr, name[0]);  INC(adr)
    END;
    IF cmd # 0 THEN INC(cmd, GetInt(mod.header + Boot.OffModBase)) END  (* Relocation *)
  END
RETURN SYSTEM.VAL(Command, cmd) END ThisCommand;


PROCEDURE Call*(command: ARRAY OF CHAR; VAR err: INTEGER);
VAR
  mod:   Module;
  mname: ModuleName;
  cname: ModuleName;
  ch:    CHAR;
  i, j:  INTEGER;
  p:     Command;
BEGIN
  i := 0;
  WHILE (i < LEN(command)) & (command[i] # '.') & (command[i] # 0X) DO
    mname[i] := command[i];  INC(i)
  END;
  IF (i >= LEN(command)) OR (command[i] # '.') THEN res := badcmd
  ELSE
    mname[i] := 0X;
    Load(mname, mod);
  END;
  IF res = noerr THEN
    INC(i);  j := 0;
    WHILE (i < LEN(command)) & (command[i] # 0X) DO
      cname[j] := command[i];  INC(i);  INC(j)
    END;
    cname[j] := 0X;
    p := ThisCommand(mod, cname);
    IF res = noerr THEN p END
  END;
  err := res
END Call;


PROCEDURE ImportPEImages;
VAR mod: Module;  next, p, i: INTEGER;
BEGIN
  (* Import image headers for modules loaded from PE EXE.  *)
  (* Note: not all images have been linked when we run, so *)
  (* only rely on values unaffected by linking.            *)
  NEW(root);  mod := root;  mod.header := Boot.BootHeader;
  REPEAT
    (* Extract module name from header *)
    p := mod.header + Boot.OffModName;  i := 0;
    SYSTEM.GET(p, mod.name[i]);
    WHILE (mod.name[i] # 0X) & (i < LEN(mod.name)-1) DO
      INC(p);  INC(i);  SYSTEM.GET(p, mod.name[i])
    END;
    mod.name[i] := 0X;  (* Guarantee 0 termination *)
    (*
    w.s("Imported ");    w.s(mod.name);
    w.s(" header at $"); w.h(mod.header); w.sl(".");
    *)
    (* Generate next module pointer, if any *)
    next := mod.header + GetInt(mod.header + Boot.OffModLength);
    IF GetInt(next + Boot.OffModLength) # 0 THEN
      NEW(mod.next);  mod.next.header := next;
    END;
    mod := mod.next;
  UNTIL mod = NIL;
END ImportPEImages;


BEGIN ImportPEImages
END Modules.