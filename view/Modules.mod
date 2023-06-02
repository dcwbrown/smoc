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
  Module*     = Boot.ModuleHeader;
  Command*    = PROCEDURE;

VAR
  root*:      Module;          (* First loaded module              *)
  res*:       INTEGER;         (* error code                       *)
  importing*: Boot.ModuleName; (* module name that errors refer to *)
  imported*:  Boot.ModuleName; (* module name that errors refer to *)


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


PROCEDURE ThisCommand(mod: Module;  command: ARRAY OF CHAR): Command;
VAR
  cmd:  INTEGER;
  adr:  INTEGER;
  name: Boot.ModuleName;
  i:    INTEGER;
BEGIN
  error(nocmd, mod.name);  cmd := 0;
  w.s("Lookup command '"); w.s(command); w.s("' in module "); w.s(mod.name); w.sl(".");
  IF mod.commands # 0 THEN
    w.s("mod.commands $"); w.h(mod.commands); w.sl(".");
    adr := mod.commands;  SYSTEM.GET(adr, name[0]);  INC(adr);
    WHILE (name[0] # 0X) & (cmd = 0) DO
      i := 0;  REPEAT INC(i); SYSTEM.GET(adr, name[i]);  INC(adr) UNTIL name[i] = 0X;
      w.s("  consider '"); w.s(name); w.sl("'.");
      IF command = name THEN SYSTEM.GET(adr, cmd);  res := noerr END;
      INC(adr, 8);
      SYSTEM.GET(adr, name[0]);  INC(adr)
    END;
    IF cmd # 0 THEN INC(cmd, mod.base) END  (* Relocation *)
  END
RETURN SYSTEM.VAL(Command, cmd) END ThisCommand;


PROCEDURE Call*(command: ARRAY OF CHAR; VAR err: INTEGER);
VAR
  mod:   Module;
  mname: Boot.ModuleName;
  cname: Boot.ModuleName;
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


BEGIN
  root := Boot.FirstModule
END Modules.