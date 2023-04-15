MODULE Trapper;  (*$OBJECT*)

IMPORT SYSTEM, Bootstrapper;

TYPE
  Exception = POINTER TO ExceptionDesc;
  ExceptionDesc = RECORD
    code:         SYSTEM.CARD32;
    flags:        SYSTEM.CARD32;
    nested:       Exception;
    address:      INTEGER;
    NumberParams: SYSTEM.CARD32;
    Params:       ARRAY 15 OF INTEGER
  END;

  Context = POINTER TO ContextDesc;
  ContextDesc = RECORD
  END;

  ExceptionPointers = POINTER TO RECORD
    exception: Exception;
    context:   Context
  END;

  ExceptionHandlerProc = PROCEDURE(p: ExceptionPointers): INTEGER;

VAR
  AddVectoredExceptionHandler: PROCEDURE(first: INTEGER; filter: ExceptionHandlerProc): INTEGER;
  res: INTEGER;
  i,j: INTEGER;
  p: Exception;

PROCEDURE WriteException(code: INTEGER);
BEGIN
  IF    code = 0C0000005H THEN Bootstrapper.Msg("Access violation!")
  ELSIF code = 0C0000006H THEN Bootstrapper.Msg("In-page error!")
  ELSIF code = 0C000001DH THEN Bootstrapper.Msg("Illegal instruction!")
  ELSIF code = 0C000008EH THEN Bootstrapper.Msg("Divide by zero!")
  ELSIF code = 0C0000094H THEN Bootstrapper.Msg("Integer divide by zero!")
                          ELSE Bootstrapper.MsgI("Exception! Code: ", code)
  END
END WriteException;

PROCEDURE ExceptionHandler(p: ExceptionPointers): INTEGER;
VAR
  module:  Bootstrapper.ModuleHeader;
  address: INTEGER;
  trapadr: INTEGER;
  detail:  INTEGER;
  trap:    INTEGER;
  line:    INTEGER;
  col:     INTEGER;
  adr:     INTEGER;
BEGIN
  address := p.exception.address;
  module  := Bootstrapper.FirstModule;
  WHILE (module # NIL) & (module.length # 0) & ((address < module.code) OR (address > module.trap)) DO
    module := module.next
  END;

  IF module = NIL THEN
    WriteException(p.exception.code);
    Bootstrapper.MsgI("Not in loaded module code: at absolute address ", address)
  ELSE
    trapadr := module.trap;
    DEC(address, module.code);  (* Make address relative to modules code *)
    SYSTEM.GET(trapadr, detail);
    WHILE detail # -1 DO
      trap := ASR(detail, 60) MOD 10H;
      line := ASR(detail, 40) MOD 100000H;
      col  := ASR(detail, 30) MOD 400H;
      adr  := detail MOD 40000000H;
      IF adr = address THEN
        detail := -1  (* End loop *)
      ELSE
        INC(trapadr, 8);  SYSTEM.GET(trapadr, detail);
      END
    END;
    IF (adr = address) & (trap <= 8) THEN
      CASE trap OF
      | 0: Bootstrapper.Msg2("modkey trap in module ",  module.name);
      | 1: Bootstrapper.Msg2("array trap in module ",   module.name);
      | 2: Bootstrapper.Msg2("type trap in module ",    module.name);
      | 3: Bootstrapper.Msg2("string trap in module ",  module.name);
      | 4: Bootstrapper.Msg2("nil trap in module ",     module.name);
      | 5: Bootstrapper.Msg2("nilProc trap in module ", module.name);
      | 6: Bootstrapper.Msg2("divide trap in module ",  module.name);
      | 7: Bootstrapper.Msg2("assert trap in module ",  module.name);
      | 8: Bootstrapper.Msg2("rtl trap in module ",     module.name);
      END;
      Bootstrapper.MsgI("Line ", line);
      Bootstrapper.MsgI("Column ", col);
    ELSE
      WriteException(p.exception.code);
      Bootstrapper.Msg2("in module ", module.name);
      Bootstrapper.MsgI("at code offset ", address)
    END
  END;

  Bootstrapper.Halt(99)
RETURN 0 END ExceptionHandler;

BEGIN
  SYSTEM.GetProcAddress(AddVectoredExceptionHandler,
                        Bootstrapper.Kernel,
                        SYSTEM.ADR("AddVectoredExceptionHandler"));

  IF AddVectoredExceptionHandler = NIL THEN
    Bootstrapper.Msg("Trapper could not access AddVectoredExceptionHandler procedure.");
    Bootstrapper.Halt(99)
  END;

  res := AddVectoredExceptionHandler(1, ExceptionHandler);
END Trapper.
