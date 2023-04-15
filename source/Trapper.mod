MODULE Trapper;  (*$OBJECT*)

IMPORT SYSTEM, Kernel;

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
  IF    code = 0C0000005H THEN Kernel.Msg("Access violation!")
  ELSIF code = 0C0000006H THEN Kernel.Msg("In-page error!")
  ELSIF code = 0C000001DH THEN Kernel.Msg("Illegal instruction!")
  ELSIF code = 0C000008EH THEN Kernel.Msg("Divide by zero!")
  ELSIF code = 0C0000094H THEN Kernel.Msg("Integer divide by zero!")
                          ELSE Kernel.MsgI("Exception! Code: ", code)
  END
END WriteException;

PROCEDURE ExceptionHandler(p: ExceptionPointers): INTEGER;
VAR
  module:  Kernel.ModuleHeader;
  address: INTEGER;
  trapadr: INTEGER;
  detail:  INTEGER;
  trap:    INTEGER;
  line:    INTEGER;
  col:     INTEGER;
  adr:     INTEGER;
BEGIN
  address := p.exception.address;
  module  := Kernel.FirstModule;
  WHILE (module # NIL) & (module.length # 0) & ((address < module.code) OR (address > module.trap)) DO
    module := module.next
  END;

  IF module = NIL THEN
    WriteException(p.exception.code);
    Kernel.MsgI("Not in loaded module code: at absolute address ", address)
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
      | 0: Kernel.Msg2("modkey trap in module ",  module.name);
      | 1: Kernel.Msg2("array trap in module ",   module.name);
      | 2: Kernel.Msg2("type trap in module ",    module.name);
      | 3: Kernel.Msg2("string trap in module ",  module.name);
      | 4: Kernel.Msg2("nil trap in module ",     module.name);
      | 5: Kernel.Msg2("nilProc trap in module ", module.name);
      | 6: Kernel.Msg2("divide trap in module ",  module.name);
      | 7: Kernel.Msg2("assert trap in module ",  module.name);
      | 8: Kernel.Msg2("rtl trap in module ",     module.name);
      END;
      Kernel.MsgI("Line ", line);
      Kernel.MsgI("Column ", col);
    ELSE
      WriteException(p.exception.code);
      Kernel.Msg2("in module ", module.name);
      Kernel.MsgI("at code offset ", address)
    END
  END;

  Kernel.Halt(99)
RETURN 0 END ExceptionHandler;

BEGIN
  SYSTEM.GetProcAddress(AddVectoredExceptionHandler,
                        Kernel.Kernel,
                        SYSTEM.ADR("AddVectoredExceptionHandler"));

  IF AddVectoredExceptionHandler = NIL THEN
    Kernel.Msg("Trapper could not access AddVectoredExceptionHandler procedure.");
    Kernel.Halt(99)
  END;

  res := AddVectoredExceptionHandler(1, ExceptionHandler);
END Trapper.
