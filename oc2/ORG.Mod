MODULE ORG;  (* DCWB June 2023; Code generator for X64 *)

(* Originally: N.Wirth, 16.4.2016 / 4.4.2017 / 17.9.2018  Oberon compiler; code generator for RISC*)

IMPORT SYSTEM, Files, ORS, ORB, w := Writer, K := Kernel;

CONST
  WordSize* = 8;
  MaxStrx   = 8000;  (* String buffer size *)

TYPE
  Item* = RECORD
    mode*: INTEGER;
    type*: ORB.Type;
    r:     INTEGER;
    a*:    INTEGER;
    b*:    INTEGER;
    rdo*:  BOOLEAN;    (* Read only *)
  END;

VAR
  Str:  ARRAY MaxStrx OF CHAR;
  Strx: INTEGER;

  pc*:  INTEGER;

  sourcefile: Files.File;
  source:     Files.Rider;
  sourceBol:  INTEGER;      (* pos at beginiing of line *)
  sourceLine: INTEGER;      (* Line number *)
  sourcechar: CHAR;

(* ---- Source display ---- *)

PROCEDURE Pos(): INTEGER;
BEGIN RETURN Files.Pos(source) - 1 END Pos;

PROCEDURE GetChar;
VAR prevch: CHAR;
BEGIN  ASSERT(~source.eof);
  prevch := sourcechar;
  Files.Read(source, sourcechar);
  IF source.eof THEN sourcechar := 0X
  ELSE
    IF (prevch = 0DX) OR (prevch = 0AX) THEN
      IF (prevch = 0DX) & (sourcechar = 0AX) THEN Files.Read(source, sourcechar) END;
      sourceBol := Pos();
      INC(sourceLine)
    END
  END
END GetChar;

PROCEDURE DisplaySourceToPos(pos: INTEGER);
VAR ch: CHAR;
BEGIN
  (* Skip trailing blanks to start of next non-blank line *)
  WHILE sourcechar = " " DO
    GetChar;
    WHILE (sourcechar = 0DX) OR (sourcechar = 0AX) DO GetChar END;
  END;

  (* Copy source text *)
  WHILE ~source.eof & (Pos() < pos) DO
    w.in(sourceLine, 4); w.s(": "); w.b(Pos() - sourceBol);

    WHILE ~source.eof & (Pos() < pos) & (sourcechar # 0DX) & (sourcechar # 0AX) DO
      w.c(sourcechar);  GetChar
    END;

    IF (sourcechar = 0DX) OR (sourcechar = 0AX) THEN
      GetChar;
      WHILE sourcechar = " " DO
        GetChar;
        WHILE (sourcechar = 0DX) OR (sourcechar = 0AX) DO GetChar END;
      END
    END;
    w.l
  END
END DisplaySourceToPos;

PROCEDURE start(str: ARRAY OF CHAR);
BEGIN DisplaySourceToPos(ORS.Pos());  w.s(str)
END start;

PROCEDURE wl; BEGIN w.l END wl;
PROCEDURE wc; BEGIN w.c(",") END wc;

PROCEDURE wbool(b: BOOLEAN);
BEGIN IF b THEN w.s("TRUE") ELSE w.s("FALSE") END END wbool;

PROCEDURE wform(form: INTEGER);
BEGIN
  IF    form = ORB.Byte    THEN w.s("Byte")
  ELSIF form = ORB.Bool    THEN w.s("Bool")
  ELSIF form = ORB.Char    THEN w.s("Char")
  ELSIF form = ORB.Int     THEN w.s("Int")
  ELSIF form = ORB.Int16   THEN w.s("Int16")
  ELSIF form = ORB.Int32   THEN w.s("Int32")
  ELSIF form = ORB.Card16  THEN w.s("Card16")
  ELSIF form = ORB.Card32  THEN w.s("Card32")
  ELSIF form = ORB.Real    THEN w.s("Real")
  ELSIF form = ORB.Set     THEN w.s("Set")
  ELSIF form = ORB.Pointer THEN w.s("Pointer")
  ELSIF form = ORB.NilTyp  THEN w.s("NilTyp")
  ELSIF form = ORB.NoTyp   THEN w.s("NoTyp")
  ELSIF form = ORB.Proc    THEN w.s("Proc")
  ELSIF form = ORB.String  THEN w.s("String")
  ELSIF form = ORB.Array   THEN w.s("Array")
  ELSIF form = ORB.Record  THEN w.s("Record")
                           ELSE w.i(form)
  END
END wform;

PROCEDURE wclass(class: INTEGER);
BEGIN
  IF    class = ORB.Head  THEN w.s("Head")
  ELSIF class = ORB.Const THEN w.s("Const")
  ELSIF class = ORB.Var   THEN w.s("Var")
  ELSIF class = ORB.Par   THEN w.s("Par")
  ELSIF class = ORB.Fld   THEN w.s("Fld")
  ELSIF class = ORB.Typ   THEN w.s("Typ")
  ELSIF class = ORB.SProc THEN w.s("SProc")
  ELSIF class = ORB.SFunc THEN w.s("SFunc")
  ELSIF class = ORB.Mod   THEN w.s("Mod")
                          ELSE w.i(class)
  END
END wclass;

PROCEDURE wsym(sym: INTEGER);
BEGIN
  IF    sym = ORS.times     THEN w.s("*")
  ELSIF sym = ORS.rdiv      THEN w.s("/")
  ELSIF sym = ORS.div       THEN w.s("DIV")
  ELSIF sym = ORS.mod       THEN w.s("MOD")
  ELSIF sym = ORS.and       THEN w.s("&")
  ELSIF sym = ORS.plus      THEN w.s("+")
  ELSIF sym = ORS.minus     THEN w.s("-")
  ELSIF sym = ORS.or        THEN w.s("OR")
  ELSIF sym = ORS.eql       THEN w.s("=")
  ELSIF sym = ORS.neq       THEN w.s("#")
  ELSIF sym = ORS.lss       THEN w.s("<")
  ELSIF sym = ORS.leq       THEN w.s("<=")
  ELSIF sym = ORS.gtr       THEN w.s(">")
  ELSIF sym = ORS.geq       THEN w.s(">=")
  ELSIF sym = ORS.in        THEN w.s("IN")
  ELSIF sym = ORS.is        THEN w.s("IS")
  ELSIF sym = ORS.arrow     THEN w.s("^")
  ELSIF sym = ORS.period    THEN w.s(".")
  ELSIF sym = ORS.char      THEN w.s("CHAR")
  ELSIF sym = ORS.int       THEN w.s("int")
  ELSIF sym = ORS.real      THEN w.s("real")
  ELSIF sym = ORS.false     THEN w.s("FALSE")
  ELSIF sym = ORS.true      THEN w.s("TRUE")
  ELSIF sym = ORS.nil       THEN w.s("NIL")
  ELSIF sym = ORS.string    THEN w.c(22X)
  ELSIF sym = ORS.not       THEN w.s("~")
  ELSIF sym = ORS.lparen    THEN w.s("(")
  ELSIF sym = ORS.lbrak     THEN w.s("[")
  ELSIF sym = ORS.lbrace    THEN w.s("{")
  ELSIF sym = ORS.ident     THEN w.s("ident")
  ELSIF sym = ORS.if        THEN w.s("IF")
  ELSIF sym = ORS.while     THEN w.s("WHILE")
  ELSIF sym = ORS.repeat    THEN w.s("REPEAT")
  ELSIF sym = ORS.case      THEN w.s("CASE")
  ELSIF sym = ORS.for       THEN w.s("FOR")
  ELSIF sym = ORS.comma     THEN w.s(",")
  ELSIF sym = ORS.colon     THEN w.s(":")
  ELSIF sym = ORS.becomes   THEN w.s(":=")
  ELSIF sym = ORS.upto      THEN w.s("..")
  ELSIF sym = ORS.rparen    THEN w.s(")")
  ELSIF sym = ORS.rbrak     THEN w.s("]")
  ELSIF sym = ORS.rbrace    THEN w.s("}")
  ELSIF sym = ORS.then      THEN w.s("THN")
  ELSIF sym = ORS.of        THEN w.s("OF")
  ELSIF sym = ORS.do        THEN w.s("DO")
  ELSIF sym = ORS.to        THEN w.s("TO")
  ELSIF sym = ORS.by        THEN w.s("BY")
  ELSIF sym = ORS.semicolon THEN w.s(";")
  ELSIF sym = ORS.end       THEN w.s("END")
  ELSIF sym = ORS.bar       THEN w.s("|")
  ELSIF sym = ORS.else      THEN w.s("ELSE")
  ELSIF sym = ORS.elsif     THEN w.s("ELSIF")
  ELSIF sym = ORS.until     THEN w.s("UNTIL")
  ELSIF sym = ORS.return    THEN w.s("RETURN")
  ELSIF sym = ORS.array     THEN w.s("ARRAY")
  ELSIF sym = ORS.record    THEN w.s("RECORD")
  ELSIF sym = ORS.pointer   THEN w.s("POINTER")
  ELSIF sym = ORS.const     THEN w.s("CONST")
  ELSIF sym = ORS.type      THEN w.s("TYPE")
  ELSIF sym = ORS.var       THEN w.s("VAR")
  ELSIF sym = ORS.procedure THEN w.s("PROCEDURE")
  ELSIF sym = ORS.begin     THEN w.s("BEGIN")
  ELSIF sym = ORS.import    THEN w.s("IMPORT")
  ELSIF sym = ORS.module    THEN w.s("MODULE")
                            ELSE w.i(sym)
  END
END wsym;


PROCEDURE wtype(t: ORB.Type);
BEGIN
  IF t = NIL THEN w.s("(NIL)") ELSE
    w.s("(form ");  wform(t.form);  w.s(", mno ");   w.i(t.mno);
    w.s(", len ");  w.i(t.len);     w.s(", size ");  w.i(t.size);  w.c(")")
  END
END wtype;

PROCEDURE wobject(o: ORB.Object);
BEGIN
  w.s("(class ");  wclass(o.class);
  w.s(", type ");  wtype(o.type);
  w.s(", name '"); w.s(o.name);
  w.s("', val ");  w.i(o.val);
  w.s(", lev ");   w.i(o.lev);
  w.c(")")
END wobject;

PROCEDURE witem(i: Item);
BEGIN
  w.s("(mode ");  wclass(i.mode);
  w.s(", r "); w.i(i.r);
  w.s(", a "); w.i(i.a);
  w.s(", b "); w.i(i.b);
  IF i.rdo THEN w.s(", readonly") END;
  w.s(", type ");  wtype(i.type);  w.c(")")
END witem;



(* ---- Item construction ---- *)

PROCEDURE MakeConstItem*(VAR x: Item;  t: ORB.Type;  v: INTEGER);
BEGIN
  start("MakeConstItem, value: "); w.i(v);
  w.s(", type "); wtype(t); wl;
  x.mode := ORB.Const;
  x.type := t;
  x.a    := v
END MakeConstItem;

PROCEDURE MakeItem*(VAR x: Item; y: ORB.Object; curlev: INTEGER);
BEGIN
  x.mode := y.class;
  x.type := y.type;
  x.a    := y.val;
  x.rdo  := y.rdo;
  IF y.class = ORB.Par THEN
    x.b := 0
  ELSIF (y.class = ORB.Const) & (y.type.form = ORB.String) THEN
    x.b := y.lev  (*len*) ;
  ELSE
    x.r := y.lev
  END;
  IF (y.lev > 0) & (y.lev # curlev) & (y.class # ORB.Const) THEN
    ORS.Mark("not accessible ")
  END;

  start("MakeItem "); wclass(x.mode);
  w.s("/"); wform(y.type.form);
  w.s(", r "); w.i(x.r);
  w.s(", a "); w.i(x.a);
  w.s(", b "); w.i(x.b);
  IF x.rdo THEN w.s(", readonly") END;
  w.s(" <- from object: "); wobject(y);
  w.s(", curlev: ");        w.i(curlev); w.l
END MakeItem;

PROCEDURE MakeRealItem*(VAR x: Item; val: REAL);
BEGIN
  start("MakeRealItem(x; val)"); wl;
  x.mode := ORB.Const;
  x.type := ORB.realType;
  x.a    := SYSTEM.VAL(INTEGER, val)
END MakeRealItem;

PROCEDURE MakeStringItem*(VAR x: Item; len: INTEGER); (*copies string from ORS-buffer to ORG-string array*)
VAR i: INTEGER;
BEGIN
  start("MakeStringItem('");
  i := 0;  WHILE (i < len) & (ORS.str[i] # 0X) DO w.c(ORS.str[i]); INC(i) END;
  w.sl("')");
  x.mode := ORB.Const;
  x.type := ORB.strType;
  x.a    := Strx;
  x.b    := len;
  i      := 0;
  IF Strx + len + 4 < MaxStrx THEN
    WHILE len > 0 DO Str[Strx] := ORS.str[i]; INC(Strx); INC(i); DEC(len) END;
    WHILE Strx MOD 4 # 0 DO Str[Strx] := 0X; INC(Strx) END
  ELSE
    ORS.Mark("too many strings")
  END
END MakeStringItem;



(* ---- Item conversion ---- *)

PROCEDURE StrToChar*(VAR x: Item);
BEGIN start("StrToChar(x)  x"); witem(x); wl;
END StrToChar;


(* ---- slection - fields, indices & pointers ---- *)

PROCEDURE Index*(VAR x, y: Item);   (* x := x[y] *)
BEGIN start("Index(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl;
END Index;

PROCEDURE DeRef*(VAR x: Item);
BEGIN start("DeRef(x), x: "); witem(x); wl;
END DeRef;

PROCEDURE Field*(VAR x: Item; y: ORB.Object);   (* x := x.y *)
BEGIN start("Field(x: Item; y: Object)"); wl;
  w.s("  x: "); witem(x);   wl;
  w.s("  y: "); wobject(y); wl;
END Field;


(* ---- Paramter passing ---- *)

PROCEDURE ValueParam*(VAR x: Item);
BEGIN start("ValueParam(x: Item)  x: "); witem(x);  wl; (* load(x) *)
END ValueParam;

PROCEDURE VarParam*(VAR x: Item; ftype: ORB.Type);
BEGIN start("VarParam(x: Item; ftype: Type)"); wl;
  w.s("  x:     "); witem(x);     wl;
  w.s("  ftype: "); wtype(ftype); wl;
END VarParam;

PROCEDURE OpenArrayParam*(VAR x: Item);
BEGIN start("OpenArrayParam(x: Item)  x: "); witem(x);  wl;
END OpenArrayParam;

PROCEDURE StringParam*(VAR x: Item);
BEGIN start("StringParam(x: Item)  x: "); witem(x);  wl;
END StringParam;


(* ---- Procedure / function calls ---- *)

PROCEDURE PrepCall*(VAR x: Item; VAR r: INTEGER);
BEGIN start("PrepCall(x: Item; r "); w.i(r); w.s(")  x "); witem(x); wl;
END PrepCall;

PROCEDURE Call*(VAR x: Item; r: INTEGER);
BEGIN start("Call(x: Item; r "); w.i(r); w.s(")  x "); witem(x); wl;
END Call;

PROCEDURE Enter*(parblksize, locblksize: INTEGER; int: BOOLEAN);
BEGIN
  start("Enter(parblksize ");  w.i(parblksize);
  w.s(", locblksize ");        w.i(locblksize);  w.sl(")")
END Enter;

PROCEDURE Return*(form: INTEGER; VAR x: Item; size: INTEGER; int: BOOLEAN);
BEGIN start("Return("); wform(form);
  w.s("; x; size "); w.i(size);
  IF int THEN w.sl("; int)") ELSE w.sl("; ~int)") END;
  w.s("  x:    "); witem(x);    wl;
END Return;


(* ---- Standard functions ---- *)

PROCEDURE Abs*(VAR x: Item);
BEGIN start("Abs(x)  x "); witem(x); wl
END Abs;

PROCEDURE Odd*(VAR x: Item);
BEGIN start("Odd(x)  x "); witem(x); wl
END Odd;

PROCEDURE Floor*(VAR x: Item);
BEGIN start("Floor(x)  x "); witem(x); wl
END Floor;

PROCEDURE Float*(VAR x: Item);
BEGIN start("Float(x)  x "); witem(x); wl
END Float;

PROCEDURE Ord*(VAR x: Item);
BEGIN start("Ord(x)  x "); witem(x); wl
END Ord;

PROCEDURE Len*(VAR x: Item);
BEGIN start("Len(x)  x "); witem(x); wl
END Len;

PROCEDURE Shift*(fct: INTEGER; VAR x, y: Item);
BEGIN start("Shift(");
  IF    fct = 0 THEN w.s("LSL")
  ELSIF fct = 1 THEN w.s("ASR")
  ELSIF fct = 2 THEN w.s("ROR")
  ELSE               w.i(fct)
  END;
  w.s("; x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END Shift;

PROCEDURE ADC*(VAR x, y: Item);
BEGIN start("ADC(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
  END ADC;

PROCEDURE SBC*(VAR x, y: Item);
BEGIN start("SBC(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
  END SBC;

PROCEDURE UML*(VAR x, y: Item);
BEGIN start("UML(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
  END UML;

PROCEDURE Bit*(VAR x, y: Item);
BEGIN start("Bit(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
  END Bit;

PROCEDURE Register*(VAR x: Item);
BEGIN start("Register(x)  x "); witem(x); wl
END Register;

PROCEDURE Adr*(VAR x: Item);
BEGIN start("Adr(x)  x "); witem(x); wl
END Adr;

PROCEDURE Condition*(VAR x: Item);
BEGIN start("Condition(x)  x "); witem(x); wl
END Condition;


(* ---- Standard procedures --- *)

PROCEDURE Increment*(upordown: INTEGER; VAR x, y: Item);
BEGIN start("Increment(");
  IF upordown = 0 THEN w.s("up") ELSE w.s("down") END;
  w.sl(", x, y) ");
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END Increment;

PROCEDURE Include*(inorex: INTEGER; VAR x, y: Item);
BEGIN start("Include"); wl
END Include;

PROCEDURE Assert*(VAR x: Item);
BEGIN start("Assert(x)  x: ");  witem(x);  wl
END Assert;

PROCEDURE New*(VAR x: Item);
BEGIN start("New(x)  x: ");  witem(x);  wl
END New;

PROCEDURE Led*(VAR x: Item);
BEGIN start("Led(x)  x: ");  witem(x);  wl
END Led;

PROCEDURE LDPSR*(VAR x: Item);
BEGIN start("LDPSR(x)  x: ");  witem(x);  wl
END LDPSR;

PROCEDURE Pack*(VAR x, y: Item);
BEGIN start("Pack(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END Pack;

PROCEDURE Unpk*(VAR x, y: Item);
BEGIN start("Unpk(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END Unpk;

PROCEDURE Get*(VAR x, y: Item);
BEGIN start("Get(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END Get;

PROCEDURE Put*(VAR x, y: Item);
BEGIN start("Put(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END Put;

PROCEDURE LDREG*(VAR x, y: Item);
BEGIN start("LDREG(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END LDREG;

PROCEDURE Copy*(VAR x, y, z: Item);
BEGIN start("Copy(x, y, z)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl;
  w.s("  z: "); witem(z); wl
END Copy;


(* ---- Sets ---- *)

PROCEDURE Set*(VAR x, y: Item);   (* x := {x .. y} *)
BEGIN start("Set(x, y)  x := {x..y}"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl;
END Set;

PROCEDURE Singleton*(VAR x: Item);  (* x := {x} *)
BEGIN start("Singleton(x)  x := {x} "); witem(x); wl
END Singleton;

PROCEDURE In*(VAR x, y: Item);  (* x := x IN y *)
BEGIN start("In(x, y)  x := x IN y"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl;
END In;

PROCEDURE SetOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
BEGIN start("SetOp("); wsym(op); w.sl("; x, y)  x := x op y");
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl;
END SetOp;


(* ---- Boolean operators ---- *)

PROCEDURE Not*(VAR x: Item);   (* x := ~x *)
BEGIN start("Not(x)  x "); witem(x); wl
END Not;

PROCEDURE And1*(VAR x: Item);   (* x := x & *)
BEGIN start("And1(x)  x "); witem(x); wl
END And1;

PROCEDURE Or1*(VAR x: Item);   (* x := x OR *)
BEGIN start("Or1(x)  x "); witem(x); wl
END Or1;

PROCEDURE And2*(VAR x, y: Item);   (* x := x & y *)
BEGIN start("And2(x)  x "); witem(x); wl
END And2;

PROCEDURE Or2*(VAR x, y: Item);   (* x := x OR y *)
BEGIN start("Or2(x)  x "); witem(x); wl
END Or2;

PROCEDURE IntRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
BEGIN start("IntRelation("); wsym(op); w.sl(", x, y)");
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl;
  END IntRelation;

PROCEDURE RealRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
BEGIN start("RealRelation("); wsym(op); w.sl(", x, y)");
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END RealRelation;

PROCEDURE StringRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
BEGIN start("StringRelation("); wsym(op); w.sl(", x, y)");
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END StringRelation;



(* ---- Arithmetic operators ---- *)

PROCEDURE AddOp*(op: INTEGER; VAR x, y: Item);   (* x := x +- y *)
BEGIN start("AddOp("); wsym(op); w.sl("; x, y) x := x +- y");
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END AddOp;

PROCEDURE MulOp*(VAR x, y: Item);   (* x := x * y *)
BEGIN start("MulOp(x, y)  x := x * y"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END MulOp;

PROCEDURE DivOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
BEGIN start("DivOp("); wsym(op); w.sl("; x, y) x := x /DIV y");
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END DivOp;

PROCEDURE RealOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
BEGIN start("RealOp("); wsym(op); w.sl("; x, y) x := x op y");
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END RealOp;

PROCEDURE Neg*(VAR x: Item);   (* x := -x *)
BEGIN start("Neg(x)  x := -x"); wl;
  w.s("  x: "); witem(x); wl;
END Neg;


(* ---- Control flow ---- *)

PROCEDURE CFJump*(VAR x: Item);
BEGIN start("CFJump(x)  x "); witem(x);  wl
END CFJump;

PROCEDURE FJump*(VAR L: INTEGER);
BEGIN start("FJump(L "); w.i(L); w.sl(")")
END FJump;

PROCEDURE BJump*(VAR L: INTEGER);
BEGIN start("BJump(L "); w.i(L); w.sl(")")
END BJump;

PROCEDURE CBJump*(VAR x: Item; L: INTEGER);
BEGIN start("CBJump(x, L "); w.i(L);
  w.s(")  x "); witem(x); wl
END CBJump;

PROCEDURE FixLink*(L: INTEGER);
BEGIN start("FixLink(L "); w.i(L); w.sl(")")
END FixLink;

PROCEDURE FixOne*(at: INTEGER);
BEGIN start("FixOne(at "); w.i(at); w.sl(")")
END FixOne;

PROCEDURE Fixup*(VAR x: Item);  BEGIN FixLink(x.a) END Fixup;

PROCEDURE Here*(): INTEGER;
BEGIN start("Here -> pc "); w.i(pc); wl;
RETURN pc END Here;

PROCEDURE For0*(VAR x, y: Item);
BEGIN start("For0(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END For0;

PROCEDURE For1*(VAR x, y, z, incr: Item; VAR L: INTEGER);
BEGIN start("For1(x, y, z, incr; L "); w.i(L);  w.sl(")");
  w.s("  x:    "); witem(x);    wl;
  w.s("  y:    "); witem(y);    wl;
  w.s("  z:    "); witem(z);    wl;
  w.s("  incr: "); witem(incr); wl
END For1;

PROCEDURE For2*(VAR x, y, incr: Item);
BEGIN start("For2(x, y)"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END For2;


(* ---- Assignment ---- *)

PROCEDURE Store*(VAR x, y: Item); (* x := y *)
BEGIN start("Store(x, y) x := y"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl;
END Store;

PROCEDURE StoreStruct*(VAR x, y: Item); (* x := y, frame = 0 *)
BEGIN start("StoreStruct(x, y) x := y"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END StoreStruct;

PROCEDURE CopyString*(VAR x, y: Item);  (* x := y *)
BEGIN start("CopyString(x, y) x := y"); wl;
  w.s("  x: "); witem(x); wl;
  w.s("  y: "); witem(y); wl
END CopyString;


(* ---- ---- *)

PROCEDURE BuildTD*(T: ORB.Type; VAR dc: INTEGER);
BEGIN start("BuildTD(T, dc "); w.i(dc); w.sl(")");
  w.s("  T: "); wtype(T); wl
END BuildTD;

PROCEDURE TypeTest*(VAR x: Item; T: ORB.Type; varpar, isguard: BOOLEAN);
BEGIN start("TypeTest(x, T, ");
  IF varpar  THEN w.s("varpar, ")  ELSE w.s("~varpar, ")  END;
  IF isguard THEN w.sl("isguard)") ELSE w.sl("~isguard)") END;
  w.s("  x: "); witem(x); wl;
  w.s("  T: "); wtype(T); wl
END TypeTest;

PROCEDURE CheckRegs*;
BEGIN start("CheckRegs"); wl
END CheckRegs;


(* ---- ---- *)

PROCEDURE Open*(v: INTEGER; sourcefn: ARRAY OF CHAR);
BEGIN w.s("ORG Open(v "); w.i(v); w.s("; '"); w.s(sourcefn); w.sl("')");
  sourcefile := Files.Old(sourcefn);  Files.Set(source, sourcefile, 0);
  sourceBol := 0;  sourceLine := 1;  GetChar;
END Open;

PROCEDURE SetDataSize*(dc: INTEGER);
BEGIN w.s("SetDataSize(dc "); w.i(dc); w.sl(")");
END SetDataSize;

PROCEDURE Header*;
BEGIN w.sl("Header")
END Header;

PROCEDURE Close*(VAR modid: ORS.Ident; key, nofent: INTEGER);
BEGIN w.s("Close("); w.s(modid);
  w.s("; key ");     w.i(key);
  w.s(", nofent ");  w.i(nofent); w.sl(")")
END Close;


BEGIN
END ORG.
