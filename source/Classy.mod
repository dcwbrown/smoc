MODULE Classy;

IMPORT w := Writer;

TYPE 
  root*    = POINTER TO rootDesc;    rootDesc*    = RECORD        thing*:  INTEGER END;
  branch1* = POINTER TO branch1Desc; branch1Desc* = RECORD (root) thing1*: INTEGER END;
  branch2* = POINTER TO branch2Desc; branch2Desc* = RECORD (root) thing2*: INTEGER END;

VAR
  r*:  root;
  b1*: branch1;
  b2*: branch2;

BEGIN
  w.sl("Classy initialisation starting.");
  NEW(b1);  b1.thing := 0;  b1.thing1 := 1;
  NEW(b2);  b2.thing := 0;  b2.thing2 := 2;

  r := b1;
  w.s("r should be branch1. ");
  CASE r OF
  | branch1:  w.sl("r IS branch1.");
  | branch2:  w.sl("r IS branch2.");
  | root:     w.sl("r IS root.");
  END;

  w.sl("Classy initialisation complete.");
END Classy.
