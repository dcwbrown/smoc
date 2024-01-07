MODULE CompileTests2;  IMPORT CompileTests;

TYPE
  r4 = POINTER TO r4desc;
  r3desc* = RECORD (CompileTests.r3desc) n4: r4 END;

BEGIN
END CompileTests2.
