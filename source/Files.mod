MODULE Files; (*$la+*) (*$lc+*)
IMPORT SYSTEM, K := Kernel, Heap, w := Writer;

CONST
  (* Win32 Const *)
  InvalidFileAttributes  = ORD({0..31});
  FileAttributeReadOnly  = {0};
  FileAttributeTemporary = {8};   (*      100H *)
  FileFlagDeleteOnClose  = {26};  (* 400,0000H *)

  GenericRead  = {31};
  GenericWrite = {30};

  openExisting = 3;
  CreateAlways = 2;
  CreateNew    = 1;

  MaxPath = 260;

  FileBegin   = 0;
  FileCurrent = 1;
  FileEnd     = 2;

  MoveFileCopyAllowed = {1};

  FileShareDelete = {2};
  FileShareRead   = {0};
  FileShareWrite  = {1};

  (* File operations for CreateFile *)
  FileOpenR    = 0;
  FileOpenW    = 1;
  FileNew      = 2;
  FileRegister = 3;


TYPE
  Int   = SYSTEM.CARD32;

  PathStr16 = ARRAY MaxPath+1 OF SYSTEM.CARD16;
  PathStr   = ARRAY MaxPath+1 OF CHAR;

  File*     = POINTER TO FileDesc;
  FileDesc* = RECORD (Heap.FinalisedDesc)
                new:    BOOLEAN;
                ronly:  BOOLEAN;
                hFile:  INTEGER;
                name*:  PathStr;
                temp:   PathStr;
                pos:    INTEGER;
                len:    INTEGER
              END;

  Rider* = RECORD
             eof*: BOOLEAN;
             res*: INTEGER;
             f:    File;
             pos:  INTEGER
           END;

VAR
  tempId: INTEGER;

  (* Win32 Interface *)
  GetFileAttributesW:      PROCEDURE(lpFileName: INTEGER): INTEGER;
  DeleteFileW:             PROCEDURE(lpFilename: INTEGER): INTEGER;
  CloseHandle:             PROCEDURE(hObject: INTEGER): INTEGER;
  FlushFileBuffers:        PROCEDURE(hFile: INTEGER): INTEGER;
  SetEndOfFile:            PROCEDURE(hFile: INTEGER): INTEGER;
  GetFileSizeEx:           PROCEDURE(hFile, lpFileSize: INTEGER): INTEGER;
  GetCurrentProcessId:     PROCEDURE(): INTEGER;
  MoveFileExW:             PROCEDURE(lpExistingFileName, lpNewFileName, dwFlags: INTEGER): INTEGER;
  CreateFileW:             PROCEDURE(lpFileName, dwDesiredAccess, dwShareMode,
                                     lpSecurityAttributes, dwCreationDisposition,
                                     dwFlagsAndAttributes, hTemplateFile: INTEGER): INTEGER;
  ReadFile:                PROCEDURE(hFile, lpBuffer, nNumberOfBytesToRead,
                                     lpNumberOfBytesRead, lpOverlapped: INTEGER): INTEGER;
  WriteFile:               PROCEDURE(hFile, lpBuffer, nNumberOfBytesToWrite,
                                     lpNumberOfBytesWritten, lpOverlapped: INTEGER): INTEGER;
  SetFilePointerEx:        PROCEDURE(hFile, liDistanceToMove,
                                     lpNewFilePointer, dwMoveMethod: INTEGER): INTEGER;
  GetEnvironmentVariableW: PROCEDURE(lpName, lpBuffer, nSize: INTEGER): INTEGER;
  GetFileAttributesExW:    PROCEDURE(lpName, fInfoLevelId, lpFileInformation: INTEGER): INTEGER;
                           (* fInfoLevelId Must be 0 (GetFileExInfoStandard) *)
  GetLastError:            PROCEDURE(): INTEGER;

PROCEDURE Finalise(ptr: Heap.Finalised);
VAR bRes: INTEGER; f: File;
BEGIN
  f := ptr(File); bRes := CloseHandle(f.hFile); ASSERT(bRes # 0);
  (*Out.String('Release ');
  IF f.new THEN Out.String(f.temp) ELSE Out.String(f.name) END; Out.Ln*)
END Finalise;

(* Operations: FileOpenR, FileOpenW, FileNew, FileRegister. *)
PROCEDURE CreateFile(name: ARRAY OF CHAR; op: INTEGER): INTEGER;
VAR hFile, len, access, mode, disposition, flags: INTEGER; name16: PathStr16;
BEGIN
  IF op = FileNew THEN
    disposition := CreateNew;
    flags       := ORD(FileAttributeTemporary+FileFlagDeleteOnClose);
  ELSIF op = FileRegister THEN
    disposition := CreateAlways;
    flags := 0;
  ELSE
    disposition := openExisting;
    flags       := 0;
  END;
  IF op = FileOpenR THEN
    access      := ORD(GenericRead);
    mode        := ORD(FileShareRead);
  ELSE
    access      := ORD(GenericRead+GenericWrite);
    mode        := ORD(FileShareRead+FileShareWrite+FileShareDelete);
  END;
  len := K.Utf8ToUtf16(name, name16);
  hFile := CreateFileW(SYSTEM.ADR(name16), access, mode, 0, disposition, flags, 0);
RETURN hFile END CreateFile;

PROCEDURE GetFileAttributes(name: ARRAY OF CHAR): INTEGER;
VAR name16: PathStr16;  len, attr: INTEGER;
BEGIN
  len  := K.Utf8ToUtf16(name, name16);
  attr := GetFileAttributesW(SYSTEM.ADR(name16));
RETURN attr END GetFileAttributes;

PROCEDURE NewFile(VAR file: File; hFile: INTEGER);
BEGIN
  NEW(file); file.hFile := hFile;
  Heap.RegisterFinalised(file, Finalise)
END NewFile;

PROCEDURE Old*(name: ARRAY OF CHAR): File;
VAR file: File; hFile: INTEGER;
    attr: INTEGER; ronly: BOOLEAN; bRes: INTEGER;
BEGIN
  attr := GetFileAttributes(name);
  IF attr # InvalidFileAttributes THEN
    ronly := FileAttributeReadOnly * SYSTEM.VAL(SET, attr) # {};
    IF ronly THEN
      hFile := CreateFile(name, FileOpenR)
    ELSE
      hFile := CreateFile(name, FileOpenW)
    END;
    IF hFile # -1 THEN
      NewFile(file, hFile);
      file.new   := FALSE;
      file.ronly := ronly;
      file.name  := name;
      file.pos   := 0;
      bRes := GetFileSizeEx(hFile, SYSTEM.ADR(file.len))
    ELSE ASSERT(FALSE)
    END
  END;
  RETURN file
END Old;

PROCEDURE SetWString(s: ARRAY OF CHAR; VAR d: ARRAY OF SYSTEM.CARD16);
VAR len: INTEGER;
BEGIN len := K.Utf8ToUtf16(s, d) END SetWString;

PROCEDURE GetUserProfile(VAR dir: ARRAY OF CHAR; VAR len: INTEGER);
VAR s, userprofile: PathStr16;
BEGIN
  SetWString("USERPROFILE", s);
  len := GetEnvironmentVariableW(SYSTEM.ADR(s), SYSTEM.ADR(userprofile), LEN(userprofile));
  len := K.Utf16ToUtf8(userprofile, dir);
(*w.s("Got user profile as '"); w.s(dir); w.sl("'.");*)
END GetUserProfile;

PROCEDURE ToHex(h: INTEGER): CHAR;
VAR  ch: CHAR;
BEGIN h := h MOD 16;
  IF h < 10 THEN ch := CHR(h +      ORD("0"))
  ELSE           ch := CHR(h - 10 + ORD("a")) END
RETURN ch END ToHex;

PROCEDURE Append* (src: ARRAY OF CHAR;  VAR dst: ARRAY OF CHAR);
VAR i, j: INTEGER;
BEGIN
  i := 0;  WHILE dst[i] # 0X DO INC(i) END;  j := 0;
  WHILE src[j] # 0X DO  dst[i] := src[j];  INC(i);  INC(j)  END;
  dst[i] := 0X
END Append;

PROCEDURE AppendHex(h: INTEGER; VAR dst: ARRAY OF CHAR);
VAR hex: ARRAY 20 OF CHAR; i, j: INTEGER;
BEGIN
  i := LEN(hex)-1;
  WHILE (i >= 0) & (h # 0) DO
    hex[i] := ToHex(h);  DEC(i);  h := h DIV 16
  END;
  IF i = 19 THEN hex[i] := "0"; DEC(i) END;
  INC(i);
  j := 0;  WHILE dst[j] # 0X DO INC(j) END;
  WHILE i < LEN(hex) DO  dst[j] := hex[i];  INC(i);  INC(j)  END;
  dst[j] := 0X
END AppendHex;

PROCEDURE MakeTempName(VAR name: ARRAY OF CHAR);
VAR
pid, time, len: INTEGER;
BEGIN
  pid  := GetCurrentProcessId();
  time := K.Time();
  GetUserProfile(name, len);
  IF len < MaxPath-60 THEN
    Append("\.smoctmp", name); AppendHex(pid, name);
    Append("-", name);         AppendHex(time, name);
    Append("-", name);         AppendHex(tempId, name);
  ELSE
    name := ".tmp";    AppendHex(time, name);
    Append("-", name); AppendHex(tempId, name);
  END;
  INC(tempId);
END MakeTempName;

PROCEDURE New*(name: ARRAY OF CHAR): File;
VAR
  hFile: INTEGER;
  file:  File;
  temp:  PathStr;
BEGIN
  MakeTempName(temp);
  hFile := CreateFile(temp, FileNew);
  IF hFile # -1 THEN
    NewFile(file, hFile);
    file.new  := TRUE;
    file.name := name;
    file.temp := temp;
    file.pos := 0; file.len := 0
  END;
  RETURN file
END New;


PROCEDURE Register*(f: File);
VAR
  hFile2:       INTEGER;
  buf:          ARRAY 10000H OF BYTE;
  bRes:         INTEGER;
  bytesRead:    SYSTEM.CARD32;
  bytesWritten: SYSTEM.CARD32;
BEGIN
  IF f.new THEN
    hFile2 := CreateFile(f.name, FileRegister);
    IF hFile2 = -1 THEN
      w.s("CreateFile '"); w.s(f.name);
      w.s("' failed, last error: $"); w.h(GetLastError()); w.l;
    END;
    ASSERT(hFile2 # -1); f.pos := 0;
    bRes := SetFilePointerEx(f.hFile, 0, 0, FileBegin);
    REPEAT
      bRes := ReadFile(
        f.hFile, SYSTEM.ADR(buf), LEN(buf), SYSTEM.ADR(bytesRead), 0
      );
      IF (bRes # 0) & (bytesRead # 0) THEN
        bRes := WriteFile(
          hFile2, SYSTEM.ADR(buf), bytesRead,
          SYSTEM.ADR(bytesWritten), 0
        );
        INC(f.pos, bytesWritten)
      END
    UNTIL (bytesRead = 0) OR (bRes = 0);
    bRes := CloseHandle(f.hFile); ASSERT(bRes # 0);
    f.hFile := hFile2; f.new := FALSE;
    (* bRes := FlushFileBuffers(hFile2) very slow *)
  END
END Register;

PROCEDURE Close*(f: File);
VAR bRes: INTEGER;
BEGIN
  bRes := FlushFileBuffers(f.hFile);
  ASSERT(bRes # 0)
END Close;

PROCEDURE Purge*(f: File);
VAR bRes: INTEGER; pos: INTEGER;
BEGIN
  bRes := SetFilePointerEx(f.hFile, 0, SYSTEM.ADR(pos), FileBegin);
  ASSERT(bRes # 0); bRes := SetEndOfFile(f.hFile); ASSERT(bRes # 0)
END Purge;

PROCEDURE Delete*(name: ARRAY OF CHAR;  VAR res: INTEGER);
VAR bRes: INTEGER;  name16: PathStr16;  len: INTEGER;
BEGIN
  len := K.Utf8ToUtf16(name, name16);
  bRes := DeleteFileW(SYSTEM.ADR(name16));
  IF bRes # 0 THEN res := 0 ELSE res := -1 END
END Delete;

PROCEDURE Rename*(old, new: ARRAY OF CHAR; VAR res: INTEGER);
VAR bRes: INTEGER;  old16, new16: PathStr16;  len: INTEGER;
BEGIN
  len := K.Utf8ToUtf16(old, old16);
  len := K.Utf8ToUtf16(new, new16);
  bRes := MoveFileExW(SYSTEM.ADR(old16), SYSTEM.ADR(new16), ORD(MoveFileCopyAllowed));
  IF bRes # 0 THEN res := 0 ELSE res := -1 END
END Rename;

PROCEDURE Length*(f: File): INTEGER;
  RETURN f.len
END Length;

PROCEDURE GetDate*(f: File; VAR t, d: INTEGER);
BEGIN ASSERT(FALSE)
END GetDate;

PROCEDURE Set*(VAR r: Rider; f: File; pos: INTEGER);
BEGIN
  IF f = NIL THEN r.f := NIL
  ELSIF pos < 0 THEN r.pos := 0; r.eof := FALSE; r.f := f
  ELSE r.pos := pos; r.eof := FALSE; r.f := f
  END
END Set;

PROCEDURE Pos*(VAR r: Rider): INTEGER;
  RETURN r.pos
END Pos;

PROCEDURE Base*(VAR r: Rider): File;
  RETURN r.f
END Base;

PROCEDURE CheckFilePos(VAR r: Rider);
VAR bRes: INTEGER;
BEGIN
  IF r.pos # r.f.pos THEN
    bRes := SetFilePointerEx(
      r.f.hFile, r.pos, SYSTEM.ADR(r.pos), FileBegin
    );
    ASSERT(bRes # 0); r.f.pos := r.pos
  END
END CheckFilePos;

PROCEDURE ReadMem*(VAR r: Rider; adr, len: INTEGER);
VAR bRes: INTEGER;  bytesRead: SYSTEM.CARD32;  f: File;
BEGIN
  f := r.f; CheckFilePos(r);
  bRes := ReadFile(f.hFile, adr, len, SYSTEM.ADR(bytesRead), 0);
  r.eof := (bRes # 0) & (bytesRead = 0);
  IF ~r.eof THEN INC(r.pos, bytesRead); INC(f.pos, bytesRead) END
END ReadMem;

PROCEDURE Read0(VAR r: Rider; VAR x: ARRAY OF BYTE);
BEGIN ReadMem(r, SYSTEM.ADR(x), LEN(x)) END Read0;

PROCEDURE ReadByte*(VAR r: Rider; VAR x: BYTE);
BEGIN Read0(r, x) END ReadByte;

PROCEDURE Read*(VAR r: Rider; VAR x: CHAR);
BEGIN Read0(r, x) END Read;

PROCEDURE ReadInt*(VAR r: Rider; VAR x: INTEGER);
BEGIN Read0(r, x) END ReadInt;

PROCEDURE ReadReal*(VAR r: Rider; VAR x: REAL);
BEGIN Read0(r, x) END ReadReal;

PROCEDURE ReadCard32*(VAR r: Rider; VAR x: SYSTEM.CARD32);
BEGIN Read0(r, x) END ReadCard32;

PROCEDURE ReadCard16*(VAR r: Rider; VAR x: SYSTEM.CARD16);
BEGIN Read0(r, x) END ReadCard16;

PROCEDURE ReadSet*(VAR r: Rider; VAR x: SET);
BEGIN Read0(r, x) END ReadSet;

PROCEDURE ReadBool*(VAR r: Rider; VAR x: BOOLEAN);
BEGIN Read0(r, x) END ReadBool;

PROCEDURE ReadNum*(VAR r: Rider; VAR x: INTEGER);
VAR s, b: BYTE; n: INTEGER;
BEGIN
  s := 0; n := 0; ReadByte(r, b);
  WHILE b >= 128 DO
    INC(n, LSL(b - 128, s)); INC(s, 7); ReadByte(r, b)
  END;
  x := n + LSL(b MOD 64 - b DIV 64 * 64, s)
END ReadNum;

PROCEDURE ReadString*(VAR r: Rider; VAR str: ARRAY OF CHAR);
VAR i: INTEGER;  done: BOOLEAN;  b: BYTE;
BEGIN
  done := FALSE; i := 0;
  WHILE ~r.eof & ~done DO
    ReadByte(r, b);  str[i] := CHR(b);
    IF r.eof THEN str[i] := 0X ELSE done := str[i] = 0X; INC(i) END
  END
END ReadString;

PROCEDURE ReadBytes*(VAR r: Rider; VAR x: ARRAY OF BYTE; n: INTEGER);
VAR
  startPos, byteCount: INTEGER;
BEGIN
  CheckFilePos(r);  startPos := r.pos;
  IF n > LEN(x) THEN byteCount := LEN(x) ELSE byteCount := n END;
  ReadMem(r, SYSTEM.ADR(x), byteCount);
  r.res := n - (r.pos - startPos)
END ReadBytes;

(* -------------------------------------------------------------------------- *)

PROCEDURE WriteMem(VAR r: Rider; adr, len: INTEGER);
VAR bRes: INTEGER;  bytesWritten: SYSTEM.CARD32;  f: File;
BEGIN
  f := r.f;  CheckFilePos(r);
  bRes := WriteFile(f.hFile, adr, len, SYSTEM.ADR(bytesWritten), 0);
  IF bRes # 0 THEN
    INC(r.pos, bytesWritten);  INC(f.pos, bytesWritten);
    IF f.pos > f.len THEN f.len := f.pos END
  END
END WriteMem;

PROCEDURE Write0(VAR r: Rider; x: ARRAY OF BYTE);
BEGIN WriteMem(r, SYSTEM.ADR(x), LEN(x)) END Write0;

PROCEDURE WriteByte*(VAR r: Rider; x: BYTE);
BEGIN Write0(r, x) END WriteByte;

PROCEDURE Write*(VAR r: Rider; x: CHAR);
BEGIN Write0(r, x) END Write;

PROCEDURE WriteInt*(VAR r: Rider; x: INTEGER);
BEGIN Write0(r, x) END WriteInt;

PROCEDURE WriteReal*(VAR r: Rider; x: REAL);
BEGIN Write0(r, x) END WriteReal;

PROCEDURE WriteCard32*(VAR r: Rider; x: SYSTEM.CARD32);
BEGIN Write0(r, x) END WriteCard32;

PROCEDURE WriteCard16*(VAR r: Rider; x: SYSTEM.CARD16);
BEGIN Write0(r, x) END WriteCard16;

PROCEDURE WriteSet*(VAR r: Rider; x: SET);
BEGIN Write0(r, x) END WriteSet;

PROCEDURE WriteBool*(VAR r: Rider; x: BOOLEAN);
BEGIN Write0(r, x) END WriteBool;

PROCEDURE WriteNum*(VAR r: Rider; x: INTEGER);
BEGIN
  WHILE (x < -64) OR (x >= 64) DO
    WriteByte(r, x MOD 128 + 128); x := x DIV 128
  END;
  WriteByte(r, x MOD 128)
END WriteNum;

PROCEDURE WriteString*(VAR r: Rider; x: ARRAY OF CHAR);
VAR i: INTEGER;
BEGIN i := 0;
  WHILE (i < LEN(x)) & (x[i] # 0X) DO Write(r, x[i]); INC(i) END;
  WriteByte(r, 0)
END WriteString;

PROCEDURE WriteBytes*(VAR r: Rider; x: ARRAY OF BYTE;  offset, length: INTEGER);
VAR startPos: INTEGER;
BEGIN
  CheckFilePos(r);  startPos := r.pos;
  ASSERT(offset + length <= LEN(x));
  WriteMem(r, SYSTEM.ADR(x) + offset, length);
  r.res := length - (r.pos - startPos)
END WriteBytes;


(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE FileAttributes*(name: ARRAY OF CHAR; VAR length, time: INTEGER);
VAR
  name16: ARRAY 1024 OF SYSTEM.CARD16;
  attributes: RECORD
    fileattribytes: SYSTEM.CARD32;
    creation:       INTEGER;
    access:         INTEGER;
    modification:   INTEGER;
    sizehigh:       SYSTEM.CARD32;
    sizelow:        SYSTEM.CARD32
  END;
  res: INTEGER;
BEGIN
  res := K.Utf8ToUtf16(name, name16);
  res := GetFileAttributesExW(SYSTEM.ADR(name16), 0, SYSTEM.ADR(attributes));
  IF res = 0  THEN
    length := -1;
    time := 0
  ELSE
    length := attributes.sizehigh * 100000000H + attributes.sizelow;
    time   := attributes.modification
  END;
END FileAttributes;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)


PROCEDURE InitWin32;
BEGIN
  K.GetProc(K.Kernel, "GetFileAttributesW",      GetFileAttributesW);      ASSERT(GetFileAttributesW      # NIL);
  K.GetProc(K.Kernel, "CreateFileW",             CreateFileW);             ASSERT(CreateFileW             # NIL);
  K.GetProc(K.Kernel, "CloseHandle",             CloseHandle);             ASSERT(CloseHandle             # NIL);
  K.GetProc(K.Kernel, "MoveFileExW",             MoveFileExW);             ASSERT(MoveFileExW             # NIL);
  K.GetProc(K.Kernel, "DeleteFileW",             DeleteFileW);             ASSERT(DeleteFileW             # NIL);
  K.GetProc(K.Kernel, "ReadFile",                ReadFile);                ASSERT(ReadFile                # NIL);
  K.GetProc(K.Kernel, "WriteFile",               WriteFile);               ASSERT(WriteFile               # NIL);
  K.GetProc(K.Kernel, "SetFilePointerEx",        SetFilePointerEx);        ASSERT(SetFilePointerEx        # NIL);
  K.GetProc(K.Kernel, "FlushFileBuffers",        FlushFileBuffers);        ASSERT(FlushFileBuffers        # NIL);
  K.GetProc(K.Kernel, "SetEndOfFile",            SetEndOfFile);            ASSERT(SetEndOfFile            # NIL);
  K.GetProc(K.Kernel, "GetFileSizeEx",           GetFileSizeEx);           ASSERT(GetFileSizeEx           # NIL);
  K.GetProc(K.Kernel, "GetEnvironmentVariableW", GetEnvironmentVariableW); ASSERT(GetEnvironmentVariableW # NIL);
  K.GetProc(K.Kernel, "GetCurrentProcessId",     GetCurrentProcessId);     ASSERT(GetCurrentProcessId     # NIL);
  K.GetProc(K.Kernel, "GetFileAttributesExW",    GetFileAttributesExW);    ASSERT(GetFileAttributesExW    # NIL);
  K.GetProc(K.Kernel, "GetLastError",            GetLastError);            ASSERT(GetLastError    # NIL);
END InitWin32;

BEGIN InitWin32
END Files.
