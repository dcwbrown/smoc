MODULE Files;
IMPORT SYSTEM, Rtl, w := Writer;

CONST
  (* Win32 Const *)
  INVALID_FILE_ATTRIBUTES   = ORD({0..31});
  FILE_ATTRIBUTE_READONLY   = {0};
  FILE_ATTRIBUTE_TEMPORARY  = {8};
  FILE_FLAG_DELETE_ON_CLOSE = {26};

  GENERIC_READ  = {31};
  GENERIC_WRITE = {30};

  OPEN_EXISTING = 3;
  CREATE_ALWAYS = 2;
  CREATE_NEW    = 1;

  MAX_PATH = 260;

  FILE_BEGIN   = 0;
  FILE_CURRENT = 1;
  FILE_END     = 2;

  MOVEFILE_COPY_ALLOWED = {1};

  FILE_SHARE_DELETE = {2};
  FILE_SHARE_READ   = {0};
  FILE_SHARE_WRITE  = {1};

  (* File operations for CreateFile *)
  FileOpenR    = 0;
  FileOpenW    = 1;
  FileNew      = 2;
  FileRegister = 3;


TYPE
  Handle       = INTEGER;
  Pointer      = INTEGER;
  LargeInteger = INTEGER;
  Dword        = SYSTEM.CARD32;
  Bool         = SYSTEM.CARD32;
  Int          = SYSTEM.CARD32;

  PathStr16 = ARRAY MAX_PATH+1 OF SYSTEM.CARD16;
  PathStr   = ARRAY MAX_PATH+1 OF CHAR8;

  File*    = POINTER TO FileDesc;
  FileDesc = RECORD (Rtl.Finalised)
               new, ronly: BOOLEAN;
               hFile:      Handle;
               name, temp: PathStr;
               pos, len:   INTEGER
             END;
  Rider*   = RECORD
               eof*: BOOLEAN;
               res*: INTEGER;
               f:    File;
               pos:  INTEGER
             END;

VAR
  tempId: INTEGER;

  (* Win32 Interface *)
  GetFileAttributesW:      PROCEDURE(lpFileName: ARRAY [untagged] OF SYSTEM.CARD16): Dword;
  DeleteFileW:             PROCEDURE(lpFilename: ARRAY [untagged] OF SYSTEM.CARD16): Bool;
  CloseHandle:             PROCEDURE(hObject: Handle): Bool;
  FlushFileBuffers:        PROCEDURE(hFile: Handle): Bool;
  SetEndOfFile:            PROCEDURE(hFile: Handle): Bool;
  GetFileSizeEx:           PROCEDURE(hFile: Handle; lpFileSize: Pointer): Bool;
  GetCurrentProcessId:     PROCEDURE(): Dword;
  MoveFileExW:             PROCEDURE(lpExistingFileName,
                                     lpNewFileName:         ARRAY [untagged] OF SYSTEM.CARD16;
                                     dwFlags:               Dword): Bool;
  CreateFileW:             PROCEDURE(lpFileName:            ARRAY [untagged] OF SYSTEM.CARD16;
                                     dwDesiredAccess,
                                     dwShareMode:           Dword;
                                     lpSecurityAttributes:  Pointer;
                                     dwCreationDisposition,
                                     dwFlagsAndAttributes:  Dword;
                                     hTemplateFile:         Handle): Handle;
  ReadFile:                PROCEDURE(hFile:                 Handle;
                                     lpBuffer:              Pointer;
                                     nNumberOfBytesToRead:  Dword;
                                     lpNumberOfBytesRead,
                                     lpOverlapped:          Pointer): Bool;
  WriteFile:               PROCEDURE(hFile:                 Handle;
                                     lpBuffer:              Pointer;
                                     nNumberOfBytesToWrite: Dword;
                                     lpNumberOfBytesWrite,
                                     lpOverlapped:          Pointer): Bool;
  SetFilePointerEx:        PROCEDURE(hFile:                 Handle;
                                     liDistanceToMove:      LargeInteger;
                                     lpNewFilePointer:      Pointer;
                                     dwMoveMethod:          Dword): Bool;
  wsprintfW:               PROCEDURE(VAR lpOut:             ARRAY [untagged] OF SYSTEM.CARD16;
                                     lpFmt:                 ARRAY [untagged] OF SYSTEM.CARD16;
                                     par1, par2:            INTEGER): Int;
  GetEnvironmentVariableW: PROCEDURE(lpName:                ARRAY [untagged] OF SYSTEM.CARD16;
                                     lpBuffer:              Pointer;
                                     nSize:                 Dword): Dword;

PROCEDURE Finalise(ptr: Rtl.Finalised);
VAR bRes: Bool; f: File;
BEGIN
  f := ptr(File); bRes := CloseHandle(f.hFile); ASSERT(bRes # 0);
  (*Out.String('Release ');
  IF f.new THEN Out.String(f.temp) ELSE Out.String(f.name) END; Out.Ln*)
END Finalise;

(* Operations: FileOpenR, FileOpenW, FileNew, FileRegister. *)
PROCEDURE CreateFile(name: ARRAY OF CHAR8; op: INTEGER): INTEGER;
VAR hFile, len, access, mode, disposition, flags: INTEGER; name16: PathStr16;
BEGIN
  IF op = FileNew THEN
    disposition := CREATE_NEW;
    flags       := ORD(FILE_ATTRIBUTE_TEMPORARY+FILE_FLAG_DELETE_ON_CLOSE);
  ELSIF op = FileRegister THEN
    disposition := CREATE_ALWAYS;
    flags := 0;
  ELSE
    disposition := OPEN_EXISTING;
    flags       := 0;
  END;
  IF op = FileOpenR THEN
    access      := ORD(GENERIC_READ);
    mode        := ORD(FILE_SHARE_READ);
  ELSE
    access      := ORD(GENERIC_READ+GENERIC_WRITE);
    mode        := ORD(FILE_SHARE_READ+FILE_SHARE_WRITE+FILE_SHARE_DELETE);
  END;
  len := Rtl.Utf8ToUtf16(name, name16);
  hFile := CreateFileW(name16, access, mode, 0, disposition, flags, 0);
RETURN hFile END CreateFile;

PROCEDURE GetFileAttributes(name: ARRAY OF CHAR8): INTEGER;
VAR name16: PathStr16;  len, attr: INTEGER;
BEGIN
  len  := Rtl.Utf8ToUtf16(name, name16);
  attr := GetFileAttributesW(name16);
RETURN attr END GetFileAttributes;

PROCEDURE NewFile(VAR file: File; hFile: Handle);
BEGIN
  NEW(file); file.hFile := hFile;
  Rtl.RegisterFinalised(file, Finalise)
END NewFile;

PROCEDURE Old*(name: ARRAY OF CHAR8): File;
VAR file: File; hFile: Handle;
    attr: Dword; ronly: BOOLEAN; bRes: Bool;
BEGIN
  attr := GetFileAttributes(name);
  IF attr # INVALID_FILE_ATTRIBUTES THEN
    ronly := FILE_ATTRIBUTE_READONLY * SYSTEM.VAL(SET, attr) # {};
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

PROCEDURE SetWString(s: ARRAY OF CHAR8; VAR d: ARRAY OF SYSTEM.CARD16);
VAR len: INTEGER;
BEGIN len := Rtl.Utf8ToUtf16(s, d) END SetWString;

PROCEDURE GetUserProfile(VAR dir: ARRAY OF CHAR8; VAR len: INTEGER);
VAR s, userprofile: PathStr16;
BEGIN
  SetWString(`USERPROFILE`, s);
  len := GetEnvironmentVariableW(s, SYSTEM.ADR(userprofile), LEN(userprofile));
  len := Rtl.Utf16ToUtf8(userprofile, dir);
(*w.s(`Got user profile as '`); w.s(dir); w.sl(`'.`);*)
END GetUserProfile;

PROCEDURE ToHex(h: INTEGER): CHAR8;
VAR  ch: CHAR8;
BEGIN h := h MOD 16;
  IF h < 10 THEN ch := CHR8(h +      ORD(`0`))
  ELSE           ch := CHR8(h - 10 + ORD(`a`)) END
RETURN ch END ToHex;

PROCEDURE Append* (src: ARRAY OF CHAR8;  VAR dst: ARRAY OF CHAR8);
VAR i, j: INTEGER;
BEGIN
  i := 0;  WHILE dst[i] # 0Y DO INC(i) END;  j := 0;
  WHILE src[j] # 0Y DO  dst[i] := src[j];  INC(i);  INC(j)  END;
  dst[i] := 0Y
END Append;

PROCEDURE AppendHex(h: INTEGER; VAR dst: ARRAY OF CHAR8);
VAR hex: ARRAY 20 OF CHAR8; i, j: INTEGER;
BEGIN
  i := LEN(hex)-1;
  WHILE (i >= 0) & (h # 0) DO
    hex[i] := ToHex(h);  DEC(i);  h := h DIV 16
  END;
  IF i = 19 THEN hex[i] := `0`; DEC(i) END;
  INC(i);
  j := 0;  WHILE dst[j] # 0Y DO INC(j) END;
  WHILE i < LEN(hex) DO  dst[j] := hex[i];  INC(i);  INC(j)  END;
  dst[j] := 0Y
END AppendHex;

PROCEDURE MakeTempName(VAR name: ARRAY OF CHAR8);
VAR
pid, time, len: INTEGER;
BEGIN
  pid  := GetCurrentProcessId();
  time := Rtl.Time();
  GetUserProfile(name, len);
  IF len < MAX_PATH-60 THEN
    Append(`\.smoctmp`, name); AppendHex(pid, name);
    Append(`-`, name);         AppendHex(time, name);
    Append(`-`, name);         AppendHex(tempId, name);
  ELSE
    name := `.tmp`;    AppendHex(time, name);
    Append(`-`, name); AppendHex(tempId, name);
  END;
  INC(tempId);
END MakeTempName;

PROCEDURE New8*(name: ARRAY OF CHAR8): File;
VAR
  hFile: Handle;
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
END New8;


PROCEDURE Register*(f: File);
VAR hFile2: Handle; buf: ARRAY 10000H OF BYTE;
    bRes: Bool; byteRead, byteWritten: Dword;
BEGIN
  IF f.new THEN
    hFile2 := CreateFile(f.name, FileRegister);
    ASSERT(hFile2 # -1); f.pos := 0;
    bRes := SetFilePointerEx(f.hFile, 0, 0, FILE_BEGIN);
    REPEAT
      bRes := ReadFile(
        f.hFile, SYSTEM.ADR(buf), LEN(buf), SYSTEM.ADR(byteRead), 0
      );
      IF (bRes # 0) & (byteRead # 0) THEN
        bRes := WriteFile(
          hFile2, SYSTEM.ADR(buf), byteRead,
          SYSTEM.ADR(byteWritten), 0
        );
        INC(f.pos, byteWritten)
      END
    UNTIL (byteRead = 0) OR (bRes = 0);
    bRes := CloseHandle(f.hFile); ASSERT(bRes # 0);
    f.hFile := hFile2; f.new := FALSE;
    (* bRes := FlushFileBuffers(hFile2) very slow *)
  END
END Register;

PROCEDURE Close*(f: File);
VAR bRes: Bool;
BEGIN
  bRes := FlushFileBuffers(f.hFile);
  ASSERT(bRes # 0)
END Close;

PROCEDURE Purge*(f: File);
VAR bRes: Bool; pos: INTEGER;
BEGIN
  bRes := SetFilePointerEx(f.hFile, 0, SYSTEM.ADR(pos), FILE_BEGIN);
  ASSERT(bRes # 0); bRes := SetEndOfFile(f.hFile); ASSERT(bRes # 0)
END Purge;

PROCEDURE Delete*(name: ARRAY OF SYSTEM.CARD16; VAR res: INTEGER);
VAR bRes: Bool;
BEGIN
  bRes := DeleteFileW(name);
  IF bRes # 0 THEN res := 0 ELSE res := -1 END
END Delete;

PROCEDURE Delete8*(name: ARRAY OF CHAR8; VAR res: INTEGER);
VAR bRes: Bool;  name16: PathStr16;  len: INTEGER;
BEGIN
  len := Rtl.Utf8ToUtf16(name, name16);
  bRes := DeleteFileW(name16);
  IF bRes # 0 THEN res := 0 ELSE res := -1 END
END Delete8;

PROCEDURE Rename*(old, new: ARRAY OF SYSTEM.CARD16; VAR res: INTEGER);
VAR bRes: Bool;
BEGIN
  bRes := MoveFileExW(old, new, ORD(MOVEFILE_COPY_ALLOWED));
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
VAR bRes: Bool;
BEGIN
  IF r.pos # r.f.pos THEN
    bRes := SetFilePointerEx(
      r.f.hFile, r.pos, SYSTEM.ADR(r.pos), FILE_BEGIN
    );
    ASSERT(bRes # 0); r.f.pos := r.pos
  END
END CheckFilePos;

PROCEDURE Read0(VAR r: Rider; VAR x: ARRAY OF BYTE);
VAR bRes: Bool; byteRead: Dword; f: File;
BEGIN
  f := r.f; CheckFilePos(r);
  bRes := ReadFile(f.hFile, SYSTEM.ADR(x), LEN(x), SYSTEM.ADR(byteRead), 0);
  r.eof := (bRes # 0) & (byteRead = 0);
  IF ~r.eof THEN INC(r.pos, byteRead); INC(f.pos, byteRead) END
END Read0;

PROCEDURE Read*(VAR r: Rider; VAR x: BYTE);
BEGIN Read0(r, x)
END Read;

PROCEDURE ReadInt*(VAR r: Rider; VAR x: INTEGER);
BEGIN Read0(r, x)
END ReadInt;

PROCEDURE ReadReal*(VAR r: Rider; VAR x: REAL);
VAR bRes: Bool; byteRead: Dword; f: File;
BEGIN Read0(r, x)
END ReadReal;

PROCEDURE ReadCard32*(VAR r: Rider; VAR x: SYSTEM.CARD32);
VAR bRes: Bool; byteRead: Dword; f: File;
BEGIN Read0(r, x)
END ReadCard32;

PROCEDURE ReadCard16*(VAR r: Rider; VAR x: SYSTEM.CARD16);
VAR bRes: Bool; byteRead: Dword; f: File;
BEGIN Read0(r, x)
END ReadCard16;

PROCEDURE ReadNum*(VAR r: Rider; VAR x: INTEGER);
VAR s, b: BYTE; n: INTEGER;
BEGIN
  s := 0; n := 0; Read(r, b);
  WHILE b >= 128 DO
    INC(n, LSL(b - 128, s)); INC(s, 7); Read(r, b)
  END;
  x := n + LSL(b MOD 64 - b DIV 64 * 64, s)
END ReadNum;

PROCEDURE ReadChar16*(VAR r: Rider; VAR x: SYSTEM.CARD16);
VAR bRes: Bool; byteRead: Dword; f: File;
BEGIN Read0(r, x)
END ReadChar16;

PROCEDURE ReadString8*(VAR r: Rider; VAR x: ARRAY OF CHAR8);
VAR i: INTEGER;  done: BOOLEAN;  b: BYTE;
BEGIN
  done := FALSE; i := 0;
  WHILE ~r.eof & ~done DO
    Read(r, b);  x[i] := CHR8(b);
    IF r.eof THEN x[i] := 0Y ELSE done := x[i] = 0Y; INC(i) END
  END
END ReadString8;

PROCEDURE ReadString16*(VAR r: Rider; VAR x: ARRAY OF SYSTEM.CARD16);
VAR i: INTEGER; done: BOOLEAN;
BEGIN
  done := FALSE; i := 0;
  WHILE ~r.eof & ~done DO
    ReadChar16(r, x[i]);
    IF r.eof THEN x[i] := 0 ELSE done := x[i] = 0; INC(i) END
  END
END ReadString16;

PROCEDURE ReadSet*(VAR r: Rider; VAR x: SET);
VAR bRes: Bool; byteRead: Dword; f: File;
BEGIN Read0(r, x)
END ReadSet;

PROCEDURE ReadBool*(VAR r: Rider; VAR x: BOOLEAN);
VAR bRes: Bool; byteRead: Dword; f: File;
BEGIN Read0(r, x)
END ReadBool;

PROCEDURE ReadBytes*(VAR r: Rider; VAR x: ARRAY OF BYTE; n: INTEGER);
VAR bRes: Bool; byteRead: Dword; f: File;
BEGIN
  f := r.f; CheckFilePos(r);
  IF n > LEN(x) THEN byteRead := LEN(x) ELSE byteRead := n END;
  bRes := ReadFile(
    f.hFile, SYSTEM.ADR(x), byteRead, SYSTEM.ADR(byteRead), 0
  );
  r.eof := (bRes # 0) & (byteRead = 0);
  IF ~r.eof THEN INC(r.pos, byteRead); INC(f.pos, byteRead) END;
  IF byteRead # n THEN r.res := n - byteRead ELSE r.res := 0 END
END ReadBytes;

(* -------------------------------------------------------------------------- *)

PROCEDURE Write0(VAR r: Rider; x: ARRAY OF BYTE);
VAR bRes: Bool; nWritten: Dword; f: File;
BEGIN
  f := r.f; CheckFilePos(r);
  bRes := WriteFile(f.hFile, SYSTEM.ADR(x), LEN(x), SYSTEM.ADR(nWritten), 0);
  IF bRes # 0 THEN
    INC(r.pos, nWritten); INC(f.pos, nWritten);
    IF f.pos > f.len THEN f.len := f.pos END
  END
END Write0;

PROCEDURE Write*(VAR r: Rider; x: BYTE);
BEGIN Write0(r, x)
END Write;

PROCEDURE WriteInt*(VAR r: Rider; x: INTEGER);
BEGIN Write0(r, x)
END WriteInt;

PROCEDURE WriteReal*(VAR r: Rider; x: REAL);
BEGIN Write0(r, x)
END WriteReal;

PROCEDURE WriteCard32*(VAR r: Rider; x: SYSTEM.CARD32);
BEGIN Write0(r, x)
END WriteCard32;

PROCEDURE WriteCard16*(VAR r: Rider; x: SYSTEM.CARD16);
BEGIN Write0(r, x)
END WriteCard16;

PROCEDURE WriteNum*(VAR r: Rider; x: INTEGER);
BEGIN
  WHILE (x < -64) OR (x >= 64) DO
    Write(r, x MOD 128 + 128); x := x DIV 128
  END;
  Write(r, x MOD 128)
END WriteNum;

PROCEDURE WriteChar16*(VAR r: Rider; x: SYSTEM.CARD16);
BEGIN Write0(r, x)
END WriteChar16;

PROCEDURE WriteString8*(VAR r: Rider; x: ARRAY OF BYTE);
VAR i: INTEGER;
BEGIN i := 0;
  WHILE (i < LEN(x)) & (x[i] # 0) DO Write(r, x[i]); INC(i) END;
  Write(r, 0)
END WriteString8;

PROCEDURE WriteString16*(VAR r: Rider; x: ARRAY OF SYSTEM.CARD16);
VAR i: INTEGER;
BEGIN i := 0;
  WHILE (i < LEN(x)) & (x[i] # 0) DO WriteChar16(r, x[i]); INC(i) END;
  WriteChar16(r, 0)
END WriteString16;

PROCEDURE WriteSet*(VAR r: Rider; x: SET);
BEGIN Write0(r, x)
END WriteSet;

PROCEDURE WriteBool*(VAR r: Rider; x: BOOLEAN);
BEGIN Write0(r, x)
END WriteBool;

PROCEDURE WriteBytes*(VAR r: Rider; x: ARRAY OF BYTE; n: INTEGER);
VAR bRes: Bool; byteWritten: Dword; f: File;
BEGIN
  f := r.f; CheckFilePos(r);
  IF n > LEN(x) THEN byteWritten := LEN(x) ELSE byteWritten := n END;
  bRes := WriteFile(
    f.hFile, SYSTEM.ADR(x), byteWritten, SYSTEM.ADR(byteWritten), 0
  );
  IF bRes # 0 THEN
    INC(r.pos, byteWritten); INC(f.pos, byteWritten);
    IF f.pos > f.len THEN f.len := f.pos END
  END;
  r.res := n - byteWritten
END WriteBytes;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE InitWin32;
BEGIN
  SYSTEM.GetProcAddress(GetFileAttributesW,      Rtl.HKernel, SYSTEM.ADR(`GetFileAttributesW`));      ASSERT(GetFileAttributesW      # NIL);
  SYSTEM.GetProcAddress(CreateFileW,             Rtl.HKernel, SYSTEM.ADR(`CreateFileW`));             ASSERT(CreateFileW             # NIL);
  SYSTEM.GetProcAddress(CloseHandle,             Rtl.HKernel, SYSTEM.ADR(`CloseHandle`));             ASSERT(CloseHandle             # NIL);
  SYSTEM.GetProcAddress(MoveFileExW,             Rtl.HKernel, SYSTEM.ADR(`MoveFileExW`));             ASSERT(MoveFileExW             # NIL);
  SYSTEM.GetProcAddress(DeleteFileW,             Rtl.HKernel, SYSTEM.ADR(`DeleteFileW`));             ASSERT(DeleteFileW             # NIL);
  SYSTEM.GetProcAddress(ReadFile,                Rtl.HKernel, SYSTEM.ADR(`ReadFile`));                ASSERT(ReadFile                # NIL);
  SYSTEM.GetProcAddress(WriteFile,               Rtl.HKernel, SYSTEM.ADR(`WriteFile`));               ASSERT(WriteFile               # NIL);
  SYSTEM.GetProcAddress(SetFilePointerEx,        Rtl.HKernel, SYSTEM.ADR(`SetFilePointerEx`));        ASSERT(SetFilePointerEx        # NIL);
  SYSTEM.GetProcAddress(FlushFileBuffers,        Rtl.HKernel, SYSTEM.ADR(`FlushFileBuffers`));        ASSERT(FlushFileBuffers        # NIL);
  SYSTEM.GetProcAddress(SetEndOfFile,            Rtl.HKernel, SYSTEM.ADR(`SetEndOfFile`));            ASSERT(SetEndOfFile            # NIL);
  SYSTEM.GetProcAddress(GetFileSizeEx,           Rtl.HKernel, SYSTEM.ADR(`GetFileSizeEx`));           ASSERT(GetFileSizeEx           # NIL);
  SYSTEM.GetProcAddress(wsprintfW,               Rtl.HUser,   SYSTEM.ADR(`wsprintfW`));               ASSERT(wsprintfW               # NIL);
  SYSTEM.GetProcAddress(GetEnvironmentVariableW, Rtl.HKernel, SYSTEM.ADR(`GetEnvironmentVariableW`)); ASSERT(GetEnvironmentVariableW # NIL);
  SYSTEM.GetProcAddress(GetCurrentProcessId,     Rtl.HKernel, SYSTEM.ADR(`GetCurrentProcessId`));     ASSERT(GetCurrentProcessId     # NIL);
END InitWin32;

BEGIN InitWin32
END Files.