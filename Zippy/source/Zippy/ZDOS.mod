IMPLEMENTATION MODULE ZDOS  (* 881030 *);

FROM SYSTEM IMPORT
    ADR, ADDRESS;
FROM Strings IMPORT
    ConcatString, CopyString, InsertSubString, StringLength;
FROM Storage IMPORT
    ALLOCATE, DEALLOCATE;
FROM AmigaDOS IMPORT
    FileLock, Lock, UnLock, AccessRead, Open, Close, Read, Write,
    CreateDir, Examine, FileInfoBlockPtr, FileInfoBlock, IoErr, FileHandle,
    ModeNewFile, ModeOldFile, Info, InfoData, InfoDataPtr, SetComment,
    SetProtection;
FROM ZGlobal IMPORT
    ZERR, ZOK, ZNoMem, ZBadDest, ZExists, ZFileOnly;
IMPORT AmigaDOS;


(* Utility routines *)

PROCEDURE TackOn(VAR path : ARRAY OF CHAR; file : ARRAY OF CHAR);
  BEGIN
    IF (path[0] # 0C) AND (path[StringLength(path)-1] # ":") THEN
      ConcatString(path,"/")
    END;
    ConcatString(path,file)
  END TackOn;

PROCEDURE PathName(lock : FileLock; VAR pathname : ARRAY OF CHAR);
  VAR
    duplock : FileLock;

  PROCEDURE FollowPath(lock : FileLock; printslash : BOOLEAN);
    VAR
      myinfo : FileInfoBlockPtr;
      newlock : FileLock;
      error : LONGINT;
    BEGIN
      IF (lock = NIL) THEN  RETURN  END;
      ALLOCATE(myinfo,SIZE(FileInfoBlock));
      IF (myinfo = NIL) THEN
        ConcatString(pathname,"OutOfMem");
        RETURN
      END;
      newlock := AmigaDOS.ParentDir(lock);
      error := IoErr();
      IF (newlock = NIL) AND (error # 0D) THEN
        ConcatString(pathname,"LockError")
      ELSE
        FollowPath(newlock,TRUE);
        IF Examine(lock,myinfo^) THEN
          ConcatString(pathname,myinfo^.fibFileName);
          IF (newlock = NIL) THEN
            ConcatString(pathname,":")
          ELSIF printslash THEN
            ConcatString(pathname,"/")
          END;
          UnLock(lock)
        END
      END;
      DEALLOCATE(myinfo,SIZE(FileInfoBlock))
    END FollowPath;

  BEGIN  (* PathName *)
    pathname[0] := 0C;
    duplock := AmigaDOS.DupLock(lock);
    FollowPath(duplock,FALSE)
  END PathName;

(*$D-*)
PROCEDURE CheckName(name : ARRAY OF CHAR; myfib : FileInfoBlockPtr) : NameStatus;
  VAR
    fl : FileLock;
    fib : ARRAY [0..SIZE(FileInfoBlock)+4] OF CHAR;
    fibp : FileInfoBlockPtr;
    x : BOOLEAN;
  BEGIN
    IF (name[0] <= " ") THEN  RETURN badname  END;
    fl := Lock(ADR(name),AccessRead);
    IF (fl = NIL) THEN  RETURN notfound  END;
    fibp := (ADR(fib) DIV 4D + 1D) * 4D;
    x := Examine(fl,fibp^);
    IF (myfib # NIL) THEN  myfib^ := fibp^  END;
    UnLock(fl);
    IF (fibp^.fibDirEntryType > 0D) THEN
      RETURN directory
    ELSE
      RETURN file
    END
  END CheckName;
(*$D+*)


(* Customized DOS Functions *)

(*$D-*)
PROCEDURE Copy(from,to : ARRAY OF CHAR) : ZERR;
  CONST
    MaxBuffer = 32767D;
    MinBuffer = 512D;
  VAR
    fromfile,tofile : FileHandle;
    buffsize : CARDINAL;
    len : LONGINT;
    buffer : ADDRESS;
    result : ZERR;
    fib : FileInfoBlock;

      PROCEDURE AllocateBuffer() : BOOLEAN;
        BEGIN
          IF (fib.fibSize > MaxBuffer) THEN
            buffsize := MaxBuffer
          ELSIF (fib.fibSize < MinBuffer) THEN
            buffsize := MinBuffer
          ELSE
            buffsize := CARDINAL(fib.fibSize)
          END;
          REPEAT
            ALLOCATE(buffer,buffsize);
            IF (buffer = NIL) THEN  buffsize := buffsize DIV 2  END;
          UNTIL (buffer # NIL) OR (buffsize < CARDINAL(MinBuffer));
          RETURN (buffer # NIL)
        END AllocateBuffer;

  BEGIN
    result := ZOK;
    fromfile := NIL;  tofile := NIL;  buffer := NIL;
    IF (CheckName(to,NIL) = badname) THEN  RETURN ZBadDest  END;
    IF (CheckName(from,ADR(fib)) # file) THEN  RETURN ZFileOnly  END;
    fromfile := Open(ADR(from),ModeOldFile);
    IF (fromfile = NIL) THEN  RETURN IoErr()  END;
    tofile := Open(ADR(to),ModeNewFile);
    IF (tofile = NIL) THEN
      result := IoErr()
    ELSIF NOT AllocateBuffer() THEN
      result := ZNoMem
    ELSE
      REPEAT
        len := Read(fromfile,buffer,LONGCARD(buffsize));
        IF (len > 0D) THEN
          len := Write(tofile,buffer,LONGCARD(len))
        ELSE
          result := IoErr()
        END
      UNTIL (len # LONGINT(buffsize))
    END;
    IF (fromfile # NIL) THEN  Close(fromfile)  END;
    IF (tofile # NIL) THEN  Close(tofile)  END;
    IF (buffer # NIL) THEN  DEALLOCATE(buffer,buffsize)  END;
    IF (result = ZOK) AND NOT SetComment(ADR(to),ADR(fib.fibComment)) THEN
      result := IoErr()
    END;
    IF (result = ZOK) AND NOT SetProtection(ADR(to),fib.fibProtection) THEN
      result := IoErr()
    END;
    RETURN result
  END Copy;
(*$D+*)

(*$D-*)
PROCEDURE NewDir(name : ARRAY OF CHAR) : ZERR;
  VAR
    l : FileLock;
    stat : NameStatus;
  BEGIN
    stat := CheckName(name,NIL);
    IF (stat = badname) THEN  RETURN ZBadDest  END;
    IF (stat # notfound) THEN  RETURN ZExists  END;
    l := CreateDir(ADR(name));
    IF (l = NIL) THEN  RETURN IoErr()  END;
    UnLock(l);
    RETURN ZOK
  END NewDir;
(*$D+*)

(*$D-*)
PROCEDURE DiskAvail(name : ARRAY OF CHAR; VAR avail : LONGINT) : ZERR;
  VAR
    fl : FileLock;
    idp : InfoDataPtr;
    id : ARRAY [0..SIZE(InfoData)+4] OF CHAR;
  BEGIN
    avail := 0D;
    fl := Lock(ADR(name),AccessRead);
    IF (fl = NIL) THEN  RETURN IoErr()  END;
    idp := (ADR(id) DIV 4D + 1D) * 4D;
    IF Info(fl,idp^) THEN
      WITH idp^ DO
        avail := (idNumBlocks - idNumBlocksUsed) * idBytesPerBlock
      END
    END;
    UnLock(fl);
    RETURN ZOK
  END DiskAvail;
(*$D+*)

END ZDOS.
