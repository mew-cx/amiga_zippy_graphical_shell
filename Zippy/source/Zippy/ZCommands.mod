IMPLEMENTATION MODULE ZCommands;  (* MEW 881103 *)

FROM SYSTEM IMPORT
    ADR;
FROM Intuition IMPORT
    GadgetPtr, StringInfoPtr, RemoveGadget, AddGadget, RefreshGList;
FROM AmigaDOS IMPORT
    FileLock, Lock, UnLock, AccessRead, CurrentDir, IoErr, FibDelete;
FROM Ports IMPORT
    GetMsg;
FROM Strings IMPORT
    CopyString, StringLength, ConcatString, LocateChar, InsertSubString;
IMPORT AmigaDOS;

FROM ZGlobal IMPORT
    ZERR, ZOK, ZBadDest, ZAbort, ZBadSource, ZExists, ZDelProt, Str255,
    NumToStr, SourceLock, OrigLock;
FROM ZTable IMPORT
    DirInfoPtr, DisplayTable, NextEntry, SaveSelections, Select, SelectCode,
    BytesSelected, NumFilesSelected, LoadTableWithDevices, EmptyTable,
    LoadTableFromDirectory;
FROM ZWindowUtil IMPORT
    ZWindowPtr, RefreshStringGad, DisplayMessage, DisplayZErr;
FROM BusyPointer IMPORT
    SpinBusyPointer;
FROM ZWindow IMPORT
    SourceGad, DestGad;
FROM ZMenu IMPORT
    SetupLocalMenus;
FROM ZDOS IMPORT
    NameStatus;
IMPORT ZDOS;


(* Internal routines *)

PROCEDURE Interrupted() : BOOLEAN;
  BEGIN
    RETURN (GetMsg(ZWindowPtr^.UserPort^) # NIL)
  END Interrupted;

PROCEDURE EnsureDestDir() : BOOLEAN;
  VAR
    result : ZERR;
    deststat : NameStatus;
    temp : Str255;
  BEGIN
    result := ZOK;
    GetPath(DestGad,temp);
    deststat := ZDOS.CheckName(temp,NIL);
    IF (deststat = badname) OR (deststat = file) THEN
      result := ZBadDest
    ELSIF (deststat = notfound) THEN
      result := ZDOS.NewDir(temp)
    END;
    DisplayZErr(result);
    RETURN (result = ZOK)
  END EnsureDestDir;


(* External Routines *)

(* Table *)

PROCEDURE DisplayDeviceTable;
  VAR result : ZERR;
  BEGIN
    SavePathToUndo(SourceGad);
    SetPath(SourceGad,"");
    result := LoadTableWithDevices();
    DisplayZErr(result);
    DisplayTable(TRUE)
  END DisplayDeviceTable;

PROCEDURE DisplayDirectoryTable;
  VAR
    result : ZERR;
    files,dirs : CARDINAL;
    str1,str2 : ARRAY [0..40] OF CHAR;
    temp : Str255;
  BEGIN
    GetPath(SourceGad,temp);
    IF (ZDOS.CheckName(temp,NIL) = badname) AND (SourceLock # NIL) THEN
      ZDOS.PathName(SourceLock,temp);
      SetPath(SourceGad,temp)
    END;
    result := LoadTableFromDirectory(SourceLock,files,dirs);
    IF (result = ZOK) THEN
      NumToStr(str1,LONGCARD(files));  NumToStr(str2,LONGCARD(dirs));
      DisplayMessage(str1," files  ",str2," dirs")
    ELSE
      DisplayZErr(result)
    END;
    DisplayTable(TRUE)
  END DisplayDirectoryTable;

PROCEDURE SetCurrentDirectory;
  VAR
    oldlock,newlock : FileLock;
    temp : Str255;
  BEGIN
    GetPath(SourceGad,temp);
    newlock := Lock(ADR(temp),AccessRead);
    IF (newlock = NIL) THEN
      DisplayZErr(ZBadSource);
      EmptyTable;
      DisplayTable(TRUE)
    ELSE
      oldlock := CurrentDir(newlock);
      IF (OrigLock = NIL) THEN
        OrigLock := oldlock
      ELSE
        UnLock(oldlock)
      END;
      SourceLock := newlock;
      SetupLocalMenus;
      DisplayDirectoryTable
    END
  END SetCurrentDirectory;


(* Path *)

PROCEDURE SavePathToUndo(g : GadgetPtr);
  VAR
    sip : StringInfoPtr;
    curr,undo : POINTER TO Str255;
  BEGIN
    sip := g^.SpecialInfo;
    curr := sip^.Buffer;  undo := sip^.UndoBuffer;
    IF (curr^[0] # 0C) AND (undo # NIL) THEN
      undo^ := curr^
    END
  END SavePathToUndo;

(*$D-*)
PROCEDURE SetPath(g : GadgetPtr; str : ARRAY OF CHAR);
  VAR
    d : INTEGER;
    sip : StringInfoPtr;
    s : POINTER TO Str255;
  BEGIN
    d := RemoveGadget(ZWindowPtr^,g^);
    sip := g^.SpecialInfo;  s := sip^.Buffer;
    CopyString(s^,str);
    d := AddGadget(ZWindowPtr^,g^,d);
    RefreshStringGad(g)
  END SetPath;
(*$D+*)

PROCEDURE GetPath(g : GadgetPtr; VAR str : ARRAY OF CHAR);
  VAR
    sip : StringInfoPtr;
    s : POINTER TO Str255;
  BEGIN
    sip := g^.SpecialInfo;  s := sip^.Buffer;
    CopyString(str,s^);
  END GetPath;

PROCEDURE SwapPaths;
  VAR
    d1,d2 : INTEGER;
    sip : StringInfoPtr;
  BEGIN
    d1 := RemoveGadget(ZWindowPtr^,SourceGad^);
    d2 := RemoveGadget(ZWindowPtr^,DestGad^);
    sip := SourceGad^.SpecialInfo;
    SourceGad^.SpecialInfo := DestGad^.SpecialInfo;
    DestGad^.SpecialInfo := sip;
    d2 := AddGadget(ZWindowPtr^,DestGad^,d2);
    d1 := AddGadget(ZWindowPtr^,SourceGad^,d1);
    RefreshGList(SourceGad^,ZWindowPtr^,NIL,2);
    SetCurrentDirectory
  END SwapPaths;

PROCEDURE UndoPath(g : GadgetPtr);
  VAR
    d : INTEGER;
    sip : StringInfoPtr;
    temp : POINTER TO Str255;
  BEGIN
    d := RemoveGadget(ZWindowPtr^,g^);
    sip := g^.SpecialInfo;
    temp := sip^.Buffer;
    sip^.Buffer := sip^.UndoBuffer;
    sip^.UndoBuffer := temp;
    d := AddGadget(ZWindowPtr^,g^,d);
    RefreshStringGad(g);
    IF (g = SourceGad) THEN  SetCurrentDirectory  END;
  END UndoPath;

PROCEDURE CopyPath(from,to : GadgetPtr);
  VAR temp : Str255;
  BEGIN
    SavePathToUndo(to);
    GetPath(from,temp);
    SetPath(to,temp);
    IF (to = SourceGad) THEN  SetCurrentDirectory  END;
  END CopyPath;

PROCEDURE ParentPath(g : GadgetPtr);
  VAR
    oldx, x : CARDINAL;
    temp : Str255;
  BEGIN
    GetPath(g,temp);
    x := StringLength(temp);
    oldx := x;
    IF (x > 0) THEN
      DEC(x);
      WHILE (x > 0) AND (temp[x] # "/") AND (temp[x] # ":") DO  DEC(x)  END;
      IF (temp[x] = ":") THEN  INC(x)  END;
      temp[x] := 0C;
      IF (x # oldx) THEN
        SetPath(g,temp);
        IF (g = SourceGad) THEN  SetCurrentDirectory  END
      END
    END
  END ParentPath;

PROCEDURE RootPath(g : GadgetPtr);
  VAR
    oldx, x : CARDINAL;
    temp : Str255;
  BEGIN
    GetPath(g,temp);
    x := StringLength(temp);
    oldx := x;
    IF (x > 0) THEN
      DEC(x);
      WHILE (x > 0) AND (temp[x] # ":") DO  DEC(x)  END;
      IF (temp[x] = ":") THEN  INC(x)  END;
      temp[x] := 0C;
      IF (x = 0) THEN  temp := ":"  END;
      IF (x # oldx) THEN
        SavePathToUndo(g);
        SetPath(g,temp);
        IF (g = SourceGad) THEN  SetCurrentDirectory  END
      END
    END
  END RootPath;


(* Selection *)

PROCEDURE SelectAllFiles;
  VAR d : DirInfoPtr;
  BEGIN
    SaveSelections;
    d := NextEntry(NIL);
    WHILE (d # NIL) DO
      IF (NOT d^.diIsDir) THEN  Select(d,On)  END;
      d := NextEntry(d)
    END;
    DisplayZErr(ZOK);
    DisplayTable(TRUE)
  END SelectAllFiles;

PROCEDURE UnSelectAll;
  VAR d : DirInfoPtr;
  BEGIN
    SaveSelections;
    d := NextEntry(NIL);
    WHILE (d # NIL) DO
      Select(d,Off);
      d := NextEntry(d)
    END;
    DisplayZErr(ZOK);
    DisplayTable(TRUE)
  END UnSelectAll;

PROCEDURE RestoreSelect;
  VAR d : DirInfoPtr;
  BEGIN
    d := NextEntry(NIL);
    WHILE (d # NIL) DO
      Select(d,Restore);
      d := NextEntry(d)
    END;
    DisplayZErr(ZOK);
    DisplayTable(TRUE)
  END RestoreSelect;

PROCEDURE ReverseSelect;
  VAR d : DirInfoPtr;
  BEGIN
    d := NextEntry(NIL);
    WHILE (d # NIL) DO
      Select(d,Toggle);
      d := NextEntry(d)
    END;
    DisplayZErr(ZOK);
    DisplayTable(TRUE)
  END ReverseSelect;

PROCEDURE SelectStats;
  VAR str1,str2 : ARRAY [0..40] OF CHAR;
  BEGIN
    NumToStr(str1,LONGCARD(BytesSelected));
    NumToStr(str2,LONGCARD(NumFilesSelected));
    DisplayMessage(str1," bytes in ",str2," files")
  END SelectStats;


(* DOS *)

PROCEDURE Move;
  VAR
    temp : Str255;
    d : DirInfoPtr;
    result : ZERR;
  BEGIN
    IF NOT EnsureDestDir() THEN  RETURN  END;
    SaveSelections;
    result := ZOK;
    d := NextEntry(NIL);
    WHILE (d # NIL) AND (result = ZOK) DO
      IF d^.diSelected THEN
        SpinBusyPointer(ZWindowPtr,TRUE);
        GetPath(DestGad,temp);
        ZDOS.TackOn(temp,d^.diName);
        DisplayMessage("Move ",d^.diName,"","");
        IF Interrupted() THEN
          result := ZAbort
        ELSIF (ZDOS.CheckName(temp,NIL) # notfound) THEN
          result := ZExists
        ELSIF NOT AmigaDOS.Rename(ADR(d^.diName),ADR(temp)) THEN
          result := IoErr()
        END;
        IF (result = ZOK) THEN  Select(d,Zap)  END
      END;
      d := NextEntry(d)
    END;
    DisplayZErr(result);
    DisplayTable(TRUE)
  END Move;

PROCEDURE Copy;
  VAR
    temp : Str255;
    d : DirInfoPtr;
    result : ZERR;
  BEGIN
    IF NOT EnsureDestDir() THEN  RETURN  END;
    SaveSelections;
    result := ZOK;
    d := NextEntry(NIL);
    WHILE (d # NIL) AND (result = ZOK) DO
      IF d^.diSelected AND NOT d^.diIsDir THEN
        SpinBusyPointer(ZWindowPtr,TRUE);
        GetPath(DestGad,temp);
        ZDOS.TackOn(temp,d^.diName);
        DisplayMessage("Copy ",d^.diName,"","");
        IF Interrupted() THEN
          result := ZAbort
        ELSE
          result := ZDOS.Copy(d^.diName,temp)
        END;
        IF (result = ZOK) THEN  Select(d,Off)  END
      END;
      d := NextEntry(d)
    END;
    DisplayZErr(result);
    DisplayTable(TRUE)
  END Copy;

PROCEDURE Delete;
  VAR
    d : DirInfoPtr;
    result : ZERR;
  BEGIN
    SaveSelections;
    result := ZOK;
    d := NextEntry(NIL);
    WHILE (d # NIL) AND (result = ZOK) DO
      IF d^.diSelected THEN
        SpinBusyPointer(ZWindowPtr,TRUE);
        DisplayMessage("Delete ",d^.diName,"","");
        IF Interrupted() THEN
          result := ZAbort
        ELSIF (FibDelete IN d^.diProt) THEN
          result := ZDelProt
        ELSIF NOT AmigaDOS.DeleteFile(ADR(d^.diName)) THEN
          result := IoErr()
        END;
        IF (result = ZOK) THEN  Select(d,Zap)  END
      END;
      d := NextEntry(d)
    END;
    DisplayZErr(result);
    DisplayTable(TRUE)
  END Delete;

(*$D-*)
PROCEDURE Launch(pgm : ARRAY OF CHAR; reward : SelectCode);
  CONST quote = 42C;
  VAR
    d : DirInfoPtr;
    result : ZERR;
    temp : Str255;
  BEGIN
    SaveSelections;
    result := ZOK;
    d := NextEntry(NIL);
    WHILE (d # NIL) AND (result = ZOK) DO
      IF d^.diSelected THEN
        SpinBusyPointer(ZWindowPtr,TRUE);
        CopyString(temp,d^.diName);
        IF (LocateChar(temp," ",0,StringLength(temp)) >= 0) THEN
          InsertSubString(temp,quote,0);
          ConcatString(temp,quote)
        END;
        InsertSubString(temp," ",0);
        InsertSubString(temp,pgm,0);
        DisplayMessage(temp,"","","");
        IF Interrupted() THEN
          result := ZAbort
        ELSIF NOT AmigaDOS.Execute(ADR(temp),NIL,NIL) THEN
          result := IoErr()
        END;
        IF (result = ZOK) AND (reward # Reread) THEN  Select(d,reward)  END
      END;
      d := NextEntry(d)
    END;
    DisplayZErr(result);
    IF (reward = Reread) THEN
      DisplayDirectoryTable
    ELSE
      DisplayTable(TRUE)
    END
  END Launch;
(*$D+*)


(* Misc *)

PROCEDURE NewDir;
  BEGIN
    IF EnsureDestDir() THEN
      DisplayDirectoryTable
    END
  END NewDir;

PROCEDURE DeviceInfo(g : GadgetPtr);
  VAR
    result : ZERR;
    bytes : LONGINT;
    temp : Str255;
  BEGIN
    GetPath(g,temp);
    result := ZDOS.DiskAvail(temp,bytes);
    IF (result = ZOK) THEN
      NumToStr(temp,LONGCARD(bytes));
      DisplayMessage(temp," bytes available","","")
    ELSE
      DisplayZErr(result)
    END
  END DeviceInfo;

END ZCommands.
