MODULE FileInfo;  (* v1.3  MEW  881027 *)

(* Written in Benchmark Modula-2 by Michael Weiblen *)
(*     PLink: EKIM     CI$ (rarely): 72506,2072     *)

(* An Intuitionalized combination of Rename, FileNote & Protect *)


FROM Intuition IMPORT
    GadgetPtr, WindowPtr, Selected, IntuiMessagePtr, OpenWindow, CloseWindow,
    FreeRemember, ActivateGadget;
FROM Strings IMPORT
    CopyString, CompareString, equal;
FROM SYSTEM IMPORT
    ADR;
FROM System IMPORT
    argc, argv;
FROM Ports IMPORT
    ReplyMsg, GetMsg, WaitPort;
FROM AmigaDOS IMPORT
    Lock, UnLock, SharedLock, FileLock, Examine, Rename, SetComment,
    SetProtection, Info, FileInfoBlock, FileInfoBlockPtr, ProtectionSet;
FROM FileInfoWindow IMPORT
    NewWindowStructure1, NameGad, NoteGad, ProtGad, SaveGad, QuitGad,
    FileInfoWindow, FileInfoWindowRememberPtr, NameGadSIBuff, NoteGadSIBuff;
FROM Terminal IMPORT
    WriteString;


CONST
  del=0; exe=1; wrt=2; rea=3; arc=4; pur=5; scr=6; hid=7;  (* Protection bits *)

VAR
  FIWindowPtr : WindowPtr;
  TheLock : FileLock;
  FIB : ARRAY [0..SIZE(FileInfoBlock)+4] OF CHAR;
  FIBPtr : FileInfoBlockPtr;
  WindowTitle : ARRAY [0..25] OF CHAR;
  x : BOOLEAN;

PROCEDURE Setup;
  BEGIN
    FIBPtr := (ADR(FIB) DIV 4D + 1D) * 4D;
    FIWindowPtr := NIL;
    FileInfoWindowRememberPtr := NIL;
    TheLock := NIL
  END Setup;

PROCEDURE Shutdown;
  BEGIN
    IF (TheLock # NIL) THEN  UnLock(TheLock)  END;
    IF (FIWindowPtr # NIL) THEN  CloseWindow(FIWindowPtr^)  END;
    FreeRemember(FileInfoWindowRememberPtr,TRUE);
    HALT
  END Shutdown;

PROCEDURE SetupTheWindow;
  VAR
    i : CARDINAL;
    a,b : BOOLEAN;
  BEGIN
    WITH FIBPtr^ DO
      CopyString(NameGadSIBuff,fibFileName);
      CopyString(NoteGadSIBuff,fibComment);
      FOR i := del TO hid DO
        a := (i > rea);
	b := (i IN fibProtection);
        IF (a AND b) OR (NOT(a) AND NOT(b)) THEN
          INCL(ProtGad[i]^.Flags,Selected)
        END
      END;
      IF (fibDirEntryType < 0D) THEN
        WindowTitle := "File Information"
      ELSE
        WindowTitle := "Directory Information"
      END;
      NewWindowStructure1^.Title := ADR(WindowTitle)
    END
  END SetupTheWindow;

PROCEDURE UserSaysSave() : BOOLEAN;
  VAR
    msgptr : IntuiMessagePtr;
    gptr : GadgetPtr;
  BEGIN
    LOOP
      msgptr := WaitPort(FIWindowPtr^.UserPort^);
      msgptr := GetMsg(FIWindowPtr^.UserPort^);
      gptr := msgptr^.IAddress;
      ReplyMsg(msgptr);
      IF (gptr = SaveGad) OR (gptr = NoteGad) THEN
        RETURN TRUE
      ELSIF (gptr = QuitGad) THEN
        RETURN FALSE
      ELSIF (gptr = NameGad) THEN
        x := ActivateGadget(NoteGad^,FIWindowPtr^,NIL)
      END
    END
  END UserSaysSave;

PROCEDURE SaveTheChanges;
  VAR
    prot : ProtectionSet;
    i : CARDINAL;
    a,b : BOOLEAN;
  BEGIN
    WITH FIBPtr^ DO
      prot := ProtectionSet{};
      FOR i := del TO hid DO
        a := (i > rea);
	b := (Selected IN ProtGad[i]^.Flags);
	IF (a AND b) OR (NOT(a) AND NOT(b)) THEN
          INCL(prot,i)
        END
      END;
      IF (prot # fibProtection) THEN
        IF NOT SetProtection(argv^[1],prot) THEN
          WriteString("FileInfo: can't set protection\n")
        END
      END;
      IF (CompareString(fibComment,NoteGadSIBuff) # equal) THEN
        IF NOT SetComment(argv^[1],ADR(NoteGadSIBuff)) THEN
          WriteString("FileInfo: can't set comment\n")
        END
      END;
      IF (CompareString(fibFileName,NameGadSIBuff) # equal) THEN
        IF NOT Rename(argv^[1],ADR(NameGadSIBuff)) THEN
          WriteString("FileInfo: can't rename\n")
        END
      END
    END
  END SaveTheChanges;

BEGIN    (* MAIN *)
  Setup;
  IF (argc # 2) OR (argv^[1]^[0] = "?") THEN
    WriteString("usage: FileInfo name\n");
    Shutdown
  END;

  TheLock := Lock(argv^[1],SharedLock);
  IF (TheLock = NIL) THEN
    WriteString("FileInfo: can't lock file\n");
    Shutdown
  END;

  IF NOT Examine(TheLock,FIBPtr^) THEN
    WriteString("FileInfo: can't examine file\n");
    Shutdown
  END;

  IF NOT FileInfoWindow() THEN
    WriteString("FileInfo: not enough memory\n");
    Shutdown
  END;

  SetupTheWindow;
  FIWindowPtr := OpenWindow(NewWindowStructure1^);
  IF (FIWindowPtr = NIL) THEN
    WriteString("FileInfo; can't open window\n");
    Shutdown
  END;

  x := ActivateGadget(NameGad^,FIWindowPtr^,NIL);
  IF UserSaysSave() THEN
    UnLock(TheLock);
    TheLock := NIL;
    SaveTheChanges
  END;
  Shutdown
END FileInfo.
