MODULE Zippy; (* MEW 881105 *)

FROM SYSTEM IMPORT
    ADR;
FROM Ports IMPORT
    ReplyMsg, GetMsg, WaitPort;
FROM AmigaDOS IMPORT
    Lock, UnLock, AccessRead, FileLock, CurrentDir;
FROM Intuition IMPORT
    IDCMPFlags, IntuiMessagePtr, GadgetPtr, DoubleClick, IDCMPFlagsSet,
    MenuPtr, MenuItemPtr, ItemAddress, MenuNull, FreeRemember, MENUNUM,
    ITEMNUM, SUBNUM, ModifyIDCMP, SelectDown, SelectUp;
FROM Strings IMPORT
    StringLength;
FROM Terminal IMPORT  (******** perhaps InOut ********)
    WriteString;

FROM ZWindow IMPORT
    SourceGad, DestGad, SliderGad, RefreshGad, DeviceGad, RootGad, ParentGad,
    SwapGad, PushGad, ClearGad, AllGad, ReverseGad, RestoreGad;
FROM ZGlobal IMPORT
    Str255, ZERR, ZOK, SourceLock, OrigLock;
FROM ZWindowUtil IMPORT
    ZWindowPtr, InGadget, DisplayZErr, OpenZWindow, CloseZWindow,
    RefreshStringGad;
FROM ZTable IMPORT
    ResizeTable, DisplayTable, DirInfoPtr, XY2DIP, SetupDragSelect,
    DoDragSelect, Select, SelectCode, FreeTable;
FROM BusyPointer IMPORT
    RestorePointer, SpinBusyPointer;
FROM ZCommands IMPORT
    SetCurrentDirectory, DisplayDeviceTable, DisplayDirectoryTable, RootPath,
    ParentPath, SwapPaths, CopyPath, UnSelectAll, SelectAllFiles, ReverseSelect,
    RestoreSelect, UndoPath, GetPath, SetPath, DeviceInfo, NewDir, Move, Copy,
    Delete, Launch, SelectStats;
FROM ZMenu IMPORT
    SetupZippyMenus, SetupGlobalMenus, UserMenuItemPtr, FreeAllMenus;
FROM ZDOS IMPORT
    TackOn;

(* Program Structure:
    BusyPointer ezMenus ezScript
    ZGlobal ZDOS ZWindow(created with PowerWindows 2.5)
    ZWindowUtil
    ZTable
    ZMenu
    ZCommands
    Zippy *)

TYPE
  ITMODE = (OFF, SLIDER, DRAGSELECT);
  Event = RECORD
    class : IDCMPFlagsSet;
    code : CARDINAL;
    x,y : INTEGER;
    gadget : GadgetPtr;
    secs,mics : LONGCARD;
    DIP : DirInfoPtr
  END;

VAR
  Msg, LastMouseDown : Event;
  ShutdownRequested : BOOLEAN;
  IntuiTickMode : ITMODE;


(* Event Processing *)

PROCEDURE SetIntuiTickMode(mode : ITMODE);
  VAR idcmp : IDCMPFlagsSet;
  BEGIN
    idcmp := ZWindowPtr^.IDCMPFlags;
    IF (mode = OFF) THEN
      EXCL(idcmp,IntuiTicks)
    ELSE
      INCL(idcmp,IntuiTicks)
    END;
    ModifyIDCMP(ZWindowPtr^,idcmp);
    IntuiTickMode := mode
  END SetIntuiTickMode;

PROCEDURE GetNextMessage;
  VAR msgptr : IntuiMessagePtr;
  BEGIN
    RestorePointer(ZWindowPtr);
    msgptr := WaitPort(ZWindowPtr^.UserPort^);
    msgptr := GetMsg(ZWindowPtr^.UserPort^);
    WITH msgptr^ DO
      WITH Msg DO
        class := Class;  code := Code;
        x := MouseX;  y := MouseY;
        gadget := IAddress;
        secs := Seconds;  mics := Micros;
        DIP := NIL
      END
    END;
    ReplyMsg(msgptr)
  END GetNextMessage;

PROCEDURE ProcessIntuiTick;
  BEGIN
    IF (IntuiTickMode = SLIDER) THEN
      DisplayTable(FALSE)
    ELSIF (IntuiTickMode = DRAGSELECT) THEN
      DoDragSelect(Msg.x,Msg.y)
    END
  END ProcessIntuiTick;

PROCEDURE ProcessGadgetDown;
  BEGIN
    IF (Msg.gadget = SliderGad) THEN
      DisplayTable(FALSE);
      SetIntuiTickMode(SLIDER)
    END
  END ProcessGadgetDown;

PROCEDURE ProcessGadgetUp;
  BEGIN
    WITH Msg DO
      IF (gadget = SourceGad) THEN
        RefreshStringGad(SourceGad);
        SetCurrentDirectory
      ELSIF (gadget = DestGad) THEN     RefreshStringGad(DestGad)
      ELSIF (gadget = SliderGad) THEN   SetIntuiTickMode(OFF)
      ELSIF (gadget = RefreshGad) THEN  DisplayDirectoryTable
      ELSIF (gadget = DeviceGad) THEN   DisplayDeviceTable
      ELSIF (gadget = RootGad) THEN     RootPath(SourceGad)
      ELSIF (gadget = ParentGad) THEN   ParentPath(SourceGad)
      ELSIF (gadget = SwapGad) THEN     SwapPaths
      ELSIF (gadget = PushGad) THEN     CopyPath(SourceGad,DestGad)
      ELSIF (gadget = ClearGad) THEN    UnSelectAll
      ELSIF (gadget = AllGad) THEN      SelectAllFiles
      ELSIF (gadget = ReverseGad) THEN  ReverseSelect
      ELSIF (gadget = RestoreGad) THEN  RestoreSelect
      END
    END
  END ProcessGadgetUp;

PROCEDURE ProcessMenuButtonGadget;
  BEGIN
    WITH Msg DO
      IF (gadget = SourceGad) THEN     UndoPath(SourceGad)
      ELSIF (gadget = DestGad) THEN    UndoPath(DestGad)
      ELSIF (gadget = RootGad) THEN    RootPath(DestGad)
      ELSIF (gadget = ParentGad) THEN  ParentPath(DestGad)
      END
    END
  END ProcessMenuButtonGadget;

PROCEDURE ProcessMouseButtons;
  VAR temp : Str255;
  BEGIN
    WITH Msg DO
      DIP := XY2DIP(x,y);
      IF (code = SelectUp) THEN
        SetIntuiTickMode(OFF)
      ELSIF (code = SelectDown) AND (DIP # NIL) THEN
        IF (LastMouseDown.DIP = DIP) AND DoubleClick(LastMouseDown.secs,
            LastMouseDown.mics,secs,mics) THEN
          WITH DIP^ DO
            IF diIsDir THEN
              GetPath(SourceGad,temp);
              TackOn(temp,diName);
              SetPath(SourceGad,temp);
              SetCurrentDirectory
            END
          END
        ELSE
	  Select(DIP,Toggle);
          DisplayZErr(ZOK);
          DisplayTable(TRUE);
          SetIntuiTickMode(DRAGSELECT);
          SetupDragSelect(x,y)
        END;
        LastMouseDown := Msg
      END
    END
  END ProcessMouseButtons;

PROCEDURE ProcessMenu;
  VAR
    mi : MenuItemPtr;
    umi : UserMenuItemPtr;
    menucode,itemnum,subnum : CARDINAL;
    gptr : GadgetPtr;
  BEGIN
    IF (Msg.code = MenuNull) AND InGadget(Msg.gadget,Msg.x,Msg.y) THEN
      ProcessMenuButtonGadget
    ELSE
      menucode := Msg.code;
      WHILE (menucode # MenuNull) DO
        SpinBusyPointer(ZWindowPtr,TRUE);
        mi := ItemAddress(ZWindowPtr^.MenuStrip^,menucode);
        itemnum := ITEMNUM(menucode);  subnum := SUBNUM(menucode);
        CASE MENUNUM(menucode) OF
          0 : CASE itemnum OF   (* Zippy *)
                0 : (* About *)  |
                1 : DisplayDirectoryTable  |
                2 : ShutdownRequested := TRUE
	      END  |
          1 : CASE itemnum OF   (* Path *)
                0 : CASE subnum OF   (* Source... *)
                      0 : RootPath(SourceGad)  |
                      1 : ParentPath(SourceGad)  |
                      2 : CopyPath(SourceGad,DestGad)  |
                      3 : DeviceInfo(SourceGad)
                    END  |
		1 : CASE subnum OF   (* Dest... *)
		      0 : RootPath(DestGad)  |
		      1 : ParentPath(DestGad)  |
		      2 : CopyPath(DestGad,SourceGad)  |
		      3 : DeviceInfo(DestGad)
                    END  |
                2 : SwapPaths
              END  |
          2 : CASE itemnum OF   (* Select *)
                0 : UnSelectAll  |
                1 : SelectAllFiles  |
                2 : ReverseSelect  |
                3 : RestoreSelect  |
                4 : SelectStats
              END  |
          3 : CASE itemnum OF   (* DOS *)
	        0 : Move  |
		1 : Copy  |
		2 : Delete  |
		3 : NewDir
              END  |
	  ELSE                 (* Process the user menus *)
	    umi := UserMenuItemPtr(mi);
	    IF (umi^.Command # NIL) THEN
	      Launch(umi^.Command^,umi^.Reward)
	    END
        END;
        menucode := mi^.NextSelect
      END
    END
  END ProcessMenu;
      
PROCEDURE ProcessMessages;
  BEGIN
    REPEAT
      GetNextMessage;
      IF    (IntuiTicks IN Msg.class) THEN    ProcessIntuiTick
      ELSIF (GadgetDown IN Msg.class) THEN    ProcessGadgetDown
      ELSIF (GadgetUp IN Msg.class) THEN      ProcessGadgetUp
      ELSIF (MouseButtons IN Msg.class) THEN  ProcessMouseButtons
      ELSIF (MenuPick IN Msg.class) THEN      ProcessMenu
      ELSIF (Closewindow IN Msg.class) THEN   ShutdownRequested := TRUE
      ELSIF (NewSize IN Msg.class) THEN
        ResizeTable;
	DisplayZErr(ZOK);
        DisplayTable(TRUE)
      END
    UNTIL ShutdownRequested
  END ProcessMessages;

PROCEDURE Shutdown;
  BEGIN
    FreeAllMenus;
    CloseZWindow;
    FreeTable;
    IF (OrigLock # NIL) THEN  OrigLock := CurrentDir(OrigLock)  END;
    IF (SourceLock # NIL) THEN  UnLock(SourceLock)  END;
    HALT
  END Shutdown;

BEGIN    (* MAIN *)
  ShutdownRequested := FALSE;
  IntuiTickMode := OFF;
  LastMouseDown.DIP := NIL;
  IF NOT OpenZWindow() THEN
    WriteString("Zippy: cant open window\n");
    Shutdown
  END;
  SetupZippyMenus;
  SetupGlobalMenus;
  ResizeTable;
  SetPath(SourceGad,"");
  SetCurrentDirectory;
  ProcessMessages;
  Shutdown
END Zippy.
