IMPLEMENTATION MODULE ZMenu;  (* MEW 881105a *)

FROM Intuition IMPORT
    MenuPtr, MenuFlags, MenuFlagsSet, MenuItem, MenuItemPtr, MenuItemFlags,
    MenuItemFlagsSet, HighNone, SetMenuStrip, ClearMenuStrip;
FROM Rasters IMPORT
    Jam1;
FROM Storage IMPORT
    ALLOCATE, DEALLOCATE;
FROM Strings IMPORT
    CompareStringCAP, Relation, StringLength;

FROM ezScript IMPORT
    OpenScript, CloseScript, GetNextToken;
FROM ezMenus IMPORT
    InitMenuStrip, AllocMenu, AllocMenuItem, AllocSubItem, AllocString,
    FreeString, FreeMenu, FreeMenuItem, FirstMenu, PrevMenu, PrevMenuItem,
    ezMenuItemFlags;
FROM ZGlobal IMPORT
    Str80;
FROM ZWindowUtil IMPORT
    ZWindowPtr, fontWidth, WBColors;
FROM ZTable IMPORT
    SelectCode;


CONST
  GlobalMenuScriptName = "s:Menus.zip";
  LocalMenuScriptName  = "Menus.zip";

VAR
  LastZippyMenu, LastGlobalMenu : MenuPtr;


(* Internal routines *)

(* Menu routines *)

PROCEDURE AllocUserMenuItem(title,command : ARRAY OF CHAR; key : CHAR;
    reward : SelectCode);
  VAR
    width : INTEGER;
    umi : UserMenuItemPtr;
    pmi : MenuItemPtr;
  BEGIN
    IF (PrevMenu = NIL) OR (title[0] = 0C) THEN  RETURN  END;
    width := INTEGER(StringLength(title)) * fontWidth + 2;
    pmi := PrevMenuItem;
    AllocMenuItem(title,0,width,key,SIZE(UserMenuItem));
    IF (PrevMenuItem = pmi) THEN  RETURN  END;
    umi := UserMenuItemPtr(PrevMenuItem);
    AllocString(umi^.Command,command);
    umi^.Reward := reward
  END AllocUserMenuItem;
    
PROCEDURE FreeUserMenuItem(VAR mi : MenuItemPtr);
  VAR umi : UserMenuItemPtr;
  BEGIN
    IF (mi # NIL) THEN
      umi := UserMenuItemPtr(mi);
      FreeString(umi^.Command);
      FreeMenuItem(mi,SIZE(UserMenuItem))
    END
  END FreeUserMenuItem;

PROCEDURE FreeUserMenu(VAR m : MenuPtr);
  VAR next : MenuPtr;
  BEGIN
    IF (m # NIL) THEN
      WHILE (m^.FirstItem # NIL) DO  FreeUserMenuItem(m^.FirstItem)  END;
      FreeMenu(m)
    END
  END FreeUserMenu;


(* Script routines *)

PROCEDURE GetSelectCode(c : CHAR) : SelectCode;
  VAR sc : SelectCode;
  BEGIN
    sc := Reread;
    IF    (CAP(c) = "Z") THEN  sc := Zap
    ELSIF (CAP(c) = "U") THEN  sc := Off
    END;
    RETURN sc
  END GetSelectCode;

PROCEDURE LoadMenuScript(name : ARRAY OF CHAR; VAR last : MenuPtr);
  VAR
    keyword,title,key,command,reward : Str80;
  BEGIN
    IF (last = NIL) THEN  RETURN  END;
    ClearMenuStrip(ZWindowPtr^);
    WHILE (last^.NextMenu # NIL) DO  FreeUserMenu(last^.NextMenu)  END;
    PrevMenu := last;
    IF OpenScript(name) THEN
      LOOP
        GetNextToken(keyword);
        IF (keyword[0] = 0C) THEN  EXIT  END;
        IF (CompareStringCAP(keyword,"MENU") = equal) THEN
          GetNextToken(title);
          AllocMenu(title)
        ELSIF (CompareStringCAP(keyword,"MENUITEM") = equal) THEN
          GetNextToken(title);
          GetNextToken(key);
          GetNextToken(command);
          GetNextToken(reward);
          IF (PrevMenu # last) THEN
            AllocUserMenuItem(title,command,key[0],GetSelectCode(reward[0]))
          END
        END
      END;
      CloseScript
    END;
    SetMenuStrip(ZWindowPtr^,FirstMenu^)
  END LoadMenuScript;


(* External routines *)

PROCEDURE SetupZippyMenus;
  CONST aboutflags = MenuItemFlagsSet{ItemEnabled,ItemText} + HighNone;
  VAR saveflags : MenuItemFlagsSet;
  BEGIN
    InitMenuStrip(ZWindowPtr);
    AllocMenu(" Zippy ");
      AllocMenuItem("About...",0,160,0C,0);
        saveflags := ezMenuItemFlags;
        ezMenuItemFlags := aboutflags;
        AllocSubItem(" The Zippy Graphic Shell", 80,200,0C,0);
        AllocSubItem(" Version 2.5   11/05/88",  80,200,0C,0);
        AllocSubItem("    by Mike Weiblen",      80,200,0C,0);
        AllocSubItem("      PLink: EKIM",        80,200,0C,0);
        AllocSubItem("      CIS: 72506,2072",    80,200,0C,0);
        AllocSubItem("      GEnie: XTH36273",    80,200,0C,0);
        ezMenuItemFlags := saveflags;
      AllocMenuItem("Refresh Display",0,160,".",0);
      AllocMenuItem("Quit",0,160,"Q",0);
    AllocMenu(" Path ");
      AllocMenuItem("Source...",0,144,0C,0);
        AllocSubItem("Root",       90,128,";",0);
        AllocSubItem("Parent",     90,128,"/",0);
        AllocSubItem("Push S to D",90,128,0C,0);
        AllocSubItem("Free Bytes", 90,128,"=",0);
      AllocMenuItem("Dest...",0,144,0C,0);
        AllocSubItem("Root",       90,128,":",0);
        AllocSubItem("Parent",     90,128,"?",0);
        AllocSubItem("Push D to S",90,128,0C,0);
        AllocSubItem("Free Bytes", 90,128,"+",0);
      AllocMenuItem("Swap S and D",0,144,"S",0);
    AllocMenu(" Select ");
      AllocMenuItem("UnSelect All",    0,176,"U",0);
      AllocMenuItem("Select All Files",0,176,"A",0);
      AllocMenuItem("Reverse Selects", 0,176,"[",0);
      AllocMenuItem("Restore Previous",0,176,"]",0);
      AllocMenuItem("Bytes Selected",  0,176,"B",0);
    AllocMenu(" DOS ");
      AllocMenuItem("Move to D",     0,160,"M",0);
      AllocMenuItem("Copy to D",     0,160,"C",0);
      AllocMenuItem("Delete",        0,160,"D",0);
      AllocMenuItem("Make New Dir D",0,160,"N",0);
    LastZippyMenu := PrevMenu;
    LastGlobalMenu  := PrevMenu;
    SetMenuStrip(ZWindowPtr^,FirstMenu^)
  END SetupZippyMenus;

PROCEDURE SetupGlobalMenus;
  BEGIN
    LoadMenuScript(GlobalMenuScriptName,LastZippyMenu);
    LastGlobalMenu := PrevMenu
  END SetupGlobalMenus;

PROCEDURE SetupLocalMenus;
  BEGIN
    LoadMenuScript(LocalMenuScriptName,LastGlobalMenu)
  END SetupLocalMenus;

PROCEDURE FreeAllMenus;
  BEGIN
    ClearMenuStrip(ZWindowPtr^);
    IF (LastZippyMenu # NIL) THEN
      WHILE (LastZippyMenu^.NextMenu # NIL) DO
        FreeUserMenu(LastZippyMenu^.NextMenu)
      END
    END;
    IF (FirstMenu # NIL) THEN
      WHILE (FirstMenu # NIL) DO
        FreeMenu(FirstMenu)
      END
    END
  END FreeAllMenus;


BEGIN
  LastZippyMenu := NIL;  LastGlobalMenu := NIL
END ZMenu.
