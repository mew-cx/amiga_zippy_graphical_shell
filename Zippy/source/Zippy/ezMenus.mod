IMPLEMENTATION MODULE ezMenus;  (* MEW 881105 *)

(* Written in Benchmark Modula-2 *)
(* by Michael Weiblen   PLink: EKIM   CIS: 72506,2072 *)

FROM SYSTEM IMPORT
    ADR, ADDRESS, BYTE;
FROM Intuition IMPORT
    WindowPtr, Menu, MenuPtr, MenuFlags, MenuFlagsSet, MenuItem, MenuItemPtr,
    MenuItemFlags, MenuItemFlagsSet, MenuItemMutualExcludeSet, IntuiText,
    IntuiTextPtr, HighComp;
FROM Rasters IMPORT
    DrawModeSet, Jam1;
FROM Strings IMPORT
    StringLength, CopyString;
FROM Storage IMPORT
    ALLOCATE, DEALLOCATE;


CONST
  MaxStringLength = 255;

TYPE
  Str = ARRAY [0..MaxStringLength] OF CHAR;

VAR
  FontHeight, FontWidth : INTEGER;


PROCEDURE InitMenuStrip(wp : WindowPtr);
  BEGIN
    TheWindow := wp;
    FontHeight := wp^.RPort^.TxHeight;
    FontWidth  := wp^.RPort^.TxWidth;
    ezFrontPen := BYTE(0);  ezBackPen := BYTE(1);
    ezDrawMode := Jam1;
    ezExcludeSet := MenuItemMutualExcludeSet{};
    ezMenuItemFlags := MenuItemFlagsSet{ItemText,ItemEnabled} + HighComp;
    ezMenuFlags := MenuFlagsSet{MenuEnabled};
    FirstMenu := NIL;
    PrevMenu := NIL;  PrevMenuItem := NIL;  PrevSubItem := NIL
  END InitMenuStrip;

(*$D-*)

PROCEDURE AllocString(VAR sp : ADDRESS; str : ARRAY OF CHAR);
  VAR
    xsp : POINTER TO Str;
    size : CARDINAL;
  BEGIN
    sp := NIL;
    size := StringLength(str) + 1;
    IF (size > 0) AND (size <= MaxStringLength) THEN
      ALLOCATE(sp,size);
      IF (sp # NIL) THEN
        xsp := sp;
        CopyString(xsp^,str)
      END
    END
  END AllocString;

PROCEDURE AllocIText(VAR it : IntuiTextPtr; str : ARRAY OF CHAR;
    left, top : INTEGER);
  BEGIN
    ALLOCATE(it,SIZE(IntuiText));  IF (it = NIL) THEN  RETURN  END;
    WITH it^ DO
      FrontPen := ezFrontPen;  ezBackPen := BackPen;  DrawMode := ezDrawMode;
      LeftEdge := left;  TopEdge := top;
      ITextFont := NIL;
      AllocString(IText,str);
      NextText := NIL
    END
  END AllocIText;

PROCEDURE AllocSubItem(str : ARRAY OF CHAR; left,width : INTEGER; key : CHAR;
    size : CARDINAL);
  VAR
    next,si : MenuItemPtr;
    top,height : INTEGER;
  BEGIN
    IF (PrevMenuItem = NIL) THEN  RETURN  END;
    IF (size < SIZE(MenuItem)) THEN  size := SIZE(MenuItem)  END;
    ALLOCATE(si,size);  IF (si = NIL) THEN  RETURN  END;
    IF (PrevSubItem # NIL) THEN
      next := PrevSubItem^.NextItem;  PrevSubItem^.NextItem := si;
      top := PrevSubItem^.TopEdge + PrevSubItem^.Height
    ELSE
      next := PrevMenuItem^.SubItem;  PrevMenuItem^.SubItem := si;
      top := 0
    END;
    height := FontHeight + 2;
    WITH si^ DO
      NextItem := next;
      LeftEdge := left;  TopEdge := top;
      Width := width;  Height := height;
      Flags := ezMenuItemFlags;
      IF (key # 0C) THEN  INCL(Flags,CommSeq)  END;
      MutualExclude := ezExcludeSet;
      AllocIText(IntuiTextPtr(ItemFill),str,1,1);
      SelectFill := NIL;
      Command := BYTE(ORD(key));
      SubItem := NIL
    END;
    PrevSubItem := si
  END AllocSubItem;

PROCEDURE AllocMenuItem(str : ARRAY OF CHAR; left,width : INTEGER; key : CHAR;
    size : CARDINAL);
  VAR
    next,mi : MenuItemPtr;
    top,height : INTEGER;
  BEGIN
    IF (PrevMenu = NIL) THEN  RETURN  END;
    IF (size < SIZE(MenuItem)) THEN  size := SIZE(MenuItem)  END;
    ALLOCATE(mi,size);  IF (mi = NIL) THEN  RETURN  END;
    IF (PrevMenuItem # NIL) THEN
      next := PrevMenuItem^.NextItem;  PrevMenuItem^.NextItem := mi;
      top := PrevMenuItem^.TopEdge + PrevMenuItem^.Height
    ELSE
      next := PrevMenu^.FirstItem;  PrevMenu^.FirstItem := mi;
      top := 0
    END;
    height := FontHeight + 2;
    WITH mi^ DO
      NextItem := next;
      LeftEdge := left;  TopEdge := top;
      Width := width;  Height := height;
      Flags := ezMenuItemFlags;
      IF (key # 0C) THEN  INCL(Flags,CommSeq)  END;
      MutualExclude := ezExcludeSet;
      AllocIText(IntuiTextPtr(ItemFill),str,1,1);
      SelectFill := NIL;
      Command := BYTE(ORD(key));
      SubItem := NIL
    END;
    PrevMenuItem := mi;  PrevSubItem := NIL
  END AllocMenuItem;

PROCEDURE AllocMenu(str : ARRAY OF CHAR);
  VAR
    next,m : MenuPtr;
    left,width : INTEGER;
  BEGIN
    ALLOCATE(m,SIZE(Menu));  IF (m = NIL) THEN  RETURN  END;
    IF (PrevMenu # NIL) THEN
      next := PrevMenu^.NextMenu;  PrevMenu^.NextMenu := m;
      left := PrevMenu^.LeftEdge + PrevMenu^.Width
    ELSE
      next := FirstMenu;  FirstMenu := m;
      left := 0
    END;
    width := INTEGER(StringLength(str)) * FontWidth + 6;
    WITH m^ DO
      NextMenu := next;
      LeftEdge := left;  TopEdge := 0;
      Width := width;  Height := FontHeight;
      Flags := ezMenuFlags;
      AllocString(MenuName,str);
      FirstItem := NIL
    END;
    PrevMenu := m;  PrevMenuItem := NIL;  PrevSubItem := NIL
  END AllocMenu;

(*$D+*)


PROCEDURE FreeString(VAR sp : ADDRESS);
  VAR xsp : POINTER TO Str;
  BEGIN
    IF (sp # NIL) THEN
      xsp := sp;
      DEALLOCATE(sp,StringLength(xsp^)+1);
      sp := NIL
    END
  END FreeString;

PROCEDURE FreeIText(VAR it : IntuiTextPtr);
  BEGIN
    IF (it # NIL) THEN
      FreeString(it^.IText);
      DEALLOCATE(it,SIZE(IntuiText));
      it := NIL
    END
  END FreeIText;

PROCEDURE FreeSubItem(VAR si : MenuItemPtr; size : CARDINAL);
  VAR next : MenuItemPtr;
  BEGIN
    IF (size < SIZE(MenuItem)) THEN  size := SIZE(MenuItem)  END;
    IF (si # NIL) THEN
      FreeIText(IntuiTextPtr(si^.ItemFill));
      next := si^.NextItem;
      DEALLOCATE(si,size);
      si := next;
      PrevSubItem := NIL
    END
  END FreeSubItem;

PROCEDURE FreeMenuItem(VAR mi : MenuItemPtr; size : CARDINAL);
  VAR next : MenuItemPtr;
  BEGIN
    IF (size < SIZE(MenuItem)) THEN  size := SIZE(MenuItem)  END;
    IF (mi # NIL) THEN
      WHILE (mi^.SubItem # NIL) DO  FreeSubItem(mi^.SubItem,0)  END;
      FreeIText(IntuiTextPtr(mi^.ItemFill));
      next := mi^.NextItem;
      DEALLOCATE(mi,size);
      mi := next;
      PrevMenuItem := NIL
    END
  END FreeMenuItem;

PROCEDURE FreeMenu(VAR m : MenuPtr);
  VAR
    next : MenuPtr;
    first : BOOLEAN;
  BEGIN
    IF (m # NIL) THEN
      WHILE (m^.FirstItem # NIL) DO  FreeMenuItem(m^.FirstItem,0)  END;
      first := (m = FirstMenu);
      FreeString(m^.MenuName);
      next := m^.NextMenu;
      DEALLOCATE(m,SIZE(Menu));
      m := next;
      IF first THEN  FirstMenu := m  END;
      PrevMenu := NIL
    END
  END FreeMenu;

END ezMenus.
