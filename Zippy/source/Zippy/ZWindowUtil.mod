IMPLEMENTATION MODULE ZWindowUtil;  (* MEW 881102 *)

FROM SYSTEM IMPORT
    ADR;
FROM Intuition IMPORT
    GadgetPtr, OpenWindow, CloseWindow, SetMenuStrip, ClearMenuStrip, IntuiText,
    IntuiTextLength, PrintIText, DisplayBeep, AddGadget, RemoveGadget,
    RefreshGList, GadgetFlags, StringInfoPtr, NewModifyProp, PropInfoFlags,
    PropInfoFlagsSet, FreeRemember;
FROM Strings IMPORT
    CopyString, StringLength, ConcatString;
FROM Drawing IMPORT
    RectFill, SetAPen, SetDrMd;
FROM Rasters IMPORT
    Jam1, Jam2;

FROM BusyPointer IMPORT
    AllocBusyPointers, FreeBusyPointers, RestorePointer;
FROM ZGlobal IMPORT
    ZERR, ZOK, ZErrDescription, Str80, Str255;
FROM ZWindow IMPORT
    ZWindow, NewWindowStructure1, SourceGad, DestGad, SliderGad,
    ZWindowRememberPtr;


VAR
  SourceUndo, DestUndo : Str255;

PROCEDURE OpenZWindow() : BOOLEAN;
  VAR sip : StringInfoPtr;
  BEGIN
    ZWindowRememberPtr := NIL;
    IF NOT ZWindow() THEN  RETURN FALSE END;
    ZWindowPtr := OpenWindow(NewWindowStructure1^);
    IF (ZWindowPtr = NIL) THEN  RETURN FALSE  END;
    SetAPen(ZWindowPtr^.RPort^,CARDINAL(Grey));
    SetDrMd(ZWindowPtr^.RPort^,Jam1);
    fontHeight := ZWindowPtr^.RPort^.TxHeight;
    fontWidth  := ZWindowPtr^.RPort^.TxWidth;
    IF NOT AllocBusyPointers(ZWindowPtr) THEN  RETURN FALSE  END;
    sip := SourceGad^.SpecialInfo;  sip^.UndoBuffer := ADR(SourceUndo);
    sip := DestGad^.SpecialInfo;    sip^.UndoBuffer := ADR(DestUndo);
    RETURN TRUE
  END OpenZWindow;

PROCEDURE CloseZWindow;
  BEGIN
    IF (ZWindowPtr # NIL) THEN
      RestorePointer(ZWindowPtr);
      FreeBusyPointers;
      CloseWindow(ZWindowPtr^)
    END;
    FreeRemember(ZWindowRememberPtr,TRUE)
  END CloseZWindow;


(*$D-*)
PROCEDURE DisplayMessage(str1,str2,str3,str4 : ARRAY OF CHAR);
  CONST MsgTopEdge = 31;
  VAR
    itext : IntuiText;
    str : ARRAY [0..80] OF CHAR;
    pos : INTEGER;
  BEGIN
    CopyString(str,str1);
    ConcatString(str,str2);
    ConcatString(str,str3);
    ConcatString(str,str4);
    WITH itext DO
      FrontPen := Black;  BackPen := Grey;
      LeftEdge := 0;  TopEdge := 0;
      DrawMode := Jam2;
      ITextFont := NIL;  NextText := NIL;
      IText := ADR(str)
    END;
    WITH ZWindowPtr^ DO
      pos := (Width - INTEGER(IntuiTextLength(itext)) - 4) DIV 2;
      IF (pos < 4) THEN  pos := 4  END;
      RectFill(RPort^,2,MsgTopEdge,Width-3,MsgTopEdge+fontHeight);
      PrintIText(RPort^,itext,pos,MsgTopEdge)
    END
  END DisplayMessage;
(*$D+*)

PROCEDURE DisplayZErr(zerr : ZERR);
  VAR str : Str80;
  BEGIN
    ZErrDescription(zerr,str);
    IF (zerr # ZOK) THEN  DisplayBeep(ZWindowPtr^.WScreen)  END;
    DisplayMessage(str,"","","")
  END DisplayZErr;


PROCEDURE InGadget(VAR g : GadgetPtr; x,y : INTEGER) : BOOLEAN;
  VAR x1,y1,x2,y2 : INTEGER;
  BEGIN
    g := ZWindowPtr^.FirstGadget;
    WHILE (g # NIL) DO
      WITH g^ DO
        x1 := LeftEdge;  y1 := TopEdge;
        x2 := x1+Width;  y2 := y1+Height;
        IF (GRelWidth IN Flags) THEN  INC(x2,ZWindowPtr^.Width)  END;
        IF (x >= x1) AND (x <= x2) AND (y >= y1) AND (y <= y2) THEN
          RETURN TRUE
        END
      END;
      g := g^.NextGadget
    END;
    RETURN FALSE
  END InGadget;

PROCEDURE RefreshStringGad(g : GadgetPtr);
  VAR
    d : INTEGER;
    sip : StringInfoPtr;
    s : POINTER TO Str255;
  BEGIN
    d := RemoveGadget(ZWindowPtr^,g^);
    sip := g^.SpecialInfo;  s := sip^.Buffer;
    WITH sip^ DO
      BufferPos := StringLength(s^);
      IF (DispCount >= BufferPos) THEN
        DispPos := 0
      ELSE
        DispPos := BufferPos - DispCount
      END
    END;
    d := AddGadget(ZWindowPtr^,g^,d);
    RefreshGList(g^,ZWindowPtr^,NIL,1)
  END RefreshStringGad;

PROCEDURE ModifySlider(pot,body : CARDINAL);
  CONST SliderFlags = PropInfoFlagsSet{FreeVert,AutoKnob};
  BEGIN
    NewModifyProp(SliderGad^,ZWindowPtr^,NIL,SliderFlags,0,pot,0,body,1)
  END ModifySlider;


BEGIN
  ZWindowPtr := NIL;
  SourceUndo[0] := 0C;  DestUndo[0] := 0C
END ZWindowUtil.
