IMPLEMENTATION MODULE ZTable  (* 881103 *);

FROM SYSTEM IMPORT
    ADR, ADDRESS, SHIFT;
FROM System IMPORT
    DOSBase;
FROM Strings IMPORT
    CopyString, StringLength, OverwriteWithSubString, ConcatString,
    CompareStringCAP, greater;
FROM AmigaDOS IMPORT
    Lock, UnLock, Examine, ExNext, FileLock, FileInfoBlockPtr, FileInfoBlock,
    BPTR, BSTR, ProtectionSet, FibArchive, FibRead, FibWrite, FibExecute,
    FibDelete;
FROM AmigaDOSExt IMPORT
    DeviceListPtr, DLTDevice, DLTDirectory, DosLibraryPtr, RootNodePtr,
    DosInfoPtr;
FROM Intuition IMPORT
    PrintIText, IntuiText, MaxPot, MaxBody, PropInfoPtr;
FROM Storage IMPORT
    ALLOCATE, DEALLOCATE;
FROM Rasters IMPORT
    Jam1, Jam2;
FROM Drawing IMPORT
    RectFill;
FROM Interrupts IMPORT
    Forbid, Permit;

FROM ZWindow IMPORT
    SliderGad, SourceGad, DestGad;
FROM ZGlobal IMPORT
    Str255, ZERR, ZOK, ZNoMem, ZBadSource, ZTooMany, NumToStr, Ratio,
    FibHidden, FibScript, FibPure;
FROM ZWindowUtil IMPORT
    ZWindowPtr, ModifySlider, fontHeight, fontWidth,
    WBColors, RefreshStringGad;
FROM BusyPointer IMPORT
    SpinBusyPointer;


CONST
  AbsoluteMaxFiles = 1000;
  tLeftEdge = 6;
  tTopEdge  = 53;


TYPE
  LINE = INTEGER;  (* physical location of a line in directory window *)
  INDEX = INTEGER; (* The legal range is [1..AbsoluteMaxFiles] *)

VAR
  Table : ARRAY [1..AbsoluteMaxFiles] OF DirInfoPtr;
  tFirst, DragStart, DragPrev, NextIndex : INDEX;
  tLen, tHeight, tWidth, tRange, ZapCount : INTEGER;


(* Internal procedures *)

PROCEDURE UpdateSlider;
  CONST fudgefactor = MaxPot DIV (AbsoluteMaxFiles + 1);
  VAR pot : CARDINAL;
  BEGIN                  
    tRange := tLen - tHeight;
    IF (tRange <= 0) THEN
      pot := 0;  tRange := 0
    ELSIF (tFirst = tRange+1) THEN
      pot := MaxPot
    ELSE
      pot := Ratio(MaxPot,tFirst-1,tRange+1) + fudgefactor
    END;
    ModifySlider(pot,Ratio(MaxBody,tHeight,tLen))
  END UpdateSlider;

PROCEDURE SliderIndex() : INDEX;
  VAR
    pip : PropInfoPtr;
    ind : INDEX;
  BEGIN
    pip := SliderGad^.SpecialInfo;
    IF (tRange = 0) THEN
      ind := 1
    ELSIF (pip^.VertPot = MaxPot) THEN
      ind := tRange+1
    ELSE
      ind := Ratio(tRange+1,pip^.VertPot,MaxPot) + 1
    END;
    RETURN ind
  END SliderIndex;

PROCEDURE Y2Line(y : INTEGER) : LINE;
  BEGIN
    IF (y < tTopEdge) THEN
      RETURN (-1)
    ELSE
      RETURN ((y - tTopEdge) DIV fontHeight)
    END
  END Y2Line;

PROCEDURE HeapSort(n : INDEX);
  VAR
    i,j,mid : INDEX;
    x : DirInfoPtr;
  BEGIN
    IF (n < 2) THEN  RETURN  END;
    mid := n DIV 2 + 1;
    SpinBusyPointer(ZWindowPtr,TRUE);
    LOOP 
      IF (mid > 1) THEN
        DEC(mid);
        x := Table[mid]
      ELSE
        x := Table[n];  Table[n] := Table[1];
        DEC(n);
        IF (n = 1) THEN
          Table[1] := x;
          RETURN
        END
      END;
      i := mid;  j := mid + mid;
      WHILE (j <= n) DO
        IF (j < n) AND NOT
            (CompareStringCAP(Table[j]^.diName,Table[j+1]^.diName) = greater) THEN
          INC(j)
        END;
        IF NOT (CompareStringCAP(x^.diName,Table[j]^.diName) = greater) THEN
          Table[i] := Table[j];
          i := j;  j := j + j
        ELSE
          j := n + 1
        END
      END;
      Table[i] := x
    END (* loop *)
  END HeapSort;

(*
PROCEDURE ProtFlags(VAR str : ARRAY OF CHAR; pset : ProtectionSet);
  VAR
    f : CARDINAL;
    a,b : BOOLEAN;
  BEGIN
    CopyString(str,"hsparwed");
    FOR f := FibDelete TO FibHidden DO
      a := (f < FibArchive);
      b := (f IN pset);
     IF (a AND b) OR (NOT(a) AND NOT(b)) THEN  str[FibHidden-f] := "-"  END
   END
  END ProtFlags;
*)

PROCEDURE DisplayALine(line : LINE);
  VAR
    fp,bp,sp : WBColors;
    fsize : ARRAY [0..33] OF CHAR;
    pflags : ARRAY [0..33] OF CHAR;
    str : Str255;
    itext : IntuiText;
    i : INTEGER;
    index : INDEX;
  BEGIN
    FOR i := 0 TO tWidth DO  str[i] := " "  END;
    index := line + tFirst;
    IF (index > tLen) THEN
      fp := Red;  bp := Red
    ELSE
      WITH Table[index]^ DO
        fp := Orange;  bp := Red;
        OverwriteWithSubString(str,diName,0);
        (*
        IF (tWidth > MaxFileNameSize + 5) THEN
          ProtFlags(pflags,diProt);
          OverwriteWithSubString(str,pflags,MaxFileNameSize+2)
        END;
        *)
        IF NOT diIsDir THEN
          fp := Black;  bp := Red;
          NumToStr(fsize, diSize);
          OverwriteWithSubString(str,fsize,CARDINAL(tWidth)-StringLength(fsize))
        END;
        IF diSelected THEN  sp := fp;  fp := bp;  bp := sp  END
      END
    END;
    str[tWidth] := 0C;
    WITH itext DO
      FrontPen := fp;  BackPen := bp;
      LeftEdge := tLeftEdge;
      TopEdge := tTopEdge + (line * fontHeight);
      DrawMode := Jam2;
      ITextFont := NIL;  NextText := NIL;
      IText := ADR(str)
    END;
    PrintIText(ZWindowPtr^.RPort^,itext,0,0)
  END DisplayALine;

PROCEDURE AllocEntry(i : INDEX) : ZERR;
  VAR result : ZERR;
  BEGIN
    result := ZOK;
    IF (i > AbsoluteMaxFiles) THEN
      result := ZTooMany
    ELSIF (Table[i] = NIL) THEN
      ALLOCATE(Table[i],SIZE(DirInfoRec));
      IF (Table[i] = NIL) THEN  result := ZNoMem  END
    END;
    RETURN result
  END AllocEntry;

PROCEDURE FreeEntries(a,b : INDEX);
  BEGIN
    WHILE (a <= b) DO
      IF (Table[a] # NIL) THEN
        DEALLOCATE(Table[a],SIZE(DirInfoRec));
        Table[a] := NIL
      END;
      INC(a)
    END
  END FreeEntries;


(* External procedures *)

(* ***** Access routines ***** *)

PROCEDURE SaveSelections;
  VAR i : INDEX;
  BEGIN
    IF (tLen > 0) THEN
      FOR i := 1 TO tLen DO
        WITH Table[i]^ DO  diWasSelected := diSelected  END
      END
    END
  END SaveSelections;

PROCEDURE NextEntry(dip : DirInfoPtr) : DirInfoPtr;
  BEGIN
    IF (dip = NIL) THEN  NextIndex := 0  END;
    INC(NextIndex);
    IF (NextIndex <= tLen) THEN
      RETURN Table[NextIndex]
    ELSE
      RETURN NIL
    END
  END NextEntry;

PROCEDURE XY2DIP(x,y : INTEGER) : DirInfoPtr;
  VAR
    line : LINE;
    i : INDEX;
  BEGIN
    DEC(x,tLeftEdge);  x := x DIV fontWidth;
    line := Y2Line(y);
    IF (x >= 0) AND (x < tWidth) AND (line >= 0) AND (line < tHeight) THEN
      i := line + tFirst;
      IF (i > 0) AND (i <= tLen) THEN  RETURN Table[i]  END
    END;
    RETURN NIL
  END XY2DIP;


(* ***** Maintenence routines ***** *)

PROCEDURE Select(dip : DirInfoPtr; code : SelectCode);
  VAR bool : BOOLEAN;
  BEGIN
    WITH dip^ DO
      IF diSelected THEN
        IF diIsDir THEN
          DEC(NumDirsSelected)
        ELSE
          DEC(NumFilesSelected);  DEC(BytesSelected,diSize)
        END
      END;
      CASE code OF
        On :      diSelected := TRUE  |
        Off :     diSelected := FALSE  |
        Toggle :  diSelected := NOT diSelected  |
        Restore :
          bool := diSelected;
          diSelected := diWasSelected;
          diWasSelected := bool  |
        Zap :
          diName[0] := 177C;  diName[1] := 177C;  diName[2] := 0C;
          INC(ZapCount);
          diSelected := FALSE
      END;
      IF diSelected THEN
        IF diIsDir THEN
          INC(NumDirsSelected)
        ELSE
          INC(NumFilesSelected);  INC(BytesSelected,diSize)
        END
      END
    END
  END Select;

PROCEDURE ResizeTable;
  VAR right,bottom : INTEGER;
  BEGIN
    right := ZWindowPtr^.Width - 16;
    bottom := ZWindowPtr^.Height - 2;
    tWidth := (right - tLeftEdge) DIV fontWidth;
    tHeight  := (bottom - tTopEdge) DIV fontHeight;
    IF (tWidth > 0) AND (tHeight > 0) THEN
      RectFill(ZWindowPtr^.RPort^, tLeftEdge, tTopEdge, right, bottom)
    ELSE
      tWidth := 1;  tHeight := 1
    END;
    UpdateSlider;
    RefreshStringGad(SourceGad);
    RefreshStringGad(DestGad)
  END ResizeTable;

PROCEDURE EmptyTable;
  BEGIN
    tLen := 0;  tRange := 0;
    tFirst := 1;
    NumFilesSelected := 0;  NumDirsSelected := 0;
    BytesSelected := 0D;
    ZapCount := 0
  END EmptyTable;

PROCEDURE LoadTableFromDirectory(lock : FileLock; VAR files,dirs : CARDINAL) : ZERR;
  VAR
    result : ZERR;
    fiblock : ARRAY [0..SIZE(FileInfoBlock)+4] OF CHAR;
    fib  : FileInfoBlockPtr;
  BEGIN
    result := ZOK;
    files := 0;  dirs := 0;
    fib := (ADR(fiblock) DIV 4D + 1D) * 4D;
    EmptyTable;
    IF (lock # NIL) AND Examine(lock,fib^) AND (fib^.fibDirEntryType > 0D) THEN
      LOOP
        SpinBusyPointer(ZWindowPtr,TRUE);
        IF NOT ExNext(lock,fib^) THEN  EXIT  END;
        IF NOT (FibHidden IN fib^.fibProtection) THEN
          result := AllocEntry(tLen+1);
          IF (result # ZOK) THEN  EXIT  END;
          INC(tLen);
          WITH Table[tLen]^ DO
            CopyString(diName,fib^.fibFileName);
            diIsDir := (fib^.fibDirEntryType > 0D);
            diProt := fib^.fibProtection;
            diSelected  := FALSE;  diWasSelected := FALSE;
            IF diIsDir THEN
              INC(dirs);  diSize := 0
            ELSE
              INC(files);  diSize  := fib^.fibSize
            END
          END
        END
      END;
      FreeEntries(tLen+1,AbsoluteMaxFiles);
      HeapSort(tLen)
    ELSE
      result := ZBadSource
    END;
    UpdateSlider;
    RETURN result
  END LoadTableFromDirectory;


PROCEDURE LoadTableWithDevices() : ZERR;

  PROCEDURE APTR(b : BPTR) : ADDRESS;
    BEGIN
      RETURN SHIFT(b,2);
    END APTR;

  PROCEDURE BtoMStr(bstr : BSTR; VAR mstr : ARRAY OF CHAR);
    VAR
      i, count : CARDINAL;
      b : POINTER TO CHAR;
    BEGIN
      b := APTR(bstr);
      count := ORD(b^);
      FOR i := 0 TO count - 1 DO
        INC(LONGCARD(b));
        mstr[i] := b^;
      END;
      mstr[count] := 0C;
    END BtoMStr;

  VAR
    result : ZERR;
    db : DosLibraryPtr;
    dr : RootNodePtr;
    di : DosInfoPtr;
    dl : DeviceListPtr;
  BEGIN
    EmptyTable;
    SpinBusyPointer(ZWindowPtr,TRUE);
    Forbid;
    db := DOSBase;
    dr := db^.dlRoot;
    di := APTR(dr^.rnInfo);
    dl := APTR(di^.diDevInfo);
    LOOP
      IF (dl = NIL) THEN  EXIT  END;
      IF ((dl^.dlType = DLTDevice) OR (dl^.dlType = DLTDirectory))
          AND (dl^.dlTask # NIL) THEN
        result := AllocEntry(tLen+1);
        IF (result # ZOK) THEN  EXIT  END;
        INC(tLen);
        WITH Table[tLen]^ DO
          BtoMStr(dl^.dlName,diName);
          ConcatString(diName,":");
          diIsDir := TRUE;
          diProt := ProtectionSet{};
          diSelected := FALSE;  diWasSelected := FALSE;
          diSize := 0
        END
      END;
      dl := APTR(dl^.dlNext)
    END;
    Permit;
    FreeEntries(tLen+1,AbsoluteMaxFiles);
    HeapSort(tLen);
    UpdateSlider;
    RETURN result
  END LoadTableWithDevices;

PROCEDURE FreeTable;
  BEGIN
    EmptyTable;
    FreeEntries(1,AbsoluteMaxFiles)
  END FreeTable;


(* ***** Display routines ***** *)

PROCEDURE DisplayTable(forced : BOOLEAN);
  VAR i : LINE;
  BEGIN
    IF forced OR (tFirst # SliderIndex()) THEN
      IF (ZapCount > 0) THEN
        HeapSort(tLen);
        DEC(tLen,ZapCount);
        FreeEntries(tLen+1,AbsoluteMaxFiles);
        UpdateSlider;
        ZapCount := 0
      END;
      tFirst := SliderIndex();
      FOR i := 0 TO tHeight-1 DO  DisplayALine(i)  END
    END
  END DisplayTable;

PROCEDURE SetupDragSelect(x,y : INTEGER);
  BEGIN
    DragStart := Y2Line(y) + tFirst;
    DragPrev := DragStart
  END SetupDragSelect;

PROCEDURE DoDragSelect(x,y : INTEGER);

  PROCEDURE RangeSelect(i : INDEX);
    VAR inc : INTEGER;
    BEGIN
      inc := 1;
      IF (i > DragStart) THEN  inc := -1  END;
      LOOP
        Select(Table[i],Toggle);
        IF (i = DragStart) THEN  EXIT  END;
        INC(i,inc)
      END
    END RangeSelect;

  VAR
    i,line : LINE;
    index,newfirst : INDEX;
  BEGIN
    line := Y2Line(y);
    newfirst := tFirst;
    IF (line < 0) THEN
      IF (newfirst > 1) THEN  DEC(newfirst)  END;
      index := newfirst
    ELSIF (line >= tHeight) THEN
      IF (newfirst <= tRange) THEN  INC(newfirst)  END;
      index := newfirst + tHeight - 1
    ELSE
      index := newfirst + line
    END;
    IF (index > tLen) THEN  index := tLen  END;
    IF (index # DragPrev) THEN
      RangeSelect(DragPrev);
      RangeSelect(index);
      DragPrev := index;
      IF (newfirst # tFirst) THEN
        tFirst := newfirst;
        UpdateSlider
      END;
      FOR i := 0 TO tHeight-1 DO  DisplayALine(i)  END
    END
  END DoDragSelect;

VAR i : INDEX;
BEGIN  (* Module initialization *)
  EmptyTable;
  FOR i := 1 TO AbsoluteMaxFiles DO  Table[i] := NIL  END
END ZTable.
