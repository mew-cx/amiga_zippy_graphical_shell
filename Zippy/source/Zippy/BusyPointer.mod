IMPLEMENTATION MODULE BusyPointer;  (* MEW 881102 *)

(* Written in Benchmark Modula-2 *)
(* by Michael Weiblen   PLink: EKIM   CIS: 72506,2072 *)

FROM SYSTEM IMPORT
    ADR;
FROM Intuition IMPORT
    WindowPtr, SetPointer, ClearPointer;
FROM Views IMPORT
    ViewPortPtr, SetRGB4, GetRGB4;
FROM Memory IMPORT
    AllocMem, FreeMem, MemReqSet, MemChip, MemClear;


TYPE
  PDATA = ARRAY [0..14] OF LONGCARD;

VAR
  BP : POINTER TO ARRAY [0..3] OF PDATA;
  NextBP : INTEGER;
  BusyPointerOn : BOOLEAN;
  r17,g17,b17,r18,g18,b18,r19,g19,b19 : CARDINAL;


(* Internal routines *)

PROCEDURE SetColors(w : WindowPtr);
  VAR vpp : ViewPortPtr;
  BEGIN
    vpp := ADR(w^.WScreen^.ViewPort);
    IF BusyPointerOn THEN
      SetRGB4(vpp^,17,10,10,10);
      SetRGB4(vpp^,18,12,0,0);
      SetRGB4(vpp^,19,15,15,15)
    ELSE
      SetRGB4(vpp^,17,r17,g17,b17);
      SetRGB4(vpp^,18,r18,g18,b18);
      SetRGB4(vpp^,19,r19,g19,b19)
    END
  END SetColors;

PROCEDURE GetColors(w : WindowPtr; entry : INTEGER; VAR r,g,b : CARDINAL);
  VAR c : LONGCARD;
  BEGIN
    c := GetRGB4(w^.WScreen^.ViewPort.ColorMap^,entry);
    r := CARDINAL((c DIV 256D) MOD 16D);
    g := CARDINAL((c DIV 16D) MOD 16D);
    b := CARDINAL(c MOD 16D);
  END GetColors;


(* External routines *)

PROCEDURE AllocBusyPointers(wp : WindowPtr) : BOOLEAN;
  VAR i : CARDINAL;
  BEGIN
    IF (wp = NIL) THEN  RETURN FALSE  END;
    BP := AllocMem(SIZE(BP^),MemReqSet{MemChip,MemClear});
    IF (BP = NIL) THEN  RETURN FALSE  END;

    GetColors(wp,17,r17,g17,b17);
    GetColors(wp,18,r18,g18,b18);
    GetColors(wp,19,r19,g19,b19);

    BP^[0][1] := 001800600H;  BP^[0][2] := 0066019E0H;
    BP^[0][3] := 026601BF0H;  BP^[0][4] := 0199067F0H;
    BP^[0][5] := 019986FF8H;  BP^[0][6] := 066609FF8H;
    BP^[0][7] := 066609FF8H;  BP^[0][8] := 019986FF8H;
    BP^[0][9] := 019906FF0H;  BP^[0][10] := 026601FF0H;
    BP^[0][11] := 006601FE0H;  BP^[0][12] := 009800780H;

    BP^[1][1] := 004800300H;  BP^[1][2] := 013200DE0H;
    BP^[1][3] := 033300FF0H;  BP^[1][4] := 04CC037F0H;
    BP^[1][5] := 04CC83FF8H;  BP^[1][6] := 03330CFF8H;
    BP^[1][7] := 03330CFF8H;  BP^[1][8] := 04CC83FF8H;
    BP^[1][9] := 04CC03FF0H;  BP^[1][10] := 033300FF0H;
    BP^[1][11] := 013200FE0H;  BP^[1][12] := 00C800380H;

    BP^[2][1] := 006000180H;  BP^[2][2] := 0198007E0H;
    BP^[2][3] := 0199027F0H;  BP^[2][4] := 066601FF0H;
    BP^[2][5] := 066601FF8H;  BP^[2][6] := 099986FF8H;
    BP^[2][7] := 099986FF8H;  BP^[2][8] := 066601FF8H;
    BP^[2][9] := 066601FF0H;  BP^[2][10] := 0199027F0H;
    BP^[2][11] := 0198007E0H;  BP^[2][12] := 006000980H;

    BP^[3][1] := 003000480H;  BP^[3][2] := 00CC013E0H;
    BP^[3][3] := 00CC033F0H;  BP^[3][4] := 033304FF0H;
    BP^[3][5] := 033304FF8H;  BP^[3][6] := 0CCC83FF8H;
    BP^[3][7] := 0CCC83FF8H;  BP^[3][8] := 033304FF8H;
    BP^[3][9] := 033304FF0H;  BP^[3][10] := 00CC037F0H;
    BP^[3][11] := 00CC013E0H;  BP^[3][12] := 003000D80H;

    RETURN TRUE
  END AllocBusyPointers;

PROCEDURE FreeBusyPointers;
  BEGIN
    IF (BP # NIL) THEN  FreeMem(BP,SIZE(BP^))  END;
    BP := NIL
  END FreeBusyPointers;

PROCEDURE SpinBusyPointer(wp : WindowPtr; right : BOOLEAN);
  BEGIN
    IF (wp = NIL) OR (BP = NIL) THEN  RETURN  END;
    IF NOT BusyPointerOn THEN
      BusyPointerOn := TRUE;
      SetColors(wp)
    END;
    IF right THEN  INC(NextBP)  ELSE  DEC(NextBP)  END;
    IF (NextBP < 0) THEN  NextBP := 3  END;
    IF (NextBP > 3) THEN  NextBP := 0  END;
    SetPointer(wp^,ADR(BP^[NextBP]),13,12,-6,-5);
  END SpinBusyPointer;

PROCEDURE RestorePointer(wp : WindowPtr);
  BEGIN
    IF (wp # NIL) AND BusyPointerOn THEN
      BusyPointerOn := FALSE;
      SetColors(wp);
      ClearPointer(wp^)
    END
  END RestorePointer;

BEGIN
  BP := NIL;  NextBP := 0;  BusyPointerOn := FALSE
END BusyPointer.
