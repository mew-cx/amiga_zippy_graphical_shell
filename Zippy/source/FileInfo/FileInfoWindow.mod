IMPLEMENTATION MODULE FileInfoWindow;  (* MEW 881027 *)

(* Originally produced by PowerWindows2.5; hand edited to cut the fluff *)

FROM SYSTEM IMPORT ADR;
FROM Intuition IMPORT IntuiTextPtr, BorderPtr, StringInfoPtr,
 GadgetActivation, GadgetActivationSet, GadgetFlags, GadgetFlagsSet,
 IDCMPFlags, IDCMPFlagsSet, WindowFlags, WindowFlagsSet,
 WBenchScreen, StrGadget, BoolGadget, GadgetMutualExcludeSet,
 GadgetPtr, RememberPtr, NewWindowPtr;
FROM Preferences IMPORT TopazEighty;
FROM PW2 IMPORT PW2RememberPtr,
 MakeNewWindowStructure, MakeBorderStructure, MakeStringInfoStructure,
 MakeGadgetStructure, MakeITextStructure, MakeTextAttrStructure,
 PW2EnoughMemory;
FROM Rasters IMPORT Jam1, Jam2;
FROM Text IMPORT TextAttrPtr, FontStyleSet, FontFlagsSet;

PROCEDURE FileInfoWindow() : BOOLEAN;
BEGIN
	PW2EnoughMemory := TRUE;
	PW2RememberPtr := ADR(FileInfoWindowRememberPtr);
	BorVec1[0] := 0;
	BorVec1[1] := 0;
	BorVec1[2] := 49;
	BorVec1[3] := 0;
	BorVec1[4] := 49;
	BorVec1[5] := 12;
	BorVec1[6] := 0;
	BorVec1[7] := 12;
	BorVec1[8] := 0;
	BorVec1[9] := 0;
Border1 := MakeBorderStructure(
	-1,-1,	(* XY origin relative to container TopLeft *)
	3,0,Jam1,	(* front pen, back pen and drawmode *)
	5,	(* number of XY vectors *)
	ADR(BorVec1),	(* pointer to XY vectors *)
	NIL	(* next border in list *)
	);
TOPAZ80 := MakeTextAttrStructure(
	ADR("topaz.font"),TopazEighty,FontStyleSet{},FontFlagsSet{}
	);
IText1 := MakeITextStructure(
	3,0,Jam2,	(* front and back text pens and drawmode *)
	8,2,	(* XY origin relative to container TopLeft *)
	TOPAZ80,	(* font pointer or NULL for default *)
	ADR("Quit"),	(* pointer to text *)
	NIL	(* next IntuiText structure *)
	);
QuitGad := MakeGadgetStructure(
	NIL,	(* next gadget *)
	280,75,	(* origin XY of hit box relative to window TopLeft *)
	48,11,	(* hit box width and height *)
	GadgetFlagsSet{},	(* gadget flags *)
	GadgetActivationSet{RelVerify},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Border1,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	IText1,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	1,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
IText2 := MakeITextStructure(
	3,0,Jam2,	(* front and back text pens and drawmode *)
	8,2,	(* XY origin relative to container TopLeft *)
	TOPAZ80,	(* font pointer or NULL for default *)
	ADR("Save"),	(* pointer to text *)
	NIL	(* next IntuiText structure *)
	);
SaveGad := MakeGadgetStructure(
	QuitGad,	(* next gadget *)
	10,75,	(* origin XY of hit box relative to window TopLeft *)
	48,11,	(* hit box width and height *)
	GadgetFlagsSet{},	(* gadget flags *)
	GadgetActivationSet{RelVerify},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Border1,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	IText2,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	1,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
	BorVec3[0] := 0;
	BorVec3[1] := 0;
	BorVec3[2] := 73;
	BorVec3[3] := 0;
	BorVec3[4] := 73;
	BorVec3[5] := 12;
	BorVec3[6] := 0;
	BorVec3[7] := 12;
	BorVec3[8] := 0;
	BorVec3[9] := 0;
Border3 := MakeBorderStructure(
	-1,-1,	(* XY origin relative to container TopLeft *)
	1,0,Jam1,	(* front pen, back pen and drawmode *)
	5,	(* number of XY vectors *)
	ADR(BorVec3),	(* pointer to XY vectors *)
	NIL	(* next border in list *)
	);
IText3 := MakeITextStructure(
	1,0,Jam1,	(* front and back text pens and drawmode *)
	20,2,	(* XY origin relative to container TopLeft *)
	TOPAZ80,	(* font pointer or NULL for default *)
	ADR("Pure"),	(* pointer to text *)
	NIL	(* next IntuiText structure *)
	);
ProtGad[5] := MakeGadgetStructure(
	SaveGad,	(* next gadget *)
	92,57,	(* origin XY of hit box relative to window TopLeft *)
	72,11,	(* hit box width and height *)
	GadgetFlagsSet{},	(* gadget flags *)
	GadgetActivationSet{ToggleSelect},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Border3,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	IText3,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	1,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
IText4 := MakeITextStructure(
	1,0,Jam1,	(* front and back text pens and drawmode *)
	12,2,	(* XY origin relative to container TopLeft *)
	TOPAZ80,	(* font pointer or NULL for default *)
	ADR("Hidden"),	(* pointer to text *)
	NIL	(* next IntuiText structure *)
	);
ProtGad[7] := MakeGadgetStructure(
	ProtGad[5],	(* next gadget *)
	256,57,	(* origin XY of hit box relative to window TopLeft *)
	72,11,	(* hit box width and height *)
	GadgetFlagsSet{},	(* gadget flags *)
	GadgetActivationSet{ToggleSelect},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Border3,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	IText4,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	1,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
IText5 := MakeITextStructure(
	1,0,Jam1,	(* front and back text pens and drawmode *)
	12,2,	(* XY origin relative to container TopLeft *)
	TOPAZ80,	(* font pointer or NULL for default *)
	ADR("Script"),	(* pointer to text *)
	NIL	(* next IntuiText structure *)
	);
ProtGad[6] := MakeGadgetStructure(
	ProtGad[7],	(* next gadget *)
	174,57,	(* origin XY of hit box relative to window TopLeft *)
	72,11,	(* hit box width and height *)
	GadgetFlagsSet{},	(* gadget flags *)
	GadgetActivationSet{ToggleSelect},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Border3,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	IText5,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	1,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
IText6 := MakeITextStructure(
	1,0,Jam1,	(* front and back text pens and drawmode *)
	8,2,	(* XY origin relative to container TopLeft *)
	TOPAZ80,	(* font pointer or NULL for default *)
	ADR("Archive"),	(* pointer to text *)
	NIL	(* next IntuiText structure *)
	);
ProtGad[4] := MakeGadgetStructure(
	ProtGad[6],	(* next gadget *)
	10,57,	(* origin XY of hit box relative to window TopLeft *)
	72,11,	(* hit box width and height *)
	GadgetFlagsSet{},	(* gadget flags *)
	GadgetActivationSet{ToggleSelect},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Border3,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	IText6,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	1,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
IText7 := MakeITextStructure(
	1,0,Jam1,	(* front and back text pens and drawmode *)
	12,2,	(* XY origin relative to container TopLeft *)
	TOPAZ80,	(* font pointer or NULL for default *)
	ADR("Delete"),	(* pointer to text *)
	NIL	(* next IntuiText structure *)
	);
ProtGad[0] := MakeGadgetStructure(
	ProtGad[4],	(* next gadget *)
	256,42,	(* origin XY of hit box relative to window TopLeft *)
	72,11,	(* hit box width and height *)
	GadgetFlagsSet{},	(* gadget flags *)
	GadgetActivationSet{ToggleSelect},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Border3,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	IText7,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	1,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
IText8 := MakeITextStructure(
	1,0,Jam1,	(* front and back text pens and drawmode *)
	8,2,	(* XY origin relative to container TopLeft *)
	TOPAZ80,	(* font pointer or NULL for default *)
	ADR("Execute"),	(* pointer to text *)
	NIL	(* next IntuiText structure *)
	);
ProtGad[1] := MakeGadgetStructure(
	ProtGad[0],	(* next gadget *)
	174,42,	(* origin XY of hit box relative to window TopLeft *)
	72,11,	(* hit box width and height *)
	GadgetFlagsSet{},	(* gadget flags *)
	GadgetActivationSet{ToggleSelect},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Border3,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	IText8,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	1,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
IText9 := MakeITextStructure(
	1,0,Jam1,	(* front and back text pens and drawmode *)
	16,2,	(* XY origin relative to container TopLeft *)
	TOPAZ80,	(* font pointer or NULL for default *)
	ADR("Write"),	(* pointer to text *)
	NIL	(* next IntuiText structure *)
	);
ProtGad[2] := MakeGadgetStructure(
	ProtGad[1],	(* next gadget *)
	92,42,	(* origin XY of hit box relative to window TopLeft *)
	72,11,	(* hit box width and height *)
	GadgetFlagsSet{},	(* gadget flags *)
	GadgetActivationSet{ToggleSelect},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Border3,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	IText9,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	1,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
IText10 := MakeITextStructure(
	1,0,Jam1,	(* front and back text pens and drawmode *)
	20,2,	(* XY origin relative to container TopLeft *)
	TOPAZ80,	(* font pointer or NULL for default *)
	ADR("Read"),	(* pointer to text *)
	NIL	(* next IntuiText structure *)
	);
ProtGad[3] := MakeGadgetStructure(
	ProtGad[2],	(* next gadget *)
	10,42,	(* origin XY of hit box relative to window TopLeft *)
	72,11,	(* hit box width and height *)
	GadgetFlagsSet{},	(* gadget flags *)
	GadgetActivationSet{ToggleSelect},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Border3,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	IText10,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	1,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
NoteGadSInfo := MakeStringInfoStructure(
	ADR(NoteGadSIBuff),	(* buffer where text will be edited *)
	NIL,	(* optional undo buffer *)
	0,	(* character position in buffer *)
	81,	(* maximum number of characters to allow *)
	0,	(* first displayed character buffer position *)
	0,	(* initial value for integer gadgets *)
	NIL	(* alternate keymap (fill in if you set the flag) *)
	);
	BorVec11[0] := 0;
	BorVec11[1] := 0;
	BorVec11[2] := 270;
	BorVec11[3] := 0;
	BorVec11[4] := 270;
	BorVec11[5] := 10;
	BorVec11[6] := 0;
	BorVec11[7] := 10;
	BorVec11[8] := 0;
	BorVec11[9] := 1;
Border11 := MakeBorderStructure(
	-4,-2,	(* XY origin relative to container TopLeft *)
	2,0,Jam1,	(* front pen, back pen and drawmode *)
	5,	(* number of XY vectors *)
	ADR(BorVec11),	(* pointer to XY vectors *)
	NIL	(* next border in list *)
	);
IText11 := MakeITextStructure(
	3,0,Jam2,	(* front and back text pens and drawmode *)
	-51,0,	(* XY origin relative to container TopLeft *)
	NIL,	(* font pointer or NULL for default *)
	ADR("Note:"),	(* pointer to text *)
	NIL	(* next IntuiText structure *)
	);
NoteGad := MakeGadgetStructure(
	ProtGad[3],	(* next gadget *)
	60,28,	(* origin XY of hit box relative to window TopLeft *)
	264,8,	(* hit box width and height *)
	GadgetFlagsSet{},	(* gadget flags *)
	GadgetActivationSet{RelVerify},	(* activation flags *)
	StrGadget,	(* gadget type flags *)
	Border11,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	IText11,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NoteGadSInfo,	(* SpecialInfo structure *)
	1,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
NameGadSInfo := MakeStringInfoStructure(
	ADR(NameGadSIBuff),	(* buffer where text will be edited *)
	NIL,	(* optional undo buffer *)
	0,	(* character position in buffer *)
	32,	(* maximum number of characters to allow *)
	0,	(* first displayed character buffer position *)
	0,	(* initial value for integer gadgets *)
	NIL	(* alternate keymap (fill in if you set the flag) *)
	);
IText12 := MakeITextStructure(
	3,0,Jam2,	(* front and back text pens and drawmode *)
	-51,0,	(* XY origin relative to container TopLeft *)
	NIL,	(* font pointer or NULL for default *)
	ADR("Name:"),	(* pointer to text *)
	NIL	(* next IntuiText structure *)
	);
NameGad := MakeGadgetStructure(
	NoteGad,	(* next gadget *)
	60,14,	(* origin XY of hit box relative to window TopLeft *)
	264,8,	(* hit box width and height *)
	GadgetFlagsSet{},	(* gadget flags *)
	GadgetActivationSet{RelVerify},	(* activation flags *)
	StrGadget,	(* gadget type flags *)
	Border11,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	IText12,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NameGadSInfo,	(* SpecialInfo structure *)
	1,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
NewWindowStructure1 := MakeNewWindowStructure(
	154,29,	(* window XY origin relative to TopLeft of screen *)
	340,91,	(* window width and height *)
	0,1,	(* detail and block pens *)
	IDCMPFlagsSet{GadgetUp},	(* IDCMP flags *)
	WindowFlagsSet{WindowDrag,WindowDepth,Activate,NoCareRefresh},	(* other window flags *)
	NameGad,	(* first gadget in gadget list *)
	NIL,	(* custom CHECKMARK imagery *)
	NIL,	(* window title *)
	NIL,	(* custom screen pointer *)
	NIL,	(* custom bitmap *)
	5,5,	(* minimum width and height *)
	-1,-1,	(* maximum width and height *)
	WBenchScreen	(* destination screen type *)
	);

	RETURN (PW2EnoughMemory);
END FileInfoWindow;


(* end of PowerWindows IMPLEMENTATION generation *)
END FileInfoWindow.
