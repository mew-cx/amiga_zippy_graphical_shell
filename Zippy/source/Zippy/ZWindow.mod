IMPLEMENTATION MODULE ZWindow;
FROM SYSTEM IMPORT ADDRESS, ADR, INLINE, REG;
FROM Intuition IMPORT MenuPtr, MenuItemPtr, GadgetPtr, IntuiTextPtr,
 BorderPtr, ImagePtr, RememberPtr, NewWindow, PropInfoPtr, StringInfoPtr,
 ScreenFlags, ScreenFlagsSet, GadgetActivation, GadgetActivationSet,
 GadgetFlags, GadgetFlagsSet, IDCMPFlags, IDCMPFlagsSet, WindowFlags,
 WindowFlagsSet, PropInfoFlags, PropInfoFlagsSet, MenuFlags, MenuFlagsSet,
 MenuItemFlags, MenuItemFlagsSet, WBenchScreen, StrGadget, PropGadget,
 BoolGadget, GadgetMutualExcludeSet, MenuItemMutualExcludeSet, NewWindowPtr;
FROM PW2 IMPORT PW2RememberPtr, MakeNewWindowStructure,
 MakeBorderStructure, MakeImageStructure, MakePropInfoStructure,
 MakeStringInfoStructure, MakeGadgetStructure, MakeITextStructure,
 PW2EnoughMemory, MakeMenuStructure, MakeMenuItemStructure;
FROM Rasters IMPORT Jam1, Jam2;

PROCEDURE ZWindowImages();
CONST
	String = "PW2BM2 Image Data";
BEGIN

	INLINE (04DFAH,00006H); (* LEA 6(PC),A6 *)
	INLINE (06000H,0002EH); (* BRA end of the Image *)
	INLINE (0FFFFH,0F800H,08000H,01800H,08FF8H,01800H,08C18H,01800H);
	INLINE (08CFFH,01800H,08CFFH,01800H,08FFFH,01800H,080FFH,01800H);
	INLINE (080FFH,01800H,08000H,01800H,0FFFFH,0F800H);
ImgDat1 := REG(14);

	INLINE (04DFAH,00006H); (* LEA 6(PC),A6 *)
	INLINE (06000H,0002EH); (* BRA end of the Image *)
	INLINE (0FFFFH,0F000H,08000H,01000H,087FEH,01000H,087E6H,01000H);
	INLINE (087C6H,01000H,08786H,01000H,08706H,01000H,08606H,01000H);
	INLINE (087FEH,01000H,08000H,01000H,0FFFFH,0F000H);
ImgDat2 := REG(14);

	INLINE (04DFAH,00006H); (* LEA 6(PC),A6 *)
	INLINE (06000H,0002EH); (* BRA end of the Image *)
	INLINE (0FFFFH,0F000H,08000H,01000H,087FEH,01000H,087FEH,01000H);
	INLINE (087FEH,01000H,087FEH,01000H,087FEH,01000H,087FEH,01000H);
	INLINE (087FEH,01000H,08000H,01000H,0FFFFH,0F000H);
ImgDat3 := REG(14);

	INLINE (04DFAH,00006H); (* LEA 6(PC),A6 *)
	INLINE (06000H,0002EH); (* BRA end of the Image *)
	INLINE (0FFFFH,0F800H,0C000H,00800H,0C3FFH,00800H,0C303H,00800H);
	INLINE (0C303H,00800H,0C303H,00800H,0C303H,00800H,0C303H,00800H);
	INLINE (0C3FFH,00800H,0C000H,00800H,0FFFFH,0F800H);
ImgDat4 := REG(14);

	INLINE (04DFAH,00006H); (* LEA 6(PC),A6 *)
	INLINE (06000H,0002EH); (* BRA end of the Image *)
	INLINE (0FFFFH,0F800H,08000H,01800H,08060H,01800H,08060H,01800H);
	INLINE (08060H,01800H,08060H,01800H,081F8H,01800H,080F0H,01800H);
	INLINE (08060H,01800H,08000H,01800H,0FFFFH,0F800H);
ImgDat5 := REG(14);

	INLINE (04DFAH,00006H); (* LEA 6(PC),A6 *)
	INLINE (06000H,0002EH); (* BRA end of the Image *)
	INLINE (0FFFFH,0F000H,08000H,01000H,08060H,01000H,080F0H,01000H);
	INLINE (081F8H,01000H,08060H,01000H,081F8H,01000H,080F0H,01000H);
	INLINE (08060H,01000H,08000H,01000H,0FFFFH,0F000H);
ImgDat6 := REG(14);

	INLINE (04DFAH,00006H); (* LEA 6(PC),A6 *)
	INLINE (06000H,0002EH); (* BRA end of the Image *)
	INLINE (0FFFFH,0F000H,08000H,01000H,0800CH,01000H,08018H,01000H);
	INLINE (08030H,01000H,08060H,01000H,080C0H,01000H,08180H,01000H);
	INLINE (08300H,01000H,08000H,01000H,0FFFFH,0F000H);
ImgDat7 := REG(14);

	INLINE (04DFAH,00006H); (* LEA 6(PC),A6 *)
	INLINE (06000H,0002EH); (* BRA end of the Image *)
	INLINE (0FFFFH,0F800H,0C000H,00800H,0C000H,00800H,0C078H,00800H);
	INLINE (0C078H,00800H,0C000H,00800H,0C078H,00800H,0C078H,00800H);
	INLINE (0C000H,00800H,0C000H,00800H,0FFFFH,0F800H);
ImgDat8 := REG(14);

	INLINE (04DFAH,00006H); (* LEA 6(PC),A6 *)
	INLINE (06000H,0002EH); (* BRA end of the Image *)
	INLINE (0FFFFH,0F800H,08000H,01800H,09FFEH,01800H,0990BH,01800H);
	INLINE (099F9H,09800H,09801H,09800H,09801H,09800H,09801H,09800H);
	INLINE (09FFFH,09800H,08000H,01800H,0FFFFH,0F800H);
ImgDat9 := REG(14);

	INLINE (04DFAH,00006H); (* LEA 6(PC),A6 *)
	INLINE (06000H,0002EH); (* BRA end of the Image *)
	INLINE (0FFFFH,0F800H,0C000H,00800H,0C030H,00800H,0C333H,00800H);
	INLINE (0C0CCH,00800H,0CF03H,0C800H,0C0CCH,00800H,0C333H,00800H);
	INLINE (0C030H,00800H,0C000H,00800H,0FFFFH,0F800H);
ImgDat10 := REG(14);
END ZWindowImages;  (* end of PowerWindows IMAGES generation *)

PROCEDURE ZWindow() : BOOLEAN;
BEGIN
	ZWindowImages; (* name all the Imagery *)
	PW2EnoughMemory := TRUE;
	PW2RememberPtr := ADR(ZWindowRememberPtr);
Image1 := MakeImageStructure(
	0,0,	(* XY origin relative to container TopLeft *)
	21,11,	(* Image width and height in pixels *)
	1,	(* number of bitplanes in Image *)
	ImgDat1,	(* pointer to ImageData *)
	00001H,00000H,	(* PlanePick and PlaneOnOff *)
	NIL	(* next Image structure *)
	);
RestoreGad := MakeGadgetStructure(
	NIL,	(* next gadget *)
	199,41,	(* origin XY of hit box relative to window TopLeft *)
	21,11,	(* hit box width and height *)
	GadgetFlagsSet{GadgImage},	(* gadget flags *)
	GadgetActivationSet{RelVerify},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Image1,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	NIL,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	0,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
Image2 := MakeImageStructure(
	0,0,	(* XY origin relative to container TopLeft *)
	20,11,	(* Image width and height in pixels *)
	1,	(* number of bitplanes in Image *)
	ImgDat2,	(* pointer to ImageData *)
	00001H,00000H,	(* PlanePick and PlaneOnOff *)
	NIL	(* next Image structure *)
	);
ReverseGad := MakeGadgetStructure(
	RestoreGad,	(* next gadget *)
	179,41,	(* origin XY of hit box relative to window TopLeft *)
	20,11,	(* hit box width and height *)
	GadgetFlagsSet{GadgImage},	(* gadget flags *)
	GadgetActivationSet{RelVerify},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Image2,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	NIL,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	0,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
Image3 := MakeImageStructure(
	0,0,	(* XY origin relative to container TopLeft *)
	20,11,	(* Image width and height in pixels *)
	1,	(* number of bitplanes in Image *)
	ImgDat3,	(* pointer to ImageData *)
	00001H,00000H,	(* PlanePick and PlaneOnOff *)
	NIL	(* next Image structure *)
	);
AllGad := MakeGadgetStructure(
	ReverseGad,	(* next gadget *)
	159,41,	(* origin XY of hit box relative to window TopLeft *)
	20,11,	(* hit box width and height *)
	GadgetFlagsSet{GadgImage},	(* gadget flags *)
	GadgetActivationSet{RelVerify},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Image3,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	NIL,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	0,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
Image4 := MakeImageStructure(
	0,0,	(* XY origin relative to container TopLeft *)
	21,11,	(* Image width and height in pixels *)
	1,	(* number of bitplanes in Image *)
	ImgDat4,	(* pointer to ImageData *)
	00001H,00000H,	(* PlanePick and PlaneOnOff *)
	NIL	(* next Image structure *)
	);
ClearGad := MakeGadgetStructure(
	AllGad,	(* next gadget *)
	138,41,	(* origin XY of hit box relative to window TopLeft *)
	21,11,	(* hit box width and height *)
	GadgetFlagsSet{GadgImage},	(* gadget flags *)
	GadgetActivationSet{RelVerify},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Image4,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	NIL,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	0,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
Image5 := MakeImageStructure(
	0,0,	(* XY origin relative to container TopLeft *)
	21,11,	(* Image width and height in pixels *)
	1,	(* number of bitplanes in Image *)
	ImgDat5,	(* pointer to ImageData *)
	00001H,00000H,	(* PlanePick and PlaneOnOff *)
	NIL	(* next Image structure *)
	);
PushGad := MakeGadgetStructure(
	ClearGad,	(* next gadget *)
	113,41,	(* origin XY of hit box relative to window TopLeft *)
	21,11,	(* hit box width and height *)
	GadgetFlagsSet{GadgImage},	(* gadget flags *)
	GadgetActivationSet{RelVerify},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Image5,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	NIL,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	0,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
Image6 := MakeImageStructure(
	0,0,	(* XY origin relative to container TopLeft *)
	20,11,	(* Image width and height in pixels *)
	1,	(* number of bitplanes in Image *)
	ImgDat6,	(* pointer to ImageData *)
	00001H,00000H,	(* PlanePick and PlaneOnOff *)
	NIL	(* next Image structure *)
	);
SwapGad := MakeGadgetStructure(
	PushGad,	(* next gadget *)
	93,41,	(* origin XY of hit box relative to window TopLeft *)
	20,11,	(* hit box width and height *)
	GadgetFlagsSet{GadgImage},	(* gadget flags *)
	GadgetActivationSet{RelVerify},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Image6,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	NIL,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	0,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
Image7 := MakeImageStructure(
	0,0,	(* XY origin relative to container TopLeft *)
	20,11,	(* Image width and height in pixels *)
	1,	(* number of bitplanes in Image *)
	ImgDat7,	(* pointer to ImageData *)
	00001H,00000H,	(* PlanePick and PlaneOnOff *)
	NIL	(* next Image structure *)
	);
ParentGad := MakeGadgetStructure(
	SwapGad,	(* next gadget *)
	73,41,	(* origin XY of hit box relative to window TopLeft *)
	20,11,	(* hit box width and height *)
	GadgetFlagsSet{GadgImage},	(* gadget flags *)
	GadgetActivationSet{RelVerify},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Image7,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	NIL,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	0,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
Image8 := MakeImageStructure(
	0,0,	(* XY origin relative to container TopLeft *)
	21,11,	(* Image width and height in pixels *)
	1,	(* number of bitplanes in Image *)
	ImgDat8,	(* pointer to ImageData *)
	00001H,00000H,	(* PlanePick and PlaneOnOff *)
	NIL	(* next Image structure *)
	);
RootGad := MakeGadgetStructure(
	ParentGad,	(* next gadget *)
	52,41,	(* origin XY of hit box relative to window TopLeft *)
	21,11,	(* hit box width and height *)
	GadgetFlagsSet{GadgImage},	(* gadget flags *)
	GadgetActivationSet{RelVerify},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Image8,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	NIL,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	0,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
Image9 := MakeImageStructure(
	0,0,	(* XY origin relative to container TopLeft *)
	21,11,	(* Image width and height in pixels *)
	1,	(* number of bitplanes in Image *)
	ImgDat9,	(* pointer to ImageData *)
	00001H,00000H,	(* PlanePick and PlaneOnOff *)
	NIL	(* next Image structure *)
	);
DeviceGad := MakeGadgetStructure(
	RootGad,	(* next gadget *)
	27,41,	(* origin XY of hit box relative to window TopLeft *)
	21,11,	(* hit box width and height *)
	GadgetFlagsSet{GadgImage},	(* gadget flags *)
	GadgetActivationSet{RelVerify},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Image9,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	NIL,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	0,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
Image10 := MakeImageStructure(
	0,0,	(* XY origin relative to container TopLeft *)
	21,11,	(* Image width and height in pixels *)
	1,	(* number of bitplanes in Image *)
	ImgDat10,	(* pointer to ImageData *)
	00001H,00000H,	(* PlanePick and PlaneOnOff *)
	NIL	(* next Image structure *)
	);
RefreshGad := MakeGadgetStructure(
	DeviceGad,	(* next gadget *)
	6,41,	(* origin XY of hit box relative to window TopLeft *)
	21,11,	(* hit box width and height *)
	GadgetFlagsSet{GadgImage},	(* gadget flags *)
	GadgetActivationSet{RelVerify},	(* activation flags *)
	BoolGadget,	(* gadget type flags *)
	Image10,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	NIL,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	NIL,	(* SpecialInfo structure *)
	0,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
SliderGadSInfo := MakePropInfoStructure(
	PropInfoFlagsSet{AutoKnob,FreeVert},	(* PROPINFO flags *)65535,0,	(* horizontal and vertical pot values *)65535,65535	(* horizontal and vertical body values *)
	);
Image11 := MakeImageStructure(
	0,0,	(* XY origin relative to container TopLeft *)
	8,108,	(* Image width and height in pixels *)
	0,	(* number of bitplanes in Image *)
	NIL,	(* pointer to ImageData *)
	00000H,00000H,	(* PlanePick and PlaneOnOff *)
	NIL	(* next Image structure *)
	);
SliderGad := MakeGadgetStructure(
	RefreshGad,	(* next gadget *)
	-15,53,	(* origin XY of hit box relative to window TopLeft *)
	16,-63,	(* hit box width and height *)
	GadgetFlagsSet{GRelRight,GRelHeight},	(* gadget flags *)
	GadgetActivationSet{RelVerify,GadgImmediate,RightBorder},	(* activation flags *)
	PropGadget,	(* gadget type flags *)
	Image11,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	NIL,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	SliderGadSInfo,	(* SpecialInfo structure *)
	0,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
DestGadSInfo := MakeStringInfoStructure(
	ADR(DestGadSIBuff),	(* buffer where text will be edited *)
	NIL,	(* optional undo buffer *)
	0,	(* character position in buffer *)
	256,	(* maximum number of characters to allow *)
	0,	(* first displayed character buffer position *)
	0,	(* initial value for integer gadgets *)
	NIL	(* alternate keymap (fill in if you set the flag) *)
	);
	BorVec1[0] := 0;
	BorVec1[1] := 0;
	BorVec1[2] := 640;
	BorVec1[3] := 0;
Border1 := MakeBorderStructure(
	-20,8,	(* XY origin relative to container TopLeft *)
	1,0,Jam1,	(* front pen, back pen and drawmode *)
	2,	(* number of XY vectors *)
	ADR(BorVec1),	(* pointer to XY vectors *)
	NIL	(* next border in list *)
	);
IText1 := MakeITextStructure(
	3,0,Jam1,	(* front and back text pens and drawmode *)
	-14,0,	(* XY origin relative to container TopLeft *)
	NIL,	(* font pointer or NULL for default *)
	ADR("D"),	(* pointer to text *)
	NIL	(* next IntuiText structure *)
	);
DestGad := MakeGadgetStructure(
	SliderGad,	(* next gadget *)
	20,21,	(* origin XY of hit box relative to window TopLeft *)
	-22,8,	(* hit box width and height *)
	GadgetFlagsSet{GRelWidth},	(* gadget flags *)
	GadgetActivationSet{RelVerify},	(* activation flags *)
	StrGadget,	(* gadget type flags *)
	Border1,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	IText1,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	DestGadSInfo,	(* SpecialInfo structure *)
	0,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
SourceGadSInfo := MakeStringInfoStructure(
	ADR(SourceGadSIBuff),	(* buffer where text will be edited *)
	NIL,	(* optional undo buffer *)
	0,	(* character position in buffer *)
	256,	(* maximum number of characters to allow *)
	0,	(* first displayed character buffer position *)
	0,	(* initial value for integer gadgets *)
	NIL	(* alternate keymap (fill in if you set the flag) *)
	);
	BorVec2[0] := 0;
	BorVec2[1] := 0;
	BorVec2[2] := 640;
	BorVec2[3] := 0;
Border2 := MakeBorderStructure(
	-20,8,	(* XY origin relative to container TopLeft *)
	1,0,Jam1,	(* front pen, back pen and drawmode *)
	2,	(* number of XY vectors *)
	ADR(BorVec2),	(* pointer to XY vectors *)
	NIL	(* next border in list *)
	);
IText2 := MakeITextStructure(
	3,0,Jam1,	(* front and back text pens and drawmode *)
	-14,0,	(* XY origin relative to container TopLeft *)
	NIL,	(* font pointer or NULL for default *)
	ADR("S"),	(* pointer to text *)
	NIL	(* next IntuiText structure *)
	);
SourceGad := MakeGadgetStructure(
	DestGad,	(* next gadget *)
	20,11,	(* origin XY of hit box relative to window TopLeft *)
	-22,8,	(* hit box width and height *)
	GadgetFlagsSet{GRelWidth},	(* gadget flags *)
	GadgetActivationSet{RelVerify},	(* activation flags *)
	StrGadget,	(* gadget type flags *)
	Border2,	(* gadget border or image to be rendered *)
	NIL,	(* alternate imagery for selection *)
	IText2,	(* first IntuiText structure *)
	GadgetMutualExcludeSet{0},	(* gadget mutual-exclude long word *)
	SourceGadSInfo,	(* SpecialInfo structure *)
	0,	(* user-definable data *)
	NIL	(* pointer to user-definable data *)
	);
NewWindowStructure1 := MakeNewWindowStructure(
	414,11,	(* window XY origin relative to TopLeft of screen *)
	226,175,	(* window width and height *)
	0,1,	(* detail and block pens *)
	IDCMPFlagsSet{NewSize,MouseButtons,GadgetDown,GadgetUp,MenuPick,Closewindow},	(* IDCMP flags *)
	WindowFlagsSet{WindowSizing,WindowDrag,WindowDepth,WindowClose,SizeBRight,Activate,NoCareRefresh},	(* other window flags *)
	SourceGad,	(* first gadget in gadget list *)
	NIL,	(* custom CHECKMARK imagery *)
	ADR("Zippy"),	(* window title *)
	NIL,	(* custom screen pointer *)
	NIL,	(* custom bitmap *)
	226,71,	(* minimum width and height *)
	9999,9999,	(* maximum width and height *)
	WBenchScreen	(* destination screen type *)
	);

	RETURN (PW2EnoughMemory);
END ZWindow;


(* end of PowerWindows IMPLEMENTATION generation *)
END ZWindow.
