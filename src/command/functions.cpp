/*
	*** Command Function Pointers
	*** src/command/functions.cpp
	Copyright T. Youngs 2007-2009

	This file is part of Aten.

	Aten is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Aten is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Aten.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "command/commands.h"

// Initialise Command Pointers
void Command::initPointers()
{
	/*
	// Store pointers to all command functions
	*/
	pointers_[NoFunction] = &function_NoFunction;
	pointers_[Joiner] = &function_Joiner;
	pointers_[Declarations] = &function_Declarations;

	// Analyse commands
	pointers_[Finalise] = &Command::function_Finalise;
	pointers_[FrameAnalyse] = &Command::function_FrameAnalyse;
	pointers_[Geometry] = &Command::function_Geometry;
	pointers_[ModelAnalyse] = &Command::function_ModelAnalyse;
	pointers_[PDens] = &Command::function_PDens;
	pointers_[PrintJobs] = &Command::function_PrintJobs;
	pointers_[RDF] = &Command::function_RDF;
	pointers_[SaveQuantities] = &Command::function_SaveQuantities;
	pointers_[TrajAnalyse] = &Command::function_TrajAnalyse;

	// Atom commands
	pointers_[AtomStyle] = &Command::function_AtomStyle;
	pointers_[GetAtom] = &Command::function_GetAtom;
	pointers_[Hide] = &Command::function_Hide;
	pointers_[SetCoords] = &Command::function_SetCoords;
	pointers_[SetCharge] = &Command::function_SetCharge;
	pointers_[SetElement] = &Command::function_SetElement;
	pointers_[SetForces] = &Command::function_SetForces;
	pointers_[SetFX] = &Command::function_SetFX;
	pointers_[SetFY] = &Command::function_SetFY;
	pointers_[SetFZ] = &Command::function_SetFZ;
	pointers_[SetId] = &Command::function_SetId;
	pointers_[SetRX] = &Command::function_SetRX;
	pointers_[SetRY] = &Command::function_SetRY;
	pointers_[SetRZ] = &Command::function_SetRZ;
	pointers_[SetVelocities] = &Command::function_SetVelocities;
	pointers_[SetVX] = &Command::function_SetVX;
	pointers_[SetVY] = &Command::function_SetVY;
	pointers_[SetVZ] = &Command::function_SetVZ;
	pointers_[Show] = &Command::function_Show;
	pointers_[ShowAll] = &Command::function_ShowAll;

	// Bond commands
	pointers_[Augment] = &Command::function_Augment;
	pointers_[BondTolerance] = &Command::function_BondTolerance;
	pointers_[ClearBonds] = &Command::function_ClearBonds;
	pointers_[ClearSelectedBonds] = &Command::function_ClearSelectedBonds;
	pointers_[NewBond] = &Command::function_NewBond;
	pointers_[NewBondId] = &Command::function_NewBondId;
	pointers_[ReBond] = &Command::function_ReBond;
	pointers_[ReBondPatterns] = &Command::function_ReBondPatterns;
	pointers_[ReBondSelection] = &Command::function_ReBondSelection;

	// Build commands
	pointers_[AddHydrogen] = &Command::function_AddHydrogen;
	pointers_[Bohr] = &Command::function_Bohr;
	pointers_[Chain] = &Command::function_Chain;
	pointers_[EndChain] = &Command::function_EndChain;
	pointers_[InsertAtom] = &Command::function_InsertAtom;
	pointers_[Locate] = &Command::function_Locate;
	pointers_[Move] = &Command::function_Move;
	pointers_[MoveToEnd] = &Command::function_MoveToEnd;
	pointers_[MoveToStart] = &Command::function_MoveToStart;
	pointers_[NewAtom] = &Command::function_NewAtom;
	pointers_[NewAtomFrac] = &Command::function_NewAtomFrac;
	pointers_[ReOrder] = &Command::function_ReOrder;
	pointers_[ResetPen] = &Command::function_ResetPen;
	pointers_[RotX] = &Command::function_RotX;
	pointers_[RotY] = &Command::function_RotY;
	pointers_[RotZ] = &Command::function_RotZ;
	pointers_[ShiftDown] = &Command::function_ShiftDown;
	pointers_[ShiftUp] = &Command::function_ShiftUp;
	pointers_[Transmute] = &Command::function_Transmute;

	// Cell commands
	pointers_[AddGenerator] = &Command::function_AddGenerator;
	pointers_[AdjustCell] = &Command::function_AdjustCell;
	pointers_[Cell] = &Command::function_Cell;
	pointers_[CellAxes] = &Command::function_CellAxes;
	pointers_[Fold] = &Command::function_Fold;
	pointers_[FoldMolecules] = &Command::function_FoldMolecules;
	pointers_[FracToReal] = &Command::function_FracToReal;
	pointers_[NoCell] = &Command::function_NoCell;
	pointers_[Pack] = &Command::function_Pack;
	pointers_[PrintCell] = &Command::function_PrintCell;
	pointers_[Replicate] = &Command::function_Replicate;
	pointers_[RotateCell] = &Command::function_RotateCell;
	pointers_[Scale] = &Command::function_Scale;
	pointers_[ScaleMolecules] = &Command::function_ScaleMolecules;
	pointers_[SetCell] = &Command::function_SetCell;
	pointers_[Spacegroup] = &Command::function_Spacegroup;

	// Charge commands
	pointers_[ChargeFF] = &Command::function_ChargeFF;
	pointers_[ChargeFromModel] = &Command::function_ChargeFromModel;
	pointers_[ChargePAtom] = &Command::function_ChargePAtom;
	pointers_[Charge] = &Command::function_Charge;
	pointers_[ChargeType] = &Command::function_ChargeType;
	pointers_[ClearCharges] = &Command::function_ClearCharges;

	// Colourscale commands
	pointers_[AddPoint] = &Command::function_AddPoint;
	pointers_[ClearPoints] = &Command::function_ClearPoints;
	pointers_[ListScales] = &Command::function_ListScales;
	pointers_[RemovePoint] = &Command::function_RemovePoint;
	pointers_[ScaleName] = &Command::function_ScaleName;
	pointers_[ScaleVisible] = &Command::function_ScaleVisible;
	pointers_[SetPoint] = &Command::function_SetPoint;
	pointers_[SetPointColour] = &Command::function_SetPointColour;
	pointers_[SetPointValue] = &Command::function_SetPointValue;

	// Disordered build commands
	pointers_[Disorder] = &Command::function_Disorder;
	pointers_[ListComponents] = &Command::function_ListComponents;
	pointers_[NMols] = &Command::function_NMols;
	pointers_[Region] = &Command::function_Region;
	pointers_[RegionCentre] = &Command::function_RegionCentre;
	pointers_[RegionCentreFrac] = &Command::function_RegionCentreFrac;
	pointers_[RegionFrac] = &Command::function_RegionFrac;
	pointers_[RegionGeometry] = &Command::function_RegionGeometry;
	pointers_[RegionGeometryFrac] = &Command::function_RegionGeometryFrac;
	pointers_[RegionOverlaps] = &Command::function_RegionOverlaps;
	pointers_[RegionShape] = &Command::function_RegionShape;
	pointers_[VdwScale] = &Command::function_VdwScale;

	// Edit Commands
	pointers_[Delete] = &Command::function_Delete;
	pointers_[Copy] = &Command::function_Copy;
	pointers_[Cut] = &Command::function_Cut;
	pointers_[Paste] = &Command::function_Paste;
	pointers_[Redo] = &Command::function_Redo;
	pointers_[Undo] = &Command::function_Undo;

	// Energy Commands
	pointers_[FrameEnergy] = &Command::function_FrameEnergy;
	pointers_[ModelEnergy] = &Command::function_ModelEnergy;
	pointers_[PrintElec] = &Command::function_PrintElec;
	pointers_[PrintEwald] = &Command::function_PrintEwald;
	pointers_[PrintInter] = &Command::function_PrintInter;
	pointers_[PrintIntra] = &Command::function_PrintIntra;
	pointers_[PrintEnergy] = &Command::function_PrintEnergy;
	pointers_[PrintSummary] = &Command::function_PrintSummary;
	pointers_[PrintVdw] = &Command::function_PrintVdw;

	// Flow control
	pointers_[If] = &function_If;
	pointers_[Break] = &Command::function_Break;
	pointers_[Continue] = &Command::function_Continue;
	pointers_[DoWhile] = &Command::function_DoWhile;
	pointers_[For] = &Command::function_For;
	pointers_[If] = &Command::function_If;
	pointers_[Return] = &Command::function_Return;
	pointers_[While] = &Command::function_While;

	// Force Commands
	pointers_[FrameForces] = &Command::function_FrameForces;
	pointers_[ModelForces] = &Command::function_ModelForces;
	pointers_[PrintForces] = &Command::function_PrintForces;

	// Forcefield Commands
        pointers_[AngleDef] = &Command::function_AngleDef;
	pointers_[BondDef] = &Command::function_BondDef;
	pointers_[ClearMap] = &Command::function_ClearMap;
	pointers_[CreateExpression] = &Command::function_CreateExpression;
	pointers_[DefaultFF] = &Command::function_DefaultFF;
	pointers_[Equivalent] = &Command::function_Equivalent;
	pointers_[FFModel] = &Command::function_FFModel;
	pointers_[FFPattern] = &Command::function_FFPattern;
	pointers_[FinaliseFF] = &Command::function_FinaliseFF;
	pointers_[GenConvert] = &Command::function_GenConvert;
	pointers_[Generator] = &Command::function_Generator;
	pointers_[GetFF] = &Command::function_GetFF;
	pointers_[InterDef] = &Command::function_InterDef;
	pointers_[LoadFF] = &Command::function_LoadFF;
	pointers_[Map] = &Command::function_Map;
	pointers_[NewFF] = &Command::function_NewFF;
	pointers_[PrintSetup] = &Command::function_PrintSetup;
	pointers_[Rules] = &Command::function_Rules;
	pointers_[SaveExpression] = &Command::function_SaveExpression;
	pointers_[TorsionDef] = &Command::function_TorsionDef;
	pointers_[TypeDef] = &Command::function_TypeDef;
	pointers_[TypeModel] = &Command::function_TypeModel;
	pointers_[TypeTest] = &Command::function_TypeTest;
	pointers_[Units] = &Command::function_Units;

	// Glyph commands
	pointers_[AutoEllipsoids] = &Command::function_AutoEllipsoids;
	pointers_[AutoPolyhedra] = &Command::function_AutoPolyhedra;
	pointers_[GlyphAtomF] = &Command::function_GlyphAtomF;
	pointers_[GlyphAtomR] = &Command::function_GlyphAtomR;
	pointers_[GlyphAtomV] = &Command::function_GlyphAtomV;
	pointers_[GlyphAtomsF] = &Command::function_GlyphAtomsF;
	pointers_[GlyphAtomsR] = &Command::function_GlyphAtomsR;
	pointers_[GlyphAtomsV] = &Command::function_GlyphAtomsV;
	pointers_[GlyphColour] = &Command::function_GlyphColour;
	pointers_[GlyphData] = &Command::function_GlyphData;
	pointers_[GlyphSolid] = &Command::function_GlyphSolid;
	pointers_[GlyphText] = &Command::function_GlyphText;
	pointers_[NewGlyph] = &Command::function_NewGlyph;

	// Grid Commands
	pointers_[AddGridPoint] = &Command::function_AddGridPoint;
	pointers_[AddNextGridPoint] = &Command::function_AddNextGridPoint;
	pointers_[FinaliseGrid] = &Command::function_FinaliseGrid;
	pointers_[GridAlpha] = &Command::function_GridAlpha;
	pointers_[GridAxes] = &Command::function_GridAxes;
	pointers_[GridColour] = &Command::function_GridColour;
	pointers_[GridColourNegative] = &Command::function_GridColourNegative;
	pointers_[GridColourscale] = &Command::function_GridColourscale;
	pointers_[GridCubic] = &Command::function_GridCubic;
	pointers_[GridCutoff] = &Command::function_GridCutoff;
	pointers_[GridLoopOrder] = &Command::function_GridLoopOrder;
	pointers_[GridOrigin] = &Command::function_GridOrigin;
	pointers_[GridOrtho] = &Command::function_GridOrtho;
	pointers_[GridSize] = &Command::function_GridSize;
	pointers_[GridStyle] = &Command::function_GridStyle;
	pointers_[GridSymmetric] = &Command::function_GridSymmetric;
	pointers_[GridUseZ] = &Command::function_GridUseZ;
	pointers_[LoadGrid] = &Command::function_LoadGrid;
	pointers_[NewGrid] = &Command::function_NewGrid;

	// Image Commands
	pointers_[SaveBitmap] = &Command::function_SaveBitmap;
	pointers_[SaveVector] = &Command::function_SaveVector;

	// Labeling Commands
	pointers_[ClearLabels] = &Command::function_ClearLabels;
	pointers_[Label] = &Command::function_Label;
	pointers_[RemoveLabel] = &Command::function_RemoveLabel;
	pointers_[RemoveLabels] = &Command::function_RemoveLabels;

	// MC Commands
	pointers_[MCAccept] = &Command::function_MCAccept;
	pointers_[MCAllow] = &Command::function_MCAllow;
	pointers_[MCMaxStep] = &Command::function_MCMaxStep;
	pointers_[MCNTrials] = &Command::function_MCNTrials;
	pointers_[PrintMC] = &Command::function_PrintMC;

	// Measurement Commands
	pointers_[Angle] = &Command::function_Angle;
	pointers_[Angles] = &Command::function_Angles;
	pointers_[ClearMeasurements] = &Command::function_ClearMeasurements;
	pointers_[Distance] = &Command::function_Distance;
	pointers_[Distances] = &Command::function_Distances;
	pointers_[ListMeasurements] = &Command::function_ListMeasurements;
	pointers_[Measure] = &Command::function_Measure;
	pointers_[Torsion] = &Command::function_Torsion;
	pointers_[Torsions] = &Command::function_Torsions;

	// Messaging Commands
	pointers_[Error] = &Command::function_Error;
	pointers_[Printf] = &Command::function_Printf;
	pointers_[Verbose] = &Command::function_Verbose;

	// Minimisation Commands
	pointers_[CGMinimise] = &Command::function_CGMinimise;
	pointers_[Converge] = &Command::function_Converge;
	pointers_[LineTolerance] = &Command::function_LineTolerance;
	pointers_[MCMinimise] = &Command::function_MCMinimise;
	pointers_[SDMinimise] = &Command::function_SDMinimise;
	pointers_[SimplexMinimise] = &Command::function_SimplexMinimise;
	
	// Model Commands
	pointers_[CreateAtoms] = &Command::function_CreateAtoms;
	pointers_[CurrentModel] = &Command::function_CurrentModel;
	pointers_[FinaliseModel] = &Command::function_FinaliseModel;
	pointers_[FirstModel] = &Command::function_FirstModel;
	pointers_[GetModel] = &Command::function_GetModel;
	pointers_[LastModel] = &Command::function_LastModel;
	pointers_[ListModels] = &Command::function_ListModels;
	pointers_[LoadModel] = &Command::function_LoadModel;
	pointers_[LogInfo] = &Command::function_LogInfo;
	pointers_[ModelTemplate] = &Command::function_ModelTemplate;
	pointers_[NewModel] = &Command::function_NewModel;
	pointers_[NextModel] = &Command::function_NextModel;
	pointers_[PrevModel] = &Command::function_PrevModel;
	pointers_[Info] = &Command::function_Info;
	pointers_[SaveModel] = &Command::function_SaveModel;
	pointers_[SetName] = &Command::function_SetName;

	// Pattern Commands
	pointers_[ClearPatterns] = &Command::function_ClearPatterns;
	pointers_[CreatePatterns] = &Command::function_CreatePatterns;
	pointers_[GetPattern] = &Command::function_GetPattern;
	pointers_[ListPatterns] = &Command::function_ListPatterns;
	pointers_[NewPattern] = &Command::function_NewPattern;

	// Preferences Commands
	pointers_[AngleLabel] = &Command::function_AngleLabel;
	pointers_[AtomDetail] = &Command::function_AtomDetail;
	pointers_[BondDetail] = &Command::function_BondDetail;
	pointers_[Colour] = &Command::function_Colour;
	pointers_[CommonElements] = &Command::function_CommonElements;
	pointers_[DensityUnits] = &Command::function_DensityUnits;
	pointers_[DistanceLabel] = &Command::function_DistanceLabel;
	pointers_[ECut] = &Command::function_ECut;
	pointers_[Elec] = &Command::function_Elec;
	pointers_[ElementAmbient] = &Command::function_ElementAmbient;
	pointers_[ElementDiffuse] = &Command::function_ElementDiffuse;
	pointers_[ElementRadius] = &Command::function_ElementRadius;
	pointers_[EnergyUnits] = &Command::function_EnergyUnits;
	pointers_[GL] = &Command::function_GL;
	pointers_[HDistance] = &Command::function_HDistance;
	pointers_[Intra] = &Command::function_Intra;
	pointers_[Key] = &Command::function_Key;
	pointers_[LabelSize] = &Command::function_LabelSize;
	pointers_[Light] = &Command::function_Light;
	pointers_[LightAmbient] = &Command::function_LightAmbient;
	pointers_[LightDiffuse] = &Command::function_LightDiffuse;
	pointers_[LightPosition] = &Command::function_LightPosition;
	pointers_[LightSpecular] = &Command::function_LightSpecular;
	pointers_[Mouse] = &Command::function_Mouse;
	pointers_[Radius] = &Command::function_Radius;
	pointers_[ReplicateFold] = &Command::function_ReplicateFold;
	pointers_[ReplicateTrim] = &Command::function_ReplicateTrim;
	pointers_[Scheme] = &Command::function_Scheme;
	pointers_[Shininess] = &Command::function_Shininess;
	pointers_[ShowOnScreen] = &Command::function_ShowOnScreen;
	pointers_[ShowOnImage] = &Command::function_ShowOnImage;
	pointers_[Style] = &Command::function_Style;
	pointers_[SwapBuffers] = &Command::function_SwapBuffers;
	pointers_[UseNiceText] = &Command::function_UseNiceText;
	pointers_[VCut] = &Command::function_VCut;
	pointers_[Vdw] = &Command::function_Vdw;
	pointers_[ZoomThrottle] = &Command::function_ZoomThrottle;

	// Read / Write Commands
	pointers_[AddReadOption] = &Command::function_AddReadOption;
	pointers_[FilterFileName] = &Command::function_FilterFileName;
	pointers_[Eof] = &Command::function_Eof;
	pointers_[Find] = &Command::function_Find;
	pointers_[GetLine] = &Command::function_GetLine;
	pointers_[PeekChar] = &Command::function_PeekChar;
	pointers_[PeekCharI] = &Command::function_PeekCharI;
	pointers_[ReadChars] = &Command::function_ReadChars;
	pointers_[ReadDouble] = &Command::function_ReadDouble;
	pointers_[ReadDoubleArray] = &Command::function_ReadDoubleArray;
	pointers_[ReadInteger] = &Command::function_ReadInteger;
	pointers_[ReadIntegerArray] = &Command::function_ReadIntegerArray;
	pointers_[ReadLine] = &Command::function_ReadLine;
	pointers_[ReadLineFormatted] = &Command::function_ReadLineFormatted;
	pointers_[ReadNext] = &Command::function_ReadNext;
	pointers_[ReadVariable] = &Command::function_ReadVariable;
	pointers_[ReadVariableFormatted] = &Command::function_ReadVariableFormatted;
	pointers_[RemoveReadOption] = &Command::function_RemoveReadOption;
	pointers_[Rewind] = &Command::function_Rewind;
	pointers_[SkipChars] = &Command::function_SkipChars;
	pointers_[SkipLine] = &Command::function_SkipLine;
	pointers_[WriteLine] = &Command::function_WriteLine;
	pointers_[WriteLineFormatted] = &Command::function_WriteLineFormatted;
	pointers_[WriteVariable] = &Command::function_WriteVariable;
	pointers_[WriteVariableFormatted] = &Command::function_WriteVariableFormatted;

	// Script Commands
	pointers_[ListScripts] = &Command::function_ListScripts;
	pointers_[LoadScript] = &Command::function_LoadScript;
	pointers_[RunScript] = &Command::function_RunScript;

	// Select Commands
	pointers_[DeSelect] = &Command::function_DeSelect;
	pointers_[DeSelectType] = &Command::function_DeSelectType;
	pointers_[Expand] = &Command::function_Expand;
	pointers_[Invert] = &Command::function_Invert;
	pointers_[Select] = &Command::function_Select;
	pointers_[SelectAll] = &Command::function_SelectAll;
	pointers_[SelectFFType] = &Command::function_SelectFFType;
	pointers_[SelectionCog] = &Command::function_SelectionCog;
	pointers_[SelectionCom] = &Command::function_SelectionCom;
	pointers_[SelectNone] = &Command::function_SelectNone;
	pointers_[SelectOverlaps] = &Command::function_SelectOverlaps;
	pointers_[SelectPattern] = &Command::function_SelectPattern;
	pointers_[SelectType] = &Command::function_SelectType;
	
	// Site Commands
	pointers_[GetSite] = &Command::function_GetSite;
	pointers_[ListSites] = &Command::function_ListSites;
	pointers_[NewSite] = &Command::function_NewSite;
	pointers_[SiteAxes] = &Command::function_SiteAxes;

	// System Commands
	pointers_[Debug] = &Command::function_Debug;
	pointers_[Gui] = &Command::function_Gui;
	pointers_[Help] = &Command::function_Help;
	pointers_[Seed] = &Command::function_Seed;
	pointers_[Quit] = &Command::function_Quit;
	pointers_[Version] = &Command::function_Version;
	
	// Trajectory Commands
	pointers_[AddFrame] = &Command::function_AddFrame;
	pointers_[ClearTrajectory] = &Command::function_ClearTrajectory;
	pointers_[FinaliseFrame] = &Command::function_FinaliseFrame;
	pointers_[FirstFrame] = &Command::function_FirstFrame;
	pointers_[LastFrame] = &Command::function_LastFrame;
	pointers_[LoadTrajectory] = &Command::function_LoadTrajectory;
	pointers_[NextFrame] = &Command::function_NextFrame;
	pointers_[PrevFrame] = &Command::function_PrevFrame;
	pointers_[SeekFrame] = &Command::function_SeekFrame;

	// Transform Commands
	pointers_[AxisRotate] = &Command::function_AxisRotate;
	pointers_[Centre] = &Command::function_Centre;
	pointers_[Translate] = &Command::function_Translate;
	pointers_[TranslateAtom] = &Command::function_TranslateAtom;
	pointers_[TranslateCell] = &Command::function_TranslateCell;
	pointers_[MatrixConvert] = &Command::function_MatrixConvert;
	pointers_[MatrixTransform] = &Command::function_MatrixTransform;
	pointers_[Mirror] = &Command::function_Mirror;

	// Variable Manipulation Commands
	pointers_[AfterChar] = &Command::function_AfterChar;
	pointers_[AToF] = &Command::function_AToF;
	pointers_[AToI] = &Command::function_AToI;
	pointers_[BeforeChar] = &Command::function_BeforeChar;
	pointers_[Contains] = &Command::function_Contains;
	pointers_[FToA] = &Command::function_FToA;
	pointers_[IToA] = &Command::function_IToA;
	pointers_[Nint] = &Command::function_Nint;
	pointers_[Normalise] = &Command::function_Normalise;	
	pointers_[StripChars] = &Command::function_StripChars;

	// Variable Operators
	pointers_[OperatorAdd] = &function_OperatorAdd;
	pointers_[OperatorAnd] = &function_OperatorAnd;
	pointers_[OperatorAssignment] = &function_OperatorAssignment;
	pointers_[OperatorAssignmentDivide] = &function_OperatorAssignmentDivide;
	pointers_[OperatorAssignmentMinus] = &function_OperatorAssignmentMinus;
	pointers_[OperatorAssignmentMultiply] = &function_OperatorAssignmentMultiply;
	pointers_[OperatorAssignmentPlus] = &function_OperatorAssignmentPlus;
	pointers_[OperatorDivide] = &function_OperatorDivide;
	pointers_[OperatorEqualTo] = &function_OperatorEqualTo;
	pointers_[OperatorGreaterThan] = &function_OperatorGreaterThan;
	pointers_[OperatorGreaterThanEqualTo] = &function_OperatorGreaterThanEqualTo;
	pointers_[OperatorLessThan] = &function_OperatorLessThan;
	pointers_[OperatorLessThanEqualTo] = &function_OperatorLessThanEqualTo;
	pointers_[OperatorMultiply] = &function_OperatorMultiply;
	pointers_[OperatorNegate] = &function_OperatorNegate;
	pointers_[OperatorNot] = &function_OperatorNot;
	pointers_[OperatorNotEqualTo] = &function_OperatorNotEqualTo;
	pointers_[OperatorOr] = &function_OperatorOr;
	pointers_[OperatorPower] = &function_OperatorPower;
	pointers_[OperatorPostfixIncrease] = &function_OperatorPostfixIncrease;
	pointers_[OperatorPostfixDecrease] = &function_OperatorPostfixDecrease;
	pointers_[OperatorPrefixIncrease] = &function_OperatorPrefixIncrease;
	pointers_[OperatorPrefixDecrease] = &function_OperatorPrefixDecrease;
	pointers_[OperatorSubtract] = &function_OperatorSubtract;

/*
	// View Commands
	pointers_[GetView] = &Command::function_GetView;
	pointers_[Orthographic] = &Command::function_Orthographic;
	pointers_[Perspective] = &Command::function_Perspective;
	pointers_[ResetView] = &Command::function_ResetView;
	pointers_[RotateView] = &Command::function_RotateView;
	pointers_[SetView] = &Command::function_SetView;
	pointers_[SpeedTest] = &Command::function_SpeedTest;
	pointers_[TranslateView] = &Command::function_TranslateView;
	pointers_[ViewAlong] = &Command::function_ViewAlong;
	pointers_[ViewAlongCell] = &Command::function_ViewAlongCell;
	pointers_[ZoomView] = &Command::function_ZoomView;
	pointers_[ZRotateView] = &Command::function_ZRotateView;
*/
}
