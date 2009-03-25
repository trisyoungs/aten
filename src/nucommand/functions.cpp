/*
	*** Command Function Pointers
	*** src/nucommand/functions.cpp
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

#include "nucommand/commands.h"

// Initialise Command Pointers
void NuCommand::initPointers()
{
	/*
	// Store pointers to all command functions
	*/
	pointers_[NoFunction] = &function_NoFunction;
	pointers_[Joiner] = &function_Joiner;
	pointers_[Initialisations] = &function_Initialisations;


	// Analyse commands
	pointers_[Finalise] = &NuCommand::function_Finalise;
	pointers_[FrameAnalyse] = &NuCommand::function_FrameAnalyse;
	pointers_[Geometry] = &NuCommand::function_Geometry;
	pointers_[ModelAnalyse] = &NuCommand::function_ModelAnalyse;
	pointers_[PDens] = &NuCommand::function_PDens;
	pointers_[PrintJobs] = &NuCommand::function_PrintJobs;
	pointers_[RDF] = &NuCommand::function_RDF;
	pointers_[SaveQuantities] = &NuCommand::function_SaveQuantities;
	pointers_[TrajAnalyse] = &NuCommand::function_TrajAnalyse;

	// Atom commands
	pointers_[AtomStyle] = &NuCommand::function_AtomStyle;
	pointers_[GetAtom] = &NuCommand::function_GetAtom;
	pointers_[Hide] = &NuCommand::function_Hide;
	pointers_[SetCoords] = &NuCommand::function_SetCoords;
	pointers_[SetCharge] = &NuCommand::function_SetCharge;
	pointers_[SetElement] = &NuCommand::function_SetElement;
	pointers_[SetForces] = &NuCommand::function_SetForces;
	pointers_[SetFX] = &NuCommand::function_SetFX;
	pointers_[SetFY] = &NuCommand::function_SetFY;
	pointers_[SetFZ] = &NuCommand::function_SetFZ;
	pointers_[SetId] = &NuCommand::function_SetId;
	pointers_[SetRX] = &NuCommand::function_SetRX;
	pointers_[SetRY] = &NuCommand::function_SetRY;
	pointers_[SetRZ] = &NuCommand::function_SetRZ;
	pointers_[SetVelocities] = &NuCommand::function_SetVelocities;
	pointers_[SetVX] = &NuCommand::function_SetVX;
	pointers_[SetVY] = &NuCommand::function_SetVY;
	pointers_[SetVZ] = &NuCommand::function_SetVZ;
	pointers_[Show] = &NuCommand::function_Show;
	pointers_[ShowAll] = &NuCommand::function_ShowAll;

	// Bond commands
	pointers_[Augment] = &NuCommand::function_Augment;
	pointers_[BondTolerance] = &NuCommand::function_BondTolerance;
	pointers_[ClearBonds] = &NuCommand::function_ClearBonds;
	pointers_[ClearSelectedBonds] = &NuCommand::function_ClearSelectedBonds;
	pointers_[NewBond] = &NuCommand::function_NewBond;
	pointers_[NewBondId] = &NuCommand::function_NewBondId;
	pointers_[ReBond] = &NuCommand::function_ReBond;
	pointers_[ReBondPatterns] = &NuCommand::function_ReBondPatterns;
	pointers_[ReBondSelection] = &NuCommand::function_ReBondSelection;

	// Build commands
	pointers_[AddHydrogen] = &NuCommand::function_AddHydrogen;
	pointers_[Bohr] = &NuCommand::function_Bohr;
	pointers_[Chain] = &NuCommand::function_Chain;
	pointers_[EndChain] = &NuCommand::function_EndChain;
	pointers_[InsertAtom] = &NuCommand::function_InsertAtom;
	pointers_[Locate] = &NuCommand::function_Locate;
	pointers_[Move] = &NuCommand::function_Move;
	pointers_[MoveToEnd] = &NuCommand::function_MoveToEnd;
	pointers_[MoveToStart] = &NuCommand::function_MoveToStart;
	pointers_[NewAtom] = &NuCommand::function_NewAtom;
	pointers_[NewAtomFrac] = &NuCommand::function_NewAtomFrac;
	pointers_[ReOrder] = &NuCommand::function_ReOrder;
	pointers_[ResetPen] = &NuCommand::function_ResetPen;
	pointers_[RotX] = &NuCommand::function_RotX;
	pointers_[RotY] = &NuCommand::function_RotY;
	pointers_[RotZ] = &NuCommand::function_RotZ;
	pointers_[ShiftDown] = &NuCommand::function_ShiftDown;
	pointers_[ShiftUp] = &NuCommand::function_ShiftUp;
	pointers_[Transmute] = &NuCommand::function_Transmute;

	// Cell commands
	pointers_[AddGenerator] = &NuCommand::function_AddGenerator;
	pointers_[AdjustCell] = &NuCommand::function_AdjustCell;
	pointers_[Cell] = &NuCommand::function_Cell;
	pointers_[CellAxes] = &NuCommand::function_CellAxes;
	pointers_[Fold] = &NuCommand::function_Fold;
	pointers_[FoldMolecules] = &NuCommand::function_FoldMolecules;
	pointers_[FracToReal] = &NuCommand::function_FracToReal;
	pointers_[NoCell] = &NuCommand::function_NoCell;
	pointers_[Pack] = &NuCommand::function_Pack;
	pointers_[PrintCell] = &NuCommand::function_PrintCell;
	pointers_[Replicate] = &NuCommand::function_Replicate;
	pointers_[RotateCell] = &NuCommand::function_RotateCell;
	pointers_[Scale] = &NuCommand::function_Scale;
	pointers_[ScaleMolecules] = &NuCommand::function_ScaleMolecules;
	pointers_[SetCell] = &NuCommand::function_SetCell;
	pointers_[Spacegroup] = &NuCommand::function_Spacegroup;

	// Charge commands
	pointers_[ChargeFF] = &NuCommand::function_ChargeFF;
	pointers_[ChargeFromModel] = &NuCommand::function_ChargeFromModel;
	pointers_[ChargePAtom] = &NuCommand::function_ChargePAtom;
	pointers_[Charge] = &NuCommand::function_Charge;
	pointers_[ChargeType] = &NuCommand::function_ChargeType;
	pointers_[ClearCharges] = &NuCommand::function_ClearCharges;

	// Colourscale commands
	pointers_[AddPoint] = &NuCommand::function_AddPoint;
	pointers_[ClearPoints] = &NuCommand::function_ClearPoints;
	pointers_[ListScales] = &NuCommand::function_ListScales;
	pointers_[RemovePoint] = &NuCommand::function_RemovePoint;
	pointers_[ScaleName] = &NuCommand::function_ScaleName;
	pointers_[ScaleVisible] = &NuCommand::function_ScaleVisible;
	pointers_[SetPoint] = &NuCommand::function_SetPoint;
	pointers_[SetPointColour] = &NuCommand::function_SetPointColour;
	pointers_[SetPointValue] = &NuCommand::function_SetPointValue;

	// Disordered build commands
	pointers_[Disorder] = &NuCommand::function_Disorder;
	pointers_[ListComponents] = &NuCommand::function_ListComponents;
	pointers_[NMols] = &NuCommand::function_NMols;
	pointers_[Region] = &NuCommand::function_Region;
	pointers_[RegionCentre] = &NuCommand::function_RegionCentre;
	pointers_[RegionCentreFrac] = &NuCommand::function_RegionCentreFrac;
	pointers_[RegionFrac] = &NuCommand::function_RegionFrac;
	pointers_[RegionGeometry] = &NuCommand::function_RegionGeometry;
	pointers_[RegionGeometryFrac] = &NuCommand::function_RegionGeometryFrac;
	pointers_[RegionOverlaps] = &NuCommand::function_RegionOverlaps;
	pointers_[RegionShape] = &NuCommand::function_RegionShape;
	pointers_[VdwScale] = &NuCommand::function_VdwScale;

	// Edit Commands
	pointers_[Delete] = &NuCommand::function_Delete;
	pointers_[Copy] = &NuCommand::function_Copy;
	pointers_[Cut] = &NuCommand::function_Cut;
	pointers_[Paste] = &NuCommand::function_Paste;
	pointers_[Redo] = &NuCommand::function_Redo;
	pointers_[Undo] = &NuCommand::function_Undo;

	// Energy Commands
	pointers_[FrameEnergy] = &NuCommand::function_FrameEnergy;
	pointers_[ModelEnergy] = &NuCommand::function_ModelEnergy;
	pointers_[PrintElec] = &NuCommand::function_PrintElec;
	pointers_[PrintEwald] = &NuCommand::function_PrintEwald;
	pointers_[PrintInter] = &NuCommand::function_PrintInter;
	pointers_[PrintIntra] = &NuCommand::function_PrintIntra;
	pointers_[PrintEnergy] = &NuCommand::function_PrintEnergy;
	pointers_[PrintSummary] = &NuCommand::function_PrintSummary;
	pointers_[PrintVdw] = &NuCommand::function_PrintVdw;

	// Flow control
	pointers_[If] = &function_If;
	pointers_[Break] = &NuCommand::function_Break;
	pointers_[Continue] = &NuCommand::function_Continue;
// 	pointers_[End] = &NuCommand::function_End;
	pointers_[For] = &NuCommand::function_For;
	pointers_[If] = &NuCommand::function_If;
// 	pointers_[Terminate] = &NuCommand::function_Terminate;

	// Force Commands
	pointers_[FrameForces] = &NuCommand::function_FrameForces;
	pointers_[ModelForces] = &NuCommand::function_ModelForces;
	pointers_[PrintForces] = &NuCommand::function_PrintForces;

	// Forcefield Commands
        pointers_[AngleDef] = &NuCommand::function_AngleDef;
	pointers_[BondDef] = &NuCommand::function_BondDef;
	pointers_[ClearMap] = &NuCommand::function_ClearMap;
	pointers_[CreateExpression] = &NuCommand::function_CreateExpression;
	pointers_[DefaultFF] = &NuCommand::function_DefaultFF;
	pointers_[Equivalent] = &NuCommand::function_Equivalent;
	pointers_[FFModel] = &NuCommand::function_FFModel;
	pointers_[FFPattern] = &NuCommand::function_FFPattern;
	pointers_[FinaliseFF] = &NuCommand::function_FinaliseFF;
	pointers_[GenConvert] = &NuCommand::function_GenConvert;
	pointers_[Generator] = &NuCommand::function_Generator;
	pointers_[GetFF] = &NuCommand::function_GetFF;
	pointers_[InterDef] = &NuCommand::function_InterDef;
	pointers_[LoadFF] = &NuCommand::function_LoadFF;
	pointers_[Map] = &NuCommand::function_Map;
	pointers_[NewFF] = &NuCommand::function_NewFF;
	pointers_[PrintSetup] = &NuCommand::function_PrintSetup;
	pointers_[Rules] = &NuCommand::function_Rules;
	pointers_[SaveExpression] = &NuCommand::function_SaveExpression;
	pointers_[TorsionDef] = &NuCommand::function_TorsionDef;
	pointers_[TypeDef] = &NuCommand::function_TypeDef;
	pointers_[TypeModel] = &NuCommand::function_TypeModel;
	pointers_[TypeTest] = &NuCommand::function_TypeTest;
	pointers_[Units] = &NuCommand::function_Units;

	// Glyph commands
	pointers_[AutoEllipsoids] = &NuCommand::function_AutoEllipsoids;
	pointers_[AutoPolyhedra] = &NuCommand::function_AutoPolyhedra;
	pointers_[GlyphAtomF] = &NuCommand::function_GlyphAtomF;
	pointers_[GlyphAtomR] = &NuCommand::function_GlyphAtomR;
	pointers_[GlyphAtomV] = &NuCommand::function_GlyphAtomV;
	pointers_[GlyphAtomsF] = &NuCommand::function_GlyphAtomsF;
	pointers_[GlyphAtomsR] = &NuCommand::function_GlyphAtomsR;
	pointers_[GlyphAtomsV] = &NuCommand::function_GlyphAtomsV;
	pointers_[GlyphColour] = &NuCommand::function_GlyphColour;
	pointers_[GlyphData] = &NuCommand::function_GlyphData;
	pointers_[GlyphSolid] = &NuCommand::function_GlyphSolid;
	pointers_[GlyphText] = &NuCommand::function_GlyphText;
	pointers_[NewGlyph] = &NuCommand::function_NewGlyph;

	// Grid Commands
	pointers_[AddGridPoint] = &NuCommand::function_AddGridPoint;
	pointers_[AddNextGridPoint] = &NuCommand::function_AddNextGridPoint;
	pointers_[FinaliseGrid] = &NuCommand::function_FinaliseGrid;
	pointers_[GridAlpha] = &NuCommand::function_GridAlpha;
	pointers_[GridAxes] = &NuCommand::function_GridAxes;
	pointers_[GridColour] = &NuCommand::function_GridColour;
	pointers_[GridColourNegative] = &NuCommand::function_GridColourNegative;
	pointers_[GridColourscale] = &NuCommand::function_GridColourscale;
	pointers_[GridCubic] = &NuCommand::function_GridCubic;
	pointers_[GridCutoff] = &NuCommand::function_GridCutoff;
	pointers_[GridLoopOrder] = &NuCommand::function_GridLoopOrder;
	pointers_[GridOrigin] = &NuCommand::function_GridOrigin;
	pointers_[GridOrtho] = &NuCommand::function_GridOrtho;
	pointers_[GridSize] = &NuCommand::function_GridSize;
	pointers_[GridStyle] = &NuCommand::function_GridStyle;
	pointers_[GridSymmetric] = &NuCommand::function_GridSymmetric;
	pointers_[GridUseZ] = &NuCommand::function_GridUseZ;
	pointers_[LoadGrid] = &NuCommand::function_LoadGrid;
	pointers_[NewGrid] = &NuCommand::function_NewGrid;

	// Image Commands
	pointers_[SaveBitmap] = &NuCommand::function_SaveBitmap;
	pointers_[SaveVector] = &NuCommand::function_SaveVector;

	// Labeling Commands
	pointers_[ClearLabels] = &NuCommand::function_ClearLabels;
	pointers_[Label] = &NuCommand::function_Label;
	pointers_[RemoveLabel] = &NuCommand::function_RemoveLabel;
	pointers_[RemoveLabels] = &NuCommand::function_RemoveLabels;

	// MC Commands
	pointers_[MCAccept] = &NuCommand::function_MCAccept;
	pointers_[MCAllow] = &NuCommand::function_MCAllow;
	pointers_[MCMaxStep] = &NuCommand::function_MCMaxStep;
	pointers_[MCNTrials] = &NuCommand::function_MCNTrials;
	pointers_[PrintMC] = &NuCommand::function_PrintMC;

	// Measurement Commands
	pointers_[Angle] = &NuCommand::function_Angle;
	pointers_[Angles] = &NuCommand::function_Angles;
	pointers_[ClearMeasurements] = &NuCommand::function_ClearMeasurements;
	pointers_[Distance] = &NuCommand::function_Distance;
	pointers_[Distances] = &NuCommand::function_Distances;
	pointers_[ListMeasurements] = &NuCommand::function_ListMeasurements;
	pointers_[Measure] = &NuCommand::function_Measure;
	pointers_[Torsion] = &NuCommand::function_Torsion;
	pointers_[Torsions] = &NuCommand::function_Torsions;

	// Messaging Commands
	pointers_[Error] = &NuCommand::function_Error;
	pointers_[Printf] = &NuCommand::function_Printf;
	pointers_[Verbose] = &NuCommand::function_Verbose;
	pointers_[Warn] = &NuCommand::function_Warn;

	// Minimisation Commands
	pointers_[CGMinimise] = &NuCommand::function_CGMinimise;
	pointers_[Converge] = &NuCommand::function_Converge;
	pointers_[LineTolerance] = &NuCommand::function_LineTolerance;
	pointers_[MCMinimise] = &NuCommand::function_MCMinimise;
	pointers_[SDMinimise] = &NuCommand::function_SDMinimise;
	pointers_[SimplexMinimise] = &NuCommand::function_SimplexMinimise;
	
	// Model Commands
	pointers_[CreateAtoms] = &NuCommand::function_CreateAtoms;
	pointers_[CurrentModel] = &NuCommand::function_CurrentModel;
	pointers_[FinaliseModel] = &NuCommand::function_FinaliseModel;
	pointers_[FirstModel] = &NuCommand::function_FirstModel;
	pointers_[GetModel] = &NuCommand::function_GetModel;
	pointers_[LastModel] = &NuCommand::function_LastModel;
	pointers_[ListModels] = &NuCommand::function_ListModels;
	pointers_[LoadModel] = &NuCommand::function_LoadModel;
	pointers_[LogInfo] = &NuCommand::function_LogInfo;
	pointers_[ModelTemplate] = &NuCommand::function_ModelTemplate;
	pointers_[NewModel] = &NuCommand::function_NewModel;
	pointers_[NextModel] = &NuCommand::function_NextModel;
	pointers_[PrevModel] = &NuCommand::function_PrevModel;
	pointers_[Info] = &NuCommand::function_Info;
	pointers_[SaveModel] = &NuCommand::function_SaveModel;
	pointers_[SetName] = &NuCommand::function_SetName;

	// Pattern Commands
	pointers_[ClearPatterns] = &NuCommand::function_ClearPatterns;
	pointers_[CreatePatterns] = &NuCommand::function_CreatePatterns;
	pointers_[GetPattern] = &NuCommand::function_GetPattern;
	pointers_[ListPatterns] = &NuCommand::function_ListPatterns;
	pointers_[NewPattern] = &NuCommand::function_NewPattern;

	// Preferences Commands
	pointers_[AngleLabel] = &NuCommand::function_AngleLabel;
	pointers_[AtomDetail] = &NuCommand::function_AtomDetail;
	pointers_[BondDetail] = &NuCommand::function_BondDetail;
	pointers_[Colour] = &NuCommand::function_Colour;
	pointers_[CommonElements] = &NuCommand::function_CommonElements;
	pointers_[DensityUnits] = &NuCommand::function_DensityUnits;
	pointers_[DistanceLabel] = &NuCommand::function_DistanceLabel;
	pointers_[ECut] = &NuCommand::function_ECut;
	pointers_[Elec] = &NuCommand::function_Elec;
	pointers_[ElementAmbient] = &NuCommand::function_ElementAmbient;
	pointers_[ElementDiffuse] = &NuCommand::function_ElementDiffuse;
	pointers_[ElementRadius] = &NuCommand::function_ElementRadius;
	pointers_[EnergyUnits] = &NuCommand::function_EnergyUnits;
	pointers_[GL] = &NuCommand::function_GL;
	pointers_[HDistance] = &NuCommand::function_HDistance;
	pointers_[Intra] = &NuCommand::function_Intra;
	pointers_[Key] = &NuCommand::function_Key;
	pointers_[LabelSize] = &NuCommand::function_LabelSize;
	pointers_[Light] = &NuCommand::function_Light;
	pointers_[LightAmbient] = &NuCommand::function_LightAmbient;
	pointers_[LightDiffuse] = &NuCommand::function_LightDiffuse;
	pointers_[LightPosition] = &NuCommand::function_LightPosition;
	pointers_[LightSpecular] = &NuCommand::function_LightSpecular;
	pointers_[Mouse] = &NuCommand::function_Mouse;
	pointers_[Radius] = &NuCommand::function_Radius;
	pointers_[ReplicateFold] = &NuCommand::function_ReplicateFold;
	pointers_[ReplicateTrim] = &NuCommand::function_ReplicateTrim;
	pointers_[Scheme] = &NuCommand::function_Scheme;
	pointers_[Shininess] = &NuCommand::function_Shininess;
	pointers_[ShowOnScreen] = &NuCommand::function_ShowOnScreen;
	pointers_[ShowOnImage] = &NuCommand::function_ShowOnImage;
	pointers_[Style] = &NuCommand::function_Style;
	pointers_[SwapBuffers] = &NuCommand::function_SwapBuffers;
	pointers_[UseNiceText] = &NuCommand::function_UseNiceText;
	pointers_[VCut] = &NuCommand::function_VCut;
	pointers_[Vdw] = &NuCommand::function_Vdw;
	pointers_[ZoomThrottle] = &NuCommand::function_ZoomThrottle;

	// Read / Write Commands
	pointers_[AddReadOption] = &NuCommand::function_AddReadOption;
	pointers_[Find] = &NuCommand::function_Find;
	pointers_[GetLine] = &NuCommand::function_GetLine;
	pointers_[ReadChars] = &NuCommand::function_ReadChars;
	pointers_[ReadInteger] = &NuCommand::function_ReadInteger;
	pointers_[ReadLine] = &NuCommand::function_ReadLine;
	pointers_[ReadLineFormatted] = &NuCommand::function_ReadLineFormatted;
	pointers_[ReadNext] = &NuCommand::function_ReadNext;
	pointers_[ReadReal] = &NuCommand::function_ReadReal;
	pointers_[ReadVar] = &NuCommand::function_ReadVar;
	pointers_[RemoveReadOption] = &NuCommand::function_RemoveReadOption;
	pointers_[Rewind] = &NuCommand::function_Rewind;
	pointers_[SkipChars] = &NuCommand::function_SkipChars;
	pointers_[SkipLine] = &NuCommand::function_SkipLine;
	pointers_[WriteLine] = &NuCommand::function_WriteLine;
	pointers_[WriteLineFormatted] = &NuCommand::function_WriteLineFormatted;
	pointers_[WriteVar] = &NuCommand::function_WriteVar;

	// Script Commands
	pointers_[ListScripts] = &NuCommand::function_ListScripts;
	pointers_[LoadScript] = &NuCommand::function_LoadScript;
	pointers_[RunScript] = &NuCommand::function_RunScript;

	// Select Commands
	pointers_[DeSelect] = &NuCommand::function_DeSelect;
	pointers_[DeSelectType] = &NuCommand::function_DeSelectType;
	pointers_[Expand] = &NuCommand::function_Expand;
	pointers_[Invert] = &NuCommand::function_Invert;
	pointers_[Select] = &NuCommand::function_Select;
	pointers_[SelectAll] = &NuCommand::function_SelectAll;
	pointers_[SelectFFType] = &NuCommand::function_SelectFFType;
	pointers_[SelectionCog] = &NuCommand::function_SelectionCog;
	pointers_[SelectionCom] = &NuCommand::function_SelectionCom;
	pointers_[SelectNone] = &NuCommand::function_SelectNone;
	pointers_[SelectOverlaps] = &NuCommand::function_SelectOverlaps;
	pointers_[SelectPattern] = &NuCommand::function_SelectPattern;
	pointers_[SelectType] = &NuCommand::function_SelectType;
	
	// Site Commands
	pointers_[GetSite] = &NuCommand::function_GetSite;
	pointers_[ListSites] = &NuCommand::function_ListSites;
	pointers_[NewSite] = &NuCommand::function_NewSite;
	pointers_[SiteAxes] = &NuCommand::function_SiteAxes;

	// System Commands
	pointers_[Debug] = &NuCommand::function_Debug;
	pointers_[Gui] = &NuCommand::function_Gui;
	pointers_[Help] = &NuCommand::function_Help;
	pointers_[Seed] = &NuCommand::function_Seed;
	pointers_[Quit] = &NuCommand::function_Quit;
	pointers_[Version] = &NuCommand::function_Version;
	
	// Trajectory Commands
	pointers_[FinaliseFrame] = &NuCommand::function_FinaliseFrame;
	pointers_[FirstFrame] = &NuCommand::function_FirstFrame;
	pointers_[LastFrame] = &NuCommand::function_LastFrame;
	pointers_[LoadTrajectory] = &NuCommand::function_LoadTrajectory;
	pointers_[NextFrame] = &NuCommand::function_NextFrame;
	pointers_[PrevFrame] = &NuCommand::function_PrevFrame;
	pointers_[SeekFrame] = &NuCommand::function_SeekFrame;

	// Transform Commands
	pointers_[AxisRotate] = &NuCommand::function_AxisRotate;
	pointers_[Centre] = &NuCommand::function_Centre;
	pointers_[Translate] = &NuCommand::function_Translate;
	pointers_[TranslateAtom] = &NuCommand::function_TranslateAtom;
	pointers_[TranslateCell] = &NuCommand::function_TranslateCell;
	pointers_[MatrixConvert] = &NuCommand::function_MatrixConvert;
	pointers_[MatrixTransform] = &NuCommand::function_MatrixTransform;
	pointers_[Mirror] = &NuCommand::function_Mirror;

	// Variable Manipulation Commands
	pointers_[AfterChar] = &NuCommand::function_AfterChar;
	pointers_[BeforeChar] = &NuCommand::function_BeforeChar;
	pointers_[Normalise] = &NuCommand::function_Normalise;	
	pointers_[StripChars] = &NuCommand::function_StripChars;

	// Variable Operators
	pointers_[OperatorAdd] = &function_OperatorAdd;
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
	pointers_[OperatorNotEqualTo] = &function_OperatorNotEqualTo;
	pointers_[OperatorPower] = &function_OperatorPower;
	pointers_[OperatorPostfixIncrease] = &function_OperatorPostfixIncrease;
	pointers_[OperatorPostfixDecrease] = &function_OperatorPostfixDecrease;
	pointers_[OperatorPrefixIncrease] = &function_OperatorPrefixIncrease;
	pointers_[OperatorPrefixDecrease] = &function_OperatorPrefixDecrease;
	pointers_[OperatorSubtract] = &function_OperatorSubtract;

/*
	// View Commands
	pointers_[GetView] = &NuCommand::function_GetView;
	pointers_[Orthographic] = &NuCommand::function_Orthographic;
	pointers_[Perspective] = &NuCommand::function_Perspective;
	pointers_[ResetView] = &NuCommand::function_ResetView;
	pointers_[RotateView] = &NuCommand::function_RotateView;
	pointers_[SetView] = &NuCommand::function_SetView;
	pointers_[SpeedTest] = &NuCommand::function_SpeedTest;
	pointers_[TranslateView] = &NuCommand::function_TranslateView;
	pointers_[ViewAlong] = &NuCommand::function_ViewAlong;
	pointers_[ViewAlongCell] = &NuCommand::function_ViewAlongCell;
	pointers_[ZoomView] = &NuCommand::function_ZoomView;
	pointers_[ZRotateView] = &NuCommand::function_ZRotateView;
*/
}
