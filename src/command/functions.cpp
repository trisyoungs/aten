/*
	*** Command Function Pointers
	*** src/command/functions.cpp
	Copyright T. Youngs 2007-2016

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

ATEN_USING_NAMESPACE

// Initialise Command Pointers
void Commands::initPointers()
{
	/*
	 * Store pointers to all command functions
	 */
	// Operators
	pointers_[OperatorAdd] = &AtenSpace::Commands::function_OperatorAdd;
	pointers_[OperatorAnd] = &AtenSpace::Commands::function_OperatorAnd;
	pointers_[OperatorAssignment] = &AtenSpace::Commands::function_OperatorAssignment;
	pointers_[OperatorAssignmentDivide] = &AtenSpace::Commands::function_OperatorAssignmentDivide;
	pointers_[OperatorAssignmentMultiply] = &AtenSpace::Commands::function_OperatorAssignmentMultiply;
	pointers_[OperatorAssignmentPlus] = &AtenSpace::Commands::function_OperatorAssignmentPlus;
	pointers_[OperatorAssignmentSubtract] = &AtenSpace::Commands::function_OperatorAssignmentSubtract;
	pointers_[OperatorDivide] = &AtenSpace::Commands::function_OperatorDivide;
	pointers_[OperatorEqualTo] = &AtenSpace::Commands::function_OperatorEqualTo;
	pointers_[OperatorGreaterThan] = &AtenSpace::Commands::function_OperatorGreaterThan;
	pointers_[OperatorGreaterThanEqualTo] = &AtenSpace::Commands::function_OperatorGreaterThanEqualTo;
	pointers_[OperatorInlineIf] = &AtenSpace::Commands::function_OperatorInlineIf;
	pointers_[OperatorLessThan] = &AtenSpace::Commands::function_OperatorLessThan;
	pointers_[OperatorLessThanEqualTo] = &AtenSpace::Commands::function_OperatorLessThanEqualTo;
	pointers_[OperatorModulus] = &AtenSpace::Commands::function_OperatorModulus;
	pointers_[OperatorMultiply] = &AtenSpace::Commands::function_OperatorMultiply;
	pointers_[OperatorNegate] = &AtenSpace::Commands::function_OperatorNegate;
	pointers_[OperatorNot] = &AtenSpace::Commands::function_OperatorNot;
	pointers_[OperatorNotEqualTo] = &AtenSpace::Commands::function_OperatorNotEqualTo;
	pointers_[OperatorOr] = &AtenSpace::Commands::function_OperatorOr;
	pointers_[OperatorPower] = &AtenSpace::Commands::function_OperatorPower;
	pointers_[OperatorPostfixIncrease] = &AtenSpace::Commands::function_OperatorPostfixIncrease;
	pointers_[OperatorPostfixDecrease] = &AtenSpace::Commands::function_OperatorPostfixDecrease;
	pointers_[OperatorPrefixIncrease] = &AtenSpace::Commands::function_OperatorPrefixIncrease;
	pointers_[OperatorPrefixDecrease] = &AtenSpace::Commands::function_OperatorPrefixDecrease;
	pointers_[OperatorSubtract] = &AtenSpace::Commands::function_OperatorSubtract;

	pointers_[NoFunction] = &AtenSpace::Commands::function_NoFunction;
	pointers_[Joiner] = &AtenSpace::Commands::function_Joiner;
	pointers_[Declarations] = &AtenSpace::Commands::function_Declarations;

	// Analyse commands
	pointers_[Finalise] = &AtenSpace::Commands::function_Finalise;
	pointers_[FrameAnalyse] = &AtenSpace::Commands::function_FrameAnalyse;
	pointers_[Geometric] = &AtenSpace::Commands::function_Geometric;
	pointers_[ModelAnalyse] = &AtenSpace::Commands::function_ModelAnalyse;
	pointers_[PDens] = &AtenSpace::Commands::function_PDens;
	pointers_[PrintJobs] = &AtenSpace::Commands::function_PrintJobs;
	pointers_[RDF] = &AtenSpace::Commands::function_RDF;
	pointers_[SaveQuantities] = &AtenSpace::Commands::function_SaveQuantities;
	pointers_[TrajAnalyse] = &AtenSpace::Commands::function_TrajAnalyse;

	// Atom commands
	pointers_[AtomStyle] = &AtenSpace::Commands::function_AtomStyle;
	pointers_[ColourAtoms] = &AtenSpace::Commands::function_ColourAtoms;
	pointers_[CurrentAtom] = &AtenSpace::Commands::function_CurrentAtom;
	pointers_[Fix] = &AtenSpace::Commands::function_Fix;
	pointers_[Free] = &AtenSpace::Commands::function_Free;
	pointers_[GetAtom] = &AtenSpace::Commands::function_GetAtom;
	pointers_[Hide] = &AtenSpace::Commands::function_Hide;
	pointers_[RecolourAtoms] = &AtenSpace::Commands::function_RecolourAtoms;
	pointers_[SetCoords] = &AtenSpace::Commands::function_SetCoords;
	pointers_[SetCharge] = &AtenSpace::Commands::function_SetCharge;
	pointers_[SetElement] = &AtenSpace::Commands::function_SetElement;
	pointers_[SetForces] = &AtenSpace::Commands::function_SetForces;
	pointers_[SetFX] = &AtenSpace::Commands::function_SetFX;
	pointers_[SetFY] = &AtenSpace::Commands::function_SetFY;
	pointers_[SetFZ] = &AtenSpace::Commands::function_SetFZ;
	pointers_[SetRX] = &AtenSpace::Commands::function_SetRX;
	pointers_[SetRY] = &AtenSpace::Commands::function_SetRY;
	pointers_[SetRZ] = &AtenSpace::Commands::function_SetRZ;
	pointers_[SetVelocities] = &AtenSpace::Commands::function_SetVelocities;
	pointers_[SetVX] = &AtenSpace::Commands::function_SetVX;
	pointers_[SetVY] = &AtenSpace::Commands::function_SetVY;
	pointers_[SetVZ] = &AtenSpace::Commands::function_SetVZ;
	pointers_[Show] = &AtenSpace::Commands::function_Show;

	// Bond commands
	pointers_[Augment] = &AtenSpace::Commands::function_Augment;
	pointers_[BondTolerance] = &AtenSpace::Commands::function_BondTolerance;
	pointers_[ClearBonds] = &AtenSpace::Commands::function_ClearBonds;
	pointers_[ClearSelectedBonds] = &AtenSpace::Commands::function_ClearSelectedBonds;
	pointers_[NewBond] = &AtenSpace::Commands::function_NewBond;
	pointers_[ReBond] = &AtenSpace::Commands::function_ReBond;
	pointers_[ReBondPatterns] = &AtenSpace::Commands::function_ReBondPatterns;
	pointers_[ReBondSelection] = &AtenSpace::Commands::function_ReBondSelection;

	// Build commands
	pointers_[AddHydrogen] = &AtenSpace::Commands::function_AddHydrogen;
	pointers_[Bohr] = &AtenSpace::Commands::function_Bohr;
	pointers_[Chain] = &AtenSpace::Commands::function_Chain;
	pointers_[EndChain] = &AtenSpace::Commands::function_EndChain;
	pointers_[GrowAtom] = &AtenSpace::Commands::function_GrowAtom;
	pointers_[InsertAtom] = &AtenSpace::Commands::function_InsertAtom;
	pointers_[Locate] = &AtenSpace::Commands::function_Locate;
	pointers_[Move] = &AtenSpace::Commands::function_Move;
	pointers_[MoveToEnd] = &AtenSpace::Commands::function_MoveToEnd;
	pointers_[MoveToStart] = &AtenSpace::Commands::function_MoveToStart;
	pointers_[NewAtom] = &AtenSpace::Commands::function_NewAtom;
	pointers_[NewAtomFrac] = &AtenSpace::Commands::function_NewAtomFrac;
	pointers_[ReOrder] = &AtenSpace::Commands::function_ReOrder;
	pointers_[ResetPen] = &AtenSpace::Commands::function_ResetPen;
	pointers_[RotX] = &AtenSpace::Commands::function_RotX;
	pointers_[RotY] = &AtenSpace::Commands::function_RotY;
	pointers_[RotZ] = &AtenSpace::Commands::function_RotZ;
	pointers_[SelectionAddHydrogen] = &AtenSpace::Commands::function_SelectionAddHydrogen;
	pointers_[SelectionGrowAtom] = &AtenSpace::Commands::function_SelectionGrowAtom;
	pointers_[SetAngle] = &AtenSpace::Commands::function_SetAngle;
	pointers_[SetAngles] = &AtenSpace::Commands::function_SetAngles;
	pointers_[SetDistance] = &AtenSpace::Commands::function_SetDistance;
	pointers_[SetDistances] = &AtenSpace::Commands::function_SetDistances;
	pointers_[SetTorsion] = &AtenSpace::Commands::function_SetTorsion;
	pointers_[SetTorsions] = &AtenSpace::Commands::function_SetTorsions;
	pointers_[ShiftDown] = &AtenSpace::Commands::function_ShiftDown;
	pointers_[ShiftUp] = &AtenSpace::Commands::function_ShiftUp;
	pointers_[Transmute] = &AtenSpace::Commands::function_Transmute;

	// Cell commands
	pointers_[AddGenerator] = &AtenSpace::Commands::function_AddGenerator;
	pointers_[AdjustCell] = &AtenSpace::Commands::function_AdjustCell;
	pointers_[Cell] = &AtenSpace::Commands::function_Cell;
	pointers_[CellAxes] = &AtenSpace::Commands::function_CellAxes;
	pointers_[Fold] = &AtenSpace::Commands::function_Fold;
	pointers_[FoldMolecules] = &AtenSpace::Commands::function_FoldMolecules;
	pointers_[FracToReal] = &AtenSpace::Commands::function_FracToReal;
	pointers_[MillerCut] = &AtenSpace::Commands::function_MillerCut;
	pointers_[NoCell] = &AtenSpace::Commands::function_NoCell;
	pointers_[Pack] = &AtenSpace::Commands::function_Pack;
	pointers_[PrintCell] = &AtenSpace::Commands::function_PrintCell;
	pointers_[Replicate] = &AtenSpace::Commands::function_Replicate;
	pointers_[RotateCell] = &AtenSpace::Commands::function_RotateCell;
	pointers_[Scale] = &AtenSpace::Commands::function_Scale;
	pointers_[ScaleMolecules] = &AtenSpace::Commands::function_ScaleMolecules;
	pointers_[SetCell] = &AtenSpace::Commands::function_SetCell;
	pointers_[SGInfo] = &AtenSpace::Commands::function_SGInfo;
	pointers_[Spacegroup] = &AtenSpace::Commands::function_Spacegroup;

	// Charge commands
	pointers_[Charge] = &AtenSpace::Commands::function_Charge;
	pointers_[ChargeFF] = &AtenSpace::Commands::function_ChargeFF;
	pointers_[ChargeFromModel] = &AtenSpace::Commands::function_ChargeFromModel;
	pointers_[ChargePAtom] = &AtenSpace::Commands::function_ChargePAtom;
	pointers_[ChargeType] = &AtenSpace::Commands::function_ChargeType;
	pointers_[ClearCharges] = &AtenSpace::Commands::function_ClearCharges;

	// Colourscale commands
	pointers_[AddPoint] = &AtenSpace::Commands::function_AddPoint;
	pointers_[ClearPoints] = &AtenSpace::Commands::function_ClearPoints;
	pointers_[ListScales] = &AtenSpace::Commands::function_ListScales;
	pointers_[RemovePoint] = &AtenSpace::Commands::function_RemovePoint;
	pointers_[ScaleInterpolate] = &AtenSpace::Commands::function_ScaleInterpolate;
	pointers_[ScaleName] = &AtenSpace::Commands::function_ScaleName;
	pointers_[ScaleVisible] = &AtenSpace::Commands::function_ScaleVisible;
	pointers_[SetPoint] = &AtenSpace::Commands::function_SetPoint;
	pointers_[SetPointColour] = &AtenSpace::Commands::function_SetPointColour;
	pointers_[SetPointValue] = &AtenSpace::Commands::function_SetPointValue;

	// Disordered build commands
	pointers_[Disorder] = &AtenSpace::Commands::function_Disorder;
	pointers_[ListComponents] = &AtenSpace::Commands::function_ListComponents;
	pointers_[SetupComponent] = &AtenSpace::Commands::function_SetupComponent;

	// Edit Commands
	pointers_[Delete] = &AtenSpace::Commands::function_Delete;
	pointers_[Copy] = &AtenSpace::Commands::function_Copy;
	pointers_[Cut] = &AtenSpace::Commands::function_Cut;
	pointers_[Paste] = &AtenSpace::Commands::function_Paste;
	pointers_[Redo] = &AtenSpace::Commands::function_Redo;
	pointers_[Undo] = &AtenSpace::Commands::function_Undo;

	// Energy Commands
	pointers_[Electrostatics] = &AtenSpace::Commands::function_Electrostatics;
	pointers_[FrameEnergy] = &AtenSpace::Commands::function_FrameEnergy;
	pointers_[ModelEnergy] = &AtenSpace::Commands::function_ModelEnergy;
	pointers_[PrintElec] = &AtenSpace::Commands::function_PrintElec;
	pointers_[PrintEwald] = &AtenSpace::Commands::function_PrintEwald;
	pointers_[PrintInter] = &AtenSpace::Commands::function_PrintInter;
	pointers_[PrintIntra] = &AtenSpace::Commands::function_PrintIntra;
	pointers_[PrintEnergy] = &AtenSpace::Commands::function_PrintEnergy;
	pointers_[PrintSummary] = &AtenSpace::Commands::function_PrintSummary;
	pointers_[PrintVdw] = &AtenSpace::Commands::function_PrintVdw;

	// Flow control
	pointers_[If] = &AtenSpace::Commands::function_If;
	pointers_[Break] = &AtenSpace::Commands::function_Break;
	pointers_[Case] = &AtenSpace::Commands::function_Case;
	pointers_[Continue] = &AtenSpace::Commands::function_Continue;
	pointers_[Default] = &AtenSpace::Commands::function_Default;
	pointers_[DoWhile] = &AtenSpace::Commands::function_DoWhile;
	pointers_[For] = &AtenSpace::Commands::function_For;
	pointers_[ForIn] = &AtenSpace::Commands::function_ForIn;
	pointers_[If] = &AtenSpace::Commands::function_If;
	pointers_[Return] = &AtenSpace::Commands::function_Return;
	pointers_[Switch] = &AtenSpace::Commands::function_Switch;
	pointers_[While] = &AtenSpace::Commands::function_While;

	// Force Commands
	pointers_[FrameForces] = &AtenSpace::Commands::function_FrameForces;
	pointers_[ModelForces] = &AtenSpace::Commands::function_ModelForces;
	pointers_[PrintForces] = &AtenSpace::Commands::function_PrintForces;

	// Forcefield Commands
        pointers_[AngleDef] = &AtenSpace::Commands::function_AngleDef;
	pointers_[AutoConversionUnit] = &AtenSpace::Commands::function_AutoConversionUnit;
	pointers_[BondDef] = &AtenSpace::Commands::function_BondDef;
	pointers_[ClearExportMap] = &AtenSpace::Commands::function_ClearExportMap;
	pointers_[ClearExpression] = &AtenSpace::Commands::function_ClearExpression;
	pointers_[ClearMap] = &AtenSpace::Commands::function_ClearMap;
	pointers_[ClearTypes] = &AtenSpace::Commands::function_ClearTypes;
	pointers_[CreateExpression] = &AtenSpace::Commands::function_CreateExpression;
	pointers_[CurrentFF] = &AtenSpace::Commands::function_CurrentFF;
	pointers_[DeleteFF] = &AtenSpace::Commands::function_DeleteFF;
	pointers_[EnergyConvert] = &AtenSpace::Commands::function_EnergyConvert;
	pointers_[Equivalent] = &AtenSpace::Commands::function_Equivalent;
	pointers_[ExportMap] = &AtenSpace::Commands::function_ExportMap;
	pointers_[FFModel] = &AtenSpace::Commands::function_FFModel;
	pointers_[FFPattern] = &AtenSpace::Commands::function_FFPattern;
	pointers_[FinaliseFF] = &AtenSpace::Commands::function_FinaliseFF;
	pointers_[FixType] = &AtenSpace::Commands::function_FixType;
	pointers_[FreeType] = &AtenSpace::Commands::function_FreeType;
	pointers_[GenerateAngle] = &AtenSpace::Commands::function_GenerateAngle;
	pointers_[GenerateBond] = &AtenSpace::Commands::function_GenerateBond;
	pointers_[GenerateTorsion] = &AtenSpace::Commands::function_GenerateTorsion;
	pointers_[GenerateVdw] = &AtenSpace::Commands::function_GenerateVdw;
	pointers_[GetCombinationRule] = &AtenSpace::Commands::function_GetCombinationRule;
	pointers_[GetFF] = &AtenSpace::Commands::function_GetFF;
	pointers_[InterDef] = &AtenSpace::Commands::function_InterDef;
	pointers_[LoadFF] = &AtenSpace::Commands::function_LoadFF;
	pointers_[Map] = &AtenSpace::Commands::function_Map;
	pointers_[NewFF] = &AtenSpace::Commands::function_NewFF;
	pointers_[PrintSetup] = &AtenSpace::Commands::function_PrintSetup;
	pointers_[PrintType] = &AtenSpace::Commands::function_PrintType;
	pointers_[RecreateExpression] = &AtenSpace::Commands::function_RecreateExpression;
	pointers_[SaveExpression] = &AtenSpace::Commands::function_SaveExpression;
	pointers_[SetCombinationRule] = &AtenSpace::Commands::function_SetCombinationRule;
	pointers_[TorsionDef] = &AtenSpace::Commands::function_TorsionDef;
	pointers_[TypeDef] = &AtenSpace::Commands::function_TypeDef;
	pointers_[TypeModel] = &AtenSpace::Commands::function_TypeModel;
	pointers_[TypeTest] = &AtenSpace::Commands::function_TypeTest;
	pointers_[Units] = &AtenSpace::Commands::function_Units;

	// Glyph commands
	pointers_[AutoEllipsoids] = &AtenSpace::Commands::function_AutoEllipsoids;
	pointers_[AutoPolyhedra] = &AtenSpace::Commands::function_AutoPolyhedra;
	pointers_[GlyphAtomF] = &AtenSpace::Commands::function_GlyphAtomF;
	pointers_[GlyphAtomR] = &AtenSpace::Commands::function_GlyphAtomR;
	pointers_[GlyphAtomV] = &AtenSpace::Commands::function_GlyphAtomV;
	pointers_[GlyphAtomsF] = &AtenSpace::Commands::function_GlyphAtomsF;
	pointers_[GlyphAtomsR] = &AtenSpace::Commands::function_GlyphAtomsR;
	pointers_[GlyphAtomsV] = &AtenSpace::Commands::function_GlyphAtomsV;
	pointers_[GlyphColour] = &AtenSpace::Commands::function_GlyphColour;
	pointers_[GlyphColours] = &AtenSpace::Commands::function_GlyphColours;
	pointers_[GlyphData] = &AtenSpace::Commands::function_GlyphData;
	pointers_[GlyphSolid] = &AtenSpace::Commands::function_GlyphSolid;
	pointers_[GlyphText] = &AtenSpace::Commands::function_GlyphText;
	pointers_[NewGlyph] = &AtenSpace::Commands::function_NewGlyph;

	// Grid Commands
	pointers_[AddFreePoint] = &AtenSpace::Commands::function_AddFreePoint;
	pointers_[AddGridPoint] = &AtenSpace::Commands::function_AddGridPoint;
	pointers_[AddNextGridPoint] = &AtenSpace::Commands::function_AddNextGridPoint;
	pointers_[CurrentGrid] = &AtenSpace::Commands::function_CurrentGrid;
	pointers_[FinaliseGrid] = &AtenSpace::Commands::function_FinaliseGrid;
	pointers_[GetGrid] = &AtenSpace::Commands::function_GetGrid;
	pointers_[GridAlpha] = &AtenSpace::Commands::function_GridAlpha;
	pointers_[GridAxes] = &AtenSpace::Commands::function_GridAxes;
	pointers_[GridColour] = &AtenSpace::Commands::function_GridColour;
	pointers_[GridColourSecondary] = &AtenSpace::Commands::function_GridColourSecondary;
	pointers_[GridColourscale] = &AtenSpace::Commands::function_GridColourscale;
	pointers_[GridCubic] = &AtenSpace::Commands::function_GridCubic;
	pointers_[GridCutoff] = &AtenSpace::Commands::function_GridCutoff;
	pointers_[GridCutoffSecondary] = &AtenSpace::Commands::function_GridCutoffSecondary;
	pointers_[GridLoopOrder] = &AtenSpace::Commands::function_GridLoopOrder;
	pointers_[GridOrigin] = &AtenSpace::Commands::function_GridOrigin;
	pointers_[GridOrtho] = &AtenSpace::Commands::function_GridOrtho;
	pointers_[GridOutline] = &AtenSpace::Commands::function_GridOutline;
	pointers_[GridPeriodic] = &AtenSpace::Commands::function_GridPeriodic;
	pointers_[InitialiseGrid] = &AtenSpace::Commands::function_InitialiseGrid;
	pointers_[GridSecondary] = &AtenSpace::Commands::function_GridSecondary;
	pointers_[GridStyle] = &AtenSpace::Commands::function_GridStyle;
	pointers_[GridStyleSecondary] = &AtenSpace::Commands::function_GridStyle;
	pointers_[GridUseZ] = &AtenSpace::Commands::function_GridUseZ;
	pointers_[GridViewPercentage] = &AtenSpace::Commands::function_GridViewPercentage;
	pointers_[GridViewPercentageSecondary] = &AtenSpace::Commands::function_GridViewPercentageSecondary;
	pointers_[GridVisible] = &AtenSpace::Commands::function_GridVisible;
	pointers_[LoadGrid] = &AtenSpace::Commands::function_LoadGrid;
	pointers_[NewGrid] = &AtenSpace::Commands::function_NewGrid;

	// Image Commands
	pointers_[SaveBitmap] = &AtenSpace::Commands::function_SaveBitmap;

	// Labeling Commands
	pointers_[ClearLabels] = &AtenSpace::Commands::function_ClearLabels;
	pointers_[Label] = &AtenSpace::Commands::function_Label;
	pointers_[RemoveLabel] = &AtenSpace::Commands::function_RemoveLabel;
	pointers_[RemoveLabels] = &AtenSpace::Commands::function_RemoveLabels;

	// Math Commands
	pointers_[Abs] = &AtenSpace::Commands::function_Abs;
	pointers_[ACos] = &AtenSpace::Commands::function_ACos;
	pointers_[ASin] = &AtenSpace::Commands::function_ASin;
	pointers_[ATan] = &AtenSpace::Commands::function_ATan;
	pointers_[Cos] = &AtenSpace::Commands::function_Cos;
	pointers_[DotProduct] = &AtenSpace::Commands::function_DotProduct;
	pointers_[Exp] = &AtenSpace::Commands::function_Exp;
	pointers_[Ln] = &AtenSpace::Commands::function_Ln;
	pointers_[Log] = &AtenSpace::Commands::function_Log;
	pointers_[Nint] = &AtenSpace::Commands::function_Nint;
	pointers_[Normalise] = &AtenSpace::Commands::function_Normalise;
	pointers_[Random] = &AtenSpace::Commands::function_Random;
	pointers_[Randomi] = &AtenSpace::Commands::function_Randomi;
	pointers_[Sin] = &AtenSpace::Commands::function_Sin;
	pointers_[Sqrt] = &AtenSpace::Commands::function_Sqrt;
	pointers_[Tan] = &AtenSpace::Commands::function_Tan;

	// MC Commands
	pointers_[PrintMC] = &AtenSpace::Commands::function_PrintMC;

	// Measurement Commands
	pointers_[ClearMeasurements] = &AtenSpace::Commands::function_ClearMeasurements;
	pointers_[GeometryCalc] = &AtenSpace::Commands::function_GeometryCalc;
	pointers_[ListMeasurements] = &AtenSpace::Commands::function_ListMeasurements;
	pointers_[Measure] = &AtenSpace::Commands::function_Measure;
	pointers_[MeasureSelected] = &AtenSpace::Commands::function_MeasureSelected;

	// Messaging Commands
	pointers_[CreateDialog] = &AtenSpace::Commands::function_CreateDialog;
	pointers_[DefaultDialog] = &AtenSpace::Commands::function_DefaultDialog;
	pointers_[Error] = &AtenSpace::Commands::function_Error;
	pointers_[Message] = &AtenSpace::Commands::function_Message;
	pointers_[Printf] = &AtenSpace::Commands::function_Printf;
	pointers_[ShowDefaultDialog] = &AtenSpace::Commands::function_ShowDefaultDialog;
	pointers_[Verbose] = &AtenSpace::Commands::function_Verbose;

	// Minimisation Commands
	pointers_[CGMinimise] = &AtenSpace::Commands::function_CGMinimise;
	pointers_[MCMinimise] = &AtenSpace::Commands::function_MCMinimise;
	pointers_[MopacMinimise] = &AtenSpace::Commands::function_MopacMinimise;
	pointers_[SDMinimise] = &AtenSpace::Commands::function_SDMinimise;
	
	// Model Commands
	pointers_[CreateAtoms] = &AtenSpace::Commands::function_CreateAtoms;
	pointers_[CurrentModel] = &AtenSpace::Commands::function_CurrentModel;
	pointers_[DeleteModel] = &AtenSpace::Commands::function_DeleteModel;
	pointers_[FinaliseModel] = &AtenSpace::Commands::function_FinaliseModel;
	pointers_[FirstModel] = &AtenSpace::Commands::function_FirstModel;
	pointers_[GetModel] = &AtenSpace::Commands::function_GetModel;
	pointers_[LastModel] = &AtenSpace::Commands::function_LastModel;
	pointers_[ListModels] = &AtenSpace::Commands::function_ListModels;
	pointers_[LoadModel] = &AtenSpace::Commands::function_LoadModel;
	pointers_[LogInfo] = &AtenSpace::Commands::function_LogInfo;
	pointers_[ModelTemplate] = &AtenSpace::Commands::function_ModelTemplate;
	pointers_[NewModel] = &AtenSpace::Commands::function_NewModel;
	pointers_[NextModel] = &AtenSpace::Commands::function_NextModel;
	pointers_[ParentModel] = &AtenSpace::Commands::function_ParentModel;
	pointers_[PrevModel] = &AtenSpace::Commands::function_PrevModel;
	pointers_[Info] = &AtenSpace::Commands::function_Info;
	pointers_[SaveModel] = &AtenSpace::Commands::function_SaveModel;
	pointers_[SaveSelection] = &AtenSpace::Commands::function_SaveSelection;
	pointers_[SetName] = &AtenSpace::Commands::function_SetName;
	pointers_[ShowAll] = &AtenSpace::Commands::function_ShowAll;

	// Model Extras Commands
	pointers_[NewBasisShell] = &AtenSpace::Commands::function_NewBasisShell;
	pointers_[NewEigenvector] = &AtenSpace::Commands::function_NewEigenvector;
	pointers_[NewVibration] = &AtenSpace::Commands::function_NewVibration;
	pointers_[PrintZMatrix] = &AtenSpace::Commands::function_PrintZMatrix;

	// Pattern Commands
	pointers_[ClearPatterns] = &AtenSpace::Commands::function_ClearPatterns;
	pointers_[CreatePatterns] = &AtenSpace::Commands::function_CreatePatterns;
	pointers_[CurrentPattern] = &AtenSpace::Commands::function_CurrentPattern;
	pointers_[FixPattern] = &AtenSpace::Commands::function_FixPattern;
	pointers_[GetPattern] = &AtenSpace::Commands::function_GetPattern;
	pointers_[ListPatterns] = &AtenSpace::Commands::function_ListPatterns;
	pointers_[NewPattern] = &AtenSpace::Commands::function_NewPattern;

	// Read / Write Commands
	pointers_[AddReadOption] = &AtenSpace::Commands::function_AddReadOption;
	pointers_[FilterFileName] = &AtenSpace::Commands::function_FilterFileName;
	pointers_[Eof] = &AtenSpace::Commands::function_Eof;
	pointers_[Find] = &AtenSpace::Commands::function_Find;
	pointers_[GetLine] = &AtenSpace::Commands::function_GetLine;
	pointers_[NextArg] = &AtenSpace::Commands::function_NextArg;
	pointers_[NextVariableArg] = &AtenSpace::Commands::function_NextVariableArg;
	pointers_[PeekChar] = &AtenSpace::Commands::function_PeekChar;
	pointers_[PeekCharI] = &AtenSpace::Commands::function_PeekCharI;
	pointers_[ReadChars] = &AtenSpace::Commands::function_ReadChars;
	pointers_[ReadDouble] = &AtenSpace::Commands::function_ReadDouble;
	pointers_[ReadDoubleArray] = &AtenSpace::Commands::function_ReadDoubleArray;
	pointers_[ReadInteger] = &AtenSpace::Commands::function_ReadInteger;
	pointers_[ReadIntegerArray] = &AtenSpace::Commands::function_ReadIntegerArray;
	pointers_[ReadLine] = &AtenSpace::Commands::function_ReadLine;
	pointers_[ReadLineFormatted] = &AtenSpace::Commands::function_ReadLineFormatted;
	pointers_[ReadNext] = &AtenSpace::Commands::function_ReadNext;
	pointers_[ReadVariable] = &AtenSpace::Commands::function_ReadVariable;
	pointers_[ReadVariableFormatted] = &AtenSpace::Commands::function_ReadVariableFormatted;
	pointers_[RemoveReadOption] = &AtenSpace::Commands::function_RemoveReadOption;
	pointers_[Rewind] = &AtenSpace::Commands::function_Rewind;
	pointers_[SkipChars] = &AtenSpace::Commands::function_SkipChars;
	pointers_[SkipLine] = &AtenSpace::Commands::function_SkipLine;
	pointers_[WriteLine] = &AtenSpace::Commands::function_WriteLine;
	pointers_[WriteLineFormatted] = &AtenSpace::Commands::function_WriteLineFormatted;
	pointers_[WriteVariable] = &AtenSpace::Commands::function_WriteVariable;
	pointers_[WriteVariableFormatted] = &AtenSpace::Commands::function_WriteVariableFormatted;

	// Pores Commands
	pointers_[CreateScheme] = &AtenSpace::Commands::function_CreateScheme;
	pointers_[DrillPores] = &AtenSpace::Commands::function_DrillPores;
	pointers_[SelectPores] = &AtenSpace::Commands::function_SelectPores;
	pointers_[Terminate] = &AtenSpace::Commands::function_Terminate;

	// Script Commands
	pointers_[ListScripts] = &AtenSpace::Commands::function_ListScripts;
	pointers_[LoadScript] = &AtenSpace::Commands::function_LoadScript;
	pointers_[RunScript] = &AtenSpace::Commands::function_RunScript;

	// Select Commands
	pointers_[DeSelect] = &AtenSpace::Commands::function_DeSelect;
	pointers_[DeSelectCode] = &AtenSpace::Commands::function_DeSelectCode;
	pointers_[DeSelectFormatted] = &AtenSpace::Commands::function_DeSelectFormatted;
	pointers_[DeSelectType] = &AtenSpace::Commands::function_DeSelectType;
	pointers_[Expand] = &AtenSpace::Commands::function_Expand;
	pointers_[Invert] = &AtenSpace::Commands::function_Invert;
	pointers_[Select] = &AtenSpace::Commands::function_Select;
	pointers_[SelectAll] = &AtenSpace::Commands::function_SelectAll;
	pointers_[SelectCode] = &AtenSpace::Commands::function_SelectCode;
	pointers_[SelectFFType] = &AtenSpace::Commands::function_SelectFFType;
	pointers_[SelectFormatted] = &AtenSpace::Commands::function_SelectFormatted;
	pointers_[SelectInsideCell] = &AtenSpace::Commands::function_SelectInsideCell;
	pointers_[SelectionCog] = &AtenSpace::Commands::function_SelectionCog;
	pointers_[SelectionCom] = &AtenSpace::Commands::function_SelectionCom;
	pointers_[SelectLine] = &AtenSpace::Commands::function_SelectLine;
	pointers_[SelectMiller] = &AtenSpace::Commands::function_SelectMiller;
	pointers_[SelectMolecule] = &AtenSpace::Commands::function_SelectMolecule;
	pointers_[SelectNone] = &AtenSpace::Commands::function_SelectNone;
	pointers_[SelectOverlaps] = &AtenSpace::Commands::function_SelectOverlaps;
	pointers_[SelectOutsideCell] = &AtenSpace::Commands::function_SelectOutsideCell;
	pointers_[SelectPattern] = &AtenSpace::Commands::function_SelectPattern;
	pointers_[SelectRadial] = &AtenSpace::Commands::function_SelectRadial;
	pointers_[SelectTree] = &AtenSpace::Commands::function_SelectTree;
	pointers_[SelectType] = &AtenSpace::Commands::function_SelectType;
	pointers_[TestSelect] = &AtenSpace::Commands::function_TestSelect;
	
	// Site Commands
	pointers_[GetSite] = &AtenSpace::Commands::function_GetSite;
	pointers_[ListSites] = &AtenSpace::Commands::function_ListSites;
	pointers_[NewSite] = &AtenSpace::Commands::function_NewSite;
	pointers_[SiteAxes] = &AtenSpace::Commands::function_SiteAxes;

	// String Commands
	pointers_[AfterStr] = &AtenSpace::Commands::function_AfterStr;
	pointers_[AToF] = &AtenSpace::Commands::function_AToF;
	pointers_[AToI] = &AtenSpace::Commands::function_AToI;
	pointers_[BeforeStr] = &AtenSpace::Commands::function_BeforeStr;
	pointers_[Contains] = &AtenSpace::Commands::function_Contains;
	pointers_[FToA] = &AtenSpace::Commands::function_FToA;
	pointers_[IToA] = &AtenSpace::Commands::function_IToA;
	pointers_[Lowercase] = &AtenSpace::Commands::function_Lowercase;
	pointers_[ReplaceChars] = &AtenSpace::Commands::function_ReplaceChars;
	pointers_[ReplaceStr] = &AtenSpace::Commands::function_ReplaceStr;
	pointers_[RemoveStr] = &AtenSpace::Commands::function_RemoveStr;
	pointers_[SPrintF] = &AtenSpace::Commands::function_SPrintF;
	pointers_[StripChars] = &AtenSpace::Commands::function_StripChars;
	pointers_[SubStr] = &AtenSpace::Commands::function_SubStr;
	pointers_[ToA] = &AtenSpace::Commands::function_ToA;
	pointers_[Uppercase] = &AtenSpace::Commands::function_Uppercase;

	// System Commands
	pointers_[Debug] = &AtenSpace::Commands::function_Debug;
	pointers_[Getenv] = &AtenSpace::Commands::function_Getenv;
	pointers_[Getenvf] = &AtenSpace::Commands::function_Getenvf;
	pointers_[Getenvi] = &AtenSpace::Commands::function_Getenvi;
	pointers_[Help] = &AtenSpace::Commands::function_Help;
	pointers_[Null] = &AtenSpace::Commands::function_Null;
	pointers_[Quit] = &AtenSpace::Commands::function_Quit;
	pointers_[SearchCommands] = &AtenSpace::Commands::function_SearchCommands;
	pointers_[Seed] = &AtenSpace::Commands::function_Seed;
	pointers_[Version] = &AtenSpace::Commands::function_Version;
	
	// Trajectory Commands
	pointers_[AddFrame] = &AtenSpace::Commands::function_AddFrame;
	pointers_[ClearTrajectory] = &AtenSpace::Commands::function_ClearTrajectory;
	pointers_[FinaliseFrame] = &AtenSpace::Commands::function_FinaliseFrame;
	pointers_[FirstFrame] = &AtenSpace::Commands::function_FirstFrame;
	pointers_[LastFrame] = &AtenSpace::Commands::function_LastFrame;
	pointers_[LoadTrajectory] = &AtenSpace::Commands::function_LoadTrajectory;
	pointers_[NextFrame] = &AtenSpace::Commands::function_NextFrame;
	pointers_[PrevFrame] = &AtenSpace::Commands::function_PrevFrame;
	pointers_[SeekFrame] = &AtenSpace::Commands::function_SeekFrame;

	// Transform Commands
	pointers_[AxisRotate] = &AtenSpace::Commands::function_AxisRotate;
	pointers_[Centre] = &AtenSpace::Commands::function_Centre;
	pointers_[FlipX] = &AtenSpace::Commands::function_FlipX;
	pointers_[FlipY] = &AtenSpace::Commands::function_FlipY;
	pointers_[FlipZ] = &AtenSpace::Commands::function_FlipZ;
	pointers_[MatrixConvert] = &AtenSpace::Commands::function_MatrixConvert;
	pointers_[MatrixTransform] = &AtenSpace::Commands::function_MatrixTransform;
	pointers_[Mirror] = &AtenSpace::Commands::function_Mirror;
	pointers_[Reorient] = &AtenSpace::Commands::function_Reorient;
	pointers_[Translate] = &AtenSpace::Commands::function_Translate;
	pointers_[TranslateAtom] = &AtenSpace::Commands::function_TranslateAtom;
	pointers_[TranslateCell] = &AtenSpace::Commands::function_TranslateCell;

	// View Commands
	pointers_[AxisRotateView] = &AtenSpace::Commands::function_AxisRotateView;
	pointers_[GetView] = &AtenSpace::Commands::function_GetView;
	pointers_[Orthographic] = &AtenSpace::Commands::function_Orthographic;
	pointers_[Perspective] = &AtenSpace::Commands::function_Perspective;
	pointers_[ResetView] = &AtenSpace::Commands::function_ResetView;
	pointers_[RotateView] = &AtenSpace::Commands::function_RotateView;
	pointers_[SetView] = &AtenSpace::Commands::function_SetView;
	pointers_[SpeedTest] = &AtenSpace::Commands::function_SpeedTest;
	pointers_[TranslateView] = &AtenSpace::Commands::function_TranslateView;
	pointers_[ViewAlong] = &AtenSpace::Commands::function_ViewAlong;
	pointers_[ViewAlongCell] = &AtenSpace::Commands::function_ViewAlongCell;
	pointers_[ZoomView] = &AtenSpace::Commands::function_ZoomView;
	pointers_[ZRotateView] = &AtenSpace::Commands::function_ZRotateView;
}
