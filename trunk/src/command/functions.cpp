/*
	*** Command Function Pointers
	*** src/command/functions.cpp
	Copyright T. Youngs 2007-2015

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
	pointers_[Finalise] = &AtenSpace::Commands::Commands::function_Finalise;
	pointers_[FrameAnalyse] = &AtenSpace::Commands::Commands::function_FrameAnalyse;
	pointers_[Geometric] = &AtenSpace::Commands::Commands::function_Geometric;
	pointers_[ModelAnalyse] = &AtenSpace::Commands::Commands::function_ModelAnalyse;
	pointers_[PDens] = &AtenSpace::Commands::Commands::function_PDens;
	pointers_[PrintJobs] = &AtenSpace::Commands::Commands::function_PrintJobs;
	pointers_[RDF] = &AtenSpace::Commands::Commands::function_RDF;
	pointers_[SaveQuantities] = &AtenSpace::Commands::Commands::function_SaveQuantities;
	pointers_[TrajAnalyse] = &AtenSpace::Commands::Commands::function_TrajAnalyse;

	// Atom commands
	pointers_[AtomStyle] = &AtenSpace::Commands::Commands::function_AtomStyle;
	pointers_[ColourAtoms] = &AtenSpace::Commands::Commands::function_ColourAtoms;
	pointers_[CurrentAtom] = &AtenSpace::Commands::Commands::function_CurrentAtom;
	pointers_[Fix] = &AtenSpace::Commands::Commands::function_Fix;
	pointers_[Free] = &AtenSpace::Commands::Commands::function_Free;
	pointers_[GetAtom] = &AtenSpace::Commands::Commands::function_GetAtom;
	pointers_[Hide] = &AtenSpace::Commands::Commands::function_Hide;
	pointers_[RecolourAtoms] = &AtenSpace::Commands::Commands::function_RecolourAtoms;
	pointers_[SetCoords] = &AtenSpace::Commands::Commands::function_SetCoords;
	pointers_[SetCharge] = &AtenSpace::Commands::Commands::function_SetCharge;
	pointers_[SetElement] = &AtenSpace::Commands::Commands::function_SetElement;
	pointers_[SetForces] = &AtenSpace::Commands::Commands::function_SetForces;
	pointers_[SetFX] = &AtenSpace::Commands::Commands::function_SetFX;
	pointers_[SetFY] = &AtenSpace::Commands::Commands::function_SetFY;
	pointers_[SetFZ] = &AtenSpace::Commands::Commands::function_SetFZ;
	pointers_[SetRX] = &AtenSpace::Commands::Commands::function_SetRX;
	pointers_[SetRY] = &AtenSpace::Commands::Commands::function_SetRY;
	pointers_[SetRZ] = &AtenSpace::Commands::Commands::function_SetRZ;
	pointers_[SetVelocities] = &AtenSpace::Commands::Commands::function_SetVelocities;
	pointers_[SetVX] = &AtenSpace::Commands::Commands::function_SetVX;
	pointers_[SetVY] = &AtenSpace::Commands::Commands::function_SetVY;
	pointers_[SetVZ] = &AtenSpace::Commands::Commands::function_SetVZ;
	pointers_[Show] = &AtenSpace::Commands::Commands::function_Show;

	// Bond commands
	pointers_[Augment] = &AtenSpace::Commands::Commands::function_Augment;
	pointers_[BondTolerance] = &AtenSpace::Commands::Commands::function_BondTolerance;
	pointers_[ClearBonds] = &AtenSpace::Commands::Commands::function_ClearBonds;
	pointers_[ClearSelectedBonds] = &AtenSpace::Commands::Commands::function_ClearSelectedBonds;
	pointers_[NewBond] = &AtenSpace::Commands::Commands::function_NewBond;
	pointers_[ReBond] = &AtenSpace::Commands::Commands::function_ReBond;
	pointers_[ReBondPatterns] = &AtenSpace::Commands::Commands::function_ReBondPatterns;
	pointers_[ReBondSelection] = &AtenSpace::Commands::Commands::function_ReBondSelection;

	// Build commands
	pointers_[AddHydrogen] = &AtenSpace::Commands::Commands::function_AddHydrogen;
	pointers_[Bohr] = &AtenSpace::Commands::Commands::function_Bohr;
	pointers_[Chain] = &AtenSpace::Commands::Commands::function_Chain;
	pointers_[EndChain] = &AtenSpace::Commands::Commands::function_EndChain;
	pointers_[GrowAtom] = &AtenSpace::Commands::Commands::function_GrowAtom;
	pointers_[InsertAtom] = &AtenSpace::Commands::Commands::function_InsertAtom;
	pointers_[Locate] = &AtenSpace::Commands::Commands::function_Locate;
	pointers_[Move] = &AtenSpace::Commands::Commands::function_Move;
	pointers_[MoveToEnd] = &AtenSpace::Commands::Commands::function_MoveToEnd;
	pointers_[MoveToStart] = &AtenSpace::Commands::Commands::function_MoveToStart;
	pointers_[NewAtom] = &AtenSpace::Commands::Commands::function_NewAtom;
	pointers_[NewAtomFrac] = &AtenSpace::Commands::Commands::function_NewAtomFrac;
	pointers_[ReOrder] = &AtenSpace::Commands::Commands::function_ReOrder;
	pointers_[ResetPen] = &AtenSpace::Commands::Commands::function_ResetPen;
	pointers_[RotX] = &AtenSpace::Commands::Commands::function_RotX;
	pointers_[RotY] = &AtenSpace::Commands::Commands::function_RotY;
	pointers_[RotZ] = &AtenSpace::Commands::Commands::function_RotZ;
	pointers_[SelectionAddHydrogen] = &AtenSpace::Commands::Commands::function_SelectionAddHydrogen;
	pointers_[SelectionGrowAtom] = &AtenSpace::Commands::Commands::function_SelectionGrowAtom;
	pointers_[SetAngle] = &AtenSpace::Commands::Commands::function_SetAngle;
	pointers_[SetDistance] = &AtenSpace::Commands::Commands::function_SetDistance;
	pointers_[SetTorsion] = &AtenSpace::Commands::Commands::function_SetTorsion;
	pointers_[ShiftDown] = &AtenSpace::Commands::Commands::function_ShiftDown;
	pointers_[ShiftUp] = &AtenSpace::Commands::Commands::function_ShiftUp;
	pointers_[Transmute] = &AtenSpace::Commands::Commands::function_Transmute;

	// Cell commands
	pointers_[AddGenerator] = &AtenSpace::Commands::Commands::function_AddGenerator;
	pointers_[AdjustCell] = &AtenSpace::Commands::Commands::function_AdjustCell;
	pointers_[Cell] = &AtenSpace::Commands::Commands::function_Cell;
	pointers_[CellAxes] = &AtenSpace::Commands::Commands::function_CellAxes;
	pointers_[Fold] = &AtenSpace::Commands::Commands::function_Fold;
	pointers_[FoldMolecules] = &AtenSpace::Commands::Commands::function_FoldMolecules;
	pointers_[FracToReal] = &AtenSpace::Commands::Commands::function_FracToReal;
	pointers_[MillerCut] = &AtenSpace::Commands::Commands::function_MillerCut;
	pointers_[NoCell] = &AtenSpace::Commands::Commands::function_NoCell;
	pointers_[Pack] = &AtenSpace::Commands::Commands::function_Pack;
	pointers_[PrintCell] = &AtenSpace::Commands::Commands::function_PrintCell;
	pointers_[Replicate] = &AtenSpace::Commands::Commands::function_Replicate;
	pointers_[RotateCell] = &AtenSpace::Commands::Commands::function_RotateCell;
	pointers_[Scale] = &AtenSpace::Commands::Commands::function_Scale;
	pointers_[ScaleMolecules] = &AtenSpace::Commands::Commands::function_ScaleMolecules;
	pointers_[SetCell] = &AtenSpace::Commands::Commands::function_SetCell;
	pointers_[SGInfo] = &AtenSpace::Commands::Commands::function_SGInfo;
	pointers_[Spacegroup] = &AtenSpace::Commands::Commands::function_Spacegroup;

	// Charge commands
	pointers_[Charge] = &AtenSpace::Commands::Commands::function_Charge;
	pointers_[ChargeFF] = &AtenSpace::Commands::Commands::function_ChargeFF;
	pointers_[ChargeFromModel] = &AtenSpace::Commands::Commands::function_ChargeFromModel;
	pointers_[ChargePAtom] = &AtenSpace::Commands::Commands::function_ChargePAtom;
	pointers_[ChargeType] = &AtenSpace::Commands::Commands::function_ChargeType;
	pointers_[ClearCharges] = &AtenSpace::Commands::Commands::function_ClearCharges;

	// Colourscale commands
	pointers_[AddPoint] = &AtenSpace::Commands::Commands::function_AddPoint;
	pointers_[ClearPoints] = &AtenSpace::Commands::Commands::function_ClearPoints;
	pointers_[ListScales] = &AtenSpace::Commands::Commands::function_ListScales;
	pointers_[RemovePoint] = &AtenSpace::Commands::Commands::function_RemovePoint;
	pointers_[ScaleInterpolate] = &AtenSpace::Commands::Commands::function_ScaleInterpolate;
	pointers_[ScaleName] = &AtenSpace::Commands::Commands::function_ScaleName;
	pointers_[ScaleVisible] = &AtenSpace::Commands::Commands::function_ScaleVisible;
	pointers_[SetPoint] = &AtenSpace::Commands::Commands::function_SetPoint;
	pointers_[SetPointColour] = &AtenSpace::Commands::Commands::function_SetPointColour;
	pointers_[SetPointValue] = &AtenSpace::Commands::Commands::function_SetPointValue;

	// Disordered build commands
	pointers_[Disorder] = &AtenSpace::Commands::Commands::function_Disorder;
	pointers_[ListComponents] = &AtenSpace::Commands::Commands::function_ListComponents;
	pointers_[SetupComponent] = &AtenSpace::Commands::Commands::function_SetupComponent;

	// Edit Commands
	pointers_[Delete] = &AtenSpace::Commands::Commands::function_Delete;
	pointers_[Copy] = &AtenSpace::Commands::Commands::function_Copy;
	pointers_[Cut] = &AtenSpace::Commands::Commands::function_Cut;
	pointers_[Paste] = &AtenSpace::Commands::Commands::function_Paste;
	pointers_[Redo] = &AtenSpace::Commands::Commands::function_Redo;
	pointers_[Undo] = &AtenSpace::Commands::Commands::function_Undo;

	// Energy Commands
	pointers_[Electrostatics] = &AtenSpace::Commands::Commands::function_Electrostatics;
	pointers_[FrameEnergy] = &AtenSpace::Commands::Commands::function_FrameEnergy;
	pointers_[ModelEnergy] = &AtenSpace::Commands::Commands::function_ModelEnergy;
	pointers_[PrintElec] = &AtenSpace::Commands::Commands::function_PrintElec;
	pointers_[PrintEwald] = &AtenSpace::Commands::Commands::function_PrintEwald;
	pointers_[PrintInter] = &AtenSpace::Commands::Commands::function_PrintInter;
	pointers_[PrintIntra] = &AtenSpace::Commands::Commands::function_PrintIntra;
	pointers_[PrintEnergy] = &AtenSpace::Commands::Commands::function_PrintEnergy;
	pointers_[PrintSummary] = &AtenSpace::Commands::Commands::function_PrintSummary;
	pointers_[PrintVdw] = &AtenSpace::Commands::Commands::function_PrintVdw;

	// Flow control
	pointers_[If] = &AtenSpace::Commands::function_If;
	pointers_[Break] = &AtenSpace::Commands::Commands::function_Break;
	pointers_[Case] = &AtenSpace::Commands::Commands::function_Case;
	pointers_[Continue] = &AtenSpace::Commands::Commands::function_Continue;
	pointers_[Default] = &AtenSpace::Commands::Commands::function_Default;
	pointers_[DoWhile] = &AtenSpace::Commands::Commands::function_DoWhile;
	pointers_[For] = &AtenSpace::Commands::Commands::function_For;
	pointers_[ForIn] = &AtenSpace::Commands::Commands::function_ForIn;
	pointers_[If] = &AtenSpace::Commands::Commands::function_If;
	pointers_[Return] = &AtenSpace::Commands::Commands::function_Return;
	pointers_[Switch] = &AtenSpace::Commands::Commands::function_Switch;
	pointers_[While] = &AtenSpace::Commands::Commands::function_While;

	// Force Commands
	pointers_[FrameForces] = &AtenSpace::Commands::Commands::function_FrameForces;
	pointers_[ModelForces] = &AtenSpace::Commands::Commands::function_ModelForces;
	pointers_[PrintForces] = &AtenSpace::Commands::Commands::function_PrintForces;

	// Forcefield Commands
        pointers_[AngleDef] = &AtenSpace::Commands::Commands::function_AngleDef;
	pointers_[AutoConversionUnit] = &AtenSpace::Commands::Commands::function_AutoConversionUnit;
	pointers_[BondDef] = &AtenSpace::Commands::Commands::function_BondDef;
	pointers_[ClearExportMap] = &AtenSpace::Commands::Commands::function_ClearExportMap;
	pointers_[ClearExpression] = &AtenSpace::Commands::Commands::function_ClearExpression;
	pointers_[ClearMap] = &AtenSpace::Commands::Commands::function_ClearMap;
	pointers_[ClearTypes] = &AtenSpace::Commands::Commands::function_ClearTypes;
	pointers_[CreateExpression] = &AtenSpace::Commands::Commands::function_CreateExpression;
	pointers_[CurrentFF] = &AtenSpace::Commands::Commands::function_CurrentFF;
	pointers_[DeleteFF] = &AtenSpace::Commands::Commands::function_DeleteFF;
	pointers_[EnergyConvert] = &AtenSpace::Commands::Commands::function_EnergyConvert;
	pointers_[Equivalent] = &AtenSpace::Commands::Commands::function_Equivalent;
	pointers_[ExportMap] = &AtenSpace::Commands::Commands::function_ExportMap;
	pointers_[FFModel] = &AtenSpace::Commands::Commands::function_FFModel;
	pointers_[FFPattern] = &AtenSpace::Commands::Commands::function_FFPattern;
	pointers_[FinaliseFF] = &AtenSpace::Commands::Commands::function_FinaliseFF;
	pointers_[FixType] = &AtenSpace::Commands::Commands::function_FixType;
	pointers_[FreeType] = &AtenSpace::Commands::Commands::function_FreeType;
	pointers_[GenerateAngle] = &AtenSpace::Commands::Commands::function_GenerateAngle;
	pointers_[GenerateBond] = &AtenSpace::Commands::Commands::function_GenerateBond;
	pointers_[GenerateTorsion] = &AtenSpace::Commands::Commands::function_GenerateTorsion;
	pointers_[GenerateVdw] = &AtenSpace::Commands::Commands::function_GenerateVdw;
	pointers_[GetCombinationRule] = &AtenSpace::Commands::Commands::function_GetCombinationRule;
	pointers_[GetFF] = &AtenSpace::Commands::Commands::function_GetFF;
	pointers_[InterDef] = &AtenSpace::Commands::Commands::function_InterDef;
	pointers_[LoadFF] = &AtenSpace::Commands::Commands::function_LoadFF;
	pointers_[Map] = &AtenSpace::Commands::Commands::function_Map;
	pointers_[NewFF] = &AtenSpace::Commands::Commands::function_NewFF;
	pointers_[PrintSetup] = &AtenSpace::Commands::Commands::function_PrintSetup;
	pointers_[PrintType] = &AtenSpace::Commands::Commands::function_PrintType;
	pointers_[RecreateExpression] = &AtenSpace::Commands::Commands::function_RecreateExpression;
	pointers_[SaveExpression] = &AtenSpace::Commands::Commands::function_SaveExpression;
	pointers_[SetCombinationRule] = &AtenSpace::Commands::Commands::function_SetCombinationRule;
	pointers_[TorsionDef] = &AtenSpace::Commands::Commands::function_TorsionDef;
	pointers_[TypeDef] = &AtenSpace::Commands::Commands::function_TypeDef;
	pointers_[TypeModel] = &AtenSpace::Commands::Commands::function_TypeModel;
	pointers_[TypeTest] = &AtenSpace::Commands::Commands::function_TypeTest;
	pointers_[Units] = &AtenSpace::Commands::Commands::function_Units;

	// Glyph commands
	pointers_[AutoEllipsoids] = &AtenSpace::Commands::Commands::function_AutoEllipsoids;
	pointers_[AutoPolyhedra] = &AtenSpace::Commands::Commands::function_AutoPolyhedra;
	pointers_[GlyphAtomF] = &AtenSpace::Commands::Commands::function_GlyphAtomF;
	pointers_[GlyphAtomR] = &AtenSpace::Commands::Commands::function_GlyphAtomR;
	pointers_[GlyphAtomV] = &AtenSpace::Commands::Commands::function_GlyphAtomV;
	pointers_[GlyphAtomsF] = &AtenSpace::Commands::Commands::function_GlyphAtomsF;
	pointers_[GlyphAtomsR] = &AtenSpace::Commands::Commands::function_GlyphAtomsR;
	pointers_[GlyphAtomsV] = &AtenSpace::Commands::Commands::function_GlyphAtomsV;
	pointers_[GlyphColour] = &AtenSpace::Commands::Commands::function_GlyphColour;
	pointers_[GlyphColours] = &AtenSpace::Commands::Commands::function_GlyphColours;
	pointers_[GlyphData] = &AtenSpace::Commands::Commands::function_GlyphData;
	pointers_[GlyphSolid] = &AtenSpace::Commands::Commands::function_GlyphSolid;
	pointers_[GlyphText] = &AtenSpace::Commands::Commands::function_GlyphText;
	pointers_[NewGlyph] = &AtenSpace::Commands::Commands::function_NewGlyph;

	// Grid Commands
	pointers_[AddFreePoint] = &AtenSpace::Commands::Commands::function_AddFreePoint;
	pointers_[AddGridPoint] = &AtenSpace::Commands::Commands::function_AddGridPoint;
	pointers_[AddNextGridPoint] = &AtenSpace::Commands::Commands::function_AddNextGridPoint;
	pointers_[CurrentGrid] = &AtenSpace::Commands::Commands::function_CurrentGrid;
	pointers_[FinaliseGrid] = &AtenSpace::Commands::Commands::function_FinaliseGrid;
	pointers_[GetGrid] = &AtenSpace::Commands::Commands::function_GetGrid;
	pointers_[GridAlpha] = &AtenSpace::Commands::Commands::function_GridAlpha;
	pointers_[GridAxes] = &AtenSpace::Commands::Commands::function_GridAxes;
	pointers_[GridColour] = &AtenSpace::Commands::Commands::function_GridColour;
	pointers_[GridColourSecondary] = &AtenSpace::Commands::Commands::function_GridColourSecondary;
	pointers_[GridColourscale] = &AtenSpace::Commands::Commands::function_GridColourscale;
	pointers_[GridCubic] = &AtenSpace::Commands::Commands::function_GridCubic;
	pointers_[GridCutoff] = &AtenSpace::Commands::Commands::function_GridCutoff;
	pointers_[GridCutoffSecondary] = &AtenSpace::Commands::Commands::function_GridCutoffSecondary;
	pointers_[GridLoopOrder] = &AtenSpace::Commands::Commands::function_GridLoopOrder;
	pointers_[GridOrigin] = &AtenSpace::Commands::Commands::function_GridOrigin;
	pointers_[GridOrtho] = &AtenSpace::Commands::Commands::function_GridOrtho;
	pointers_[InitialiseGrid] = &AtenSpace::Commands::Commands::function_InitialiseGrid;
	pointers_[GridSecondary] = &AtenSpace::Commands::Commands::function_GridSecondary;
	pointers_[GridStyle] = &AtenSpace::Commands::Commands::function_GridStyle;
	pointers_[GridUseZ] = &AtenSpace::Commands::Commands::function_GridUseZ;
	pointers_[GridVisible] = &AtenSpace::Commands::Commands::function_GridVisible;
	pointers_[LoadGrid] = &AtenSpace::Commands::Commands::function_LoadGrid;
	pointers_[NewGrid] = &AtenSpace::Commands::Commands::function_NewGrid;

	// Image Commands
	pointers_[SaveBitmap] = &AtenSpace::Commands::Commands::function_SaveBitmap;
	pointers_[SaveMovie] = &AtenSpace::Commands::Commands::function_SaveMovie;
	pointers_[SaveVibrationMovie] = &AtenSpace::Commands::Commands::function_SaveVibrationMovie;

	// Labeling Commands
	pointers_[ClearLabels] = &AtenSpace::Commands::Commands::function_ClearLabels;
	pointers_[Label] = &AtenSpace::Commands::Commands::function_Label;
	pointers_[RemoveLabel] = &AtenSpace::Commands::Commands::function_RemoveLabel;
	pointers_[RemoveLabels] = &AtenSpace::Commands::Commands::function_RemoveLabels;

	// Math Commands
	pointers_[Abs] = &AtenSpace::Commands::Commands::function_Abs;
	pointers_[ACos] = &AtenSpace::Commands::Commands::function_ACos;
	pointers_[ASin] = &AtenSpace::Commands::Commands::function_ASin;
	pointers_[ATan] = &AtenSpace::Commands::Commands::function_ATan;
	pointers_[Cos] = &AtenSpace::Commands::Commands::function_Cos;
	pointers_[DotProduct] = &AtenSpace::Commands::Commands::function_DotProduct;
	pointers_[Exp] = &AtenSpace::Commands::Commands::function_Exp;
	pointers_[Ln] = &AtenSpace::Commands::Commands::function_Ln;
	pointers_[Log] = &AtenSpace::Commands::Commands::function_Log;
	pointers_[Nint] = &AtenSpace::Commands::Commands::function_Nint;
	pointers_[Normalise] = &AtenSpace::Commands::Commands::function_Normalise;
	pointers_[Random] = &AtenSpace::Commands::Commands::function_Random;
	pointers_[Randomi] = &AtenSpace::Commands::Commands::function_Randomi;
	pointers_[Sin] = &AtenSpace::Commands::Commands::function_Sin;
	pointers_[Sqrt] = &AtenSpace::Commands::Commands::function_Sqrt;
	pointers_[Tan] = &AtenSpace::Commands::Commands::function_Tan;

	// MC Commands
	pointers_[PrintMC] = &AtenSpace::Commands::Commands::function_PrintMC;

	// Measurement Commands
	pointers_[ClearMeasurements] = &AtenSpace::Commands::Commands::function_ClearMeasurements;
	pointers_[GeometryCalc] = &AtenSpace::Commands::Commands::function_GeometryCalc;
	pointers_[ListMeasurements] = &AtenSpace::Commands::Commands::function_ListMeasurements;
	pointers_[Measure] = &AtenSpace::Commands::Commands::function_Measure;
	pointers_[MeasureSelected] = &AtenSpace::Commands::Commands::function_MeasureSelected;

	// Messaging Commands
	pointers_[CreateDialog] = &AtenSpace::Commands::Commands::function_CreateDialog;
	pointers_[DefaultDialog] = &AtenSpace::Commands::Commands::function_DefaultDialog;
	pointers_[Error] = &AtenSpace::Commands::Commands::function_Error;
	pointers_[Message] = &AtenSpace::Commands::Commands::function_Message;
	pointers_[Printf] = &AtenSpace::Commands::Commands::function_Printf;
	pointers_[ShowDefaultDialog] = &AtenSpace::Commands::Commands::function_ShowDefaultDialog;
	pointers_[Verbose] = &AtenSpace::Commands::Commands::function_Verbose;

	// Minimisation Commands
	pointers_[CGMinimise] = &AtenSpace::Commands::Commands::function_CGMinimise;
	pointers_[Converge] = &AtenSpace::Commands::Commands::function_Converge;
	pointers_[LineTolerance] = &AtenSpace::Commands::Commands::function_LineTolerance;
	pointers_[MCMinimise] = &AtenSpace::Commands::Commands::function_MCMinimise;
	pointers_[MopacMinimise] = &AtenSpace::Commands::Commands::function_MopacMinimise;
	pointers_[SDMinimise] = &AtenSpace::Commands::Commands::function_SDMinimise;
	
	// Model Commands
	pointers_[CreateAtoms] = &AtenSpace::Commands::Commands::function_CreateAtoms;
	pointers_[CurrentModel] = &AtenSpace::Commands::Commands::function_CurrentModel;
	pointers_[DeleteModel] = &AtenSpace::Commands::Commands::function_DeleteModel;
	pointers_[FinaliseModel] = &AtenSpace::Commands::Commands::function_FinaliseModel;
	pointers_[FirstModel] = &AtenSpace::Commands::Commands::function_FirstModel;
	pointers_[GetModel] = &AtenSpace::Commands::Commands::function_GetModel;
	pointers_[LastModel] = &AtenSpace::Commands::Commands::function_LastModel;
	pointers_[ListModels] = &AtenSpace::Commands::Commands::function_ListModels;
	pointers_[LoadModel] = &AtenSpace::Commands::Commands::function_LoadModel;
	pointers_[LogInfo] = &AtenSpace::Commands::Commands::function_LogInfo;
	pointers_[ModelTemplate] = &AtenSpace::Commands::Commands::function_ModelTemplate;
	pointers_[NewModel] = &AtenSpace::Commands::Commands::function_NewModel;
	pointers_[NextModel] = &AtenSpace::Commands::Commands::function_NextModel;
	pointers_[ParentModel] = &AtenSpace::Commands::Commands::function_ParentModel;
	pointers_[PrevModel] = &AtenSpace::Commands::Commands::function_PrevModel;
	pointers_[Info] = &AtenSpace::Commands::Commands::function_Info;
	pointers_[SaveModel] = &AtenSpace::Commands::Commands::function_SaveModel;
	pointers_[SaveSelection] = &AtenSpace::Commands::Commands::function_SaveSelection;
	pointers_[SetName] = &AtenSpace::Commands::Commands::function_SetName;
	pointers_[ShowAll] = &AtenSpace::Commands::Commands::function_ShowAll;

	// Model Extras Commands
	pointers_[NewBasisShell] = &AtenSpace::Commands::Commands::function_NewBasisShell;
	pointers_[NewEigenvector] = &AtenSpace::Commands::Commands::function_NewEigenvector;
	pointers_[NewVibration] = &AtenSpace::Commands::Commands::function_NewVibration;
	pointers_[PrintZMatrix] = &AtenSpace::Commands::Commands::function_PrintZMatrix;

	// Pattern Commands
	pointers_[ClearPatterns] = &AtenSpace::Commands::Commands::function_ClearPatterns;
	pointers_[CreatePatterns] = &AtenSpace::Commands::Commands::function_CreatePatterns;
	pointers_[CurrentPattern] = &AtenSpace::Commands::Commands::function_CurrentPattern;
	pointers_[FixPattern] = &AtenSpace::Commands::Commands::function_FixPattern;
	pointers_[GetPattern] = &AtenSpace::Commands::Commands::function_GetPattern;
	pointers_[ListPatterns] = &AtenSpace::Commands::Commands::function_ListPatterns;
	pointers_[NewPattern] = &AtenSpace::Commands::Commands::function_NewPattern;

	// Read / Write Commands
	pointers_[AddReadOption] = &AtenSpace::Commands::Commands::function_AddReadOption;
	pointers_[FilterFileName] = &AtenSpace::Commands::Commands::function_FilterFileName;
	pointers_[Eof] = &AtenSpace::Commands::Commands::function_Eof;
	pointers_[Find] = &AtenSpace::Commands::Commands::function_Find;
	pointers_[GetLine] = &AtenSpace::Commands::Commands::function_GetLine;
	pointers_[NextArg] = &AtenSpace::Commands::Commands::function_NextArg;
	pointers_[NextVariableArg] = &AtenSpace::Commands::Commands::function_NextVariableArg;
	pointers_[PeekChar] = &AtenSpace::Commands::Commands::function_PeekChar;
	pointers_[PeekCharI] = &AtenSpace::Commands::Commands::function_PeekCharI;
	pointers_[ReadChars] = &AtenSpace::Commands::Commands::function_ReadChars;
	pointers_[ReadDouble] = &AtenSpace::Commands::Commands::function_ReadDouble;
	pointers_[ReadDoubleArray] = &AtenSpace::Commands::Commands::function_ReadDoubleArray;
	pointers_[ReadInteger] = &AtenSpace::Commands::Commands::function_ReadInteger;
	pointers_[ReadIntegerArray] = &AtenSpace::Commands::Commands::function_ReadIntegerArray;
	pointers_[ReadLine] = &AtenSpace::Commands::Commands::function_ReadLine;
	pointers_[ReadLineFormatted] = &AtenSpace::Commands::Commands::function_ReadLineFormatted;
	pointers_[ReadNext] = &AtenSpace::Commands::Commands::function_ReadNext;
	pointers_[ReadVariable] = &AtenSpace::Commands::Commands::function_ReadVariable;
	pointers_[ReadVariableFormatted] = &AtenSpace::Commands::Commands::function_ReadVariableFormatted;
	pointers_[RemoveReadOption] = &AtenSpace::Commands::Commands::function_RemoveReadOption;
	pointers_[Rewind] = &AtenSpace::Commands::Commands::function_Rewind;
	pointers_[SkipChars] = &AtenSpace::Commands::Commands::function_SkipChars;
	pointers_[SkipLine] = &AtenSpace::Commands::Commands::function_SkipLine;
	pointers_[WriteLine] = &AtenSpace::Commands::Commands::function_WriteLine;
	pointers_[WriteLineFormatted] = &AtenSpace::Commands::Commands::function_WriteLineFormatted;
	pointers_[WriteVariable] = &AtenSpace::Commands::Commands::function_WriteVariable;
	pointers_[WriteVariableFormatted] = &AtenSpace::Commands::Commands::function_WriteVariableFormatted;

	// Pores Commands
	pointers_[CreateScheme] = &AtenSpace::Commands::Commands::function_CreateScheme;
	pointers_[DrillPores] = &AtenSpace::Commands::Commands::function_DrillPores;
	pointers_[SelectPores] = &AtenSpace::Commands::Commands::function_SelectPores;
	pointers_[Terminate] = &AtenSpace::Commands::Commands::function_Terminate;

	// Script Commands
	pointers_[ListScripts] = &AtenSpace::Commands::Commands::function_ListScripts;
	pointers_[LoadScript] = &AtenSpace::Commands::Commands::function_LoadScript;
	pointers_[RunScript] = &AtenSpace::Commands::Commands::function_RunScript;

	// Select Commands
	pointers_[DeSelect] = &AtenSpace::Commands::Commands::function_DeSelect;
	pointers_[DeSelectFor] = &AtenSpace::Commands::Commands::function_DeSelectFor;
	pointers_[DeSelectFormatted] = &AtenSpace::Commands::Commands::function_DeSelectFormatted;
	pointers_[DeSelectType] = &AtenSpace::Commands::Commands::function_DeSelectType;
	pointers_[Expand] = &AtenSpace::Commands::Commands::function_Expand;
	pointers_[Invert] = &AtenSpace::Commands::Commands::function_Invert;
	pointers_[Select] = &AtenSpace::Commands::Commands::function_Select;
	pointers_[SelectAll] = &AtenSpace::Commands::Commands::function_SelectAll;
	pointers_[SelectFFType] = &AtenSpace::Commands::Commands::function_SelectFFType;
	pointers_[SelectFor] = &AtenSpace::Commands::Commands::function_SelectFor;
	pointers_[SelectFormatted] = &AtenSpace::Commands::Commands::function_SelectFormatted;
	pointers_[SelectInsideCell] = &AtenSpace::Commands::Commands::function_SelectInsideCell;
	pointers_[SelectionCog] = &AtenSpace::Commands::Commands::function_SelectionCog;
	pointers_[SelectionCom] = &AtenSpace::Commands::Commands::function_SelectionCom;
	pointers_[SelectLine] = &AtenSpace::Commands::Commands::function_SelectLine;
	pointers_[SelectMiller] = &AtenSpace::Commands::Commands::function_SelectMiller;
	pointers_[SelectMolecule] = &AtenSpace::Commands::Commands::function_SelectMolecule;
	pointers_[SelectNone] = &AtenSpace::Commands::Commands::function_SelectNone;
	pointers_[SelectOverlaps] = &AtenSpace::Commands::Commands::function_SelectOverlaps;
	pointers_[SelectOutsideCell] = &AtenSpace::Commands::Commands::function_SelectOutsideCell;
	pointers_[SelectPattern] = &AtenSpace::Commands::Commands::function_SelectPattern;
	pointers_[SelectRadial] = &AtenSpace::Commands::Commands::function_SelectRadial;
	pointers_[SelectTree] = &AtenSpace::Commands::Commands::function_SelectTree;
	pointers_[SelectType] = &AtenSpace::Commands::Commands::function_SelectType;
	
	// Site Commands
	pointers_[GetSite] = &AtenSpace::Commands::Commands::function_GetSite;
	pointers_[ListSites] = &AtenSpace::Commands::Commands::function_ListSites;
	pointers_[NewSite] = &AtenSpace::Commands::Commands::function_NewSite;
	pointers_[SiteAxes] = &AtenSpace::Commands::Commands::function_SiteAxes;

	// String Commands
	pointers_[AfterStr] = &AtenSpace::Commands::Commands::function_AfterStr;
	pointers_[AToF] = &AtenSpace::Commands::Commands::function_AToF;
	pointers_[AToI] = &AtenSpace::Commands::Commands::function_AToI;
	pointers_[BeforeStr] = &AtenSpace::Commands::Commands::function_BeforeStr;
	pointers_[Contains] = &AtenSpace::Commands::Commands::function_Contains;
	pointers_[FToA] = &AtenSpace::Commands::Commands::function_FToA;
	pointers_[IToA] = &AtenSpace::Commands::Commands::function_IToA;
	pointers_[Lowercase] = &AtenSpace::Commands::Commands::function_Lowercase;
	pointers_[ReplaceChars] = &AtenSpace::Commands::Commands::function_ReplaceChars;
	pointers_[ReplaceStr] = &AtenSpace::Commands::Commands::function_ReplaceStr;
	pointers_[RemoveStr] = &AtenSpace::Commands::Commands::function_RemoveStr;
	pointers_[SPrintF] = &AtenSpace::Commands::Commands::function_SPrintF;
	pointers_[StripChars] = &AtenSpace::Commands::Commands::function_StripChars;
	pointers_[SubStr] = &AtenSpace::Commands::Commands::function_SubStr;
	pointers_[ToA] = &AtenSpace::Commands::Commands::function_ToA;
	pointers_[Uppercase] = &AtenSpace::Commands::Commands::function_Uppercase;

	// System Commands
	pointers_[Debug] = &AtenSpace::Commands::Commands::function_Debug;
	pointers_[Getenv] = &AtenSpace::Commands::Commands::function_Getenv;
	pointers_[Getenvf] = &AtenSpace::Commands::Commands::function_Getenvf;
	pointers_[Getenvi] = &AtenSpace::Commands::Commands::function_Getenvi;
	pointers_[Help] = &AtenSpace::Commands::Commands::function_Help;
	pointers_[Null] = &AtenSpace::Commands::Commands::function_Null;
	pointers_[Quit] = &AtenSpace::Commands::Commands::function_Quit;
	pointers_[SearchCommands] = &AtenSpace::Commands::Commands::function_SearchCommands;
	pointers_[Seed] = &AtenSpace::Commands::Commands::function_Seed;
	pointers_[Version] = &AtenSpace::Commands::Commands::function_Version;
	
	// Trajectory Commands
	pointers_[AddFrame] = &AtenSpace::Commands::Commands::function_AddFrame;
	pointers_[ClearTrajectory] = &AtenSpace::Commands::Commands::function_ClearTrajectory;
	pointers_[FinaliseFrame] = &AtenSpace::Commands::Commands::function_FinaliseFrame;
	pointers_[FirstFrame] = &AtenSpace::Commands::Commands::function_FirstFrame;
	pointers_[LastFrame] = &AtenSpace::Commands::Commands::function_LastFrame;
	pointers_[LoadTrajectory] = &AtenSpace::Commands::Commands::function_LoadTrajectory;
	pointers_[NextFrame] = &AtenSpace::Commands::Commands::function_NextFrame;
	pointers_[PrevFrame] = &AtenSpace::Commands::Commands::function_PrevFrame;
	pointers_[SeekFrame] = &AtenSpace::Commands::Commands::function_SeekFrame;

	// Transform Commands
	pointers_[AxisRotate] = &AtenSpace::Commands::Commands::function_AxisRotate;
	pointers_[Centre] = &AtenSpace::Commands::Commands::function_Centre;
	pointers_[FlipX] = &AtenSpace::Commands::Commands::function_FlipX;
	pointers_[FlipY] = &AtenSpace::Commands::Commands::function_FlipY;
	pointers_[FlipZ] = &AtenSpace::Commands::Commands::function_FlipZ;
	pointers_[MatrixConvert] = &AtenSpace::Commands::Commands::function_MatrixConvert;
	pointers_[MatrixTransform] = &AtenSpace::Commands::Commands::function_MatrixTransform;
	pointers_[Mirror] = &AtenSpace::Commands::Commands::function_Mirror;
	pointers_[Reorient] = &AtenSpace::Commands::Commands::function_Reorient;
	pointers_[Translate] = &AtenSpace::Commands::Commands::function_Translate;
	pointers_[TranslateAtom] = &AtenSpace::Commands::Commands::function_TranslateAtom;
	pointers_[TranslateCell] = &AtenSpace::Commands::Commands::function_TranslateCell;

	// View Commands
	pointers_[AxisRotateView] = &AtenSpace::Commands::Commands::function_AxisRotateView;
	pointers_[GetView] = &AtenSpace::Commands::Commands::function_GetView;
	pointers_[Orthographic] = &AtenSpace::Commands::Commands::function_Orthographic;
	pointers_[Perspective] = &AtenSpace::Commands::Commands::function_Perspective;
	pointers_[ResetView] = &AtenSpace::Commands::Commands::function_ResetView;
	pointers_[RotateView] = &AtenSpace::Commands::Commands::function_RotateView;
	pointers_[SetView] = &AtenSpace::Commands::Commands::function_SetView;
	pointers_[SpeedTest] = &AtenSpace::Commands::Commands::function_SpeedTest;
	pointers_[TranslateView] = &AtenSpace::Commands::Commands::function_TranslateView;
	pointers_[ViewAlong] = &AtenSpace::Commands::Commands::function_ViewAlong;
	pointers_[ViewAlongCell] = &AtenSpace::Commands::Commands::function_ViewAlongCell;
	pointers_[ZoomView] = &AtenSpace::Commands::Commands::function_ZoomView;
	pointers_[ZRotateView] = &AtenSpace::Commands::Commands::function_ZRotateView;
}
