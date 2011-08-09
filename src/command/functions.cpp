/*
	*** Command Function Pointers
	*** src/command/functions.cpp
	Copyright T. Youngs 2007-2011

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
	// Operators
	pointers_[OperatorAdd] = &function_OperatorAdd;
	pointers_[OperatorAnd] = &function_OperatorAnd;
	pointers_[OperatorAssignment] = &function_OperatorAssignment;
	pointers_[OperatorAssignmentDivide] = &function_OperatorAssignmentDivide;
	pointers_[OperatorAssignmentMultiply] = &function_OperatorAssignmentMultiply;
	pointers_[OperatorAssignmentPlus] = &function_OperatorAssignmentPlus;
	pointers_[OperatorAssignmentSubtract] = &function_OperatorAssignmentSubtract;
	pointers_[OperatorDivide] = &function_OperatorDivide;
	pointers_[OperatorEqualTo] = &function_OperatorEqualTo;
	pointers_[OperatorGreaterThan] = &function_OperatorGreaterThan;
	pointers_[OperatorGreaterThanEqualTo] = &function_OperatorGreaterThanEqualTo;
	pointers_[OperatorLessThan] = &function_OperatorLessThan;
	pointers_[OperatorLessThanEqualTo] = &function_OperatorLessThanEqualTo;
	pointers_[OperatorModulus] = &function_OperatorModulus;
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

	pointers_[NoFunction] = &function_NoFunction;
	pointers_[Joiner] = &function_Joiner;
	pointers_[Declarations] = &function_Declarations;

	// Analyse commands
	pointers_[Finalise] = &Command::function_Finalise;
	pointers_[FrameAnalyse] = &Command::function_FrameAnalyse;
	pointers_[Geometric] = &Command::function_Geometric;
	pointers_[ModelAnalyse] = &Command::function_ModelAnalyse;
	pointers_[PDens] = &Command::function_PDens;
	pointers_[PrintJobs] = &Command::function_PrintJobs;
	pointers_[RDF] = &Command::function_RDF;
	pointers_[SaveQuantities] = &Command::function_SaveQuantities;
	pointers_[TrajAnalyse] = &Command::function_TrajAnalyse;

	// Atom commands
	pointers_[AtomStyle] = &Command::function_AtomStyle;
	pointers_[ColourAtoms] = &Command::function_ColourAtoms;
	pointers_[CurrentAtom] = &Command::function_CurrentAtom;
	pointers_[Fix] = &Command::function_Fix;
	pointers_[Free] = &Command::function_Free;
	pointers_[GetAtom] = &Command::function_GetAtom;
	pointers_[Hide] = &Command::function_Hide;
	pointers_[RecolourAtoms] = &Command::function_RecolourAtoms;
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
	pointers_[SelectionAddHydrogen] = &Command::function_SelectionAddHydrogen;
	pointers_[SetAngle] = &Command::function_SetAngle;
	pointers_[SetDistance] = &Command::function_SetDistance;
	pointers_[SetTorsion] = &Command::function_SetTorsion;
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
	pointers_[MillerCut] = &Command::function_MillerCut;
	pointers_[NoCell] = &Command::function_NoCell;
	pointers_[Pack] = &Command::function_Pack;
	pointers_[PrintCell] = &Command::function_PrintCell;
	pointers_[Replicate] = &Command::function_Replicate;
	pointers_[RotateCell] = &Command::function_RotateCell;
	pointers_[Scale] = &Command::function_Scale;
	pointers_[ScaleMolecules] = &Command::function_ScaleMolecules;
	pointers_[SetCell] = &Command::function_SetCell;
	pointers_[SGInfo] = &Command::function_SGInfo;
	pointers_[Spacegroup] = &Command::function_Spacegroup;

	// Charge commands
	pointers_[Charge] = &Command::function_Charge;
	pointers_[ChargeFF] = &Command::function_ChargeFF;
	pointers_[ChargeFromModel] = &Command::function_ChargeFromModel;
	pointers_[ChargePAtom] = &Command::function_ChargePAtom;
	pointers_[ChargeType] = &Command::function_ChargeType;
	pointers_[ClearCharges] = &Command::function_ClearCharges;

	// Colourscale commands
	pointers_[AddPoint] = &Command::function_AddPoint;
	pointers_[ClearPoints] = &Command::function_ClearPoints;
	pointers_[ListScales] = &Command::function_ListScales;
	pointers_[RemovePoint] = &Command::function_RemovePoint;
	pointers_[ScaleInterpolate] = &Command::function_ScaleInterpolate;
	pointers_[ScaleName] = &Command::function_ScaleName;
	pointers_[ScaleVisible] = &Command::function_ScaleVisible;
	pointers_[SetPoint] = &Command::function_SetPoint;
	pointers_[SetPointColour] = &Command::function_SetPointColour;
	pointers_[SetPointValue] = &Command::function_SetPointValue;

	// Disordered build commands
	pointers_[Disorder] = &Command::function_Disorder;
	pointers_[ListComponents] = &Command::function_ListComponents;
	pointers_[SetupComponent] = &Command::function_SetupComponent;

	// Edit Commands
	pointers_[Delete] = &Command::function_Delete;
	pointers_[Copy] = &Command::function_Copy;
	pointers_[Cut] = &Command::function_Cut;
	pointers_[Paste] = &Command::function_Paste;
	pointers_[Redo] = &Command::function_Redo;
	pointers_[Undo] = &Command::function_Undo;

	// Energy Commands
	pointers_[ECut] = &Command::function_ECut;
	pointers_[Electrostatics] = &Command::function_Electrostatics;
	pointers_[FrameEnergy] = &Command::function_FrameEnergy;
	pointers_[ModelEnergy] = &Command::function_ModelEnergy;
	pointers_[PrintElec] = &Command::function_PrintElec;
	pointers_[PrintEwald] = &Command::function_PrintEwald;
	pointers_[PrintInter] = &Command::function_PrintInter;
	pointers_[PrintIntra] = &Command::function_PrintIntra;
	pointers_[PrintEnergy] = &Command::function_PrintEnergy;
	pointers_[PrintSummary] = &Command::function_PrintSummary;
	pointers_[PrintVdw] = &Command::function_PrintVdw;
	pointers_[VCut] = &Command::function_VCut;

	// Flow control
	pointers_[If] = &function_If;
	pointers_[Break] = &Command::function_Break;
	pointers_[Case] = &Command::function_Case;
	pointers_[Continue] = &Command::function_Continue;
	pointers_[Default] = &Command::function_Default;
	pointers_[DoWhile] = &Command::function_DoWhile;
	pointers_[For] = &Command::function_For;
	pointers_[ForIn] = &Command::function_ForIn;
	pointers_[If] = &Command::function_If;
	pointers_[Return] = &Command::function_Return;
	pointers_[Switch] = &Command::function_Switch;
	pointers_[While] = &Command::function_While;

	// Force Commands
	pointers_[FrameForces] = &Command::function_FrameForces;
	pointers_[ModelForces] = &Command::function_ModelForces;
	pointers_[PrintForces] = &Command::function_PrintForces;

	// Forcefield Commands
        pointers_[AngleDef] = &Command::function_AngleDef;
	pointers_[AutoConversionUnit] = &Command::function_AutoConversionUnit;
	pointers_[BondDef] = &Command::function_BondDef;
	pointers_[ClearExportMap] = &Command::function_ClearExportMap;
	pointers_[ClearExpression] = &Command::function_ClearExpression;
	pointers_[ClearMap] = &Command::function_ClearMap;
	pointers_[ClearTypes] = &Command::function_ClearTypes;
	pointers_[CreateExpression] = &Command::function_CreateExpression;
	pointers_[CurrentFF] = &Command::function_CurrentFF;
	pointers_[DeleteFF] = &Command::function_DeleteFF;
	pointers_[EnergyConvert] = &Command::function_EnergyConvert;
	pointers_[Equivalent] = &Command::function_Equivalent;
	pointers_[ExportMap] = &Command::function_ExportMap;
	pointers_[FFModel] = &Command::function_FFModel;
	pointers_[FFPattern] = &Command::function_FFPattern;
	pointers_[FinaliseFF] = &Command::function_FinaliseFF;
	pointers_[FixType] = &Command::function_FixType;
	pointers_[FreeType] = &Command::function_FreeType;
	pointers_[GenerateAngle] = &Command::function_GenerateAngle;
	pointers_[GenerateBond] = &Command::function_GenerateBond;
	pointers_[GenerateTorsion] = &Command::function_GenerateTorsion;
	pointers_[GenerateVdw] = &Command::function_GenerateVdw;
	pointers_[GetCombinationRule] = &Command::function_GetCombinationRule;
	pointers_[GetFF] = &Command::function_GetFF;
	pointers_[InterDef] = &Command::function_InterDef;
	pointers_[LoadFF] = &Command::function_LoadFF;
	pointers_[Map] = &Command::function_Map;
	pointers_[NewFF] = &Command::function_NewFF;
	pointers_[PrintSetup] = &Command::function_PrintSetup;
	pointers_[PrintType] = &Command::function_PrintType;
	pointers_[RecreateExpression] = &Command::function_RecreateExpression;
	pointers_[SaveExpression] = &Command::function_SaveExpression;
	pointers_[SetCombinationRule] = &Command::function_SetCombinationRule;
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
	pointers_[GlyphColours] = &Command::function_GlyphColours;
	pointers_[GlyphData] = &Command::function_GlyphData;
	pointers_[GlyphSolid] = &Command::function_GlyphSolid;
	pointers_[GlyphText] = &Command::function_GlyphText;
	pointers_[NewGlyph] = &Command::function_NewGlyph;

	// Grid Commands
	pointers_[AddFreePoint] = &Command::function_AddFreePoint;
	pointers_[AddGridPoint] = &Command::function_AddGridPoint;
	pointers_[AddNextGridPoint] = &Command::function_AddNextGridPoint;
	pointers_[CurrentGrid] = &Command::function_CurrentGrid;
	pointers_[FinaliseGrid] = &Command::function_FinaliseGrid;
	pointers_[GetGrid] = &Command::function_GetGrid;
	pointers_[GridAlpha] = &Command::function_GridAlpha;
	pointers_[GridAxes] = &Command::function_GridAxes;
	pointers_[GridColour] = &Command::function_GridColour;
	pointers_[GridColourSecondary] = &Command::function_GridColourSecondary;
	pointers_[GridColourscale] = &Command::function_GridColourscale;
	pointers_[GridCubic] = &Command::function_GridCubic;
	pointers_[GridCutoff] = &Command::function_GridCutoff;
	pointers_[GridCutoffSecondary] = &Command::function_GridCutoffSecondary;
	pointers_[GridLoopOrder] = &Command::function_GridLoopOrder;
	pointers_[GridOrigin] = &Command::function_GridOrigin;
	pointers_[GridOrtho] = &Command::function_GridOrtho;
	pointers_[InitialiseGrid] = &Command::function_InitialiseGrid;
	pointers_[GridSecondary] = &Command::function_GridSecondary;
	pointers_[GridStyle] = &Command::function_GridStyle;
	pointers_[GridUseZ] = &Command::function_GridUseZ;
	pointers_[GridVisible] = &Command::function_GridVisible;
	pointers_[LoadGrid] = &Command::function_LoadGrid;
	pointers_[NewGrid] = &Command::function_NewGrid;

	// Image Commands
	pointers_[SaveBitmap] = &Command::function_SaveBitmap;
	pointers_[SaveMovie] = &Command::function_SaveMovie;
	pointers_[SaveVibrationMovie] = &Command::function_SaveVibrationMovie;

	// Labeling Commands
	pointers_[ClearLabels] = &Command::function_ClearLabels;
	pointers_[Label] = &Command::function_Label;
	pointers_[RemoveLabel] = &Command::function_RemoveLabel;
	pointers_[RemoveLabels] = &Command::function_RemoveLabels;

	// Math Commands
	pointers_[Abs] = &Command::function_Abs;
	pointers_[ACos] = &Command::function_ACos;
	pointers_[ASin] = &Command::function_ASin;
	pointers_[ATan] = &Command::function_ATan;
	pointers_[Cos] = &Command::function_Cos;
	pointers_[DotProduct] = &Command::function_DotProduct;
	pointers_[Exp] = &Command::function_Exp;
	pointers_[Ln] = &Command::function_Ln;
	pointers_[Log] = &Command::function_Log;
	pointers_[Nint] = &Command::function_Nint;
	pointers_[Normalise] = &Command::function_Normalise;
	pointers_[Random] = &Command::function_Random;
	pointers_[Randomi] = &Command::function_Randomi;
	pointers_[Sin] = &Command::function_Sin;
	pointers_[Sqrt] = &Command::function_Sqrt;
	pointers_[Tan] = &Command::function_Tan;

	// MC Commands
	pointers_[MCAccept] = &Command::function_MCAccept;
	pointers_[MCAllow] = &Command::function_MCAllow;
	pointers_[MCMaxStep] = &Command::function_MCMaxStep;
	pointers_[MCNTrials] = &Command::function_MCNTrials;
	pointers_[PrintMC] = &Command::function_PrintMC;

	// Measurement Commands
	pointers_[ClearMeasurements] = &Command::function_ClearMeasurements;
	pointers_[GeometryCalc] = &Command::function_GeometryCalc;
	pointers_[ListMeasurements] = &Command::function_ListMeasurements;
	pointers_[Measure] = &Command::function_Measure;
	pointers_[MeasureSelected] = &Command::function_MeasureSelected;

	// Messaging Commands
	pointers_[CreateDialog] = &Command::function_CreateDialog;
	pointers_[DefaultDialog] = &Command::function_DefaultDialog;
	pointers_[Error] = &Command::function_Error;
	pointers_[Message] = &Command::function_Message;
	pointers_[Printf] = &Command::function_Printf;
	pointers_[Verbose] = &Command::function_Verbose;

	// Minimisation Commands
	pointers_[CGMinimise] = &Command::function_CGMinimise;
	pointers_[Converge] = &Command::function_Converge;
	pointers_[LineTolerance] = &Command::function_LineTolerance;
	pointers_[MCMinimise] = &Command::function_MCMinimise;
	pointers_[MopacMinimise] = &Command::function_MopacMinimise;
	pointers_[SDMinimise] = &Command::function_SDMinimise;
	
	// Model Commands
	pointers_[CreateAtoms] = &Command::function_CreateAtoms;
	pointers_[CurrentModel] = &Command::function_CurrentModel;
	pointers_[DeleteModel] = &Command::function_DeleteModel;
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
	pointers_[ParentModel] = &Command::function_ParentModel;
	pointers_[PrevModel] = &Command::function_PrevModel;
	pointers_[Info] = &Command::function_Info;
	pointers_[SaveModel] = &Command::function_SaveModel;
	pointers_[SaveSelection] = &Command::function_SaveSelection;
	pointers_[SetName] = &Command::function_SetName;
	pointers_[ShowAll] = &Command::function_ShowAll;

	// Model Extras Commands
	pointers_[NewBasisShell] = &Command::function_NewBasisShell;
	pointers_[NewEigenvector] = &Command::function_NewEigenvector;
	pointers_[NewVibration] = &Command::function_NewVibration;
	pointers_[PrintZMatrix] = &Command::function_PrintZMatrix;

	// Pattern Commands
	pointers_[ClearPatterns] = &Command::function_ClearPatterns;
	pointers_[CreatePatterns] = &Command::function_CreatePatterns;
	pointers_[CurrentPattern] = &Command::function_CurrentPattern;
	pointers_[FixPattern] = &Command::function_FixPattern;
	pointers_[GetPattern] = &Command::function_GetPattern;
	pointers_[ListPatterns] = &Command::function_ListPatterns;
	pointers_[NewPattern] = &Command::function_NewPattern;

	// Read / Write Commands
	pointers_[AddReadOption] = &Command::function_AddReadOption;
	pointers_[FilterFileName] = &Command::function_FilterFileName;
	pointers_[Eof] = &Command::function_Eof;
	pointers_[Find] = &Command::function_Find;
	pointers_[GetLine] = &Command::function_GetLine;
	pointers_[NextArg] = &Command::function_NextArg;
	pointers_[NextVariableArg] = &Command::function_NextVariableArg;
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
	pointers_[DeSelectFor] = &Command::function_DeSelectFor;
	pointers_[DeSelectFormatted] = &Command::function_DeSelectFormatted;
	pointers_[DeSelectType] = &Command::function_DeSelectType;
	pointers_[Expand] = &Command::function_Expand;
	pointers_[Invert] = &Command::function_Invert;
	pointers_[Select] = &Command::function_Select;
	pointers_[SelectAll] = &Command::function_SelectAll;
	pointers_[SelectFFType] = &Command::function_SelectFFType;
	pointers_[SelectFor] = &Command::function_SelectFor;
	pointers_[SelectFormatted] = &Command::function_SelectFormatted;
	pointers_[SelectInsideCell] = &Command::function_SelectInsideCell;
	pointers_[SelectionCog] = &Command::function_SelectionCog;
	pointers_[SelectionCom] = &Command::function_SelectionCom;
	pointers_[SelectLine] = &Command::function_SelectLine;
	pointers_[SelectMiller] = &Command::function_SelectMiller;
	pointers_[SelectMolecule] = &Command::function_SelectMolecule;
	pointers_[SelectNone] = &Command::function_SelectNone;
	pointers_[SelectOverlaps] = &Command::function_SelectOverlaps;
	pointers_[SelectOutsideCell] = &Command::function_SelectOutsideCell;
	pointers_[SelectPattern] = &Command::function_SelectPattern;
	pointers_[SelectRadial] = &Command::function_SelectRadial;
	pointers_[SelectTree] = &Command::function_SelectTree;
	pointers_[SelectType] = &Command::function_SelectType;
	
	// Site Commands
	pointers_[GetSite] = &Command::function_GetSite;
	pointers_[ListSites] = &Command::function_ListSites;
	pointers_[NewSite] = &Command::function_NewSite;
	pointers_[SiteAxes] = &Command::function_SiteAxes;

	// String Commands
	pointers_[AfterStr] = &Command::function_AfterStr;
	pointers_[AToF] = &Command::function_AToF;
	pointers_[AToI] = &Command::function_AToI;
	pointers_[BeforeStr] = &Command::function_BeforeStr;
	pointers_[Contains] = &Command::function_Contains;
	pointers_[FToA] = &Command::function_FToA;
	pointers_[IToA] = &Command::function_IToA;
	pointers_[Lowercase] = &Command::function_Lowercase;
	pointers_[ReplaceChars] = &Command::function_ReplaceChars;
	pointers_[ReplaceStr] = &Command::function_ReplaceStr;
	pointers_[RemoveStr] = &Command::function_RemoveStr;
	pointers_[SPrintF] = &Command::function_SPrintF;
	pointers_[StripChars] = &Command::function_StripChars;
	pointers_[SubStr] = &Command::function_SubStr;
	pointers_[ToA] = &Command::function_ToA;
	pointers_[Uppercase] = &Command::function_Uppercase;

	// System Commands
	pointers_[Debug] = &Command::function_Debug;
	pointers_[Getenv] = &Command::function_Getenv;
	pointers_[Getenvf] = &Command::function_Getenvf;
	pointers_[Getenvi] = &Command::function_Getenvi;
	pointers_[Gui] = &Command::function_Gui;
	pointers_[Help] = &Command::function_Help;
	pointers_[Null] = &Command::function_Null;
	pointers_[Quit] = &Command::function_Quit;
	pointers_[SearchCommands] = &Command::function_SearchCommands;
	pointers_[Seed] = &Command::function_Seed;
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
	pointers_[FlipX] = &Command::function_FlipX;
	pointers_[FlipY] = &Command::function_FlipY;
	pointers_[FlipZ] = &Command::function_FlipZ;
	pointers_[MatrixConvert] = &Command::function_MatrixConvert;
	pointers_[MatrixTransform] = &Command::function_MatrixTransform;
	pointers_[Mirror] = &Command::function_Mirror;
	pointers_[Reorient] = &Command::function_Reorient;
	pointers_[Translate] = &Command::function_Translate;
	pointers_[TranslateAtom] = &Command::function_TranslateAtom;
	pointers_[TranslateCell] = &Command::function_TranslateCell;

	// View Commands
	pointers_[AxisRotateView] = &Command::function_AxisRotateView;
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
}

