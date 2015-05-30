/*
	*** Command Functions
	*** src/parser/commands.h
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

#ifndef ATEN_COMMANDS_H
#define ATEN_COMMANDS_H

#include "parser/vtypes.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class CommandList;
class CommandNode;
class CommandData;
class Command;
class TreeNode;
class Bundle;
class ReturnValue;
class Aten;

ATEN_USING_NAMESPACE

class CommandData
{
	/*
	 * Command description
	 */
	public:
	// Command keyword
	const char* keyword;
	// Command arguments
	const char* arguments;
	// Return-value datatype
	VTypes::DataType returnType;
	// Command argument names
	const char* argText;
	// Command syntax
	const char* syntax;

	// Return whether command accepts any arguments
	bool hasArguments();
};

// Command actions
class Commands
{
	public:
	// Constructor / Destructor
	Commands(Aten& aten);
	~Commands();

	private:
	// Reference to Aten
	Aten& aten_;
	
	public:
	// Command list
	enum Function
	{
		// Operators
		OperatorAdd,
		OperatorAnd,
		OperatorAssignment,
		OperatorAssignmentDivide,
		OperatorAssignmentMultiply,
		OperatorAssignmentPlus,
		OperatorAssignmentSubtract,
		OperatorDivide,
		OperatorEqualTo,
		OperatorGreaterThan,
		OperatorGreaterThanEqualTo,
		OperatorInlineIf,
		OperatorLessThan,
		OperatorLessThanEqualTo,
		OperatorModulus,
		OperatorMultiply,
		OperatorNegate,
		OperatorNot,
		OperatorNotEqualTo,
		OperatorOr,
		OperatorPostfixDecrease,
		OperatorPostfixIncrease,
		OperatorPower,
		OperatorPrefixDecrease,
		OperatorPrefixIncrease,
		OperatorSubtract,
		
		// AST-Specific nodes
		NoFunction,
		Joiner,
		Declarations,
	
		// Analysis commands
		Finalise,
		FrameAnalyse,
		Geometric,
		ModelAnalyse,
		PDens,
		PrintJobs,
		RDF,
		SaveQuantities,
		TrajAnalyse,
	
		// Atom Commands
		AtomStyle,
		ColourAtoms,
		CurrentAtom,
		Fix,
		Free,
		GetAtom,
		Hide,
		RecolourAtoms,
		SetCharge,
		SetCoords,
		SetElement,
		SetForces,
		SetFX,
		SetFY,
		SetFZ,
		SetRX,
		SetRY,
		SetRZ,
		SetVelocities,
		SetVX,
		SetVY,
		SetVZ,
		Show,
	
		// Bond commands
		Augment,
		BondTolerance,
		ClearBonds,
		ClearSelectedBonds,
		NewBond,
		ReBond,
		ReBondPatterns,
		ReBondSelection,
	
		// Build commands
		AddHydrogen,
		Bohr,
		Chain,
		EndChain,
		GrowAtom,
		InsertAtom,
		Locate,
		Move,
		MoveToEnd,
		MoveToStart,
		NewAtom,
		NewAtomFrac,
		ReOrder,
		ResetPen,
		RotX,
		RotY,
		RotZ,
		SelectionAddHydrogen,
		SelectionGrowAtom,
		ShiftDown,
		ShiftUp,
		Transmute,
	
		// Cell commands
		AddGenerator,
		AdjustCell,
		Cell,
		CellAxes,
		Fold,
		FoldMolecules,
		FracToReal,
		MillerCut,
		NoCell,
		Pack,
		PrintCell,
		Replicate,
		RotateCell,
		Scale,
		ScaleMolecules,
		SetCell,
		SGInfo,
		Spacegroup,
	
		// Charge commands
		Charge,
		ChargeFF,
		ChargeFromModel,
		ChargePAtom,
		ChargeType,
		ClearCharges,
	
		// Colourscale commands
		AddPoint,
		ClearPoints,
		ListScales,
		RemovePoint,
		ScaleInterpolate,
		ScaleName,
		ScaleVisible,
		SetPoint,
		SetPointColour,
		SetPointValue,
	
		// Disordered build commands
		Disorder,
		ListComponents,
		SetupComponent,
	
		// Edit commands
		Copy,
		Cut,
		Delete,
		Paste,
		Redo,
		Undo,
	
		// Energy Commands
		Electrostatics,
		FrameEnergy,
		ModelEnergy,
		PrintElec,
		PrintEwald,
		PrintInter,
		PrintIntra,
		PrintEnergy,
		PrintSummary,
		PrintVdw,
	
		// Flow control
		Break,
		Case,
		Continue,
		Default,
		DoWhile,
		For,
		ForIn,
		If,
		Return,
		Switch,
		While,
	
		// Force Commands
		FrameForces,
		ModelForces,
		PrintForces,
	
		// Forcefield/Expression Commands
		AngleDef,
		AutoConversionUnit,
		BondDef,
		ClearExportMap,
		ClearExpression,
		ClearMap,
		ClearTypes,
		CreateExpression,
		CurrentFF,
		DeleteFF,
		EnergyConvert,
		Equivalent,
		ExportMap,
		FFModel,
		FFPattern,
		FinaliseFF,
		FixType,
		FreeType,
		GenerateAngle,
		GenerateBond,
		GenerateTorsion,
		GenerateVdw,
		GetCombinationRule,
		GetFF,
		InterDef,
		LoadFF,
		Map,
		NewFF,
		PrintSetup,
		PrintType,
		RecreateExpression,
		SaveExpression,
		SetCombinationRule,
		TorsionDef,
		TypeDef,
		TypeModel,
		TypeTest,
		Units,
	
		// Glyph commands
		AutoEllipsoids,
		AutoPolyhedra,
		GlyphAtomF,
		GlyphAtomR,
		GlyphAtomV,
		GlyphAtomsF,
		GlyphAtomsR,
		GlyphAtomsV,
		GlyphColour,
		GlyphColours,
		GlyphData,
		GlyphSolid,
		GlyphText,
		NewGlyph,
	
		// Grid Commands
		AddFreePoint,
		AddGridPoint,
		AddNextGridPoint,
		CurrentGrid,
		FinaliseGrid,
		GetGrid,
		GridAlpha,
		GridAxes,
		GridColour,
		GridColourSecondary,
		GridColourscale,
		GridCubic,
		GridCutoff,
		GridCutoffSecondary,
		GridLoopOrder,
		GridOrigin,
		GridOrtho,
		GridOutline,
		GridPeriodic,
		GridSecondary,
		GridStyle,
		GridStyleSecondary,
		GridUseZ,
		GridVisible,
		InitialiseGrid,
		LoadGrid,
		NewGrid,
	
		// Image Commands
		SaveBitmap,
		SaveMovie,
		SaveVibrationMovie,
	
		// Labeling commands
		ClearLabels,
		Label,
		RemoveLabel,
		RemoveLabels,

		// Math Commands
		Abs,
		ACos,
		ASin,
		ATan,
		Cos,
		DotProduct,
		Exp,
		Ln,
		Log,
		Nint,
		Normalise,
		Random,
		Randomi,
		Sin,
		Sqrt,
		Tan,

		// MC Commands
		PrintMC,
	
		// Measurements
		ClearMeasurements,
		GeometryCalc,
		ListMeasurements,
		Measure,
		MeasureSelected,
	
		// Messaging and GUI
		CreateDialog,
		DefaultDialog,
		Error,
		Message,
		Printf,
		ShowDefaultDialog,
		Verbose,
	
		// Minimisation Commands
		CGMinimise,
		Converge,
		LineTolerance,
		MCMinimise,
		MopacMinimise,
		SDMinimise,

		// Model Commands
		CreateAtoms,
		CurrentModel,
		DeleteModel,
		FinaliseModel,
		FirstModel,
		GetModel,
		Info,
		LastModel,
		ListModels,
		LoadModel,
		LogInfo,
		ModelTemplate,
		NewModel,
		NextModel,
		ParentModel,
		PrevModel,
		SaveModel,
		SaveSelection,
		SetName,
		ShowAll,

		// Model Extras Commands
		NewBasisShell,
		NewEigenvector,
		NewVibration,
		PrintZMatrix,
	
		// Pattern Commands
		ClearPatterns,
		CreatePatterns,
		CurrentPattern,
		FixPattern,
		GetPattern,
		ListPatterns,
		NewPattern,

		// Pores commands
		CreateScheme,
		DrillPores,
		SelectPores,
		Terminate,

		// Read / Write Commands
		AddReadOption,
		Eof,
		FilterFileName,
		Find,
		GetLine,
		NextArg,
		NextVariableArg,
		PeekChar,
		PeekCharI,
		ReadChars,
		ReadDouble,
		ReadDoubleArray,
		ReadInteger,
		ReadIntegerArray,
		ReadLine,
		ReadLineFormatted,
		ReadNext,
		ReadVariable,
		ReadVariableFormatted,
		RemoveReadOption,
		Rewind,
		SkipChars,
		SkipLine,
		WriteLine,
		WriteLineFormatted,
		WriteVariable,
		WriteVariableFormatted,

		// Script Commands
		ListScripts,
		LoadScript,
		RunScript,

		// Select Commands
		DeSelect,
		DeSelectCode,
		DeSelectFormatted,
		DeSelectType,
		Expand,
		Invert,
		Select,
		SelectAll,
		SelectCode,
		SelectFFType,
		SelectFormatted,
		SelectInsideCell,
		SelectionCog,
		SelectionCom,
		SelectLine,
		SelectMiller,
		SelectMolecule,
		SelectNone,
		SelectOverlaps,
		SelectOutsideCell,
		SelectPattern,
		SelectRadial,
		SelectTree,
		SelectType,

		// Site Commands
		GetSite,
		ListSites,
		NewSite,
		SiteAxes,

		// String Commands
		AfterStr,
		AToF,
		AToI,
		BeforeStr,
		Contains,
		FToA,
		IToA,
		Lowercase,
		ReplaceChars,
		ReplaceStr,
		RemoveStr,
		SPrintF,
		StripChars,
		SubStr,
		ToA,
		Uppercase,

		// System commands
		Debug,
		Getenv,
		Getenvf,
		Getenvi,
		Help,
		Null,
		Quit,
		SearchCommands,
		Seed,
		Version,

		// Trajectory Commands
		AddFrame,
		ClearTrajectory,
		FinaliseFrame,
		FirstFrame,
		LastFrame,
		LoadTrajectory,
		NextFrame,
		PrevFrame,
		SeekFrame,

		// Transformation Commands
		AxisRotate,
		Centre,
		FlipX,
		FlipY,
		FlipZ,
		MatrixConvert,
		MatrixTransform,
		Mirror,
		Reorient,
		SetAngle,
		SetAngles,
		SetDistance,
		SetDistances,
		SetTorsion,
		SetTorsions,
		Translate,
		TranslateAtom,
		TranslateCell,
		TranslateWorld,

		// View
		AxisRotateView,
		GetView,
		Orthographic,
		Perspective,
		ResetView,
		RotateView,
		SetView,
		SpeedTest,
		TranslateView,
		ViewAlong,
		ViewAlongCell,
		ZoomView,
		ZRotateView,

		nCommands
	};
	// Return enumerated command id from string
	static Commands::Function command(QString name);
	// Return command name
	static const char* command(Commands::Function cf);


	/*
	 * Function declarations
	 */
	private:
	// Internals / Joiners
	bool function_NoFunction(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Joiner(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Declarations(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Analyse commands
	bool function_Finalise(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FrameAnalyse(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Geometric(CommandNode* c, Bundle& obj, ReturnValue& rv);	
	bool function_ModelAnalyse(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PDens(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PrintJobs(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_RDF(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SaveQuantities(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_TrajAnalyse(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Atom Commands
	bool function_AtomStyle(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ColourAtoms(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_CurrentAtom(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Fix(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Free(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GetAtom(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Hide(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_RecolourAtoms(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetCoords(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetCharge(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetElement(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetForces(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetFX(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetFY(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetFZ(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetRX(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetRY(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetRZ(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetVelocities(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetVX(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetVY(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetVZ(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Show(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Bond commands
	bool function_Augment(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_BondTolerance(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ClearBonds(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ClearSelectedBonds(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_NewBond(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ReBond(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ReBondPatterns(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ReBondSelection(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Build commands
	bool function_AddHydrogen(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Bohr(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Chain(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_EndChain(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GrowAtom(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_InsertAtom(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Locate(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Move(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_MoveToEnd(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_MoveToStart(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_NewAtom(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_NewAtomFrac(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ReOrder(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ResetPen(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_RotX(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_RotY(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_RotZ(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectionAddHydrogen(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectionGrowAtom(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ShiftDown(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ShiftUp(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Transmute(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Cell commands
	bool function_AddGenerator(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_AdjustCell(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Cell(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_CellAxes(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Fold(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FoldMolecules(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FracToReal(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_MillerCut(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_NoCell(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Pack(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PrintCell(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Replicate(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_RotateCell(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Scale(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ScaleMolecules(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetCell(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SGInfo(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Spacegroup(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Charge commands
	bool function_Charge(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ChargeFF(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ChargeFromModel(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ChargePAtom(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ChargeType(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ClearCharges(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Colourscale commands
	bool function_AddPoint(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ClearPoints(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ListScales(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_RemovePoint(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ScaleInterpolate(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ScaleName(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ScaleVisible(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetPoint(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetPointColour(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetPointValue(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Disordered build commands
	bool function_Disorder(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ListComponents(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetupComponent(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Edit commands
	bool function_Copy(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Cut(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Delete(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Paste(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Redo(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Undo(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Energy Commands
	bool function_Electrostatics(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FrameEnergy(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ModelEnergy(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PrintElec(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PrintEwald(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PrintInter(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PrintIntra(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PrintEnergy(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PrintSummary(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PrintVdw(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Flow control
	bool function_Break(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Case(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Continue(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Default(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_DoWhile(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_For(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ForIn(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_If(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Return(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Switch(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_While(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Force Commands
	bool function_FrameForces(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ModelForces(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PrintForces(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Forcefield Commands
	bool function_AngleDef(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_AutoConversionUnit(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_BondDef(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ClearExportMap(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ClearExpression(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ClearMap(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ClearTypes(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_CreateExpression(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_CurrentFF(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_DeleteFF(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_EnergyConvert(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Equivalent(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ExportMap(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FFModel(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FFPattern(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FinaliseFF(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FixType(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FreeType(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GenerateAngle(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GenerateBond(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GenerateTorsion(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GenerateVdw(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GetCombinationRule(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GetFF(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_InterDef(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_LoadFF(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Map(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_NewFF(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PrintSetup(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PrintType(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_RecreateExpression(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SaveExpression(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetCombinationRule(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_TorsionDef(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_TypeDef(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_TypeModel(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_TypeTest(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Units(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Glyph commands
	bool function_AutoEllipsoids(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_AutoPolyhedra(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GlyphAtomF(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GlyphAtomR(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GlyphAtomV(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GlyphAtomsF(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GlyphAtomsR(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GlyphAtomsV(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GlyphColour(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GlyphColours(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GlyphData(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GlyphSolid(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GlyphText(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_NewGlyph(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Grid Commands
	bool function_AddGridPoint(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_AddFreePoint(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_AddNextGridPoint(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_CurrentGrid(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FinaliseGrid(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GetGrid(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridAlpha(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridAxes(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridColour(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridColourSecondary(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridColourscale(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridCubic(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridCutoff(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridCutoffSecondary(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridOrtho(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridLoopOrder(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridOrigin(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridOutline(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridPeriodic(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridSecondary(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridStyle(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridStyleSecondary(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridUseZ(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GridVisible(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_InitialiseGrid(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_LoadGrid(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_NewGrid(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Image Commands
	bool function_SaveBitmap(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SaveMovie(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SaveVibrationMovie(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Labeling commands
	bool function_ClearLabels(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Label(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_RemoveLabel(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_RemoveLabels(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Math Commands
	bool function_Abs(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ACos(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ASin(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ATan(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Cos(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_DotProduct(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Exp(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Ln(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Log(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Nint(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Normalise(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Random(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Randomi(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Sin(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Sqrt(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Tan(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// MC Commands
	bool function_PrintMC(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Measurement and Geometry
	bool function_ClearMeasurements(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GeometryCalc(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ListMeasurements(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Measure(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_MeasureSelected(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Messaging / GUI
	bool function_CreateDialog(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_DefaultDialog(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Error(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Message(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Printf(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ShowDefaultDialog(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Verbose(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Minimisation Commands
	bool function_CGMinimise(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Converge(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_LineTolerance(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_MCMinimise(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_MopacMinimise(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SDMinimise(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Model Commands
	bool function_CreateAtoms(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_CurrentModel(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_DeleteModel(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FinaliseModel(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FirstModel(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GetModel(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Info(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_LastModel(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ListModels(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_LoadModel(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_LogInfo(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ModelTemplate(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_NewModel(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_NextModel(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ParentModel(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PrevModel(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SaveModel(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SaveSelection(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetName(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ShowAll(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Model Extra Commands
	bool function_NewBasisShell(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_NewEigenvector(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_NewVibration(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PrintZMatrix(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Pattern Commands
	bool function_ClearPatterns(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_CreatePatterns(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_CurrentPattern(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FixPattern(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GetPattern(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ListPatterns(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_NewPattern(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Pores commands
	bool function_CreateScheme(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_DrillPores(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectPores(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Terminate(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Read / Write Commands
	bool function_AddReadOption(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FilterFileName(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Eof(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Find(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GetLine(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_NextArg(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_NextVariableArg(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PeekChar(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PeekCharI(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ReadChars(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ReadDouble(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ReadDoubleArray(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ReadInteger(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ReadIntegerArray(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ReadLine(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ReadLineFormatted(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ReadNext(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ReadVariable(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ReadVariableFormatted(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_RemoveReadOption(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Rewind(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SkipChars(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SkipLine(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_WriteLine(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_WriteLineFormatted(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_WriteVariable(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_WriteVariableFormatted(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Script Commands
	bool function_ListScripts(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_LoadScript(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_RunScript(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Select Commands
	bool function_DeSelect(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_DeSelectCode(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_DeSelectFormatted(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_DeSelectType(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Expand(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Invert(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Select(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectAll(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectCode(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectFFType(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectFormatted(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectInsideCell(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectionCog(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectionCom(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectLine(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectNone(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectMiller(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectMolecule(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectOutsideCell(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectOverlaps(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectPattern(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectRadial(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectTree(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SelectType(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Site Commands
	bool function_GetSite(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ListSites(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_NewSite(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SiteAxes(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// String Commands
	bool function_AfterStr(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_AToF(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_AToI(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_BeforeStr(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Contains(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FToA(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_IToA(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Lowercase(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ReplaceChars(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ReplaceStr(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_RemoveStr(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SPrintF(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_StripChars(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SubStr(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ToA(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Uppercase(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// System Commands
	bool function_Debug(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Getenv(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Getenvf(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Getenvi(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Help(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Null(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Quit(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SearchCommands(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Seed(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Version(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Trajectory Commands
	bool function_AddFrame(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ClearTrajectory(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FinaliseFrame(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FirstFrame(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_LastFrame(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_LoadTrajectory(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_NextFrame(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_PrevFrame(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SeekFrame(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Transform Commands
	bool function_AxisRotate(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Centre(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FlipX(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FlipY(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_FlipZ(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_MatrixConvert(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_MatrixTransform(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Mirror(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Reorient(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetAngle(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetAngles(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetDistance(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetDistances(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetTorsion(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetTorsions(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Translate(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_TranslateAtom(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_TranslateCell(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_TranslateWorld(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Variable Operators
	bool function_OperatorAdd(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorAnd(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorAssignment(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorAssignmentDivide(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorAssignmentMultiply(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorAssignmentPlus(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorAssignmentSubtract(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorDivide(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorEqualTo(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorGreaterThan(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorGreaterThanEqualTo(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorInlineIf(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorLessThan(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorLessThanEqualTo(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorModulus(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorMultiply(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorNegate(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorNot(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorNotEqualTo(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorOr(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorPostfixDecrease(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorPostfixIncrease(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorPower(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorPrefixDecrease(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorPrefixIncrease(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_OperatorSubtract(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// View
	bool function_AxisRotateView(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_GetView(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Orthographic(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_Perspective(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ResetView(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_RotateView(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SetView(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_SpeedTest(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_TranslateView(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ViewAlong(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ViewAlongCell(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ZoomView(CommandNode* c, Bundle& obj, ReturnValue& rv);
	bool function_ZRotateView(CommandNode* c, Bundle& obj, ReturnValue& rv);


	/*
	 * Function descriptions / syntax etc.
	 */
	public:
	// Pointer to function typedef
	typedef bool (Commands::*CommandFunction)(CommandNode* c, Bundle& obj, ReturnValue& rv);
	// Macro definition for call function
	#define CALL_COMMAND(object,ptrToMember) ((object).*(ptrToMember))

	private:
	// Command data
	static CommandData data_[Commands::nCommands];
	// Function pointers
	CommandFunction pointers_[Commands::nCommands];

	private:
	// Initialise function pointers
	void initPointers();

	public:
	// Return function data for specified command
	static CommandData data(Commands::Function func);
	// Execute specified command
	bool call(Commands::Function cf, CommandNode* node, Bundle& bundle, ReturnValue& rv);
};

ATEN_END_NAMESPACE

#endif

