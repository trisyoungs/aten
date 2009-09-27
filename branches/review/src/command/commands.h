/*
	*** Command Functions
	*** src/parser/commands.h
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

#ifndef ATEN_COMMANDS_H
#define ATEN_COMMANDS_H

#include "base/bundle.h"
#include "parser/returnvalue.h"

// Forward declarations
class CommandList;
class CommandNode;
class CommandData;
class Command;

// Function pointer typedef and call #define
typedef bool (*CommandFunction)(CommandNode *c, Bundle &obj, ReturnValue &rv);
#define CALL_COMMAND(object,ptrToMember) ((object).*(ptrToMember)) 

class CommandData
{
	public:
	/*
	// Command description
	*/
	public:
	// Command keyword
	const char *keyword;
	// Command arguments
	const char *arguments;
	// Return-value datatype
	VTypes::DataType returnType;
	// Command argument names
	const char *argText;
	// Command syntax
	const char *syntax;

	// Return whether command accepts any arguments
	bool hasArguments();
};

// Command actions
class Command
{

	public:
	// Constructor / Destructor
	Command();
	~Command();

	// Command list
	enum Function {

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
		GetAtom,
		Hide,
		SetCharge,
		SetCoords,
		SetElement,
		SetForces,
		SetFX,
		SetFY,
		SetFZ,
		SetId,
		SetRX,
		SetRY,
		SetRZ,
		SetVelocities,
		SetVX,
		SetVY,
		SetVZ,
		Show,
		ShowAll,
	
		// Bond commands
		Augment,
		BondTolerance,
		ClearBonds,
		ClearSelectedBonds,
		NewBond,
		NewBondId,
		ReBond,
		ReBondPatterns,
		ReBondSelection,
	
		// Build commands
		AddHydrogen,
		Bohr,
		Chain,
		EndChain,
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
		NMols,
		RegionCentre,
		RegionCentreFrac,
		RegionGeometry,
		RegionGeometryFrac,
		RegionOverlaps,
		RegionRotation,
		RegionShape,
		SetRegion,
		SetRegionFrac,
		VdwScale,
	
		// Edit commands
		Copy,
		Cut,
		Delete,
		Paste,
		Redo,
		Undo,
	
		// Energy Commands
		ECut,
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
		VCut,
	
		// Flow control
		Break,
		Continue,
		DoWhile,
		For,
		If,
		Return,
		While,
	
		// Force Commands
		FrameForces,
		ModelForces,
		PrintForces,
	
		// Forcefield/Expression Commands
		AngleDef,
		BondDef,
		ClearMap,
		ClearExportMap,
		CreateExpression,
		DefaultFF,
		Equivalent,
		ExportMap,
		FFModel,
		FFPattern,
		FinaliseFF,
		GenConvert,
		GeneratorData,
		GetFF,
		InterDef,
		LoadFF,
		Map,
		NewFF,
		PrintSetup,
		Rules,
		SaveExpression,
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
		GlyphData,
		GlyphSolid,
		GlyphText,
		NewGlyph,
	
		// Grid Commands
		AddGridPoint,
		AddNextGridPoint,
		FinaliseGrid,
		GetGrid,
		GridAlpha,
		GridAxes,
		GridColour,
		GridColourNegative,
		GridColourscale,
		GridCubic,
		GridCutoff,
		GridLoopOrder,
		GridOrigin,
		GridOrtho,
		GridSize,
		GridStyle,
		GridSymmetric,
		GridUseZ,
		GridVisible,
		LoadGrid,
		NewGrid,
	
		// Image Commands
		SaveBitmap,
		SaveVector,
	
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
		Sin,
		Sqrt,
		Tan,

		// MC Commands
		MCAccept,
		MCAllow,
		MCMaxStep,
		MCNTrials,
		PrintMC,
	
		// Measurements
		Angle,
		Angles,
		ClearMeasurements,
		Distance,
		Distances,
		ListMeasurements,
		Measure,
		Torsion,
		Torsions,
	
		// Messaging
		Error,
		Printf,
		Verbose,
	
		// Minimisation Commands
		CGMinimise,
		Converge,
		LineTolerance,
		MCMinimise,
		SDMinimise,
		SimplexMinimise,
		
		// Model Commands
		CreateAtoms,
		CurrentModel,
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
		PrevModel,
		SaveModel,
		SetName,
	
		// Pattern Commands
		ClearPatterns,
		CreatePatterns,
		FixPattern,
		GetPattern,
		ListPatterns,
		NewPattern,

		// Read / Write Commands
		AddReadOption,
		Eof,
		FilterFileName,
		Find,
		GetLine,
		NextArg,
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
		DeSelectFormatted,
		DeSelectType,
		Expand,
		Invert,
		Select,
		SelectAll,
		SelectFFType,
		SelectFormatted,
		SelectInsideCell,
		SelectionCog,
		SelectionCom,
		SelectLine,
		SelectMiller,
		SelectNone,
		SelectOverlaps,
		SelectOutsideCell,
		SelectPattern,
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
		ReplaceChars,
		StripChars,

		// System commands
		Debug,
		Getenv,
		Getenvf,
		Getenvi,
		Gui,
		Help,
		Seed,
		Quit,
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
		MatrixConvert,
		MatrixTransform,
		Mirror,
		Reorient,
		Translate,
		TranslateAtom,
		TranslateCell,
		TranslateWorld,

		// View
		AxisRotateView,
		GetView,
		LookAt,
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
	Command::Function command(const char*);

	/*
	// Function declarations
	*/
	private:
	// AST-specific commands
	static bool function_NoFunction(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Joiner(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Declarations(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Analyse commands
	static bool function_Finalise(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_FrameAnalyse(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Geometric(CommandNode *c, Bundle &obj, ReturnValue &rv);	
	static bool function_ModelAnalyse(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_PDens(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_PrintJobs(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RDF(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SaveQuantities(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_TrajAnalyse(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Atom Commands
	static bool function_AtomStyle(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GetAtom(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Hide(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetCoords(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetCharge(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetElement(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetForces(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetFX(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetFY(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetFZ(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetId(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetRX(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetRY(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetRZ(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetVelocities(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetVX(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetVY(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetVZ(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Show(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ShowAll(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Bond commands
	static bool function_Augment(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_BondTolerance(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ClearBonds(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ClearSelectedBonds(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_NewBond(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_NewBondId(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ReBond(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ReBondPatterns(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ReBondSelection(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Build commands
	static bool function_AddHydrogen(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Bohr(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Chain(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_EndChain(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_InsertAtom(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Locate(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Move(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_MoveToEnd(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_MoveToStart(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_NewAtom(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_NewAtomFrac(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ReOrder(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ResetPen(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RotX(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RotY(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RotZ(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ShiftDown(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ShiftUp(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Transmute(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Cell commands
	static bool function_AddGenerator(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_AdjustCell(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Cell(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_CellAxes(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Fold(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_FoldMolecules(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_FracToReal(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_MillerCut(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_NoCell(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Pack(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_PrintCell(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Replicate(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RotateCell(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Scale(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ScaleMolecules(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetCell(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SGInfo(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Spacegroup(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Charge commands
	static bool function_Charge(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ChargeFF(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ChargeFromModel(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ChargePAtom(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ChargeType(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ClearCharges(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Colourscale commands
	static bool function_AddPoint(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ClearPoints(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ListScales(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RemovePoint(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ScaleInterpolate(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ScaleName(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ScaleVisible(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetPoint(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetPointColour(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetPointValue(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Disordered build commands
	static bool function_Disorder(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ListComponents(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_NMols(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RegionCentre(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RegionCentreFrac(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RegionGeometry(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RegionGeometryFrac(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RegionOverlaps(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RegionRotation(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RegionShape(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetRegion(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetRegionFrac(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_VdwScale(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Edit commands
	static bool function_Copy(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Cut(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Delete(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Paste(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Redo(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Undo(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Energy Commands
	static bool function_ECut(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Electrostatics(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_FrameEnergy(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ModelEnergy(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_PrintElec(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_PrintEwald(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_PrintInter(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_PrintIntra(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_PrintEnergy(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_PrintSummary(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_PrintVdw(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_VCut(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Flow control
	static bool function_Break(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Continue(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_DoWhile(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_For(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_If(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Return(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_While(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Force Commands
	static bool function_FrameForces(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ModelForces(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_PrintForces(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Forcefield Commands
	static bool function_AngleDef(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_BondDef(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ClearMap(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ClearExportMap(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_CreateExpression(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_DefaultFF(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Equivalent(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ExportMap(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_FinaliseFF(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_FFModel(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_FFPattern(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GenConvert(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GeneratorData(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GetFF(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_InterDef(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_LoadFF(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Map(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_NewFF(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_PrintSetup(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Rules(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SaveExpression(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_TorsionDef(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_TypeDef(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_TypeModel(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_TypeTest(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Units(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Glyph commands
	static bool function_AutoEllipsoids(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_AutoPolyhedra(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GlyphAtomF(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GlyphAtomR(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GlyphAtomV(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GlyphAtomsF(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GlyphAtomsR(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GlyphAtomsV(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GlyphColour(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GlyphData(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GlyphSolid(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GlyphText(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_NewGlyph(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Grid Commands
	static bool function_AddGridPoint(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_AddNextGridPoint(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_FinaliseGrid(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GetGrid(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GridAlpha(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GridAxes(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GridColour(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GridColourNegative(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GridColourscale(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GridCubic(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GridCutoff(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GridOrtho(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GridLoopOrder(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GridOrigin(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GridSize(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GridStyle(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GridSymmetric(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GridUseZ(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GridVisible(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_LoadGrid(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_NewGrid(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Image Commands
	static bool function_SaveBitmap(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SaveVector(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Labeling commands
	static bool function_ClearLabels(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Label(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RemoveLabel(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RemoveLabels(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Math Commands
	static bool function_Abs(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ACos(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ASin(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ATan(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Cos(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_DotProduct(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Exp(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Ln(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Log(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Nint(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Normalise(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Sin(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Sqrt(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Tan(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// MC Commands
	static bool function_MCAccept(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_MCAllow(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_MCMaxStep(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_MCNTrials(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_PrintMC(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Measurements
	static bool function_Angle(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Angles(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ClearMeasurements(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Distance(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Distances(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ListMeasurements(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Measure(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Torsion(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Torsions(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Messaging
	static bool function_Error(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Printf(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Verbose(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Minimisation Commands
	static bool function_CGMinimise(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Converge(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_LineTolerance(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_MCMinimise(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SDMinimise(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SimplexMinimise(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Model Commands
	static bool function_CreateAtoms(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_CurrentModel(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_FinaliseModel(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_FirstModel(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GetModel(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Info(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_LastModel(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ListModels(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_LoadModel(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_LogInfo(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ModelTemplate(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_NewModel(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_NextModel(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_PrevModel(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SaveModel(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetName(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Pattern Commands
	static bool function_ClearPatterns(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_CreatePatterns(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_FixPattern(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GetPattern(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ListPatterns(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_NewPattern(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Read / Write Commands
	static bool function_AddReadOption(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_FilterFileName(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Eof(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Find(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GetLine(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_NextArg(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_PeekChar(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_PeekCharI(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ReadChars(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ReadDouble(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ReadDoubleArray(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ReadInteger(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ReadIntegerArray(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ReadLine(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ReadLineFormatted(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ReadNext(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ReadVariable(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ReadVariableFormatted(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RemoveReadOption(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Rewind(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SkipChars(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SkipLine(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_WriteLine(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_WriteLineFormatted(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_WriteVariable(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_WriteVariableFormatted(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Script Commands
	static bool function_ListScripts(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_LoadScript(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RunScript(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Select Commands
	static bool function_DeSelect(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_DeSelectFormatted(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_DeSelectType(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Expand(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Invert(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Select(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SelectAll(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SelectFFType(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SelectFormatted(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SelectInsideCell(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SelectionCog(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SelectionCom(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SelectLine(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SelectNone(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SelectMiller(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SelectOutsideCell(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SelectOverlaps(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SelectPattern(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SelectType(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Site Commands
	static bool function_GetSite(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ListSites(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_NewSite(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SiteAxes(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// String Commands
	static bool function_AfterStr(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_AToF(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_AToI(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_BeforeStr(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Contains(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_FToA(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_IToA(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ReplaceChars(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_StripChars(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// System Commands
	static bool function_Debug(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Getenv(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Getenvf(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Getenvi(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Gui(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Seed(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Help(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Quit(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Version(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Trajectory Commands
	static bool function_AddFrame(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ClearTrajectory(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_FinaliseFrame(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_FirstFrame(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_LastFrame(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_LoadTrajectory(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_NextFrame(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_PrevFrame(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SeekFrame(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Transform Commands
	static bool function_AxisRotate(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Centre(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_MatrixConvert(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_MatrixTransform(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Mirror(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Reorient(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Translate(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_TranslateAtom(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_TranslateCell(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_TranslateWorld(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// Variable Operators
	static bool function_OperatorAdd(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorAnd(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorAssignment(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorAssignmentDivide(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorAssignmentMultiply(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorAssignmentPlus(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorAssignmentSubtract(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorDivide(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorEqualTo(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorGreaterThan(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorGreaterThanEqualTo(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorLessThan(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorLessThanEqualTo(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorModulus(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorMultiply(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorNegate(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorNot(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorNotEqualTo(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorOr(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorPostfixDecrease(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorPostfixIncrease(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorPower(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorPrefixDecrease(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorPrefixIncrease(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_OperatorSubtract(CommandNode *c, Bundle &obj, ReturnValue &rv);
	// View
	static bool function_AxisRotateView(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_GetView(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_LookAt(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Orthographic(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_Perspective(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ResetView(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_RotateView(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SetView(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_SpeedTest(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_TranslateView(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ViewAlong(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ViewAlongCell(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ZoomView(CommandNode *c, Bundle &obj, ReturnValue &rv);
	static bool function_ZRotateView(CommandNode *c, Bundle &obj, ReturnValue &rv);

	/*
	// Function descriptions / syntax etc.
	*/
	private:
	// Function pointers
	CommandFunction pointers_[Command::nCommands];

	public:
	// Function data
	static CommandData data[Command::nCommands];
	// Initialise function pointers
	void initPointers();
	// Execute specified command
	bool call(Command::Function cf, CommandNode *node, ReturnValue &rv);
};

// External declaration
extern Command commands;

#endif

