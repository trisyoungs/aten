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

#ifndef ATEN_NUCOMMANDS_H
#define ATEN_NUCOMMANDS_H

#include "base/bundle.h"
#include "parser/returnvalue.h"

// Forward declarations
class NuCommandList;
class NuCommandNode;
class NuCommandData;
class NuCommand;

// Function pointer typedef and call #define
typedef bool (*NuCommandFunction)(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
#define CALL_NUCOMMAND(object,ptrToMember) ((object).*(ptrToMember)) 

class NuCommandData
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
	// Command argument names
	const char *argText;
	// Return-value datatype
	NuVTypes::DataType returnType;
	// Command syntax
	const char *syntax;

	// Return whether command accepts any arguments
	bool hasArguments();
};

// Command actions
class NuCommand
{

	public:
	// Constructor / Destructor
	NuCommand();
	~NuCommand();

	// Command list
	enum Function {
		
		// AST-Specific nodes
		NoFunction,
		Joiner,
		Initialisations,
	
		// Analysis commands
		Finalise,
		FrameAnalyse,
		Geometry,
		ModelAnalyse,
		Pdens,
		PrintJobs,
		Rdf,
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
		NoCell,
		Pack,
		PrintCell,
		Replicate,
		RotateCell,
		Scale,
		ScaleMolecules,
		SetCell,
		Spacegroup,
	
		// Charge commands
		ChargeFF,
		ChargeFromModel,
		ChargePAtom,
		Charge,
		ChargeType,
		ClearCharges,
	
		// Colourscale commands
		AddPoint,
		ClearPoints,
		ListScales,
		RemovePoint,
		ScaleName,
		ScaleVisible,
		SetPoint,
		SetPointColour,
		SetPointValue,
	
		// Disordered build commands
		Disorder,
		ListComponents,
		NMols,
		Region,
		RegionCentre,
		RegionCentreFrac,
		RegionFrac,
		RegionGeometry,
		RegionGeometryFrac,
		RegionOverlaps,
		RegionShape,
		VdwScale,
	
		// Edit commands
		Copy,
		Cut,
		Delete,
		Paste,
		Redo,
		Undo,
	
		// Energy Commands
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
		Continue,
		End,
		For,
		If,
		Terminate,
	
		// Force Commands
		FrameForces,
		ModelForces,
		PrintForces,
	
		// Forcefield/Expression Commands
		AngleDef,
		BondDef,
		ClearMap,
		CreatEexpression,
		DefaultFF,
		Equivalent,
		FFModel,
		FFPattern,
		FFPatternId,
		FinaliseFF,
		GenConvert,
		Generator,
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
		GridColourScale,
		GridCubic,
		GridCutoff,
		GridLoopOrder,
		GridOrigin,
		GridOrtho,
		GridSize,
		GridStyle,
		GridSymmetric,
		GridUseZ,
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
	
		// MC Commands
		MCAccept,
		MCAllow,
		MCMaxStep,
		MCNTrials,
		PrintMC,
	
		// Measurements
		Angle,
		Angles,
		ClearMEasurements,
		Distance,
		Distances,
		ListMeasurements,
		Measure,
		Torsion,
		Torsions,
	
		// Messaging
		Error,
		Print,
		Verbose,
		Warn,
	
		// Minimisation Commands
		CGMinimise,
		Converge,
		Linetol,
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
		GetPattern,
		ListPatterns,
		NewPattern,
	
		// Preferences Commands
		AngleLabel,
		AtomDetail,
		BondDetail,
		Colour,
		CommonElements,
		DensityUnits,
		DistanceLabel,
		ECut,
		Elec,
		ElementAmbient,
		ElementDiffuse,
		ElementRadius,
		EnergyUnits,
		GL,
		HDistance,
		Intra,
		Key,
		LabelSize,
		Light,
		LightAmbient,
		LightDiffuse,
		LightPosition,
		LightSpecular,
		Mouse,
		Radius,
		ReplicateFold,
		ReplicateTrim,
		Scheme,
		Shininess,
		ShowOnScreen,
		ShowOnImage,
		Style,
		SwapBuffers,
		UseNiceText,
		VCut,
		Vdw,
		ZoomThrottle,

		// Read / Write Commands
		AddReadOption,
		Find,
		GetLine,
		ReadChars,
		ReadFloat,
		ReadInteger,
		ReadLine,
		ReadNext,
		ReadVar,
		RemoveReadOption,
		Rewind,
		SkipChars,
		SkipLine,
		WriteLine,
		WriteVar,

		// Script Commands
		ListScripts,
		LoadScript,
		RunScript,

		// Select Commands
		DeSelect,
		DeSelectType,
		Expand,
		Invert,
		Select,
		SelectAll,
		SelectFFType,
		SelectionCog,
		SelectionCom,
		SelectNone,
		SelectOverlaps,
		SelectPattern,
		SelectType,

		// Site Commands
		GetSite,
		ListSites,
		NewSite,
		SiteAxes,

		// System commands
		Debug,
		Gui,
		Help,
		Seed,
		Quit,
		Version,

		// Trajectory Commands
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
		Translate,
		TranslateAtom,
		TranslateCell,

		// Variable Manipulation
		AfterChar,
		BeforeChar,
		Normalise,
		StripChars,

		// Variable Operators
		OperatorAdd,
		OperatorAssignment,
		OperatorDivide,
		OperatorEqualTo,
		OperatorGreaterThan,
		OperatorGreaterThanEqualTo,
		OperatorLessThan,
		OperatorLessThanEqualTo,
		OperatorMultiply,
		OperatorNegate,
		OperatorNotEqualTo,
		OperatorPower,
		OperatorSubtract,

		// View
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

		nFunctions
	};
	// Return enumerated command id from string
	NuCommand::Function command(const char*);

	/*
	// Function declarations
	*/
	private:
	// AST-specific commands
	static bool function_NoFunction(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Joiner(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Initialisations(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Analyse commands
	static bool function_Finalise(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_FrameAnalyse(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Geometry(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);	
	static bool function_ModelAnalyse(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Pdens(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_PrintJobs(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Rdf(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SaveQuantities(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_TrajAnalyse(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Atom Commands
	static bool function_AtomStyle(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GetAtom(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Hide(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetCoords(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetCharge(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetElement(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetForces(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetFX(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetFY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetFZ(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetId(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetRX(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetRY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetRZ(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetVelocities(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetVX(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetVY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetVZ(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Show(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ShowAll(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Bond commands
	static bool function_Augment(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_BondTolerance(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ClearBonds(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ClearSelectedBonds(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_NewBond(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_NewBondId(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ReBond(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ReBondPatterns(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ReBondSelection(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Build commands
	static bool function_AddHydrogen(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Bohr(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Chain(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_EndChain(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_InsertAtom(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Locate(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Move(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_MoveToEnd(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_MoveToStart(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_NewAtom(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_NewAtomFrac(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ReOrder(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ResetPen(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_RotX(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_RotY(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_RotZ(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ShiftDown(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ShiftUp(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Transmute(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Cell commands
	static bool function_AddGenerator(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_AdjustCell(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Cell(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_CellAxes(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Fold(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_FoldMolecules(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_FracToReal(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_NoCell(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Pack(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_PrintCell(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Replicate(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_RotateCell(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Scale(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ScaleMolecules(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetCell(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Spacegroup(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Charge commands
	static bool function_ChargeFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ChargeFromModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ChargePAtom(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Charge(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ChargeType(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ClearCharges(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Colourscale commands
	static bool function_AddPoint(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ClearPoints(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ListScales(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_RemovePoint(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ScaleName(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ScaleVisible(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetPoint(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetPointColour(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetPointValue(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Disordered build commands
	static bool function_Disorder(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ListComponents(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_NMols(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Region(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_RegionCentre(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_RegionCentreFrac(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_RegionFrc(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_RegionGeometry(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_RegionGeometryFrac(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_RegionOverlaps(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_RegionShape(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_VdwScale(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Edit commands
	static bool function_Copy(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Cut(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Delete(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Paste(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Redo(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Undo(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Energy Commands
	static bool function_FrameEnergy(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ModelEnergy(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_PrintElec(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_PrintEwald(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_PrintInter(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_PrintIntra(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_PrintEnergy(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_PrintSummary(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_PrintVdw(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Filter Commands
	static bool function_Exact(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Extension(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Glob(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Id(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Name(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Nickname(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Zmap(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Flow control
	static bool function_Break(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Continue(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Else(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ElseIf(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_End(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_For(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Goto(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Gotononif(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_If(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Terminate(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Force Commands
	static bool function_FrameForces(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ModelForces(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_PrintForces(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Forcefield Commands
	static bool function_AngleDef(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_BondDef(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ClearMap(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_CreatEexpression(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_DefaultFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Equivalent(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_FinaliseFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_FFModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_FFPattern(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_FFPatternId(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GenConvert(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Generator(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GetFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_InterDef(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_LoadFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Map(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_NewFF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_PrintSetup(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Rules(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SaveExpression(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_TorsionDef(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_TypeDef(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_TypeModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_TypeTest(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Units(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Glyph commands
	static bool function_AutoEllipsoids(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_AutoPolyhedra(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GlyphAtomF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GlyphAtomR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GlyphAtomV(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GlyphAtomsF(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GlyphAtomsR(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GlyphAtomsV(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GlyphColour(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GlyphData(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GlyphSolid(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GlyphText(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_NewGlyph(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Grid Commands
	static bool function_AddGridPoint(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_AddNextGridPoint(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_FinaliseGrid(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GetGrid(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GridAlpha(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GridAxes(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GridColour(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GridColourNegative(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GridColourScale(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GridCubic(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GridCutoff(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GridOrtho(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GridLoopOrder(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GridOrigin(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GridSize(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GridStyle(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GridSymmetric(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GridUseZ(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_LoadGrid(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_NewGrid(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Image Commands
	static bool function_SaveBitmap(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SaveVector(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Labeling commands
	static bool function_ClearLabels(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Label(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_RemoveLabel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_RemoveLabels(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// MC Commands
	static bool function_MCAccept(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_MCAllow(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_MCMaxStep(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_MCNTrials(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_PrintMC(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Measurements
	static bool function_Angle(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Angles(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ClearMeasurements(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Distance(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Distances(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ListMeasurements(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Measure(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Torsion(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Torsions(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Messaging
	static bool function_Error(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Print(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Verbose(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Warn(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Minimisation Commands
	static bool function_CGMinimise(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Converge(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_LineTol(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_MCMinimise(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SDMinimise(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SimplexMinimise(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Model Commands
	static bool function_CreateAtoms(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_CurrentModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_FinalisMmodel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_FirstModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GetModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Info(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_LastModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ListModels(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_LoadModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_LogInfo(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ModelTemplate(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_NewModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_NextModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_PrevModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SaveModel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetName(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Pattern Commands
	static bool function_ClearPatterns(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_CreatePatterns(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GetPattern(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ListPatterns(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_NewPattern(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Preferences Commands
	static bool function_AngleLabel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_AtomDetail(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_BondDetail(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Colour(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_CommonElements(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_DensityUnits(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_DistanceLabel(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ECut(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Elec(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ElementAmbient(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ElementDiffuse(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ElementRadius(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_EnergyUnits(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_HDistance(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Intra(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GL(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Key(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_LabelSize(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Light(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_LightAmbient(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_LightDiffuse(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_LightPosition(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_LightSpecular(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Mouse(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Radius(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ReplicateFold(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ReplicateTrim(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Scheme(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Shininess(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ShowOnScreen(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ShowOnImage(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Style(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SwapBuffers(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_UseNiceText(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_VCut(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Vdw(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ZoomThrottle(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Read / Write Commands
	static bool function_AddReadOption(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Find(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_GetLine(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ReadChars(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ReadFloat(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ReadInteger(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ReadLine(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ReadNext(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ReadVar(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_RemoveReadOption(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Rewind(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SkipChars(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SkipLine(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_WriteLine(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_WriteVar(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Script Commands
	static bool function_ListScripts(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_LoadScript(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_RunScript(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Select Commands
	static bool function_DeSelect(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_DeSelectType(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Expand(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Invert(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Select(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SelectAll(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SelectFFType(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SelectionCog(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SelectionCom(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SelectNone(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SelectOverlaps(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SelectPattern(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SelectType(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Site Commands
	static bool function_GetSite(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ListSites(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_NewSite(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SiteAxes(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// System Commands
	static bool function_Debug(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Gui(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Seed(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Help(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Quit(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Version(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Trajectory Commands
	static bool function_FinaliseFrame(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_FirstFrame(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_LastFrame(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_LoadTrajectory(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_NextFrame(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_PrevFrame(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SeekFrame(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Transform Commands
	static bool function_AxisRotate(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Centre(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_MatrixConvert(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_MatrixTransform(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Mirror(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Translate(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_TranslateAtom(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_TranslateCell(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Variable Manipulation
	static bool function_AfterChar(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_BeforeChar(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Decrease(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Increase(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Normalise(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_StripChars(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// Variable Operators
	static bool function_OperatorAdd(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_OperatorAssignment(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_OperatorDivide(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_OperatorEqualTo(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_OperatorGreaterThan(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_OperatorGreaterThanEqualTo(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_OperatorLessThan(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_OperatorLessThanEqualTo(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_OperatorMultiply(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_OperatorNegate(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_OperatorNotEqualTo(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_OperatorPower(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_OperatorSubtract(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	// View
	static bool function_GetView(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Orthographic(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_Perspective(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ResetView(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_RotateView(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SetView(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_SpeedTest(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_TranslateView(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ViewAlong(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ViewAlongCell(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ZoomView(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);
	static bool function_ZRotateView(NuCommandNode *c, Bundle &obj, NuReturnValue &rv);

	/*
	// Function descriptions / syntax etc.
	*/
	private:
	// Function pointers
	NuCommandFunction pointers_[NuCommand::nFunctions];
	// Dummy CommandList for use with non-flow call() function
	NuCommandList *dummyCommandList_;
	// Dummy CommandNode (owned by dummyCommandList_)
	NuCommandNode *dummyCommandNode_;

	public:
	// Function data
	static NuCommandData data[NuCommand::nFunctions];
	// Initialise function pointers
	void initPointers();
	// Execute specified command
	int call(NuCommand::Function cf, NuCommandNode *node, NuReturnValue &rv);
};

// External declaration
extern NuCommand nucommands;

#endif
