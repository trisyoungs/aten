/*
	*** Model Variable and Array
	*** src/parser/model.cpp
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

#include "parser/model.h"
#include "parser/stepnode.h"
#include "model/model.h"
#include "main/aten.h"

ATEN_USING_NAMESPACE

/*
 * Variable
 */

// Constructor
ModelVariable::ModelVariable(Model* ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ModelData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
ModelVariable::~ModelVariable()
{
}

/*
 * Accessors
 */

// Accessor data
Accessor ModelVariable::accessorData[ModelVariable::nAccessors] = {
	{ "angles",		VTypes::MeasurementData,	-1, true },
	{ "atoms",		VTypes::AtomData,		-1, true },
	{ "bonds",		VTypes::BondData,		-1, true },
	{ "cell",		VTypes::CellData,		0, false },
	{ "componentDensity",	VTypes::DoubleData,		0, false },
	{ "componentPartition",	VTypes::IntegerData,		0, false },
	{ "componentPolicy",	VTypes::StringData,		0, false },
	{ "componentPopulation",VTypes::IntegerData,		0, false },
	{ "distances",		VTypes::MeasurementData,	-1, true },
	{ "eigenvectors",	VTypes::EigenvectorData,	-1, true },
	{ "energy",		VTypes::EnergyStoreData,	0, true },
	{ "ffAngles",		VTypes::ForcefieldBoundData,	-1, true },
	{ "ffBonds",		VTypes::ForcefieldBoundData,	-1, true },
	{ "ffMass",		VTypes::DoubleData,		0, true },
	{ "ffTorsions",		VTypes::ForcefieldBoundData,	-1, true },
	{ "ffTypes",		VTypes::ForcefieldAtomData,	-1, true },
	{ "ff",			VTypes::ForcefieldData,		0, false },
	{ "filename",		VTypes::StringData,		0, true },
	{ "frame",		VTypes::ModelData,		0, true },
	{ "frames",		VTypes::ModelData,		-1, true },
	{ "glyphs",		VTypes::GlyphData,		-1, true },
	{ "grids",		VTypes::GridData,		-1, true },
	{ "id",			VTypes::IntegerData,		0, true },
	{ "mass",		VTypes::DoubleData,		0, true },
	{ "name",		VTypes::StringData,		0, false },
	{ "nAngles",		VTypes::IntegerData,		0, true },
	{ "nAtoms",		VTypes::IntegerData,		0, true },
	{ "nBasisCartesians",	VTypes::IntegerData,		0, true },
	{ "nBasisShells",	VTypes::IntegerData,		0, true },
	{ "nBonds",		VTypes::IntegerData,		0, true },
	{ "nDistances",		VTypes::IntegerData,		0, true },
	{ "nEigenvalues",	VTypes::IntegerData,		0, true },
	{ "nFFAngles",		VTypes::IntegerData,		0, true },
	{ "nFFBonds",		VTypes::IntegerData,		0, true },
	{ "nFFTorsions",	VTypes::IntegerData,		0, true },
	{ "nFFTypes",		VTypes::IntegerData,		0, true },
	{ "nFrames",		VTypes::IntegerData,		0, true },
	{ "nGlyphs",		VTypes::IntegerData,		0, true },
	{ "nGrids",		VTypes::IntegerData,		0, true },
	{ "nPatterns",		VTypes::IntegerData,		0, true },
	{ "nSelected",		VTypes::IntegerData,		0, true },
	{ "nTorsions",		VTypes::IntegerData,		0, true },
	{ "nUnknown",		VTypes::IntegerData,		0, true },
	{ "nVibrations",	VTypes::IntegerData,		0, true },
	{ "patterns",		VTypes::PatternData,		-1, true },
	{ "propagateStyle",	VTypes::IntegerData,		0, false },
	{ "repeatCellNegative",	VTypes::VectorData,		0, false },
	{ "repeatCellPositive",	VTypes::VectorData,		0, false },
	{ "selection",		VTypes::AtomData,		-1, true },
	{ "torsions",		VTypes::MeasurementData,	-1, true },
	{ "vibrations",		VTypes::VibrationData,		0, true },
	{ "zMatrix",		VTypes::ZMatrixData,		0, true }
};

// Function data
FunctionAccessor ModelVariable::functionData[ModelVariable::nFunctions] = {
	{ "addHydrogen",	VTypes::NoData,		aten_->commandArguments(Commands::AddHydrogen),		aten_->commandArgText(Commands::AddHydrogen) },
	{ "angleEnergy",	VTypes::DoubleData,	"",							"" },
	{ "atomWithBit",	VTypes::AtomData,	"i",							"" },
	{ "augment",		VTypes::NoData,		aten_->commandArguments(Commands::Augment),		aten_->commandArgText(Commands::Augment) },
	{ "axisRotateSelection",VTypes::NoData,		aten_->commandArguments(Commands::AxisRotate),		aten_->commandArgText(Commands::AxisRotate) },
	{ "bondEnergy",		VTypes::DoubleData,	"",							"" },
	{ "centreSelection",	VTypes::NoData,		aten_->commandArguments(Commands::Centre),		aten_->commandArgText(Commands::Centre) },
	{ "charge",		VTypes::NoData,		aten_->commandArguments(Commands::Charge),		aten_->commandArgText(Commands::Charge) },
	{ "clearBonds",		VTypes::NoData,		aten_->commandArguments(Commands::ClearBonds),		aten_->commandArgText(Commands::ClearBonds) },
	{ "clearCharges",	VTypes::NoData,		aten_->commandArguments(Commands::ClearCharges),		aten_->commandArgText(Commands::ClearCharges) },
	{ "clearPatterns",	VTypes::NoData,		"",							aten_->commandArgText(Commands::ClearPatterns) },
	{ "clearSelectedBonds",	VTypes::NoData,		aten_->commandArguments(Commands::ClearSelectedBonds),	aten_->commandArgText(Commands::ClearSelectedBonds) },
	{ "copy",		VTypes::NoData,		aten_->commandArguments(Commands::Copy),			aten_->commandArgText(Commands::Copy) },
	{ "cut",		VTypes::NoData,		aten_->commandArguments(Commands::Cut),			aten_->commandArgText(Commands::Cut) },
	{ "delete",		VTypes::NoData,		aten_->commandArguments(Commands::Delete),		aten_->commandArgText(Commands::Delete) },
	{ "deselect",		VTypes::NoData,		aten_->commandArguments(Commands::DeSelect),		aten_->commandArgText(Commands::DeSelect) },
	{ "elecEnergy",		VTypes::DoubleData,	"",							"" },
	{ "expand",		VTypes::NoData,		aten_->commandArguments(Commands::Expand),		aten_->commandArgText(Commands::Expand) },
	{ "flipSelectionX",	VTypes::NoData,		aten_->commandArguments(Commands::FlipX),		aten_->commandArgText(Commands::FlipX) },
	{ "flipSelectionY",	VTypes::NoData,		aten_->commandArguments(Commands::FlipY),		aten_->commandArgText(Commands::FlipY) },
	{ "flipSelectionZ",	VTypes::NoData,		aten_->commandArguments(Commands::FlipZ),		aten_->commandArgText(Commands::FlipZ) },
	{ "interEnergy",	VTypes::DoubleData,	"",							"" },
	{ "intraEnergy",	VTypes::DoubleData,	"",							"" },
	{ "isPeriodic",		VTypes::IntegerData,	"",							"" },
	{ "matrixConvertSelection",	 VTypes::NoData,	aten_->commandArguments(Commands::MatrixConvert),         aten_->commandArgText(Commands::MatrixConvert) },
	{ "matrixTransformSelection",	 VTypes::NoData,	aten_->commandArguments(Commands::MatrixTransform),         aten_->commandArgText(Commands::MatrixTransform) },
	{ "mirrorSelection",	VTypes::NoData,		aten_->commandArguments(Commands::Mirror),		aten_->commandArgText(Commands::Mirror) },
	{ "movePen",		VTypes::NoData,		aten_->commandArguments(Commands::Move),		aten_->commandArgText(Commands::Move) },
	{ "moveToEnd",		VTypes::NoData,		aten_->commandArguments(Commands::MoveToEnd),		aten_->commandArgText(Commands::MoveToEnd) },
	{ "moveToStart",	VTypes::NoData,		aten_->commandArguments(Commands::MoveToStart),		aten_->commandArgText(Commands::MoveToStart) },
	{ "newAtom",		VTypes::AtomData,	aten_->commandArguments(Commands::NewAtom),		aten_->commandArgText(Commands::NewAtom) },
	{ "newAtomFrac",	VTypes::AtomData,	aten_->commandArguments(Commands::NewAtomFrac),		aten_->commandArgText(Commands::NewAtomFrac) },
	{ "newBasisShell",	VTypes::BasisShellData,	aten_->commandArguments(Commands::NewBasisShell),	aten_->commandArgText(Commands::NewBasisShell) },
	{ "newBond",		VTypes::BondData,	aten_->commandArguments(Commands::NewBond),		aten_->commandArgText(Commands::NewBond) },
	{ "newEigenvector",	VTypes::EigenvectorData,aten_->commandArguments(Commands::NewEigenvector),	aten_->commandArgText(Commands::NewEigenvector) },
	{ "newGlyph",		VTypes::GlyphData,	aten_->commandArguments(Commands::NewGlyph),		aten_->commandArgText(Commands::NewGlyph) },
	{ "newGrid",		VTypes::GridData,	aten_->commandArguments(Commands::NewGrid),		aten_->commandArgText(Commands::NewGrid) },
	{ "newPattern",		VTypes::PatternData,	aten_->commandArguments(Commands::NewPattern),		aten_->commandArgText(Commands::NewPattern) },
	{ "newVibration",	VTypes::VibrationData,	aten_->commandArguments(Commands::NewVibration),	aten_->commandArgText(Commands::NewVibration) },
	{ "paste",		VTypes::NoData,		aten_->commandArguments(Commands::Paste),		aten_->commandArgText(Commands::Paste) },
	{ "rebond",		VTypes::NoData,		aten_->commandArguments(Commands::ReBond),		aten_->commandArgText(Commands::ReBond) },
	{ "rebondPatterns",	VTypes::NoData,		aten_->commandArguments(Commands::ReBondPatterns),	aten_->commandArgText(Commands::ReBondPatterns) },
	{ "rebondSelection",	VTypes::NoData,		aten_->commandArguments(Commands::ReBondSelection),	aten_->commandArgText(Commands::ReBondSelection) },
	{ "redo",		VTypes::NoData,		aten_->commandArguments(Commands::Redo),		aten_->commandArgText(Commands::Redo) },
	{ "reorder",		VTypes::NoData,		aten_->commandArguments(Commands::ReOrder),		aten_->commandArgText(Commands::ReOrder) },
	{ "reorientSelection",	VTypes::NoData,		aten_->commandArguments(Commands::Reorient),		aten_->commandArgText(Commands::Reorient) },
	{ "resetPen",		VTypes::NoData,		aten_->commandArguments(Commands::ResetPen),		aten_->commandArgText(Commands::ResetPen) },
	{ "rotatePenX",		VTypes::NoData,		aten_->commandArguments(Commands::RotX),		aten_->commandArgText(Commands::RotX) },
	{ "rotatePenY",		VTypes::NoData,		aten_->commandArguments(Commands::RotY),		aten_->commandArgText(Commands::RotY) },
	{ "rotatePenZ",		VTypes::NoData,		aten_->commandArguments(Commands::RotZ),		aten_->commandArgText(Commands::RotZ) },
	{ "saveBitmap",		VTypes::NoData,		aten_->commandArguments(Commands::SaveBitmap),		aten_->commandArgText(Commands::SaveBitmap) },
	{ "select",		VTypes::NoData,		aten_->commandArguments(Commands::Select),		aten_->commandArgText(Commands::Select) },
	{ "selectAll",		VTypes::NoData,		aten_->commandArguments(Commands::SelectAll),		aten_->commandArgText(Commands::SelectAll) },
	{ "selectionAddHydrogen",VTypes::NoData,	aten_->commandArguments(Commands::SelectionAddHydrogen),aten_->commandArgText(Commands::SelectionAddHydrogen) },
	{ "selectNone",		VTypes::NoData,		aten_->commandArguments(Commands::SelectNone),		aten_->commandArgText(Commands::SelectNone) },
	{ "selectTree",		VTypes::NoData,		aten_->commandArguments(Commands::SelectTree),		aten_->commandArgText(Commands::SelectTree) },
	{ "setAngles",		VTypes::NoData,		aten_->commandArguments(Commands::SetAngles),		aten_->commandArgText(Commands::SetAngles) },
	{ "setDistances",	VTypes::NoData,		aten_->commandArguments(Commands::SetDistances),	aten_->commandArgText(Commands::SetDistances) },
	{ "setTorsions",	VTypes::NoData,		aten_->commandArguments(Commands::SetTorsions),		aten_->commandArgText(Commands::SetTorsions) },
	{ "setupComponent",	VTypes::NoData,		aten_->commandArguments(Commands::SetupComponent),	aten_->commandArgText(Commands::SetupComponent) },
	{ "shiftDown",		VTypes::NoData,		aten_->commandArguments(Commands::ShiftDown),		aten_->commandArgText(Commands::ShiftDown) },
	{ "shiftUp",		VTypes::NoData,		aten_->commandArguments(Commands::ShiftUp),		aten_->commandArgText(Commands::ShiftUp) },
	{ "showAll",		VTypes::NoData,		aten_->commandArguments(Commands::ShowAll),		aten_->commandArgText(Commands::ShowAll) },
	{ "toAngstroms",	VTypes::NoData,		"",							"" },
	{ "torsionEnergy",	VTypes::DoubleData,	"",							"" },
	{ "translateSelection",	VTypes::NoData,		aten_->commandArguments(Commands::Translate),		aten_->commandArgText(Commands::Translate) },
	{ "transmute",		VTypes::NoData,		aten_->commandArguments(Commands::Transmute),		aten_->commandArgText(Commands::Transmute) },
	{ "undo",		VTypes::NoData,		aten_->commandArguments(Commands::Undo),		aten_->commandArgText(Commands::Undo) },
	{ "vdwEnergy",		VTypes::DoubleData,	"",							"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* ModelVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return ModelVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* ModelVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("ModelVariable::accessorSearch");
	StepNode* result = NULL;
	int i = 0;
	i = Variable::searchAccessor(name, nAccessors, accessorData);
	if (i == -1)
	{
		// No accessor found - is it a function definition?
		// for (i = 0; i < nFunctions; i++) if (strcmp(qPrintable(functionData[i].name),s) == 0) break;
		i = Variable::searchAccessor(name, nFunctions, functionData);
		if (i == -1)
		{
			Messenger::print("Error: Type 'Model&' has no member or function named '%s'.", qPrintable(name));
			printValidAccessors(nAccessors, accessorData, nFunctions, functionData);
			Messenger::exit("ModelVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, qPrintable(functionData[i].name));
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Model&' function named '%s'.", qPrintable(name));
			Messenger::exit("ModelVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ModelData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, qPrintable(functionData[i].name)))
		{
			Messenger::print("Error: Syntax for 'Model&' function '%s' is '%s(%s)'.", qPrintable(functionData[i].name), qPrintable(functionData[i].name), qPrintable(functionData[i].argText) );
			delete result;
			result = NULL;
		}
	}
	else
	{
		Messenger::print(Messenger::Parse, "Accessor match = %i (%s)", i, qPrintable(accessorData[i].name));
		// Were we given an array index when we didn't want one?
		if ((accessorData[i].arraySize == 0) && (arrayIndex != NULL))
		{
			Messenger::print("Error: Irrelevant array index provided for member '%s'.", qPrintable(accessorData[i].name));
			result = NULL;
		}
		// Were we given an argument list when we didn't want one?
		if (argList != NULL)
		{
			Messenger::print("Error: Argument list given to 'Model&' array member '%s'.", qPrintable(name));
			Messenger::exit("ModelVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::ModelData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("ModelVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ModelVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ModelVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Model type.\n", i);
		Messenger::exit("ModelVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", qPrintable(accessorData[i].name));
		Messenger::exit("ModelVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", qPrintable(accessorData[i].name), arrayIndex, accessorData[i].arraySize);
			Messenger::exit("ModelVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	Model* ptr = (Model*) rv.asPointer(VTypes::ModelData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ModelData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (ModelVariable::Angles):
			if (!hasArrayIndex) rv.set(VTypes::MeasurementData, ptr->angleMeasurements());
			else if (arrayIndex > ptr->nAngleMeasurements())
			{
				Messenger::print("Angle array index (%i) is out of bounds for model '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::MeasurementData, ptr->angleMeasurement(arrayIndex-1));
			break;
		case (ModelVariable::Atoms):
			if (!hasArrayIndex) rv.set(VTypes::AtomData, ptr->atoms());
			else if (arrayIndex > ptr->nAtoms())
			{
				Messenger::print("Atom array index (%i) is out of bounds for model '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::AtomData, ptr->atom(arrayIndex-1));
			break;
		case (ModelVariable::Bonds):
			if (!hasArrayIndex) rv.set(VTypes::BondData, ptr->bonds());
			else if (arrayIndex > ptr->nBonds())
			{
				Messenger::print("Bond array index (%i) is out of bounds for model '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::BondData, ptr->bond(arrayIndex-1));
			break;
		case (ModelVariable::Celldata):
			rv.set(VTypes::CellData, &ptr->cell());
			break;
		case (ModelVariable::ComponentDensity):
			rv.set(ptr->componentDensity());
			break;
		case (ModelVariable::ComponentPartition):
			rv.set(ptr->componentPartition()+1);
			break;
		case (ModelVariable::ComponentPolicy):
			rv.set( Model::insertionPolicy(ptr->componentInsertionPolicy()) );
			break;
		case (ModelVariable::ComponentPopulation):
			rv.set(ptr->componentPopulation());
			break;
		case (ModelVariable::Distances):
			if (!hasArrayIndex) rv.set(VTypes::MeasurementData, ptr->distanceMeasurements());
			else if (arrayIndex > ptr->nDistanceMeasurements())
			{
				Messenger::print("Distance array index (%i) is out of bounds for model '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::MeasurementData, ptr->distanceMeasurement(arrayIndex-1));
			break;
		case (ModelVariable::Energy):
			rv.set(VTypes::EnergyStoreData, &ptr->energy);
			break;
		case (ModelVariable::FFAngles):
			if (!hasArrayIndex)
			{
				if (ptr->forcefieldAngles() != NULL) rv.set(VTypes::ForcefieldBoundData, ptr->forcefieldAngles()->item, ptr->forcefieldAngles());
				else rv.set(VTypes::PatternBoundData, NULL);
			}
			else if (arrayIndex > ptr->nForcefieldAngles())
			{
				Messenger::print("Forcefield angle array index (%i) is out of bounds for model '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::ForcefieldBoundData, ptr->forcefieldAngle(arrayIndex-1)->item, ptr->forcefieldAngle(arrayIndex-1));
			break;
		case (ModelVariable::FFBonds):
			if (!hasArrayIndex)
			{
				if (ptr->forcefieldBonds() != NULL) rv.set(VTypes::ForcefieldBoundData, ptr->forcefieldBonds()->item, ptr->forcefieldBonds());
				else rv.set(VTypes::ForcefieldBoundData, NULL);
			}
			else if (arrayIndex > ptr->nForcefieldBonds())
			{
				Messenger::print("Forcefield bond array index (%i) is out of bounds for model '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::ForcefieldBoundData, ptr->forcefieldBond(arrayIndex-1)->item, ptr->forcefieldBond(arrayIndex-1));
			break;
		case (ModelVariable::FFTorsions):
			if (!hasArrayIndex)
			{
				if (ptr->forcefieldTorsions() != NULL) rv.set(VTypes::ForcefieldBoundData, ptr->forcefieldTorsions()->item, ptr->forcefieldTorsions());
				else rv.set(VTypes::ForcefieldBoundData, NULL);
			}
			else if (arrayIndex > ptr->nForcefieldTorsions())
			{
				Messenger::print("Forcefield torsion array index (%i) is out of bounds for model '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::ForcefieldBoundData, ptr->forcefieldTorsion(arrayIndex-1)->item, ptr->forcefieldTorsion(arrayIndex-1));
			break;
		case (ModelVariable::FFMass):
			rv.set(ptr->forcefieldMass());
			break;
		case (ModelVariable::FFTypes):
			if (!hasArrayIndex)
			{
				if (ptr->uniqueForcefieldTypes() != NULL) rv.set(VTypes::ForcefieldAtomData, ptr->uniqueForcefieldTypes()->item, ptr->uniqueForcefieldTypes());
				else rv.set(VTypes::ForcefieldAtomData, NULL);
			}
			else if (arrayIndex > ptr->nUniqueForcefieldTypes())
			{
				Messenger::print("Forcefield types array index (%i) is out of bounds for model '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::ForcefieldAtomData, ptr->uniqueForcefieldType(arrayIndex-1)->item, ptr->uniqueForcefieldType(arrayIndex-1));
			break;
		case (ModelVariable::FField):
			rv.set(VTypes::ForcefieldData, ptr->forcefield());
			break;
		case (ModelVariable::Filename):
			rv.set(ptr->filename());
			break;
		case (ModelVariable::Frame):
			rv.set(VTypes::ModelData, ptr->trajectoryCurrentFrame());
			break;
		case (ModelVariable::Frames):
			// Only works for a cached trajectory
			if (!ptr->isTrajectoryCached())
			{
				Messenger::print("Trajectory for model '%s' is not cached - individual frame pointers not available.", qPrintable(ptr->name()));
				result = false;
			}
			else if (!hasArrayIndex) rv.set(VTypes::ModelData, ptr->trajectoryFrame(0));
			else if ((arrayIndex < 1) || (arrayIndex > ptr->nTrajectoryFrames()))
			{
				Messenger::print("Frame array index '%i' is out of bounds for model '%s' whose trajectory has %i frames.", arrayIndex, qPrintable(ptr->name()), ptr->nTrajectoryFrames());
				result = false;
			}
			else rv.set(VTypes::ModelData, ptr->trajectoryFrame(arrayIndex-1));
			break;
		case (ModelVariable::Glyphs):
			if (!hasArrayIndex)
			{
				if (ptr->glyphs() != NULL) rv.set(VTypes::GlyphData, ptr->glyphs());
				else rv.set(VTypes::GlyphData, NULL);
			}
			else if (arrayIndex > ptr->nGlyphs())
			{
				Messenger::print("Glyph array index (%i) is out of bounds for model '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::GlyphData, ptr->glyph(arrayIndex-1));
			break;
		case (ModelVariable::Grids):
			if (!hasArrayIndex)
			{
				if (ptr->grids() != NULL) rv.set(VTypes::GridData, ptr->grids());
				else rv.set(VTypes::GridData, NULL);
			}
			else if (arrayIndex > ptr->nGrids())
			{
				Messenger::print("Grid array index (%i) is out of bounds for model '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::GridData, ptr->grid(arrayIndex-1));
			break;
		case (ModelVariable::Id):
			rv.set(aten_->modelIndex(ptr)+1);
			break;
		case (ModelVariable::Mass):
			rv.set(ptr->mass());
			break;
		case (ModelVariable::Name):
			rv.set(ptr->name());
			break;
 		case (ModelVariable::NAngles):
			rv.set(ptr->nAngleMeasurements());
			break;
		case (ModelVariable::NAtoms):
			rv.set(ptr->nAtoms());
			break;
		case (ModelVariable::NBasisCartesians):
			rv.set(ptr->nCartesianBasisFunctions());
			break;
		case (ModelVariable::NBasisShells):
			rv.set(ptr->nBasisShells());
			break;
		case (ModelVariable::NBonds):
			rv.set(ptr->nBonds());
			break;
		case (ModelVariable::NDistances):
			rv.set(ptr->nDistanceMeasurements());
			break;
 		case (ModelVariable::NEigenvectors):
			rv.set(ptr->nEigenvectors());
			break;
		case (ModelVariable::NFFAngles):
			rv.set(ptr->nForcefieldAngles());
			break;
		case (ModelVariable::NFFBonds):
			rv.set(ptr->nForcefieldBonds());
			break;
		case (ModelVariable::NFFTorsions):
			rv.set(ptr->nForcefieldTorsions());
			break;
		case (ModelVariable::NFFTypes):
			rv.set(ptr->nUniqueForcefieldTypes());
			break;
		case (ModelVariable::NFrames):
			rv.set(ptr->nTrajectoryFrames());
			break;
		case (ModelVariable::NGlyphs):
			rv.set(ptr->nGlyphs());
			break;
		case (ModelVariable::NGrids):
			rv.set(ptr->nGrids());
			break;
		case (ModelVariable::NPatterns):
			rv.set(ptr->nPatterns());
			break;
		case (ModelVariable::NSelected):
			rv.set(ptr->nSelected());
			break;
		case (ModelVariable::NTorsions):
			rv.set(ptr->nTorsionMeasurements());
			break;
		case (ModelVariable::NUnknown):
			rv.set(ptr->nUnknownAtoms());
			break;
		case (ModelVariable::NVibrations):
			rv.set(ptr->nVibrations());
			break;
		case (ModelVariable::Patterns):
			if (!hasArrayIndex) rv.set(VTypes::PatternData, ptr->patterns());
			else if (arrayIndex > ptr->nPatterns())
			{
				Messenger::print("Pattern array index (%i) is out of bounds for model '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::PatternData, ptr->pattern(arrayIndex-1));
			break;
		case (ModelVariable::PropagateStyle):
			rv.set(ptr->trajectoryPropagateParentStyle());
			break;
		case (ModelVariable::RepeatCellNegative):
			rv.set(ptr->repeatCellsNegative().x, ptr->repeatCellsNegative().y, ptr->repeatCellsNegative().z);
			break;
		case (ModelVariable::RepeatCellPositive):
			rv.set(ptr->repeatCellsPositive().x, ptr->repeatCellsPositive().y, ptr->repeatCellsPositive().z);
			break;
		case (ModelVariable::Selection):
			if (!hasArrayIndex)
			{
				if (ptr->selection() == NULL) rv.set(VTypes::AtomData, NULL);
				else rv.set(VTypes::AtomData, ptr->selection()->item, ptr->selection());
			}
			else if (arrayIndex > ptr->nSelected())
			{
				Messenger::print("Selection array index (%i) is out of bounds for model '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::AtomData, ptr->selected(arrayIndex-1)->item, ptr->selected(arrayIndex-1));
			break;
		case (ModelVariable::Torsions):
			if (!hasArrayIndex) rv.set(VTypes::MeasurementData, ptr->torsionMeasurements());
			else if (arrayIndex > ptr->nTorsionMeasurements())
			{
				Messenger::print("Torsions array index (%i) is out of bounds for model '%s'", arrayIndex, qPrintable(ptr->name()));
				result = false;
			}
			else rv.set(VTypes::MeasurementData, ptr->torsionMeasurement(arrayIndex-1));
			break;
		case (ModelVariable::ZMatrix):
			rv.set(VTypes::ZMatrixData, ptr->zMatrix());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ModelVariable.\n", qPrintable(accessorData[i].name));
			result = false;
			break;
	}
	Messenger::exit("ModelVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ModelVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ModelVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Model type.\n", i);
		Messenger::exit("ModelVariable::setAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;

	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = checkAccessorArrays(accessorData[acc], newValue, hasArrayIndex, arrayIndex);
	if (!result)
	{
		Messenger::exit("ModelVariable::setAccessor");
		return false;
	}

	// Get current data from ReturnValue
	Model::InsertionPolicy inspol;
	Model* ptr = (Model*) sourcerv.asPointer(VTypes::ModelData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ModelData));
		result = false;
	}
	int n;
	if (result) switch (acc)
	{
		case (ModelVariable::Celldata):
			ptr->setCell( ((UnitCell*) newValue.asPointer(VTypes::CellData)) );
			break;
		case (ModelVariable::ComponentDensity):
			ptr->setComponentDensity( newValue.asDouble() );
			break;
		case (ModelVariable::ComponentPartition):
			ptr->setComponentPartition( newValue.asInteger()-1 );
			break;
		case (ModelVariable::ComponentPolicy):
			inspol = Model::insertionPolicy(newValue.asString(), true);
			if (inspol == Model::nInsertionPolicies) result = false;
			else ptr->setComponentInsertionPolicy(inspol);
			break;
		case (ModelVariable::ComponentPopulation):
			ptr->setComponentPopulation( newValue.asInteger() );
			break;
		case (ModelVariable::FField):
			ptr->setForcefield( (Forcefield*) newValue.asPointer(VTypes::ForcefieldData) );
			break;
		case (ModelVariable::Name):
			ptr->setName(newValue.asString());
			break;
		case (ModelVariable::PropagateStyle):
			ptr->setTrajectoryPropagateParentStyle( newValue.asBool() );
			break;
		case (ModelVariable::RepeatCellNegative):
			if (newValue.type() == VTypes::VectorData) for (n=0; n<3; ++n) ptr->setRepeatCellsNegative(n, newValue.asVector(result)[n]);
			else if (newValue.arraySize() != -1) for (n=0; n<newValue.arraySize(); ++n) ptr->setRepeatCellsNegative(n, newValue.asInteger(n, result));
			else if (hasArrayIndex) ptr->setRepeatCellsNegative(arrayIndex-1, newValue.asInteger(result));
			else for (n=0; n<3; ++n) ptr->setRepeatCellsNegative(n, newValue.asInteger(result));
			ptr->logChange(Log::Style);
			break;
		case (ModelVariable::RepeatCellPositive):
			if (newValue.type() == VTypes::VectorData) for (n=0; n<3; ++n) ptr->setRepeatCellsPositive(n, newValue.asVector(result)[n]);
			else if (newValue.arraySize() != -1) for (n=0; n<newValue.arraySize(); ++n) ptr->setRepeatCellsPositive(n, newValue.asInteger(n, result));
			else if (hasArrayIndex) ptr->setRepeatCellsPositive(arrayIndex-1, newValue.asInteger(result));
			else for (n=0; n<3; ++n) ptr->setRepeatCellsPositive(n, newValue.asInteger(result));
			ptr->logChange(Log::Style);
			break;
		default:
			printf("ModelVariable::setAccessor doesn't know how to use member '%s'.\n", qPrintable(accessorData[acc].name));
			result = false;
			break;
	}
	Messenger::exit("ModelVariable::setAccessor");
	return result;
}

// Perform desired function
bool ModelVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("ModelVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Model type.\n", i);
		Messenger::exit("ModelVariable::performFunction");
		return false;
	}

	// Get current data from ReturnValue
	bool result = true;
	Model* ptr = (Model*) rv.asPointer(VTypes::ModelData, result);
	int bit;

	// Construct temporary bundle object containing our model pointer
	Bundle bundle(ptr);

	if (result) switch (i)
	{
		case (ModelVariable::AddHydrogen):
			result = aten_->callCommand(Commands::AddHydrogen, node, rv, bundle);
			break;
		case (ModelVariable::AngleEnergy):
			rv.set( ptr->angleEnergy(ptr, result));
			break;
		case (ModelVariable::AtomWithBit):
			rv.reset();
			bit = node->argi(0);
			for (Atom* j = ptr->atoms(); j != NULL; j = j->next) if (j->bit() == bit) rv.set(VTypes::AtomData, j);
			break;
 		case (ModelVariable::Augment):
			result = aten_->callCommand(Commands::Augment, node, rv, bundle);
			break;
		case (ModelVariable::AxisRotateSelection):
			result = aten_->callCommand(Commands::AxisRotate, node, rv, bundle);
			break;
		case (ModelVariable::BondEnergy):
			rv.set( ptr->bondEnergy(ptr, result));
			break;
		case (ModelVariable::CentreSelection):
			result = aten_->callCommand(Commands::Centre, node, rv, bundle);
			break;
 		case (ModelVariable::Charge):
			result = aten_->callCommand(Commands::Charge, node, rv, bundle);
			break;
 		case (ModelVariable::ClearBonds):
			result = aten_->callCommand(Commands::ClearBonds, node, rv, bundle);
			break;
 		case (ModelVariable::ClearCharges):
			result = aten_->callCommand(Commands::ClearCharges, node, rv, bundle);
			break;
		case (ModelVariable::ClearPatterns):
			ptr->clearPatterns();
			result = true;
			break;
 		case (ModelVariable::ClearSelectedBonds):
			result = aten_->callCommand(Commands::ClearSelectedBonds, node, rv, bundle);
			break;
 		case (ModelVariable::Copy):
			result = aten_->callCommand(Commands::Copy, node, rv, bundle);
			break;
 		case (ModelVariable::Cut):
			result = aten_->callCommand(Commands::Cut, node, rv, bundle);
			break;
 		case (ModelVariable::Delete):
			result = aten_->callCommand(Commands::Delete, node, rv, bundle);
			break;
		case (ModelVariable::DeSelect):
			result = aten_->callCommand(Commands::DeSelect, node, rv, bundle);
			break;
		case (ModelVariable::ElectrostaticEnergy):
			rv.set( ptr->electrostaticEnergy(ptr, result));
			break;
 		case (ModelVariable::Expand):
			result = aten_->callCommand(Commands::Expand, node, rv, bundle);
			break;
		case (ModelVariable::FlipSelectionX):
			result = aten_->callCommand(Commands::FlipX, node, rv, bundle);
			break;
		case (ModelVariable::FlipSelectionY):
			result = aten_->callCommand(Commands::FlipY, node, rv, bundle);
			break;
		case (ModelVariable::FlipSelectionZ):
			result = aten_->callCommand(Commands::FlipZ, node, rv, bundle);
			break;
		case (ModelVariable::InterEnergy):
			rv.set( ptr->intermolecularEnergy(ptr, result));
			break;
		case (ModelVariable::IntraEnergy):
			rv.set( ptr->intramolecularEnergy(ptr, result));
			break;
		case (ModelVariable::IsPeriodic):
			rv.set( ptr->isPeriodic() );
			break;
		case (ModelVariable::MatrixConvertSelection):
			result = aten_->callCommand(Commands::MatrixConvert, node, rv, bundle);
			break;
		case (ModelVariable::MatrixTransformSelection):
			result = aten_->callCommand(Commands::MatrixTransform, node, rv, bundle);
			break;
		case (ModelVariable::MirrorSelection):
			result = aten_->callCommand(Commands::Mirror, node, rv, bundle);
			break;
 		case (ModelVariable::MovePen):
			result = aten_->callCommand(Commands::Move, node, rv, bundle);
			break;
 		case (ModelVariable::MoveToEnd):
			result = aten_->callCommand(Commands::MoveToEnd, node, rv, bundle);
			break;
 		case (ModelVariable::MoveToStart):
			result = aten_->callCommand(Commands::MoveToStart, node, rv, bundle);
			break;
 		case (ModelVariable::NewAtom):
			result = aten_->callCommand(Commands::NewAtom, node, rv, bundle);
			break;
 		case (ModelVariable::NewAtomFrac):
			result = aten_->callCommand(Commands::NewAtomFrac, node, rv, bundle);
			break;
 		case (ModelVariable::NewBasisShell):
			result = aten_->callCommand(Commands::NewBasisShell, node, rv, bundle);
			break;
 		case (ModelVariable::NewBond):
			result = aten_->callCommand(Commands::NewBond, node, rv, bundle);
			break;
 		case (ModelVariable::NewEigenvector):
			result = aten_->callCommand(Commands::NewEigenvector, node, rv, bundle);
			break;
 		case (ModelVariable::NewGlyph):
			result = aten_->callCommand(Commands::NewGlyph, node, rv, bundle);
			break;
 		case (ModelVariable::NewGrid):
			result = aten_->callCommand(Commands::NewGrid, node, rv, bundle);
			break;
		case (ModelVariable::NewPattern):
			rv.set(VTypes::PatternData, ptr->addPattern(node->argc(0), node->argi(1), node->argi(2)) );
			result = (rv.asPointer(VTypes::PatternData) != NULL);
			break;
 		case (ModelVariable::NewVibration):
			result = aten_->callCommand(Commands::NewVibration, node, rv, bundle);
			break;
 		case (ModelVariable::Paste):
			result = aten_->callCommand(Commands::Paste, node, rv, bundle);
			break;
 		case (ModelVariable::ReBond):
			result = aten_->callCommand(Commands::ReBond, node, rv, bundle);
			break;
 		case (ModelVariable::ReBondPatterns):
			result = aten_->callCommand(Commands::ReBondPatterns, node, rv, bundle);
			break;
 		case (ModelVariable::ReBondSelection):
			result = aten_->callCommand(Commands::ReBondSelection, node, rv, bundle);
			break;
 		case (ModelVariable::Redo):
			result = aten_->callCommand(Commands::Redo, node, rv, bundle);
			break;
 		case (ModelVariable::ReOrder):
			result = aten_->callCommand(Commands::ReOrder, node, rv, bundle);
			break;
		case (ModelVariable::ReOrientSelection):
			result = aten_->callCommand(Commands::Reorient, node, rv, bundle);
			break;
 		case (ModelVariable::ResetPen):
			result = aten_->callCommand(Commands::ResetPen, node, rv, bundle);
			break;
 		case (ModelVariable::RotatePenX):
			result = aten_->callCommand(Commands::RotX, node, rv, bundle);
			break;
 		case (ModelVariable::RotatePenY):
			result = aten_->callCommand(Commands::RotY, node, rv, bundle);
			break;
 		case (ModelVariable::RotatePenZ):
			result = aten_->callCommand(Commands::RotZ, node, rv, bundle);
			break;
		case (ModelVariable::SaveBitmap):
			result = aten_->callCommand(Commands::SaveBitmap, node, rv, bundle);
			break;
 		case (ModelVariable::Select):
			result = aten_->callCommand(Commands::Select, node, rv, bundle);
			break;
 		case (ModelVariable::SelectAll):
			result = aten_->callCommand(Commands::SelectAll, node, rv, bundle);
			break;
 		case (ModelVariable::SelectionAddHydrogen):
			result = aten_->callCommand(Commands::SelectionAddHydrogen, node, rv, bundle);
			break;
 		case (ModelVariable::SelectNone):
			result = aten_->callCommand(Commands::SelectNone, node, rv, bundle);
			break;
 		case (ModelVariable::SelectTree):
			result = aten_->callCommand(Commands::SelectTree, node, rv, bundle);
			break;
 		case (ModelVariable::SetupComponent):
			result = aten_->callCommand(Commands::SetupComponent, node, rv, bundle);
			break;
 		case (ModelVariable::ShiftDown):
			result = aten_->callCommand(Commands::ShiftDown, node, rv, bundle);
			break;
 		case (ModelVariable::ShiftUp):
			result = aten_->callCommand(Commands::ShiftUp, node, rv, bundle);
			break;
		case (ModelVariable::ShowAll):
			result = aten_->callCommand(Commands::ShowAll, node, rv, bundle);
			break;
 		case (ModelVariable::ToAngstroms):
			ptr->bohrToAngstrom();
			break;
 		case (ModelVariable::TorsionEnergy):
			rv.set( ptr->torsionEnergy(ptr, result));
			break;
		case (ModelVariable::TranslateSelection):
			result = aten_->callCommand(Commands::Translate, node, rv, bundle);
			break;
 		case (ModelVariable::Transmute):
			result = aten_->callCommand(Commands::Transmute, node, rv, bundle);
			break;
 		case (ModelVariable::Undo):
			result = aten_->callCommand(Commands::Undo, node, rv, bundle);
			break;
		case (ModelVariable::VdwEnergy):
			rv.set( ptr->vdwEnergy(ptr, result));
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in ModelVariable.\n", qPrintable(functionData[i].name));
			result = false;
			break;
	}
	Messenger::exit("ModelVariable::performFunction");
	return result;
}

/*
 * Variable Array
 */

// Constructor
ModelArrayVariable::ModelArrayVariable(TreeNode* sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ModelData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode* ModelArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return ModelVariable::accessorSearch(name, arrayIndex, argList);
}
