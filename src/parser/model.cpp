/*
	*** Model Variable and Array
	*** src/parser/model.cpp
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

#include "parser/model.h"
#include "parser/stepnode.h"
#include "model/model.h"
#include "base/constants.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "main/aten.h"

/*
// Variable
*/

// Constructor
ModelVariable::ModelVariable(Model *ptr, bool constant)
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
// Accessors
*/

// Accessor data
Accessor ModelVariable::accessorData[ModelVariable::nAccessors] = {
	{ "angles",		VTypes::MeasurementData,	-1, TRUE },
	{ "atoms",		VTypes::AtomData,		-1, TRUE },
	{ "bonds",		VTypes::BondData,		-1, TRUE },
	{ "cell",		VTypes::CellData,		0, FALSE },
	{ "componentDensity",	VTypes::DoubleData,		0, FALSE },
	{ "componentPartition",	VTypes::IntegerData,		0, FALSE },
	{ "componentPolicy",	VTypes::StringData,		0, FALSE },
	{ "componentPopulation",VTypes::IntegerData,		0, FALSE },
	{ "distances",		VTypes::MeasurementData,	-1, TRUE },
	{ "eigenvectors",	VTypes::EigenvectorData,	-1, TRUE },
	{ "energy",		VTypes::EnergyStoreData,	0, TRUE },
	{ "ffAngles",		VTypes::ForcefieldBoundData,	-1, TRUE },
	{ "ffBonds",		VTypes::ForcefieldBoundData,	-1, TRUE },
	{ "ffMass",		VTypes::DoubleData,		0, TRUE },
	{ "ffTorsions",		VTypes::ForcefieldBoundData,	-1, TRUE },
	{ "ffTypes",		VTypes::ForcefieldAtomData,	-1, TRUE },
	{ "ff",			VTypes::ForcefieldData,		0, FALSE },
	{ "filename",		VTypes::StringData,		0, TRUE },
	{ "frame",		VTypes::ModelData,		0, TRUE },
	{ "frames",		VTypes::ModelData,		-1, TRUE },
	{ "glyphs",		VTypes::GlyphData,		-1, TRUE },
	{ "grids",		VTypes::GridData,		-1, TRUE },
	{ "id",			VTypes::IntegerData,		0, TRUE },
	{ "mass",		VTypes::DoubleData,		0, TRUE },
	{ "name",		VTypes::StringData,		0, FALSE },
	{ "nAngles",		VTypes::IntegerData,		0, TRUE },
	{ "nAtoms",		VTypes::IntegerData,		0, TRUE },
	{ "nBasisCartesians",	VTypes::IntegerData,		0, TRUE },
	{ "nBasisShells",	VTypes::IntegerData,		0, TRUE },
	{ "nBonds",		VTypes::IntegerData,		0, TRUE },
	{ "nDistances",		VTypes::IntegerData,		0, TRUE },
	{ "nEigenvalues",	VTypes::IntegerData,		0, TRUE },
	{ "nFFAngles",		VTypes::IntegerData,		0, TRUE },
	{ "nFFBonds",		VTypes::IntegerData,		0, TRUE },
	{ "nFFTorsions",	VTypes::IntegerData,		0, TRUE },
	{ "nFFTypes",		VTypes::IntegerData,		0, TRUE },
	{ "nFrames",		VTypes::IntegerData,		0, TRUE },
	{ "nGlyphs",		VTypes::IntegerData,		0, TRUE },
	{ "nGrids",		VTypes::IntegerData,		0, TRUE },
	{ "nPatterns",		VTypes::IntegerData,		0, TRUE },
	{ "nSelected",		VTypes::IntegerData,		0, TRUE },
	{ "nTorsions",		VTypes::IntegerData,		0, TRUE },
	{ "nUnknown",		VTypes::IntegerData,		0, TRUE },
	{ "nVibrations",	VTypes::IntegerData,		0, TRUE },
	{ "patterns",		VTypes::PatternData,		-1, TRUE },
	{ "propagateStyle",	VTypes::IntegerData,		0, FALSE },
	{ "selection",		VTypes::AtomData,		-1, TRUE },
	{ "torsions",		VTypes::MeasurementData,	-1, TRUE },
	{ "vibrations",		VTypes::VibrationData,		0, TRUE },
	{ "zMatrix",		VTypes::ZMatrixData,		0, TRUE }
};

// Function data
FunctionAccessor ModelVariable::functionData[ModelVariable::nFunctions] = {
	{ "addHydrogen",	VTypes::NoData,		Command::arguments(Command::AddHydrogen),	Command::argText(Command::AddHydrogen) },
	{ "angleEnergy",	VTypes::DoubleData,	"",						"" },
	{ "augment",		VTypes::NoData,		Command::arguments(Command::Augment),		Command::argText(Command::Augment) },
	{ "bondEnergy",		VTypes::DoubleData,	"",						"" },
	{ "charge",		VTypes::NoData,		Command::arguments(Command::Charge),		Command::argText(Command::Charge) },
	{ "clearBonds",		VTypes::NoData,		Command::arguments(Command::ClearBonds),	Command::argText(Command::ClearBonds) },
	{ "clearCharges",	VTypes::NoData,		Command::arguments(Command::ClearCharges),	Command::argText(Command::ClearCharges) },
	{ "clearSelectedBonds",	VTypes::NoData,		Command::arguments(Command::ClearSelectedBonds),Command::argText(Command::ClearSelectedBonds) },
	{ "copy",		VTypes::NoData,		Command::arguments(Command::Copy),		Command::argText(Command::Copy) },
	{ "cut",		VTypes::NoData,		Command::arguments(Command::Cut),		Command::argText(Command::Cut) },
	{ "delete",		VTypes::NoData,		Command::arguments(Command::Delete),		Command::argText(Command::Delete) },
	{ "deselect",		VTypes::NoData,		Command::arguments(Command::DeSelect),		Command::argText(Command::DeSelect) },
	{ "elecEnergy",		VTypes::DoubleData,	"",						"" },
	{ "expand",		VTypes::NoData,		Command::arguments(Command::Expand),		Command::argText(Command::Expand) },
	{ "finalise",		VTypes::NoData,		Command::arguments(Command::Finalise),		Command::argText(Command::Finalise) },
	{ "interEnergy",	VTypes::DoubleData,	"",						"" },
	{ "intraEnergy",	VTypes::DoubleData,	"",						"" },
	{ "moveToEnd",		VTypes::NoData,		Command::arguments(Command::MoveToEnd),		Command::argText(Command::MoveToEnd) },
	{ "moveToStart",	VTypes::NoData,		Command::arguments(Command::MoveToStart),	Command::argText(Command::MoveToStart) },
	{ "newAtom",		VTypes::AtomData,	Command::arguments(Command::NewAtom),		Command::argText(Command::NewAtom) },
	{ "newAtomFrac",	VTypes::AtomData,	Command::arguments(Command::NewAtomFrac),	Command::argText(Command::NewAtomFrac) },
	{ "newBasisShell",	VTypes::BasisShellData,	Command::arguments(Command::NewBasisShell),	Command::argText(Command::NewBasisShell) },
	{ "newBond",		VTypes::BondData,	Command::arguments(Command::NewBond),		Command::argText(Command::NewBond) },
	{ "newBondId",		VTypes::BondData,	Command::arguments(Command::NewBondId),		Command::argText(Command::NewBondId) },
	{ "newEigenvector",	VTypes::EigenvectorData,Command::arguments(Command::NewEigenvector),	Command::argText(Command::NewEigenvector) },
	{ "newGlyph",		VTypes::GlyphData,	Command::arguments(Command::NewGlyph),		Command::argText(Command::NewGlyph) },
	{ "newGrid",		VTypes::GridData,	Command::arguments(Command::NewGrid),		Command::argText(Command::NewGrid) },
	{ "newVibration",	VTypes::VibrationData,	Command::arguments(Command::NewVibration),	Command::argText(Command::NewVibration) },
	{ "paste",		VTypes::NoData,		Command::arguments(Command::Paste),		Command::argText(Command::Paste) },
	{ "rebond",		VTypes::NoData,		Command::arguments(Command::ReBond),		Command::argText(Command::ReBond) },
	{ "rebondPatterns",	VTypes::NoData,		Command::arguments(Command::ReBondPatterns),	Command::argText(Command::ReBondPatterns) },
	{ "rebondSelection",	VTypes::NoData,		Command::arguments(Command::ReBondSelection),	Command::argText(Command::ReBondSelection) },
	{ "redo",		VTypes::NoData,		Command::arguments(Command::Redo),		Command::argText(Command::Redo) },
	{ "reorder",		VTypes::NoData,		Command::arguments(Command::ReOrder),		Command::argText(Command::ReOrder) },
	{ "saveBitmap",		VTypes::NoData,		Command::arguments(Command::SaveBitmap),	Command::argText(Command::SaveBitmap) },
	{ "select",		VTypes::NoData,		Command::arguments(Command::Select),		Command::argText(Command::Select) },
	{ "selectAll",		VTypes::NoData,		Command::arguments(Command::SelectAll),		Command::argText(Command::SelectAll) },
	{ "selectionAddHydrogen",VTypes::NoData,	Command::arguments(Command::SelectionAddHydrogen),	Command::argText(Command::SelectionAddHydrogen) },
	{ "selectNone",		VTypes::NoData,		Command::arguments(Command::SelectNone),	Command::argText(Command::SelectNone) },
	{ "selectTree",		VTypes::NoData,		Command::arguments(Command::SelectTree),	Command::argText(Command::SelectTree) },
	{ "setupComponent",	VTypes::NoData,		Command::arguments(Command::SetupComponent),	Command::argText(Command::SetupComponent) },
	{ "shiftDown",		VTypes::NoData,		Command::arguments(Command::ShiftDown),		Command::argText(Command::ShiftDown) },
	{ "shiftUp",		VTypes::NoData,		Command::arguments(Command::ShiftUp),		Command::argText(Command::ShiftUp) },
	{ "showAll",		VTypes::NoData,		Command::arguments(Command::ShowAll),		Command::argText(Command::ShowAll) },
	{ "toAngstroms",	VTypes::NoData,		"",						"" },
	{ "torsionEnergy",	VTypes::DoubleData,	"",						"" },
	{ "transmute",		VTypes::NoData,		Command::arguments(Command::Transmute),		Command::argText(Command::Transmute) },
	{ "undo",		VTypes::NoData,		Command::arguments(Command::Undo),		Command::argText(Command::Undo) },
	{ "vdwEnergy",		VTypes::DoubleData,	"",						"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *ModelVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return ModelVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *ModelVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("ModelVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	i = Variable::searchAccessor(s, nAccessors, accessorData);
	if (i == -1)
	{
		// No accessor found - is it a function definition?
		// for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		i = Variable::searchAccessor(s, nFunctions, functionData);
		if (i == -1)
		{
			msg.print("Error: Type 'Model&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("ModelVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'Model&' function '%s'.\n", s);
			msg.exit("ModelVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ModelData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'Model&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
			delete result;
			result = NULL;
		}
	}
	else
	{
		msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
		// Were we given an array index when we didn't want one?
		if ((accessorData[i].arraySize == 0) && (arrayindex != NULL))
		{
			msg.print("Error: Irrelevant array index provided for member '%s'.\n", accessorData[i].name);
			result = NULL;
		}
		// Were we given an argument list when we didn't want one?
		if (arglist != NULL)
		{
			msg.print("Error: Argument list given to 'Model&' array member '%s'.\n", s);
			msg.exit("ModelVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::ModelData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("ModelVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ModelVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ModelVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Model type.\n", i);
		msg.exit("ModelVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("ModelVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("ModelVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Model *ptr = (Model*) rv.asPointer(VTypes::ModelData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::ModelData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (ModelVariable::Angles):
			if (!hasArrayIndex) rv.set(VTypes::MeasurementData, ptr->angleMeasurements());
			else if (arrayIndex > ptr->nAngleMeasurements())
			{
				msg.print("Angle array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::MeasurementData, ptr->angleMeasurement(arrayIndex-1));
			break;
		case (ModelVariable::Atoms):
			if (!hasArrayIndex) rv.set(VTypes::AtomData, ptr->atoms());
			else if (arrayIndex > ptr->nAtoms())
			{
				msg.print("Atom array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::AtomData, ptr->atom(arrayIndex-1));
			break;
		case (ModelVariable::Bonds):
			if (!hasArrayIndex) rv.set(VTypes::BondData, ptr->bonds());
			else if (arrayIndex > ptr->nBonds())
			{
				msg.print("Bond array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::BondData, ptr->bond(arrayIndex-1));
			break;
		case (ModelVariable::Celldata):
			rv.set(VTypes::CellData, ptr->cell());
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
				msg.print("Distance array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
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
				msg.print("Forcefield angle array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
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
				msg.print("Forcefield bond array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
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
				msg.print("Forcefield torsion array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
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
				msg.print("Forcefield types array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
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
			if (!ptr->trajectoryIsCached())
			{
				msg.print("Trajectory for model '%s' is not cached - individual frame pointers not available.\n", ptr->name());
				result = FALSE;
			}
			else if (!hasArrayIndex) rv.set(VTypes::ModelData, ptr->trajectoryFrame(0));
			else if ((arrayIndex < 1) || (arrayIndex > ptr->nTrajectoryFrames()))
			{
				msg.print("Frame array index '%i' is out of bounds for model '%s' whose trajectory has %i frames.\n", arrayIndex, ptr->name(), ptr->nTrajectoryFrames());
				result = FALSE;
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
				msg.print("Glyph array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
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
				msg.print("Grid array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::GridData, ptr->grid(arrayIndex-1));
			break;
		case (ModelVariable::Id):
			rv.set(aten.modelIndex(ptr)+1);
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
				msg.print("Pattern array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::PatternData, ptr->pattern(arrayIndex-1));
			break;
		case (ModelVariable::PropagateStyle):
			rv.set(ptr->trajectoryPropagateParentStyle());
			break;
		case (ModelVariable::Selection):
			if (!hasArrayIndex)
			{
				if (ptr->selection() == NULL) rv.set(VTypes::AtomData, NULL);
				else rv.set(VTypes::AtomData, ptr->selection()->item, ptr->selection());
			}
			else if (arrayIndex > ptr->nSelected())
			{
				msg.print("Selection array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::AtomData, ptr->selected(arrayIndex-1)->item, ptr->selected(arrayIndex-1));
			break;
		case (ModelVariable::Torsions):
			if (!hasArrayIndex) rv.set(VTypes::MeasurementData, ptr->torsionMeasurements());
			else if (arrayIndex > ptr->nTorsionMeasurements())
			{
				msg.print("Torsions array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::MeasurementData, ptr->torsionMeasurement(arrayIndex-1));
			break;
		case (ModelVariable::ZMatrix):
			rv.set(VTypes::ZMatrixData, ptr->zMatrix());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ModelVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ModelVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ModelVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ModelVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Model type.\n", i);
		msg.exit("ModelVariable::setAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = TRUE;
	if (accessorData[i].arraySize != 0)
	{
		if (hasArrayIndex)
		{
			if ((accessorData[i].arraySize > 0) && ( (arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize) ))
			{
				msg.print("Error: Array index provided for member '%s' is out of range (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
				result = FALSE;
			}
			if (newvalue.arraySize() > 0)
			{
				msg.print("Error: An array can't be assigned to the single valued member '%s'.\n", accessorData[i].name);
				result = FALSE;
			}
		}
		else
		{
			if (newvalue.arraySize() > accessorData[i].arraySize)
			{
				msg.print("Error: The array being assigned to member '%s' is larger than the size of the desination array (%i cf. %i).\n", accessorData[i].name, newvalue.arraySize(), accessorData[i].arraySize);
				result = FALSE;
			}
		}
	}
	else
	{
		// This is not an array member, so cannot be assigned an array unless its a Vector
		if (newvalue.arraySize() != -1)
		{
			if (accessorData[i].returnType != VTypes::VectorData)
			{
				msg.print("Error: An array can't be assigned to the single valued member '%s'.\n", accessorData[i].name);
				result = FALSE;
			}
			else if ((newvalue.type() != VTypes::VectorData) && (newvalue.arraySize() != 3))
			{
				msg.print("Error: Only an array of size 3 can be assigned to a vector (member '%s').\n", accessorData[i].name);
				result = FALSE;
			}
		}
	}
	if (!result)
	{
		msg.exit("ModelVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	Model::InsertionPolicy inspol;
	Model *ptr = (Model*) sourcerv.asPointer(VTypes::ModelData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::ModelData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (ModelVariable::Celldata):
			ptr->setCell( ((UnitCell*) newvalue.asPointer(VTypes::CellData)) );
			break;
		case (ModelVariable::ComponentDensity):
			ptr->setComponentDensity( newvalue.asDouble() );
			break;
		case (ModelVariable::ComponentPartition):
			ptr->setComponentPartition( newvalue.asInteger()-1 );
			break;
		case (ModelVariable::ComponentPolicy):
			inspol = Model::insertionPolicy(newvalue.asString(), TRUE);
			if (inspol == Model::nInsertionPolicies) result = FALSE;
			else ptr->setComponentInsertionPolicy(inspol);
			break;
		case (ModelVariable::ComponentPopulation):
			ptr->setComponentPopulation( newvalue.asInteger() );
			break;
		case (ModelVariable::FField):
			ptr->setForcefield( (Forcefield*) newvalue.asPointer(VTypes::ForcefieldData) );
			break;
		case (ModelVariable::Name):
			ptr->setName(newvalue.asString());
			break;
		case (ModelVariable::PropagateStyle):
			ptr->setTrajectoryPropagateParentStyle( newvalue.asBool() );
			break;

		default:
			printf("ModelVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("ModelVariable::setAccessor");
	return result;
}

// Perform desired function
bool ModelVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("ModelVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Model type.\n", i);
		msg.exit("ModelVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Model *ptr = (Model*) rv.asPointer(VTypes::ModelData, result);
	// Construct temporary bundle object containing our model pointer
	Bundle bundle(ptr);
// 	ReturnValue temprv;
	if (result) switch (i)
	{
		case (ModelVariable::AddHydrogen):
			result = aten.commands.call(Command::AddHydrogen, node, rv, bundle);
			break;
		case (ModelVariable::AngleEnergy):
			rv.set( ptr->angleEnergy(ptr, result));
			break;
 		case (ModelVariable::Augment):
			result = aten.commands.call(Command::Augment, node, rv, bundle);
			break;
		case (ModelVariable::BondEnergy):
			rv.set( ptr->bondEnergy(ptr, result));
			break;
 		case (ModelVariable::Charge):
			result = aten.commands.call(Command::Charge, node, rv, bundle);
			break;
 		case (ModelVariable::ClearBonds):
			result = aten.commands.call(Command::ClearBonds, node, rv, bundle);
			break;
 		case (ModelVariable::ClearCharges):
			result = aten.commands.call(Command::ClearCharges, node, rv, bundle);
			break;
 		case (ModelVariable::ClearSelectedBonds):
			result = aten.commands.call(Command::ClearSelectedBonds, node, rv, bundle);
			break;
 		case (ModelVariable::Copy):
			result = aten.commands.call(Command::Copy, node, rv, bundle);
			break;
 		case (ModelVariable::Cut):
			result = aten.commands.call(Command::Cut, node, rv, bundle);
			break;
 		case (ModelVariable::Delete):
			result = aten.commands.call(Command::Delete, node, rv, bundle);
			break;
		case (ModelVariable::DeSelect):
			result = aten.commands.call(Command::DeSelect, node, rv, bundle);
			break;
		case (ModelVariable::ElectrostaticEnergy):
			rv.set( ptr->electrostaticEnergy(ptr, result));
			break;
 		case (ModelVariable::Expand):
			result = aten.commands.call(Command::Expand, node, rv, bundle);
			break;
 		case (ModelVariable::Finalise):
			result = aten.commands.call(Command::Finalise, node, rv, bundle);
			break;
		case (ModelVariable::InterEnergy):
			rv.set( ptr->intermolecularEnergy(ptr, result));
			break;
		case (ModelVariable::IntraEnergy):
			rv.set( ptr->intramolecularEnergy(ptr, result));
			break;
 		case (ModelVariable::MoveToEnd):
			result = aten.commands.call(Command::MoveToEnd, node, rv, bundle);
			break;
 		case (ModelVariable::MoveToStart):
			result = aten.commands.call(Command::MoveToStart, node, rv, bundle);
			break;
 		case (ModelVariable::NewAtom):
			result = aten.commands.call(Command::NewAtom, node, rv, bundle);
			break;
 		case (ModelVariable::NewAtomFrac):
			result = aten.commands.call(Command::NewAtomFrac, node, rv, bundle);
			break;
 		case (ModelVariable::NewBasisShell):
			result = aten.commands.call(Command::NewBasisShell, node, rv, bundle);
			break;
 		case (ModelVariable::NewBond):
			result = aten.commands.call(Command::NewBond, node, rv, bundle);
			break;
 		case (ModelVariable::NewBondId):
			result = aten.commands.call(Command::NewBondId, node, rv, bundle);
			break;
 		case (ModelVariable::NewEigenvector):
			result = aten.commands.call(Command::NewEigenvector, node, rv, bundle);
			break;
 		case (ModelVariable::NewGlyph):
			result = aten.commands.call(Command::NewGlyph, node, rv, bundle);
			break;
 		case (ModelVariable::NewGrid):
			result = aten.commands.call(Command::NewGrid, node, rv, bundle);
			break;
 		case (ModelVariable::NewVibration):
			result = aten.commands.call(Command::NewVibration, node, rv, bundle);
			break;
 		case (ModelVariable::Paste):
			result = aten.commands.call(Command::Paste, node, rv, bundle);
			break;
 		case (ModelVariable::ReBond):
			result = aten.commands.call(Command::ReBond, node, rv, bundle);
			break;
 		case (ModelVariable::ReBondPatterns):
			result = aten.commands.call(Command::ReBondPatterns, node, rv, bundle);
			break;
 		case (ModelVariable::ReBondSelection):
			result = aten.commands.call(Command::ReBondSelection, node, rv, bundle);
			break;
 		case (ModelVariable::Redo):
			result = aten.commands.call(Command::Redo, node, rv, bundle);
			break;
 		case (ModelVariable::ReOrder):
			result = aten.commands.call(Command::ReOrder, node, rv, bundle);
			break;
 		case (ModelVariable::SaveBitmap):
			result = aten.commands.call(Command::SaveBitmap, node, rv, bundle);
			break;
 		case (ModelVariable::Select):
			result = aten.commands.call(Command::Select, node, rv, bundle);
			break;
 		case (ModelVariable::SelectAll):
			result = aten.commands.call(Command::SelectAll, node, rv, bundle);
			break;
 		case (ModelVariable::SelectionAddHydrogen):
			result = aten.commands.call(Command::SelectionAddHydrogen, node, rv, bundle);
			break;
 		case (ModelVariable::SelectNone):
			result = aten.commands.call(Command::SelectNone, node, rv, bundle);
			break;
 		case (ModelVariable::SelectTree):
			result = aten.commands.call(Command::SelectTree, node, rv, bundle);
			break;
 		case (ModelVariable::SetupComponent):
			result = aten.commands.call(Command::SetupComponent, node, rv, bundle);
			break;
 		case (ModelVariable::ShiftDown):
			result = aten.commands.call(Command::ShiftDown, node, rv, bundle);
			break;
 		case (ModelVariable::ShiftUp):
			result = aten.commands.call(Command::ShiftUp, node, rv, bundle);
			break;
		case (ModelVariable::ShowAll):
			result = aten.commands.call(Command::ShowAll, node, rv, bundle);
			break;
 		case (ModelVariable::ToAngstroms):
			ptr->bohrToAngstrom();
			break;
 		case (ModelVariable::TorsionEnergy):
			rv.set( ptr->torsionEnergy(ptr, result));
			break;
 		case (ModelVariable::Transmute):
			result = aten.commands.call(Command::Transmute, node, rv, bundle);
			break;
 		case (ModelVariable::Undo):
			result = aten.commands.call(Command::Undo, node, rv, bundle);
			break;
		case (ModelVariable::VdwEnergy):
			rv.set( ptr->vdwEnergy(ptr, result));
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in ModelVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ModelVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void ModelVariable::printAccessors()
{
	if (ModelVariable::nAccessors > 0)
	{
		msg.print("Valid accessors are:\n");
		for (int n=0; n<ModelVariable::nAccessors; ++n) msg.print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		msg.print("\n");
	}
	if ((ModelVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		msg.print("Valid functions are:\n");
		for (int n=0; n<ModelVariable::nFunctions; ++n) msg.print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		msg.print("\n");
	}
}

/*
// Variable Array
*/

// Constructor
ModelArrayVariable::ModelArrayVariable(TreeNode *sizeexpr, bool constant)
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
StepNode *ModelArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return ModelVariable::accessorSearch(s, arrayindex, arglist);
}
