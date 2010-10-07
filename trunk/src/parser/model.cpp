/*
	*** Model Variable and Array
	*** src/parser/model.cpp
	Copyright T. Youngs 2007-2010

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
	{ "distances",		VTypes::MeasurementData,	-1, TRUE },
	{ "eigenvectors",	VTypes::EigenvectorData,	-1, TRUE },
	{ "ffangles",		VTypes::ForcefieldBoundData,	-1, TRUE },
	{ "ffbonds",		VTypes::ForcefieldBoundData,	-1, TRUE },
	{ "ffmass",		VTypes::DoubleData,		0, TRUE },
	{ "fftorsions",		VTypes::ForcefieldBoundData,	-1, TRUE },
	{ "fftypes",		VTypes::ForcefieldAtomData,	-1, TRUE },
	{ "ff",			VTypes::ForcefieldData,		0, FALSE },
	{ "filename",		VTypes::StringData,		0, TRUE },
	{ "frame",		VTypes::ModelData,		0, TRUE },
	{ "frames",		VTypes::ModelData,		-1, TRUE },
	{ "glyphs",		VTypes::GlyphData,		-1, TRUE },
	{ "id",			VTypes::IntegerData,		0, TRUE },
	{ "mass",		VTypes::DoubleData,		0, TRUE },
	{ "name",		VTypes::StringData,		0, FALSE },
	{ "nangles",		VTypes::IntegerData,		0, TRUE },
	{ "natoms",		VTypes::IntegerData,		0, TRUE },
	{ "nbasiscartesians",	VTypes::IntegerData,		0, TRUE },
	{ "nbasisshells",	VTypes::IntegerData,		0, TRUE },
	{ "nbonds",		VTypes::IntegerData,		0, TRUE },
	{ "ndistances",		VTypes::IntegerData,		0, TRUE },
	{ "neigenvalues",	VTypes::IntegerData,		0, TRUE },
	{ "nffangles",		VTypes::IntegerData,		0, TRUE },
	{ "nffbonds",		VTypes::IntegerData,		0, TRUE },
	{ "nfftorsions",	VTypes::IntegerData,		0, TRUE },
	{ "nfftypes",		VTypes::IntegerData,		0, TRUE },
	{ "nframes",		VTypes::IntegerData,		0, TRUE },
	{ "nglyphs",		VTypes::IntegerData,		0, TRUE },
	{ "npatterns",		VTypes::IntegerData,		0, TRUE },
	{ "nrequested",		VTypes::IntegerData,		0, FALSE },
	{ "nselected",		VTypes::IntegerData,		0, TRUE },
	{ "ntorsions",		VTypes::IntegerData,		0, TRUE },
	{ "nunknown",		VTypes::IntegerData,		0, TRUE },
	{ "nvibrations",	VTypes::IntegerData,		0, TRUE },
	{ "patterns",		VTypes::PatternData,		-1, TRUE },
	{ "region",		VTypes::RegionData,		0, TRUE },
	{ "torsions",		VTypes::MeasurementData,	-1, TRUE },
	{ "vibrations",		VTypes::VibrationData,		0, TRUE },
	{ "zmatrix",		VTypes::ZMatrixData,		0, TRUE }
};

// Function data
FunctionAccessor ModelVariable::functionData[ModelVariable::nFunctions] = {
	{ "addhydrogen",	VTypes::NoData,		Command::arguments(Command::AddHydrogen),	Command::argText(Command::AddHydrogen) },
	{ "augment",		VTypes::NoData,		Command::arguments(Command::Augment),		Command::argText(Command::Augment) },
	{ "charge",		VTypes::NoData,		Command::arguments(Command::Charge),		Command::argText(Command::Charge) },
	{ "clearbonds",		VTypes::NoData,		Command::arguments(Command::ClearBonds),	Command::argText(Command::ClearBonds) },
	{ "clearcharges",	VTypes::NoData,		Command::arguments(Command::ClearCharges),	Command::argText(Command::ClearCharges) },
	{ "clearselectedbonds",	VTypes::NoData,		Command::arguments(Command::ClearSelectedBonds),Command::argText(Command::ClearSelectedBonds) },
	{ "copy",		VTypes::NoData,		Command::arguments(Command::Copy),		Command::argText(Command::Copy) },
	{ "cut",		VTypes::NoData,		Command::arguments(Command::Cut),		Command::argText(Command::Cut) },
	{ "delete",		VTypes::NoData,		Command::arguments(Command::Delete),		Command::argText(Command::Delete) },
	{ "finalise",		VTypes::NoData,		Command::arguments(Command::Finalise),		Command::argText(Command::Finalise) },
	{ "movetoend",		VTypes::NoData,		Command::arguments(Command::MoveToEnd),		Command::argText(Command::MoveToEnd) },
	{ "movetostart",	VTypes::NoData,		Command::arguments(Command::MoveToStart),	Command::argText(Command::MoveToStart) },
	{ "newatom",		VTypes::AtomData,	Command::arguments(Command::NewAtom),		Command::argText(Command::NewAtom) },
	{ "newatomfrac",	VTypes::AtomData,	Command::arguments(Command::NewAtomFrac),	Command::argText(Command::NewAtomFrac) },
	{ "newbasisshell",	VTypes::BasisShellData,	Command::arguments(Command::NewBasisShell),	Command::argText(Command::NewBasisShell) },
	{ "newbond",		VTypes::BondData,	Command::arguments(Command::NewBond),		Command::argText(Command::NewBond) },
	{ "newbondid",		VTypes::BondData,	Command::arguments(Command::NewBondId),		Command::argText(Command::NewBondId) },
	{ "neweigenvector",	VTypes::EigenvectorData,Command::arguments(Command::NewEigenvector),	Command::argText(Command::NewEigenvector) },
	{ "newglyph",		VTypes::GlyphData,	Command::arguments(Command::NewGlyph),		Command::argText(Command::NewGlyph) },
	{ "newgrid",		VTypes::GridData,	Command::arguments(Command::NewGrid),		Command::argText(Command::NewGrid) },
	{ "newvibration",	VTypes::VibrationData,	Command::arguments(Command::NewVibration),	Command::argText(Command::NewVibration) },
	{ "paste",		VTypes::NoData,		Command::arguments(Command::Paste),		Command::argText(Command::Paste) },
	{ "rebond",		VTypes::NoData,		Command::arguments(Command::ReBond),		Command::argText(Command::ReBond) },
	{ "rebondpatterns",	VTypes::NoData,		Command::arguments(Command::ReBondPatterns),	Command::argText(Command::ReBondPatterns) },
	{ "rebondselection",	VTypes::NoData,		Command::arguments(Command::ReBondSelection),	Command::argText(Command::ReBondSelection) },
	{ "redo",		VTypes::NoData,		Command::arguments(Command::Redo),		Command::argText(Command::Redo) },
	{ "reorder",		VTypes::NoData,		Command::arguments(Command::ReOrder),		Command::argText(Command::ReOrder) },
	{ "savebitmap",		VTypes::NoData,		Command::arguments(Command::SaveBitmap),	Command::argText(Command::SaveBitmap) },
	{ "selectionaddhydrogen",VTypes::NoData,	Command::arguments(Command::SelectionAddHydrogen),	Command::argText(Command::SelectionAddHydrogen) },
	{ "shiftdown",		VTypes::NoData,		Command::arguments(Command::ShiftDown),		Command::argText(Command::ShiftDown) },
	{ "shiftup",		VTypes::NoData,		Command::arguments(Command::ShiftUp),		Command::argText(Command::ShiftUp) },
	{ "showall",		VTypes::NoData,		Command::arguments(Command::ShowAll),		Command::argText(Command::ShowAll) },
	{ "toangstroms",	VTypes::NoData,		"",						"Convert cell and atom coordinates from Bohr to Angstroms" },
	{ "transmute",		VTypes::NoData,		Command::arguments(Command::Transmute),		Command::argText(Command::Transmute) },
	{ "undo",		VTypes::NoData,		Command::arguments(Command::Undo),		Command::argText(Command::Undo) }
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
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		// No accessor found - is it a function definition?
		for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		if (i == nFunctions)
		{
			msg.print("Error: Type 'model&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("ModelVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'model&' function '%s'.\n", s);
			msg.exit("ModelVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ModelData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'model&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		else result = new StepNode(i, VTypes::ModelData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
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
	Model *ptr= (Model*) rv.asPointer(VTypes::ModelData, result);
	if (result && (ptr == NULL))
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
		case (ModelVariable::Distances):
			if (!hasArrayIndex) rv.set(VTypes::MeasurementData, ptr->distanceMeasurements());
			else if (arrayIndex > ptr->nDistanceMeasurements())
			{
				msg.print("Distance array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::MeasurementData, ptr->distanceMeasurement(arrayIndex-1));
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
		case (ModelVariable::NPatterns):
			rv.set(ptr->nPatterns());
			break;
		case (ModelVariable::NRequested):
			rv.set(ptr->nRequested());
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
		case (ModelVariable::Patterns):
			if (!hasArrayIndex) rv.set(VTypes::PatternData, ptr->patterns());
			else if (arrayIndex > ptr->nPatterns())
			{
				msg.print("Pattern array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::PatternData, ptr->pattern(arrayIndex-1));
			break;
		case (ModelVariable::Region):
			rv.set(VTypes::RegionData, ptr->region());
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
	Model *ptr= (Model*) sourcerv.asPointer(VTypes::ModelData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::ModelData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (ModelVariable::Celldata):
			ptr->setCell( ((Cell*) newvalue.asPointer(VTypes::CellData)) );
			break;
		case (ModelVariable::FField):
			ptr->setForcefield( (Forcefield*) newvalue.asPointer(VTypes::ForcefieldData) );
			break;
		case (ModelVariable::Name):
			ptr->setName(newvalue.asString());
			break;
		case (ModelVariable::NRequested):
			ptr->setNRequested(newvalue.asInteger());
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
	Model *ptr= (Model*) rv.asPointer(VTypes::ModelData, result);
	// Construct temporary bundle object containing our model pointer
	Bundle bundle(ptr);
// 	ReturnValue temprv;
	if (result) switch (i)
	{
		case (ModelVariable::AddHydrogen):
			result = aten.commands.call(Command::AddHydrogen, node, rv, bundle);
			break;
 		case (ModelVariable::Augment):
			result = aten.commands.call(Command::Augment, node, rv, bundle);
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
 		case (ModelVariable::Finalise):
			result = aten.commands.call(Command::Finalise, node, rv, bundle);
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
 		case (ModelVariable::SelectionAddHydrogen):
			result = aten.commands.call(Command::SelectionAddHydrogen, node, rv, bundle);
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
 		case (ModelVariable::Transmute):
			result = aten.commands.call(Command::Transmute, node, rv, bundle);
			break;
 		case (ModelVariable::Undo):
			result = aten.commands.call(Command::Undo, node, rv, bundle);
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
