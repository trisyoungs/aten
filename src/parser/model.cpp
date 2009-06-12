/*
	*** Model Variable and Array
	*** src/parser/model.cpp
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

#include "parser/model.h"
#include "parser/stepnode.h"
#include "model/model.h"
#include "base/constants.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
	{ "angleterms",		VTypes::ForcefieldBoundData,	-1, TRUE },
	{ "atoms",		VTypes::AtomData,		-1, TRUE },
	{ "atomtypes",		VTypes::ForcefieldAtomData,	-1, TRUE },
	{ "bonds",		VTypes::BondData,		-1, TRUE },
	{ "bondterms",		VTypes::ForcefieldBoundData,	-1, TRUE },
	{ "cell",		VTypes::CellData,		0, FALSE },
	{ "distances",		VTypes::MeasurementData,	-1, TRUE },
	{ "frame",		VTypes::ModelData,		0, TRUE },
	{ "frames",		VTypes::ModelData,		-1, TRUE },
	{ "name",		VTypes::StringData,		0, FALSE },
	{ "nangles",		VTypes::IntegerData,		0, TRUE },
	{ "nangleterms",	VTypes::IntegerData,		0, TRUE },
	{ "natoms",		VTypes::IntegerData,		0, TRUE },
	{ "natomtypes",		VTypes::IntegerData,		0, TRUE },
	{ "nbonds",		VTypes::IntegerData,		0, TRUE },
	{ "nbondterms",		VTypes::IntegerData,		0, TRUE },
	{ "ndistances",		VTypes::IntegerData,		0, TRUE },
	{ "nframes",		VTypes::IntegerData,		0, TRUE },
	{ "npatterns",		VTypes::IntegerData,		0, TRUE },
	{ "nselected",		VTypes::IntegerData,		0, TRUE },
	{ "ntorsions",		VTypes::IntegerData,		0, TRUE },
	{ "ntorsionterms",	VTypes::IntegerData,		0, TRUE },
	{ "patterns",		VTypes::PatternData,		-1, TRUE },
	{ "torsions",		VTypes::MeasurementData,	-1, TRUE },
	{ "torsionterms",	VTypes::ForcefieldBoundData,	-1, TRUE }
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
		result->addArgumentList(arglist);
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
	if (result) switch (acc)
	{
		case (ModelVariable::Angles):
			if (!hasArrayIndex) rv.set(VTypes::MeasurementData, ptr->angles());
			else if (arrayIndex > ptr->nAngles())
			{
				msg.print("Angle array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::MeasurementData, ptr->angle(arrayIndex-1));
			break;
		case (ModelVariable::AngleTerms):
			if (!hasArrayIndex) rv.set(VTypes::ForcefieldBoundData, ptr->uniqueAngleTerms());
			else if (arrayIndex > ptr->nUniqueAngleTerms())
			{
				msg.print("Angle term array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::ForcefieldBoundData, ptr->uniqueAngleTerm(arrayIndex-1));
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
		case (ModelVariable::Atomtypes):
			if (!hasArrayIndex) rv.set(VTypes::ForcefieldAtomData, ptr->uniqueTypes());
			else if (arrayIndex > ptr->nUniqueTypes())
			{
				msg.print("Unique types array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::ForcefieldAtomData, ptr->uniqueType(arrayIndex-1));
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
		case (ModelVariable::BondTerms):
			if (!hasArrayIndex) rv.set(VTypes::ForcefieldBoundData, ptr->uniqueBondTerms());
			else if (arrayIndex > ptr->nUniqueBondTerms())
			{
				msg.print("Bond term array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::ForcefieldBoundData, ptr->uniqueBondTerm(arrayIndex-1));
			break;
		case (ModelVariable::Celldata):
			rv.set(VTypes::CellData, ptr->cell());
			break;
		case (ModelVariable::Distances):
			if (!hasArrayIndex) rv.set(VTypes::MeasurementData, ptr->distances());
			else if (arrayIndex > ptr->nDistances())
			{
				msg.print("Distance array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::MeasurementData, ptr->distance(arrayIndex-1));
			break;
		case (ModelVariable::Frame):
			rv.set(VTypes::ModelData, ptr->currentFrame());
			break;
		case (ModelVariable::Frames):
			// Only works for a cached trajectory
			if (!ptr->trajectoryIsCached())
			{
				msg.print("Trajectory for model '%s' is not cached - individual frame pointers not available.\n", ptr->name());
				result = FALSE;
			}
			else if (!hasArrayIndex) rv.set(VTypes::ModelData, ptr->frame(0));
			else if ((arrayIndex < 1) || (arrayIndex > ptr->nFrames()))
			{
				msg.print("Frame array index '%i' is out of bounds for model '%s' whose trajectory has %i frames.\n", arrayIndex, ptr->name(), ptr->nFrames());
				result = FALSE;
			}
			else rv.set(VTypes::ModelData, ptr->frame(arrayIndex-1));
			break;
		case (ModelVariable::Name):
			rv.set(ptr->name());
			break;
 		case (ModelVariable::NAngles):
			rv.set(ptr->nAngles());
			break;
		case (ModelVariable::NAngleTerms):
			rv.set(ptr->nUniqueAngleTerms());
			break;
		case (ModelVariable::NAtoms):
			rv.set(ptr->nAtoms());
			break;
		case (ModelVariable::NAtomtypes):
			rv.set(ptr->nUniqueTypes());
			break;
		case (ModelVariable::NBonds):
			rv.set(ptr->nBonds());
			break;
		case (ModelVariable::NBondTerms):
			rv.set(ptr->nUniqueBondTerms());
			break;
 		case (ModelVariable::NDistances):
			rv.set(ptr->nDistances());
			break;
		case (ModelVariable::NFrames):
			rv.set(ptr->nFrames());
			break;
		case (ModelVariable::NPatterns):
			rv.set(ptr->nPatterns());
			break;
		case (ModelVariable::NSelected):
			rv.set(ptr->nSelected());
			break;
 		case (ModelVariable::NTorsions):
			rv.set(ptr->nTorsions());
			break;
		case (ModelVariable::NTorsionTerms):
			rv.set(ptr->nUniqueTorsionTerms());
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
		case (ModelVariable::Torsions):
			if (!hasArrayIndex) rv.set(VTypes::MeasurementData, ptr->torsions());
			else if (arrayIndex > ptr->nTorsions())
			{
				msg.print("Torsions array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::MeasurementData, ptr->torsion(arrayIndex-1));
			break;
		case (ModelVariable::TorsionTerms):
			if (!hasArrayIndex) rv.set(VTypes::ForcefieldBoundData, ptr->uniqueTorsionTerms());
			else if (arrayIndex > ptr->nUniqueTorsionTerms())
			{
				msg.print("Torsion term array index (%i) is out of bounds for model '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::ForcefieldBoundData, ptr->uniqueTorsionTerm(arrayIndex-1));
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
			if ((newvalue.arraySize() > 0) && (newvalue.arraySize() != accessorData[i].arraySize))
			{
				msg.print("Error: The array being assigned to member '%s' is not of the same size (%i cf. %i).\n", accessorData[i].name, newvalue.arraySize(), accessorData[i].arraySize);
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
	if (result) switch (acc)
	{
		case (ModelVariable::Celldata):
			ptr->setCell( ((Cell*) newvalue.asPointer(VTypes::CellData)) );
			break;
		case (ModelVariable::Name):
			ptr->setName(newvalue.asString());
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
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in ModelVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ModelVariable::performFunction");
	return result;
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
