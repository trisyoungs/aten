/*
	*** Pattern Variable
	*** src/parser/pattern.cpp
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

#include "parser/pattern.h"
#include "parser/stepnode.h"
#include "base/atom.h"
#include "base/pattern.h"
#include "base/elements.h"
#include "model/model.h"
#include <string.h>

// Constructor
PatternVariable::PatternVariable(Pattern *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::PatternData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
PatternVariable::~PatternVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor PatternVariable::accessorData[PatternVariable::nAccessors] = {
	{ "angles", 	VTypes::PatternBoundData,	-1, TRUE },
	{ "atoms", 	VTypes::ForcefieldAtomData,	-1, TRUE },
	{ "bonds", 	VTypes::PatternBoundData,	-1, TRUE },
	{ "cog", 	VTypes::VectorData,		-1, TRUE },
	{ "com", 	VTypes::VectorData,		-1, TRUE },
	{ "firstatom",	VTypes::AtomData,		0, TRUE },
	{ "firstatomid",VTypes::IntegerData,		0, TRUE },
	{ "forcefield",	VTypes::ForcefieldData,		0, FALSE },
	{ "lastatom",	VTypes::AtomData,		0, TRUE },
	{ "lastatomid",	VTypes::IntegerData,		0, TRUE },
	{ "name",	VTypes::StringData,		0, FALSE },
	{ "nangles",	VTypes::IntegerData,		0, TRUE },
	{ "natoms",	VTypes::IntegerData,		0, TRUE },
	{ "nbonds",	VTypes::IntegerData,		0, TRUE },
	{ "nmolatoms",	VTypes::IntegerData,		0, TRUE },
	{ "nmols",	VTypes::IntegerData,		0, TRUE },
	{ "ntorsions",	VTypes::IntegerData,		0, TRUE },
	{ "torsions",	VTypes::PatternBoundData,	-1, TRUE }
};

// Function data
FunctionAccessor PatternVariable::functionData[PatternVariable::nFunctions] = {
};

// Search variable access list for provided accessor (call private static function)
StepNode *PatternVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return PatternVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *PatternVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("PatternVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		// No accessor found - is it a function definition?
		for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		if (i == nFunctions)
		{
			msg.print("Error: Type 'pattern&' has no member or function named '%s'.\n", s);
			msg.exit("PatternVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'pattern&' function '%s'.\n", s);
			msg.exit("PatternVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::PatternData, functionData[i].returnType);
		result->addArgumentList(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'pattern&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		else result = new StepNode(i, VTypes::PatternData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("PatternVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool PatternVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("PatternVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Pattern type.\n", i);
		msg.exit("PatternVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("PatternVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("PatternVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Pattern *ptr= (Pattern*) rv.asPointer(VTypes::PatternData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::PatternData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (PatternVariable::Angles):
			if (!hasArrayIndex) rv.set(VTypes::PatternBoundData, ptr->angles());
			else if (arrayIndex > ptr->nAngles())
			{
				msg.print("Angle array index (%i) is out of bounds for pattern '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::PatternBoundData, ptr->angle(arrayIndex-1));
			break;
		case (PatternVariable::Atoms):
			if (!hasArrayIndex) rv.set(VTypes::AtomData, ptr->parent()->atom(ptr->startAtom()));
			else if (arrayIndex > ptr->totalAtoms())
			{
				msg.print("Atom array index (%i) is out of bounds for pattern '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::AtomData, ptr->parent()->atom(arrayIndex-1+ptr->startAtom()));
			break;
		case (PatternVariable::Bonds):
			if (!hasArrayIndex) rv.set(VTypes::PatternBoundData, ptr->bonds());
			else if (arrayIndex > ptr->nBonds())
			{
				msg.print("Bond array index (%i) is out of bounds for pattern '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::PatternBoundData, ptr->bond(arrayIndex-1));
			break;
		case (PatternVariable::Cog):
			rv.set(ptr->calculateCog(arrayIndex-1));
			break;
		case (PatternVariable::Com):
			rv.set(ptr->calculateCom(arrayIndex-1));
			break;
		case (PatternVariable::FirstAtom):
			rv.set(VTypes::AtomData, ptr->firstAtom());
			break;
		case (PatternVariable::FirstAtomId):
			rv.set(ptr->startAtom() + 1);
			break;
		case (PatternVariable::FField):
			rv.set(VTypes::ForcefieldData, ptr->forcefield());
			break;
		case (PatternVariable::LastAtom):
			rv.set(VTypes::AtomData, ptr->lastAtom());
			break;
		case (PatternVariable::LastAtomId):
			rv.set(ptr->endAtom() + 1);
			break;
		case (PatternVariable::Name):
			rv.set(ptr->name());
			break;
		case (PatternVariable::NAngles):
			rv.set(ptr->nAngles());
			break;
		case (PatternVariable::NAtoms):
			rv.set(ptr->totalAtoms());
			break;
		case (PatternVariable::NBonds):
			rv.set(ptr->nBonds());
			break;
		case (PatternVariable::NMolAtoms):
			rv.set(ptr->nAtoms());
			break;
		case (PatternVariable::NMols):
			rv.set(ptr->nMolecules());
			break;
		case (PatternVariable::NTorsions):
			rv.set(ptr->nTorsions());
			break;
		case (PatternVariable::Torsions):
			if (!hasArrayIndex) rv.set(VTypes::PatternBoundData, ptr->torsions());
			else if (arrayIndex > ptr->nTorsions())
			{
				msg.print("Torsion array index (%i) is out of bounds for pattern '%s'\n", arrayIndex, ptr->name());
				result = FALSE;
			}
			else rv.set(VTypes::PatternBoundData, ptr->torsion(arrayIndex-1));
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in PatternVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("PatternVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool PatternVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("PatternVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Pattern type.\n", i);
		msg.exit("PatternVariable::setAccessor");
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
		msg.exit("PatternVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	Pattern *ptr= (Pattern*) sourcerv.asPointer(VTypes::PatternData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::PatternData));
		result = FALSE;
	}
	// Set value based on enumerated id
	if (result) switch (acc)
	{
		case (PatternVariable::Name):
			ptr->setName(newvalue.asString());
			break;
		case (PatternVariable::FField):
 			ptr->setForcefield( (Forcefield*) newvalue.asPointer(VTypes::ForcefieldData));
			break;
		default:
			printf("PatternVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("PatternVariable::setAccessor");
	return result;
}

// Perform desired function
bool PatternVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("PatternVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Pattern type.\n", i);
		msg.exit("PatternVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Pattern *ptr= (Pattern*) rv.asPointer(VTypes::PatternData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in PatternVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("PatternVariable::performFunction");
	return result;
}

/*
// Variable Array
*/

// Constructor
PatternArrayVariable::PatternArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::PatternData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *PatternArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return PatternVariable::accessorSearch(s, arrayindex, arglist);
}
