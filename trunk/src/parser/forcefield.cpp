/*
	*** Forcefield Variable and Array
	*** src/parser/forcefield.cpp
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

#include "parser/forcefield.h"
#include "parser/stepnode.h"
#include "base/atom.h"
#include "base/constants.h"
#include "base/elements.h"
#include "ff/forcefield.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
// Variable
*/

// Constructor
ForcefieldVariable::ForcefieldVariable(Forcefield *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ForcefieldData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
ForcefieldVariable::~ForcefieldVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor ForcefieldVariable::accessorData[ForcefieldVariable::nAccessors] = {
	{ "atomtypes",		VTypes::ForcefieldAtomData,	-1, TRUE },
	{ "filename",		VTypes::StringData,		0, TRUE },
	{ "name",		VTypes::StringData,		0, FALSE }
};

// Search variable access list for provided accessor (call private static function)
StepNode *ForcefieldVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return ForcefieldVariable::accessorSearch(s, arrayindex);
}

// Private static function to search accessors
StepNode *ForcefieldVariable::accessorSearch(const char *s, TreeNode *arrayindex)
{
	msg.enter("ForcefieldVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'forcefield&' has no member named '%s'.\n", s);
		msg.exit("ForcefieldVariable::accessorSearch");
		return NULL;
	}
	msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
	// Were we given an array index when we didn't want one?
	if ((accessorData[i].arraySize == 0) && (arrayindex != NULL))
	{
		msg.print("Error: Irrelevant array index provided for member '%s'.\n", accessorData[i].name);
		result = NULL;
	}
	else result = new StepNode(i, VTypes::ForcefieldData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	msg.exit("ForcefieldVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ForcefieldVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ForcefieldVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Forcefield type.\n", i);
		msg.exit("ForcefieldVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("ForcefieldVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("ElementVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Forcefield *ptr= (Forcefield*) rv.asPointer(VTypes::ForcefieldData, result);
	if (result) switch (acc)
	{
		case (ForcefieldVariable::AtomTypes):
			rv.set(VTypes::ForcefieldAtomData, ptr->types());
			break;
		case (FileName):
			rv.set( ptr->filename() );
			break;
		case (Name):
			rv.set( ptr->name() );
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ForcefieldVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ForcefieldVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ForcefieldVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ForcefieldVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Forcefield type.\n", i);
		msg.exit("ForcefieldVariable::setAccessor");
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
		msg.exit("ForcefieldVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	Forcefield *ptr= (Forcefield*) sourcerv.asPointer(VTypes::ForcefieldData, result);
	if (result) switch (acc)
	{
		case (Name):
			ptr->setName( newvalue.asString() );
			break;
		default:
			printf("ForcefieldVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("ForcefieldVariable::setAccessor");
	return result;
}

/*
// Variable Array
*/

// Constructor
ForcefieldArrayVariable::ForcefieldArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ForcefieldData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *ForcefieldArrayVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return ForcefieldVariable::accessorSearch(s, arrayindex);
}
