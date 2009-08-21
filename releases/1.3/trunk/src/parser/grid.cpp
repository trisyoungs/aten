/*
	*** Grid Variable and Array
	*** src/parser/grid.cpp
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

#include "parser/grid.h"
#include "parser/stepnode.h"
#include "classes/grid.h"
#include "base/constants.h"
#include "base/elements.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
// Variable
*/

// Constructor
GridVariable::GridVariable(Grid *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::GridData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
GridVariable::~GridVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor GridVariable::accessorData[GridVariable::nAccessors] = {
	{ "axes",	VTypes::CellData,	FALSE, TRUE },
	{ "name",	VTypes::StringData,	FALSE, FALSE },
	{ "nx",		VTypes::IntegerData,	FALSE, TRUE },
	{ "ny",		VTypes::IntegerData,	FALSE, TRUE },
	{ "nz",		VTypes::IntegerData,	FALSE, TRUE },
	{ "origin", 	VTypes::VectorData,	FALSE, FALSE },
	{ "visible",	VTypes::IntegerData,	FALSE, FALSE }
};

// Function data
FunctionAccessor GridVariable::functionData[GridVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *GridVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return GridVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *GridVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("GridVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		// No accessor found - is it a function definition?
		for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		if (i == nFunctions)
		{
			msg.print("Error: Type 'grid&' has no member or function named '%s'.\n", s);
			msg.exit("GridVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'grid&' function '%s'.\n", s);
			msg.exit("GridVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::GridData, functionData[i].returnType);
		result->addArgumentList(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'grid&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		else result = new StepNode(i, VTypes::GridData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("GridVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool GridVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("GridVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Grid type.\n", i);
		msg.exit("GridVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("GridVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("GridVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Grid *ptr= (Grid*) rv.asPointer(VTypes::GridData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::GridData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (GridVariable::Axes):
			rv.set(VTypes::CellData, ptr->cell());
			break;
		case (GridVariable::Name):
			rv.set(ptr->name());
			break;
		case (GridVariable::NX):
		case (GridVariable::NY):
		case (GridVariable::NZ):
			rv.set(ptr->nPoints().get(acc-GridVariable::NX));
			break;
		case (GridVariable::Origin):
			rv.set(ptr->origin());
			break;
		case (GridVariable::Visible):
			rv.set(ptr->isVisible());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in GridVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("GridVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool GridVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("GridVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Grid type.\n", i);
		msg.exit("GridVariable::setAccessor");
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
		msg.exit("GridVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	Grid *ptr= (Grid*) sourcerv.asPointer(VTypes::GridData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::GridData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (GridVariable::Name):
			ptr->setName( newvalue.asString() );
			break;
		case (GridVariable::Origin):
			ptr->setOrigin( newvalue.asVector() );
			break;
		case (GridVariable::Visible):
			ptr->setVisible( newvalue.asBool() );
			break;
		default:
			printf("GridVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("GridVariable::setAccessor");
	return result;
}

// Perform desired function
bool GridVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("GridVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Grid type.\n", i);
		msg.exit("GridVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Grid *ptr= (Grid*) rv.asPointer(VTypes::GridData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in GridVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("GridVariable::performFunction");
	return result;
}

/*
// Variable Array
*/

// Constructor
GridArrayVariable::GridArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::GridData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *GridArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return GridVariable::accessorSearch(s, arrayindex, arglist);
}

