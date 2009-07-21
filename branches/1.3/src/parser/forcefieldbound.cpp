/*
	*** ForcefieldBound Variable and Array
	*** src/parser/forcefieldbound.cpp
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

#include "parser/forcefieldbound.h"
#include "parser/stepnode.h"
#include "base/atom.h"
#include "base/constants.h"
#include "base/elements.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
// Variable
*/

// Constructor
ForcefieldBoundVariable::ForcefieldBoundVariable(ForcefieldBound *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ForcefieldBoundData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
ForcefieldBoundVariable::~ForcefieldBoundVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor ForcefieldBoundVariable::accessorData[ForcefieldBoundVariable::nAccessors] = {
	{ "data",	VTypes::DoubleData,	MAXFFPARAMDATA, FALSE },
        { "form",	VTypes::StringData,	0, FALSE },
        { "natoms",	VTypes::IntegerData,	0, TRUE },
        { "type",	VTypes::StringData,	0, FALSE },
        { "typenames",	VTypes::StringData,	MAXFFPARAMDATA, FALSE }
};

// Function data
FunctionAccessor ForcefieldBoundVariable::functionData[ForcefieldBoundVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *ForcefieldBoundVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return ForcefieldBoundVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *ForcefieldBoundVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("ForcefieldBoundVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		// No accessor found - is it a function definition?
		for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		if (i == nFunctions)
		{
			msg.print("Error: Type 'ffbound&' has no member or function named '%s'.\n", s);
			msg.exit("ForcefieldBoundVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'ffbound&' function '%s'.\n", s);
			msg.exit("ForcefieldBoundVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ForcefieldBoundData, functionData[i].returnType);
		result->addArgumentList(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'ffbound&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		else result = new StepNode(i, VTypes::ForcefieldBoundData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("ForcefieldBoundVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ForcefieldBoundVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ForcefieldBoundVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ForcefieldBound type.\n", i);
		msg.exit("ForcefieldBoundVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("ForcefieldBoundVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("ForcefieldBoundVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	ForcefieldBound *ptr= (ForcefieldBound*) rv.asPointer(VTypes::ForcefieldBoundData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::ForcefieldBoundData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ForcefieldBoundVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ForcefieldBoundVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ForcefieldBoundVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ForcefieldBoundVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ForcefieldBound type.\n", i);
		msg.exit("ForcefieldBoundVariable::setAccessor");
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
		msg.exit("ForcefieldBoundVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	ForcefieldBound *ptr= (ForcefieldBound*) sourcerv.asPointer(VTypes::ForcefieldBoundData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::ForcefieldBoundData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		default:
			printf("ForcefieldBoundVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("ForcefieldBoundVariable::setAccessor");
	return result;
}

// Perform desired function
bool ForcefieldBoundVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("ForcefieldBoundVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for ForcefieldBound type.\n", i);
		msg.exit("ForcefieldBoundVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	ForcefieldBound *ptr= (ForcefieldBound*) rv.asPointer(VTypes::ForcefieldBoundData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in ForcefieldBoundVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ForcefieldBoundVariable::performFunction");
	return result;
}

/*
// Variable Array
*/

// Constructor
ForcefieldBoundArrayVariable::ForcefieldBoundArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ForcefieldBoundData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *ForcefieldBoundArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return ForcefieldBoundVariable::accessorSearch(s, arrayindex, arglist);
}
