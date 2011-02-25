/*
	*** BasisPrimitive Variable and Array
	*** src/parser/basisprimitive.cpp
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

#include "parser/basisprimitive.h"
#include "parser/stepnode.h"
#include "classes/basisshell.h"
#include "base/constants.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
// Variable
*/

// Constructor
BasisPrimitiveVariable::BasisPrimitiveVariable(BasisPrimitive *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::BasisPrimitiveData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
BasisPrimitiveVariable::~BasisPrimitiveVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor BasisPrimitiveVariable::accessorData[BasisPrimitiveVariable::nAccessors] = {
	{ "exponent",		VTypes::DoubleData,	0, FALSE },
	{ "coefficients",	VTypes::DoubleData,	-1, TRUE }
};

// Function data
FunctionAccessor BasisPrimitiveVariable::functionData[BasisPrimitiveVariable::nFunctions] = {
	{ "addcoefficient",	VTypes::NoData,		"N",	"double coeff" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *BasisPrimitiveVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return BasisPrimitiveVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *BasisPrimitiveVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("BasisPrimitiveVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		// No accessor found - is it a function definition?
		for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		if (i == nFunctions)
		{
			msg.print("Error: Type 'basisprimitive&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("BasisPrimitiveVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'basisprimitive&' function '%s'.\n", s);
			msg.exit("BasisPrimitiveVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::BasisPrimitiveData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'basisprimitive&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		else result = new StepNode(i, VTypes::BasisPrimitiveData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("BasisPrimitiveVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool BasisPrimitiveVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("BasisPrimitiveVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for BasisPrimitive type.\n", i);
		msg.exit("BasisPrimitiveVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("BasisPrimitiveVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("BasisPrimitiveVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	BasisPrimitive *ptr= (BasisPrimitive*) rv.asPointer(VTypes::BasisPrimitiveData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::BasisPrimitiveData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (BasisPrimitiveVariable::Exponent):
			rv.set(ptr->exponent());
			break;
		case (BasisPrimitiveVariable::Coefficients):
			if ((arrayIndex < 1) || (arrayIndex > ptr->nCoefficients()))
			{
				msg.print("Array index [%i] is out of range for 'coefficients' member.\n", arrayIndex);
				result = FALSE;
			}
			else rv.set(ptr->coefficient(arrayIndex-1));
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in BasisPrimitiveVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("BasisPrimitiveVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool BasisPrimitiveVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("BasisPrimitiveVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for BasisPrimitive type.\n", i);
		msg.exit("BasisPrimitiveVariable::setAccessor");
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
		msg.exit("BasisPrimitiveVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	BasisPrimitive *ptr= (BasisPrimitive*) sourcerv.asPointer(VTypes::BasisPrimitiveData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::BasisPrimitiveData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (BasisPrimitiveVariable::Exponent):
			ptr->setExponent( newvalue.asDouble() );
			break;
		default:
			printf("BasisPrimitiveVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("BasisPrimitiveVariable::setAccessor");
	return result;
}

// Perform desired function
bool BasisPrimitiveVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("BasisPrimitiveVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for BasisPrimitive type.\n", i);
		msg.exit("BasisPrimitiveVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	BasisPrimitive *ptr= (BasisPrimitive*) rv.asPointer(VTypes::BasisPrimitiveData, result);
	if (result) switch (i)
	{
		case (BasisPrimitiveVariable::AddCoefficient):
			ptr->addCoefficient( node->argd(0) );
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in BasisPrimitiveVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("BasisPrimitiveVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void BasisPrimitiveVariable::printAccessors()
{
	if (BasisPrimitiveVariable::nAccessors > 0)
	{
		msg.print("Valid accessors are:\n");
		for (int n=0; n<BasisPrimitiveVariable::nAccessors; ++n) msg.print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		msg.print("\n");
	}
	if ((BasisPrimitiveVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		msg.print("Valid functions are:\n");
		for (int n=0; n<BasisPrimitiveVariable::nFunctions; ++n) msg.print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		msg.print("\n");
	}
}

/*
// Variable Array
*/

// Constructor
BasisPrimitiveArrayVariable::BasisPrimitiveArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::BasisPrimitiveData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *BasisPrimitiveArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return BasisPrimitiveVariable::accessorSearch(s, arrayindex, arglist);
}

