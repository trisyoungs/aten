/*
	*** BasisPrimitive Variable and Array
	*** src/parser/basisprimitive.cpp
	Copyright T. Youngs 2007-2015

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
#include "base/basisshell.h"
#include "math/constants.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ATEN_USING_NAMESPACE

/*
// Variable
*/

// Constructor
BasisPrimitiveVariable::BasisPrimitiveVariable(BasisPrimitive* ptr, bool constant)
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
	{ "coefficients",	VTypes::DoubleData,	-1, true },
	{ "exponent",		VTypes::DoubleData,	0, false }
};

// Function data
FunctionAccessor BasisPrimitiveVariable::functionData[BasisPrimitiveVariable::nFunctions] = {
	{ "addCoefficient",	VTypes::NoData,		"N",	"double coeff" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* BasisPrimitiveVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return BasisPrimitiveVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* BasisPrimitiveVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("BasisPrimitiveVariable::accessorSearch");
	StepNode* result = NULL;
	int i = 0;
	i = Variable::searchAccessor(name, nAccessors, accessorData);
	if (i == -1)
	{
		// No accessor found - is it a function definition?
		// for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		i = Variable::searchAccessor(name, nFunctions, functionData);
		if (i == -1)
		{
			Messenger::print("Error: Type 'BasisPrimitive&' has no member or function named '%s'.", qPrintable(name));
			printAccessors();
			Messenger::exit("BasisPrimitiveVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'BasisPrimitive&' function '%s'.", qPrintable(name));
			Messenger::exit("BasisPrimitiveVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::BasisPrimitiveData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'BasisPrimitive&' function '%s' is '%s(%s)'.", functionData[i].name, functionData[i].name, functionData[i].argText );
			delete result;
			result = NULL;
		}
	}
	else
	{
		Messenger::print(Messenger::Parse, "Accessor match = %i (%s)", i, accessorData[i].name);
		// Were we given an array index when we didn't want one?
		if ((accessorData[i].arraySize == 0) && (arrayIndex != NULL))
		{
			Messenger::print("Error: Irrelevant array index provided for member '%s'.", accessorData[i].name);
			result = NULL;
		}
		// Were we given an argument list when we didn't want one?
		if (argList != NULL)
		{
			Messenger::print("Error: Argument list given to 'BasisPrimitive&' array member '%s'.", qPrintable(name));
			Messenger::exit("BasisPrimitiveVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::BasisPrimitiveData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("BasisPrimitiveVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool BasisPrimitiveVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("BasisPrimitiveVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for BasisPrimitive type.\n", i);
		Messenger::exit("BasisPrimitiveVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", accessorData[i].name);
		Messenger::exit("BasisPrimitiveVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("BasisPrimitiveVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	BasisPrimitive* ptr = (BasisPrimitive*) rv.asPointer(VTypes::BasisPrimitiveData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::BasisPrimitiveData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (BasisPrimitiveVariable::Exponent):
			rv.set(ptr->exponent());
			break;
		case (BasisPrimitiveVariable::Coefficients):
			if ((arrayIndex < 1) || (arrayIndex > ptr->nCoefficients()))
			{
				Messenger::print("Array index [%i] is out of range for 'coefficients' member.", arrayIndex);
				result = false;
			}
			else rv.set(ptr->coefficient(arrayIndex-1));
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in BasisPrimitiveVariable.\n", accessorData[i].name);
			result = false;
			break;
	}
	Messenger::exit("BasisPrimitiveVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool BasisPrimitiveVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("BasisPrimitiveVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for BasisPrimitive type.\n", i);
		Messenger::exit("BasisPrimitiveVariable::setAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = true;
	if (accessorData[i].arraySize != 0)
	{
		if (hasArrayIndex)
		{
			if ((accessorData[i].arraySize > 0) && ( (arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize) ))
			{
				Messenger::print("Error: Array index provided for member '%s' is out of range (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
				result = false;
			}
			if (newValue.arraySize() > 0)
			{
				Messenger::print("Error: An array can't be assigned to the single valued member '%s'.", accessorData[i].name);
				result = false;
			}
		}
		else
		{
			if (newValue.arraySize() > accessorData[i].arraySize)
			{
				Messenger::print("Error: The array being assigned to member '%s' is larger than the size of the desination array (%i cf. %i).", accessorData[i].name, newValue.arraySize(), accessorData[i].arraySize);
				result = false;
			}
		}
	}
	else
	{
		// This is not an array member, so cannot be assigned an array unless its a Vector
		if (newValue.arraySize() != -1)
		{
			if (accessorData[i].returnType != VTypes::VectorData)
			{
				Messenger::print("Error: An array can't be assigned to the single valued member '%s'.", accessorData[i].name);
				result = false;
			}
			else if ((newValue.type() != VTypes::VectorData) && (newValue.arraySize() != 3))
			{
				Messenger::print("Error: Only an array of size 3 can be assigned to a vector (member '%s').", accessorData[i].name);
				result = false;
			}
		}
	}
	if (!result)
	{
		Messenger::exit("BasisPrimitiveVariable::setAccessor");
		return false;
	}
	// Get current data from ReturnValue
	BasisPrimitive* ptr = (BasisPrimitive*) sourcerv.asPointer(VTypes::BasisPrimitiveData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::BasisPrimitiveData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (BasisPrimitiveVariable::Exponent):
			ptr->setExponent( newValue.asDouble() );
			break;
		default:
			printf("BasisPrimitiveVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = false;
			break;
	}
	Messenger::exit("BasisPrimitiveVariable::setAccessor");
	return result;
}

// Perform desired function
bool BasisPrimitiveVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("BasisPrimitiveVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for BasisPrimitive type.\n", i);
		Messenger::exit("BasisPrimitiveVariable::performFunction");
		return false;
	}
	// Get current data from ReturnValue
	bool result = true;
	BasisPrimitive* ptr = (BasisPrimitive*) rv.asPointer(VTypes::BasisPrimitiveData, result);
	if (result) switch (i)
	{
		case (BasisPrimitiveVariable::AddCoefficient):
			ptr->addCoefficient( node->argd(0) );
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in BasisPrimitiveVariable.\n", functionData[i].name);
			result = false;
			break;
	}
	Messenger::exit("BasisPrimitiveVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void BasisPrimitiveVariable::printAccessors()
{
	if (BasisPrimitiveVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:");
		QString accessors;
		for (int n=0; n<BasisPrimitiveVariable::nAccessors; ++n) accessors += QString("%1%2%3").arg(n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print(accessors);
	}
	if ((BasisPrimitiveVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:");
		QString functions;
		for (int n=0; n<BasisPrimitiveVariable::nFunctions; ++n) functions += QString("%1%2(%3)").arg(n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print(functions);
	}
}

/*
// Variable Array
*/

// Constructor
BasisPrimitiveArrayVariable::BasisPrimitiveArrayVariable(TreeNode* sizeexpr, bool constant)
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
StepNode* BasisPrimitiveArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return BasisPrimitiveVariable::accessorSearch(name, arrayIndex, argList);
}

