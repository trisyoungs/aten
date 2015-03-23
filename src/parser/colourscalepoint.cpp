/*
	*** ColourScalePoint Variable and Array
	*** src/parser/ColourScalePoint.cpp
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

#include "parser/colourscalepoint.h"
#include "parser/stepnode.h"
#include "base/colourscalepoint.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ATEN_USING_NAMESPACE

/*
// Variable
*/

// Constructor
ColourScalePointVariable::ColourScalePointVariable(ColourScalePoint* ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ColourScalePointData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
ColourScalePointVariable::~ColourScalePointVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor ColourScalePointVariable::accessorData[ColourScalePointVariable::nAccessors] = {
	{ "colour",		VTypes::DoubleData,	4, FALSE },
	{ "value",		VTypes::DoubleData,	0, TRUE }
};

// Function data
FunctionAccessor ColourScalePointVariable::functionData[ColourScalePointVariable::nFunctions] = {
	{ "_dummy_",	VTypes::NoData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* ColourScalePointVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return ColourScalePointVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* ColourScalePointVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("ColourScalePointVariable::accessorSearch");
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
			Messenger::print("Error: Type 'ColourScalePoint&' has no member or function named '%s'.", qPrintable(name));
			printAccessors();
			Messenger::exit("ColourScalePointVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'ColourScalePoint&' function named '%s'.", qPrintable(name));
			Messenger::exit("ColourScalePointVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ColourScalePointData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'ColourScalePoint&' function '%s' is '%s(%s)'.", functionData[i].name, functionData[i].name, functionData[i].argText );
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
			Messenger::print("Error: Argument list given to 'ColourScalePoint&' array member '%s'.", qPrintable(name));
			Messenger::exit("ColourScalePointVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::ColourScalePointData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("ColourScalePointVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ColourScalePointVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ColourScalePointVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ColourScalePoint type.\n", i);
		Messenger::exit("ColourScalePointVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", accessorData[i].name);
		Messenger::exit("ColourScalePointVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("ColourScalePointVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	ColourScalePoint* ptr = (ColourScalePoint*) rv.asPointer(VTypes::ColourScalePointData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ColourScalePointData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (ColourScalePointVariable::Colour):
			if (hasArrayIndex) rv.set( ptr->colour()[arrayIndex-1] );
			else rv.setArray( VTypes::DoubleData, ptr->colour(), 4);
			break;
		case (ColourScalePointVariable::Value):
			rv.set(ptr->value());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ColourScalePointVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	Messenger::exit("ColourScalePointVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ColourScalePointVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ColourScalePointVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ColourScalePoint type.\n", i);
		Messenger::exit("ColourScalePointVariable::setAccessor");
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
				Messenger::print("Error: Array index provided for member '%s' is out of range (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
				result = FALSE;
			}
			if (newValue.arraySize() > 0)
			{
				Messenger::print("Error: An array can't be assigned to the single valued member '%s'.", accessorData[i].name);
				result = FALSE;
			}
		}
		else
		{
			if (newValue.arraySize() > accessorData[i].arraySize)
			{
				Messenger::print("Error: The array being assigned to member '%s' is larger than the size of the desination array (%i cf. %i).", accessorData[i].name, newValue.arraySize(), accessorData[i].arraySize);
				result = FALSE;
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
				result = FALSE;
			}
			else if ((newValue.type() != VTypes::VectorData) && (newValue.arraySize() != 3))
			{
				Messenger::print("Error: Only an array of size 3 can be assigned to a vector (member '%s').", accessorData[i].name);
				result = FALSE;
			}
		}
	}
	if (!result)
	{
		Messenger::exit("ColourScalePointVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	ColourScalePoint* ptr = (ColourScalePoint*) sourcerv.asPointer(VTypes::ColourScalePointData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ColourScalePointData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		default:
			printf("ColourScalePointVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	Messenger::exit("ColourScalePointVariable::setAccessor");
	return result;
}

// Perform desired function
bool ColourScalePointVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("ColourScalePointVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for ColourScalePoint type.\n", i);
		Messenger::exit("ColourScalePointVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	ColourScalePoint* ptr = (ColourScalePoint*) rv.asPointer(VTypes::ColourScalePointData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in ColourScalePointVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	Messenger::exit("ColourScalePointVariable::performFunction");
	return result;
}


// Print valid accessors/functions
void ColourScalePointVariable::printAccessors()
{
	if (ColourScalePointVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:");
		for (int n=0; n<ColourScalePointVariable::nAccessors; ++n) Messenger::print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print("");
	}
	if ((ColourScalePointVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:");
		for (int n=0; n<ColourScalePointVariable::nFunctions; ++n) Messenger::print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print("");
	}
}

/*
// Variable Array
*/

// Constructor
ColourScalePointArrayVariable::ColourScalePointArrayVariable(TreeNode* sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ColourScalePointData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode* ColourScalePointArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return ColourScalePointVariable::accessorSearch(name, arrayIndex, argList);
}

