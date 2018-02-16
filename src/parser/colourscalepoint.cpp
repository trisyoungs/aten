/*
	*** ColourScalePoint Variable and Array
	*** src/parser/ColourScalePoint.cpp
	Copyright T. Youngs 2007-2018

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
 * Variable
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
 * Accessors
 */

// Accessor data
Accessor ColourScalePointVariable::accessorData[ColourScalePointVariable::nAccessors] = {
	{ "colour",		VTypes::DoubleData,	4, false },
	{ "value",		VTypes::DoubleData,	0, true }
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
		// for (i = 0; i < nFunctions; i++) if (strcmp(qPrintable(functionData[i].name),s) == 0) break;
		i = Variable::searchAccessor(name, nFunctions, functionData);
		if (i == -1)
		{
			Messenger::print("Error: Type 'ColourScalePoint&' has no member or function named '%s'.", qPrintable(name));
			printValidAccessors(nAccessors, accessorData, nFunctions, functionData);
			Messenger::exit("ColourScalePointVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, qPrintable(functionData[i].name));
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'ColourScalePoint&' function named '%s'.", qPrintable(name));
			Messenger::exit("ColourScalePointVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ColourScalePointData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, qPrintable(functionData[i].name)))
		{
			Messenger::print("Error: Syntax for 'ColourScalePoint&' function '%s' is '%s(%s)'.", qPrintable(functionData[i].name), qPrintable(functionData[i].name), qPrintable(functionData[i].argText) );
			delete result;
			result = NULL;
		}
	}
	else
	{
		Messenger::print(Messenger::Parse, "Accessor match = %i (%s)", i, qPrintable(accessorData[i].name));
		// Were we given an array index when we didn't want one?
		if ((accessorData[i].arraySize == 0) && (arrayIndex != NULL))
		{
			Messenger::print("Error: Irrelevant array index provided for member '%s'.", qPrintable(accessorData[i].name));
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
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", qPrintable(accessorData[i].name));
		Messenger::exit("ColourScalePointVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", qPrintable(accessorData[i].name), arrayIndex, accessorData[i].arraySize);
			Messenger::exit("ColourScalePointVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	ColourScalePoint* ptr = (ColourScalePoint*) rv.asPointer(VTypes::ColourScalePointData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ColourScalePointData));
		result = false;
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
			printf("Internal Error: Access to member '%s' has not been defined in ColourScalePointVariable.\n", qPrintable(accessorData[i].name));
			result = false;
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
		return false;
	}
	Accessors acc = (Accessors) i;

	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = checkAccessorArrays(accessorData[acc], newValue, hasArrayIndex, arrayIndex);
	if (!result)
	{
		Messenger::exit("ColourScalePointVariable::setAccessor");
		return false;
	}
	
	// Get current data from ReturnValue
	ColourScalePoint* ptr = (ColourScalePoint*) sourcerv.asPointer(VTypes::ColourScalePointData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ColourScalePointData));
		result = false;
	}
	if (result) switch (acc)
	{
		default:
			printf("ColourScalePointVariable::setAccessor doesn't know how to use member '%s'.\n", qPrintable(accessorData[acc].name));
			result = false;
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
		return false;
	}
	// Get current data from ReturnValue
	bool result = true;
	ColourScalePoint* ptr = (ColourScalePoint*) rv.asPointer(VTypes::ColourScalePointData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in ColourScalePointVariable.\n", qPrintable(functionData[i].name));
			result = false;
			break;
	}
	Messenger::exit("ColourScalePointVariable::performFunction");
	return result;
}

/*
 * Variable Array
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

