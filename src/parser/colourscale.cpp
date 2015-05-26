/*
	*** ColourScale Variable and Array
	*** src/parser/ColourScale.cpp
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

#include "parser/colourscale.h"
#include "parser/stepnode.h"
#include "base/colourscale.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ATEN_USING_NAMESPACE

/*
 * Variable
 */

// Constructor
ColourScaleVariable::ColourScaleVariable(ColourScale* ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ColourScaleData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
ColourScaleVariable::~ColourScaleVariable()
{
}

/*
 * Accessors
 */

// Accessor data
Accessor ColourScaleVariable::accessorData[ColourScaleVariable::nAccessors] = {
	{ "nPoints",	VTypes::IntegerData,		0, true },
	{ "points",	VTypes::ColourScalePointData,	-1, true }
};

// Function data
FunctionAccessor ColourScaleVariable::functionData[ColourScaleVariable::nFunctions] = {
	{ "addPoint",	VTypes::ColourScalePointData,	"NNNNn",	"double value, doublr r, double g, double b, double a = 1.0" },
	{ "clear",	VTypes::NoData,			"",		"" },
	{ "colour",	VTypes::NoData,			"N^N^N^N^n",	"double value, double* r, double* g, double* b, double* a = NULL" },
};

// Search variable access list for provided accessor (call private static function)
StepNode* ColourScaleVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return ColourScaleVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* ColourScaleVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("ColourScaleVariable::accessorSearch");
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
			Messenger::print("Error: Type 'ColourScale&' has no member or function named '%s'.", qPrintable(name));
			printAccessors();
			Messenger::exit("ColourScaleVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'ColourScale&' function named '%s'.", qPrintable(name));
			Messenger::exit("ColourScaleVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ColourScaleData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'ColourScale&' function '%s' is '%s(%s)'.", functionData[i].name, functionData[i].name, functionData[i].argText );
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
			Messenger::print("Error: Argument list given to 'ColourScale&' array member '%s'.", qPrintable(name));
			Messenger::exit("ColourScalelVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::ColourScaleData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("ColourScaleVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ColourScaleVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ColourScaleVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ColourScale type.\n", i);
		Messenger::exit("ColourScaleVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", accessorData[i].name);
		Messenger::exit("ColourScaleVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("ColourScaleVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	ColourScale* ptr = (ColourScale*) rv.asPointer(VTypes::ColourScaleData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ColourScaleData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (ColourScaleVariable::NPoints):
			rv.set(ptr->nPoints());
			break;
		case (ColourScaleVariable::Point):
			if ((arrayIndex < 1) || (arrayIndex > ptr->nPoints()))
			{
				Messenger::print("Array index %i is out of range for 'points' member in ColourScale.");
				result = false;
			}
			else rv.set(VTypes::ColourScalePointData, ptr->point(arrayIndex-1));
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ColourScaleVariable.\n", accessorData[i].name);
			result = false;
			break;
	}
	Messenger::exit("ColourScaleVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ColourScaleVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ColourScaleVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ColourScale type.\n", i);
		Messenger::exit("ColourScaleVariable::setAccessor");
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
		Messenger::exit("ColourScaleVariable::setAccessor");
		return false;
	}
	// Get current data from ReturnValue
	ColourScale* ptr = (ColourScale*) sourcerv.asPointer(VTypes::ColourScaleData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ColourScaleData));
		result = false;
	}
	if (result) switch (acc)
	{
		default:
			printf("ColourScaleVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = false;
			break;
	}
	Messenger::exit("ColourScaleVariable::setAccessor");
	return result;
}

// Perform desired function
bool ColourScaleVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("ColourScaleVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for ColourScale type.\n", i);
		Messenger::exit("ColourScaleVariable::performFunction");
		return false;
	}
	// Get current data from ReturnValue
	bool result = true;
	ColourScale* ptr = (ColourScale*) rv.asPointer(VTypes::ColourScaleData, result);
	GLfloat colour[4];
	if (result) switch (i)
	{
		case (ColourScaleVariable::AddPoint):
			rv.set(VTypes::ColourScalePointData, ptr->addPointAtEnd(node->argd(0), node->argd(1), node->argd(2), node->argd(3), node->hasArg(4) ? node->argd(4) : 1.0f));
			break;
		case (ColourScaleVariable::Clear):
			ptr->clear();
			rv.reset();
			break;
		case (ColourScaleVariable::Colour):
			ptr->colour(node->argd(0), colour);
			for (int n = 1; n < node->nArgs(); ++n)
			{
				rv = colour[n-1];
				node->setArg(n, rv);
			}
			rv.reset();
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in ColourScaleVariable.\n", functionData[i].name);
			result = false;
			break;
	}
	Messenger::exit("ColourScaleVariable::performFunction");
	return result;
}


// Print valid accessors/functions
void ColourScaleVariable::printAccessors()
{
	if (ColourScaleVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:");
		QString accessors;
		for (int n=0; n<ColourScaleVariable::nAccessors; ++n) accessors += QString("%1%2%3").arg(n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print(accessors);
	}
	if ((ColourScaleVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:");
		QString functions;
		for (int n=0; n<ColourScaleVariable::nFunctions; ++n) functions += QString("%1%2(%3)").arg(n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print(functions);
	}
}

/*
 * Variable Array
 */

// Constructor
ColourScaleArrayVariable::ColourScaleArrayVariable(TreeNode* sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ColourScaleData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode* ColourScaleArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return ColourScaleVariable::accessorSearch(name, arrayIndex, argList);
}

