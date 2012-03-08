/*
	*** ColourScale Variable and Array
	*** src/parser/ColourScale.cpp
	Copyright T. Youngs 2007-2012

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
#include "classes/colourscale.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
// Variable
*/

// Constructor
ColourScaleVariable::ColourScaleVariable(ColourScale *ptr, bool constant)
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
// Accessors
*/

// Accessor data
Accessor ColourScaleVariable::accessorData[ColourScaleVariable::nAccessors] = {
	{ "nPoints",	VTypes::IntegerData,		0, TRUE },
	{ "points",	VTypes::ColourScalePointData,	-1, TRUE }
};

// Function data
FunctionAccessor ColourScaleVariable::functionData[ColourScaleVariable::nFunctions] = {
	{ "addPoint",	VTypes::ColourScalePointData,	"NNNNn",	"double value, doublr r, double g, double b, double a = 1.0" },
	{ "clear",	VTypes::NoData,			"",		"" },
	{ "colour",	VTypes::NoData,			"N^N^N^N^n",	"double value, double *r, double *g, double *b, double *a = NULL" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *ColourScaleVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return ColourScaleVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *ColourScaleVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("ColourScaleVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	i = Variable::searchAccessor(s, nAccessors, accessorData);
	if (i == -1)
	{
		// No accessor found - is it a function definition?
		// for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		i = Variable::searchAccessor(s, nFunctions, functionData);
		if (i == -1)
		{
			msg.print("Error: Type 'ColourScale&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("ColourScaleVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'ColourScale&' function '%s'.\n", s);
			msg.exit("ColourScaleVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ColourScaleData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'ColourScale&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		// Were we given an argument list when we didn't want one?
		if (arglist != NULL)
		{
			msg.print("Error: Argument list given to 'ColourScale&' array member '%s'.\n", s);
			msg.exit("ColourScalelVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::ColourScaleData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("ColourScaleVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ColourScaleVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ColourScaleVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ColourScale type.\n", i);
		msg.exit("ColourScaleVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("ColourScaleVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("ColourScaleVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	ColourScale *ptr = (ColourScale*) rv.asPointer(VTypes::ColourScaleData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::ColourScaleData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (ColourScaleVariable::NPoints):
			rv.set(ptr->nPoints());
			break;
		case (ColourScaleVariable::Point):
			if ((arrayIndex < 1) || (arrayIndex > ptr->nPoints()))
			{
				msg.print("Array index %i is out of range for 'points' member in ColourScale.\n");
				result = FALSE;
			}
			else rv.set(VTypes::ColourScalePointData, ptr->point(arrayIndex-1));
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ColourScaleVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ColourScaleVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ColourScaleVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ColourScaleVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ColourScale type.\n", i);
		msg.exit("ColourScaleVariable::setAccessor");
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
		msg.exit("ColourScaleVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	ColourScale *ptr = (ColourScale*) sourcerv.asPointer(VTypes::ColourScaleData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::ColourScaleData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		default:
			printf("ColourScaleVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("ColourScaleVariable::setAccessor");
	return result;
}

// Perform desired function
bool ColourScaleVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("ColourScaleVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for ColourScale type.\n", i);
		msg.exit("ColourScaleVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	ColourScale *ptr = (ColourScale*) rv.asPointer(VTypes::ColourScaleData, result);
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
			result = FALSE;
			break;
	}
	msg.exit("ColourScaleVariable::performFunction");
	return result;
}


// Print valid accessors/functions
void ColourScaleVariable::printAccessors()
{
	if (ColourScaleVariable::nAccessors > 0)
	{
		msg.print("Valid accessors are:\n");
		for (int n=0; n<ColourScaleVariable::nAccessors; ++n) msg.print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		msg.print("\n");
	}
	if ((ColourScaleVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		msg.print("Valid functions are:\n");
		for (int n=0; n<ColourScaleVariable::nFunctions; ++n) msg.print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		msg.print("\n");
	}
}

/*
// Variable Array
*/

// Constructor
ColourScaleArrayVariable::ColourScaleArrayVariable(TreeNode *sizeexpr, bool constant)
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
StepNode *ColourScaleArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return ColourScaleVariable::accessorSearch(s, arrayindex, arglist);
}

