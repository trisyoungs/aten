/*
	*** Widget Variable
	*** src/parser/widget.cpp
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

#include "parser/widget.h"
#include "parser/stepnode.h"
#include "parser/treegui.h"
//#include "base/constants.h"
//#include "model/model.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Constructors
WidgetVariable::WidgetVariable(TreeGuiWidget *widget, bool constant)
{
	// Private variables
	returnType_ = VTypes::WidgetData;
	readOnly_ = constant;
	pointerData_ = widget;
}

// Destructor
WidgetVariable::~WidgetVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor WidgetVariable::accessorData[WidgetVariable::nAccessors] = {
	{ "prefs",	VTypes::PreferencesData,	0, TRUE }
};

// Function data
FunctionAccessor WidgetVariable::functionData[WidgetVariable::nFunctions] = {
	{ "convertenergy",	VTypes::DoubleData,	"NS",	"double value, string oldunits" },
	{ "findelement",	VTypes::ElementData,	"S",	"string name" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *WidgetVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return WidgetVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *WidgetVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("WidgetVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		// No accessor found - is it a function definition?
		for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		if (i == nFunctions)
		{
			msg.print("Error: Type 'dialog&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("WidgetVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'dialog&' function '%s'.\n", s);
			msg.exit("WidgetVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::AtenData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'dialog&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		else result = new StepNode(i, VTypes::AtenData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("WidgetVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool WidgetVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("WidgetVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Aten type.\n", i);
		msg.exit("WidgetVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("WidgetVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < (acc == WidgetVariable::Elements ? 0 : 1)) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("WidgetVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Variables used in retrieval
	bool result = TRUE;
	TreeGuiWidget *ptr = (TreeGuiWidget*) rv.asPointer(VTypes::WidgetData, result);
	if (result) switch (acc)
	{
		case (WidgetVariable::Elements):

			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in WidgetVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("WidgetVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool WidgetVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("WidgetVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Aten type.\n", i);
		msg.exit("WidgetVariable::setAccessor");
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
		msg.exit("WidgetVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	TreeGuiWidget *ptr = (TreeGuiWidget*) sourcerv.asPointer(VTypes::WidgetData, result);
	switch (acc)
	{
		default:
			printf("WidgetVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("WidgetVariable::setAccessor");
	return result;
}

// Perform desired function
bool WidgetVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("WidgetVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Aten type.\n", i);
		msg.exit("WidgetVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	int el;
	TreeGuiWidget *ptr = (TreeGuiWidget*) rv.asPointer(VTypes::WidgetData, result);
	if (result) switch (i)
	{
		case (WidgetVariable::ConvertEnergy):
			break;
		case (WidgetVariable::FindElement):
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in WidgetVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("WidgetVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void WidgetVariable::printAccessors()
{
	if (WidgetVariable::nAccessors > 0)
	{
		msg.print("Valid accessors are:\n");
		for (int n=0; n<WidgetVariable::nAccessors; ++n) msg.print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		msg.print("\n");
	}
	if ((WidgetVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		msg.print("Valid functions are:\n");
		for (int n=0; n<WidgetVariable::nFunctions; ++n) msg.print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		msg.print("\n");
	}
}

/*
// Variable Array
*/

// Constructor
WidgetArrayVariable::WidgetArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::WidgetData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *WidgetArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return WidgetVariable::accessorSearch(s, arrayindex, arglist);
}
