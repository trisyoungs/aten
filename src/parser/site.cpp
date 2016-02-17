/*
	*** Site Variable and Array
	*** src/parser/site.cpp
	Copyright T. Youngs 2007-2016

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

#include "parser/site.h"
#include "parser/stepnode.h"

ATEN_USING_NAMESPACE

/*
 * Variable
 */

// Constructor
SiteVariable::SiteVariable(Site* ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::SiteData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
SiteVariable::~SiteVariable()
{
}

/*
 * Accessors
 */

// Accessor data
Accessor SiteVariable::accessorData[SiteVariable::nAccessors] = {
	{ ".dummy",	VTypes::IntegerData,	0, true }
};

// Function data
FunctionAccessor SiteVariable::functionData[SiteVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* SiteVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return SiteVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* SiteVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("SiteVariable::accessorSearch");
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
			Messenger::print("Error: Type 'Site&' has no member or function named '%s'.", qPrintable(name));
			printValidAccessors(nAccessors, accessorData, nFunctions, functionData);
			Messenger::exit("SiteVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, qPrintable(functionData[i].name));
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Site&' function named '%s'.", qPrintable(name));
			Messenger::exit("SiteVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::SiteData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, qPrintable(functionData[i].name)))
		{
			Messenger::print("Error: Syntax for 'Site&' function '%s' is '%s(%s)'.", qPrintable(functionData[i].name), qPrintable(functionData[i].name), qPrintable(functionData[i].argText) );
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
			Messenger::print("Error: Argument list given to 'Site&' array member '%s'.", qPrintable(name));
			Messenger::exit("SiteVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::SiteData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("SiteVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool SiteVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("SiteVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Site type.\n", i);
		Messenger::exit("SiteVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", qPrintable(accessorData[i].name));
		Messenger::exit("SiteVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", qPrintable(accessorData[i].name), arrayIndex, accessorData[i].arraySize);
			Messenger::exit("SiteVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	Site* ptr = (Site*) rv.asPointer(VTypes::SiteData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::SiteData));
		result = false;
	}
	if (result) switch (acc)
	{
		default:
			printf("Internal Error: Access to member '%s' has not been defined in SiteVariable.\n", qPrintable(accessorData[i].name));
			result = false;
			break;
	}
	Messenger::exit("SiteVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool SiteVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("SiteVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Site type.\n", i);
		Messenger::exit("SiteVariable::setAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;

	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = checkAccessorArrays(accessorData[acc], newValue, hasArrayIndex, arrayIndex);
	if (!result)
	{
		Messenger::exit("SiteVariable::setAccessor");
		return false;
	}

	// Get current data from ReturnValue
	Site* ptr = (Site*) sourcerv.asPointer(VTypes::SiteData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::SiteData));
		result = false;
	}
	if (result) switch (acc)
	{
		default:
			printf("SiteVariable::setAccessor doesn't know how to use member '%s'.\n", qPrintable(accessorData[acc].name));
			result = false;
			break;
	}
	Messenger::exit("SiteVariable::setAccessor");
	return result;
}

// Perform desired function
bool SiteVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("SiteVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Site type.\n", i);
		Messenger::exit("SiteVariable::performFunction");
		return false;
	}

	// Get current data from ReturnValue
	bool result = true;
	Site* ptr = (Site*) rv.asPointer(VTypes::SiteData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in SiteVariable.\n", qPrintable(functionData[i].name));
			result = false;
			break;
	}
	Messenger::exit("SiteVariable::performFunction");
	return result;
}

/*
 * Variable Array
 */

// Constructor
SiteArrayVariable::SiteArrayVariable(TreeNode* sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::SiteData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode* SiteArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return SiteVariable::accessorSearch(name, arrayIndex, argList);
}

