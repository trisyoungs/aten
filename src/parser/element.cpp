/*
	*** Elements Variable
	*** src/parser/element.cpp
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

#include "parser/element.h"
#include "parser/stepnode.h"
#include "base/elementmap.h"
#include <string.h>

ATEN_USING_NAMESPACE

// Constructor
ElementVariable::ElementVariable(Element* el, bool constant)
{
	// Private variables
	returnType_ = VTypes::ElementData;
	readOnly_ = constant;
	pointerData_ = el;
}

// Destructor
ElementVariable::~ElementVariable()
{
}

/*
 * Accessors
 */

// Accessor data
Accessor ElementVariable::accessorData[ElementVariable::nAccessors] = {
	{ "colour",	VTypes::DoubleData,	4, false },
	{ "group",	VTypes::IntegerData,	0, true },
	{ "mass",	VTypes::DoubleData,	0, true },
	{ "name",	VTypes::StringData,	0, true },
	{ "radius",	VTypes::DoubleData,	0, false },
	{ "symbol",	VTypes::StringData,	0, true },
	{ "z",		VTypes::IntegerData,	0, true }
};

// Function data
FunctionAccessor ElementVariable::functionData[ElementVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* ElementVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return ElementVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* ElementVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("ElementVariable::accessorSearch");
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
			Messenger::print("Error: Type 'Element&' has no member or function named '%s'.", qPrintable(name));
			printValidAccessors(nAccessors, accessorData, nFunctions, functionData);
			Messenger::exit("ElementVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, qPrintable(functionData[i].name));
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Element&' function named '%s'.", qPrintable(name));
			Messenger::exit("ElementVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ElementData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, qPrintable(functionData[i].name)))
		{
			Messenger::print("Error: Syntax for 'Element&' function '%s' is '%s(%s)'.", qPrintable(functionData[i].name), qPrintable(functionData[i].name), qPrintable(functionData[i].argText) );
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
			Messenger::print("Error: Argument list given to 'Element&' array member '%s'.", qPrintable(name));
			Messenger::exit("ElementVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::ElementData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("ElementVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ElementVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ElementVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Elements type.\n", i);
		Messenger::exit("ElementVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", qPrintable(accessorData[i].name));
		Messenger::exit("ElementVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", qPrintable(accessorData[i].name), arrayIndex, accessorData[i].arraySize);
			Messenger::exit("ElementVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result;
	Element* ptr = (Element*) rv.asPointer(VTypes::ElementData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ElementData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (ElementVariable::Colour):
			if (hasArrayIndex) rv.set( ptr->colour[arrayIndex-1] );
			else rv.setArray( VTypes::DoubleData, &ptr->colour, 4);
			break;
		case (ElementVariable::Group):
			rv.set( ptr->group );
			break;
		case (ElementVariable::Mass):
			rv.set( ptr->atomicMass );
			break;
		case (ElementVariable::Name):
			rv.set( ptr->name );
			break;
		case (ElementVariable::Radius):
			rv.set( ptr->atomicRadius );
			break;
		case (ElementVariable::Symbol):
			rv.set( ptr->symbol );
			break;
		case (ElementVariable::Z):
			rv.set( ptr->z );
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ElementVariable.\n", qPrintable(accessorData[i].name));
			result = false;
			break;
	}
	Messenger::exit("ElementVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ElementVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ElementVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Elements type.\n", i);
		Messenger::exit("ElementVariable::setAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;

	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = checkAccessorArrays(accessorData[acc], newValue, hasArrayIndex, arrayIndex);
	if (!result)
	{
		Messenger::exit("ElementVariable::setAccessor");
		return false;
	}

	// Get current data from ReturnValue
	Element* ptr = (Element*) sourcerv.asPointer(VTypes::ElementData, result);
	int n;
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ElementData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (ElementVariable::Colour):
			if (newValue.type() == VTypes::VectorData) for (n=0; n<3; ++n) ptr->colour[n] = newValue.asVector(result)[n];
			else if (newValue.arraySize() != -1) for (n=0; n<newValue.arraySize(); ++n) ptr->colour[n] = newValue.asDouble(n, result);
			else if (hasArrayIndex) ptr->colour[arrayIndex-1] = newValue.asDouble(result);
			else for (n=0; n<4; ++n) ptr->colour[n] = newValue.asDouble(result);
			break;
		case (ElementVariable::Radius):
			ptr->atomicRadius = newValue.asDouble(result);
			break;
		default:
			printf("ElementVariable::setAccessor doesn't know how to use member '%s'.\n", qPrintable(accessorData[acc].name));
			result = false;
			break;
	}
	Messenger::exit("ElementVariable::setAccessor");
	return result;
}

// Perform desired function
bool ElementVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("ElementVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Element type.\n", i);
		Messenger::exit("ElementVariable::performFunction");
		return false;
	}
	// Get current data from ReturnValue
	bool result = true;
	Element* ptr = (Element*) rv.asPointer(VTypes::ElementData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in ElementVariable.\n", qPrintable(functionData[i].name));
			result = false;
			break;
	}
	Messenger::exit("ElementVariable::performFunction");
	return result;
}
