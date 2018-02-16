/*
	*** Bond Variable and Array
	*** src/parser/bond.cpp
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

#include "parser/bond.h"
#include "parser/stepnode.h"
#include "base/bond.h"
#include "math/constants.h"
#include "base/elementmap.h"
#include "model/model.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ATEN_USING_NAMESPACE

/*
 * Variable
 */

// Constructor
BondVariable::BondVariable(Bond* ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::BondData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
BondVariable::~BondVariable()
{
}

/*
 * Accessors
 */

// Accessor data
Accessor BondVariable::accessorData[BondVariable::nAccessors] = {
	{ "i",		VTypes::AtomData,	0, true },
	{ "j",		VTypes::AtomData,	0, true },
	{ "order",	VTypes::DoubleData,	0, true },
	{ "type",	VTypes::StringData,	0, false }
};

// Function data
FunctionAccessor BondVariable::functionData[BondVariable::nFunctions] = {
	{ "partner",	VTypes::AtomData,	"A",	"Atom i" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* BondVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return BondVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* BondVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("BondVariable::accessorSearch");
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
			Messenger::print("Error: Type 'Bond&' has no member or function named '%s'.", qPrintable(name));
			printValidAccessors(nAccessors, accessorData, nFunctions, functionData);
			Messenger::exit("BondVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, qPrintable(functionData[i].name));
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Bond&' function '%s'.", qPrintable(name));
			Messenger::exit("BondVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::BondData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, qPrintable(functionData[i].name)))
		{
			Messenger::print("Error: Syntax for 'Bond&' function '%s' is '%s(%s)'.", qPrintable(functionData[i].name), qPrintable(functionData[i].name), qPrintable(functionData[i].argText) );
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
			Messenger::print("Error: Argument list given to 'Bond&' array member '%s'.", qPrintable(name));
			Messenger::exit("BondVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::BondData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("BondVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool BondVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("BondVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Bond type.\n", i);
		Messenger::exit("BondVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", qPrintable(accessorData[i].name));
		Messenger::exit("BondVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", qPrintable(accessorData[i].name), arrayIndex, accessorData[i].arraySize);
			Messenger::exit("BondVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	Bond* ptr = (Bond*) rv.asPointer(VTypes::BondData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::BondData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (BondVariable::I):
			rv.set(VTypes::AtomData, ptr->atomI());
			break;
		case (BondVariable::J):
			rv.set(VTypes::AtomData, ptr->atomJ());
			break;
		case (BondVariable::Order):
			rv.set(ptr->order());
			break;
		case (BondVariable::Type):
			rv.set(Bond::bondType(ptr->type()));
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in BondVariable.\n", qPrintable(accessorData[i].name));
			result = false;
			break;
	}
	Messenger::exit("BondVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool BondVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("BondVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Bond type.\n", i);
		Messenger::exit("BondVariable::setAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;

	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = checkAccessorArrays(accessorData[acc], newValue, hasArrayIndex, arrayIndex);
	if (!result)
	{
		Messenger::exit("BondVariable::setAccessor");
		return false;
	}

	// Get current data from ReturnValue
	Bond* ptr = (Bond*) sourcerv.asPointer(VTypes::BondData, result);
	Bond::BondType bt;
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::BondData));
		result = false;
	}
	Model* ptrParent = ptr->atomI()->parent();
	if (result) switch (acc)
	{
		case (BondVariable::Type):
			bt = Bond::bondType(newValue.asString(), true);
			if (bt == Bond::nBondTypes) break;
			ptrParent->changeBond(ptr, bt);
			break;
		default:
			printf("BondVariable::setAccessor doesn't know how to use member '%s'.\n", qPrintable(accessorData[acc].name));
			result = false;
			break;
	}
	Messenger::exit("BondVariable::setAccessor");
	return result;
}

// Perform desired function
bool BondVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("BondVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Bond type.\n", i);
		Messenger::exit("BondVariable::performFunction");
		return false;
	}
	// Get current data from ReturnValue
	bool result = true;
	Bond* ptr = (Bond*) rv.asPointer(VTypes::BondData, result);
	if (result) switch (i)
	{
		case (BondVariable::Partner):
			rv.set(VTypes::AtomData, ptr->partner( (Atom*) node->argp(0, VTypes::AtomData)));
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in BondVariable.\n", qPrintable(functionData[i].name));
			result = false;
			break;
	}
	Messenger::exit("BondVariable::performFunction");
	return result;
}

/*
 * Variable Array
 */

// Constructor
BondArrayVariable::BondArrayVariable(TreeNode* sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::BondData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode* BondArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return BondVariable::accessorSearch(name, arrayIndex, argList);
}

