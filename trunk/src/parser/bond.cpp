/*
	*** Bond Variable and Array
	*** src/parser/bond.cpp
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

#include "parser/bond.h"
#include "parser/stepnode.h"
#include "base/bond.h"
#include "math/constants.h"
#include "base/elements.h"
#include "model/model.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
// Variable
*/

// Constructor
BondVariable::BondVariable(Bond *ptr, bool constant)
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
// Accessors
*/

// Accessor data
Accessor BondVariable::accessorData[BondVariable::nAccessors] = {
	{ "i",		VTypes::AtomData,	0, TRUE },
	{ "j",		VTypes::AtomData,	0, TRUE },
	{ "order",	VTypes::DoubleData,	0, TRUE },
	{ "type",	VTypes::StringData,	0, FALSE }
};

// Function data
FunctionAccessor BondVariable::functionData[BondVariable::nFunctions] = {
	{ "partner",	VTypes::AtomData,	"A",	"Atom i" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *BondVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return BondVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *BondVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("BondVariable::accessorSearch");
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
			msg.print("Error: Type 'Bond&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("BondVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'Bond&' function '%s'.\n", s);
			msg.exit("BondVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::BondData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'Bond&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
			msg.print("Error: Argument list given to 'Bond&' array member '%s'.\n", s);
			msg.exit("BondVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::BondData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("BondVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool BondVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("BondVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Bond type.\n", i);
		msg.exit("BondVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("BondVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("BondVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Bond *ptr = (Bond*) rv.asPointer(VTypes::BondData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::BondData));
		result = FALSE;
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
			printf("Internal Error: Access to member '%s' has not been defined in BondVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("BondVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool BondVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("BondVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Bond type.\n", i);
		msg.exit("BondVariable::setAccessor");
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
		msg.exit("BondVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	Bond *ptr = (Bond*) sourcerv.asPointer(VTypes::BondData, result);
	Bond::BondType bt;
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::BondData));
		result = FALSE;
	}
	Model* ptrParent = ptr->atomI()->parent();
	if (result) switch (acc)
	{
		case (BondVariable::Type):
			bt = Bond::bondType(newvalue.asString(), TRUE);
			if (bt == Bond::nBondTypes) break;
			ptrParent->changeBond(ptr, bt);
			break;
		default:
			printf("BondVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("BondVariable::setAccessor");
	return result;
}

// Perform desired function
bool BondVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("BondVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Bond type.\n", i);
		msg.exit("BondVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Bond *ptr = (Bond*) rv.asPointer(VTypes::BondData, result);
	if (result) switch (i)
	{
		case (BondVariable::Partner):
			rv.set(VTypes::AtomData, ptr->partner( (Atom*) node->argp(0, VTypes::AtomData)));
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in BondVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("BondVariable::performFunction");
	return result;
}


// Print valid accessors/functions
void BondVariable::printAccessors()
{
	if (BondVariable::nAccessors > 0)
	{
		msg.print("Valid accessors are:\n");
		for (int n=0; n<BondVariable::nAccessors; ++n) msg.print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		msg.print("\n");
	}
	if ((BondVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		msg.print("Valid functions are:\n");
		for (int n=0; n<BondVariable::nFunctions; ++n) msg.print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		msg.print("\n");
	}
}

/*
// Variable Array
*/

// Constructor
BondArrayVariable::BondArrayVariable(TreeNode *sizeexpr, bool constant)
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
StepNode *BondArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return BondVariable::accessorSearch(s, arrayindex, arglist);
}

