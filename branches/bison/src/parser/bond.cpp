/*
	*** Bond Variable and Array
	*** src/parser/bond.cpp
	Copyright T. Youngs 2007-2009

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
#include "base/constants.h"
#include "base/elements.h"
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
	{ "i",		VTypes::AtomData,	FALSE, TRUE },
	{ "j",		VTypes::AtomData,	FALSE, TRUE },
	{ "order",	VTypes::DoubleData,	FALSE, TRUE },
	{ "type",	VTypes::StringData,	FALSE, TRUE }
};

// Search variable access list for provided accessor (call private static function)
StepNode *BondVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return BondVariable::accessorSearch(s, arrayindex);
}

// Private static function to search accessors
StepNode *BondVariable::accessorSearch(const char *s, TreeNode *arrayindex)
{
	msg.enter("BondVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'bond&' has no member named '%s'.\n", s);
		msg.exit("BondVariable::accessorSearch");
		return NULL;
	}
	// Create a suitable AccessNode to return...
	msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
	result = new StepNode(i, VTypes::BondData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly);
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
		printf("Internal Error: Accessor id %i is out of range for Bond type.\n");
		msg.exit("BondVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((!accessorData[i].isArray) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("BondVariable::retrieveAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Bond *ptr= (Bond*) rv.asPointer(VTypes::BondData, result);
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
		printf("Internal Error: Accessor id %i is out of range for Bond type.\n");
		msg.exit("BondVariable::setAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if (!accessorData[i].isArray)
	{
		if (hasArrayIndex) msg.print("Warning: Irrelevant array index provided for member '%s'.\n", accessorData[i].name);
	}
	else if (!hasArrayIndex)
	{
		msg.print("Error: No array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("BondVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Bond *ptr= (Bond*) sourcerv.asPointer(VTypes::BondData, result);
	switch (acc)
	{
		default:
			printf("BondVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("BondVariable::setAccessor");
	return result;
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
StepNode *BondArrayVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return BondVariable::accessorSearch(s, arrayindex);
}

