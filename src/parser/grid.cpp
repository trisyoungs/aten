/*
	*** Grid Variable and Array
	*** src/parser/grid.cpp
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

#include "parser/grid.h"
#include "parser/stepnode.h"
#include "classes/grid.h"
#include "base/constants.h"
#include "base/elements.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
// Variable
*/

// Constructor
GridVariable::GridVariable(Grid *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::GridData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
GridVariable::~GridVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor GridVariable::accessorData[GridVariable::nAccessors] = {
	{ "axes",	VTypes::CellData,	FALSE, TRUE },
	{ "name",	VTypes::StringData,	FALSE, FALSE }
};

// Search variable access list for provided accessor (call private static function)
StepNode *GridVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return GridVariable::accessorSearch(s, arrayindex);
}

// Private static function to search accessors
StepNode *GridVariable::accessorSearch(const char *s, TreeNode *arrayindex)
{
	msg.enter("GridVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'grid&' has no member named '%s'.\n", s);
		msg.exit("GridVariable::accessorSearch");
		return NULL;
	}
	// Create a suitable AccessNode to return...
	msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
	result = new StepNode(i, VTypes::GridData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly);
	msg.exit("GridVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool GridVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("GridVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Grid type.\n");
		msg.exit("GridVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((!accessorData[i].isArray) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("GridVariable::retrieveAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Grid *ptr= (Grid*) rv.asPointer(VTypes::GridData, result);
	if (result) switch (acc)
	{
		case (GridVariable::Name):
			rv.set(ptr->name());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in GridVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("GridVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool GridVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("GridVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Grid type.\n");
		msg.exit("GridVariable::setAccessor");
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
		msg.exit("GridVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Grid *ptr= (Grid*) sourcerv.asPointer(VTypes::GridData, result);
	switch (acc)
	{
		case (GridVariable::Name):
			ptr->setName( newvalue.asString() );
			break;
		default:
			printf("GridVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("GridVariable::setAccessor");
	return result;
}

/*
// Variable Array
*/

// Constructor
GridArrayVariable::GridArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::GridData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *GridArrayVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return GridVariable::accessorSearch(s, arrayindex);
}

