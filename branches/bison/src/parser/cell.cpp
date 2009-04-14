/*
	*** Cell Variable and Array
	*** src/parser/cell.cpp
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

#include "parser/cell.h"
#include "parser/stepnode.h"
#include "base/cell.h"
#include "base/sysfunc.h"
#include "base/spacegroup.h"
#include "model/model.h"
#include <string.h>

/*
// Variable
*/

// Constructor
CellVariable::CellVariable(Cell *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::CellData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
CellVariable::~CellVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor CellVariable::accessorData[CellVariable::nAccessors] = {
	{ "a",		VTypes::DoubleData,	FALSE, FALSE },
	{ "b",		VTypes::DoubleData,	FALSE, FALSE },
	{ "c",		VTypes::DoubleData,	FALSE, FALSE },
	{ "alpha",	VTypes::DoubleData,	FALSE, FALSE },
	{ "beta",	VTypes::DoubleData,	FALSE, FALSE },
	{ "gamma",	VTypes::DoubleData,	FALSE, FALSE },
	{ "ax",		VTypes::DoubleData,	FALSE, FALSE },
	{ "ay",		VTypes::DoubleData,	FALSE, FALSE },
	{ "az",		VTypes::DoubleData,	FALSE, FALSE },
	{ "bx",		VTypes::DoubleData,	FALSE, FALSE },
	{ "by",		VTypes::DoubleData,	FALSE, FALSE },
	{ "bz",		VTypes::DoubleData,	FALSE, FALSE },
	{ "cx",		VTypes::DoubleData,	FALSE, FALSE },
	{ "cy",		VTypes::DoubleData,	FALSE, FALSE },
	{ "cz",		VTypes::DoubleData,	FALSE, FALSE },
	{ "centrex",	VTypes::DoubleData,	FALSE, TRUE },
	{ "centrey",	VTypes::DoubleData,	FALSE, TRUE },
	{ "centrez",	VTypes::DoubleData,	FALSE, TRUE },
	{ "density",	VTypes::DoubleData,	FALSE, TRUE },
	{ "matrix", 	VTypes::DoubleData,	TRUE, FALSE },
	{ "sgid",	VTypes::IntegerData,	FALSE, FALSE },
	{ "sgname",	VTypes::StringData,	FALSE, FALSE },
	{ "sgsetting",	VTypes::IntegerData,	FALSE, FALSE },
	{ "type",	VTypes::StringData,	FALSE, TRUE },
	{ "volume",	VTypes::DoubleData,	FALSE, TRUE },
};

// Search variable access list for provided accessor (call private static function)
StepNode *CellVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return CellVariable::accessorSearch(s, arrayindex);
}

// Private static function to search accessors
StepNode *CellVariable::accessorSearch(const char *s, TreeNode *arrayindex)
{
	msg.enter("CellVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'cell&' has no member named '%s'.\n", s);
		msg.exit("CellVariable::accessorSearch");
		return NULL;
	}
	// Create a suitable AccessNode to return...
	msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
	result = new StepNode(i, VTypes::CellData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly);
	msg.exit("CellVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool CellVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("CellVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Cell type.\n");
		msg.exit("CellVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((!accessorData[i].isArray) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("CellVariable::retrieveAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Cell *ptr= (Cell*) rv.asPointer(VTypes::CellData, result);
	if (result) switch (acc)
	{
		case (CellVariable::A):
		case (CellVariable::B):
		case (CellVariable::C):
			rv.set(ptr->lengths().get(acc - CellVariable::A));
			break;
		case (CellVariable::Alpha):
		case (CellVariable::Beta):
		case (CellVariable::Gamma):
			rv.set(ptr->angles().get(acc - CellVariable::Alpha));
			break;
		case (CellVariable::AX):
		case (CellVariable::AY):
		case (CellVariable::AZ):
		case (CellVariable::BX):
		case (CellVariable::BY):
		case (CellVariable::BZ):
		case (CellVariable::CX):
		case (CellVariable::CY):
		case (CellVariable::CZ):
			rv.set(ptr->axes().element(acc - CellVariable::AX));
			break;
		case (CellVariable::CentreX):
		case (CellVariable::CentreY):
		case (CellVariable::CentreZ):
			rv.set(ptr->centre().get(acc - CellVariable::CentreX));
			break;
		case (CellVariable::Density):
			rv.set(ptr->density());
			break;
		case (CellVariable::Matrix):
			if ((arrayIndex < 1) || (arrayIndex > 9))
			{
				msg.print("Array index [%i] is out of range for 'matrix' member.\n", arrayIndex);
				result = FALSE;
			}
			else rv.set(ptr->axes().element(arrayIndex-1));
			break;
		case (CellVariable::SpacegroupId):
			rv.set(ptr->spacegroup());
			break;
		case (CellVariable::SpacegroupName):
			rv.set(spacegroups.name(ptr->spacegroup()));
			break;
		case (CellVariable::SpacegroupSetting):
			rv.set(ptr->spacegroupSetting());
			break;
		case (CellVariable::Type):
			rv.set(lowerCase(Cell::cellType(ptr->type())));
			break;
		case (CellVariable::Volume):
			rv.set(ptr->volume());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in CellVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("CellVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool CellVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("CellVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Cell type.\n");
		msg.exit("CellVariable::setAccessor");
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
		msg.exit("CellVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Cell *ptr = (Cell*) sourcerv.asPointer(VTypes::CellData, result);
	if (result) switch (acc)
	{
		case (CellVariable::A):
		case (CellVariable::B):
		case (CellVariable::C):
		case (CellVariable::Alpha):
		case (CellVariable::Beta):
		case (CellVariable::Gamma):
		case (CellVariable::AX):
		case (CellVariable::AY):
		case (CellVariable::AZ):
		case (CellVariable::BX):
		case (CellVariable::BY):
		case (CellVariable::BZ):
		case (CellVariable::CX):
		case (CellVariable::CY):
		case (CellVariable::CZ):
			// Cast accessor into a CellParameter
			ptr->parent()->setCell( (Cell::CellParameter) acc, newvalue.asDouble());
			break;
		case (CellVariable::Matrix):
			// Cast accessor into a CellParameter
			ptr->parent()->setCell( (Cell::CellParameter) ((arrayIndex-1) + Cell::CellAX), newvalue.asDouble());
			break;
		default:
			printf("CellVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("CellVariable::setAccessor");
	return result;
}

/*
// Variable Array
*/

// Constructor
CellArrayVariable::CellArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::CellData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *CellArrayVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return CellVariable::accessorSearch(s, arrayindex);
}
