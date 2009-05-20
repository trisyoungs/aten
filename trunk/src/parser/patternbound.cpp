/*
	*** PatternBound Variable
	*** src/parser/patternbound.cpp
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

#include "parser/patternbound.h"
#include "parser/stepnode.h"
#include "base/pattern.h"
#include "classes/forcefieldbound.h"
#include <string.h>

// Constructor
PatternBoundVariable::PatternBoundVariable(PatternBound *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::PatternBoundData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
PatternBoundVariable::~PatternBoundVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor PatternBoundVariable::accessorData[PatternBoundVariable::nAccessors] = {
	{ "data", 	VTypes::DoubleData,		MAXFFBOUNDTYPES, TRUE },
	{ "escale", 	VTypes::DoubleData,		0, TRUE },
	{ "form", 	VTypes::StringData,		0, TRUE },
	{ "id", 	VTypes::IntegerData,		MAXFFBOUNDTYPES, TRUE },
	{ "typenames", 	VTypes::StringData,		MAXFFBOUNDTYPES, TRUE },
	{ "vscale",	VTypes::DoubleData,		0, TRUE }
};

// Search variable access list for provided accessor (call private static function)
StepNode *PatternBoundVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return PatternBoundVariable::accessorSearch(s, arrayindex);
}

// Private static function to search accessors
StepNode *PatternBoundVariable::accessorSearch(const char *s, TreeNode *arrayindex)
{
	msg.enter("PatternBoundVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'pattern&' has no member named '%s'.\n", s);
		msg.exit("PatternBoundVariable::accessorSearch");
		return NULL;
	}
	msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
	// Were we given an array index when we didn't want one?
	if ((accessorData[i].arraySize == 0) && (arrayindex != NULL))
	{
		msg.print("Error: Irrelevant array index provided for member '%s'.\n", accessorData[i].name);
		result = NULL;
	}
	else result = new StepNode(i, VTypes::PatternBoundData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	msg.exit("PatternBoundVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool PatternBoundVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("PatternBoundVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Pattern type.\n");
		msg.exit("PatternBoundVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("PatternBoundVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("ElementVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	PatternBound *ptr= (PatternBound*) rv.asPointer(VTypes::PatternBoundData, result);
	if (result) switch (acc)
	{
		case (PatternBoundVariable::Data):
			if (ptr->data() == NULL)
			{
				msg.print("NULL ForcefieldBound pointer found in PatternBound class.\n");
				result = FALSE;
			}
			else if (hasArrayIndex) rv.set(ptr->data()->parameter(arrayIndex-1));
			else rv.setArray(VTypes::DoubleData, ptr->data()->parameters(), MAXFFPARAMDATA);
			break;
		case (PatternBoundVariable::EScale):
			if (ptr->data() == NULL)
			{
				msg.print("NULL ForcefieldBound pointer found in PatternBound class.\n");
				result = FALSE;
			}
			else
			{
				if (ptr->data()->type() != ForcefieldBound::TorsionInteraction)
				{
					msg.print("Tried to retrieve the 1-4 coulombic scale factor for a non-torsion bound interaction.\n");
					result = FALSE;
				}
				else rv.set(ptr->data()->parameter(TF_ESCALE));
			}
			break;
		case (PatternBoundVariable::Form):
			if (ptr->data() == NULL)
			{
				msg.print("NULL ForcefieldBound pointer found in PatternBound class.\n");
				result = FALSE;
			}
			else rv.set(ptr->data()->formText());
			break;
		case (PatternBoundVariable::Id):
			if (ptr->data() == NULL)
			{
				msg.print("NULL ForcefieldBound pointer found in PatternBound class.\n");
				result = FALSE;
			}
			else if (hasArrayIndex) rv.set(ptr->atomId(arrayIndex-1)+1);
			else
			{
				// Need to adjust atom ids to go from 1-N....
				int ids[MAXFFPARAMDATA];
				for (int n=0; n<MAXFFPARAMDATA; ++n) ids[n] = ptr->atomIds_[n];
				rv.setArray(VTypes::IntegerData, &ids, MAXFFPARAMDATA);
			}
			break;
		case (PatternBoundVariable::VScale):
			if (ptr->data() == NULL)
			{
				msg.print("NULL ForcefieldBound pointer found in PatternBound class.\n");
				result = FALSE;
			}
			else
			{
				if (ptr->data()->type() != ForcefieldBound::TorsionInteraction)
				{
					msg.print("Tried to retrieve the 1-4 VDW scale factor for a non-torsion bound interaction.\n");
					result = FALSE;
				}
				else rv.set(ptr->data()->parameter(TF_VSCALE));
			}
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in PatternBoundVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("PatternBoundVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool PatternBoundVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("PatternBoundVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Pattern type.\n");
		msg.exit("PatternBoundVariable::setAccessor");
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
			if ((newvalue.arraySize() > 0) && (newvalue.arraySize() != accessorData[i].arraySize))
			{
				msg.print("Error: The array being assigned to member '%s' is not of the same size (%i cf. %i).\n", accessorData[i].name, newvalue.arraySize(), accessorData[i].arraySize);
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
		msg.exit("PatternBoundVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	PatternBound *ptr= (PatternBound*) sourcerv.asPointer(VTypes::PatternBoundData, result);
	// Set value based on enumerated id
	if (result) switch (acc)
	{
		default:
			printf("PatternBoundVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("PatternBoundVariable::setAccessor");
	return result;
}

/*
// Variable Array
*/

// Constructor
PatternBoundArrayVariable::PatternBoundArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::PatternBoundData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *PatternBoundArrayVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return PatternBoundVariable::accessorSearch(s, arrayindex);
}
