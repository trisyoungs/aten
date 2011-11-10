/*
	*** EnergyStore Variable and Array
	*** src/parser/EnergyStore.cpp
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

#include "parser/energystore.h"
#include "parser/stepnode.h"
#include "ff/energystore.h"
#include "base/constants.h"
#include "base/elements.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
// Variable
*/

// Constructor
EnergyStoreVariable::EnergyStoreVariable(EnergyStore *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::EnergyStoreData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
EnergyStoreVariable::~EnergyStoreVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor EnergyStoreVariable::accessorData[EnergyStoreVariable::nAccessors] = {
	{ "angle",		VTypes::DoubleData,	0, TRUE },
	{ "bond",		VTypes::DoubleData,	0, TRUE },
	{ "electrostatic",	VTypes::DoubleData,	0, TRUE },
	{ "torsion",		VTypes::DoubleData,	0, TRUE },
	{ "total",		VTypes::DoubleData,	0, TRUE },
	{ "ureyBradley",	VTypes::DoubleData,	0, TRUE },
	{ "vdw",		VTypes::DoubleData,	0, TRUE }
};

// Function data
FunctionAccessor EnergyStoreVariable::functionData[EnergyStoreVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *EnergyStoreVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return EnergyStoreVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *EnergyStoreVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("EnergyStoreVariable::accessorSearch");
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
			msg.print("Error: Type 'EnergyStore&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("EnergyStoreVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'EnergyStore&' function '%s'.\n", s);
			msg.exit("EnergyStoreVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::EnergyStoreData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'EnergyStore&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
			msg.print("Error: Argument list given to 'EnergyStore&' array member '%s'.\n", s);
			msg.exit("EnergyStoreVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::EnergyStoreData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("EnergyStoreVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool EnergyStoreVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("EnergyStoreVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for EnergyStore type.\n", i);
		msg.exit("EnergyStoreVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("EnergyStoreVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("EnergyStoreVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	EnergyStore *ptr = (EnergyStore*) rv.asPointer(VTypes::EnergyStoreData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::EnergyStoreData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (EnergyStoreVariable::Angle):
			rv.set(ptr->angle());
			break;
		case (EnergyStoreVariable::Bond):
			rv.set(ptr->bond());
			break;
		case (EnergyStoreVariable::Electrostatic):
			rv.set(ptr->electrostatic());
			break;
		case (EnergyStoreVariable::Torsion):
			rv.set(ptr->torsion());
			break;
		case (EnergyStoreVariable::Total):
			rv.set(ptr->total());
			break;
		case (EnergyStoreVariable::UreyBradley):
			rv.set(ptr->ureyBradley());
			break;
		case (EnergyStoreVariable::Vdw):
			rv.set(ptr->vdw());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in EnergyStoreVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("EnergyStoreVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool EnergyStoreVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("EnergyStoreVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for EnergyStore type.\n", i);
		msg.exit("EnergyStoreVariable::setAccessor");
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
		msg.exit("EnergyStoreVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	EnergyStore *ptr = (EnergyStore*) sourcerv.asPointer(VTypes::EnergyStoreData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::EnergyStoreData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		default:
			printf("EnergyStoreVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("EnergyStoreVariable::setAccessor");
	return result;
}

// Perform desired function
bool EnergyStoreVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("EnergyStoreVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for EnergyStore type.\n", i);
		msg.exit("EnergyStoreVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	EnergyStore *ptr = (EnergyStore*) rv.asPointer(VTypes::EnergyStoreData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in EnergyStoreVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("EnergyStoreVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void EnergyStoreVariable::printAccessors()
{
	if (EnergyStoreVariable::nAccessors > 0)
	{
		msg.print("Valid accessors are:\n");
		for (int n=0; n<EnergyStoreVariable::nAccessors; ++n) msg.print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		msg.print("\n");
	}
	if ((EnergyStoreVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		msg.print("Valid functions are:\n");
		for (int n=0; n<EnergyStoreVariable::nFunctions; ++n) msg.print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		msg.print("\n");
	}
}

/*
// Variable Array
*/

// Constructor
EnergyStoreArrayVariable::EnergyStoreArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::EnergyStoreData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *EnergyStoreArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return EnergyStoreVariable::accessorSearch(s, arrayindex, arglist);
}

