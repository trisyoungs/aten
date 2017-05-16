/*
	*** EnergyStore Variable and Array
	*** src/parser/EnergyStore.cpp
	Copyright T. Youngs 2007-2017

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
#include "math/constants.h"
#include "base/elementmap.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ATEN_USING_NAMESPACE

/*
 * Variable
 */

// Constructor
EnergyStoreVariable::EnergyStoreVariable(EnergyStore* ptr, bool constant)
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
 * Accessors
 */

// Accessor data
Accessor EnergyStoreVariable::accessorData[EnergyStoreVariable::nAccessors] = {
	{ "angle",		VTypes::DoubleData,	0, true },
	{ "bond",		VTypes::DoubleData,	0, true },
	{ "electrostatic",	VTypes::DoubleData,	0, true },
	{ "torsion",		VTypes::DoubleData,	0, true },
	{ "total",		VTypes::DoubleData,	0, true },
	{ "ureyBradley",	VTypes::DoubleData,	0, true },
	{ "vdw",		VTypes::DoubleData,	0, true }
};

// Function data
FunctionAccessor EnergyStoreVariable::functionData[EnergyStoreVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* EnergyStoreVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return EnergyStoreVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* EnergyStoreVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("EnergyStoreVariable::accessorSearch");
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
			Messenger::print("Error: Type 'EnergyStore&' has no member or function named '%s'.", qPrintable(name));
			printValidAccessors(nAccessors, accessorData, nFunctions, functionData);
			Messenger::exit("EnergyStoreVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, qPrintable(functionData[i].name));
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'EnergyStore&' function named '%s'.", qPrintable(name));
			Messenger::exit("EnergyStoreVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::EnergyStoreData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, qPrintable(functionData[i].name)))
		{
			Messenger::print("Error: Syntax for 'EnergyStore&' function '%s' is '%s(%s)'.", qPrintable(functionData[i].name), qPrintable(functionData[i].name), qPrintable(functionData[i].argText) );
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
			Messenger::print("Error: Argument list given to 'EnergyStore&' array member '%s'.", qPrintable(name));
			Messenger::exit("EnergyStoreVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::EnergyStoreData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("EnergyStoreVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool EnergyStoreVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("EnergyStoreVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for EnergyStore type.\n", i);
		Messenger::exit("EnergyStoreVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", qPrintable(accessorData[i].name));
		Messenger::exit("EnergyStoreVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", qPrintable(accessorData[i].name), arrayIndex, accessorData[i].arraySize);
			Messenger::exit("EnergyStoreVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	EnergyStore* ptr = (EnergyStore*) rv.asPointer(VTypes::EnergyStoreData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::EnergyStoreData));
		result = false;
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
			printf("Internal Error: Access to member '%s' has not been defined in EnergyStoreVariable.\n", qPrintable(accessorData[i].name));
			result = false;
			break;
	}
	Messenger::exit("EnergyStoreVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool EnergyStoreVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("EnergyStoreVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for EnergyStore type.\n", i);
		Messenger::exit("EnergyStoreVariable::setAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;

	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = checkAccessorArrays(accessorData[acc], newValue, hasArrayIndex, arrayIndex);
	if (!result)
	{
		Messenger::exit("EnergyStoreVariable::setAccessor");
		return false;
	}

	// Get current data from ReturnValue
	EnergyStore* ptr = (EnergyStore*) sourcerv.asPointer(VTypes::EnergyStoreData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::EnergyStoreData));
		result = false;
	}
	if (result) switch (acc)
	{
		default:
			printf("EnergyStoreVariable::setAccessor doesn't know how to use member '%s'.\n", qPrintable(accessorData[acc].name));
			result = false;
			break;
	}
	Messenger::exit("EnergyStoreVariable::setAccessor");
	return result;
}

// Perform desired function
bool EnergyStoreVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("EnergyStoreVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for EnergyStore type.\n", i);
		Messenger::exit("EnergyStoreVariable::performFunction");
		return false;
	}

	// Get current data from ReturnValue
	bool result = true;
	EnergyStore* ptr = (EnergyStore*) rv.asPointer(VTypes::EnergyStoreData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in EnergyStoreVariable.\n", qPrintable(functionData[i].name));
			result = false;
			break;
	}
	Messenger::exit("EnergyStoreVariable::performFunction");
	return result;
}

/*
 * Variable Array
 */

// Constructor
EnergyStoreArrayVariable::EnergyStoreArrayVariable(TreeNode* sizeexpr, bool constant)
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
StepNode* EnergyStoreArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return EnergyStoreVariable::accessorSearch(name, arrayIndex, argList);
}

