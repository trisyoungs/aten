/*
	*** Vibration Variable and Array
	*** src/parser/vibration.cpp
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

#include "parser/vibration.h"
#include "parser/stepnode.h"
#include "base/vibration.h"
#include "model/model.h"
#include <string.h>

/*
// Variable
*/

// Constructor
VibrationVariable::VibrationVariable(Vibration *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::VibrationData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
VibrationVariable::~VibrationVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor VibrationVariable::accessorData[VibrationVariable::nAccessors] = {
	{ "displacements",	VTypes::VectorData,		-1, TRUE },
	{ "frequency",		VTypes::DoubleData,		0, FALSE },
	{ "intensity",		VTypes::DoubleData,		0, FALSE },
	{ "name",		VTypes::StringData,		0, FALSE },
	{ "ndisplacements",	VTypes::IntegerData,		0, TRUE },
	{ "rmass",		VTypes::DoubleData,		0, FALSE }
};

// Function data
FunctionAccessor VibrationVariable::functionData[VibrationVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *VibrationVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return VibrationVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *VibrationVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("VibrationVariable::accessorSearch");
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
			msg.print("Error: Type 'Vibration&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("VibrationVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'Vibration&' function '%s'.\n", s);
			msg.exit("VibrationVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::VibrationData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'Vibration&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
			msg.print("Error: Argument list given to 'Vibration&' array member '%s'.\n", s);
			msg.exit("VibrationVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::VibrationData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("VibrationVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool VibrationVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("VibrationVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Vibration type.\n", i);
		msg.exit("VibrationVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("VibrationVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("VibrationVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Vibration *ptr = (Vibration*) rv.asPointer(VTypes::VibrationData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::VibrationData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (VibrationVariable::Displacements):
			if (!hasArrayIndex) rv.set( ptr->displacement(0) );
			else if (arrayIndex > ptr->nDisplacements())
			{
				msg.print("Displacement array index (%i) is out of bounds for vibration.\n", arrayIndex);
				result = FALSE;
			}
			else rv.set( ptr->displacement(arrayIndex-1) );
			break;
		case (VibrationVariable::Frequency):
			rv.set(ptr->frequency());
			break;
		case (VibrationVariable::Intensity):
			rv.set(ptr->intensity());
			break;
		case (VibrationVariable::Name):
			rv.set(ptr->name());
			break;
		case (VibrationVariable::NDisplacements):
			rv.set(ptr->nDisplacements());
			break;
		case (VibrationVariable::ReducedMass):
			rv.set(ptr->reducedMass());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in VibrationVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("VibrationVariable::retrieveAccessor");
	return result;
}

// Set specified data
bool VibrationVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("VibrationVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Vibration type.\n", i);
		msg.exit("VibrationVariable::setAccessor");
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
		msg.exit("VibrationVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	Vec3<double> v;
	int n;
	Vibration *ptr = (Vibration*) sourcerv.asPointer(VTypes::VibrationData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::VibrationData));
		result = FALSE;
	}
	// Set value based on enumerated id
	if (result) switch (acc)
	{
		case (VibrationVariable::Displacements):
			if (!hasArrayIndex) ptr->setDisplacement(arrayIndex-1, newvalue.asVector() );
			else if (arrayIndex > ptr->nDisplacements())
			{
				msg.print("Displacement array index (%i) is out of bounds for vibration.\n", arrayIndex);
				result = FALSE;
			}
			else ptr->setDisplacement(arrayIndex-1, newvalue.asVector() );
			break;
		case (VibrationVariable::Frequency):
			ptr->setFrequency( newvalue.asDouble() );
			break;
		case (VibrationVariable::Intensity):
			ptr->setFrequency( newvalue.asDouble() );
			break;
		case (VibrationVariable::Name):
			ptr->setName( newvalue.asString() );
			break;
		case (VibrationVariable::ReducedMass):
			ptr->setReducedMass( newvalue.asDouble() );
			break;
		default:
			printf("VibrationVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("VibrationVariable::setAccessor");
	return result;
}

// Perform desired function
bool VibrationVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("VibrationVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Vibration type.\n", i);
		msg.exit("VibrationVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Vibration *ptr = (Vibration*) rv.asPointer(VTypes::VibrationData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in VibrationVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("VibrationVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void VibrationVariable::printAccessors()
{
	if (VibrationVariable::nAccessors > 0)
	{
		msg.print("Valid accessors are:\n");
		for (int n=0; n<VibrationVariable::nAccessors; ++n) msg.print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		msg.print("\n");
	}
	if ((VibrationVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		msg.print("Valid functions are:\n");
		for (int n=0; n<VibrationVariable::nFunctions; ++n) msg.print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		msg.print("\n");
	}
}

/*
// Variable Array
*/

// Constructor
VibrationArrayVariable::VibrationArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::VibrationData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *VibrationArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return VibrationVariable::accessorSearch(s, arrayindex, arglist);
}
