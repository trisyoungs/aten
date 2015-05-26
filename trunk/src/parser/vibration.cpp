/*
	*** Vibration Variable and Array
	*** src/parser/vibration.cpp
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

#include "parser/vibration.h"
#include "parser/stepnode.h"
#include "base/vibration.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

/*
 * Variable
 */

// Constructor
VibrationVariable::VibrationVariable(Vibration* ptr, bool constant)
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
 * Accessors
 */

// Accessor data
Accessor VibrationVariable::accessorData[VibrationVariable::nAccessors] = {
	{ "displacements",	VTypes::VectorData,		-1, true },
	{ "frequency",		VTypes::DoubleData,		0, false },
	{ "intensity",		VTypes::DoubleData,		0, false },
	{ "name",		VTypes::StringData,		0, false },
	{ "ndisplacements",	VTypes::IntegerData,		0, true },
	{ "rmass",		VTypes::DoubleData,		0, false }
};

// Function data
FunctionAccessor VibrationVariable::functionData[VibrationVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* VibrationVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return VibrationVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* VibrationVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("VibrationVariable::accessorSearch");
	StepNode* result = NULL;
	int i = 0;
	i = Variable::searchAccessor(name, nAccessors, accessorData);
	if (i == -1)
	{
		// No accessor found - is it a function definition?
		// for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		i = Variable::searchAccessor(name, nFunctions, functionData);
		if (i == -1)
		{
			Messenger::print("Error: Type 'Vibration&' has no member or function named '%s'.", qPrintable(name));
			printAccessors();
			Messenger::exit("VibrationVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Vibration&' function named '%s'.", qPrintable(name));
			Messenger::exit("VibrationVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::VibrationData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'Vibration&' function '%s' is '%s(%s)'.", functionData[i].name, functionData[i].name, functionData[i].argText );
			delete result;
			result = NULL;
		}
	}
	else
	{
		Messenger::print(Messenger::Parse, "Accessor match = %i (%s)", i, accessorData[i].name);
		// Were we given an array index when we didn't want one?
		if ((accessorData[i].arraySize == 0) && (arrayIndex != NULL))
		{
			Messenger::print("Error: Irrelevant array index provided for member '%s'.", accessorData[i].name);
			result = NULL;
		}
		// Were we given an argument list when we didn't want one?
		if (argList != NULL)
		{
			Messenger::print("Error: Argument list given to 'Vibration&' array member '%s'.", qPrintable(name));
			Messenger::exit("VibrationVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::VibrationData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("VibrationVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool VibrationVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("VibrationVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Vibration type.\n", i);
		Messenger::exit("VibrationVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", accessorData[i].name);
		Messenger::exit("VibrationVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("VibrationVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	Vibration* ptr = (Vibration*) rv.asPointer(VTypes::VibrationData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::VibrationData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (VibrationVariable::Displacements):
			if (!hasArrayIndex) rv.set( ptr->displacement(0) );
			else if (arrayIndex > ptr->nDisplacements())
			{
				Messenger::print("Displacement array index (%i) is out of bounds for vibration.", arrayIndex);
				result = false;
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
			result = false;
			break;
	}
	Messenger::exit("VibrationVariable::retrieveAccessor");
	return result;
}

// Set specified data
bool VibrationVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("VibrationVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Vibration type.\n", i);
		Messenger::exit("VibrationVariable::setAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = true;
	if (accessorData[i].arraySize != 0)
	{
		if (hasArrayIndex)
		{
			if ((accessorData[i].arraySize > 0) && ( (arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize) ))
			{
				Messenger::print("Error: Array index provided for member '%s' is out of range (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
				result = false;
			}
			if (newValue.arraySize() > 0)
			{
				Messenger::print("Error: An array can't be assigned to the single valued member '%s'.", accessorData[i].name);
				result = false;
			}
		}
		else
		{
			if ((newValue.arraySize() > 0) && (newValue.arraySize() != accessorData[i].arraySize))
			{
				Messenger::print("Error: The array being assigned to member '%s' is not of the same size (%i cf. %i).", accessorData[i].name, newValue.arraySize(), accessorData[i].arraySize);
				result = false;
			}
		}
	}
	else
	{
		// This is not an array member, so cannot be assigned an array unless its a Vector
		if (newValue.arraySize() != -1)
		{
			if (accessorData[i].returnType != VTypes::VectorData)
			{
				Messenger::print("Error: An array can't be assigned to the single valued member '%s'.", accessorData[i].name);
				result = false;
			}
			else if ((newValue.type() != VTypes::VectorData) && (newValue.arraySize() != 3))
			{
				Messenger::print("Error: Only an array of size 3 can be assigned to a vector (member '%s').", accessorData[i].name);
				result = false;
			}
		}
	}
	if (!result)
	{
		Messenger::exit("VibrationVariable::setAccessor");
		return false;
	}
	// Get current data from ReturnValue
	Vec3<double> v;
	int n;
	Vibration* ptr = (Vibration*) sourcerv.asPointer(VTypes::VibrationData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::VibrationData));
		result = false;
	}
	// Set value based on enumerated id
	if (result) switch (acc)
	{
		case (VibrationVariable::Displacements):
			if (!hasArrayIndex) ptr->setDisplacement(arrayIndex-1, newValue.asVector() );
			else if (arrayIndex > ptr->nDisplacements())
			{
				Messenger::print("Displacement array index (%i) is out of bounds for vibration.", arrayIndex);
				result = false;
			}
			else ptr->setDisplacement(arrayIndex-1, newValue.asVector() );
			break;
		case (VibrationVariable::Frequency):
			ptr->setFrequency( newValue.asDouble() );
			break;
		case (VibrationVariable::Intensity):
			ptr->setFrequency( newValue.asDouble() );
			break;
		case (VibrationVariable::Name):
			ptr->setName( newValue.asString() );
			break;
		case (VibrationVariable::ReducedMass):
			ptr->setReducedMass( newValue.asDouble() );
			break;
		default:
			printf("VibrationVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = false;
			break;
	}
	Messenger::exit("VibrationVariable::setAccessor");
	return result;
}

// Perform desired function
bool VibrationVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("VibrationVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Vibration type.\n", i);
		Messenger::exit("VibrationVariable::performFunction");
		return false;
	}
	// Get current data from ReturnValue
	bool result = true;
	Vibration* ptr = (Vibration*) rv.asPointer(VTypes::VibrationData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in VibrationVariable.\n", functionData[i].name);
			result = false;
			break;
	}
	Messenger::exit("VibrationVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void VibrationVariable::printAccessors()
{
	if (VibrationVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:");
		QString accessors;
		for (int n=0; n<VibrationVariable::nAccessors; ++n) accessors += QString("%1%2%3").arg(n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print(accessors);
	}
	if ((VibrationVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:");
		QString functions;
		for (int n=0; n<VibrationVariable::nFunctions; ++n) functions += QString("%1%2(%3)").arg(n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print(functions);
	}
}

/*
 * Variable Array
 */

// Constructor
VibrationArrayVariable::VibrationArrayVariable(TreeNode* sizeexpr, bool constant)
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
StepNode* VibrationArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return VibrationVariable::accessorSearch(name, arrayIndex, argList);
}
