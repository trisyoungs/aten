/*
	*** Eigenvector Variable and Array
	*** src/parser/EigenVector.cpp
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

#include "parser/eigenvector.h"
#include "parser/stepnode.h"
#include "base/eigenvector.h"
#include "math/constants.h"
#include "base/elements.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
// Variable
*/

// Constructor
EigenvectorVariable::EigenvectorVariable(Eigenvector *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::EigenvectorData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
EigenvectorVariable::~EigenvectorVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor EigenvectorVariable::accessorData[EigenvectorVariable::nAccessors] = {
	{ "eigenvalue",	VTypes::DoubleData,	0, FALSE },
	{ "name",	VTypes::StringData,	0, FALSE },
	{ "occupancy",	VTypes::DoubleData,	0, FALSE },
	{ "size",	VTypes::IntegerData,	0, TRUE },
	{ "vector",	VTypes::IntegerData,	-1, FALSE }
};

// Function data
FunctionAccessor EigenvectorVariable::functionData[EigenvectorVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *EigenvectorVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return EigenvectorVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *EigenvectorVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("EigenvectorVariable::accessorSearch");
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
			msg.print("Error: Type 'EigenVector&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("EigenvectorVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'EigenVector&' function '%s'.\n", s);
			msg.exit("EigenvectorVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::EigenvectorData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'EigenVector&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
			msg.print("Error: Argument list given to 'EigenVector&' array member '%s'.\n", s);
			msg.exit("EigenvectorVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::EigenvectorData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("EigenvectorVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool EigenvectorVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("EigenvectorVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Eigenvector type.\n", i);
		msg.exit("EigenvectorVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("EigenvectorVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("EigenvectorVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Eigenvector *ptr = (Eigenvector*) rv.asPointer(VTypes::EigenvectorData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::EigenvectorData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (EigenvectorVariable::Eigenvalue):
			rv.set(ptr->eigenvalue());
			break;
		case (EigenvectorVariable::Occupancy):
			rv.set(ptr->occupancy());
			break;
		case (EigenvectorVariable::Name):
			rv.set(ptr->name());
			break;
		case (EigenvectorVariable::Size):
			rv.set(ptr->size());
			break;
		case (EigenvectorVariable::Vector):
			if (hasArrayIndex) rv.set(ptr->value(arrayIndex-1));
			else rv.setArray(VTypes::DoubleData, ptr->eigenvector(), ptr->size());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in EigenVectorVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("EigenVectorVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool EigenvectorVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("EigenvectorVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Eigenvector type.\n", i);
		msg.exit("EigenvectorVariable::setAccessor");
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
		msg.exit("EigenvectorVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	Eigenvector *ptr = (Eigenvector*) sourcerv.asPointer(VTypes::EigenvectorData, result);
	int n;
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::EigenvectorData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (EigenvectorVariable::Eigenvalue):
			ptr->setEigenvalue( newvalue.asDouble() );
			break;
		case (EigenvectorVariable::Name):
			ptr->setName( newvalue.asString() );
			break;
		case (EigenvectorVariable::Occupancy):
			ptr->setOccupancy( newvalue.asDouble() );
			break;
		case (EigenvectorVariable::Size):
			ptr->initialise( newvalue.asInteger() );
			break;
		case (EigenvectorVariable::Vector):
			if ((newvalue.arraySize() != -1) && (newvalue.arraySize() <= ptr->size())) for (n=0; n<newvalue.arraySize(); ++n) ptr->setValue(n, newvalue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setValue(arrayIndex-1, newvalue.asDouble());
			else for (n=0; n<MAXFFPARAMDATA; ++n) ptr->setValue(n, newvalue.asDouble());
			break;
		default:
			printf("EigenvectorVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("EigenvectorVariable::setAccessor");
	return result;
}

// Perform desired function
bool EigenvectorVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("EigenvectorVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Eigenvector type.\n", i);
		msg.exit("EigenvectorVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Eigenvector *ptr = (Eigenvector*) rv.asPointer(VTypes::EigenvectorData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in EigenvectorVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("EigenvectorVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void EigenvectorVariable::printAccessors()
{
	if (EigenvectorVariable::nAccessors > 0)
	{
		msg.print("Valid accessors are:\n");
		for (int n=0; n<EigenvectorVariable::nAccessors; ++n) msg.print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		msg.print("\n");
	}
	if ((EigenvectorVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		msg.print("Valid functions are:\n");
		for (int n=0; n<EigenvectorVariable::nFunctions; ++n) msg.print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		msg.print("\n");
	}
}

/*
// Variable Array
*/

// Constructor
EigenvectorArrayVariable::EigenvectorArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::EigenvectorData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *EigenvectorArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return EigenvectorVariable::accessorSearch(s, arrayindex, arglist);
}

