/*
	*** Eigenvector Variable and Array
	*** src/parser/eigenvector.cpp
	Copyright T. Youngs 2007-2018

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
#include "base/elementmap.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

ATEN_USING_NAMESPACE

/*
 * Variable
 */

// Constructor
EigenvectorVariable::EigenvectorVariable(Eigenvector* ptr, bool constant)
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
 * Accessors
 */

// Accessor data
Accessor EigenvectorVariable::accessorData[EigenvectorVariable::nAccessors] = {
	{ "eigenvalue",	VTypes::DoubleData,	0, false },
	{ "name",	VTypes::StringData,	0, false },
	{ "occupancy",	VTypes::DoubleData,	0, false },
	{ "size",	VTypes::IntegerData,	0, true },
	{ "vector",	VTypes::IntegerData,	-1, false }
};

// Function data
FunctionAccessor EigenvectorVariable::functionData[EigenvectorVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* EigenvectorVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return EigenvectorVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* EigenvectorVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("EigenvectorVariable::accessorSearch");
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
			Messenger::print("Error: Type 'EigenVector&' has no member or function named '%s'.", qPrintable(name));
			printValidAccessors(nAccessors, accessorData, nFunctions, functionData);
			Messenger::exit("EigenvectorVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, qPrintable(functionData[i].name));
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'EigenVector&' function named '%s'.", qPrintable(name));
			Messenger::exit("EigenvectorVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::EigenvectorData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, qPrintable(functionData[i].name)))
		{
			Messenger::print("Error: Syntax for 'EigenVector&' function '%s' is '%s(%s)'.", qPrintable(functionData[i].name), qPrintable(functionData[i].name), qPrintable(functionData[i].argText) );
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
			Messenger::print("Error: Argument list given to 'EigenVector&' array member '%s'.", qPrintable(name));
			Messenger::exit("EigenvectorVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::EigenvectorData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("EigenvectorVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool EigenvectorVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("EigenvectorVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Eigenvector type.\n", i);
		Messenger::exit("EigenvectorVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", qPrintable(accessorData[i].name));
		Messenger::exit("EigenvectorVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", qPrintable(accessorData[i].name), arrayIndex, accessorData[i].arraySize);
			Messenger::exit("EigenvectorVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	Eigenvector* ptr = (Eigenvector*) rv.asPointer(VTypes::EigenvectorData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::EigenvectorData));
		result = false;
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
			printf("Internal Error: Access to member '%s' has not been defined in EigenVectorVariable.\n", qPrintable(accessorData[i].name));
			result = false;
			break;
	}
	Messenger::exit("EigenVectorVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool EigenvectorVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("EigenvectorVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Eigenvector type.\n", i);
		Messenger::exit("EigenvectorVariable::setAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;

	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = checkAccessorArrays(accessorData[acc], newValue, hasArrayIndex, arrayIndex);
	if (!result)
	{
		Messenger::exit("EigenvectorVariable::setAccessor");
		return false;
	}

	// Get current data from ReturnValue
	Eigenvector* ptr = (Eigenvector*) sourcerv.asPointer(VTypes::EigenvectorData, result);
	int n;
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::EigenvectorData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (EigenvectorVariable::Eigenvalue):
			ptr->setEigenvalue( newValue.asDouble() );
			break;
		case (EigenvectorVariable::Name):
			ptr->setName( newValue.asString() );
			break;
		case (EigenvectorVariable::Occupancy):
			ptr->setOccupancy( newValue.asDouble() );
			break;
		case (EigenvectorVariable::Size):
			ptr->initialise( newValue.asInteger() );
			break;
		case (EigenvectorVariable::Vector):
			if ((newValue.arraySize() != -1) && (newValue.arraySize() <= ptr->size())) for (n=0; n<newValue.arraySize(); ++n) ptr->setValue(n, newValue.asDouble(n, result));
			else if (hasArrayIndex) ptr->setValue(arrayIndex-1, newValue.asDouble());
			else for (n=0; n<MAXFFPARAMDATA; ++n) ptr->setValue(n, newValue.asDouble());
			break;
		default:
			printf("EigenvectorVariable::setAccessor doesn't know how to use member '%s'.\n", qPrintable(accessorData[acc].name));
			result = false;
			break;
	}
	Messenger::exit("EigenvectorVariable::setAccessor");
	return result;
}

// Perform desired function
bool EigenvectorVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("EigenvectorVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Eigenvector type.\n", i);
		Messenger::exit("EigenvectorVariable::performFunction");
		return false;
	}
	// Get current data from ReturnValue
	bool result = true;
	Eigenvector* ptr = (Eigenvector*) rv.asPointer(VTypes::EigenvectorData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in EigenvectorVariable.\n", qPrintable(functionData[i].name));
			result = false;
			break;
	}
	Messenger::exit("EigenvectorVariable::performFunction");
	return result;
}

/*
 * Variable Array
 */

// Constructor
EigenvectorArrayVariable::EigenvectorArrayVariable(TreeNode* sizeexpr, bool constant)
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
StepNode* EigenvectorArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return EigenvectorVariable::accessorSearch(name, arrayIndex, argList);
}

