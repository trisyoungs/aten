/*
	*** BasisShell Variable and Array
	*** src/parser/basisshell.cpp
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

#include "parser/basisshell.h"
#include "parser/stepnode.h"
#include "classes/basisshell.h"
#include "base/constants.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
// Variable
*/

// Constructor
BasisShellVariable::BasisShellVariable(BasisShell *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::BasisShellData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
BasisShellVariable::~BasisShellVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor BasisShellVariable::accessorData[BasisShellVariable::nAccessors] = {
	{ "atomId",		VTypes::IntegerData,		0, FALSE },
	{ "nPrimitives",	VTypes::IntegerData,		0, TRUE },
	{ "primitives",		VTypes::BasisPrimitiveData,	-1, TRUE },
	{ "type",		VTypes::StringData,		0, FALSE }
};

// Function data
FunctionAccessor BasisShellVariable::functionData[BasisShellVariable::nFunctions] = {
	{ "addPrimitive",	VTypes::BasisPrimitiveData,	"Nn*",	"double exponent, double c1 = 0.0 ..." }
};

// Search variable access list for provided accessor (call private static function)
StepNode *BasisShellVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return BasisShellVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *BasisShellVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("BasisShellVariable::accessorSearch");
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
			msg.print("Error: Type 'basisshell&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("BasisShellVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'basisshell&' function '%s'.\n", s);
			msg.exit("BasisShellVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::BasisShellData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'basisshell&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		else result = new StepNode(i, VTypes::BasisShellData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("BasisShellVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool BasisShellVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("BasisShellVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for BasisShell type.\n", i);
		msg.exit("BasisShellVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("BasisShellVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("BasisShellVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	BasisShell *ptr = (BasisShell*) rv.asPointer(VTypes::BasisShellData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::BasisShellData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (BasisShellVariable::AtomId):
			rv.set(ptr->atomId()+1);
			break;
		case (BasisShellVariable::Primitives):
			if ((arrayIndex < 1) || (arrayIndex > ptr->nPrimitives()))
			{
				msg.print("Array index [%i] is out of range for 'primitives' member.\n", arrayIndex);
				result = FALSE;
			}
			else rv.set(VTypes::BasisPrimitiveData, ptr->primitive(arrayIndex-1));
			break;
		case (BasisShellVariable::Type):
			rv.set(BasisShell::basisShellType(ptr->type()));
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in BasisShellVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("BasisShellVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool BasisShellVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("BasisShellVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for BasisShell type.\n", i);
		msg.exit("BasisShellVariable::setAccessor");
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
		msg.exit("BasisShellVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	BasisShell *ptr = (BasisShell*) sourcerv.asPointer(VTypes::BasisShellData, result);
	if ((!result) || (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::BasisShellData));
		result = FALSE;
	}
	BasisShell::BasisShellType bft;
	if (result) switch (acc)
	{
		case (BasisShellVariable::AtomId):
			ptr->setAtomId( newvalue.asInteger() - 1);
			break;
		case (BasisShellVariable::Type):
			bft = BasisShell::basisShellType( newvalue.asString() );
			if (bft == BasisShell::nBasisShellTypes) result = FALSE;
			else ptr->setType(bft);
			break;
		default:
			printf("BasisShellVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("BasisShellVariable::setAccessor");
	return result;
}

// Perform desired function
bool BasisShellVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("BasisShellVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for BasisShell type.\n", i);
		msg.exit("BasisShellVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	BasisShell *ptr = (BasisShell*) rv.asPointer(VTypes::BasisShellData, result);
	BasisPrimitive *prim;
	int n;
	if (result) switch (i)
	{
		case (BasisShellVariable::AddPrimitive):
			prim = ptr->addPrimitive();
			prim->setExponent( node->argd(0) );
			for (n=1; n<node->nArgs(); ++n) prim->addCoefficient(node->argd(n));
			rv.set(VTypes::BasisPrimitiveData, prim);
			break;
		default:
			printf("Internal Error: Access to function '%s' has not been defined in BasisShellVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("BasisShellVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void BasisShellVariable::printAccessors()
{
	if (BasisShellVariable::nAccessors > 0)
	{
		msg.print("Valid accessors are:\n");
		for (int n=0; n<BasisShellVariable::nAccessors; ++n) msg.print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		msg.print("\n");
	}
	if ((BasisShellVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		msg.print("Valid functions are:\n");
		for (int n=0; n<BasisShellVariable::nFunctions; ++n) msg.print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		msg.print("\n");
	}
}

/*
// Variable Array
*/

// Constructor
BasisShellArrayVariable::BasisShellArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::BasisShellData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *BasisShellArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return BasisShellVariable::accessorSearch(s, arrayindex, arglist);
}

