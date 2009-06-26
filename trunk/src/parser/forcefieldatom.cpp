/*
	*** ForcefieldAtom Variable and Array
	*** src/parser/forcefieldatom.cpp
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

#include "parser/forcefieldatom.h"
#include "parser/stepnode.h"
#include "classes/forcefieldatom.h"
#include "base/constants.h"
#include <string.h>

/*
// Variable
*/

// Constructor
ForcefieldAtomVariable::ForcefieldAtomVariable(ForcefieldAtom *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ForcefieldAtomData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
ForcefieldAtomVariable::~ForcefieldAtomVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor ForcefieldAtomVariable::accessorData[ForcefieldAtomVariable::nAccessors] = {
	{ "charge",		VTypes::DoubleData,		0, FALSE },
	{ "data",		VTypes::DoubleData,		MAXFFPARAMDATA, FALSE },
	{ "description",	VTypes::StringData,		0, FALSE },
	{ "equivalent",		VTypes::StringData,		0, FALSE },
	{ "form",		VTypes::StringData,		0, FALSE },
	{ "id",			VTypes::IntegerData,		0, TRUE },
	{ "name",		VTypes::StringData,		0, FALSE },
	{ "neta",		VTypes::StringData,		0, FALSE },
	{ "ff",			VTypes::ForcefieldData,		0, TRUE }
};

// Function data
FunctionAccessor ForcefieldAtomVariable::functionData[ForcefieldAtomVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *ForcefieldAtomVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return ForcefieldAtomVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *ForcefieldAtomVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("ForcefieldAtomVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		// No accessor found - is it a function definition?
		for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		if (i == nFunctions)
		{
			msg.print("Error: Type 'ffatom&' has no member or function named '%s'.\n", s);
			msg.exit("ForcefieldAtomVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'ffatom&' function '%s'.\n", s);
			msg.exit("ForcefieldAtomVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ForcefieldAtomData, functionData[i].returnType);
		result->addArgumentList(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'ffatom&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		else result = new StepNode(i, VTypes::ForcefieldAtomData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("ForcefieldAtomVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ForcefieldAtomVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ForcefieldAtomVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ForcefieldAtom type.\n", i);
		msg.exit("ForcefieldAtomVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("ForcefieldAtomVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("ForcefieldAtomVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	ForcefieldAtom *ptr= (ForcefieldAtom*) rv.asPointer(VTypes::ForcefieldAtomData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::ForcefieldAtomData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (ForcefieldAtomVariable::Charge):
			rv.set(ptr->charge());
			break;
		case (ForcefieldAtomVariable::Data):
			if ((arrayIndex < 1) || (arrayIndex > MAXFFPARAMDATA))
			{
				msg.print("Array index [%i] is out of range for 'data' member.\n", arrayIndex);
				result = FALSE;
			}
			else rv.set(ptr->parameter(arrayIndex-1));
			break;
		case (ForcefieldAtomVariable::Description):
			rv.set(ptr->description());
			break;
		case (ForcefieldAtomVariable::Equivalent):
			rv.set(ptr->equivalent());
			break;
		case (ForcefieldAtomVariable::Form):
			rv.set(VdwFunctions::VdwFunctions[ptr->vdwForm()].keyword);
			break;
		case (ForcefieldAtomVariable::Id):
			rv.set(ptr->typeId());
			break;
		case (ForcefieldAtomVariable::Name):
			rv.set(ptr->name());
			break;
		case (ForcefieldAtomVariable::Neta):
			rv.set(ptr->netaString());
			break;
		case (ForcefieldAtomVariable::ParentFF):
			rv.set(VTypes::ForcefieldData, ptr->parent());
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ForcefieldAtomVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ForcefieldAtomVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ForcefieldAtomVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ForcefieldAtomVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ForcefieldAtom type.\n", i);
		msg.exit("ForcefieldAtomVariable::retrieveAccessor");
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
		msg.exit("ForcefieldAtomVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	VdwFunctions::VdwFunction vf;
	ForcefieldAtom *ptr= (ForcefieldAtom*) sourcerv.asPointer(VTypes::ForcefieldAtomData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::ForcefieldAtomData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (ForcefieldAtomVariable::Charge):
			ptr->setCharge(newvalue.asDouble());
			break;
		case (ForcefieldAtomVariable::Data):
			ptr->setParameter(arrayIndex-1, newvalue.asDouble());
			break;
		case (ForcefieldAtomVariable::Description):
			ptr->setDescription(newvalue.asString());
			break;
		case (ForcefieldAtomVariable::Equivalent):
			ptr->setEquivalent(newvalue.asString());
			break;
		case (ForcefieldAtomVariable::Form):
			vf = VdwFunctions::vdwFunction(newvalue.asString());
			if (vf == VdwFunctions::None) result = FALSE;
			else ptr->setVdwForm(vf);
			break;
		case (ForcefieldAtomVariable::Name):
			ptr->setName(newvalue.asString());
			break;
		case (ForcefieldAtomVariable::Neta):
			ptr->setNeta(newvalue.asString(), NULL, ptr);
			break;
		default:
			printf("ForcefieldAtomVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("ForcefieldAtomVariable::setAccessor");
	return result;
}

// Perform desired function
bool ForcefieldAtomVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("ForcefieldAtomVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for ForcefieldAtom type.\n", i);
		msg.exit("ForcefieldAtomVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	ForcefieldAtom *ptr= (ForcefieldAtom*) rv.asPointer(VTypes::ForcefieldAtomData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in ForcefieldAtomVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ForcefieldAtomVariable::performFunction");
	return result;
}

/*
// Variable Array
*/

// Constructor
ForcefieldAtomArrayVariable::ForcefieldAtomArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ForcefieldAtomData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *ForcefieldAtomArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return ForcefieldAtomVariable::accessorSearch(s, arrayindex, arglist);
}
