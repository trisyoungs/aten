/*
	*** ZMatrixElement Variable and Array
	*** src/parser/zmatrixelement.cpp
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

#include "parser/zmatrixelement.h"
#include "parser/stepnode.h"
#include "classes/zmatrix.h"
#include "model/model.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
// Variable
*/

// Constructor
ZMatrixElementVariable::ZMatrixElementVariable(ZMatrixElement *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ZMatrixElementData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
ZMatrixElementVariable::~ZMatrixElementVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor ZMatrixElementVariable::accessorData[ZMatrixElementVariable::nAccessors] = {
	{ "angle",		VTypes::DoubleData,	0, FALSE },
	{ "angleatom",		VTypes::AtomData,	0, TRUE },
	{ "anglename",		VTypes::StringData,	0, FALSE },
	{ "atom",		VTypes::AtomData,	4, TRUE },
	{ "distance",		VTypes::DoubleData,	0, FALSE },
	{ "distanceatom",	VTypes::AtomData,	0, TRUE },
	{ "distancename",	VTypes::StringData,	0, FALSE },
	{ "negateangle", 	VTypes::IntegerData,	0, FALSE },
	{ "negatedistance", 	VTypes::IntegerData,	0, FALSE },
	{ "negatetorsion", 	VTypes::IntegerData,	0, FALSE },
	{ "targetatom",		VTypes::AtomData,	0, TRUE },
	{ "torsion",		VTypes::DoubleData,	0, FALSE },
	{ "torsionatom",	VTypes::AtomData,	0, TRUE },
	{ "torsionname",	VTypes::StringData,	0, FALSE }
};

// Function data
FunctionAccessor ZMatrixElementVariable::functionData[ZMatrixElementVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *ZMatrixElementVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return ZMatrixElementVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *ZMatrixElementVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("ZMatrixElementVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		// No accessor found - is it a function definition?
		for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		if (i == nFunctions)
		{
			msg.print("Error: Type 'zmatrixelement&' has no member or function named '%s'.\n", s);
			printAccessors();
			msg.exit("ZMatrixElementVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'zmatrixelement&' function '%s'.\n", s);
			msg.exit("ZMatrixElementVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ZMatrixElementData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'zmatrixelement&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		else result = new StepNode(i, VTypes::ZMatrixElementData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("ZMatrixElementVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ZMatrixElementVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ZMatrixElementVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ZMatrixElement type.\n", i);
		msg.exit("ZMatrixElementVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("ZMatrixElementVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("ZMatrixElementVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	ZMatrixElement *ptr= (ZMatrixElement*) rv.asPointer(VTypes::ZMatrixElementData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::ZMatrixElementData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (ZMatrixElementVariable::Angle):
			rv.set( ptr->angle() );
			break;
		case (ZMatrixElementVariable::AngleAtom):
			rv.set(VTypes::AtomData, ptr->atom(2));
			break;
		case (ZMatrixElementVariable::AngleName):
			rv.set( ptr->angleName() );
			break;
		case (ZMatrixElementVariable::AtomData):
			rv.set(VTypes::AtomData, ptr->atom(arrayIndex-1));
			break;
		case (ZMatrixElementVariable::Distance):
			rv.set( ptr->distance() );
			break;
		case (ZMatrixElementVariable::DistanceAtom):
			rv.set(VTypes::AtomData, ptr->atom(1));
			break;
		case (ZMatrixElementVariable::DistanceName):
			rv.set( ptr->distanceName() );
			break;
		case (ZMatrixElementVariable::NegateAngle):
			rv.set( ptr->negated(1) );
			break;
		case (ZMatrixElementVariable::NegateDistance):
			rv.set( ptr->negated(0) );
			break;
		case (ZMatrixElementVariable::NegateTorsion):
			rv.set( ptr->negated(2) );
			break;
		case (ZMatrixElementVariable::TargetAtom):
			rv.set(VTypes::AtomData, ptr->atom(0));
			break;
		case (ZMatrixElementVariable::Torsion):
			rv.set( ptr->torsion() );
			break;
		case (ZMatrixElementVariable::TorsionAtom):
			rv.set(VTypes::AtomData, ptr->atom(3));
			break;
		case (ZMatrixElementVariable::TorsionName):
			rv.set( ptr->torsionName() );
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ZMatrixElementVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ZMatrixElementVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ZMatrixElementVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ZMatrixElementVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ZMatrixElement type.\n", i);
		msg.exit("ZMatrixElementVariable::setAccessor");
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
			if ((newvalue.arraySize() > 0) && (accessorData[i].returnType != VTypes::VectorData))
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
		msg.exit("ZMatrixElementVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	ZMatrixElement *ptr= (ZMatrixElement*) sourcerv.asPointer(VTypes::ZMatrixElementData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::ZMatrixElementData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (ZMatrixElementVariable::Angle):
			ptr->setAngle( newvalue.asDouble() );
			break;
		case (ZMatrixElementVariable::AngleName):
			ptr->setAngleName( newvalue.asString() );
			break;
		case (ZMatrixElementVariable::Distance):
			ptr->setDistance( newvalue.asDouble() );
			break;
		case (ZMatrixElementVariable::DistanceName):
			ptr->setDistanceName( newvalue.asString() );
			break;
		case (ZMatrixElementVariable::NegateAngle):
			ptr->setNegated(1, newvalue.asBool() );
			break;
		case (ZMatrixElementVariable::NegateDistance):
			ptr->setNegated(0, newvalue.asBool() );
			break;
		case (ZMatrixElementVariable::NegateTorsion):
			ptr->setNegated(2, newvalue.asBool() );
			break;
		case (ZMatrixElementVariable::Torsion):
			ptr->setTorsion( newvalue.asDouble() );
			break;
		case (ZMatrixElementVariable::TorsionName):
			ptr->setTorsionName( newvalue.asString() );
			break;
		default:
			printf("ZMatrixElementVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("ZMatrixElementVariable::setAccessor");
	return result;
}

// Perform desired function
bool ZMatrixElementVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("ZMatrixElementVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for ZMatrixElement type.\n", i);
		msg.exit("ZMatrixElementVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	ZMatrixElement *ptr= (ZMatrixElement*) rv.asPointer(VTypes::ZMatrixElementData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in ZMatrixElementVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ZMatrixElementVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void ZMatrixElementVariable::printAccessors()
{
	if (ZMatrixElementVariable::nAccessors > 0)
	{
		msg.print("Valid accessors are:\n");
		for (int n=0; n<ZMatrixElementVariable::nAccessors; ++n) msg.print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		msg.print("\n");
	}
	if ((ZMatrixElementVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		msg.print("Valid functions are:\n");
		for (int n=0; n<ZMatrixElementVariable::nFunctions; ++n) msg.print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		msg.print("\n");
	}
}

/*
// Variable Array
*/

// Constructor
ZMatrixElementArrayVariable::ZMatrixElementArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ZMatrixElementData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *ZMatrixElementArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return ZMatrixElementVariable::accessorSearch(s, arrayindex, arglist);
}
