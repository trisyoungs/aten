/*
	*** ZMatrixElement Variable and Array
	*** src/parser/zmatrixelement.cpp
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

#include "parser/zmatrixelement.h"
#include "parser/stepnode.h"
#include "base/zmatrix.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

/*
 * Variable
 */

// Constructor
ZMatrixElementVariable::ZMatrixElementVariable(ZMatrixElement* ptr, bool constant)
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
 * Accessors
 */

// Accessor data
Accessor ZMatrixElementVariable::accessorData[ZMatrixElementVariable::nAccessors] = {
	{ "angle",		VTypes::DoubleData,	0, false },
	{ "angleAtom",		VTypes::AtomData,	0, true },
	{ "angleName",		VTypes::StringData,	0, false },
	{ "atom",		VTypes::AtomData,	4, true },
	{ "distance",		VTypes::DoubleData,	0, false },
	{ "distanceAtom",	VTypes::AtomData,	0, true },
	{ "distanceName",	VTypes::StringData,	0, false },
	{ "negateAngle", 	VTypes::IntegerData,	0, false },
	{ "negateDistance", 	VTypes::IntegerData,	0, false },
	{ "negateTorsion", 	VTypes::IntegerData,	0, false },
	{ "targetAtom",		VTypes::AtomData,	0, true },
	{ "torsion",		VTypes::DoubleData,	0, false },
	{ "torsionAtom",	VTypes::AtomData,	0, true },
	{ "torsionName",	VTypes::StringData,	0, false }
};

// Function data
FunctionAccessor ZMatrixElementVariable::functionData[ZMatrixElementVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* ZMatrixElementVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return ZMatrixElementVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* ZMatrixElementVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("ZMatrixElementVariable::accessorSearch");
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
			Messenger::print("Error: Type 'ZMatrixElement&' has no member or function named '%s'.", qPrintable(name));
			printValidAccessors(nAccessors, accessorData, nFunctions, functionData);
			Messenger::exit("ZMatrixElementVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, qPrintable(functionData[i].name));
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'ZMatrixElement&' function named '%s'.", qPrintable(name));
			Messenger::exit("ZMatrixElementVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ZMatrixElementData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, qPrintable(functionData[i].name)))
		{
			Messenger::print("Error: Syntax for 'ZMatrixElement&' function '%s' is '%s(%s)'.", qPrintable(functionData[i].name), qPrintable(functionData[i].name), qPrintable(functionData[i].argText) );
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
			Messenger::print("Error: Argument list given to 'ZMatrixElement&' array member '%s'.", qPrintable(name));
			Messenger::exit("ZMatrixElementVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::ZMatrixElementData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("ZMatrixElementVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ZMatrixElementVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ZMatrixElementVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ZMatrixElement type.\n", i);
		Messenger::exit("ZMatrixElementVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", qPrintable(accessorData[i].name));
		Messenger::exit("ZMatrixElementVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", qPrintable(accessorData[i].name), arrayIndex, accessorData[i].arraySize);
			Messenger::exit("ZMatrixElementVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	ZMatrixElement* ptr = (ZMatrixElement*) rv.asPointer(VTypes::ZMatrixElementData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ZMatrixElementData));
		result = false;
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
			printf("Internal Error: Access to member '%s' has not been defined in ZMatrixElementVariable.\n", qPrintable(accessorData[i].name));
			result = false;
			break;
	}
	Messenger::exit("ZMatrixElementVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ZMatrixElementVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ZMatrixElementVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ZMatrixElement type.\n", i);
		Messenger::exit("ZMatrixElementVariable::setAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;

	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = checkAccessorArrays(accessorData[acc], newValue, hasArrayIndex, arrayIndex);
	if (!result)
	{
		Messenger::exit("ZMatrixElementVariable::setAccessor");
		return false;
	}

	// Get current data from ReturnValue
	ZMatrixElement* ptr = (ZMatrixElement*) sourcerv.asPointer(VTypes::ZMatrixElementData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ZMatrixElementData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (ZMatrixElementVariable::Angle):
			ptr->setAngle( newValue.asDouble() );
			break;
		case (ZMatrixElementVariable::AngleName):
			ptr->setAngleName( newValue.asString() );
			break;
		case (ZMatrixElementVariable::Distance):
			ptr->setDistance( newValue.asDouble() );
			break;
		case (ZMatrixElementVariable::DistanceName):
			ptr->setDistanceName( newValue.asString() );
			break;
		case (ZMatrixElementVariable::NegateAngle):
			ptr->setNegated(1, newValue.asBool() );
			break;
		case (ZMatrixElementVariable::NegateDistance):
			ptr->setNegated(0, newValue.asBool() );
			break;
		case (ZMatrixElementVariable::NegateTorsion):
			ptr->setNegated(2, newValue.asBool() );
			break;
		case (ZMatrixElementVariable::Torsion):
			ptr->setTorsion( newValue.asDouble() );
			break;
		case (ZMatrixElementVariable::TorsionName):
			ptr->setTorsionName( newValue.asString() );
			break;
		default:
			printf("ZMatrixElementVariable::setAccessor doesn't know how to use member '%s'.", qPrintable(accessorData[acc].name));
			result = false;
			break;
	}
	Messenger::exit("ZMatrixElementVariable::setAccessor");
	return result;
}

// Perform desired function
bool ZMatrixElementVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("ZMatrixElementVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for ZMatrixElement type.\n", i);
		Messenger::exit("ZMatrixElementVariable::performFunction");
		return false;
	}

	// Get current data from ReturnValue
	bool result = true;
	ZMatrixElement* ptr = (ZMatrixElement*) rv.asPointer(VTypes::ZMatrixElementData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in ZMatrixElementVariable.\n", qPrintable(functionData[i].name));
			result = false;
			break;
	}
	Messenger::exit("ZMatrixElementVariable::performFunction");
	return result;
}

/*
 * Variable Array
 */

// Constructor
ZMatrixElementArrayVariable::ZMatrixElementArrayVariable(TreeNode* sizeexpr, bool constant)
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
StepNode* ZMatrixElementArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return ZMatrixElementVariable::accessorSearch(name, arrayIndex, argList);
}
