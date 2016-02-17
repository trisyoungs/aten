/*
	*** ZMatrix Variable and Array
	*** src/parser/zmatrix.cpp
	Copyright T. Youngs 2007-2016

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

#include "parser/zmatrix.h"
#include "parser/stepnode.h"
#include "base/zmatrix.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

/*
 * Variable
 */

// Constructor
ZMatrixVariable::ZMatrixVariable(ZMatrix* ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ZMatrixData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
ZMatrixVariable::~ZMatrixVariable()
{
}

/*
 * Accessors
 */

// Accessor data
Accessor ZMatrixVariable::accessorData[ZMatrixVariable::nAccessors] = {
	{ "angleNames",		VTypes::StringData,		-1, false },
	{ "angles",		VTypes::DoubleData,		-1, false },
	{ "distanceNames",	VTypes::StringData,		-1, false },
	{ "distances",		VTypes::DoubleData,		-1, false },
	{ "elements",		VTypes::ZMatrixElementData,	-1, true },
	{ "nAngles",		VTypes::IntegerData,		0, true },
	{ "nDistances",		VTypes::IntegerData,		0, true },
	{ "nElements",		VTypes::IntegerData,		0, true },
	{ "nTorsions",		VTypes::IntegerData,		0, true },
	{ "torsionNames",	VTypes::StringData,		-1, false },
	{ "torsions",		VTypes::DoubleData,		-1, false },
};

// Function data
FunctionAccessor ZMatrixVariable::functionData[ZMatrixVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* ZMatrixVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return ZMatrixVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* ZMatrixVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("ZMatrixVariable::accessorSearch");
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
			Messenger::print("Error: Type 'ZMatrix&' has no member or function named '%s'.", qPrintable(name));
			printValidAccessors(nAccessors, accessorData, nFunctions, functionData);
			Messenger::exit("ZMatrixVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, qPrintable(functionData[i].name));
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'ZMatrix&' function named '%s'.", qPrintable(name));
			Messenger::exit("ZMatrixVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ZMatrixData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, qPrintable(functionData[i].name)))
		{
			Messenger::print("Error: Syntax for 'ZMatrix&' function '%s' is '%s(%s)'.", qPrintable(functionData[i].name), qPrintable(functionData[i].name), qPrintable(functionData[i].argText) );
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
			Messenger::print("Error: Argument list given to 'ZMatrix&' array member '%s'.", qPrintable(name));
			Messenger::exit("ZMatrixVariable::accessorSearch");
			return NULL;
		}
		result = new StepNode(i, VTypes::ZMatrixData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("ZMatrixVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ZMatrixVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ZMatrixVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ZMatrix type.\n", i);
		Messenger::exit("ZMatrixVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", qPrintable(accessorData[i].name));
		Messenger::exit("ZMatrixVariable::retrieveAccessor");
		return false;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", qPrintable(accessorData[i].name), arrayIndex, accessorData[i].arraySize);
			Messenger::exit("ZMatrixVariable::retrieveAccessor");
			return false;
		}
	}
	// Get current data from ReturnValue
	bool result = true;
	ReturnValue temprv;
	ZMatrix* ptr = (ZMatrix*) rv.asPointer(VTypes::ZMatrixData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ZMatrixData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (ZMatrixVariable::AngleNames):
			if (!hasArrayIndex)
			{
				if (ptr->angles() != NULL) rv.set(ptr->angle(0)->name());
				else
				{
					Messenger::print("No angle data in ZMatrix to return.");
					result = false;
				}
			}
			else if (arrayIndex > ptr->nAngles())
			{
				Messenger::print("Angle name array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = false;
			}
			else rv.set(ptr->angle(arrayIndex-1)->name());
			break;
		case (ZMatrixVariable::Angles):
			if (!hasArrayIndex)
			{
				if (ptr->angles() != NULL)
				{
					ptr->angles()->execute(temprv);
					rv.set(temprv.asDouble());
				}
				else
				{
					Messenger::print("No angle data in ZMatrix to return.");
					result = false;
				}
			}
			else if (arrayIndex > ptr->nAngles())
			{
				Messenger::print("Angle value array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = false;
			}
			else
			{
				ptr->angle(arrayIndex-1)->execute(temprv);
				rv.set(temprv.asDouble());
			}
			break;
		case (ZMatrixVariable::DistanceNames):
			if (!hasArrayIndex)
			{
				if (ptr->distances() != NULL) rv.set(ptr->distance(0)->name());
				else
				{
					Messenger::print("No distance data in ZMatrix to return.");
					result = false;
				}
			}
			else if (arrayIndex > ptr->nDistances())
			{
				Messenger::print("Distance name array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = false;
			}
			else rv.set(ptr->distance(arrayIndex-1)->name());
			break;
		case (ZMatrixVariable::Distances):
			if (!hasArrayIndex)
			{
				if (ptr->distances() != NULL)
				{
					ptr->distances()->execute(temprv);
					rv.set(temprv.asDouble());
				}
				else
				{
					Messenger::print("No distance data in ZMatrix to return.");
					result = false;
				}
			}
			else if (arrayIndex > ptr->nDistances())
			{
				Messenger::print("Distance value array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = false;
			}
			else
			{
				ptr->distance(arrayIndex-1)->execute(temprv);
				rv.set(temprv.asDouble());
			}
			break;
		case (ZMatrixVariable::Elements):
			if (!hasArrayIndex) rv.set(VTypes::ZMatrixElementData, ptr->elements());
			else if (arrayIndex > ptr->nElements())
			{
				Messenger::print("Element array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = false;
			}
			else rv.set(VTypes::ZMatrixElementData, ptr->element(arrayIndex-1));
			break;
		case (ZMatrixVariable::NAngles):
			rv.set( ptr->nAngles() );
			break;
		case (ZMatrixVariable::NDistances):
			rv.set( ptr->nDistances() );
			break;
		case (ZMatrixVariable::NElements):
			rv.set( ptr->nElements() );
			break;
		case (ZMatrixVariable::NTorsions):
			rv.set( ptr->nTorsions() );
			break;
		case (ZMatrixVariable::TorsionNames):
			if (!hasArrayIndex)
			{
				if (ptr->torsions() != NULL) rv.set(ptr->torsion(0)->name());
				else
				{
					Messenger::print("No torsion data in ZMatrix to return.");
					result = false;
				}
			}
			else if (arrayIndex > ptr->nTorsions())
			{
				Messenger::print("Torsion name array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = false;
			}
			else rv.set(ptr->torsion(arrayIndex-1)->name());
			break;
		case (ZMatrixVariable::Torsions):
			if (!hasArrayIndex)
			{
				if (ptr->torsions() != NULL)
				{
					ptr->torsions()->execute(temprv);
					rv.set(temprv.asDouble());
				}
				else
				{
					Messenger::print("No torsion data in ZMatrix to return.");
					result = false;
				}
			}
			else if (arrayIndex > ptr->nTorsions())
			{
				Messenger::print("Torsion value array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = false;
			}
			else
			{
				ptr->torsion(arrayIndex-1)->execute(temprv);
				rv.set(temprv.asDouble());
			}
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ZMatrixVariable.\n", qPrintable(accessorData[i].name));
			result = false;
			break;
	}
	Messenger::exit("ZMatrixVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ZMatrixVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("ZMatrixVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ZMatrix type.\n", i);
		Messenger::exit("ZMatrixVariable::setAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;

	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = checkAccessorArrays(accessorData[acc], newValue, hasArrayIndex, arrayIndex);
	if (!result)
	{
		Messenger::exit("ZMatrixVariable::setAccessor");
		return false;
	}

	// Get current data from ReturnValue
	ZMatrix* ptr = (ZMatrix*) sourcerv.asPointer(VTypes::ZMatrixData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ZMatrixData));
		result = false;
	}
	if (result) switch (acc)
	{
		case (ZMatrixVariable::AngleNames):
			if (!hasArrayIndex)
			{
				Messenger::print("Can't set names of all angle variables in ZMatrix at once.");
				result = false;
			}
			else if (arrayIndex > ptr->nAngles())
			{
				Messenger::print("Angle name array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = false;
			}
			else ptr->angle(arrayIndex-1)->setName( newValue.asString() );
			break;
		case (ZMatrixVariable::Angles):
			if (!hasArrayIndex)
			{
				Messenger::print("Can't set values of all angle variables in ZMatrix at once.");
				result = false;
			}
			else if (arrayIndex > ptr->nAngles())
			{
				Messenger::print("Angle value array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = false;
			}
			else ptr->setVariable(ptr->angle(arrayIndex-1), newValue.asDouble());
			break;
		case (ZMatrixVariable::DistanceNames):
			if (!hasArrayIndex)
			{
				Messenger::print("Can't set names of all distance variables in ZMatrix at once.");
				result = false;
			}
			else if (arrayIndex > ptr->nDistances())
			{
				Messenger::print("Distance name array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = false;
			}
			else ptr->distance(arrayIndex-1)->setName( newValue.asString() );
			break;
		case (ZMatrixVariable::Distances):
			if (!hasArrayIndex)
			{
				Messenger::print("Can't set values of all distance variables in ZMatrix at once.");
				result = false;
			}
			else if (arrayIndex > ptr->nDistances())
			{
				Messenger::print("Distance value array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = false;
			}
			else ptr->setVariable(ptr->distance(arrayIndex-1), newValue.asDouble());
			break;
		case (ZMatrixVariable::TorsionNames):
			if (!hasArrayIndex)
			{
				Messenger::print("Can't set names of all torsion variables in ZMatrix at once.");
				result = false;
			}
			else if (arrayIndex > ptr->nTorsions())
			{
				Messenger::print("Torsion name array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = false;
			}
			else ptr->torsion(arrayIndex-1)->setName( newValue.asString() );
			break;
		case (ZMatrixVariable::Torsions):
			if (!hasArrayIndex)
			{
				Messenger::print("Can't set values of all torsion variables in ZMatrix at once.");
				result = false;
			}
			else if (arrayIndex > ptr->nTorsions())
			{
				Messenger::print("Torsion value array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = false;
			}
			else ptr->setVariable(ptr->torsion(arrayIndex-1), newValue.asDouble());
			break;
		default:
			printf("ZMatrixVariable::setAccessor doesn't know how to use member '%s'.\n", qPrintable(accessorData[acc].name));
			result = false;
			break;
	}
	Messenger::exit("ZMatrixVariable::setAccessor");
	return result;
}

// Perform desired function
bool ZMatrixVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("ZMatrixVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for ZMatrix type.\n", i);
		Messenger::exit("ZMatrixVariable::performFunction");
		return false;
	}

	// Get current data from ReturnValue
	bool result = true;
	ZMatrix* ptr = (ZMatrix*) rv.asPointer(VTypes::ZMatrixData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in ZMatrixVariable.\n", qPrintable(functionData[i].name));
			result = false;
			break;
	}
	Messenger::exit("ZMatrixVariable::performFunction");
	return result;
}

/*
 * Variable Array
 */

// Constructor
ZMatrixArrayVariable::ZMatrixArrayVariable(TreeNode* sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::ZMatrixData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode* ZMatrixArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return ZMatrixVariable::accessorSearch(name, arrayIndex, argList);
}
