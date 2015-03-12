/*
	*** ZMatrix Variable and Array
	*** src/parser/zmatrix.cpp
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

#include "parser/zmatrix.h"
#include "parser/stepnode.h"
#include "base/zmatrix.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

/*
// Variable
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
// Accessors
*/

// Accessor data
Accessor ZMatrixVariable::accessorData[ZMatrixVariable::nAccessors] = {
	{ "angleNames",		VTypes::StringData,		-1, FALSE },
	{ "angles",		VTypes::DoubleData,		-1, FALSE },
	{ "distanceNames",	VTypes::StringData,		-1, FALSE },
	{ "distances",		VTypes::DoubleData,		-1, FALSE },
	{ "elements",		VTypes::ZMatrixElementData,	-1, TRUE },
	{ "nAngles",		VTypes::IntegerData,		0, TRUE },
	{ "nDistances",		VTypes::IntegerData,		0, TRUE },
	{ "nElements",		VTypes::IntegerData,		0, TRUE },
	{ "nTorsions",		VTypes::IntegerData,		0, TRUE },
	{ "torsionNames",	VTypes::StringData,		-1, FALSE },
	{ "torsions",		VTypes::DoubleData,		-1, FALSE },
};

// Function data
FunctionAccessor ZMatrixVariable::functionData[ZMatrixVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* ZMatrixVariable::findAccessor(const char* s, TreeNode* arrayIndex, TreeNode* argList)
{
	return ZMatrixVariable::accessorSearch(s, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* ZMatrixVariable::accessorSearch(const char* s, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("ZMatrixVariable::accessorSearch");
	StepNode* result = NULL;
	int i = 0;
	i = Variable::searchAccessor(s, nAccessors, accessorData);
	if (i == -1)
	{
		// No accessor found - is it a function definition?
		// for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		i = Variable::searchAccessor(s, nFunctions, functionData);
		if (i == -1)
		{
			Messenger::print("Error: Type 'ZMatrix&' has no member or function named '%s'.", s);
			printAccessors();
			Messenger::exit("ZMatrixVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, functionData[i].name);
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'ZMatrix&' function '%s'.", s);
			Messenger::exit("ZMatrixVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::ZMatrixData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			Messenger::print("Error: Syntax for 'ZMatrix&' function '%s' is '%s(%s)'.", functionData[i].name, functionData[i].name, functionData[i].argText );
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
			Messenger::print("Error: Argument list given to 'ZMatrix&' array member '%s'.", s);
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
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", accessorData[i].name);
		Messenger::exit("ZMatrixVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			Messenger::print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			Messenger::exit("ZMatrixVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	ReturnValue temprv;
	ZMatrix* ptr = (ZMatrix*) rv.asPointer(VTypes::ZMatrixData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ZMatrixData));
		result = FALSE;
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
					result = FALSE;
				}
			}
			else if (arrayIndex > ptr->nAngles())
			{
				Messenger::print("Angle name array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = FALSE;
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
					result = FALSE;
				}
			}
			else if (arrayIndex > ptr->nAngles())
			{
				Messenger::print("Angle value array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = FALSE;
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
					result = FALSE;
				}
			}
			else if (arrayIndex > ptr->nDistances())
			{
				Messenger::print("Distance name array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = FALSE;
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
					result = FALSE;
				}
			}
			else if (arrayIndex > ptr->nDistances())
			{
				Messenger::print("Distance value array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = FALSE;
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
				result = FALSE;
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
					result = FALSE;
				}
			}
			else if (arrayIndex > ptr->nTorsions())
			{
				Messenger::print("Torsion name array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = FALSE;
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
					result = FALSE;
				}
			}
			else if (arrayIndex > ptr->nTorsions())
			{
				Messenger::print("Torsion value array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = FALSE;
			}
			else
			{
				ptr->torsion(arrayIndex-1)->execute(temprv);
				rv.set(temprv.asDouble());
			}
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ZMatrixVariable.\n", accessorData[i].name);
			result = FALSE;
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
				Messenger::print("Error: Array index provided for member '%s' is out of range (%i, range is 1-%i).", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
				result = FALSE;
			}
			if ((newValue.arraySize() > 0) && (accessorData[i].returnType != VTypes::VectorData))
			{
				Messenger::print("Error: An array can't be assigned to the single valued member '%s'.", accessorData[i].name);
				result = FALSE;
			}
		}
		else
		{
			if (newValue.arraySize() > accessorData[i].arraySize)
			{
				Messenger::print("Error: The array being assigned to member '%s' is larger than the size of the desination array (%i cf. %i).", accessorData[i].name, newValue.arraySize(), accessorData[i].arraySize);
				result = FALSE;
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
				result = FALSE;
			}
			else if ((newValue.type() != VTypes::VectorData) && (newValue.arraySize() != 3))
			{
				Messenger::print("Error: Only an array of size 3 can be assigned to a vector (member '%s').", accessorData[i].name);
				result = FALSE;
			}
		}
	}
	if (!result)
	{
		Messenger::exit("ZMatrixVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	ZMatrix* ptr = (ZMatrix*) sourcerv.asPointer(VTypes::ZMatrixData, result);
	if ((!result) || (ptr == NULL))
	{
		Messenger::print("Invalid (NULL) %s reference encountered.", VTypes::dataType(VTypes::ZMatrixData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (ZMatrixVariable::AngleNames):
			if (!hasArrayIndex)
			{
				Messenger::print("Can't set names of all angle variables in ZMatrix at once.");
				result = FALSE;
			}
			else if (arrayIndex > ptr->nAngles())
			{
				Messenger::print("Angle name array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = FALSE;
			}
			else ptr->angle(arrayIndex-1)->setName( newValue.asString() );
			break;
		case (ZMatrixVariable::Angles):
			if (!hasArrayIndex)
			{
				Messenger::print("Can't set values of all angle variables in ZMatrix at once.");
				result = FALSE;
			}
			else if (arrayIndex > ptr->nAngles())
			{
				Messenger::print("Angle value array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = FALSE;
			}
			else ptr->setVariable(ptr->angle(arrayIndex-1), newValue.asDouble());
			break;
		case (ZMatrixVariable::DistanceNames):
			if (!hasArrayIndex)
			{
				Messenger::print("Can't set names of all distance variables in ZMatrix at once.");
				result = FALSE;
			}
			else if (arrayIndex > ptr->nDistances())
			{
				Messenger::print("Distance name array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = FALSE;
			}
			else ptr->distance(arrayIndex-1)->setName( newValue.asString() );
			break;
		case (ZMatrixVariable::Distances):
			if (!hasArrayIndex)
			{
				Messenger::print("Can't set values of all distance variables in ZMatrix at once.");
				result = FALSE;
			}
			else if (arrayIndex > ptr->nDistances())
			{
				Messenger::print("Distance value array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = FALSE;
			}
			else ptr->setVariable(ptr->distance(arrayIndex-1), newValue.asDouble());
			break;
		case (ZMatrixVariable::TorsionNames):
			if (!hasArrayIndex)
			{
				Messenger::print("Can't set names of all torsion variables in ZMatrix at once.");
				result = FALSE;
			}
			else if (arrayIndex > ptr->nTorsions())
			{
				Messenger::print("Torsion name array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = FALSE;
			}
			else ptr->torsion(arrayIndex-1)->setName( newValue.asString() );
			break;
		case (ZMatrixVariable::Torsions):
			if (!hasArrayIndex)
			{
				Messenger::print("Can't set values of all torsion variables in ZMatrix at once.");
				result = FALSE;
			}
			else if (arrayIndex > ptr->nTorsions())
			{
				Messenger::print("Torsion value array index (%i) is out of bounds for ZMatrix.", arrayIndex);
				result = FALSE;
			}
			else ptr->setVariable(ptr->torsion(arrayIndex-1), newValue.asDouble());
			break;
		default:
			printf("ZMatrixVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
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
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	ZMatrix* ptr = (ZMatrix*) rv.asPointer(VTypes::ZMatrixData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in ZMatrixVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	Messenger::exit("ZMatrixVariable::performFunction");
	return result;
}

// Print valid accessors/functions
void ZMatrixVariable::printAccessors()
{
	if (ZMatrixVariable::nAccessors > 0)
	{
		Messenger::print("Valid accessors are:");
		for (int n=0; n<ZMatrixVariable::nAccessors; ++n) Messenger::print("%s%s%s", n == 0 ? " " : ", ", accessorData[n].name, accessorData[n].arraySize > 0 ? "[]" : "");
		Messenger::print("");
	}
	if ((ZMatrixVariable::nFunctions > 0) && (strcmp(functionData[0].name,".dummy") != 0))
	{
		Messenger::print("Valid functions are:");
		for (int n=0; n<ZMatrixVariable::nFunctions; ++n) Messenger::print("%s%s(%s)", n == 0 ? " " : ", ", functionData[n].name, functionData[n].argText);
		Messenger::print("");
	}
}

/*
// Variable Array
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
StepNode* ZMatrixArrayVariable::findAccessor(const char* s, TreeNode* arrayIndex, TreeNode* argList)
{
	return ZMatrixVariable::accessorSearch(s, arrayIndex, argList);
}
