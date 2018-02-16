/*
	*** Vector Variable and Array
	*** src/parser/vector.cpp
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

#include "parser/vector.h"
#include "parser/stepnode.h"

ATEN_USING_NAMESPACE

// Constructors
VectorVariable::VectorVariable(bool constant)
{
	// Private variables
	returnType_ = VTypes::VectorData;
	readOnly_ = constant;
}
VectorVariable::VectorVariable(Vec3<double> v, bool constant) : vectorData_(v)
{
	// Private variables
	returnType_ = VTypes::VectorData;
	readOnly_ = constant;
}
VectorVariable::VectorVariable(TreeNode* x, TreeNode* y, TreeNode* z)
{
	// Private variables
	constX_ = x;
	constY_ = y;
	constZ_ = z;
	readOnly_ = true;
	returnType_ = VTypes::VectorData;
}

// Destructor
VectorVariable::~VectorVariable()
{
}

/*
// Set / Get
*/

// Set value of variable
bool VectorVariable::set(ReturnValue& rv)
{
	if (readOnly_)
	{
		Messenger::print("A constant value (in this case a vector) cannot be assigned to.");
		return false;
	}
	bool success = false;
	if (rv.arraySize() == -1) vectorData_ = rv.asVector(success);
	else if (rv.arraySize() == 3)
	{
		vectorData_.x = rv.asDouble(0, success);
		if (success) vectorData_.y = rv.asDouble(1, success);
		if (success) vectorData_.z = rv.asDouble(2, success);
	}
	else
	{
		Messenger::print("Error: Array assigned to vector variable must contain three elements.");
		success = false;
	}
	return success;
}

// Reset variable
bool VectorVariable::reCreate()
{
	ReturnValue rv1,rv2,rv3;
	if ((!constX_->execute(rv1)) || (!constY_->execute(rv2)) || (!constZ_->execute(rv3))) return false;
	bool s1, s2, s3;
	vectorData_.set(rv1.asDouble(s1), rv2.asDouble(s2), rv3.asDouble(s3));
	if (s1 && s2 && s3) return true;
	else return false;
}

// Reset variable
void VectorVariable::reset()
{
	vectorData_.set(0.0,0.0,0.0);
}

// Return value of node
bool VectorVariable::execute(ReturnValue& rv)
{
	// If this vector is a constant, read the three stored expressions to recreate it
	if (readOnly_) reCreate();
	rv.set(vectorData_);
	return true;
}

// Print node contents
void VectorVariable::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	tab += prefix;

	// Output node data
	if (readOnly_)
	{
		reCreate();
		printf("[C]%s{%f,%f,%f} (constant value)\n", qPrintable(tab), vectorData_.x, vectorData_.y, vectorData_.z);
	}
	else printf("[V]%s{%f,%f,%f} (variable, name=%s)\n", qPrintable(tab), vectorData_.x, vectorData_.y, vectorData_.z, qPrintable(name_));
}

/*
 * Accessors
 */

// Accessor data
Accessor VectorVariable::accessorData[VectorVariable::nAccessors] = {
	{ "mag", VTypes::DoubleData, false, false },
	{ "x", VTypes::DoubleData, false, false },
	{ "y", VTypes::DoubleData, false, false },
	{ "z", VTypes::DoubleData, false, false }
};

// Function data
FunctionAccessor VectorVariable::functionData[VectorVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode* VectorVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return VectorVariable::accessorSearch(name, arrayIndex, argList);
}

// Private static function to search accessors
StepNode* VectorVariable::accessorSearch(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	Messenger::enter("VectorVariable::accessorSearch");
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
			Messenger::print("Error: Type 'Vector' has no member or function named '%s'.", qPrintable(name));
			Messenger::exit("VectorVariable::accessorSearch");
			return NULL;
		}
		Messenger::print(Messenger::Parse, "FunctionAccessor match = %i (%s)", i, qPrintable(functionData[i].name));
		if (arrayIndex != NULL)
		{
			Messenger::print("Error: Array index given to 'Vector' function named '%s'.", qPrintable(name));
			Messenger::exit("VectorVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::VectorData, functionData[i].returnType);
		result->addJoinedArguments(argList);
		if (!result->checkArguments(functionData[i].arguments, qPrintable(functionData[i].name)))
		{
			Messenger::print("Error: Syntax for 'Vector' function '%s' is '%s(%s)'.", qPrintable(functionData[i].name), qPrintable(functionData[i].name), qPrintable(functionData[i].argText) );
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
			Messenger::print("Error: Argument list given to 'Vector&' array member '%s'.", qPrintable(name));
			Messenger::exit("VectorVariable::accessorSearch");
			return NULL;
		}
		else result = new StepNode(i, VTypes::VectorData, arrayIndex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	Messenger::exit("VectorVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool VectorVariable::retrieveAccessor(int i, ReturnValue& rv, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("VectorVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Vector type.\n", i);
		Messenger::exit("VectorVariable::retrieveAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if (hasArrayIndex)
	{
		Messenger::print("Error: Unnecessary array index provided for member '%s'.", qPrintable(accessorData[i].name));
		Messenger::exit("VectorVariable::retrieveAccessor");
		return false;
	}
	// Get current data from ReturnValue
	bool result = true;
	Vec3<double> v = rv.asVector(result);
	if (result) switch (acc)
	{
		case (VectorVariable::Magnitude):
			rv.set(v.magnitude());
			break;
		case (VectorVariable::X):
			rv.set(v.x);
			break;
		case (VectorVariable::Y):
			rv.set(v.y);
			break;
		case (VectorVariable::Z):
			rv.set(v.z);
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in VectorVariable.\n", qPrintable(accessorData[i].name));
			result = false;
			break;
	}
	Messenger::exit("VectorVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool VectorVariable::setAccessor(int i, ReturnValue& sourcerv, ReturnValue& newValue, bool hasArrayIndex, int arrayIndex)
{
	Messenger::enter("VectorVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Vector type.\n", i);
		Messenger::exit("VectorVariable::setAccessor");
		return false;
	}
	Accessors acc = (Accessors) i;

	// Check for correct lack/presence of array index given to original accessor, and nature of new value
	bool result = checkAccessorArrays(accessorData[acc], newValue, hasArrayIndex, arrayIndex);
	if (!result)
	{
		Messenger::exit("VectorVariable::setAccessor");
		return false;
	}

	// Get current data from ReturnValue
	Vec3<double>& v = sourcerv.vector();
	if (result) switch (acc)
	{
		case (VectorVariable::Magnitude):
			// Normalise existing vector, then multiply by new magnitude
			v.normalise();
			v *= newValue.asDouble(result);
			break;
		case (VectorVariable::X):
		case (VectorVariable::Y):
		case (VectorVariable::Z):
			v.set(acc - VectorVariable::X, newValue.asDouble(result));
			break;
		default:
			printf("VectorVariable::setAccessor doesn't know how to use member '%s'.\n", qPrintable(accessorData[acc].name));
			result = false;
			break;
	}
	Messenger::exit("VectorVariable::setAccessor");
	return result;
}

// Perform desired function
bool VectorVariable::performFunction(int i, ReturnValue& rv, TreeNode* node)
{
	Messenger::enter("VectorVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Vector type.\n", i);
		Messenger::exit("VectorVariable::performFunction");
		return false;
	}

	// Get current data from ReturnValue
	bool result = true;
	Vec3<double> v = rv.asVector();
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in VectorVariable.\n", qPrintable(functionData[i].name));
			result = false;
			break;
	}
	Messenger::exit("VectorVariable::performFunction");
	return result;
}

/*
 * Variable Array
 */

// Constructor
VectorArrayVariable::VectorArrayVariable(TreeNode* sizeexpr, bool constant) : arraySizeExpression_(sizeexpr)
{
	// Private variables
	returnType_ = VTypes::VectorData;
	vectorArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
}

// Destructor
VectorArrayVariable::~VectorArrayVariable()
{
	if (vectorArrayData_ != NULL) delete[] vectorArrayData_;
}

/*
// Set / Get
*/

// Set from returnvalue node
bool VectorArrayVariable::set(ReturnValue& rv)
{
	if (readOnly_)
	{
		Messenger::print("A constant value (in this case a vector array) cannot be assigned to.");
		return false;
	}
	if (vectorArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", qPrintable(name_));
		return false;
	}
	// Loop over array elements and set them
	for (int i=0; i<arraySize_; i++) vectorArrayData_[i] = rv.asVector();
	return true;
}

// Set array element from returnvalue node
bool VectorArrayVariable::setAsArray(ReturnValue& rv, int arrayIndex)
{
	if (readOnly_)
	{
		Messenger::print("A constant value (in this case a vector array?) cannot be assigned to.");
		return false;
	}
	if (vectorArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.", qPrintable(name_));
		return false;
	}
	// Check index
	if ((arrayIndex < 0) || (arrayIndex >= arraySize_))
	{
		Messenger::print("Index %i out of bounds for array '%s'.", arrayIndex+1, qPrintable(name_));
		return false;
	}
	// Set individual element
	vectorArrayData_[arrayIndex] = rv.asVector();
	return true;
}

// Reset variable
void VectorArrayVariable::reset()
{
	if (vectorArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", qPrintable(name_));
		return;
	}
	// Loop over array elements and set them
	for (int i=0; i<arraySize_; i++) vectorArrayData_[i] = 0;
}

// Return value of node
bool VectorArrayVariable::execute(ReturnValue& rv)
{
	Messenger::print("A whole vector array ('%s') cannot be passed as a value.", qPrintable(name_));
	return false;
}

// Return value of node as array
bool VectorArrayVariable::executeAsArray(ReturnValue& rv, int arrayIndex)
{
	// Check bounds
	if ((arrayIndex < 0) || (arrayIndex >= arraySize_))
	{
		Messenger::print("Error: Array index %i is out of bounds for array '%s'.", arrayIndex+1, qPrintable(name_));
		return false;
	}
	rv.set( vectorArrayData_[arrayIndex] );
	return true;
}

// Print node contents
void VectorArrayVariable::nodePrint(int offset, const char* prefix)
{
	// Construct tabbed offset
	QString tab;
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab += "   |--> ";
	tab += prefix;

	// Output node data
	printf("[V]%s (integer array, name=%s, current size=%i)\n", qPrintable(tab), qPrintable(name_), arraySize_);
}

// Initialise array
bool VectorArrayVariable::initialise()
{
	// We define our own initialise() function to take over from the inherited default from Variable
	// Get size of array to create
	ReturnValue newsize;
	if (!arraySizeExpression_->execute(newsize))
	{
		Messenger::print("Failed to find size for vector array '%s'.", qPrintable(name_));
		return false;
	}
	// If the array is already allocated, free it only if the size is different
	if ((arraySize_ != newsize.asInteger()) && (vectorArrayData_ != NULL)) { delete[] vectorArrayData_; vectorArrayData_ = NULL; }
	// Store new array size
	arraySize_ = newsize.asInteger();
	if ((arraySize_ > 0) && (vectorArrayData_ == NULL)) vectorArrayData_ = new Vec3<double>[arraySize_];
	if (initialValue_ == NULL) reset();
	else
	{
		ReturnValue rv;
		if (initialValue_->execute(rv))
		{
			if (!set(rv)) return false;
		}
		else return false;
	}
	return true;
}

// Search variable access list for provided accessor
StepNode* VectorArrayVariable::findAccessor(QString name, TreeNode* arrayIndex, TreeNode* argList)
{
	return VectorVariable::accessorSearch(name, arrayIndex, argList);
}
