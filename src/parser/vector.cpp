/*
	*** Vector Variable and Array
	*** src/parser/vector.cpp
	Copyright T. Youngs 2007-2010

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
#include "base/constants.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
VectorVariable::VectorVariable(TreeNode *x, TreeNode *y, TreeNode *z)
{
	// Private variables
	constX_ = x;
	constY_ = y;
	constZ_ = z;
	readOnly_ = TRUE;
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
bool VectorVariable::set(ReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case a vector) cannot be assigned to.\n");
		return FALSE;
	}
	bool success = FALSE;
	if (rv.arraySize() == -1) vectorData_ = rv.asVector(success);
	else if (rv.arraySize() == 3)
	{
		vectorData_.x = rv.asDouble(0, success);
		if (success) vectorData_.y = rv.asDouble(1, success);
		if (success) vectorData_.z = rv.asDouble(2, success);
	}
	else
	{
		msg.print("Error: Array assigned to vector variable must contain three elements.\n");
		success = FALSE;
	}
	return success;
}

// Reset variable
bool VectorVariable::reCreate()
{
	ReturnValue rv1,rv2,rv3;
	if ((!constX_->execute(rv1)) || (!constY_->execute(rv2)) || (!constZ_->execute(rv3))) return FALSE;
	bool s1, s2, s3;
	vectorData_.set(rv1.asDouble(s1), rv2.asDouble(s2), rv3.asDouble(s3));
	if (s1 && s2 && s3) return TRUE;
	else return FALSE;
}

// Reset variable
void VectorVariable::reset()
{
	vectorData_.set(0.0,0.0,0.0);
}

// Return value of node
bool VectorVariable::execute(ReturnValue &rv)
{
	// If this vector is a constant, read the three stored expressions to recreate it
	if (readOnly_) reCreate();
	rv.set(vectorData_);
	return TRUE;
}

// Print node contents
void VectorVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	if (readOnly_)
	{
		reCreate();
		printf("[C]%s{%f,%f,%f} (constant value)\n", tab, vectorData_.x, vectorData_.y, vectorData_.z);
	}
	else printf("[V]%s{%f,%f,%f} (variable, name=%s)\n", tab, vectorData_.x, vectorData_.y, vectorData_.z, name_.get());
	delete[] tab;
}

/*
// Accessors
*/

// Accessor data
Accessor VectorVariable::accessorData[VectorVariable::nAccessors] = {
	{ "mag", VTypes::DoubleData, FALSE, FALSE },
	{ "x", VTypes::DoubleData, FALSE, FALSE },
	{ "y", VTypes::DoubleData, FALSE, FALSE },
	{ "z", VTypes::DoubleData, FALSE, FALSE }
};

// Function data
FunctionAccessor VectorVariable::functionData[VectorVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *VectorVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return VectorVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *VectorVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("VectorVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		// No accessor found - is it a function definition?
		for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		if (i == nFunctions)
		{
			msg.print("Error: Type 'vector' has no member or function named '%s'.\n", s);
			msg.exit("VectorVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'vector' function '%s'.\n", s);
			msg.exit("VectorVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::VectorData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'vector' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		else result = new StepNode(i, VTypes::VectorData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("VectorVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool VectorVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("VectorVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Vector type.\n", i);
		msg.exit("VectorVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if (hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("VectorVariable::retrieveAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
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
			printf("Internal Error: Access to member '%s' has not been defined in VectorVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("VectorVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool VectorVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("VectorVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Vector type.\n", i);
		msg.exit("VectorVariable::setAccessor");
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
		msg.exit("VectorVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	Vec3<double> v = sourcerv.asVector(result);
	if (result) switch (acc)
	{
		case (VectorVariable::Magnitude):
			// Normalise existing vector, then multiply by new magnitude
			v.normalise();
			sourcerv.set( v * newvalue.asDouble(result) );
			break;
		case (VectorVariable::X):
		case (VectorVariable::Y):
		case (VectorVariable::Z):
			sourcerv.set(acc - VectorVariable::X, newvalue.asDouble(result));
			break;
		default:
			printf("VectorVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("VectorVariable::setAccessor");
	return result;
}

// Perform desired function
bool VectorVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("VectorVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Vector type.\n", i);
		msg.exit("VectorVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Vec3<double> v = rv.asVector();
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in VectorVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("VectorVariable::performFunction");
	return result;
}

/*
// Variable Array
*/

// Constructor
VectorArrayVariable::VectorArrayVariable(TreeNode *sizeexpr, bool constant) : arraySizeExpression_(sizeexpr)
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
bool VectorArrayVariable::set(ReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case a vector array) cannot be assigned to.\n");
		return FALSE;
	}
	if (vectorArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", name_.get());
		return FALSE;
	}
	// Loop over array elements and set them
	for (int i=0; i<arraySize_; i++) vectorArrayData_[i] = rv.asVector();
	return TRUE;
}

// Set array element from returnvalue node
bool VectorArrayVariable::setAsArray(ReturnValue &rv, int arrayindex)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case a vector array?) cannot be assigned to.\n");
		return FALSE;
	}
	if (vectorArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", name_.get());
		return FALSE;
	}
	// Check index
	if ((arrayindex < 0) || (arrayindex >= arraySize_))
	{
		msg.print("Index %i out of bounds for array '%s'.\n", arrayindex+1, name_.get());
		return FALSE;
	}
	// Set individual element
	vectorArrayData_[arrayindex] = rv.asVector();
	return TRUE;
}

// Reset variable
void VectorArrayVariable::reset()
{
	if (vectorArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", name_.get());
		return;
	}
	// Loop over array elements and set them
	for (int i=0; i<arraySize_; i++) vectorArrayData_[i] = 0;
}

// Return value of node
bool VectorArrayVariable::execute(ReturnValue &rv)
{
	msg.print("A whole vector array ('%s') cannot be passed as a value.\n", name_.get());
	return FALSE;
}

// Return value of node as array
bool VectorArrayVariable::executeAsArray(ReturnValue &rv, int arrayindex)
{
	// Check bounds
	if ((arrayindex < 0) || (arrayindex >= arraySize_))
	{
		msg.print("Error: Array index %i is out of bounds for array '%s'.\n", arrayindex+1, name_.get());
		return FALSE;
	}
	rv.set( vectorArrayData_[arrayindex] );
	return TRUE;
}

// Print node contents
void VectorArrayVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	printf("[V]%s (integer array, name=%s, current size=%i)\n", tab, name_.get(), arraySize_);
	delete[] tab;
}

// Initialise array
bool VectorArrayVariable::initialise()
{
	// We define our own initialise() function to take over from the inherited default from Variable
	// Get size of array to create
	ReturnValue newsize;
	if (!arraySizeExpression_->execute(newsize))
	{
		msg.print("Failed to find size for vector array '%s'.\n", name_.get());
		return FALSE;
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
			if (!set(rv)) return FALSE;
		}
		else return FALSE;
	}
	return TRUE;
}

// Search variable access list for provided accessor
StepNode *VectorArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return VectorVariable::accessorSearch(s, arrayindex, arglist);
}
