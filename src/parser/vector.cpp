/*
	*** Vector Variable and Array
	*** src/parser/vector.cpp
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
	vectorData_ = rv.asVector();
	return TRUE;
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
	{ "x", VTypes::DoubleData, FALSE, FALSE },
	{ "y", VTypes::DoubleData, FALSE, FALSE },
	{ "z", VTypes::DoubleData, FALSE, FALSE }
};

// Search variable access list for provided accessor (call private static function)
StepNode *VectorVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return VectorVariable::accessorSearch(s, arrayindex);
}

// Private static function to search accessors
StepNode *VectorVariable::accessorSearch(const char *s, TreeNode *arrayindex)
{
	msg.enter("VectorVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'vector' has no member named '%s'.\n", s);
		msg.exit("VectorVariable::accessorSearch");
		return NULL;
	}
	// Create a suitable AccessNode to return...
	msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
	result = new StepNode(i, VTypes::VectorData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly);
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
		printf("Internal Error: Accessor id %i is out of range for Vector type.\n");
		msg.exit("VectorVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if (!accessorData[i].isArray)
	{
		if (hasArrayIndex) msg.print("Warning: Irrelevant array index provided for member '%s'.\n", accessorData[i].name);
	}
	else if (!hasArrayIndex)
	{
		msg.print("Error: No array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("VectorVariable::retrieveAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Vec3<double> v = rv.asVector(result);
	if (result) switch (acc)
	{
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
		printf("Internal Error: Accessor id %i is out of range for Vector type.\n");
		msg.exit("VectorVariable::setAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if (!accessorData[i].isArray)
	{
		if (hasArrayIndex) msg.print("Warning: Irrelevant array index provided for member '%s'.\n", accessorData[i].name);
	}
	else if (!hasArrayIndex)
	{
		msg.print("Error: No array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("VectorVariable::setAccessor");
		return FALSE;
	}
	// ReturnValue contains a copy of the vector data...
	bool result = TRUE;
	Vec3<double> v = sourcerv.asVector(result);
	if (result) switch (acc)
	{
		case (VectorVariable::X):
		case (VectorVariable::Y):
		case (VectorVariable::Z):
			sourcerv.set(acc, newvalue.asDouble(result));
			break;
		default:
			printf("VectorVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("VectorVariable::setAccessor");
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
	// If the array is already allocated, free it.
	if (vectorArrayData_ != NULL) printf("Array exists already...\n");	
	if (vectorArrayData_ != NULL) delete[] vectorArrayData_;
	// Get size of array to create
	ReturnValue newsize;
	if (!arraySizeExpression_->execute(newsize))
	{
		msg.print("Failed to find array size for '%s'.\n", name_.get());
		return FALSE;
	}
	// Create new array
	arraySize_ = newsize.asInteger();
	if (arraySize_ > 0) vectorArrayData_ = new Vec3<double>[arraySize_];
	if (initialValue_ == NULL) reset();
	else
	{
		ReturnValue rv;
		if (initialValue_->execute(rv))
		{
			if (set(rv)) return TRUE;
			else
			{
				msg.print("Error: Variable %s is of type '%s', and cannot be initialised from a value of type '%s'.\n", name_.get(), VTypes::dataType(returnType_), VTypes::dataType(rv.type()));
				return FALSE;
			}
		}
		return FALSE;
	}
	return TRUE;
}

// Search variable access list for provided accessor
StepNode *VectorArrayVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return VectorVariable::accessorSearch(s, arrayindex);
}
