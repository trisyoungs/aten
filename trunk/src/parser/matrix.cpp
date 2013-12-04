/*
	*** Matrix Variable and Array
	*** src/parser/matrix.cpp
	Copyright T. Youngs 2007-2013

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

#include "parser/matrix.h"
#include "parser/stepnode.h"
#include "base/constants.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Constructors
MatrixVariable::MatrixVariable(bool constant)
{
	// Private variables
	returnType_ = VTypes::MatrixData;
	readOnly_ = constant;
}
MatrixVariable::MatrixVariable(Matrix m, bool constant) : matrixData_(m)
{
	// Private variables
	returnType_ = VTypes::MatrixData;
	readOnly_ = constant;
}
MatrixVariable::MatrixVariable(TreeNode *xx, TreeNode *xy, TreeNode *xz, TreeNode *yx, TreeNode *yy, TreeNode *yz, TreeNode *zx, TreeNode *zy, TreeNode *zz)
{
	// Private variables
	constXX_ = xx;
	constXY_ = xy;
	constXZ_ = xz;
	constYX_ = yx;
	constYY_ = yy;
	constYZ_ = yz;
	constZX_ = zx;
	constZY_ = zy;
	constZZ_ = zz;
	readOnly_ = TRUE;
	returnType_ = VTypes::MatrixData;
}

// Destructor
MatrixVariable::~MatrixVariable()
{
}

/*
// Set / Get
*/

// Set value of variable
bool MatrixVariable::set(ReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case a matrix) cannot be assigned to.\n");
		return FALSE;
	}
	bool success = FALSE;
	if (rv.arraySize() == -1) matrixData_ = rv.asMatrix(success);
	else if (rv.arraySize() == 9)
	{
		for (int n=0; n<9; ++n)
		{
			matrixData_[n/3*4+n%3] = rv.asDouble(n, success);
			if (!success) break;
		}
	}
	else
	{
		msg.print("Error: Array assigned to matrix variable must contain nine elements.\n");
		success = FALSE;
	}
	return success;
}

// Reset variable
bool MatrixVariable::reCreate()
{
	ReturnValue rv[9];
	if (!constXX_->execute(rv[0])) return FALSE;
	if (!constXY_->execute(rv[1])) return FALSE;
	if (!constXZ_->execute(rv[2])) return FALSE;
	if (!constYX_->execute(rv[3])) return FALSE;
	if (!constYY_->execute(rv[4])) return FALSE;
	if (!constYZ_->execute(rv[5])) return FALSE;
	if (!constZX_->execute(rv[6])) return FALSE;
	if (!constZY_->execute(rv[7])) return FALSE;
	if (!constZZ_->execute(rv[8])) return FALSE;
	bool success;
	for (int n=0; n<9; ++n)
	{
		matrixData_[n/3*4+n%3] = rv[n].asDouble(success);
		if (!success) return FALSE;
	}
	return TRUE;
}

// Reset variable
void MatrixVariable::reset()
{
	matrixData_.zero();
}

// Return value of node
bool MatrixVariable::execute(ReturnValue &rv)
{
	// If this matrix is a constant, read the nine stored expressions to recreate it
	if (readOnly_) reCreate();
	rv.set(matrixData_);
	return TRUE;
}

// Print node contents
void MatrixVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	Dnchar tab(offset+32);
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab.strcat("   |--> ");
	tab.strcat(prefix);

	// Output node data
	if (readOnly_)
	{
		reCreate();
		printf("[C]%s{%f,%f,%f,%f,%f,%f,%f,%f,%f} (constant value)\n", tab.get(), matrixData_[0], matrixData_[1], matrixData_[2], matrixData_[4], matrixData_[5], matrixData_[6], matrixData_[8], matrixData_[9], matrixData_[10]);
	}
	else printf("[V]%s{%f,%f,%f,%f,%f,%f,%f,%f,%f} (variable, name=%s)\n", tab.get(), matrixData_[0], matrixData_[1], matrixData_[2], matrixData_[4], matrixData_[5], matrixData_[6], matrixData_[8], matrixData_[9], matrixData_[10], name_.get());
	delete[] tab;
}

/*
// Accessors
*/

// Accessor data
Accessor MatrixVariable::accessorData[MatrixVariable::nAccessors] = {
	{ "determinant", VTypes::DoubleData, FALSE, TRUE },
	{ "xx", VTypes::DoubleData, FALSE, FALSE },
	{ "xy", VTypes::DoubleData, FALSE, FALSE },
	{ "xz", VTypes::DoubleData, FALSE, FALSE },
	{ "yx", VTypes::DoubleData, FALSE, FALSE },
	{ "yy", VTypes::DoubleData, FALSE, FALSE },
	{ "yz", VTypes::DoubleData, FALSE, FALSE },
	{ "zx", VTypes::DoubleData, FALSE, FALSE },
	{ "zy", VTypes::DoubleData, FALSE, FALSE },
	{ "zz", VTypes::DoubleData, FALSE, FALSE }
};

// Function data
FunctionAccessor MatrixVariable::functionData[MatrixVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *MatrixVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return MatrixVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *MatrixVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("MatrixVariable::accessorSearch");
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
			msg.print("Error: Type 'Matrix' has no member or function named '%s'.\n", s);
			msg.exit("MatrixVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'Matrix' function '%s'.\n", s);
			msg.exit("MatrixVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::MatrixData, functionData[i].returnType);
		result->addJoinedArguments(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'Matrix' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		// Were we given an argument list when we didn't want one?
		if (arglist != NULL)
		{
			msg.print("Error: Argument list given to 'Matrix&' array member '%s'.\n", s);
			msg.exit("MatrixVariable::accessorSearch");
			return NULL;
		}
		else result = new StepNode(i, VTypes::MatrixData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("MatrixVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool MatrixVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("MatrixVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Matrix type.\n", i);
		msg.exit("MatrixVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if (hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("MatrixVariable::retrieveAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Matrix m = rv.asMatrix(result);
	if (result) switch (acc)
	{
		case (MatrixVariable::Determinant):
			rv.set(m.determinant());
			break;
		case (MatrixVariable::XX):
		case (MatrixVariable::XY):
		case (MatrixVariable::XZ):
		case (MatrixVariable::YX):
		case (MatrixVariable::YY):
		case (MatrixVariable::YZ):
		case (MatrixVariable::ZX):
		case (MatrixVariable::ZY):
		case (MatrixVariable::ZZ):
			rv.set(m[(acc-MatrixVariable::XX)/3*4+(acc-MatrixVariable::XX)%3]);
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in MatrixVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("MatrixVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool MatrixVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("MatrixVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Matrix type.\n", i);
		msg.exit("MatrixVariable::setAccessor");
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
		// This is not an array member, so cannot be assigned an array unless its a Matrix
		if (newvalue.arraySize() != -1)
		{
			if (accessorData[i].returnType != VTypes::MatrixData)
			{
				msg.print("Error: An array can't be assigned to the single valued member '%s'.\n", accessorData[i].name);
				result = FALSE;
			}
			else if ((newvalue.type() != VTypes::MatrixData) && (newvalue.arraySize() != 9))
			{
				msg.print("Error: Only an array of size 9 can be assigned to a matrix (member '%s').\n", accessorData[i].name);
				result = FALSE;
			}
		}
	}
	if (!result)
	{
		msg.exit("MatrixVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	Matrix& m = sourcerv.matrix();
	if (result) switch (acc)
	{
		case (MatrixVariable::XX):
		case (MatrixVariable::XY):
		case (MatrixVariable::XZ):
		case (MatrixVariable::YX):
		case (MatrixVariable::YY):
		case (MatrixVariable::YZ):
		case (MatrixVariable::ZX):
		case (MatrixVariable::ZY):
		case (MatrixVariable::ZZ):
			m[(acc-MatrixVariable::XX)/3*4+(acc-MatrixVariable::XX)%3] = newvalue.asDouble(result);
			break;
		default:
			printf("MatrixVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("MatrixVariable::setAccessor");
	return result;
}

// Perform desired function
bool MatrixVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("MatrixVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Matrix type.\n", i);
		msg.exit("MatrixVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Matrix m = rv.asMatrix();
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in MatrixVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("MatrixVariable::performFunction");
	return result;
}

/*
// Variable Array
*/

// Constructor
MatrixArrayVariable::MatrixArrayVariable(TreeNode *sizeexpr, bool constant) : arraySizeExpression_(sizeexpr)
{
	// Private variables
	returnType_ = VTypes::MatrixData;
	matrixArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
}

// Destructor
MatrixArrayVariable::~MatrixArrayVariable()
{
	if (matrixArrayData_ != NULL) delete[] matrixArrayData_;
}

/*
// Set / Get
*/

// Set from returnvalue node
bool MatrixArrayVariable::set(ReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case a matrix array) cannot be assigned to.\n");
		return FALSE;
	}
	if (matrixArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", name_.get());
		return FALSE;
	}
	// Loop over array elements and set them
	for (int i=0; i<arraySize_; i++) matrixArrayData_[i] = rv.asMatrix();
	return TRUE;
}

// Set array element from returnvalue node
bool MatrixArrayVariable::setAsArray(ReturnValue &rv, int arrayindex)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case a matrix array?) cannot be assigned to.\n");
		return FALSE;
	}
	if (matrixArrayData_ == NULL)
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
	matrixArrayData_[arrayindex] = rv.asMatrix();
	return TRUE;
}

// Reset variable
void MatrixArrayVariable::reset()
{
	if (matrixArrayData_ == NULL)
	{
		printf("Internal Error: Array '%s' has not been initialised.\n", name_.get());
		return;
	}
	// Loop over array elements and set them
	for (int i=0; i<arraySize_; i++) matrixArrayData_[i].zero();
}

// Return value of node
bool MatrixArrayVariable::execute(ReturnValue &rv)
{
	msg.print("A whole matrix array ('%s') cannot be passed as a value.\n", name_.get());
	return FALSE;
}

// Return value of node as array
bool MatrixArrayVariable::executeAsArray(ReturnValue &rv, int arrayindex)
{
	// Check bounds
	if ((arrayindex < 0) || (arrayindex >= arraySize_))
	{
		msg.print("Error: Array index %i is out of bounds for array '%s'.\n", arrayindex+1, name_.get());
		return FALSE;
	}
	rv.set( matrixArrayData_[arrayindex] );
	return TRUE;
}

// Print node contents
void MatrixArrayVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	Dnchar tab(offset+32);
	for (int n=0; n<offset-1; n++) tab += '\t';
	if (offset > 1) tab.strcat("   |--> ");
	tab.strcat(prefix);

	// Output node data
	printf("[V]%s (integer array, name=%s, current size=%i)\n", tab.get(), name_.get(), arraySize_);
}

// Initialise array
bool MatrixArrayVariable::initialise()
{
	// We define our own initialise() function to take over from the inherited default from Variable
	// Get size of array to create
	ReturnValue newsize;
	if (!arraySizeExpression_->execute(newsize))
	{
		msg.print("Failed to find size for matrix array '%s'.\n", name_.get());
		return FALSE;
	}
	// If the array is already allocated, free it only if the size is different
	if ((arraySize_ != newsize.asInteger()) && (matrixArrayData_ != NULL)) { delete[] matrixArrayData_; matrixArrayData_ = NULL; }
	// Store new array size
	arraySize_ = newsize.asInteger();
	if ((arraySize_ > 0) && (matrixArrayData_ == NULL)) matrixArrayData_ = new Matrix[arraySize_];
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
StepNode *MatrixArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return MatrixVariable::accessorSearch(s, arrayindex, arglist);
}
