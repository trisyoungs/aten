/*
	*** Vector Variable
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
NuVectorVariable::NuVectorVariable(bool constant)
{
	// Private variables
	returnType_ = NuVTypes::VectorData;
	readOnly_ = constant;
}
NuVectorVariable::NuVectorVariable(Vec3<double> v, bool constant) : vectorData_(v)
{
	// Private variables
	returnType_ = NuVTypes::VectorData;
	readOnly_ = constant;
}
NuVectorVariable::NuVectorVariable(TreeNode *x, TreeNode *y, TreeNode *z)
{
	// Private variables
	constX_ = x;
	constY_ = y;
	constZ_ = z;
	readOnly_ = TRUE;
	returnType_ = NuVTypes::VectorData;
}

// Destructor
NuVectorVariable::~NuVectorVariable()
{
}

/*
// Set / Get
*/

// Set value of variable
bool NuVectorVariable::set(NuReturnValue &rv)
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
bool NuVectorVariable::reCreate()
{
	NuReturnValue rv1,rv2,rv3;
	if ((!constX_->execute(rv1)) || (!constY_->execute(rv2)) || (!constZ_->execute(rv3))) return FALSE;
	bool s1, s2, s3;
	vectorData_.set(rv1.asReal(s1), rv2.asReal(s2), rv3.asReal(s3));
	if (s1 && s2 && s3) return TRUE;
	else return FALSE;
}

// Reset variable
void NuVectorVariable::reset()
{
	vectorData_.set(0.0,0.0,0.0);
}

// Return value of node
bool NuVectorVariable::execute(NuReturnValue &rv)
{
	// If this vector is a constant, read the three stored expressions to recreate it
	if (readOnly_) reCreate();
	rv.set(vectorData_);
	return TRUE;
}

// Print node contents
void NuVectorVariable::nodePrint(int offset, const char *prefix)
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
Accessor NuVectorVariable::accessorData[NuVectorVariable::nAccessors] = {
	{ "x", NuVTypes::DoubleData, FALSE, FALSE },
	{ "y", NuVTypes::DoubleData, FALSE, FALSE },
	{ "z", NuVTypes::DoubleData, FALSE, FALSE }
};

// Search variable access list for provided accessor (call private static function)
StepNode *NuVectorVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return NuVectorVariable::accessorSearch(s, arrayindex);
}

// Private static function to search accessors
StepNode *NuVectorVariable::accessorSearch(const char *s, TreeNode *arrayindex)
{
	msg.enter("NuVectorVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'vector' has no member named '%s'.\n", s);
		msg.exit("NuVectorVariable::accessorSearch");
		return NULL;
	}
	// Create a suitable AccessNode to return...
	msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
	result = new StepNode(i, NuVTypes::VectorData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly);
	msg.exit("NuVectorVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool NuVectorVariable::retrieveAccessor(int i, NuReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("NuVectorVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Vector type.\n");
		msg.exit("NuVectorVariable::retrieveAccessor");
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
		msg.exit("NuVectorVariable::retrieveAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Vec3<double> v = rv.asVector(result);
	if (result) switch (acc)
	{
		case (NuVectorVariable::X):
			rv.set(v.x);
			break;
		case (NuVectorVariable::Y):
			rv.set(v.y);
			break;
		case (NuVectorVariable::Z):
			rv.set(v.z);
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in VectorVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("NuVectorVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool NuVectorVariable::setAccessor(int i, NuReturnValue &sourcerv, NuReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("NuVectorVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Vector type.\n");
		msg.exit("NuVectorVariable::setAccessor");
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
		msg.exit("NuVectorVariable::setAccessor");
		return FALSE;
	}
	// ReturnValue contains a copy of the vector data...
	bool result = TRUE;
	Vec3<double> v = sourcerv.asVector(result);
	if (result) switch (acc)
	{
		case (NuVectorVariable::X):
		case (NuVectorVariable::Y):
		case (NuVectorVariable::Z):
			sourcerv.set(acc, newvalue.asReal(result));
			break;
		default:
			printf("NuVectorVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("NuVectorVariable::setAccessor");
	return result;
}
