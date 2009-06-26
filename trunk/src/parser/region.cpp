/*
	*** Region Variable and Array
	*** src/parser/region.cpp
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

#include "parser/region.h"
#include "parser/stepnode.h"
#include "base/region.h"
#include "model/model.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
// Variable
*/

// Constructor
RegionVariable::RegionVariable(ComponentRegion *ptr, bool constant)
{
	// Private variables
	returnType_ = VTypes::RegionData;
	readOnly_ = constant;
	pointerData_ = ptr;
}

// Destructor
RegionVariable::~RegionVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor RegionVariable::accessorData[RegionVariable::nAccessors] = {
	{ "centre",		VTypes::VectorData,	0, FALSE },
	{ "centrefrac",		VTypes::VectorData,	0, FALSE },
	{ "gemetry",		VTypes::VectorData,	0, FALSE },
	{ "geometryfrac",	VTypes::VectorData,	0, FALSE },
	{ "iscentrefrac",	VTypes::IntegerData,	0, TRUE },
	{ "isgeometryfrac",	VTypes::IntegerData,	0, TRUE },
	{ "overlap",		VTypes::IntegerData,	0, FALSE },
	{ "shape",		VTypes::StringData,	0, FALSE },
	{ "xrotation",		VTypes::DoubleData,	0, FALSE },
	{ "yrotation",		VTypes::IntegerData,	0, FALSE }
};

// Function data
FunctionAccessor RegionVariable::functionData[RegionVariable::nFunctions] = {
	{ ".dummy",	VTypes::IntegerData,	"",	"" }
};

// Search variable access list for provided accessor (call private static function)
StepNode *RegionVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return RegionVariable::accessorSearch(s, arrayindex, arglist);
}

// Private static function to search accessors
StepNode *RegionVariable::accessorSearch(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	msg.enter("RegionVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		// No accessor found - is it a function definition?
		for (i = 0; i < nFunctions; i++) if (strcmp(functionData[i].name,s) == 0) break;
		if (i == nFunctions)
		{
			msg.print("Error: Type 'grid&' has no member or function named '%s'.\n", s);
			msg.exit("RegionVariable::accessorSearch");
			return NULL;
		}
		msg.print(Messenger::Parse, "FunctionAccessor match = %i (%s)\n", i, functionData[i].name);
		if (arrayindex != NULL)
		{
			msg.print("Error: Array index given to 'grid&' function '%s'.\n", s);
			msg.exit("RegionVariable::accessorSearch");
			return NULL;
		}
		// Add and check supplied arguments...
		result = new StepNode(i, VTypes::RegionData, functionData[i].returnType);
		result->addArgumentList(arglist);
		if (!result->checkArguments(functionData[i].arguments, functionData[i].name))
		{
			msg.print("Error: Syntax for 'grid&' function '%s' is '%s(%s)'.\n", functionData[i].name, functionData[i].name, functionData[i].argText );
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
		else result = new StepNode(i, VTypes::RegionData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize);
	}
	msg.exit("RegionVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool RegionVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("RegionVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Region type.\n", i);
		msg.exit("RegionVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("RegionVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("RegionVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	ComponentRegion *ptr= (ComponentRegion*) rv.asPointer(VTypes::RegionData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::RegionData));
		result = FALSE;
	}
	if (result) switch (acc)
	{
		case (RegionVariable::Centre):
			if (ptr->isCentreFrac()) msg.print("Warning: Region centre is currently defined in fractional coordinates.\n");
			rv.set( ptr->centre() );
			break;
		case (RegionVariable::CentreFrac):
			if (!ptr->isCentreFrac()) msg.print("Warning: Region centre is currently defined in real coordinates.\n");
			rv.set( ptr->centre() );
			break;
		case (RegionVariable::Geometry):
			if (ptr->isGeometryFrac()) msg.print("Warning: Region geometry is currently defined in fractional coordinates.\n");
			rv.set( ptr->geometry() );
			break;
		case (RegionVariable::GeometryFrac):
			if (!ptr->isGeometryFrac()) msg.print("Warning: Region geometry is currently defined in real coordinates.\n");
			rv.set( ptr->geometry() );
			break;
		case (RegionVariable::IsCentreFrac):
			rv.set( ptr->isCentreFrac() );
			break;
		case (RegionVariable::IsGeometryFrac):
			rv.set( ptr->isGeometryFrac() );
			break;
		case (RegionVariable::Overlap):
			rv.set( ptr->allowOverlap() );
			break;
		case (RegionVariable::Shape):
			rv.set( ComponentRegion::regionShape(ptr->shape()) );
			break;
		case (RegionVariable::XRotation):
			rv.set( ptr->rotations().x );
			break;
		case (RegionVariable::YRotation):
			rv.set( ptr->rotations().y );
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in RegionVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("RegionVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool RegionVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("RegionVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Region type.\n", i);
		msg.exit("RegionVariable::setAccessor");
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
		msg.exit("RegionVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	ComponentRegion *ptr= (ComponentRegion*) sourcerv.asPointer(VTypes::RegionData, result);
	if (result && (ptr == NULL))
	{
		msg.print("Invalid (NULL) %s reference encountered.\n", VTypes::dataType(VTypes::RegionData));
		result = FALSE;
	}
	ComponentRegion::RegionShape rs;
	Vec3<double> v;
	if (result) switch (acc)
	{
		case (RegionVariable::Centre):
			ptr->setCentre( newvalue.asVector() );
			break;
		case (RegionVariable::CentreFrac):
			ptr->setCentreFrac( newvalue.asVector() );
			break;
		case (RegionVariable::Geometry):
			ptr->setGeometry( newvalue.asVector() );
			break;
		case (RegionVariable::GeometryFrac):
			ptr->setGeometryFrac( newvalue.asVector() );
			break;
		case (RegionVariable::Overlap):
			ptr->setAllowOverlap( newvalue.asBool() );
			break;
		case (RegionVariable::Shape):
			rs = ComponentRegion::regionShape(newvalue.asString());
			if (rs != ComponentRegion::nRegionShapes) ptr->setShape(rs);
			else result = FALSE;
			break;
		case (RegionVariable::XRotation):
		case (RegionVariable::YRotation):
			v = ptr->rotations();
			v.set(acc-RegionVariable::XRotation, newvalue.asDouble());
			ptr->setRotations(v);
			break;
		default:
			printf("RegionVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("RegionVariable::setAccessor");
	return result;
}

// Perform desired function
bool RegionVariable::performFunction(int i, ReturnValue &rv, TreeNode *node)
{
	msg.enter("RegionVariable::performFunction");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nFunctions))
	{
		printf("Internal Error: FunctionAccessor id %i is out of range for Region type.\n", i);
		msg.exit("RegionVariable::performFunction");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	ComponentRegion *ptr= (ComponentRegion*) rv.asPointer(VTypes::RegionData, result);
	if (result) switch (i)
	{
		default:
			printf("Internal Error: Access to function '%s' has not been defined in RegionVariable.\n", functionData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("RegionVariable::performFunction");
	return result;
}

/*
// Variable Array
*/

// Constructor
RegionArrayVariable::RegionArrayVariable(TreeNode *sizeexpr, bool constant)
{
	// Private variables
	returnType_ = VTypes::RegionData;
	pointerArrayData_ = NULL;
	arraySize_ = 0;
	nodeType_ = TreeNode::ArrayVarNode;
	readOnly_ = constant;
	arraySizeExpression_ = sizeexpr;
}

// Search variable access list for provided accessor
StepNode *RegionArrayVariable::findAccessor(const char *s, TreeNode *arrayindex, TreeNode *arglist)
{
	return RegionVariable::accessorSearch(s, arrayindex, arglist);
}

