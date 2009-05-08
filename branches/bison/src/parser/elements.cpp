/*
	*** Elements Variable
	*** src/parser/elements.cpp
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

#include "parser/elements.h"
#include "parser/stepnode.h"
#include "base/elements.h"
// #include <stdio.h>
// #include <stdlib.h>
#include <string.h>

// Constructor
ElementVariable::ElementVariable()
{
	// Private variables
	returnType_ = VTypes::ElementData;
	readOnly_ = TRUE;
}

// Destructor
ElementVariable::~ElementVariable()
{
}

/*
// Accessors
*/

// Accessor data
Accessor ElementVariable::accessorData[ElementVariable::nAccessors] = {
	{ "ambient",	VTypes::DoubleData,	4, FALSE },
	{ "colour",	VTypes::DoubleData,	4, FALSE },
	{ "diffuse",	VTypes::DoubleData,	4, FALSE },
	{ "mass",	VTypes::DoubleData,	0, TRUE },
	{ "name",	VTypes::DoubleData,	0, TRUE },
	{ "radius",	VTypes::DoubleData,	0, FALSE },
	{ "symbol",	VTypes::StringData,	0, TRUE }
};

// Search variable access list for provided accessor (call private static function)
StepNode *ElementVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return ElementVariable::accessorSearch(s, arrayindex);
}

// Private static function to search accessors
StepNode *ElementVariable::accessorSearch(const char *s, TreeNode *arrayindex)
{
	msg.enter("ElementVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'elements&' has no member named '%s'.\n", s);
		msg.exit("ElementVariable::accessorSearch");
		return NULL;
	}
	msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
	// Were we given an array index when we didn't want one?
	if ((accessorData[i].arraySize == 0) && (arrayindex != NULL))
	{
		msg.print("Error: Irrelevant array index provided for member '%s'.\n", accessorData[i].name);
		result = NULL;
	}
	else result = new StepNode(i, VTypes::ElementData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly, accessorData[i].arraySize != 0);
	msg.exit("ElementVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ElementVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ElementVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Elements type.\n");
		msg.exit("ElementVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((accessorData[i].arraySize == 0) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("ElementVariable::retrieveAccessor");
		return FALSE;
	}
	else if ((accessorData[i].arraySize > 0) && (hasArrayIndex))
	{
		if ((arrayIndex < 1) || (arrayIndex > accessorData[i].arraySize))
		{
			msg.print("Error: Array index out of bounds for member '%s' (%i, range is 1-%i).\n", accessorData[i].name, arrayIndex, accessorData[i].arraySize);
			msg.exit("ElementVariable::retrieveAccessor");
			return FALSE;
		}
	}
	// Get current data from ReturnValue
	bool result;
	Element *ptr = (Element*) rv.asPointer(VTypes::ElementData, result);
	if (result) switch (acc)
	{
		case (ElementVariable::Colour):
		case (ElementVariable::Ambient):
			if (hasArrayIndex) rv.set( ptr->ambientColour[arrayIndex-1] );
			else rv.set( VTypes::DoubleData, &ptr->ambientColour, 4);
			break;
		case (ElementVariable::Diffuse):
			if (hasArrayIndex) rv.set( ptr->diffuseColour[arrayIndex-1] );
			else rv.set( VTypes::DoubleData, &ptr->diffuseColour, 4);
			break;
		case (ElementVariable::Mass):
			rv.set( ptr->atomicMass );
			break;
		case (ElementVariable::Name):
			rv.set( ptr->name );
			break;
		case (ElementVariable::Radius):
			rv.set( ptr->atomicRadius );
			break;
		case (ElementVariable::Symbol):
			rv.set( ptr->symbol );
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ElementVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ElementVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool ElementVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ElementVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Elements type.\n");
		msg.exit("ElementVariable::setAccessor");
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
		msg.exit("ElementVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	Element *ptr = (Element*) sourcerv.asPointer(VTypes::ElementData, result);
	int n;
	if (result) switch (acc)
	{
		case (ElementVariable::Ambient):
			if (newvalue.arraySize() == 4) for (n=0; n<4; ++n) ptr->ambientColour[n] = newvalue.asDouble(n, result);
			else if (hasArrayIndex) ptr->ambientColour[arrayIndex-1] = newvalue.asDouble(result);
			else for (n=0; n<4; ++n) ptr->ambientColour[n] = newvalue.asDouble(result);
			break;
		case (ElementVariable::Colour):
			if (newvalue.arraySize() == 4) for (n=0; n<4; ++n)
			{
				ptr->ambientColour[n] = newvalue.asDouble(n, result);
				ptr->diffuseColour[n] = newvalue.asDouble(n, result) * 0.75;
			}
			else if (hasArrayIndex)
			{
				ptr->ambientColour[arrayIndex-1] = newvalue.asDouble(result);
				ptr->diffuseColour[arrayIndex-1] = newvalue.asDouble(result) * 0.75;
			}
			else for (n=0; n<4; ++n)
			{
				ptr->ambientColour[n] = newvalue.asDouble(result);
				ptr->diffuseColour[n] = newvalue.asDouble(result) * 0.75;
			}
			break;
		case (ElementVariable::Diffuse):
			if (newvalue.arraySize() == 4) for (n=0; n<4; ++n) ptr->diffuseColour[n] = newvalue.asDouble(n, result);
			else if (hasArrayIndex) ptr->diffuseColour[arrayIndex-1] = newvalue.asDouble(result);
			else for (n=0; n<4; ++n) ptr->diffuseColour[n] = newvalue.asDouble(result);
			break;
		case (ElementVariable::Radius):
			ptr->atomicRadius = newvalue.asDouble(result);
			break;
		default:
			printf("ElementVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("ElementVariable::setAccessor");
	return result;
}
