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
// Set / Get
*/

// Set value of variable
bool ElementVariable::set(ReturnValue &rv)
{
	msg.print("A constant value (in this case the Elements table) cannot be assigned to.\n");
	return FALSE;
}

// Reset variable
void ElementVariable::reset()
{
}

// Return value of node
bool ElementVariable::execute(ReturnValue &rv)
{
	// If this vector is a constant, read the three stored expressions to recreate it
	rv.set(VTypes::ElementData, &elements());
	return TRUE;
}

// Print node contents
void ElementVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	printf("[C]%s&%li (elements) (constant value)\n", tab, &elements());
	delete[] tab;
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
	// Create a suitable AccessNode to return...
	msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
	result = new StepNode(i, VTypes::ElementData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly);
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
	// Get current data from ReturnValue
	bool result = TRUE;
	Element *ptr = (Element*) rv.asPointer(VTypes::ElementData, result);
	if (result) switch (acc)
	{
		case (ElementVariable::Mass):
			rv.set(elements().atomicMass(arrayIndex));
			break;
		case (ElementVariable::Name):
			rv.set(elements().name(arrayIndex));
			break;
		case (ElementVariable::Symbol):
			rv.set(elements().symbol(arrayIndex));
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
	// Check for correct lack/presence of array index given
	if (accessorData[i].arraySize == 0)
	{
		if (hasArrayIndex) msg.print("Warning: Irrelevant array index provided for member '%s'.\n", accessorData[i].name);
	}
	else if (!hasArrayIndex)
	{
		printf("size = %i\n", accessorData[i].arraySize);
		msg.print("Error: No array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("ElementVariable::setAccessor");
		return FALSE;
	}
	bool result = TRUE;
	switch (acc)
	{
		case (ElementVariable::Diffuse):
			printf("ARSE\n");
// 			else rv.set(elements().atomicMass(arrayIndex));
			break;
		default:
			printf("ElementVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("ElementVariable::setAccessor");
	return result;
}
