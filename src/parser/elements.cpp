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
ElementsVariable::ElementsVariable()
{
	// Private variables
	returnType_ = NuVTypes::ElementsData;
	readOnly_ = TRUE;
}

// Destructor
ElementsVariable::~ElementsVariable()
{
}

/*
// Set / Get
*/

// Set value of variable
bool ElementsVariable::set(NuReturnValue &rv)
{
	msg.print("A constant value (in this case the Elements table) cannot be assigned to.\n");
	return FALSE;
}

// Reset variable
void ElementsVariable::reset()
{
}

// Return value of node
bool ElementsVariable::execute(NuReturnValue &rv)
{
	// If this vector is a constant, read the three stored expressions to recreate it
	rv.set(NuVTypes::ElementsData, &elements());
	return TRUE;
}

// Print node contents
void ElementsVariable::nodePrint(int offset, const char *prefix)
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
Accessor ElementsVariable::accessorData[ElementsVariable::nAccessors] = {
	{ "mass",	NuVTypes::RealData,	TRUE, TRUE },
	{ "name",	NuVTypes::RealData,	TRUE, TRUE },
	{ "nelements",	NuVTypes::IntegerData,	FALSE, TRUE },
	{ "symbol",	NuVTypes::StringData,	TRUE, TRUE }
};

// Search variable access list for provided accessor (call private static function)
StepNode *ElementsVariable::findAccessor(const char *s, bool array)
{
	return ElementsVariable::accessorSearch(s, array);
}

// Private static function to search accessors
StepNode *ElementsVariable::accessorSearch(const char *s, bool array)
{
	msg.enter("ElementsVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'elements&' has no member named '%s'.\n", s);
		msg.exit("ElementsVariable::accessorSearch");
		return NULL;
	}
	// Create a suitable AccessNode to return...
	printf("Accessor match = %i\n", i);
	result = new StepNode(i, NuVTypes::ElementsData, accessorData[i].returnType, accessorData[i].isReadOnly);
	msg.exit("ElementsVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ElementsVariable::retrieveAccessor(int i, NuReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ElementsVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Elements type.\n");
		msg.exit("ElementsVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if (!accessorData[i].isArray)
	{
		if (hasArrayIndex) msg.print("Warning: Irrelevent array index provided for member '%s'.\n", accessorData[i].name);
	}
	else if (!hasArrayIndex)
	{
		msg.print("Error: No array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("ElementsVariable::retrieveAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	if (result) switch (acc)
	{
		case (ElementsVariable::Mass):
			if ((arrayIndex < 1) || (arrayIndex > elements().nElements()))
			{
				msg.print("Array index [%i] is out of range for 'mass' member.\n", arrayIndex);
				result = FALSE;
			}
			else rv.set(elements().atomicMass(arrayIndex));
			break;
		case (ElementsVariable::Name):
			if ((arrayIndex < 1) || (arrayIndex > elements().nElements()))
			{
				msg.print("Array index [%i] is out of range for 'name' member.\n", arrayIndex);
				result = FALSE;
			}
			else rv.set(elements().name(arrayIndex));
			break;
		case (ElementsVariable::NElements):
			rv.set(elements().nElements());
			break;
		case (ElementsVariable::Symbol):
			if ((arrayIndex < 1) || (arrayIndex > elements().nElements()))
			{
				msg.print("Array index [%i] is out of range for 'symbol' member.\n", arrayIndex);
				result = FALSE;
			}
			else rv.set(elements().symbol(arrayIndex));
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ElementsVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ElementsVariable::retrieveAccessor");
	return result;
}

/*
// Set specified data
bool ElementsVariable::set(void *classptr, AccessStep *step, Variable *srcvar)
{
	msg.enter("ElementsVariable::set");
	bool result = TRUE;
	// We don't need to cast the classptr since we use the global singleton
// 	printf("Enumerated ID supplied to ElementsVariable is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > ElementsVariable::nAccessors))
	{
		printf("Unknown enumeration %i given to ElementsVariable::set.\n", vid);
		msg.exit("ElementsVariable::set");
		return FALSE;
	}
	// Check read-only status
	if (accessorPointers[vid]->readOnly())
	{
		msg.print("Member '%s' of 'elements' type is read-only.\n", accessorPointers[vid]->name());
		msg.exit("ElementsVariable::set");
		return FALSE;
	}
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("ElementsVariable::set");
		return FALSE;
	}
	// Set value based on enumerated id
	switch (vid)
	{
		default:
			printf("ElementsVariable::set doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("ElementsVariable::set");
	return result;
}
*/
