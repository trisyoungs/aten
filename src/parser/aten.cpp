/*
	*** Aten Variable
	*** src/parser/aten.cpp
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

#include "parser/aten.h"
#include "parser/stepnode.h"
#include "base/constants.h"
#include "main/aten.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Constructors
NuAtenVariable::NuAtenVariable()
{
	// Private variables
	returnType_ = NuVTypes::AtenData;
	readOnly_ = TRUE;
}

// Destructor
NuAtenVariable::~NuAtenVariable()
{
}

/*
// Set / Get
*/

// Set value of variable
bool NuAtenVariable::set(NuReturnValue &rv)
{
	msg.print("A constant value (in this case Aten itself) cannot be assigned to.\n");
	return FALSE;
}

// Reset variable
void NuAtenVariable::reset()
{
	// No action
}

// Return value of node
bool NuAtenVariable::execute(NuReturnValue &rv)
{
	rv.set(NuVTypes::AtenData, &aten);
	return TRUE;
}

// Print node contents
void NuAtenVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	printf("[V]%s&%li (Aten) (constant value)\n", tab, &aten);
	delete[] tab;
}

/*
// Accessors
*/

// Accessor data
Accessor NuAtenVariable::accessorData[NuAtenVariable::nAccessors] = {
	{ "model",	NuVTypes::ModelData,	TRUE, FALSE },
	{ "models",	NuVTypes::ModelData,	TRUE, TRUE }
};

// Search variable access list for provided accessor (call private static function)
StepNode *NuAtenVariable::findAccessor(const char *s)
{
	return NuAtenVariable::accessorSearch(s);
}

// Private static function to search accessors
StepNode *NuAtenVariable::accessorSearch(const char *s)
{
	msg.enter("NuAtenVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'aten&' has no member named '%s'.\n", s);
		msg.exit("NuAtenVariable::accessorSearch");
		return NULL;
	}
	// Create a suitable AccessNode to return...
	printf("Accessor match = %i\n", i);
	result = new StepNode(i, NuVTypes::AtenData, accessorData[i].returnType);
	msg.exit("NuAtenVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool NuAtenVariable::retrieveAccessor(int i, NuReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("NuAtenVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Aten type.\n");
		msg.exit("NuAtenVariable::retrieveAccessor");
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
		msg.exit("NuAtenVariable::retrieveAccessor");
		return FALSE;
	}
	// Variables used in retrieval
	Model *m;
	bool result;
	if (result) switch (acc)
	{
		case (NuAtenVariable::CurrentModel):
			rv.set(NuVTypes::ModelData, aten.currentModel());
			break;
		case (NuAtenVariable::Models):
			m = aten.model(arrayIndex-1);
			if (m == NULL) result = FALSE;
			else rv.set(NuVTypes::ModelData, m);
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in AtenVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("NuAtenVariable::retrieveAccessor");
	return result;
}
