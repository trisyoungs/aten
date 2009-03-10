/*
	*** Forcefield Variable
	*** src/parser/forcefield.cpp
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

#include "parser/forcefield.h"
#include "parser/stepnode.h"
#include "base/atom.h"
#include "base/constants.h"
#include "base/elements.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Constructor
ForcefieldVariable::ForcefieldVariable(Forcefield *ptr, bool constant) : ffData_(ptr)
{
	// Private variables
	returnType_ = NuVTypes::ForcefieldData;
	readOnly_ = constant;
}

// Destructor
ForcefieldVariable::~ForcefieldVariable()
{
}

/*
// Set / Get
*/

// Set value of variable
bool ForcefieldVariable::set(NuReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case a forcefield&) cannot be assigned to.\n");
		return FALSE;
	}
	bool success;
	ffData_ = rv.asPointer(NuVTypes::ForcefieldData, success);
	return success;
}

// Reset variable
void ForcefieldVariable::reset()
{
	ffData_ = NULL;
}

// Return value of node
bool ForcefieldVariable::execute(NuReturnValue &rv)
{
	// If this vector is a constant, read the three stored expressions to recreate it
	rv.set(NuVTypes::ForcefieldData, ffData_);
	return TRUE;
}

// Print node contents
void ForcefieldVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	if (readOnly_) printf("[C]%s%li (forcefield) (constant value)\n", tab, ffData_);
	else printf("[V]%s%li (forcefield) (variable, name=%s)\n", tab, ffData_, name_.get());
	delete[] tab;
}

/*
// Accessors
*/

// Accessor data
Accessor ForcefieldVariable::accessorData[ForcefieldVariable::nAccessors] = {
// 	{ "i",		NuVTypes::AtomData,	 FALSE, TRUE },
};

// Search variable access list for provided accessor (call private static function)
StepNode *ForcefieldVariable::findAccessor(const char *s, bool array)
{
	return ForcefieldVariable::accessorSearch(s, array);
}

// Private static function to search accessors
StepNode *ForcefieldVariable::accessorSearch(const char *s, bool array)
{
	msg.enter("ForcefieldVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'forcefield&' has no member named '%s'.\n", s);
		msg.exit("ForcefieldVariable::accessorSearch");
		return NULL;
	}
	// Create a suitable AccessNode to return...
	printf("Accessor match = %i\n", i);
	result = new StepNode(i, NuVTypes::ForcefieldData, accessorData[i].returnType, accessorData[i].isReadOnly);
	msg.exit("ForcefieldVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ForcefieldVariable::retrieveAccessor(int i, NuReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ForcefieldVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Forcefield type.\n");
		msg.exit("ForcefieldVariable::retrieveAccessor");
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
		msg.exit("ForcefieldVariable::retrieveAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Forcefield *ptr= (Forcefield*) rv.asPointer(NuVTypes::ForcefieldData, result);
	if (result) switch (acc)
	{
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ForcefieldVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ForcefieldVariable::retrieveAccessor");
	return result;
}

