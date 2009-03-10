/*
	*** ForcefieldBound Variable
	*** src/parser/forcefieldbound.cpp
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

#include "parser/forcefieldbound.h"
#include "parser/stepnode.h"
#include "base/atom.h"
#include "base/constants.h"
#include "base/elements.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Constructor
ForcefieldBoundVariable::ForcefieldBoundVariable(ForcefieldBound *ptr, bool constant) : ffboundData_(ptr)
{
	// Private variables
	returnType_ = NuVTypes::ForcefieldBoundData;
	readOnly_ = constant;
}

// Destructor
ForcefieldBoundVariable::~ForcefieldBoundVariable()
{
}

/*
// Set / Get
*/

// Set value of variable
bool ForcefieldBoundVariable::set(NuReturnValue &rv)
{
	if (readOnly_)
	{
		msg.print("A constant value (in this case a ffbound&) cannot be assigned to.\n");
		return FALSE;
	}
	bool success;
	ffboundData_ = rv.asPointer(NuVTypes::ForcefieldBoundData, success);
	return success;
}

// Reset variable
void ForcefieldBoundVariable::reset()
{
	ffboundData_ = NULL;
}

// Return value of node
bool ForcefieldBoundVariable::execute(NuReturnValue &rv)
{
	// If this vector is a constant, read the three stored expressions to recreate it
	rv.set(NuVTypes::ForcefieldBoundData, ffboundData_);
	return TRUE;
}

// Print node contents
void ForcefieldBoundVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	if (readOnly_) printf("[C]%s&%li (ffbound) (constant value)\n", tab, ffboundData_);
	else printf("[V]%s&%li (ffbound) (variable, name=%s)\n", tab, ffboundData_, name_.get());
	delete[] tab;
}

/*
// Accessors
*/

// Accessor data
Accessor ForcefieldBoundVariable::accessorData[ForcefieldBoundVariable::nAccessors] = {
	{ "data",	NuVTypes::RealData,		TRUE, FALSE },
        { "form",	NuVTypes::CharacterData,	FALSE, FALSE },
        { "natoms",	NuVTypes::IntegerData,		FALSE, TRUE },
        { "type",	NuVTypes::CharacterData,	FALSE, FALSE },
        { "typenames",	NuVTypes::CharacterData, 	TRUE, FALSE }
};

// Search variable access list for provided accessor (call private static function)
StepNode *ForcefieldBoundVariable::findAccessor(const char *s, bool array)
{
	return ForcefieldBoundVariable::accessorSearch(s, array);
}

// Private static function to search accessors
StepNode *ForcefieldBoundVariable::accessorSearch(const char *s, bool array)
{
	msg.enter("ForcefieldBoundVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'ffbound&' has no member named '%s'.\n", s);
		msg.exit("ForcefieldBoundVariable::accessorSearch");
		return NULL;
	}
	// Create a suitable AccessNode to return...
	printf("Accessor match = %i\n", i);
	result = new StepNode(i, NuVTypes::ForcefieldBoundData, accessorData[i].returnType, accessorData[i].isReadOnly);
	msg.exit("ForcefieldBoundVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool ForcefieldBoundVariable::retrieveAccessor(int i, NuReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("ForcefieldBoundVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for ForcefieldBound type.\n");
		msg.exit("ForcefieldBoundVariable::retrieveAccessor");
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
		msg.exit("ForcefieldBoundVariable::retrieveAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	ForcefieldBound *ptr= (ForcefieldBound*) rv.asPointer(NuVTypes::ForcefieldBoundData, result);
	if (result) switch (acc)
	{
		default:
			printf("Internal Error: Access to member '%s' has not been defined in ForcefieldBoundVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("ForcefieldBoundVariable::retrieveAccessor");
	return result;
}

