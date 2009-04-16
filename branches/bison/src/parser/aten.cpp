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
AtenVariable::AtenVariable()
{
	// Private variables
	returnType_ = VTypes::AtenData;
	readOnly_ = TRUE;
}

// Destructor
AtenVariable::~AtenVariable()
{
}

/*
// Set / Get
*/

// Set value of variable
bool AtenVariable::set(ReturnValue &rv)
{
	msg.print("A constant value (in this case Aten itself) cannot be assigned to.\n");
	return FALSE;
}

// Reset variable
void AtenVariable::reset()
{
	// No action
}

// Return value of node
bool AtenVariable::execute(ReturnValue &rv)
{
	rv.set(VTypes::AtenData, &aten);
	return TRUE;
}

// Print node contents
void AtenVariable::nodePrint(int offset, const char *prefix)
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
Accessor AtenVariable::accessorData[AtenVariable::nAccessors] = {
	{ "model",	VTypes::ModelData,	FALSE, TRUE },
	{ "elements",	VTypes::ElementsData,	FALSE, TRUE },
	{ "models",	VTypes::ModelData,	TRUE, TRUE }
};

// Search variable access list for provided accessor (call private static function)
StepNode *AtenVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return AtenVariable::accessorSearch(s, arrayindex);
}

// Private static function to search accessors
StepNode *AtenVariable::accessorSearch(const char *s, TreeNode *arrayindex)
{
	msg.enter("AtenVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'aten&' has no member named '%s'.\n", s);
		msg.exit("AtenVariable::accessorSearch");
		return NULL;
	}
	// Create a suitable AccessNode to return...
	msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
	result = new StepNode(i, VTypes::AtenData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly);
	msg.exit("AtenVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool AtenVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("AtenVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Aten type.\n");
		msg.exit("AtenVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((!accessorData[i].isArray) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("AtenVariable::retrieveAccessor");
		return FALSE;
	}
	// Variables used in retrieval
	Model *m;
	bool result = TRUE;
	if (result) switch (acc)
	{
		case (AtenVariable::CurrentModel):
			rv.set(VTypes::ModelData, aten.currentModel());
			break;
		case (AtenVariable::Elements):
			rv.set(VTypes::ElementsData, &elements());
			break;
		case (AtenVariable::Models):
			m = aten.model(arrayIndex-1);
			if (m == NULL) result = FALSE;
			else rv.set(VTypes::ModelData, m);
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in AtenVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("AtenVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool AtenVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("AtenVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Aten type.\n");
		msg.exit("AtenVariable::setAccessor");
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
		msg.exit("AtenVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue
	bool result = TRUE;
	Aten *ptr= (Aten*) sourcerv.asPointer(VTypes::AtenData, result);
	switch (acc)
	{
		default:
			printf("AtenVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("AtenVariable::setAccessor");
	return result;
}
