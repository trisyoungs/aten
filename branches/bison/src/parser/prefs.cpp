/*
	*** Prefs Variable
	*** src/parser/prefs.cpp
	Copyright T. Youngs 2007-2009

	This file is part of Aten.

	Prefs is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	Prefs is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with Prefs.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "parser/prefs.h"
#include "parser/stepnode.h"
#include "base/constants.h"
#include "classes/prefs.h"
#include "parser/commandnode.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Constructors
PrefsVariable::PrefsVariable()
{
	// Private variables
	returnType_ = VTypes::PreferencesData;
	readOnly_ = TRUE;
}

// Destructor
PrefsVariable::~PrefsVariable()
{
}

/*
// Set / Get
*/

// Set value of variable
bool PrefsVariable::set(ReturnValue &rv)
{
	msg.print("A constant value (in this case the Prefs) cannot be assigned to.\n");
	return FALSE;
}

// Reset variable
void PrefsVariable::reset()
{
	// No action
}

// Return value of node
bool PrefsVariable::execute(ReturnValue &rv)
{
	rv.set(VTypes::PreferencesData, &prefs);
	return TRUE;
}

// Print node contents
void PrefsVariable::nodePrint(int offset, const char *prefix)
{
	// Construct tabbed offset
	char *tab;
	tab = new char[offset+32];
	tab[0] = '\0';
	for (int n=0; n<offset-1; n++) strcat(tab,"\t");
	if (offset > 1) strcat(tab,"   |--> ");
	strcat(tab,prefix);
	// Output node data
	printf("[V]%s&%li (Prefs) (constant value)\n", tab, &prefs);
	delete[] tab;
}

/*
// Accessors
*/

// Accessor data - name, type, array?, ro?
Accessor PrefsVariable::accessorData[PrefsVariable::nAccessors] = {
	{ "energyunit",	VTypes::StringData,	FALSE, FALSE }
};

// Search variable access list for provided accessor (call private static function)
StepNode *PrefsVariable::findAccessor(const char *s, TreeNode *arrayindex)
{
	return PrefsVariable::accessorSearch(s, arrayindex);
}

// Private static function to search accessors
StepNode *PrefsVariable::accessorSearch(const char *s, TreeNode *arrayindex)
{
	msg.enter("PrefsVariable::accessorSearch");
	StepNode *result = NULL;
	int i = 0;
	for (i = 0; i < nAccessors; i++) if (strcmp(accessorData[i].name,s) == 0) break;
	if (i == nAccessors)
	{
		msg.print("Error: Type 'aten&' has no member named '%s'.\n", s);
		msg.exit("PrefsVariable::accessorSearch");
		return NULL;
	}
	// Create a suitable AccessNode to return...
	msg.print(Messenger::Parse, "Accessor match = %i (%s)\n", i, accessorData[i].name);
	result = new StepNode(i, VTypes::PreferencesData, arrayindex, accessorData[i].returnType, accessorData[i].isReadOnly);
	msg.exit("PrefsVariable::accessorSearch");
	return result;
}

// Retrieve desired value
bool PrefsVariable::retrieveAccessor(int i, ReturnValue &rv, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("PrefsVariable::retrieveAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Prefs type.\n");
		msg.exit("PrefsVariable::retrieveAccessor");
		return FALSE;
	}
	Accessors acc = (Accessors) i;
	// Check for correct lack/presence of array index given
	if ((!accessorData[i].isArray) && hasArrayIndex)
	{
		msg.print("Error: Unnecessary array index provided for member '%s'.\n", accessorData[i].name);
		msg.exit("PrefsVariable::retrieveAccessor");
		return FALSE;
	}
	// Variables used in retrieval
	Model *m;
	bool result = TRUE;
	if (result) switch (acc)
	{
		case (PrefsVariable::EnergyUnit):
			rv.set(Prefs::energyUnit(prefs.energyUnit()));
			break;
		default:
			printf("Internal Error: Access to member '%s' has not been defined in PrefsVariable.\n", accessorData[i].name);
			result = FALSE;
			break;
	}
	msg.exit("PrefsVariable::retrieveAccessor");
	return result;
}

// Set desired value
bool PrefsVariable::setAccessor(int i, ReturnValue &sourcerv, ReturnValue &newvalue, bool hasArrayIndex, int arrayIndex)
{
	msg.enter("PrefsVariable::setAccessor");
	// Cast 'i' into Accessors enum value
	if ((i < 0) || (i >= nAccessors))
	{
		printf("Internal Error: Accessor id %i is out of range for Prefs type.\n");
		msg.exit("PrefsVariable::setAccessor");
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
		msg.exit("PrefsVariable::setAccessor");
		return FALSE;
	}
	// Get current data from ReturnValue (as a sanity check - we don't actually use the pointer)
	bool result = TRUE;
	Prefs *ptr= (Prefs*) sourcerv.asPointer(VTypes::PreferencesData, result);
	switch (acc)
	{
		case (PrefsVariable::EnergyUnit):
			// Call the
			if (!CommandNode::run(Command::EnergyUnits, "s", newvalue.asString())) result = FALSE;
			sourcerv.set(Prefs::energyUnit(prefs.energyUnit()));
			break;
		default:
			printf("PrefsVariable::setAccessor doesn't know how to use member '%s'.\n", accessorData[acc].name);
			result = FALSE;
			break;
	}
	msg.exit("PrefsVariable::setAccessor");
	return result;
}
