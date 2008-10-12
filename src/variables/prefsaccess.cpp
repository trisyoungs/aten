/*
	*** Prefs Access
	*** src/variables/prefsaccess.cpp
	Copyright T. Youngs 2007,2008

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

#include "main/aten.h"
#include "variables/prefsaccess.h"
#include "variables/accessstep.h"
#include "variables/vaccess.h"
#include "command/command.h"
#include "model/model.h"
#include "base/elements.h"
#include "base/messenger.h"
#include "classes/prefs.h"

PrefsAccessors prefsAccessors;

// Constructor
PrefsAccessors::PrefsAccessors()
{
 	accessorPointers[PrefsAccessors::EnergyUnit] = addAccessor("energyunit",	VTypes::CharacterData,	FALSE);
};

// Retrieve specified data
bool PrefsAccessors::retrieve(void *classptr, AccessStep *step, ReturnValue &rv)
{
	msg.enter("PrefsAccessors::retrieve");
	bool result = TRUE;
	// We don't need to cast the classptr since we use the global singleton
// 	printf("Enumerated ID supplied to PrefsAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > PrefsAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to PrefsAccessors::set.\n", vid);
		msg.exit("PrefsAccessors::retrieve");
		return FALSE;
	} 
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("PrefsAccessors::retrieve");
		return FALSE;
	}
	// Retrieve value based on enumerated id
	switch (vid)
	{
		case (PrefsAccessors::EnergyUnit):
			rv.set(Prefs::energyUnit(prefs.energyUnit()));
			break;
		default:
			printf("PrefsAccessors::retrieve doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("PrefsAccessors::retrieve");
	return result;
}

// Set specified data
bool PrefsAccessors::set(void *classptr, AccessStep *step, Variable *srcvar)
{
	msg.enter("PrefsAccessors::set");
	bool result = TRUE;
	// We don't need to cast the classptr since we use the global singleton
// 	printf("Enumerated ID supplied to PrefsAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > PrefsAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to PrefsAccessors::set.\n", vid);
		msg.exit("PrefsAccessors::set");
		return FALSE;
	}
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("PrefsAccessors::set");
		return FALSE;
	}
	CommandNode *c = new CommandNode;
	// Set value based on enumerated id
	switch (vid)
	{
		case (PrefsAccessors::EnergyUnit):
			c->addConstant(srcvar->asCharacter(), TRUE);
			if (commands.call(Command::CA_ENERGYUNITS, c) != Command::Success) result = FALSE;
			break;
		default:
			printf("PrefsAccessors::set doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	delete c;
	msg.exit("PrefsAccessors::set");
	return result;
}
