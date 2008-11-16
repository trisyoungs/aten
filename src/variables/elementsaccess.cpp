/*
	*** Elements Access
	*** src/variables/elementsaccess.cpp
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
#include "variables/elementsaccess.h"
#include "variables/accessstep.h"
#include "variables/vaccess.h"
#include "base/elements.h"
#include "base/messenger.h"

ElementsAccessors elementsAccessors;

// Constructor
ElementsAccessors::ElementsAccessors()
{
	accessorPointers[ElementsAccessors::Mass] = addAccessor("mass", VTypes::RealData, TRUE, elements.nElements());
	accessorPointers[ElementsAccessors::Name] = addAccessor("name", VTypes::CharacterData, TRUE, elements.nElements());
	accessorPointers[ElementsAccessors::NElements] = addAccessor("nelements", VTypes::IntegerData, TRUE);
	accessorPointers[ElementsAccessors::Symbol] = addAccessor("symbol", VTypes::CharacterData, TRUE, elements.nElements());
};

// Retrieve specified data
bool ElementsAccessors::retrieve(void *classptr, AccessStep *step, ReturnValue &rv)
{
	msg.enter("ElementsAccessors::retrieve");
	bool result = TRUE;
	// We don't need to cast the classptr since we use the global singleton
// 	printf("Enumerated ID supplied to ElementsAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > ElementsAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to ElementsAccessors::set.\n", vid);
		msg.exit("ElementsAccessors::retrieve");
		return FALSE;
	} 
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("ElementsAccessors::retrieve");
		return FALSE;
	}
	// Retrieve value based on enumerated id
	switch (vid)
	{
		case (ElementsAccessors::Mass):
			rv.set(elements.atomicMass(index));
			break;
		case (ElementsAccessors::Name):
			rv.set(elements.name(index));
			break;
		case (ElementsAccessors::NElements):
			rv.set(elements.nElements());
			break;
		case (ElementsAccessors::Symbol):
			rv.set(elements.symbol(index));
			break;
		default:
			printf("ElementsAccessors::retrieve doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("ElementsAccessors::retrieve");
	return result;
}

// Set specified data
bool ElementsAccessors::set(void *classptr, AccessStep *step, Variable *srcvar)
{
	msg.enter("ElementsAccessors::set");
	bool result = TRUE;
	// We don't need to cast the classptr since we use the global singleton
// 	printf("Enumerated ID supplied to ElementsAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > ElementsAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to ElementsAccessors::set.\n", vid);
		msg.exit("ElementsAccessors::set");
		return FALSE;
	}
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("ElementsAccessors::set");
		return FALSE;
	}
	// Set value based on enumerated id
	switch (vid)
	{
		default:
			printf("ElementsAccessors::set doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("ElementsAccessors::set");
	return result;
}
