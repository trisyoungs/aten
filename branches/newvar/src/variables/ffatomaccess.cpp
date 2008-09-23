/*
	*** ForcefieldAtom Access
	*** src/variables/ffatomacess.cpp
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

#include "variables/ffatomaccess.h"
#include "variables/vaccess.h"
#include "classes/forcefieldatom.h"
#include "base/elements.h"
#include "base/messenger.h"

FFAtomAccessors ffatomAccessors;

// Constructor
FFAtomAccessors::FFAtomAccessors()
{
	accessorPointers[FFAtomAccessors::Atomtype] = addAccessor("atomtype",		VTypes::CharacterData, FALSE);
	accessorPointers[FFAtomAccessors::Charge] = addAccessor("charge",		VTypes::RealData, FALSE);
	accessorPointers[FFAtomAccessors::Data] = addAccessor("data",		VTypes::RealData, FALSE);
	accessorPointers[FFAtomAccessors::Description] = addAccessor("description",		VTypes::CharacterData, FALSE);
	accessorPointers[FFAtomAccessors::Equivalent] = addAccessor("equivalent",		VTypes::CharacterData, FALSE);
	accessorPointers[FFAtomAccessors::Id] = addAccessor("id",		VTypes::IntegerData, FALSE);
	accessorPointers[FFAtomAccessors::Name] = addAccessor("name",		VTypes::CharacterData, FALSE);
 	accessorPointers[FFAtomAccessors::ParentFF] = addAccessor("ff",		VTypes::ForcefieldData, TRUE);
};

// Retrieve specified data
bool FFAtomAccessors::retrieve(void *classptr, int vid, ReturnValue &rv)
{
	msg.enter("FFAtomAccessors::retrieve");
	bool result = TRUE;
	// Cast pointer into ForcefieldAtom*
	ForcefieldAtom *i = (ForcefieldAtom*) classptr;
	if (i == NULL) printf("Warning - NULL ForcefieldAtom pointer passed to FFAtomAccessors::retrieve.\n");
// 	printf("Enumerated ID supplied to FFAtomAccessors is %i.\n", vid);
	// Check range of supplied vid
	if ((vid < 0) || (vid > FFAtomAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to FFAtomAccessors::set.\n", vid);
		msg.exit("FFAtomAccessors::set");
		return FALSE;
	} 
	// Retrieve value based on enumerated id
	switch (vid)
	{
		case (FFAtomAccessors::Charge):
			rv.set(i->charge());
			break;
		default:
			printf("Unknown enumeration %i given to FFAtomAccessors::retrieve.\n", vid);
			result = FALSE;
			break;
	}
	msg.exit("FFAtomAccessors::retrieve");
	return result;
}

// Set specified data
bool FFAtomAccessors::set(void *classptr, int vid, Variable *srcvar)
{
	msg.enter("FFAtomAccessors::set");
	bool result = TRUE;
	// Cast pointer into ForcefieldAtom*
	ForcefieldAtom *i = (ForcefieldAtom*) classptr;
	if (i == NULL) printf("Warning - NULL ForcefieldAtom pointer passed to FFAtomAccessors::set.\n");
// 	printf("Enumerated ID supplied to FFAtomAccessors is %i.\n", vid);
	// Check range of supplied vid
	if ((vid < 0) || (vid > FFAtomAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to FFAtomAccessors::set.\n", vid);
		msg.exit("FFAtomAccessors::set");
		return FALSE;
	} 
	// Set value based on enumerated id
	switch (vid)
	{
		case (FFAtomAccessors::Charge):
			i->setCharge(srcvar->asDouble());
			break;
		case (FFAtomAccessors::Charge):
			msg.print("Member '%s' in ForcefieldAtom is read-only.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
		default:
			break;
	}
	msg.exit("FFAtomAccessors::set");
	return result;
}
