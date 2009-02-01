/*
	*** Bond Access
	*** src/variables/bondaccess.cpp
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

#include "variables/bondaccess.h"
#include "variables/accessstep.h"
#include "variables/vaccess.h"
#include "base/bond.h"
#include "base/messenger.h"

BondAccessors bondAccessors;

// Constructor
BondAccessors::BondAccessors()
{
	accessorPointers[BondAccessors::I] = addAccessor("i",		VTypes::AtomData, TRUE);
	accessorPointers[BondAccessors::J] = addAccessor("j",		VTypes::AtomData, TRUE);
	accessorPointers[BondAccessors::Order] = addAccessor("order",	VTypes::RealData, TRUE);
	accessorPointers[BondAccessors::Type] = addAccessor("type",	VTypes::CharacterData, TRUE);
};

// Retrieve specified data
bool BondAccessors::retrieve(void *classptr, AccessStep *step, ReturnValue &rv)
{
	msg.enter("BondAccessors::retrieve");
	bool result = TRUE;
	// Cast pointer into Bond*
	Bond *b = (Bond*) classptr;
	if (b == NULL) printf("Warning - NULL Bond pointer passed to BondAccessors::retrieve.\n");
// 	printf("Enumerated ID supplied to BondAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > BondAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to BondAccessors::set.\n", vid);
		msg.exit("BondAccessors::retrieve");
		return FALSE;
	}
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("BondAccessors::retrieve");
		return FALSE;
	}
	// Retrieve value based on enumerated id
	switch (vid)
	{
		case (BondAccessors::I):
			rv.set(b->atomI(), VTypes::AtomData);
			break;
		case (BondAccessors::J):
			rv.set(b->atomJ(), VTypes::AtomData);
			break;
		case (BondAccessors::Order):
			rv.set(b->order());
			break;
		case (BondAccessors::Type):
			rv.set(Bond::bondType(b->type()));
			break;
		default:
			printf("BondAccessors::retrieve doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("BondAccessors::retrieve");
	return result;
}

// Set specified data
bool BondAccessors::set(void *classptr, AccessStep *step, Variable *srcvar)
{
	msg.enter("BondAccessors::set");
	bool result = TRUE;
	// Cast pointer into Bond*
	Bond *i = (Bond*) classptr;
	if (i == NULL) printf("Warning - NULL Bond pointer passed to BondAccessors::set.\n");
// 	printf("Enumerated ID supplied to BondAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > BondAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to BondAccessors::set.\n", vid);
		msg.exit("BondAccessors::set");
		return FALSE;
	}
	// Check read-only status
	if (accessorPointers[vid]->readOnly())
	{
		msg.print("Member '%s' of 'bond' type is read-only.\n", accessorPointers[vid]->name());
		msg.exit("BondAccessors::set");
		return FALSE;
	}
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("BondAccessors::set");
		return FALSE;
	}
	// Set value based on enumerated id
	switch (vid)
	{
		default:
			printf("BondAccessors::set doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("BondAccessors::set");
	return result;
}
