/*
	*** ForcefieldBound Access
	*** src/variables/ffboundaccess.cpp
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

#include "variables/ffboundaccess.h"
#include "variables/accessstep.h"
#include "variables/vaccess.h"
#include "classes/forcefieldbound.h"
#include "base/elements.h"
#include "base/messenger.h"

FFBoundAccessors ffboundAccessors;

// Constructor
FFBoundAccessors::FFBoundAccessors()
{
	accessorPointers[FFBoundAccessors::Data] = addAccessor("data",		VTypes::RealData, FALSE, MAXFFPARAMDATA);
	accessorPointers[FFBoundAccessors::Form] = addAccessor("form",		VTypes::CharacterData, FALSE);
	accessorPointers[FFBoundAccessors::TypeNames] = addListAccessor("typenames",	VTypes::CharacterData);
};

// Retrieve specified data
bool FFBoundAccessors::retrieve(void *classptr, AccessStep *step, ReturnValue &rv)
{
	msg.enter("FFBoundAccessors::retrieve");
	bool result = TRUE;
	// Cast pointer into ForcefieldBound*
	ForcefieldBound *ffb = (ForcefieldBound*) classptr;
	if (ffb == NULL) printf("Warning - NULL ForcefieldBound pointer passed to FFBoundAccessors::retrieve.\n");
// 	printf("Enumerated ID supplied to FFBoundAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > FFBoundAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to FFBoundAccessors::set.\n", vid);
		msg.exit("FFBoundAccessors::retrieve");
		return FALSE;
	}
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("FFBoundAccessors::retrieve");
		return FALSE;
	}
	// Retrieve value based on enumerated id
	switch (vid)
	{
		case (FFBoundAccessors::Data):
			rv.set(ffb->parameter(index-1));
			break;
		case (FFBoundAccessors::Form):
			rv.set(ffb->formText());
			break;
		default:
			printf("FFBoundAccessors::retrieve doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("FFBoundAccessors::retrieve");
	return result;
}

// Set specified data
bool FFBoundAccessors::set(void *classptr, AccessStep *step, Variable *srcvar)
{
	msg.enter("FFBoundAccessors::set");
	bool result = TRUE;
	// Cast pointer into ForcefieldBound*
	ForcefieldBound *ffb = (ForcefieldBound*) classptr;
	if (ffb == NULL) printf("Warning - NULL ForcefieldBound pointer passed to FFBoundAccessors::set.\n");
// 	printf("Enumerated ID supplied to FFBoundAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > FFBoundAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to FFBoundAccessors::set.\n", vid);
		msg.exit("FFBoundAccessors::set");
		return FALSE;
	} 
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("FFBoundAccessors::set");
		return FALSE;
	}
	// Set value based on enumerated id
	switch (vid)
	{
//		case (FFBoundAccessors::Data):
//			ffb->setCharge(srcvar->asDouble());
//			break;
		case (FFBoundAccessors::Data):
			msg.print("Member '%s' in ForcefieldBound is read-only.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
		default:
			printf("FFBoundAccessors::set doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("FFBoundAccessors::set");
	return result;
}
