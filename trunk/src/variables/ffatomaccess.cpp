/*
	*** ForcefieldAtom Access
	*** src/variables/ffatomaccess.cpp
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
#include "variables/accessstep.h"
#include "variables/vaccess.h"
#include "classes/forcefieldatom.h"
#include "base/elements.h"
#include "base/messenger.h"

FFAtomAccessors ffatomAccessors;

// Constructor
FFAtomAccessors::FFAtomAccessors()
{
	accessorPointers[FFAtomAccessors::Atomtype] = addAccessor("atomtype",		VTypes::CharacterData, TRUE);
	accessorPointers[FFAtomAccessors::Charge] = addAccessor("charge",		VTypes::RealData, FALSE);
	accessorPointers[FFAtomAccessors::Data] = addAccessor("data",		VTypes::RealData, FALSE, MAXFFPARAMDATA);
	accessorPointers[FFAtomAccessors::Description] = addAccessor("description",		VTypes::CharacterData, FALSE);
	accessorPointers[FFAtomAccessors::Equivalent] = addAccessor("equivalent",		VTypes::CharacterData, FALSE);
	accessorPointers[FFAtomAccessors::Form] = addAccessor("form",		VTypes::CharacterData, FALSE);
	accessorPointers[FFAtomAccessors::Id] = addAccessor("id",		VTypes::IntegerData, TRUE);
	accessorPointers[FFAtomAccessors::Name] = addAccessor("name",		VTypes::CharacterData, FALSE);
 	accessorPointers[FFAtomAccessors::ParentFF] = addAccessor("ff",		VTypes::ForcefieldData, TRUE);
};

// Retrieve specified data
bool FFAtomAccessors::retrieve(void *classptr, AccessStep *step, ReturnValue &rv)
{
	msg.enter("FFAtomAccessors::retrieve");
	bool result = TRUE;
	// Cast pointer into ForcefieldAtom*
	ForcefieldAtom *ffa = (ForcefieldAtom*) classptr;
	if (ffa == NULL) printf("Warning - NULL ForcefieldAtom pointer passed to FFAtomAccessors::retrieve.\n");
// 	printf("Enumerated ID supplied to FFAtomAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > FFAtomAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to FFAtomAccessors::set.\n", vid);
		msg.exit("FFAtomAccessors::set");
		return FALSE;
	}
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("FFAtomAccessors::set");
		return FALSE;
	}
	// Retrieve value based on enumerated id
	switch (vid)
	{
		case (FFAtomAccessors::Atomtype):
			rv.set(ffa->atomtypeString());
			break;
		case (FFAtomAccessors::Charge):
			rv.set(ffa->charge());
			break;
		case (FFAtomAccessors::Data):
			rv.set(ffa->parameter(index-1));
			break;
		case (FFAtomAccessors::Description):
			rv.set(ffa->description());
			break;
		case (FFAtomAccessors::Equivalent):
			rv.set(ffa->equivalent());
			break;
		case (FFAtomAccessors::Form):
			rv.set(VdwFunctions::VdwFunctions[ffa->vdwForm()].keyword);
			break;
		case (FFAtomAccessors::Id):
			rv.set(ffa->typeId());
			break;
		case (FFAtomAccessors::Name):
			rv.set(ffa->name());
			break;
		case (FFAtomAccessors::ParentFF):
			rv.set(ffa->parent(), VTypes::ForcefieldData);
			break;
		default:
			printf("FFAtomAccessors::retrieve doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("FFAtomAccessors::retrieve");
	return result;
}

// Set specified data
bool FFAtomAccessors::set(void *classptr, AccessStep *step, Variable *srcvar)
{
	msg.enter("FFAtomAccessors::set");
	bool result = TRUE;
	VdwFunctions::VdwFunction vf;
	// Cast pointer into ForcefieldAtom*
	ForcefieldAtom *ffa = (ForcefieldAtom*) classptr;
	if (ffa == NULL) printf("Warning - NULL ForcefieldAtom pointer passed to FFAtomAccessors::set.\n");
// 	printf("Enumerated ID supplied to FFAtomAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > FFAtomAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to FFAtomAccessors::set.\n", vid);
		msg.exit("FFAtomAccessors::set");
		return FALSE;
	}
	// Check read-only status
	if (accessorPointers[vid]->readOnly())
	{
		msg.print("Member '%s' of 'prefs' type is read-only.\n", accessorPointers[vid]->name());
		msg.exit("PrefsAccessors::set");
		return FALSE;
	}
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("FFAtomAccessors::set");
		return FALSE;
	}
	// Set value based on enumerated id
	switch (vid)
	{
		case (FFAtomAccessors::Charge):
			ffa->setCharge(srcvar->asDouble());
			break;
		case (FFAtomAccessors::Data):
			ffa->setParameter(index-1, srcvar->asDouble());
			break;
		case (FFAtomAccessors::Description):
			ffa->setDescription(srcvar->asCharacter());
			break;
		case (FFAtomAccessors::Equivalent):
			ffa->setEquivalent(srcvar->asCharacter());
			break;
		case (FFAtomAccessors::Form):
			vf = VdwFunctions::vdwFunction(srcvar->asCharacter());
			if (vf == VdwFunctions::None) result = FALSE;
			else ffa->setVdwForm(vf);
			break;
		case (FFAtomAccessors::Name):
			ffa->setName(srcvar->asCharacter());
			break;
		default:
			printf("FFAtomAccessors::set doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("FFAtomAccessors::set");
	return result;
}
