/*
	*** Pattern Access
	*** src/variables/patternaccess.cpp
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
#include "ff/forcefield.h"

#include "variables/patternaccess.h"
#include "variables/accessstep.h"
#include "variables/vaccess.h"
#include "base/pattern.h"
#include "base/messenger.h"

PatternAccessors patternAccessors;

// Constructor
PatternAccessors::PatternAccessors()
{
	accessorPointers[PatternAccessors::Angles] = addListAccessor("angles",	VTypes::ForcefieldBoundData);
	accessorPointers[PatternAccessors::Angles] = addListAccessor("atoms",	VTypes::ForcefieldAtomData);
	accessorPointers[PatternAccessors::Bonds] = addListAccessor("bonds",	VTypes::ForcefieldBoundData);
	accessorPointers[PatternAccessors::FirstAtom] = addAccessor("firstatom",	VTypes::AtomData, TRUE);
	accessorPointers[PatternAccessors::FirstAtomId] = addAccessor("firstatomid",	VTypes::IntegerData, TRUE);
	accessorPointers[PatternAccessors::FField] = addAccessor("forcefield",	VTypes::ForcefieldData, FALSE);
	accessorPointers[PatternAccessors::LastAtom] = addAccessor("lastatom",	VTypes::AtomData, TRUE);
	accessorPointers[PatternAccessors::LastAtomId] = addAccessor("lastatomid",	VTypes::IntegerData, TRUE);
	accessorPointers[PatternAccessors::Name] = addAccessor("name",		VTypes::CharacterData, FALSE);
	accessorPointers[PatternAccessors::NAngles] = addAccessor("nangles",	VTypes::IntegerData, TRUE);
	accessorPointers[PatternAccessors::NAtoms] = addAccessor("natoms",	VTypes::IntegerData, TRUE);
	accessorPointers[PatternAccessors::NBonds] = addAccessor("nbonds",	VTypes::IntegerData, TRUE);
	accessorPointers[PatternAccessors::NMolAtoms] = addAccessor("nmolatoms",	VTypes::IntegerData, TRUE);
	accessorPointers[PatternAccessors::NMolAtoms] = addAccessor("nmols",		VTypes::IntegerData, TRUE);
	accessorPointers[PatternAccessors::NTorsions] = addAccessor("ntorsions",	VTypes::IntegerData, TRUE);
};

// Retrieve specified data
bool PatternAccessors::retrieve(void *classptr, AccessStep *step, ReturnValue &rv)
{
	msg.enter("PatternAccessors::retrieve");
	bool result = TRUE;
	// Cast pointer into Pattern*
	Pattern *p = (Pattern*) classptr;
	if (p == NULL) printf("Warning - NULL Pattern pointer passed to PatternAccessors::retrieve.\n");
// 	printf("Enumerated ID supplied to PatternAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > PatternAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to PatternAccessors::set.\n", vid);
		msg.exit("PatternAccessors::set");
		return FALSE;
	} 
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (step->hasArrayIndex())
	{
		if (accessorPointers[vid]->isArray())
		{
			// Get index and do simple lower-limit check
			index = step->arrayIndex();
			if (index < 1)
			{
				printf("Array index '%i' given to member '%s' in PatternAccessors::retrieve is out of bounds.\n", index, accessorPointers[vid]->name());
				msg.exit("PatternAccessors::retrieve");
				return FALSE;
			}
		}
		else
		{
			printf("Array index given to member '%s' in PatternAccessors::retrieve, but it is not an array.\n", accessorPointers[vid]->name());
			msg.exit("PatternAccessors::retrieve");
			return FALSE;
		}
	}
	else
	{
		if (accessorPointers[vid]->isArray())
		{
			printf("Array index missing for member '%s' in PatternAccessors::retrieve.\n", accessorPointers[vid]->name());
			msg.exit("PatternAccessors::retrieve");
			return FALSE;
		}
	}
	// Retrieve value based on enumerated id
	switch (vid)
	{
		case (PatternAccessors::FirstAtom):
			rv.set(p->firstAtom(), VTypes::AtomData);
			break;
		case (PatternAccessors::FirstAtomId):
			rv.set(p->startAtom());
			break;
		case (PatternAccessors::FField):
			rv.set(p->forcefield(), VTypes::ForcefieldData);
			break;
		case (PatternAccessors::LastAtom):
			rv.set(p->lastAtom(), VTypes::AtomData);
			break;
		case (PatternAccessors::LastAtomId):
			rv.set(p->endAtom());
			break;
		case (PatternAccessors::Name):
			rv.set(p->name());
			break;
		case (PatternAccessors::NAngles):
			rv.set(p->nAngles());
			break;
		case (PatternAccessors::NAtoms):
			rv.set(p->totalAtoms());
			break;
		case (PatternAccessors::NBonds):
			rv.set(p->nBonds());
			break;
		case (PatternAccessors::NMolAtoms):
			rv.set(p->nAtoms());
			break;
		case (PatternAccessors::NMols):
			rv.set(p->nMolecules());
			break;
		case (PatternAccessors::NTorsions):
			rv.set(p->nTorsions());
			break;
		default:
			printf("PatternAccessors::retrieve doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("PatternAccessors::retrieve");
	return result;
}

// Set specified data
bool PatternAccessors::set(void *classptr, AccessStep *step, Variable *srcvar)
{
	msg.enter("PatternAccessors::set");
	bool result = TRUE;
	// Cast pointer into Pattern*
	Pattern *p = (Pattern*) classptr;
	if (p == NULL) printf("Warning - NULL Pattern pointer passed to PatternAccessors::set.\n");
// 	printf("Enumerated ID supplied to PatternAccessors is %i.\n", vid);
	// Check range of supplied vid
	int vid = step->variableId();
	if ((vid < 0) || (vid > PatternAccessors::nAccessors))
	{
		printf("Unknown enumeration %i given to PatternAccessors::set.\n", vid);
		msg.exit("PatternAccessors::set");
		return FALSE;
	} 
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (step->hasArrayIndex())
	{
		if (accessorPointers[vid]->isArray())
		{
			// Get index and do simple lower-limit check
			index = step->arrayIndex();
			if (index < 1)
			{
				printf("Array index '%i' given to member '%s' in PatternAccessors::set is out of bounds.\n", index, accessorPointers[vid]->name());
				msg.exit("PatternAccessors::set");
				return FALSE;
			}
		}
		else
		{
			printf("Array index given to member '%s' in PatternAccessors::set, but it is not an array.\n", accessorPointers[vid]->name());
			msg.exit("PatternAccessors::set");
			return FALSE;
		}
	}
	else
	{
		if (accessorPointers[vid]->isArray())
		{
			printf("Array index missing for member '%s' in PatternAccessors::set.\n", accessorPointers[vid]->name());
			msg.exit("PatternAccessors::set");
			return FALSE;
		}
	}
	// Set value based on enumerated id
	switch (vid)
	{
		case (PatternAccessors::Name):
			p->setName(srcvar->asCharacter());
			break;
		case (PatternAccessors::FField):
 			p->setForcefield( (Forcefield*) srcvar->asPointer(VTypes::ForcefieldData));
			break;
		case (PatternAccessors::FirstAtom):
		case (PatternAccessors::FirstAtomId):
		case (PatternAccessors::LastAtom):
		case (PatternAccessors::LastAtomId):
		case (PatternAccessors::NAngles):
		case (PatternAccessors::NAtoms):
		case (PatternAccessors::NBonds):
		case (PatternAccessors::NMolAtoms):
		case (PatternAccessors::NMols):
		case (PatternAccessors::NTorsions):
			msg.print("Member '%s' in Pattern is read-only.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
		default:
			printf("PatternAccessors::set doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("PatternAccessors::set");
	return result;
}
