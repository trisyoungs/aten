/*
	*** Pattern Access
	*** src/variables/patternaccess.cpp
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

#include "ff/forcefield.h"
#include "variables/patternaccess.h"
#include "variables/accessstep.h"
#include "variables/vaccess.h"
#include "model/model.h"
#include "base/pattern.h"
#include "base/messenger.h"

PatternAccessors patternAccessors;

// Constructor
PatternAccessors::PatternAccessors()
{
	accessorPointers[PatternAccessors::Angles] = addListAccessor("angles",	VTypes::PatternBoundData);
	accessorPointers[PatternAccessors::Atoms] = addListAccessor("atoms",	VTypes::AtomData);
	accessorPointers[PatternAccessors::Bonds] = addListAccessor("bonds",	VTypes::PatternBoundData);
	accessorPointers[PatternAccessors::Cog] = addListAccessor("cog", VTypes::ConstVectorData);
	accessorPointers[PatternAccessors::Com] = addListAccessor("com", VTypes::ConstVectorData);
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
	accessorPointers[PatternAccessors::NMols] = addAccessor("nmols",		VTypes::IntegerData, TRUE);
	accessorPointers[PatternAccessors::NTorsions] = addAccessor("ntorsions",	VTypes::IntegerData, TRUE);
	accessorPointers[PatternAccessors::Torsions] = addListAccessor("torsions",	VTypes::PatternBoundData);
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
		msg.exit("PatternAccessors::retrieve");
		return FALSE;
	} 
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("PatternAccessors::retrieve");
		return FALSE;
	}
	// Retrieve value based on enumerated id
	switch (vid)
	{
		case (PatternAccessors::Angles):
			rv.set(p->angle(index-1), VTypes::PatternBoundData);
			break;
		case (PatternAccessors::Atoms):
			rv.set(p->parent()->atom(index-1), VTypes::AtomData);
			break;
		case (PatternAccessors::Bonds):
			rv.set(p->bond(index-1), VTypes::PatternBoundData);
			break;
		case (PatternAccessors::Cog):
			rv.set(p->calculateCog(index-1));
			break;
		case (PatternAccessors::Com):
			rv.set(p->calculateCom(index-1));
			break;
		case (PatternAccessors::FirstAtom):
			rv.set(p->firstAtom(), VTypes::AtomData);
			break;
		case (PatternAccessors::FirstAtomId):
			rv.set(p->startAtom() + 1);
			break;
		case (PatternAccessors::FField):
			rv.set(p->forcefield(), VTypes::ForcefieldData);
			break;
		case (PatternAccessors::LastAtom):
			rv.set(p->lastAtom(), VTypes::AtomData);
			break;
		case (PatternAccessors::LastAtomId):
			rv.set(p->endAtom() + 1);
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
		case (PatternAccessors::Torsions):
			rv.set(p->torsion(index-1), VTypes::PatternBoundData);
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
	// Check read-only status
	if (accessorPointers[vid]->readOnly())
	{
		msg.print("Member '%s' of 'pattern' type is read-only.\n", accessorPointers[vid]->name());
		msg.exit("PatternAccessors::set");
		return FALSE;
	}
	// Get arrayindex (if there is one) and check that we needed it in the first place
	int index;
	if (!checkIndex(index, step, accessorPointers[vid]))
	{
		msg.exit("PatternAccessors::set");
		return FALSE;
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
		default:
			printf("PatternAccessors::set doesn't know how to use member '%s'.\n", accessorPointers[vid]->name());
			result = FALSE;
			break;
	}
	msg.exit("PatternAccessors::set");
	return result;
}
