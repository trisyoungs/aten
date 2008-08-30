/*
	*** Molecular mechanics forcefield
	*** src/ff/forcefield.cpp
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

#include "classes/forcefield.h"
#include "base/prefs.h"
#include "base/messenger.h"

// Forcefield keywords
const char *ForcefieldKeywords[Forcefield::nForcefieldCommands] = { "_NULL_", "name", "units", "rules", "types", "generator", "convert", "equivalents", "vdw", "bonds", "angles", "torsions", "vscale", "escale" };
Forcefield::ForcefieldCommand Forcefield::forcefieldCommand(const char *s)
{
	return (Forcefield::ForcefieldCommand) enumSearch("forcefield keyword",Forcefield::nForcefieldCommands,ForcefieldKeywords,s);
}

// Constructor
Forcefield::Forcefield()
{
	// Private variables
	rules_ = Rules::None;
	energyUnit_ = Prefs::KiloJoules;
	// Create _NDEF_ type common to all FFs)
	ForcefieldAtom *ffa = types_.add();
	ffa->setParent(this);
	ffa->setName("_NDEF_");
	ffa->setTypeId(-1);
	for (int i=0; i<MAXFFGENDATA; i++) energyGenerators_[i] = FALSE;

	// Public variables
	next = NULL;
	prev = NULL;
}

// Destructor
Forcefield::~Forcefield()
{
}

// Sets the name of the Forcefield
void Forcefield::setName(const char *s)
{
	name_.set(s);
}

// Returns the name of the Forcefield
const char *Forcefield::name()
{
	return name_.get();
}

// Sets the filename of the Forcefield
void Forcefield::setFilename(const char *s)
{
	filename_.set(s);
}

// Returns the filename of the Forcefield
const char *Forcefield::filename()
{
	return filename_.get();
}

// Returns the typing rules of the Forcefield
Rules::ForcefieldRules Forcefield::rules()
{
	return rules_;
}

// Set conversion flag for energetic generator data
void Forcefield::setEnergyGenerator(int n)
{
	if ((n < 0) || (n > MAXFFGENDATA)) msg.print("Index %i is out of range for generator data.\n", n);
	else energyGenerators_[n] = TRUE;
}

// Set internal energy unit of forcefield
void Forcefield::setEnergyUnit(Prefs::EnergyUnit eu)
{
	energyUnit_ = eu;
}

// Returns the number of atom types specified in the Forcefield
int Forcefield::nTypes()
{
	return types_.nItems();
}

// Add a new type to the forcefield
ForcefieldAtom *Forcefield::addType()
{
	ForcefieldAtom *ffa = types_.add();
	ffa->setParent(this);
	ffa->setTypeId(types_.nItems()-1);
	return ffa;
}

// Returns the head of the atomtype list
ForcefieldAtom *Forcefield::types()
{
	return types_.first();
}

// Returns nth defined atomtype
ForcefieldAtom *Forcefield::type(int n)
{
	if ((n < 0) || (n > types_.nItems()))
	{
		printf("Index %i is out of range for Forcefield::types_\n",n);
		return NULL;
	}
	return types_[n];
}

// Add bond term to the forcefield
ForcefieldBound *Forcefield::addBond(BondFunctions::BondFunction form)
{
	ForcefieldBound *ffb = bonds_.add();
	ffb->setType(ForcefieldBound::BondInteraction);
	ffb->setBondStyle(form);
	return ffb;
}

// Return number of terms defined in bonds list
int Forcefield::nBonds()
{
	return bonds_.nItems();
}

// Returns the bond list
ForcefieldBound *Forcefield::bonds()
{
	return bonds_.first();
}

// Returns nth defined bond
ForcefieldBound *Forcefield::bond(int n)
{
	if ((n < 0) || (n > bonds_.nItems()))
	{
		printf("Index %i is out of range for Forcefield::bonds_\n",n);
		return NULL;
	}
	return bonds_[n];
}

// Add angle term to the forcefield
ForcefieldBound *Forcefield::addAngle(AngleFunctions::AngleFunction form)
{
	ForcefieldBound *ffb = angles_.add();
	ffb->setType(ForcefieldBound::AngleInteraction);
	ffb->setAngleStyle(form);
	return ffb;
}

// Return number of terms defined in angles list
int Forcefield::nAngles()
{
	return angles_.nItems();
}

// Returns the angle list
ForcefieldBound *Forcefield::angles()
{
	return angles_.first();
}

// Returns nth defined angle
ForcefieldBound *Forcefield::angle(int n)
{
	if ((n < 0) || (n > angles_.nItems()))
	{
		printf("Index %i is out of range for Forcefield::angles_\n",n);
		return NULL;
	}
	return angles_[n];
}

// Add torsions term to the forcefield
ForcefieldBound *Forcefield::addTorsion(TorsionFunctions::TorsionFunction form)
{
	ForcefieldBound *ffb = torsions_.add();
	ffb->setType(ForcefieldBound::TorsionInteraction);
	ffb->setTorsionStyle(form);
	return ffb;
}

// Return number of terms defined in torsions list
int Forcefield::nTorsions()
{
	return torsions_.nItems();
}

// Returns the angle list
ForcefieldBound *Forcefield::torsions()
{
	return torsions_.first();
}

// Returns nth defined torsion
ForcefieldBound *Forcefield::torsion(int n)
{
	if ((n < 0) || (n > torsions_.nItems()))
	{
		printf("Index %i is out of range for Forcefield::torsions_\n",n);
		return NULL;
	}
	return torsions_[n];
}

// Character-match the atomtype names supplied
int Forcefield::matchType(const Dnchar &a, const Dnchar &b)
{
	return matchType(a.get(),b.get());
}

// Search FF for type ID
ForcefieldAtom *Forcefield::findType(int query)
{
	// Search for the typeId_ specified and return the internal integer id (i.e. position in atomtype list)
	msg.enter("Forcefield::findType[int]");
	ForcefieldAtom *result;
	for (result = types_.first(); result != NULL; result = result->next)
		if (query == result->typeId()) break;
	msg.exit("Forcefield::findType[int]");
	return result;
}

// Search FF for type name
ForcefieldAtom *Forcefield::findType(const char *query)
{
	// Search for the atomname specified and return the internal integer id (i.e. position in atomtype list)
	// We return the first occurrence we find (since there may be more than one - only typeId_ need be unique)
	// Search both names and equivalents (since aliases may be defined that are not themselves defined as types_)
	msg.enter("Forcefield::findType[char]");
	ForcefieldAtom *result;
	for (result = types_.first(); result != NULL; result = result->next)
		if ((strcmp(result->name(),query) == 0) || (strcmp(result->equivalent(),query) == 0)) break;
	msg.exit("Forcefield::findType[char]");
	return result;
}

// Return description of typeId_
ForcefieldAtom *Forcefield::findByTypeId(int i, ForcefieldAtom *excluding)
{
	msg.enter("Forcefield::findByTypeId");
	ForcefieldAtom *result = NULL;
	for (result = types_.first(); result != NULL; result = result->next)
		if ((result->typeId() == i) && (result != excluding)) break;
//	if (result == NULL) printf("Forcefield::typeOfId <<<< FFID %i not found in forcefield >>>>\n",i);
	msg.exit("Forcefield::findByTypeId");
	return result;
}


// Match two forcefield type strings
int Forcefield::matchType(const char *test, const char *target)
{
	// Text-match the single atomtype 'i' to the passed string.
	// Wildcard '*' in the atomname matches rest of string.
	// Return 1 for a wildcard, '0' for an exact match, and an arbitrary '10' for no match
	if (strcmp(test,target) == 0) return 0;
	bool wild = FALSE, failed = FALSE;
	int length = max(strlen(test),strlen(target));
	for (int n=0; n <length; n++)
	{
		if ((target[n] == '*') || (test[n] == '*'))
		{
			wild = TRUE;
			break;
		}
		else if (test[n] != target[n])
		{
			failed = TRUE;
			break;
		}
	}
	if (failed) return 10;
	if (wild) return 1;
	printf("Forcefield::matchType <<<< Weird error - missed exact match? >>>>\n");
	return 10;
}

// Match atom types to names
int Forcefield::matchTypes(ForcefieldAtom *ffi, ForcefieldAtom *ffj, const char *typei, const char *typej)
{
	// Type Match routines - string match the name of types_ 'i' and 'j' to the string'd types_
	// specified in the bond / angle / torsion data supplied. Only check 'one way round' - the routine
	// must be called again with i and j swapped over to test the inverse case.
	// Matches against 'equiv' atomnames.
	msg.enter("Forcefield::matchTypes");
	int matchi, matchj;
	// Best case - exact, direct match:
	if ((strcmp(ffi->equivalent(),typei) == 0) && (strcmp(ffj->equivalent(),typej) == 0))
	{
		msg.exit("Forcefield::matchTypes");
		return 0;
	}
	// No such luck, so match each atom separately
	matchi = matchType(ffi->equivalent(),typei);
	matchj = matchType(ffj->equivalent(),typej);
	msg.exit("Forcefield::matchTypes");
	return (matchi + matchj);
}

// Scoring method for the following parameter-search routines works as follows:
//	0  : Exact match to all parameters
//	1-9: Partial match with wildcards
//	10+: One or more parameters did not match

// Find bond type
ForcefieldBound *Forcefield::findBond(ForcefieldAtom *ffi, ForcefieldAtom *ffj)
{
	// Search the forcefield for the bond definition for the interaction of the atom types i-j
	// Return NULL if no match found ('result' remains 'NULL' if no kind of match is found).
	msg.enter("Forcefield::findBond");
	ForcefieldBound *result = NULL;
	int matchij, matchji, bestmatch;
	bestmatch = 10;
	ForcefieldBound *b = bonds_.first();
	while (b != NULL)
	{
		// See how close the match is between the atom forcefield types and the bond specification
		matchij = matchTypes(ffi,ffj,b->typeName(0),b->typeName(1));
		matchji = matchTypes(ffj,ffi,b->typeName(0),b->typeName(1));
		if (matchji < matchij) matchij = matchji;	// Take the better (smaller) of the two results
		if (matchij < 10)
		{
			if (matchij < bestmatch)	// Better match
			{
				result = b;
				bestmatch = matchij;
			}
		}
		if (bestmatch == 0) break;
		b = b ->next;
	}
	msg.exit("Forcefield::findBond");
	return result;
}

// Find angle type
ForcefieldBound *Forcefield::findAngle(ForcefieldAtom *ffi, ForcefieldAtom *ffj, ForcefieldAtom *ffk)
{
	// Search the forcefield for the angle definition for the interaction of the atom types i-j-k
	// Return NULL is no match found.
	msg.enter("Forcefield::findAngle");
	ForcefieldBound *result = NULL;
	int matchj, matchik, matchki, bestmatch;
	bestmatch = 10;
	ForcefieldBound *a = angles_.first();
	while (a != NULL)
	{
		// See how close the match is between the atom forcefield types and the angle specification
		// Check the central atom of the angle first
		matchj = matchType(ffj->equivalent(),a->typeName(1));
		if (matchj != 10)
		{
			matchik = matchTypes(ffi,ffk,a->typeName(0),a->typeName(2));
			matchki = matchTypes(ffk,ffi,a->typeName(0),a->typeName(2));
			// Take the better of the two results
			if (matchki < matchik) matchik = matchki;
			// Add on the score from the central atom
			matchik += matchj;
			if (matchik < 10)
			{
				if (matchik < bestmatch)
				{
					result = a;
					bestmatch = matchik;
				}
			}
		}
		if (bestmatch == 0) break;		// Early exit for an exact match
		a = a ->next;
	}
	msg.exit("Forcefield::findAngle");
	return result;
}

// Find torsion type
ForcefieldBound *Forcefield::findTorsion(ForcefieldAtom *ffi, ForcefieldAtom *ffj, ForcefieldAtom *ffk, ForcefieldAtom *ffl)
{
	// Search the forcefield for the torsion definition for the interaction of the atom types i-j-k-l
	// Return NULL is no match found.
	msg.enter("Forcefield::findTorsion");
	ForcefieldBound *result = NULL;
	int matchil, matchli, matchjk, matchkj, matchijkl, matchlkji, bestmatch;
	bestmatch = 10;
	ForcefieldBound *t = torsions_.first();
	while (t != NULL)
	{
		// See how close the match is between the atom forcefield types and the torsion specification
		matchil = matchTypes(ffi,ffl,t->typeName(0),t->typeName(3));
		matchli = matchTypes(ffl,ffi,t->typeName(0),t->typeName(3));
		matchjk = matchTypes(ffj,ffk,t->typeName(1),t->typeName(2));
		matchkj = matchTypes(ffk,ffj,t->typeName(1),t->typeName(2));
		matchijkl = matchil + matchjk;
		matchlkji = matchli + matchkj;
		if (matchlkji < matchijkl) matchijkl = matchlkji;
		if (matchijkl < 10)
		{
			if (matchijkl < bestmatch)
			{
				result = t;
				bestmatch = matchijkl;
			}
		}
		if (bestmatch == 0) break;
		t = t->next;
	}
	msg.exit("Forcefield::findTorsion");
	return result;
}

void Forcefield::convertParameters()
{
	// Convert units of all the energetic parameters within the forcefield from the forcefield's current units to the program's internal units (specified in prefs)
	msg.enter("Forcefield::convertParameters");
	ForcefieldParams *p;
	ForcefieldBound *ffb;
	ForcefieldAtom *ffa;
	int n;
	// VDW and Generator Data
	// Note: First loop (for VDW) is from 1,n+1 instead of 0,n since we skip the dummy atom type which is n=0, present for all ffs.
	for (ffa = types_.first(); ffa != NULL; ffa = ffa->next)
	{
		p = &ffa->params();
		if (ffa->vdwForm() != VdwFunctions::None)
		{
			for (n=0; n<MAXFFPARAMDATA; n++) if (VdwFunctions::VdwFunctions[ffa->vdwForm()].isEnergyParameter[n]) p->data[n] = prefs.convertEnergy(p->data[n], energyUnit_);
		}
		// Only convert those parameters for which the 'energyGenerators_[]' flag is TRUE
		if (ffa->generator() != NULL)
		{
			for (n=0; n<MAXFFGENDATA; n++)
				if (energyGenerators_[n]) ffa->setGenerator(n, prefs.convertEnergy(ffa->generator(n), energyUnit_));
		}
	}
	// Bonds 
	for (ffb = bonds_.first(); ffb != NULL; ffb = ffb->next)
	{
		if (ffb->bondStyle() == BondFunctions::None) continue;
		p = &ffb->params();
		for (n=0; n<MAXFFPARAMDATA; n++) if (BondFunctions::BondFunctions[ffb->bondStyle()].isEnergyParameter[n]) p->data[n] = prefs.convertEnergy(p->data[n], energyUnit_);
	}
	// Angles
	for (ffb = angles_.first(); ffb != NULL; ffb = ffb->next)
	{
		if (ffb->angleStyle() == AngleFunctions::None) continue;
		p = &ffb->params();
		for (n=0; n<MAXFFPARAMDATA; n++) if (AngleFunctions::AngleFunctions[ffb->angleStyle()].isEnergyParameter[n]) p->data[n] = prefs.convertEnergy(p->data[n], energyUnit_);
	}
	// Torsions
	for (ffb = torsions_.first(); ffb != NULL; ffb = ffb->next)
	{
		if (ffb->torsionStyle() == TorsionFunctions::None) continue;
		p = &ffb->params();
		for (n=0; n<MAXFFPARAMDATA-2; n++) if (TorsionFunctions::TorsionFunctions[ffb->torsionStyle()].isEnergyParameter[n]) p->data[n] = prefs.convertEnergy(p->data[n], energyUnit_);
	}
	// Set new energy unit of the forcefield to the programs internal unit
	energyUnit_ = prefs.energyUnit();
	msg.exit("Forcefield::convertParameters");
}
