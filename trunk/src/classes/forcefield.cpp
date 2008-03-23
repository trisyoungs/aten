/*
	*** Molecular mechanics forcefield
	*** src/classes/forcefield.cpp
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
#include "base/debug.h"

// Constructors 
ForcefieldParams::ForcefieldParams()
{
	for (int i=0; i<MAXFFPARAMDATA; i++) data[i] = 0.0;
}

ForcefieldAtom::ForcefieldAtom()
{
	// Private variables
	name_.set("Unnamed");
	typeId_ = -1;
	charge_ = 0.0;
	vdwForm_ = VF_UNSPECIFIED;
	generator_ = NULL;
	// Public variables
	prev = NULL;
	next = NULL;
}

ForcefieldBound::ForcefieldBound()
{
	// Private variables
	type_ = FFC_NITEMS;
	// Public variables
	prev = NULL;
	next = NULL;
}

Forcefield::Forcefield()
{
	// Private variables
	rules_ = FFR_NORULES;
	nGenerators_ = 0;
	energyGenerators_ = NULL;
	// Public variables
	next = NULL;
	prev = NULL;
}

// Destructors
ForcefieldAtom::~ForcefieldAtom()
{
	if (generator_ != NULL) delete[] generator_;
}

Forcefield::~Forcefield()
{
	// Delete all parameter lists
	if (energyGenerators_ != NULL) delete[] energyGenerators_;
}

/*
// ForcefieldAtom
*/

// Set functional form of VDW
void ForcefieldAtom::setVdwForm(VdwFunction vf)
{
	vdwForm_ = vf;
}

// Returns the funcional VDW form
VdwFunction ForcefieldAtom::vdwForm()
{
	return vdwForm_;
}

// Returns the ffid of the type
int ForcefieldAtom::typeId()
{
	return typeId_;
}

// Returns the charge of the type
double ForcefieldAtom::charge()
{
	return charge_;
}

// Returns the name of the type
const char *ForcefieldAtom::name()
{
	return name_.get();
}

// Returns the equivalent name of the type
const char *ForcefieldAtom::equivalent()
{
	return equivalent_.get();
}

// Returns the description of the type
const char *ForcefieldAtom::description()
{
	return description_.get();
}

// Returns the atomtype description
Atomtype *ForcefieldAtom::atomType()
{
	return &atomType_;
}

// Returns ForcefieldParams structure
ForcefieldParams &ForcefieldAtom::params()
{
	return params_;
}

// Copy structure
void ForcefieldAtom::copy(ForcefieldAtom *source)
{
	vdwForm_ = source->vdwForm_;
	typeId_ = source->typeId_;
	name_ = source->name_;
	equivalent_ = source->equivalent_;
	description_ = source->description_;
	params_ = source->params_;
	//*generator_;
	charge_ = source->charge_;
}

/*
// ForcefieldBound
*/

// Set the type of bound interaction
void ForcefieldBound::setType(BoundType fc)
{
	type_ = fc;
}

// Return the type of bound interaction
BoundType ForcefieldBound::type()
{
	return type_;
}

// Return the functional form
ForcefieldBound::BoundForms ForcefieldBound::functionalForm()
{
	return functionalForm_;
}

// Set the bond functional form
void ForcefieldBound::setBondStyle(BondFunction bf)
{
	functionalForm_.bondFunc = bf;
}

// Set the angle functional form
void ForcefieldBound::setAngleStyle(AngleFunction af)
{
	functionalForm_.angleFunc = af;
}

// Set the torsion functional form
void ForcefieldBound::setTorsionStyle(TorsionFunction tf)
{
	functionalForm_.torsionFunc = tf;
}

// Return the data[] array in *params
ForcefieldParams &ForcefieldBound::params()
{
	return params_; 
}

// Return the atom type 'n'
const char *ForcefieldBound::atomType(int n)
{
	return (n < MAXFFBOUNDTYPES ? atomTypes_[n].get() : "OUTOFRANGE");
}

/*
// Forcefield
*/

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

// Returns the typing rules of the Forcefield
ForcefieldRules Forcefield::rules()
{
	return rules_;
}

// Returns the number of atom types specified in the Forcefield
int Forcefield::nAtomtypes()
{
	return types_.nItems();
}

// Returns the head of tha atomtype list
ForcefieldAtom *Forcefield::types()
{
	return types_.first();
}

// Returns the bond list
ForcefieldBound *Forcefield::bonds()
{
	return bonds_.first();
}

// Returns the angle list
ForcefieldBound *Forcefield::angles()
{
	return angles_.first();
}

// Returns the angle list
ForcefieldBound *Forcefield::torsions()
{
	return torsions_.first();
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
	dbgBegin(DM_CALLS,"Forcefield::find_type[int]");
	ForcefieldAtom *result;
	for (result = types_.first(); result != NULL; result = result->next)
		if (query == result->typeId_) break;
	dbgEnd(DM_CALLS,"Forcefield::find_type[int]");
	return result;
}

// Search FF for type name
ForcefieldAtom *Forcefield::findType(const char *query)
{
	// Search for the atomname specified and return the internal integer id (i.e. position in atomtype list)
	// We return the first occurrence we find (since there may be more than one - only typeId_ need be unique)
	// Search both names and equivalents (since aliases may be defined that are not themselves defined as types_)
	dbgBegin(DM_CALLS,"Forcefield::find_type[char]");
	ForcefieldAtom *result;
	for (result = types_.first(); result != NULL; result = result->next)
		if ((result->name_ == query) || (result->equivalent_ == query)) break;
	dbgEnd(DM_CALLS,"Forcefield::find_type[char]");
	return result;
}

// Return description of typeId_
Atomtype *Forcefield::typeOfId(int i)
{
	dbgBegin(DM_CALLS,"Forcefield::get_atomtype_of_typeId_");
	ForcefieldAtom *result = NULL;
	for (result = types_.first(); result != NULL; result = result->next)
		if (result->typeId_ == i) break;
	if (result == NULL) printf("Forcefield::get_atomtype_of_typeId_ <<<< FFID %i not found in forcefield >>>>\n",i);
	dbgEnd(DM_CALLS,"Forcefield::get_atomtype_of_typeId_");
	return &result->atomType_;
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
int Forcefield::matchTypes(ForcefieldAtom *ffi, ForcefieldAtom *ffj, const Dnchar &ti, const Dnchar &tj)
{
	// Type Match routines - string match the name of types_ 'i' and 'j' to the string'd types_
	// specified in the bond / angle / torsion data supplied. Only check 'one way round' - the routine
	// must be called again with i and j swapped over to test the inverse case.
	// Matches against 'equiv' atomnames.
	dbgBegin(DM_CALLS,"Forcefield::matchTypes");
	int matchi, matchj;
	// Best case - exact, direct match:
	if ((ffi->equivalent_ == ti) && (ffj->equivalent_ == tj))
	{
		dbgEnd(DM_CALLS,"Forcefield::matchTypes");
		return 0;
	}
	// No such luck, so match each atom separately
	matchi = matchType(ffi->equivalent_,ti);
	matchj = matchType(ffj->equivalent_,tj);
	dbgEnd(DM_CALLS,"Forcefield::matchTypes");
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
	dbgBegin(DM_CALLS,"Forcefield::findBond");
	ForcefieldBound *result = NULL;
	int matchij, matchji, bestmatch;
	bestmatch = 10;
	ForcefieldBound *b = bonds_.first();
	while (b != NULL)
	{
		// See how close the match is between the atom forcefield types and the bond specification
		matchij = matchTypes(ffi,ffj,b->atomTypes_[0],b->atomTypes_[1]);
		matchji = matchTypes(ffj,ffi,b->atomTypes_[0],b->atomTypes_[1]);
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
	dbgEnd(DM_CALLS,"Forcefield::findBond");
	return result;
}

// Find angle type
ForcefieldBound *Forcefield::findAngle(ForcefieldAtom *ffi, ForcefieldAtom *ffj, ForcefieldAtom *ffk)
{
	// Search the forcefield for the angle definition for the interaction of the atom types i-j-k
	// Return NULL is no match found.
	dbgBegin(DM_CALLS,"Forcefield::findAngle");
	ForcefieldBound *result = NULL;
	int matchj, matchik, matchki, bestmatch;
	bestmatch = 10;
	ForcefieldBound *a = angles_.first();
	while (a != NULL)
	{
		// See how close the match is between the atom forcefield types and the angle specification
		// Check the central atom of the angle first
		matchj = matchType(ffj->equivalent_,a->atomTypes_[1]);
		if (matchj != 10)
		{
			matchik = matchTypes(ffi,ffk,a->atomTypes_[0],a->atomTypes_[2]);
			matchki = matchTypes(ffk,ffi,a->atomTypes_[0],a->atomTypes_[2]);
			if (matchki < matchik) matchik = matchki;	// Take the better of the two results
			matchik += matchj;		// Add on the score from the central atom
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
	dbgEnd(DM_CALLS,"Forcefield::findAngle");
	return result;
}

// Find torsion type
ForcefieldBound *Forcefield::findTorsion(ForcefieldAtom *ffi, ForcefieldAtom *ffj, ForcefieldAtom *ffk, ForcefieldAtom *ffl)
{
	// Search the forcefield for the torsion definition for the interaction of the atom types i-j-k-l
	// Return NULL is no match found.
	dbgBegin(DM_CALLS,"Forcefield::findTorsion");
	ForcefieldBound *result = NULL;
	int matchil, matchli, matchjk, matchkj, matchijkl, matchlkji, bestmatch;
	bestmatch = 10;
	ForcefieldBound *t = torsions_.first();
	while (t != NULL)
	{
		// See how close the match is between the atom forcefield types and the torsion specification
		matchil = matchTypes(ffi,ffl,t->atomTypes_[0],t->atomTypes_[3]);
		matchli = matchTypes(ffl,ffi,t->atomTypes_[0],t->atomTypes_[3]);
		matchjk = matchTypes(ffj,ffk,t->atomTypes_[1],t->atomTypes_[2]);
		matchkj = matchTypes(ffk,ffj,t->atomTypes_[1],t->atomTypes_[2]);
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
	dbgEnd(DM_CALLS,"Forcefield::findTorsion");
	return result;
}

void Forcefield::convertParameters(EnergyUnit ff_eunit)
{
	// Convert units of all the energetic parameters within the forcefield from the unit supplied into program internal units (specified in prefs)
	// Check for 'NULL' pointers for ff_param variables (for e.g. rule-based forcefields)
	dbgBegin(DM_CALLS,"Forcefield::convertParameters");
	ForcefieldParams *p;
	ForcefieldBound *b;
	ForcefieldAtom *ffa;
	int n;
	// VDW and Generator Data
	// Note: First loop (for VDW) is from 1,n+1 instead of 0,n since we skip the dummy atom type which is n=0, present for all ffs.
	for (ffa = types_.first(); ffa != NULL; ffa = ffa->next)
	{
		p = &ffa->params_;
		switch (ffa->vdwForm())
		{
			case (VF_UNSPECIFIED):
				break;
			case (VF_LJ):
				p->data[VF_LJ_EPS] = prefs.convertEnergy(p->data[VF_LJ_EPS],ff_eunit);
				break;
			default:
				printf("Don't know how to convert forcefield parameters for this VDW type.\n");
				break;
		}
		// Only convert those parameters for which the 'energyGenerators_[]' flag is TRUE
		if (ffa->generator_ != NULL)
		{
			for (n=0; n<nGenerators_; n++)
				if (energyGenerators_[n]) ffa->generator_[n] = prefs.convertEnergy(ffa->generator_[n],ff_eunit);
		}
	}
	// Bonds 
	for (b = bonds_.first(); b != NULL; b = b->next)
	{
		p = &b->params_;
		switch (b->functionalForm_.bondFunc)
		{
			case (BF_UNSPECIFIED):
				break;
			case (BF_HARMONIC):
				p->data[BF_HARMONIC_K] = prefs.convertEnergy(p->data[BF_HARMONIC_K],ff_eunit);
				break;
			default:
				printf("Don't know how to convert forcefield parameters for this bond type.\n");
				break;
		}
	}
	// Angles
	for (b = angles_.first(); b != NULL; b = b->next)
	{
		p = &b->params_;
		switch (b->functionalForm_.angleFunc)
		{
			case (AF_UNSPECIFIED):
				break;
			case (AF_HARMONIC):
				p->data[AF_HARMONIC_K] = prefs.convertEnergy(p->data[AF_HARMONIC_K],ff_eunit);
				break;
			default:
				printf("Don't know how to convert forcefield parameters for this angle type.\n");
				break;
		}
	}
	// Torsions
	for (b = torsions_.first(); b != NULL; b = b->next)
	{
		p = &b->params_;
		switch (b->functionalForm_.torsionFunc)
		{
			case (TF_UNSPECIFIED):
				break;
			case (TF_COSINE):
				p->data[TF_COSINE_K] = prefs.convertEnergy(p->data[TF_COSINE_K],ff_eunit);
				break;
			case (TF_COS3):
				p->data[TF_COS3_K1] = prefs.convertEnergy(p->data[TF_COS3_K1],ff_eunit);
				p->data[TF_COS3_K2] = prefs.convertEnergy(p->data[TF_COS3_K2],ff_eunit);
				p->data[TF_COS3_K3] = prefs.convertEnergy(p->data[TF_COS3_K3],ff_eunit);
				break;
			case (TF_COS3C):
				p->data[TF_COS3C_K0] = prefs.convertEnergy(p->data[TF_COS3C_K0],ff_eunit);
				p->data[TF_COS3C_K1] = prefs.convertEnergy(p->data[TF_COS3C_K1],ff_eunit);
				p->data[TF_COS3C_K2] = prefs.convertEnergy(p->data[TF_COS3C_K2],ff_eunit);
				p->data[TF_COS3C_K3] = prefs.convertEnergy(p->data[TF_COS3C_K3],ff_eunit);
				break;
			case (TF_COS4):
				p->data[TF_COS4_K1] = prefs.convertEnergy(p->data[TF_COS4_K1],ff_eunit);
				p->data[TF_COS4_K2] = prefs.convertEnergy(p->data[TF_COS4_K2],ff_eunit);
				p->data[TF_COS4_K3] = prefs.convertEnergy(p->data[TF_COS4_K3],ff_eunit);
				p->data[TF_COS4_K4] = prefs.convertEnergy(p->data[TF_COS4_K4],ff_eunit);
				break;
			default:
				printf("Don't know how to convert forcefield parameters for this torsion type.\n");
				break;
		}
	}
	dbgEnd(DM_CALLS,"Forcefield::convertParameters");
}
