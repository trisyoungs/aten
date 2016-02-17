/*
	*** Molecular mechanics forcefield
	*** src/ff/forcefield.cpp
	Copyright T. Youngs 2007-2016

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
#include "base/forcefieldatom.h"
#include "base/forcefieldbound.h"
#include "base/sysfunc.h"
#include <QRegularExpression>

ATEN_USING_NAMESPACE

// Forcefield keywords
const char* ForcefieldKeywords[Forcefield::nForcefieldCommands] = { "angles", "bonds", "convert", "data", "defines", "escale", "equivalents", "function", "impropers", "inter", "message", "name", "torsions", "types", "uatypes", "units", "ureybradleys", "vdw", "vscale" };
Forcefield::ForcefieldCommand Forcefield::forcefieldCommand(QString s)
{
	return (Forcefield::ForcefieldCommand) enumSearch("forcefield keyword",Forcefield::nForcefieldCommands,ForcefieldKeywords,s);
}

// Constructor
Forcefield::Forcefield() : ListItem<Forcefield>()
{
	// Private variables
	energyUnit_ = Prefs::KiloJoules;
	// Create _NDEF_ type common to all FFs)
	ForcefieldAtom* ffa = types_.add();
	ffa->setParent(this);
	ffa->setName("_NDEF_");
	ffa->setTypeId(-1);
	vdwGenerator_ = NULL;
	bondGenerator_ = NULL;
	angleGenerator_ = NULL;
	torsionGenerator_ = NULL;
}

// Destructor
Forcefield::~Forcefield()
{
}

/*
 * Specifications
 */

// Sets the name of the Forcefield
void Forcefield::setName(QString name)
{
	name_ = name;
}

// Returns the name of the Forcefield
QString Forcefield::name()
{
	return name_;
}

// Sets the filename of the Forcefield
void Forcefield::setFilename(QString filename)
{
	filename_ = filename;
}

// Returns the filename of the Forcefield
QString Forcefield::filename()
{
	return filename_;
}

// Return internal energy unit of forcefield
Prefs::EnergyUnit Forcefield::energyUnit()
{
	return energyUnit_;
}

// Set internal energy unit of forcefield
void Forcefield::setEnergyUnit(Prefs::EnergyUnit eu)
{
	energyUnit_ = eu;
}

/*
 * Types
 */

// Returns the number of atom types specified in the Forcefield
int Forcefield::nTypes()
{
	return types_.nItems();
}

// Add a new type to the forcefield
ForcefieldAtom* Forcefield::addType()
{
	ForcefieldAtom* ffa = types_.add();
	ffa->setParent(this);
	ffa->setTypeId(types_.nItems()-1);
	return ffa;
}

// Returns the head of the atomtype list
ForcefieldAtom* Forcefield::types()
{
	return types_.first();
}

// Returns nth defined atomtype
ForcefieldAtom* Forcefield::type(int n)
{
	if ((n < 0) || (n > types_.nItems()))
	{
		printf("Index %i is out of range for Forcefield::types_\n",n);
		return NULL;
	}
	return types_[n];
}

// Search FF for type ID
ForcefieldAtom* Forcefield::findType(int query)
{
	// Search for the typeId_ specified and return the internal integer id (i.e. position in atomtype list)
	Messenger::enter("Forcefield::findType[int]");
	ForcefieldAtom* result;
	for (result = types_.first(); result != NULL; result = result->next)
		if (query == result->typeId()) break;
	Messenger::exit("Forcefield::findType[int]");
	return result;
}

	// Find the named atomtype
ForcefieldAtom* Forcefield::findType(QString name)
{
	// Search for the atomname specified and return the internal integer id (i.e. position in atomtype list)
	// We return the first occurrence we find (since there may be more than one - only typeId_ need be unique)
	// Search both names and equivalents (since aliases may be defined that are not themselves defined as types_)
	Messenger::enter("Forcefield::findType[char]");
	ForcefieldAtom* result;
	for (result = types_.first(); result != NULL; result = result->next) if ((result->name() == name) || (result->equivalent() == name)) break;
	Messenger::exit("Forcefield::findType[char]");
	return result;
}

// Return description of typeId_
ForcefieldAtom* Forcefield::findByTypeId(int i, ForcefieldAtom* excluding)
{
	Messenger::enter("Forcefield::findByTypeId");
	ForcefieldAtom* result = NULL;
	for (result = types_.first(); result != NULL; result = result->next) if ((result->typeId() == i) && (result != excluding)) break;
//	if (result == NULL) printf("Forcefield::typeOfId <<<< FFID %i not found in forcefield >>>>\n",i);
	Messenger::exit("Forcefield::findByTypeId");
	return result;
}

// Return number of type defines in forcefield
int Forcefield::nTypeDefines()
{
	return typeDefines_.nItems();
}

// Return type defines list
Neta* Forcefield::typeDefines()
{
	return typeDefines_.first();
}

// Find type define
Neta* Forcefield::typeDefine(QString name)
{
	Neta* node;
	for (node = typeDefines_.first(); node != NULL; node = node->next) if (name == node->name()) break;
	return node;
}

// Returns whether the specified forcefield type is contained in this forcefield
bool Forcefield::containsType(ForcefieldAtom* type)
{
	return types_.contains(type);
}

/*
 * Bonding Interactions
 */

// Add bond term to the forcefield
ForcefieldBound* Forcefield::addBond(BondFunctions::BondFunction form)
{
	ForcefieldBound* ffb = bonds_.add();
	ffb->setType(ForcefieldBound::BondInteraction);
	ffb->setBondForm(form);
	return ffb;
}

// Return number of terms defined in bonds list
int Forcefield::nBonds()
{
	return bonds_.nItems();
}

// Returns the bond list
ForcefieldBound* Forcefield::bonds()
{
	return bonds_.first();
}

// Returns nth defined bond
ForcefieldBound* Forcefield::bond(int n)
{
	if ((n < 0) || (n > bonds_.nItems()))
	{
		printf("Index %i is out of range for Forcefield::bonds_\n",n);
		return NULL;
	}
	return bonds_[n];
}

// Find bond type
ForcefieldBound* Forcefield::findBond(ForcefieldAtom* ffi, ForcefieldAtom* ffj)
{
	// Search the forcefield for the bond definition for the interaction of the atom types i-j
	// Return NULL if no match found ('result' remains 'NULL' if no kind of match is found).
	Messenger::enter("Forcefield::findBond");
	ForcefieldBound* result = NULL;
	int matchij, matchji, bestmatch;
	bestmatch = 10;
	ForcefieldBound* b = bonds_.first();
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
	Messenger::exit("Forcefield::findBond");
	return result;
}

// Retrieve bond data corresponding to specified names
ForcefieldBound* Forcefield::findBond(QString typei, QString typej)
{
	// Search the forcefield for the bond definition for the interaction of the atom types i-j
	// Return NULL if no match found ('result' remains 'NULL' if no kind of match is found).
	Messenger::enter("Forcefield::findBond[string]");
	ForcefieldBound* result = NULL;
	int matchij, matchji, bestmatch;
	bestmatch = 10;
	ForcefieldBound* b = bonds_.first();
	while (b != NULL)
	{
		// See how close the match is between the atom forcefield types and the bond specification
		matchij = matchTypes(typei, typej, b->typeName(0), b->typeName(1));
		matchji = matchTypes(typej, typei, b->typeName(0), b->typeName(1));
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
	Messenger::exit("Forcefield::findBond[string]");
	return result;
}

/*
 * Angle Interactions
 */

// Add angle term to the forcefield
ForcefieldBound* Forcefield::addAngle(AngleFunctions::AngleFunction form)
{
	ForcefieldBound* ffb = angles_.add();
	ffb->setType(ForcefieldBound::AngleInteraction);
	ffb->setAngleForm(form);
	return ffb;
}

// Return number of terms defined in angles list
int Forcefield::nAngles()
{
	return angles_.nItems();
}

// Returns the angle list
ForcefieldBound* Forcefield::angles()
{
	return angles_.first();
}

// Returns nth defined angle
ForcefieldBound* Forcefield::angle(int n)
{
	if ((n < 0) || (n > angles_.nItems()))
	{
		printf("Index %i is out of range for Forcefield::angles_\n",n);
		return NULL;
	}
	return angles_[n];
}

// Find angle type
ForcefieldBound* Forcefield::findAngle(ForcefieldAtom* ffi, ForcefieldAtom* ffj, ForcefieldAtom* ffk)
{
	// Search the forcefield for the angle definition for the interaction of the atom types i-j-k
	// Return NULL is no match found.
	Messenger::enter("Forcefield::findAngle");
	ForcefieldBound* result = NULL;
	int matchj, matchik, matchki, bestmatch;
	bestmatch = 10;
	ForcefieldBound* a = angles_.first();
	while (a != NULL)
	{
		// See how close the match is between the atom forcefield types and the angle specification
		// Check the central atom of the angle first
		matchj = matchType(ffj->equivalent(),a->typeName(1));
		if (matchj != 10)
		{
			matchik = matchTypes(ffi, ffk, a->typeName(0), a->typeName(2));
			matchki = matchTypes(ffk, ffi, a->typeName(0), a->typeName(2));
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
		// Early exit for an exact match
		if (bestmatch == 0) break;
		a = a->next;
	}
	Messenger::exit("Forcefield::findAngle");
	return result;
}

// Find angle type
ForcefieldBound* Forcefield::findAngle(QString typei, QString typej, QString typek)
{
	// Search the forcefield for the angle definition for the interaction of the atom types i-j-k
	// Return NULL is no match found.
	Messenger::enter("Forcefield::findAngle[string[");
	ForcefieldBound* result = NULL;
	int matchj, matchik, matchki, bestmatch;
	bestmatch = 10;
	ForcefieldBound* a = angles_.first();
	while (a != NULL)
	{
		// See how close the match is between the atom forcefield types and the angle specification
		// Check the central atom of the angle first
		matchj = matchType(typej, a->typeName(1));
		if (matchj != 10)
		{
			matchik = matchTypes(typei, typek, a->typeName(0), a->typeName(2));
			matchki = matchTypes(typek, typei, a->typeName(0), a->typeName(2));
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
		// Early exit for an exact match
		if (bestmatch == 0) break;
		a = a->next;
	}
	Messenger::exit("Forcefield::findAngle[string[");
	return result;
}

/*
 * Torsion Interactions
 */

// Add torsions term to the forcefield
ForcefieldBound* Forcefield::addTorsion(TorsionFunctions::TorsionFunction form)
{
	ForcefieldBound* ffb = torsions_.add();
	ffb->setType(ForcefieldBound::TorsionInteraction);
	ffb->setTorsionForm(form);
	return ffb;
}

// Return number of terms defined in torsions list
int Forcefield::nTorsions()
{
	return torsions_.nItems();
}

// Returns the torsion list
ForcefieldBound* Forcefield::torsions()
{
	return torsions_.first();
}

// Returns nth defined torsion
ForcefieldBound* Forcefield::torsion(int n)
{
	if ((n < 0) || (n > torsions_.nItems()))
	{
		printf("Index %i is out of range for Forcefield::torsions_\n",n);
		return NULL;
	}
	return torsions_[n];
}

// Find torsion type
ForcefieldBound* Forcefield::findTorsion(ForcefieldAtom* ffi, ForcefieldAtom* ffj, ForcefieldAtom* ffk, ForcefieldAtom* ffl)
{
	// Search the forcefield for the torsion definition for the interaction of the atom types i-j-k-l
	// Return NULL is no match found.
	Messenger::enter("Forcefield::findTorsion");
	ForcefieldBound* result = NULL;
	int matchil, matchli, matchjk, matchkj, matchijkl, matchlkji, bestmatch;
	bestmatch = 10;
	ForcefieldBound* t = torsions_.first();
	while (t != NULL)
	{
		// See how close the match is between the atom forcefield types and the torsion specification
		matchil = matchTypes(ffi, ffl, t->typeName(0), t->typeName(3));
		matchli = matchTypes(ffl, ffi, t->typeName(0), t->typeName(3));
		matchjk = matchTypes(ffj, ffk, t->typeName(1), t->typeName(2));
		matchkj = matchTypes(ffk, ffj, t->typeName(1), t->typeName(2));
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
	Messenger::exit("Forcefield::findTorsion");
	return result;
}

// Retrieve torsion data corresponding to specified names
ForcefieldBound* Forcefield::findTorsion(QString typei, QString typej, QString typek, QString typel)
{
	// Search the forcefield for the torsion definition for the interaction of the atom types i-j-k-l
	// Return NULL is no match found.
	Messenger::enter("Forcefield::findTorsion[string]");
	ForcefieldBound* result = NULL;
	int matchil, matchli, matchjk, matchkj, matchijkl, matchlkji, bestmatch;
	bestmatch = 10;
	ForcefieldBound* t = torsions_.first();
	while (t != NULL)
	{
		// See how close the match is between the atom forcefield types and the torsion specification
		matchil = matchTypes(typei, typel, t->typeName(0), t->typeName(3));
		matchli = matchTypes(typel, typei, t->typeName(0), t->typeName(3));
		matchjk = matchTypes(typej, typek, t->typeName(1), t->typeName(2));
		matchkj = matchTypes(typek, typej, t->typeName(1), t->typeName(2));
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
	Messenger::exit("Forcefield::findTorsion[string]");
	return result;
}

/*
 * Improper Torsion Interactions
 */

// Add torsions term to the forcefield
ForcefieldBound* Forcefield::addImproper(TorsionFunctions::TorsionFunction form)
{
	ForcefieldBound* ffb = impropers_.add();
	ffb->setType(ForcefieldBound::ImproperInteraction);
	ffb->setTorsionForm(form);
	return ffb;
}

// Return number of improper torsion terms defined in list
int Forcefield::nImpropers()
{
	return impropers_.nItems();
}

// Returns the improper torsion list
ForcefieldBound* Forcefield::impropers()
{
	return impropers_.first();
}

// Returns nth defined improper torsion
ForcefieldBound* Forcefield::improper(int n)
{
	if ((n < 0) || (n > impropers_.nItems()))
	{
		printf("Index %i is out of range for Forcefield::impropers_\n",n);
		return NULL;
	}
	return impropers_[n];
}

// Retrieve improper torsion data corresponding to specified names
ForcefieldBound* Forcefield::findImproper(QString typei, QString typej, QString typek, QString typel)
{
	// Search the forcefield for the improper torsion definition for the interaction of the atom types i-j-k-l
	// Return NULL is no match found.
	Messenger::enter("Forcefield::findImproper[string]");
	ForcefieldBound* result = NULL;
	int matchil, matchli, matchjk, matchkj, matchijkl, matchlkji, bestmatch;
	bestmatch = 10;
	ForcefieldBound* t = impropers_.first();
	while (t != NULL)
	{
		// See how close the match is between the atom forcefield types and the torsion specification
		matchil = matchTypes(typei,typel,t->typeName(0),t->typeName(3));
		matchli = matchTypes(typel,typei,t->typeName(0),t->typeName(3));
		matchjk = matchTypes(typej,typek,t->typeName(1),t->typeName(2));
		matchkj = matchTypes(typek,typej,t->typeName(1),t->typeName(2));
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
	Messenger::exit("Forcefield::findImproper[string]");
	return result;
}

/*
// Urey-Bradley Interactions
*/

// Add Urey-Bradley term to the forcefield
ForcefieldBound* Forcefield::addUreyBradley(BondFunctions::BondFunction form)
{
	ForcefieldBound* ffb = ureyBradleys_.add();
	ffb->setType(ForcefieldBound::UreyBradleyInteraction);
	ffb->setBondForm(form);
	return ffb;
}

// Return number of terms defined in Urey-Bradley list
int Forcefield::nUreyBradleys()
{
	return ureyBradleys_.nItems();
}

// Returns the Urey-Bradley list
ForcefieldBound* Forcefield::ureyBradleys()
{
	return ureyBradleys_.first();
}

// Returns nth defined Urey-Bradley
ForcefieldBound* Forcefield::ureyBradley(int n)
{
	if ((n < 0) || (n > ureyBradleys_.nItems()))
	{
		printf("Index %i is out of range for Forcefield::ureyBradley list\n",n);
		return NULL;
	}
	return ureyBradleys_[n];
}

// Find Urey-Bradley type
ForcefieldBound* Forcefield::findUreyBradley(ForcefieldAtom* ffi, ForcefieldAtom* ffj, ForcefieldAtom* ffk)
{
	// Search the forcefield for the Urey-Bradley definition for the interaction of the atom types i-j-k
	// Return NULL is no match found.
	Messenger::enter("Forcefield::findUreyBradley");
	ForcefieldBound* result = NULL;
	int matchj, matchik, matchki, bestmatch;
	bestmatch = 10;
	ForcefieldBound* a = ureyBradleys_.first();
	while (a != NULL)
	{
		// See how close the match is between the atom forcefield types and the Urey-Bradley specification
		// Check the central atom of the Urey-Bradley first
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
	Messenger::exit("Forcefield::findUreyBradley");
	return result;
}

// Find Urey-Bradley type
ForcefieldBound* Forcefield::findUreyBradley(QString typei, QString typej, QString typek)
{
	// Search the forcefield for the Urey-Bradley definition for the interaction of the atom types i-j-k
	// Return NULL is no match found.
	Messenger::enter("Forcefield::findUreyBradley[string]");
	ForcefieldBound* result = NULL;
	int matchj, matchik, matchki, bestmatch;
	bestmatch = 10;
	ForcefieldBound* a = ureyBradleys_.first();
	while (a != NULL)
	{
		// See how close the match is between the atom forcefield types and the Urey-Bradley specification
		// Check the central atom of the Urey-Bradley first
		matchj = matchType(typej,a->typeName(1));
		if (matchj != 10)
		{
			matchik = matchTypes(typei,typek,a->typeName(0),a->typeName(2));
			matchki = matchTypes(typek,typei,a->typeName(0),a->typeName(2));
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
	Messenger::exit("Forcefield::findUreyBradley[string]");
	return result;
}

/*
 * Parameter Matching
 */

// Match two forcefield type strings
int Forcefield::matchType(QString test, QString target)
{
	// Text-match the single atomtype 'i' to the passed string.
	// Wildcard '*' in the atomname matches rest of string.
	// Return 1 for a wildcard, '0' for an exact match, and an arbitrary '10' for no match
	if (test == target) return 0;
	bool wild = false, failed = false;
	int length = std::min(test.length(), target.length());
	for (int n=0; n <length; ++n)
	{
		if ((target.at(n) == '*') || (test.at(n) == '*'))
		{
			wild = true;
			break;
		}
		else if (test.at(n) != target.at(n))
		{
			failed = true;
			break;
		}
	}
	if (failed) return 10;
	if (wild) return 1;
	if (test.length() == target.length()) printf("Forcefield::matchType <<<< Weird error - missed exact match? >>>>\n");
	return 10;
}

// Match atom types to names
int Forcefield::matchTypes(ForcefieldAtom* ffi, ForcefieldAtom* ffj, QString typei, QString typej)
{
	// Type Match routines - string match the name of types_ 'i' and 'j' to the string'd types_
	// specified in the bond / angle / torsion data supplied. Only check 'one way round' - the routine
	// must be called again with i and j swapped over to test the inverse case.
	// Matches against 'equiv' atomnames.
	Messenger::enter("Forcefield::matchTypes");
	int matchi, matchj;
	// Best case - exact, direct match:
	if ((ffi->equivalent() == typei) && (ffj->equivalent() == typej))
	{
		Messenger::exit("Forcefield::matchTypes");
		return 0;
	}
	// No such luck, so match each atom separately
	matchi = matchType(ffi->equivalent(), typei);
	matchj = matchType(ffj->equivalent(), typej);
	Messenger::exit("Forcefield::matchTypes");
	return (matchi + matchj);
}

// Match names of supplied typenames and test names 
int Forcefield::matchTypes(QString testi, QString testj, QString typei, QString typej)
{
	// Type Match routines - string match the name of types_ 'i' and 'j' to the string'd types_
	// specified in the bond / angle / torsion data supplied. Only check 'one way round' - the routine
	// must be called again with i and j swapped over to test the inverse case.
	// Matches against 'equiv' atomnames.
	Messenger::enter("Forcefield::matchTypes[string]");
	int matchi, matchj;

	// Best case - exact, direct match:
	if ((testi == typei) && (testj == typej))
	{
		Messenger::exit("Forcefield::matchTypes[string]");
		return 0;
	}
	// No such luck, so match each atom separately
	matchi = matchType(testi, typei);
	matchj = matchType(testj, typej);
	Messenger::exit("Forcefield::matchTypes[string]");
	return (matchi + matchj);
}

// Scoring method for the following parameter-search routines works as follows:
//	0  : Exact match to all parameters
//	1-9: Partial match with wildcards
//	10+: One or more parameters did not match

/*
 * Misc
 */

void Forcefield::convertParameters()
{
	// Convert units of all the energetic parameters within the forcefield from the forcefield's current units to the program's internal units (specified in prefs)
	Messenger::enter("Forcefield::convertParameters");
	ForcefieldBound* ffb;
	ForcefieldAtom* ffa;
	int n;
	Variable* v;
	ReturnValue newvalue, oldvalue;
	// VDW and extra defined data - skip first definition which is '_NDEF_'
	for (ffa = types_.first()->next; ffa != NULL; ffa = ffa->next)
	{
		if (ffa->vdwForm() != VdwFunctions::None)
		{
			for (n=0; n<MAXFFPARAMDATA; n++) if (VdwFunctions::VdwFunctions[ffa->vdwForm()].isEnergyParameter[n]) ffa->setParameter(n,prefs.convertEnergy(ffa->parameter(n), energyUnit_));
		}
		// Only convert those parameters which are contained in the energyData_ list
		for (n=0; n<energyData_.count(); ++n)
		{
			v = ffa->data(energyData_.at(n));
			if (v == NULL)
			{
				Messenger::print("Couldn't find energy data named '%s'...", qPrintable(energyData_.at(n)));
				continue;
			}
			v->execute(oldvalue);
			newvalue.set(prefs.convertEnergy(oldvalue.asDouble(), energyUnit_));
			v->set(newvalue);
		}
	}

	// Bonds 
	for (ffb = bonds_.first(); ffb != NULL; ffb = ffb->next)
	{
		if (ffb->bondForm() == BondFunctions::None) continue;
		for (n=0; n<MAXFFPARAMDATA; n++) if (BondFunctions::BondFunctions[ffb->bondForm()].isEnergyParameter[n]) ffb->setParameter(n, prefs.convertEnergy(ffb->parameter(n), energyUnit_));
	}

	// Angles
	for (ffb = angles_.first(); ffb != NULL; ffb = ffb->next)
	{
		if (ffb->angleForm() == AngleFunctions::None) continue;
		for (n=0; n<MAXFFPARAMDATA; n++) if (AngleFunctions::AngleFunctions[ffb->angleForm()].isEnergyParameter[n]) ffb->setParameter(n, prefs.convertEnergy(ffb->parameter(n), energyUnit_));
	}

	// Torsions
	for (ffb = torsions_.first(); ffb != NULL; ffb = ffb->next)
	{
		if (ffb->torsionForm() == TorsionFunctions::None) continue;
		for (n=0; n<MAXFFPARAMDATA-2; n++) if (TorsionFunctions::TorsionFunctions[ffb->torsionForm()].isEnergyParameter[n]) ffb->setParameter(n, prefs.convertEnergy(ffb->parameter(n), energyUnit_));
	}

	// Set new energy unit of the forcefield to the programs internal unit
	energyUnit_ = prefs.energyUnit();
	Messenger::exit("Forcefield::convertParameters");
}
