/*
	*** Molecule pattern
	*** src/base/pattern.cpp
	Copyright T. Youngs 2007-2015

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

#include "base/pattern.h"
#include "base/elements.h"
#include "classes/ring.h"
#include "model/model.h"
#include "ff/forcefield.h"
#include "classes/forcefieldbound.h"
#include "classes/forcefieldatom.h"
#include "main/aten.h"

// Constructors
Pattern::Pattern() : ListItem<Pattern>()
{
	parent_ = NULL;
	id_ = 0;
	nMolecules_ = 0;
	nExpectedMols_ = 0;
	nAtoms_ = 0;
	totalAtoms_ = 0;
	startAtom_ = 0;
	endAtom_ = 0;
	firstAtom_ = NULL;
	lastAtom_ = NULL;
	fixed_ = FALSE;
	forcefield_ = NULL;
	conMatrix_ = NULL;
	elecScaleMatrix_ = NULL;
	vdwScaleMatrix_ = NULL;
	incomplete_ = FALSE;
	testAtomLimit_ = FALSE;
	testElement_ = FALSE;
	testBonding_ = FALSE;
	noIntramolecular_ = FALSE;
	atomsFixed_ = FALSE;
	addDummyTerms_ = FALSE;
}

PatternAtom::PatternAtom() : ListItem<PatternAtom>()
{
	// Private variables
	data_ = NULL;
	atom_ = NULL;
	forcefieldDataId_ = -1;
}

PatternBound::PatternBound() : ListItem<PatternBound>()
{
	// Private variables
	for (int i=0; i<MAXFFBOUNDTYPES; i++) atomIds_[i] = -1;
	data_ = NULL;
	forcefieldDataId_ = -1;
}

// Destructors
Pattern::~Pattern()
{
	deleteExpression();
}

/*
// PatternAtom
*/

// Set ff type of pattern atom
void PatternAtom::setData(ForcefieldAtom *ffa)
{
	data_ = ffa;
}

// Get ff type of pattern atom
ForcefieldAtom *PatternAtom::data()
{
	return data_;
}

// Set pointer to atom in patterns representative molecule
void PatternAtom::setAtom(Atom *a)
{
	atom_ = a;
}

// Get pointer to atom in patterns representative molecule
Atom *PatternAtom::atom()
{
	return atom_;
}

// Return integer index of unique atom data reference
int PatternAtom::forcefieldDataId() const
{
	return forcefieldDataId_;
}

// Set integer index of unique atom data reference
void PatternAtom::setForcefieldDataId(int id)
{
	forcefieldDataId_ = id;
}

/*
// Pattern Bound
*/

// Set atom id
void PatternBound::setAtomId(int n, int i)
{
	(n < MAXFFBOUNDTYPES ? atomIds_[n] = i : printf("OUTOFRANGE:PatternBound"));
}

// Return atom id
int PatternBound::atomId(int n) const
{
	return atomIds_[n];
}

// Set function data
void PatternBound::setData(ForcefieldBound *ffb)
{
	data_ = ffb;
}

// Get function data
ForcefieldBound *PatternBound::data()
{
	return data_;
}

// Return integer index of unique bound data reference
int PatternBound::forcefieldDataId() const
{
	return forcefieldDataId_;
}

// Set integer index of unique bound data reference
void PatternBound::setForcefieldDataId(int id)
{
	forcefieldDataId_ = id;
}

/*
// Pattern
*/

// Add the atom data specified
void Pattern::addAtomData(Atom *i, ForcefieldAtom *ffa)
{
	PatternAtom *pa = atoms_.add();
	pa->setAtom(i);
	pa->setData(ffa);
	pa->setForcefieldDataId(-1);
	// Add this to the unique types (by pointer) list
	allForcefieldTypes_.addUnique(ffa);
	// Add this to the unique types list if it isn't there already (type name equivalants in force)
	int n;
	for (n=0; n<uniqueForcefieldTypes_.nItems(); ++n) if (strcmp(uniqueForcefieldTypes_[n]->item->name(),ffa->name()) == 0) break;
	if (n == uniqueForcefieldTypes_.nItems()) uniqueForcefieldTypes_.add(ffa, 1);
	else uniqueForcefieldTypes_[n]->data = uniqueForcefieldTypes_[n]->data + 1;
	pa->setForcefieldDataId(n);
}

// Add the bond data specified
void Pattern::addBondData(ForcefieldBound *ffb, int i, int j)
{
	PatternBound *pb = bonds_.add();
	pb->setAtomId(0,i);
	pb->setAtomId(1,j);
	pb->setData(ffb);
	pb->setForcefieldDataId(-1);
	// Add this to the unique bonds list if it isn't there already
	int n;
	for (n=0; n<forcefieldBonds_.nItems(); ++n) if (forcefieldBonds_[n]->item == ffb) break;
	if (n == forcefieldBonds_.nItems()) forcefieldBonds_.add(ffb, 1);
	else forcefieldBonds_[n]->data = forcefieldBonds_[n]->data + 1;
	pb->setForcefieldDataId(n);
}

// Add the angle data specified
void Pattern::addAngleData(ForcefieldBound *ffb, int i, int j, int k)
{
	PatternBound *pb = angles_.add();
	pb->setAtomId(0,i);
	pb->setAtomId(1,j);
	pb->setAtomId(2,k);
	pb->setData(ffb);
	pb->setForcefieldDataId(-1);
	// Add this to the forcefield angles list if it isn't there already
	int n;
	for (n=0; n<forcefieldAngles_.nItems(); ++n) if (forcefieldAngles_[n]->item == ffb) break;
	if (n == forcefieldAngles_.nItems()) forcefieldAngles_.add(ffb, 1);
	else forcefieldAngles_[n]->data = forcefieldAngles_[n]->data + 1;
	pb->setForcefieldDataId(n);
}

// Add the torsion data specified
void Pattern::addTorsionData(ForcefieldBound *ffb, int i, int j, int k, int l)
{
	PatternBound *pb = torsions_.add();
	pb->setAtomId(0,i);
	pb->setAtomId(1,j);
	pb->setAtomId(2,k);
	pb->setAtomId(3,l);
	pb->setData(ffb);
	pb->setForcefieldDataId(-1);
	// Add this to the forcefield torsions list if it isn't there already
	int n;
	for (n=0; n<forcefieldTorsions_.nItems(); ++n) if (forcefieldTorsions_[n]->item == ffb) break;
	if (n == forcefieldTorsions_.nItems()) forcefieldTorsions_.add(ffb, 1);
	else forcefieldTorsions_[n]->data = forcefieldTorsions_[n]->data + 1;
	pb->setForcefieldDataId(n);
}

// Sets the ID of the pattern
void Pattern::setId(int i)
{
	id_ = i;
}

// Returns then numerical ID of the pattern
int Pattern::id() const
{
	return id_;
}

// Returns head of the atom list for this pattern (located in main model list)
Atom *Pattern::firstAtom()
{
	return firstAtom_;
}

// Sets pointer to the first atom in this pattern (located in main model list)
void Pattern::setFirstAtom(Atom* i)
{
	firstAtom_ = i;
}

// Returns last of the atom list for this pattern (located in main model list)
Atom *Pattern::lastAtom()
{
	return lastAtom_;
}

// Sets pointer to the last atom in this pattern (located in main model list)
void Pattern::setLastAtom(Atom* i)
{
	lastAtom_ = i;
}

// Calculate the global atom number offset of the first atom of the molecule
int Pattern::offset(int mol) const
{
	return startAtom_ + mol*nAtoms_;
}

// Returns the number of atoms in one molecule of the pattern
int Pattern::nAtoms() const
{
	return nAtoms_;
}

// Return number of bonds in one molecule of the pattern
int Pattern::nBonds() const
{
	return bonds_.nItems();
}

// Return number of angles in one molecule of the pattern
int Pattern::nAngles() const
{
	return angles_.nItems();
}

// Return number of torsions in one molecule of the pattern
int Pattern::nTorsions() const
{
	return torsions_.nItems();
}

// Return first bonds of the pattern
PatternBound *Pattern::bonds()
{
	return bonds_.first();
}

// Return first angle of the pattern
PatternBound *Pattern::angles()
{
	return angles_.first();
}

// Return first torsion of the pattern
PatternBound *Pattern::torsions()
{
	return torsions_.first();
}

// Return selected bond of the pattern
PatternBound *Pattern::bond(int i)
{
	return bonds_[i];
}

// Return selected angle of the pattern
PatternBound *Pattern::angle(int i)
{
	return angles_[i];
}

// Return selected torsion of the pattern
PatternBound *Pattern::torsion(int i)
{
	return torsions_[i];
}

// Return number of forcefield bonds used in the pattern
int Pattern::nForcefieldBonds() const
{
	return forcefieldBonds_.nItems();
}

// Return number of forcefield angles used in the pattern
int Pattern::nForcefieldAngles() const
{
	return forcefieldAngles_.nItems();
}

// Return number of forcefield torsions used in the pattern
int Pattern::nForcefieldTorsions() const
{
	return forcefieldTorsions_.nItems();
}

// Return number of forcefield types (by name) used in the pattern
int Pattern::nUniqueForcefieldTypes() const
{
	return uniqueForcefieldTypes_.nItems();
}

// Return first forcefield bond used in the pattern
Refitem<ForcefieldBound,int> *Pattern::forcefieldBonds()
{
	return forcefieldBonds_.first();
}

// Return first forcefield angle used in the pattern
Refitem<ForcefieldBound,int> *Pattern::forcefieldAngles()
{
	return forcefieldAngles_.first();
}

// Return first forcefield torsion used in the pattern
Refitem<ForcefieldBound,int> *Pattern::forcefieldTorsions()
{
	return forcefieldTorsions_.first();
}

// Return first unique (by name) forcefield type used in the pattern
Refitem<ForcefieldAtom,int> *Pattern::uniqueForcefieldTypes()
{
	return uniqueForcefieldTypes_.first();
}

// Return first unique (by pointer) forcefield type used in the pattern
Refitem<ForcefieldAtom,int> *Pattern::allForcefieldTypes()
{
	return allForcefieldTypes_.first();
}

// Return selected forcefield bond used in the pattern
Refitem<ForcefieldBound,int> *Pattern::forcefieldBond(int i)
{
	return forcefieldBonds_[i];
}

// Return selected forcefield angle used in the pattern
Refitem<ForcefieldBound,int> *Pattern::forcefieldAngle(int i)
{
	return forcefieldAngles_[i];
}

// Return selected forcefield torsion used in the pattern
Refitem<ForcefieldBound,int> *Pattern::forcefieldTorsion(int i)
{
	return forcefieldTorsions_[i];
}

// Return selected forcefield type used in the pattern
Refitem<ForcefieldAtom,int> *Pattern::uniqueForcefieldType(int i)
{
	return uniqueForcefieldTypes_[i];
}

// Return whether the positions of all molecules/atoms in the pattern are fixed in minimisations
bool Pattern::areAtomsFixed() const
{
	return atomsFixed_;
}

// Set whether the positions of all molecules/atoms in the pattern are fixed in minimisations
void Pattern::setAtomsFixed(bool b)
{
	atomsFixed_ = b;
}

// Set whether dummy terms will be generated for missing intramoleculars
void Pattern::setAddDummyTerms(bool b)
{
	addDummyTerms_ = b;
}

// Sets the starting atom of the model
void Pattern::setStartAtom(int n)
{
	startAtom_ = n;
}

// Returns the starting atom id of the pattern
int Pattern::startAtom() const
{
	return startAtom_;
}

// Sets the end atom of the model
void Pattern::setEndAtom(int n)
{
	endAtom_ = n;
}

// Returns the ending atom id of the pattern
int Pattern::endAtom() const
{
	return endAtom_;
}

// (Re)Calculate totalAtoms_
void Pattern::calcTotalAtoms()
{
	totalAtoms_ = nAtoms_ * nMolecules_;
}

// Returns the total number of atoms in the pattern
int Pattern::totalAtoms() const
{
	return totalAtoms_;
}

// Sets the number of molecules in the pattern
void Pattern::setNMolecules(int n)
{
	nMolecules_ = n;
}

// Returns the number of molecules in the pattern
int Pattern::nMolecules() const
{
	return nMolecules_;
}

// Sets the expected number of molecules in the pattern
void Pattern::setNExpectedMolecules(int n)
{
	nExpectedMols_ = n;
}

// Returns the expected number of molecules in the pattern
int Pattern::nExpectedMolecules() const
{
	return nExpectedMols_;
}

// Sets the parent model
void Pattern::setParent(Model *m)
{
	parent_ = m;
}

// Returns the model for which the pattern was created
Model *Pattern::parent()
{
	return parent_;
}

// Sets the 'fixed' property of the pattern
void Pattern::setFixed(bool b)
{
	fixed_ = b;
}

// Returns whether the pattern is fixed
bool Pattern::isFixed() const
{
	return fixed_;
}

// Sets the name of the pattern 
void Pattern::setName(const char *s)
{
	name_ = s;
}

// Returns the pattern name
const char *Pattern::name() const
{
	return name_.get();
}

// Sets the forcefield to use in the pattern
void Pattern::setForcefield(Forcefield *newff)
{	
	forcefield_ = newff;
	if (forcefield_ != NULL) msg.print("Forcefield '%s' is now assigned to pattern '%s'.\n", forcefield_->name(), name());
	else msg.print("Pattern '%s' now has no associated forcefield.\n", name());
}

// Gets the forcefield associated with the pattern
Forcefield *Pattern::forcefield()
{
	return forcefield_;
}

// Returns whether the atomlimit in the pattern is valid
bool Pattern::isAtomLimitOk() const
{
	return testAtomLimit_;
}

// Returns whether the element composition in the pattern molecules is uniform
bool Pattern::areElementsOk() const
{
	return testElement_;
}

// Returns whether the bonding in the pattern molecules is uniform
bool Pattern::isBondingOk() const
{
	return testBonding_;
}

// Postfix increment
Pattern *Pattern::operator++()
{
	return (this->next);
}

// Returns a pointer to the ring list structure
List<Ring>* Pattern::ringList()
{
	return &rings_;
}

// Return number of rings in current pattern
int Pattern::nRings()
{
	return rings_.nItems();
}

// Returns the first ring in the ring list
Ring *Pattern::rings()
{
	return rings_.first();
}

// Returns whether atom id i is in a ring, or both atoms i and j are in the same ring
bool Pattern::atomsInRing(int id_i, int id_j)
{
	if (id_j == -1)
	{
		Atom *i = atoms_[id_i]->atom();
		for (Ring *r = rings_.first(); r != NULL; r = r->next) if (r->containsAtom(i)) return TRUE;
	}
	else
	{
		Atom *i = atoms_[id_i]->atom();
		Atom *j = atoms_[id_j]->atom();
		for (Ring *r = rings_.first(); r != NULL; r = r->next) if ((r->containsAtom(i)) && (r->containsAtom(j))) return TRUE;
	}
	return FALSE;
}

// Initialise
void Pattern::initialise(int patid, int start, int mols, int atomsmol)
{
	// Initialise atom pointers / values in pattern.
	msg.enter("Pattern::initialise");
	if (parent_ == NULL)
	{
		printf("Internal Error: Owner model has not been set in pattern!\n");
		msg.exit("Pattern::initialise");
		return;
	}
	// Store parameters
	id_ = patid;
	nMolecules_ = mols;			// Number of molecules in pattern
	nAtoms_ = atomsmol;			// Number of atoms per molecule
	totalAtoms_ = mols*nAtoms_;		// Total number of atoms described by the pattern
	startAtom_ = start;			// Starting atom (integer position in atom list)
	endAtom_ = start + nAtoms_ - 1;		// Last atom in first molecule (integer position in atom list)
	// Set atom pointers
	if (startAtom_ > parent_->nAtoms())
	{
		// Can't get first atom (probably the pattern extends past nAtoms_)
		msg.print("Initial atom in pattern is past end of model's atom list (%i).\n",endAtom_);
		firstAtom_ = NULL;
	}
	else
	{
		// Get the first atom in the list
		Atom *i = parent_->atoms();
		// Skip past atoms until we get to startAtom_ (ranges from 0 upwards)
		for (int n=0; n<startAtom_; n++) i = i->next;
		firstAtom_ = i;
	}
	msg.print(Messenger::Verbose, "New pattern node : start=%i, nMols=%i, nAtoms/mol=%i, totalAtoms=%i, name=%s\n", startAtom_+1, nMolecules_, nAtoms_, totalAtoms_, name_.get());
	msg.exit("Pattern::initialise");
}

// Empty the selected pattern
void Pattern::empty()
{
	// Set all variables to reflect an empty pattern
	msg.enter("Pattern::empty");
	// Zero everything except nAtoms_
	firstAtom_ = NULL;
	lastAtom_ = NULL;
	nMolecules_ = 0;
	totalAtoms_ = 0;
	msg.exit("Pattern::empty");
}

// Set contents of pattern
void Pattern::setContents(int newstartAtom_, int newnMolecules_, int newnAtoms_)
{
	if (newstartAtom_ != -1) startAtom_ = newstartAtom_;
	if (newnMolecules_ != -1) nMolecules_ = newnMolecules_;
	if (newnAtoms_ != -1) nAtoms_ = newnAtoms_;
	totalAtoms_ = nMolecules_ * nAtoms_;
	endAtom_ = startAtom_ + nAtoms_ - 1;
}

/*
// Expression
*/

// Remove energy expression
void Pattern::deleteExpression()
{
	// Clear the energy expression for the pattern node
	msg.enter("Pattern::deleteExpression");
	atoms_.clear();
	bonds_.clear();
	angles_.clear();
	torsions_.clear();
	uniqueForcefieldTypes_.clear();
	allForcefieldTypes_.clear();
	if (conMatrix_ != NULL)
	{
		for (int n=0; n<nAtoms_; n++) delete[] conMatrix_[n];
		delete[] conMatrix_;
	}
	conMatrix_ = NULL;
	if (vdwScaleMatrix_ != NULL)
	{
		for (int n=0; n<nAtoms_; n++) delete[] vdwScaleMatrix_[n];
		delete[] vdwScaleMatrix_;
	}
	vdwScaleMatrix_ = NULL;
	if (elecScaleMatrix_ != NULL)
	{
		for (int n=0; n<nAtoms_; n++) delete[] elecScaleMatrix_[n];
		delete[] elecScaleMatrix_;
	}
	elecScaleMatrix_ = NULL;
	msg.exit("Pattern::deleteExpression");
}

// Create connectivity and scaling matrices for molecules in pattern
void Pattern::createMatrices()
{
	msg.enter("Pattern::createMatrices");
	int n, m, a1, a2;
	PatternBound *pb;

	if (conMatrix_ != NULL) msg.print("Pattern::createMatrices : Warning - connectivity matrix was already allocated.\n");
	conMatrix_ = new int*[nAtoms_];
	for (n=0; n<nAtoms_; n++) conMatrix_[n] = new int[nAtoms_];
	if (vdwScaleMatrix_ != NULL) msg.print("Pattern::createMatrices : Warning - VDW scaling matrix was already allocated.\n");
	vdwScaleMatrix_ = new double*[nAtoms_];
	for (n=0; n<nAtoms_; n++) vdwScaleMatrix_[n] = new double[nAtoms_];
	if (elecScaleMatrix_ != NULL) msg.print("Pattern::createMatrices : Warning - electrostatic scaling matrix was already allocated.\n");
	elecScaleMatrix_ = new double*[nAtoms_];
	for (n=0; n<nAtoms_; n++) elecScaleMatrix_[n] = new double[nAtoms_];

	msg.print("Connectivity matrix.....initialising....");
	for (n=0; n<nAtoms_; ++n)
		for (m=0; m<nAtoms_; ++m) conMatrix_[n][m] = 0;

	// Print out the bonding matrix
/*	printf("Bonding Matrix\n");
	for (n=0; n<nAtoms_; n++)
	{
		for (m=0; m<nAtoms_; m++) printf (" %2i ",conMatrix_[n][m]);
		printf("\n");
	} */

	// Since the full transformation to the connectivity matrix is quite intensive, we will only do this for patterns containing less than 1000 atoms
	if (nAtoms_ < 1000)
	{
		msg.print("seeding.....");
		// First, build up the bond matrix
		for (pb = bonds_.first(); pb != NULL; pb = pb->next)
		{
			conMatrix_[ pb->atomId(0) ] [ pb->atomId(1) ] = 1;
			conMatrix_[ pb->atomId(1) ] [ pb->atomId(0) ] = 1;
		}

		// Now, transform into the connectivity matrix.
		msg.print("transforming (full).....");
		for (a1=0; a1<nAtoms_; a1++)
		{
			for (a2=0; a2<nAtoms_; a2++)
			{
				if (conMatrix_[a1][a2] != 0)
				{
					// A2 *is* bound directly to A1.
					// So, we may increase the distance of A2 from all *other* atoms that A1 is bound to by one hop.
					for (m=0; m<nAtoms_; m++)
					{
						// We only potentially increase the distance if :
						//	1) The atom 'm' is *not equal* to a2 (i.e. is not itself)
						//	2) The atom 'm' we're looking at is also bound to a1.
						if ((m != a2) && (conMatrix_[a1][m] != 0))
						if ((conMatrix_[a1][m] != 0))
						{
							// We only actually increase the distance if :
							// 	1) Atom 'm' is not directly bound to a2 **OR**
							//	2) The combined distances of m->a1 and a1->a2 is less than m->a2
							// The last check means that only the minimum distance m->a2 persists at the end
							if ((conMatrix_[m][a2] == 0) || (conMatrix_[a1][m]+conMatrix_[a1][a2] < conMatrix_[m][a2]))
								conMatrix_[m][a2] = conMatrix_[a1][m] + conMatrix_[a1][a2];
						}
					}
				}
			}
		}
	}
	else
	{
		// Create minimal transformation matrix, using only bond, angle, and torsion data
		msg.print("transforming (minimal).....");

		// There may be more than one consecutive bound fragment in the pattern, so we must perform treeSelects in order to populate the initial matrix
		Atom* i = firstAtom_;
		int count = 100, ii, jj, diagii;
		parent_->selectNone(TRUE);
		while (i != NULL)
		{
			// Check that we are still in the current pattern
			if (i->id() > endAtom_) break;

			// Treeselect from current atom
			parent_->selectTree(i, TRUE);

			// For the current marked selection, set the diagonal matrix elements to the current 'count' value
			for (Refitem<Atom,int>* ri = parent_->selection(TRUE); ri != NULL; ri = ri->next)
			{
				ii = ri->item->id() - startAtom_;
				conMatrix_[ii][ii] = count;
			}

			// Find next unmarked atom
			while (i && i->isSelected(TRUE)) i = i->next;

			// Deselect all atoms, and increase count
			parent_->selectNone(TRUE);
			++count;
		}

		// The diagonal elements of conMatrix_ now indicate the parent fragments of each atom
		// For all atoms within a given fragment, set the connectivity to the diagonal value to start with
		for (ii = 0; ii < nAtoms_; ++ii)
		{
			diagii = conMatrix_[ii][ii];
			for (jj = 0; jj < nAtoms_; ++jj)
			{
				if (diagii != conMatrix_[jj][jj]) continue;
				conMatrix_[ii][jj] = diagii;
				conMatrix_[jj][ii] = diagii;
			}
		}

		// Done with the diagonals now, so zero them
		for (ii = 0; ii < nAtoms_; ++ii) conMatrix_[ii][ii] = 0;

		// Now, add bonds to matrix
		for (pb = bonds_.first(); pb != NULL; pb = pb->next)
		{
			conMatrix_[ pb->atomId(0) ] [ pb->atomId(1) ] = 1;
			conMatrix_[ pb->atomId(1) ] [ pb->atomId(0) ] = 1;
		}

		// Angles (but don't overwrite bonds)
		for (pb = angles_.first(); pb != NULL; pb = pb->next)
		{
			if (conMatrix_[pb->atomId(0)][pb->atomId(2)] == 1) continue;
			conMatrix_[pb->atomId(0)][pb->atomId(2)] = 2;
			conMatrix_[pb->atomId(2)][pb->atomId(0)] = 2;
		}

		// Torsions (but don't overwrite angles or bonds)
		for (pb = torsions_.first(); pb != NULL; pb = pb->next)
		{
			if (conMatrix_[pb->atomId(0)][pb->atomId(3)] < 3) continue;
			conMatrix_[pb->atomId(0)][pb->atomId(3)] = 3;
			conMatrix_[pb->atomId(3)][pb->atomId(0)] = 3;
		}
	}
	msg.print("done.\n");

// 	printf("Connectivity Matrix\n");
// 	for (n=0; n<nAtoms_; n++)
// 	{
// 		for (m=0; m<nAtoms_; m++) printf ("%2i",conMatrix_[n][m]);
// 		printf("\n"); 
// 	} 

	// Update contents of scale matrices
	updateScaleMatrices();

	msg.exit("Pattern::createMatrices");
}

// Update scale matrices in pattern
void Pattern::updateScaleMatrices()
{
	msg.enter("Pattern::updateScaleMatrices");
	int i, j;
	PatternBound *pb;
	// Set all matrix elements to '1.0' initially. Then cycle over torsions, then angles, then bonds and set values accordingly.
	for (i=0; i<nAtoms_; i++)
		for (j=0; j<nAtoms_; j++)
		{
			vdwScaleMatrix_[i][j] = 1.0;
			elecScaleMatrix_[i][j] = 1.0;
		}
	// Interactions at ends of torsion atoms are scaled by the factors stored in the torsion term
	for (pb = torsions_.first(); pb != NULL; pb = pb->next)
	{
		i = pb->atomId(0);
		j = pb->atomId(3);
		if ((i < 0) || (i >= nAtoms_) || (j < 0) || (j >= nAtoms_))
		{
			printf("Internal Error : One or both atom IDs (%i, %i) associated to torsion patternbound are invalid for pattern '%s'.\n", i, j, name_.get());
			continue;
		}
		vdwScaleMatrix_[i][j] = pb->data()->vdwScale();
		vdwScaleMatrix_[j][i] = pb->data()->vdwScale();
		elecScaleMatrix_[i][j] = pb->data()->elecScale();
		elecScaleMatrix_[j][i] = pb->data()->elecScale();
	}
	// Atoms at end of angles are excluded from vdw/elec interactions.
	// Note that these elements are zeroed in the matrices, but the connectivity matrix is used to determined whether they are calculated.
	for (pb = angles_.first(); pb != NULL; pb = pb->next)
	{
		i = pb->atomId(0);
		j = pb->atomId(2);
		if ((i < 0) || (i >= nAtoms_) || (j < 0) || (j >= nAtoms_))
		{
			printf("Internal Error : One or both atom IDs (%i, %i) associated to angle patternbound are invalid for pattern '%s'.\n", i, j, name_.get());
			continue;
		}
		vdwScaleMatrix_[i][j] = 0.0;
		vdwScaleMatrix_[j][i] = 0.0;
		elecScaleMatrix_[i][j] = 0.0;
		elecScaleMatrix_[j][i] = 0.0;
	}
	for (pb = bonds_.first(); pb != NULL; pb = pb->next)
	{
		i = pb->atomId(0);
		j = pb->atomId(1);
		if ((i < 0) || (i >= nAtoms_) || (j < 0) || (j >= nAtoms_))
		{
			printf("Internal Error : One or both atom IDs (%i, %i) associated to torsion patternbound are invalid for pattern '%s'.\n", i, j, name_.get());
			continue;
		}
		vdwScaleMatrix_[i][j] = 0.0;
		vdwScaleMatrix_[j][i] = 0.0;
		elecScaleMatrix_[i][j] = 0.0;
		elecScaleMatrix_[j][i] = 0.0;
	}
// 	printf("VSCALE Matrix\n");
// 	for (n=0; n<nAtoms_; n++)
// 	{
// 		for (m=0; m<nAtoms_; m++) printf ("%8.4f ",vdwScaleMatrix_[n][m]);
// 		printf("\n"); 
// 	}

	msg.exit("Pattern::updateScaleMatrices");
}

// Validate pattern
bool Pattern::validate()
{
	// Test the pattern for validity and internal consistency
	msg.enter("Pattern::validate");
	bool result, ok;
	result = TRUE;
	int mnAtoms_ = parent_->nAtoms();
	int *elcomp1, *elcomp2, a, m;
	// Set all test flags to FALSE
	testAtomLimit_ = FALSE;
	testElement_ = FALSE;
	testBonding_ = FALSE;
	// 1) Check number of atoms does not exceed number in model
	if (startAtom_+totalAtoms_ > mnAtoms_)
	{
		msg.print("Pattern's last atom is beyond the number of atoms in the model.\n");
		msg.print("No pattern defined for model.\n");
		// Can't do much else if this is the case, so break early.
		msg.exit("Pattern::validate");
		return FALSE;
	}
	else testAtomLimit_ = TRUE;
	// 2) Elemental composition of individual molecules within pattern
	elcomp1 = new int[Elements().nElements()+1];
	elcomp2 = new int[Elements().nElements()+1];
	for (m=0; m<Elements().nElements()+1; m++) elcomp1[m] = 0;
	if (nMolecules_ == 1) testElement_ = TRUE;
	else
	{
		Atom *i = firstAtom_;
		for (m=0; m<nMolecules_; m++)
		{
			ok = TRUE;
			if (m == 0)
			{
				// Calculate the reference atomic composition from molecule 1
				for (a=0; a<nAtoms_; a++)
				{
					elcomp1[i->element()] ++;
					i = i->next;
				}
			}
			else
			{
				// Calculate the test atomic composition...
				for (a=0; a<Elements().nElements()+1; a++) elcomp2[a] = 0;
				for (a=0; a<nAtoms_; a++)
				{
					elcomp2[i->element()] ++; i = i->next;
				}
				// ... and test against reference
				for (a=0; a<Elements().nElements()+1; a++)
					if (elcomp1[a] != elcomp2[a]) ok = FALSE;
			}
			if (!ok)
			{
				msg.print("Pattern failed element composition test at molecule %i.\n",m+1);
				result = FALSE;
				break;
			}
		}
	}
	delete elcomp1;
	delete elcomp2;
	// 3) Bonding within molecules in pattern
	//TODO
	if (!result) msg.print("No pattern defined for model.\n");
	msg.exit("Pattern::validate");
	return result;
}

// Create (or return existing) dummy bond term for supplied atom types
ForcefieldBound *Pattern::createDummyBond(ForcefieldAtom *i, ForcefieldAtom *j)
{
	// Search for existing term...
	ForcefieldBound *ffb;
	for (ffb = dummyForcefieldBonds_.first(); ffb != NULL; ffb = ffb->next) if (ffb->namesMatch(i->equivalent(), j->equivalent())) break;
	if (ffb == NULL)
	{
		ffb = dummyForcefieldBonds_.add();
		ffb->setType(ForcefieldBound::BondInteraction);
		ffb->setBondForm(BondFunctions::Ignore);
		ffb->setTypeName(0, i->equivalent());
		ffb->setTypeName(1, j->equivalent());
	}
	return ffb;
}

// Create (or return existing) angle bond term for supplied atom types
ForcefieldBound *Pattern::createDummyAngle(ForcefieldAtom *i, ForcefieldAtom *j, ForcefieldAtom *k)
{
	// Search for existing term...
	ForcefieldBound *ffb;
	for (ffb = dummyForcefieldAngles_.first(); ffb != NULL; ffb = ffb->next) if (ffb->namesMatch(i->equivalent(), j->equivalent(), k->equivalent())) break;
	if (ffb == NULL)
	{
		ffb = dummyForcefieldAngles_.add();
		ffb->setType(ForcefieldBound::AngleInteraction);
		ffb->setAngleForm(AngleFunctions::Ignore);
		ffb->setTypeName(0, i->equivalent());
		ffb->setTypeName(1, j->equivalent());
		ffb->setTypeName(2, k->equivalent());
	}
	return ffb;
}

// Create (or return existing) angle bond term for supplied atom types
ForcefieldBound *Pattern::createDummyTorsion(ForcefieldAtom *i, ForcefieldAtom *j, ForcefieldAtom *k, ForcefieldAtom *l)
{
	// Search for existing term...
	ForcefieldBound *ffb;
	for (ffb = dummyForcefieldTorsions_.first(); ffb != NULL; ffb = ffb->next) if (ffb->namesMatch(i->equivalent(), j->equivalent(), k->equivalent(), l->equivalent())) break;
	if (ffb == NULL)
	{
		ffb = dummyForcefieldTorsions_.add();
		ffb->setType(ForcefieldBound::TorsionInteraction);
		ffb->setTorsionForm(TorsionFunctions::Ignore);
		ffb->setTypeName(0, i->equivalent());
		ffb->setTypeName(1, j->equivalent());
		ffb->setTypeName(2, k->equivalent());
		ffb->setTypeName(3, l->equivalent());
	}
	return ffb;
}

// Calculate centre of geometry for molecule 'mol' in pattern, from (Model) config supplied or parent_ if NULL
Vec3<double> Pattern::calculateCog(int mol, Model *srcmodel)
{
	// Calculate the centre of geometry for this molecule
	msg.enter("Pattern::calculateCog");
	int offset = startAtom_ + mol*nAtoms_;
	if (srcmodel == NULL) srcmodel = parent_;
	msg.print(Messenger::Verbose,"Calculating COG for pattern '%s', molecule %i (starting at %i, nMols=%i)\n", name_.get(), mol, offset, nMolecules_);
	static Vec3<double> cog, mim_i;
	UnitCell *cell = srcmodel->cell();
	Atom **modelatoms = srcmodel->atomArray();
	cog = modelatoms[offset]->r();
	for (int a1=1; a1 < nAtoms_; ++a1)
	{
		// Do minimum image w.r.t. first atom in molecule
// 		mim_i = cell->mim(modelatoms[a1]->r(), modelatoms[offset]->r());
		mim_i = cell->mim(modelatoms[offset]->r(), cog / a1);
		cog += mim_i;
		++offset;
	}
	cog /= nAtoms_;
	msg.exit("Pattern::calculateCog");
	return cog;
}

// Calculate centre of mass for molecule 'mol' in pattern, from config supplied
Vec3<double> Pattern::calculateCom(int mol, Model *srcmodel)
{
	// Calculate the centre of geometry for this molecule
	msg.enter("Pattern::calculateCom");
	msg.print(Messenger::Verbose,"Calculating centre-of-mass for molecule %i in pattern '%s' (pattern nMols=%i)\n", mol, name_.get(), nMolecules_);
	Vec3<double> com;
	if (srcmodel == NULL) srcmodel = parent_;
	double massnorm = 0.0;
	static Vec3<double> mim_i;
	int offset = startAtom_ + mol*nAtoms_;
	com.zero();
	msg.print(Messenger::Verbose,"molecule_com : Offset = %i\n", offset);
	UnitCell *cell = srcmodel->cell();
	Atom **modelatoms = srcmodel->atomArray();
	for (int a1=offset; a1<offset+nAtoms_; a1++)
	{
		// Do minimum image w.r.t. first atom in molecule
		mim_i = cell->mim(modelatoms[a1]->r(), modelatoms[offset]->r());
		com += mim_i * Elements().atomicMass(modelatoms[a1]->element());
		massnorm += Elements().atomicMass(modelatoms[a1]->element());
	}
	com /= massnorm;
	msg.exit("Pattern::calculateCom");
	return com;
}

/*
// Data Propagation / Selector Routines
*/

void Pattern::propagateAtomtypes()
{
	// Copy type information contained in the first molecule in the pattern to all other molecules in the pattern
	msg.enter("Pattern::propagateAtomtypes");
	Atom *i, *j;
	int n, m;
	j = firstAtom_;
	for (n=0; n<nAtoms_; n++) j = j->next;
	// Loop over other molecules and copy the data
	for (n=1; n<nMolecules_; n++)
	{
		// Set pointer 'i' to be the starting atom of the first molecule
		i = firstAtom_;
		for (m=0; m<nAtoms_; m++)
		{
			j->setEnvironment(i->environment());
			j->setType(i->type());
			i = i->next;
			j = j->next;
		}
	}
	msg.exit("Pattern::propagateAtomtypes");
}

void Pattern::propagateBondTypes()
{
	// Copy the bond type data in the first molecule of the pattern to all other molecules in the pattern.
	// With a loop over all other molecules, loop over the atoms of the first molecule. For each bond on this atom,
	// find the relative atom id and search for the corresponding atom in the n'th molecule.
	msg.enter("Pattern::propagateBondTypes");
	int n,m,o,offset;
	Atom *i, *j, *k;
	Refitem<Bond,int> *bref;
	Bond *b1, *b2;
	// Set the pointer 'j' to be the first atom of the second molecule
	j = firstAtom_;
	for (n=0; n<nAtoms_; n++) j = j->next;
	// Loop over remaining molecules
	for (n=1; n<nMolecules_; n++)
	{
		// Set 'i' to be the first atom of the first molecule
		i = firstAtom_;
		// Loop over atoms in molecule
		for (m=0; m<nAtoms_; m++)
		{
			bref = i->bonds();
			while (bref != NULL)
			{
				// Get id offset between 'i' and the atom at the other end of the bond 'b'
				b1 = bref->item;
				offset = b1->partner(i)->id() - i->id();
				// Now get atom in current molecule with same id offset
				k = j;
				if (offset > 0) for (o=0; o<offset; o++) k = k->next;
				else for (o=0; o<abs(offset); o++) k = k->prev;
				// 'k' now points to the bond partner of 'j'
				b2 = j->findBond(k);
				if (b2 == NULL) msg.print("Bizarre fatal error. Couldn't find bond in Pattern::propagateBondTypes\n");
				else b2->setType(b1->type());
				bref = bref->next;
			}
			i = i->next;
			j = j->next;
		}
	}
	msg.exit("Pattern::propagateBondTypes");
}

// Select atom 'i' in all molecules
void Pattern::selectAtom(int id, bool markonly, bool deselect)
{
	msg.enter("Pattern::selectAtom");
	int n,m;
	Atom *i = firstAtom_;
	for (m=0; m<nMolecules_; m++)
	{
		for (n=0; n<nAtoms_; n++)
		{
			if (n == id) deselect ? parent_->deselectAtom(i, markonly) : parent_->selectAtom(i, markonly);
			i = i->next;
		}
	}
	msg.exit("Pattern::selectAtom");
}

/*
// Atomic Routines
*/

// Append atom
Atom *Pattern::appendCopy(Atom *source)
{
	// Append the supplied atom to the pattern's 'local' atom list
	msg.enter("Pattern::appendCopy");
	Atom *newatom = new Atom;
	firstAtom_ == NULL ? firstAtom_ = newatom : lastAtom_->next = newatom;
	newatom->prev = lastAtom_;
	lastAtom_ = newatom;
	newatom->copy(source);
	totalAtoms_ ++;
	msg.exit("Pattern::appendCopy");
	return newatom;
}

// Delete atom
void Pattern::deleteAtom(Atom *xatom)
{
	// Delete the supplied atom from the pattern's 'local' atom list
	msg.enter("Pattern::deleteAtom");
	xatom->prev == NULL ? firstAtom_ = xatom->next : xatom->prev->next = xatom->next;
	xatom->next == NULL ? lastAtom_ = xatom->prev : xatom->next->prev = xatom->prev;
	delete xatom;
	totalAtoms_ --;
	msg.exit("Pattern::deleteAtom");
}

// Delete atoms from end
void Pattern::deleteAtomsFromEnd(int count)
{
	// Deletes a number 'n' of atoms from the end of the list (i.e. recently created ones)
	msg.enter("Pattern::deleteAtomsFromEnd");
	for (int n=0; n<count; n++) deleteAtom(lastAtom_);
	msg.exit("Pattern::deleteAtomsFromEnd");
}

/*
// Ring Routines
*/

// Find rings
void Pattern::findRings()
{
	msg.enter("Pattern::findRings");
	int n, rsize;
	bool okay = TRUE;
	Atom *i;
	Ring path;
	// Loop over atoms, searching for rings on each
	i = firstAtom_;
	for (n=0; n<nAtoms_; n++)
	{
		if (i->nBonds() > 1)
		{
			// Loop over searches for different ring sizes
			for (rsize=3; rsize<=prefs.maxRingSize(); rsize++)
			{
				path.clear();
				path.setRequestedSize(rsize);
				// Call the recursive search to extend the path by this atom
				okay = ringSearch(i,&path);
				if (!okay) break;
			}
		}
		i = i->next;
		if (!okay) break;
	}
	if ((!okay) && (rings_.nItems() == prefs.maxRings())) msg.print("Maximum number of rings (%i) reached for pattern '%s'...\n", prefs.maxRings(), name_.get());
	msg.print(Messenger::Verbose, "Pattern '%s' contains %i cycles of %i atoms or less.\n", name_.get(), rings_.nItems(), prefs.maxRingSize());
	msg.exit("Pattern::findRings");
}

// Ring search
bool Pattern::ringSearch(Atom *i, Ring *currentpath)
{
	// Extend the path (ring) passed by the atom 'i', searching for a path length of 'ringsize'
	msg.enter("Pattern::ringSearch");
	Refitem<Bond,int> *bref;
	Ring *r;
	Refitem<Atom,int> *lastra;
	bool done, maxreached = FALSE;
	// Otherwise, add it to the current path
	lastra = currentpath->lastAtom();
	if (currentpath->addAtom(i))
	{
		msg.print(Messenger::Verbose," --- Atom i added to path :  ");
		currentpath->print();
		// Adding the atom did not exceed the requested ring size, and is not a duplicate
		// Go through the list of atoms bound to 'i' and then:
		//  -- If nAtoms_<=requestedsize && 'i' is bound to the first atom in the path, store the ring.
		//  Otherwise, if nAtoms_<requestedsize then extend the ring by each of the bound atoms in turn.
		done = FALSE;
		bref = i->bonds();
		while (bref != NULL)
		{
			// Don't consider the atom we just came from...
			if (lastra != NULL)
				if (bref->item->partner(i) == lastra->item) { bref = bref->next; continue; }
			// Depending on the size check, extend or store current path.
			if (currentpath->nAtoms() == currentpath->requestedSize())
			{
				// The correct number of atoms is in the current path. Does it form a cycle?
				if (i->findBond(currentpath->atoms()->item) != NULL)
				{
					// Check to see if this ring already exists in the rings_ list...
					if (!isRingInList(currentpath))
					{
						msg.print(Messenger::Verbose," --- Storing current ring.\n");
						if (rings_.nItems() == prefs.maxRings()) maxreached = TRUE;
						else
						{
							r = rings_.add();
							r->setParent(this);
							r->copy(currentpath);
							r->finalise();
							r->print();
							done = TRUE;
						}
					}
				}
			}
			else
			{
				// Current path is not long enough, so extend it
				if (!ringSearch(bref->item->partner(i),currentpath)) maxreached = TRUE;
			}
			bref = bref->next;
			if (done || maxreached) break;
		}
		// Return the list to its original state
		msg.print(Messenger::Verbose," --- Removing atom %s[%p] from current path...\n",Elements().symbol(i),i);
		currentpath->removeAtom(currentpath->lastAtom());
	}
// 	else printf(" --- Atom is already in list, or adding it exceeds specified ringsize.\n");
	msg.exit("Pattern::ringSearch");
	return (!maxreached);
}

// Search existing ring list for existence of supplied ring
bool Pattern::isRingInList(Ring *source)
{
	for (Ring *r = rings_.first(); r != NULL; r = r->next) if (*r == *source) return TRUE;
	return FALSE;
}

// Return total bond order penalty of atoms in one molecule of the pattern
int Pattern::totalBondOrderPenalty()
{
	Atom *i = firstAtom_;
	int result = 0;
	for (int n=0; n<nAtoms_; n++)
	{
		result += Elements().bondOrderPenalty(i, i->totalBondOrder()/2);
		i = i->next;
	}
	return result;
}

// New augmentation code
void Pattern::augment()
{
	msg.enter("Pattern::augment");
	Atom *i, *j;
	Reflist<Bond,Bond::BondType> refbonds;
	Refitem<Bond,Bond::BondType> *rb;
	Refitem<Bond,int> *bref, *heavybond = NULL, *bestref, *bref2;
	Refitem<Atom,int> *aref;
	Reflist<Bond,int> bondlist;
	Bond *b1, *b2, *b3;
	Bond::BondType bt;
	int n, nHeavy, totalpenalty, ringpenalty, newpenalty, m, o, p;

	msg.print("Augmenting bonds in pattern %s...\n",name_.get());
	/*
	Assume the structure is chemically 'correct' - i.e. each atom is bound to a likely number of other atoms.
	If hydrogens are missing then the results will be unpredictable.
	Based on methods suggested in:
	'Automatic atom type and bond type perception in molecular mechanical calculations'
	J. Wang, W. Wang, P. A. Kollman, and D. A. Case
	Journal of Molecular Graphics and Modelling, 25 (2), 247-260 (2006)
	*/
	// Stage 1 - Augment heavy atoms with only one heavy atom bond
	i = firstAtom_;
	for (n=0; n<nAtoms_; n++)
	{
		if (i->element() == 1) { i = i->next; continue; }
		// Calculate number of heavy atoms attached
		nHeavy = 0;
		for (bref = i->bonds(); bref != NULL ; bref = bref->next)
		{
			if (bref->item->partner(i)->element() != 1)
			{
				nHeavy ++;
				heavybond = bref;
			}
		}
		if (nHeavy == 1) parent_->changeBond(heavybond->item, heavybond->item->augmented());
		i = i->next;
	}

	// Stage 2 - Augment around heavy atom centres
 	i = firstAtom_;
	for (n=0; n<nAtoms_; n++)
	{
		if ((i->element() == 1) || (i->nBonds() < 2))
		{
			i = i->next;
			continue;
		}

		// Construct a working reflist of non-hydrogen bonds around this atom, and the current penalty value centred on it
		o = 0;
		totalpenalty = 0;
		refbonds.clear();
		for (bref = i->bonds(); bref != NULL; bref = bref->next)
		{
			j = bref->item->partner(i);
			if (j->element() == 1) continue;

			refbonds.add(bref->item, bref->item->type());
			totalpenalty += Elements().bondOrderPenalty(j, j->totalBondOrder()/2);
			if (bref->item->type() == Bond::Single) ++o;
		}
		// If all bonds are single, no point in changing them so move on
		if (o == i->nBonds())
		{
			i = i->next;
			continue;
		}

		// Now, shuffle bond types and see if a better overall penalty can be found
		for (o=1; o<refbonds.nItems(); ++o)
		{
			// Shuffle bond types in our temporary list
			bt = refbonds[0]->item->type();
			for (m=0; m<refbonds.nItems()-1; ++m) refbonds[m]->data = refbonds[m+1]->data;
			refbonds[refbonds.nItems()-1]->data = bt;

			// Calculate new penalty
			newpenalty = 0;
			for (rb = refbonds.first(); rb != NULL; rb = rb->next)
			{
				j = rb->item->partner(i);
				newpenalty += Elements().bondOrderPenalty(j, (j->totalBondOrder() - rb->item->order()*2 + Bond::order(rb->data)*2)/2);
			}

			// If new penalty is better, change the bonds around
			if (newpenalty < totalpenalty)
			{
				for (rb = refbonds.first(); rb != NULL; rb = rb->next) parent_->changeBond(rb->item, rb->data);
				totalpenalty = newpenalty;
			}
		}
		i = i->next;
	}

	// Stage 3 - Augmenting each bond locally
	// Construct a bond list for us to work from
	bondlist.clear();
 	i = firstAtom_;
	for (n=0; n<nAtoms_; n++)
	{
		if ((i->element() == 1) || (Elements().bondOrderPenalty(i, i->totalBondOrder()/2) == 0))
		{
			i = i->next;
			continue;
		}
		for (bref = i->bonds(); bref != NULL; bref = bref->next)
		{
			j = bref->item->partner(i);
			if (i->id() > j->id()) continue;
			bondlist.addUnique(bref->item);
		}
		i = i->next;
	}
	// Now we work with the bondlist. Repeatedly make the best move we possible can from all available bonds, until no more good moves are available
	if (bondlist.nItems() > 0) while (1)
	{
		for (bref = bondlist.first(); bref != NULL; bref = bref->next)
		{
			b1 = bref->item;
			i = b1->atomI();
			j = b1->atomJ();
			// Store original bond type and get augmented 'best' penalty
			bt = b1->type();
			b1->setType(b1->augmented());
			bref->data = Elements().bondOrderPenalty(i, i->totalBondOrder()/2) + Elements().bondOrderPenalty(j, j->totalBondOrder()/2);
			// Reset back to original bond type and subtract orginal bond order penalty
			b1->setType(bt);
			bref->data -= (Elements().bondOrderPenalty(i, i->totalBondOrder()/2) + Elements().bondOrderPenalty(j, j->totalBondOrder()/2));
// 			printf("Option: for bond %i-%i change from %s to %s gives delta of %i\n", i->id()+1, j->id()+1, Bond::bondType(bt), Bond::bondType(b1->augmented()), bref->data);
		}
		// Find lowest score (i.e. best move to make)
		bestref = bondlist.first();
		for (bref = bondlist.first(); bref != NULL; bref = bref->next) if (bref->data < bestref->data) bestref = bref;
		if (bestref->data >= 0) break;
		else parent_->changeBond(bestref->item, bestref->item->augmented());
	}
	
	// Stage 4 - Attempt to fix any problems, mostly with (poly)cyclic systems
	// Get total, reference bond order penalty for the molecule - we will try to reduce this as much as possible if we can
	totalpenalty = totalBondOrderPenalty();
	msg.print(Messenger::Verbose, "Bond order penalty after first pass is %i.\n", totalpenalty);
	if (totalpenalty > 0)
	{
		msg.print("...Augmentation second pass...\n");
		// Construct bond reference list for the first molecule, storing current bond type in extradata for our base reference
		Reflist<Bond,Bond::BondType> bondlist;
		i = firstAtom_;
		for (n=0; n<nAtoms_; n++)
		{
			for (bref = i->bonds(); bref != NULL; bref = bref->next) bondlist.addUnique(bref->item, bref->item->type());
			i = i->next;
		}
		// First, search for atoms in rings that have a bond order penalty
		for (Ring *r = rings_.first(); r != NULL; r = r->next)
		{
			// Try a straight augmentation of the ring first...
			for (rb = r->bonds(); rb != NULL; rb = rb->next)
				parent_->changeBond(rb->item, rb->item->augmented());

			// Get current total bond-order penalty of atoms in ring
			ringpenalty = r->totalBondOrderPenalty();
			if (ringpenalty == 0) continue;
			// Store current bond types in the ring bondlist's extradata so we can undo any changes
			r->storeBondTypes();

			// Find a double bond in the current ring, and convert into one single and two double bonds
 			for (aref = r->atoms(); aref != NULL; aref = aref->next)
			{
				b2 = aref->item->findBond(r->getNext(aref)->item);
				if (b2->type() != Bond::Double) continue;
				// Get bond neighbours
				b1 = aref->item->findBond(r->getPrev(aref)->item);
				b3 = r->getNext(aref)->item->findBond(r->getNext(r->getNext(aref))->item);
				b1->setType(Bond::Double);
				b2->setType(Bond::Single);
				b3->setType(Bond::Double);
				// Get new total bond order for ring - if we've made things worse, revert and try just shifting the bond
				newpenalty = totalBondOrderPenalty();
				if (newpenalty > ringpenalty)
				{
					// Try shift left...
					r->recallBondTypes();
					b1->setType(Bond::Double);
					b2->setType(Bond::Single);
					newpenalty = totalBondOrderPenalty();
					if (newpenalty > ringpenalty)
					{
						// Try shift right...
						r->recallBondTypes();
						b3->setType(Bond::Double);
						b2->setType(Bond::Single);
						newpenalty = totalBondOrderPenalty();
						if (newpenalty > ringpenalty)
						{
							r->recallBondTypes();
							continue;
						}
					}
				}
				// Now try to re-augment other atoms in the ring with the modified bonding arrangement
 				for (rb = r->bonds(); rb != NULL; rb = rb->next)
 					parent_->changeBond(rb->item, rb->item->augmented());
				// Check total again - if less than the previous 'ringpenalty' than store new bonds and continue
				newpenalty = r->totalBondOrderPenalty();
				if (newpenalty < ringpenalty)
				{
					r->storeBondTypes();
					totalpenalty = totalpenalty - ringpenalty + newpenalty;
					ringpenalty = newpenalty;
					//printf("New total penalty = %i\n", totalpenalty);
				}
				else r->recallBondTypes();
				if (newpenalty == 0) break;
			}

		}
		msg.print(Messenger::Verbose, "Bond order penalty after second pass is %i.\n", totalpenalty);
	}
	// Copy bond types from extradata back into first pattern molecule, using the parent_ so it may be undone
	propagateBondTypes();
	msg.exit("Pattern::augment");
}

void printstuff(Pattern *p)
{
	Atom *i = p->firstAtom();
	for (int n=0; n<p->nAtoms(); n++)
	{
		msg.print(Messenger::Verbose,"Atom %i, %s[%i], nbonds=%i, type=%s\n", n, Elements().symbol(i),
			i->id(),i->nBonds(),Atom::atomEnvironment(i->environment()));
		i = i->next;
	}
}

/*
// Atom typing
*/

// Describe atom / ring types
void Pattern::describeAtoms()
{
	// 1) Locate ring structures
	msg.print(Messenger::Verbose, "Detecting rings in pattern '%s'...\n", name_.get());
	findRings();
	// 2) Reset atom environments
	msg.print(Messenger::Verbose, "Determining atom environments in pattern '%s'...\n", name_.get());
	clearEnvironments();
	printstuff(this);
	// 3) Assign hybridisation types
	assignEnvironments();
	printstuff(this);
	// 4) Go through the ring list and see if any are aromatic
	msg.print(Messenger::Verbose, "Assigning ring aromaticities in pattern '%s'...\n", name_.get());
	for (Ring *r = rings_.first(); r != NULL; r = r->next) r->detectType();
}

// Clear environment data
void Pattern::clearEnvironments()
{
	// Set all environment flags of the atoms in pattern to Atomtype::NoEnvironment
	msg.enter("Pattern::clearEnvironments");
	Atom *i = firstAtom_;
	for (int n=0; n<nAtoms_; n++)
	{
		i->setEnvironment(Atom::NoEnvironment);
		i = i->next;
	}
	msg.exit("Pattern::clearEnvironments");
}

// Assign environment data
void Pattern::assignEnvironments()
{
	// Assign hybridisation types to the atoms in this pattern.
	msg.enter("Pattern::assignEnvironments");
	Atom *i = firstAtom_;
	int nsingle, nother;
	for (int n=0; n<nAtoms_; n++)
	{
		// Set to no environment to begin with
		i->setEnvironment(Atom::NoEnvironment);
		// Work out the hybridisation based on the bond types connected to the atom.
		// We can increase the hybridisation at any point, but never decrease it.
		nsingle = 0;
		nother = 0;
		for (Refitem<Bond,int> *bref = i->bonds(); bref != NULL; bref = bref->next)
		{
			switch (bref->item->type())
			{
				case (Bond::Single):
					nsingle ++;
					break;
				case (Bond::Aromatic):
				case (Bond::Double):
				case (Bond::Triple):
					nother ++;
					break;
				default:
					printf("Warning - Unrecognised bond type in Pattern::assignEnvironments.\n");
					nother ++;
					break;
			}
		}
		if (i->nBonds() == 0) i->setEnvironment(Atom::UnboundEnvironment);
		else if (nother == 0) i->setEnvironment(Atom::PureEnvironment);
		else i->setEnvironment(Atom::NonPureEnvironment);
		i = i->next;
	}
	msg.exit("Pattern::assignEnvironments");
}

// Type atoms in pattern
bool Pattern::typeAtoms()
{
	// Assign atom types from the forcefield based on the typing rules supplied.
	// Since there may be more than one match for a given atom (when relaxed rules are used, e.g.
	// UFF) we find the best of the types available. If any one criterion doesn't match in the atom 
	// type description, we reject it. Otherwise, store the number of criteria that matched and only
	// accept a different atom type if we manage to match a complete set containing more rules.
	// Return FALSE if one or more atoms could not be typed
	msg.enter("Pattern::typeAtoms");
	int a, newmatch, bestmatch, nfailed;
	Neta *at;
	Atom *i;
	Forcefield *ff;
	ForcefieldAtom *ffa;
	bool result = TRUE;
	// Select the forcefield we're typing with. First, if this pattern doesn't have a specific ff, take the model's ff
	ff = forcefield_;
	if (ff == NULL)
	{
		// No forcefield associated to pattern - grab parent Model's
		ff = parent_->forcefield();
		if (ff == NULL)
		{
			msg.print("Typing pattern %s (using default forcefield)...", name());
			ff = aten.currentForcefield();
		}
		else msg.print("Typing pattern %s (inheriting Model's forcefield '%s')...", name(), ff->name());
	}
	else msg.print("Typing pattern %s (using associated forcefield '%s')...", name(), ff->name());	
	if (ff == NULL)
	{
		msg.print("Can't type pattern '%s' - no forcefield associated to pattern or model, and no default set.\n", name_.get());
		msg.exit("Pattern::typeAtoms");
		return FALSE;
	}
	// Loop over atoms in the pattern's molecule
	i = firstAtom_;
	nfailed = 0;
	for (a=0; a<nAtoms_; ++a)
	{
		// Check to see if this atom type has been manually set
		if (i->hasFixedType())
		{
			i = i->next;
			continue;
		}
		msg.print(Messenger::Typing,"Pattern::typeAtoms : FFTyping atom number %i, element %s\n", a, Elements().symbol(i->element()));
		bestmatch = 0;
		parent_->setAtomType(i, NULL, FALSE);
		// Check for element 'XX' first
		if (i->element() == 0)
		{
			msg.print("Failed to type atom %i since it has no element type.\n",i->id()+1);
			nfailed ++;
			result = FALSE;
		}
		// Loop over forcefield atom types
		for (ffa = ff->types(); ffa != NULL; ffa = ffa->next)
		{
			// Grab next atomtype
			at = ffa->neta();

			// First, check element is the same, otherwise skip
			if (i->element() != at->characterElement()) continue;
			
			// See how well this ff description matches the environment of our atom 'i'
			msg.print(Messenger::Typing,"Pattern::typeAtoms : Matching type id %i\n",ffa->typeId());
			newmatch = at->matchAtom(i,&rings_,parent_);
			msg.print(Messenger::Typing,"Pattern::typeAtoms : ...Total match score for type %i = %i\n", ffa->typeId(), newmatch);
			if (newmatch > bestmatch)
			{
				// Better match found...
				bestmatch = newmatch;
				i->setType(ffa);
			}
		}
		if (i->type() == NULL)
		{
			msg.print("Failed to type atom - %s, id = %i, nbonds = %i.\n", Elements().name(i), i->id()+1, i->nBonds());
			nfailed ++;
			result = FALSE;
		}
		else msg.print(Messenger::Typing,"Assigned forcefield type for atom is : %i (%s)\n", i->type()->typeId(), i->type()->name());
		i = i->next;
	}
	// Print warning if we failed...
	if (nfailed != 0) msg.print("Failed to type %i atoms in pattern '%s'.\n", nfailed, name_.get());
	msg.exit("Pattern::typeAtoms");
	return result;
}
