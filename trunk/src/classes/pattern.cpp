/*
	*** Molecule pattern
	*** src/classes/pattern.cpp
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

#include "classes/pattern.h"
#include "classes/ring.h"
#include "templates/vector3.h"
#include "model/model.h"
#include "base/elements.h"
#include "base/messenger.h"

// Constructors
Pattern::Pattern()
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

	// Public variables
	molecule = new Model;
	prev = NULL;
	next = NULL;
}

PatternAtom::PatternAtom()
{
	// Private variables
	data_ = NULL;
	atom_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;
}

PatternBound::PatternBound()
{
	// Private variables
	for (int i=0; i<MAXFFBOUNDTYPES; i++) id_[i] = -1;
	data_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;
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

/*
// Pattern Bound
*/

// Set atom id
void PatternBound::setAtomId(int n, int i)
{
	(n < MAXFFBOUNDTYPES ? id_[n] = i : printf("OUTOFRANGE:PatternBound"));
}

// Return atom id
int PatternBound::atomId(int n)
{
	return id_[n];
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

/*
// Pattern
*/

// Sets the ID of the pattern
void Pattern::setId(int i)
{
	id_ = i;
}

// Returns then numerical ID of the pattern
int Pattern::id()
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
int Pattern::offset(int mol)
{
	return startAtom_ + mol*nAtoms_;
}

// Returns the number of atoms in one molecule of the pattern
int Pattern::nAtoms()
{
	return nAtoms_;
}

// Return number of bonds in one molecule of the pattern
int Pattern::nBonds()
{
	return bonds_.nItems();
}

// Return number of angles in one molecule of the pattern
int Pattern::nAngles()
{
	return angles_.nItems();
}

// Return number of torsions in one molecule of the pattern
int Pattern::nTorsions()
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

// Sets the starting atom of the model
void Pattern::setStartAtom(int n)
{
	startAtom_ = n;
}

// Returns the starting atom id of the pattern
int Pattern::startAtom()
{
	return startAtom_;
}

// Sets the end atom of the model
void Pattern::setEndAtom(int n)
{
	endAtom_ = n;
}

// Returns the ending atom id of the pattern
int Pattern::endAtom()
{
	return endAtom_;
}

// (Re)Calculate totalAtoms_
void Pattern::calcTotalAtoms()
{
	totalAtoms_ = nAtoms_ * nMolecules_;
}

// Returns the total number of atoms in the pattern
int Pattern::totalAtoms()
{
	return totalAtoms_;
}

// Sets the number of molecules in the pattern
void Pattern::setNMolecules(int n)
{
	nMolecules_ = n;
}

// Returns the number of molecules in the pattern
int Pattern::nMolecules()
{
	return nMolecules_;
}

// Sets the expected number of molecules in the pattern
void Pattern::setNExpectedMolecules(int n)
{
	nExpectedMols_ = n;
}

// Returns the expected number of molecules in the pattern
int Pattern::nExpectedMolecules()
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
bool Pattern::isFixed()
{
	return fixed_;
}

// Sets the name of the pattern 
void Pattern::setName(const char *s)
{
	name_ = s;
}

// Returns the pattern name
const char *Pattern::name()
{
	return name_.get();
}

// Sets the forcefield to use in the pattern
void Pattern::setForcefield(Forcefield *newff)
{
	forcefield_ = newff;
}

// Gets the forcefield associated with the pattern
Forcefield *Pattern::forcefield()
{
	return forcefield_;
}

// Returns whether the atomlimit in the pattern is valid
bool Pattern::isAtomLimitOk()
{
	return testAtomLimit_;
}

// Returns whether the element composition in the pattern molecules is uniform
bool Pattern::areElementsOk()
{
	return testElement_;
}

// Returns whether the bonding in the pattern molecules is uniform
bool Pattern::isBondingOk()
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

// Returns the first ring in the ring list
Ring *Pattern::rings()
{
	return rings_.first();
}

// Initialise
void Pattern::initialise(int patid, int start, int mols, int atomsmol)
{
	// Initialise atom pointers / values in pattern.
	msg.enter("Pattern::initialise");
	if (parent_ == NULL)
	{
		printf("Owner model has not been set in pattern!\n");
		msg.exit("Pattern::initialise");
		return;
	}
	// Store parameters
	id_ = patid;
	nMolecules_ = mols;			// Number of molecules in pattern
	nAtoms_ = atomsmol;		// Number of atoms per molecule
	totalAtoms_ = mols*nAtoms_;	// Total number of atoms described by the pattern
	startAtom_ = start;		// Starting atom (integer position in atom list)
	endAtom_ = start + nAtoms_ - 1;	// Last atom in first molecule (integer position in atom list)
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
	msg.print("New pattern node : start=%i, nMols=%i, nAtoms/mol=%i, totalAtoms=%i, name=%s\n", startAtom_, nMolecules_, nAtoms_, totalAtoms_, name_.get());
	msg.exit("Pattern::initialise");
}

// Empty the selected pattern
void Pattern::empty()
{
	// Set all variables to reflect an empty pattern
	msg.enter("Pattern::empty_pattern");
	// Zero everything except nAtoms_
	firstAtom_ = NULL;
	lastAtom_ = NULL;
	nMolecules_ = 0;
	totalAtoms_ = 0;
	msg.exit("Pattern::empty_pattern");
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
	// Note - this must be called *after* the expression bound terms have been filled in.
	msg.enter("Pattern::createMatrices");
	int n, m, a1, a2;
	PatternBound *pb;
	for (n=0; n<nAtoms_; n++)
		for (m=0; m<nAtoms_; m++) conMatrix_[n][m] = 0;

	// First, build up the bond matrix
	for (pb = bonds_.first(); pb != NULL; pb = pb->next)
	{
		conMatrix_[ pb->atomId(0) ] [ pb->atomId(1) ] = 1;
		conMatrix_[ pb->atomId(1) ] [ pb->atomId(0) ] = 1;
	}

	// Print out the bonding matrix
/*	printf("Bonding Matrix\n");
	for (n=0; n<nAtoms_; n++)
	{
		for (m=0; m<nAtoms_; m++) printf (" %2i ",conMatrix_[n][m]);
		printf("\n");
	} */

	// Now, transform into the connectivity matrix.
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

/*	printf("Connectivity Matrix\n");
	for (n=0; n<nAtoms_; n++)
	{
		for (m=0; m<nAtoms_; m++) printf ("%2i",conMatrix_[n][m]);
		printf("\n"); 
	} */

	// Create scale matrices
	updateScaleMatrices();

	msg.exit("Pattern::createMatrices");
}

// Update scale matrices in pattern
void Pattern::updateScaleMatrices()
{
	msg.enter("Pattern::updateScaleMatrices");
	int n, m;
	PatternBound *pb;
	// Set all matrix elements to '1.0' initially. Then cycle over torsions, then angles, then bonds and set values accordingly.
	for (n=0; n<nAtoms_; n++)
		for (m=0; m<nAtoms_; m++)
		{
			vdwScaleMatrix_[n][m] = 1.0;
			elecScaleMatrix_[n][m] = 1.0;
		}
	// Interactions at tneds of torsion atoms are scaled by the factors stored in the torsion term
	for (pb = torsions_.first(); pb != NULL; pb = pb->next)
	{
		vdwScaleMatrix_[ pb->atomId(0) ] [ pb->atomId(3) ] = pb->data()->params().data[TF_VSCALE];
		vdwScaleMatrix_[ pb->atomId(3) ] [ pb->atomId(0) ] = pb->data()->params().data[TF_VSCALE];
		elecScaleMatrix_[ pb->atomId(0) ] [ pb->atomId(3) ] = pb->data()->params().data[TF_ESCALE];
		elecScaleMatrix_[ pb->atomId(3) ] [ pb->atomId(0) ] = pb->data()->params().data[TF_ESCALE];
	}
	// Atoms at end of angles are excluded from vdw/elec interactions.
	// Note that these elements are zeroed in the matrices, but the connectivity matrix is used to determined whether they are calculated.
	for (pb = angles_.first(); pb != NULL; pb = pb->next)
	{
		vdwScaleMatrix_[ pb->atomId(0) ] [ pb->atomId(2) ] = 0.0;
		vdwScaleMatrix_[ pb->atomId(2) ] [ pb->atomId(0) ] = 0.0;
		elecScaleMatrix_[ pb->atomId(0) ] [ pb->atomId(2) ] = 0.0;
		elecScaleMatrix_[ pb->atomId(2) ] [ pb->atomId(0) ] = 0.0;
	}
	for (pb = bonds_.first(); pb != NULL; pb = pb->next)
	{
		vdwScaleMatrix_[ pb->atomId(0) ] [ pb->atomId(1) ] = 0.0;
		vdwScaleMatrix_[ pb->atomId(1) ] [ pb->atomId(0) ] = 0.0;
		elecScaleMatrix_[ pb->atomId(0) ] [ pb->atomId(1) ] = 0.0;
		elecScaleMatrix_[ pb->atomId(1) ] [ pb->atomId(0) ] = 0.0;
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
	elcomp1 = new int[elements.nElements()+1];
	elcomp2 = new int[elements.nElements()+1];
	for (m=0; m<elements.nElements()+1; m++) elcomp1[m] = 0;
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
				for (a=0; a<elements.nElements()+1; a++) elcomp2[a] = 0;
				for (a=0; a<nAtoms_; a++)
				{
					elcomp2[i->element()] ++; i = i->next;
				}
				// ... and test against reference
				for (a=0; a<elements.nElements()+1; a++)
					if (elcomp1[a] != elcomp2[a]) ok = FALSE;
			}
			if (!ok)
			{
				msg.print("Pattern::validate : Failed element composition test at molecule %i.\n",m+1);
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

// Calculate centre of geometry for molecule 'mol' in pattern, from config supplied
Vec3<double> Pattern::calculateCog(Model *srcmodel, int mol)
{
	// Calculate the centre of geometry for this molecule
	msg.enter("Pattern::calculate_cog");
	int offset = startAtom_ + mol*nAtoms_;
	msg.print(Messenger::Verbose,"Pattern::calculate_cog : Calculating for pattern '%s', molecule %i (starting at %i, nMols=%i)\n", name_.get(), mol, offset, nMolecules_);
	static Vec3<double> cog, mim_i;
	Cell *cell = srcmodel->cell();
	cog.zero();
	Atom **modelatoms = srcmodel->atomArray();
	for (int a1=offset; a1<offset+nAtoms_; a1++)
	{
		// Do minimum image w.r.t. first atom in molecule
		mim_i = cell->mim(modelatoms[a1]->r(), modelatoms[offset]->r());
		cog += mim_i;
	}
	cog /= nAtoms_;
	msg.exit("Pattern::calculate_cog");
	return cog;
}

// Calculate centre of mass for molecule 'mol' in pattern, from config supplied
Vec3<double> Pattern::calculateCom(Model *srcmodel, int mol)
{
	// Calculate the centre of geometry for this molecule
	msg.enter("Pattern::calculateCom");
	msg.print(Messenger::Verbose,"Calculating centre-of-mass for molecule %i in pattern '%s' (pattern nMols=%i)\n", mol, name_.get(), nMolecules_);
	Vec3<double> com;
	double massnorm = 0.0;
	static Vec3<double> mim_i;
	int offset = startAtom_ + mol*nAtoms_;
	com.zero();
	msg.print(Messenger::Verbose,"molecule_com : Offset = %i\n",offset);
	Cell *cell = srcmodel->cell();
	Atom **modelatoms = srcmodel->atomArray();
	for (int a1=offset; a1<offset+nAtoms_; a1++)
	{
		// Do minimum image w.r.t. first atom in molecule
		mim_i = cell->mim(modelatoms[a1]->r(), modelatoms[offset]->r());
		com += mim_i * elements.atomicMass(modelatoms[a1]->element());
		massnorm += elements.atomicMass(modelatoms[a1]->element());
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
	// Copy type information contained in the first molecule in the pattern to all other molecules in the pattern, and the pattern's representative molecule
	msg.enter("Pattern::propagateAtomtypes");
	Atom *i, *j;
	int n, m;
	// Set 'j' to be the starting atom of the second molecule
	// Set representative molecule data at the same time
	j = firstAtom_;
	i = molecule->atoms();
	for (n=0; n<nAtoms_; n++)
	{
		i->setEnvironment(j->environment());
		i->setType(j->type());
		i = i->next;
		j = j->next;
	}
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
				else b2->setOrder(b1->order());
				bref = bref->next;
			}
			i = i->next;
			j = j->next;
		}
	}
	msg.exit("Pattern::propagateBondTypes");
}

// Select atom 'i' in all molecules
void Pattern::selectAtom(int id)
{
	msg.exit("Pattern::selectAtom");
	int n,m;
	Atom *i = firstAtom_;
	for (m=0; m<nMolecules_; m++)
	{
		for (n=0; n<nAtoms_; n++)
		{
			if (n == id) parent_->selectAtom(i);
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

// Clear 'tempi' values of all atoms in pattern
void Pattern::resetTempI(int value)
{
	// Sets the tempi variables of all atoms in the pattern to the value specified
	Atom *i = firstAtom_;
	for (int m=0; m<nAtoms_; m++)
	{
		i->tempi = value;
		i = i->next;
	}
}

/*
// Ring Routines
*/

// Mark atoms
void Pattern::markRingAtoms(Atom *i)
{
	// Recursive routine to knock-out atoms that have a lower ring forming potential than
	// their bond number suggests. If called with an atom whose potential is zero, we stop
	// since we've reached an already 'excluded' atom. Otherwise, check the bound neighbours
	// and reassess the potential of the atom. If its marked as zero potential, re-call the
	// routine on any neighbours that aren't already zero-marked.
	msg.enter("Pattern::markRingAtoms");
	if (i->tempi == 0)
	{
		msg.exit("Pattern::markRingAtoms");
		return;
	}
	else
	{
		// Check the listed neighbours to set the potential of this atom
		int count = 0;
		Refitem<Bond,int> *bref = i->bonds();
		while (bref != NULL)
		{
			if (bref->item->partner(i)->tempi != 0) count++;
			bref = bref->next;
		}
		// 'count' now contains the number of neighbours that have a non-zero potential.
		// If its a dead-end, re-call the routine. If not, just re-set the atom's potential
		switch (count)
		{
			case (1):
				// No potential for this atom to be in a ring, so mark it as zero'd...
				i->tempi = 0;
				// ...and call the routine on any bound neighbours since they now need to be checked.
				bref = i->bonds();
				while (bref != NULL)
				{
					if (bref->item->partner(i)->tempi != 0) markRingAtoms(bref->item->partner(i));
					bref = bref->next;
				}
				break;
			case (2): i->tempi = 1; break;
			case (3): i->tempi = 3; break;
			default : i->tempi = 6; break;
		}
	}
	msg.exit("Pattern::markRingAtoms");
}

// Find rings
void Pattern::findRings()
{
	// Locate rings in the molecule of the current pattern.
	// Maintain a local array corresponding to whether specific atoms are 'done' or not - i.e., whether
	// they have been check as as much as they need to be checked.
	msg.enter("Pattern::findRings");
	int n, rsize, ringpotential;
	Atom *i;
	Refitem<Bond,int> *bref;
	Ring path;
	// Set the initial states of the atoms. i->tempi maintains the maximum possible number of rings that each atom can form based on the number of bonds it has.
	i = firstAtom_;
	for (n=0; n<nAtoms_; n++)
	{
		switch (i->nBonds())
		{
			case (0) : i->tempi = 0; break;
			case (1) : i->tempi = 0; break;
			case (2) : i->tempi = 1; break;
			case (3) : i->tempi = 3; break;
			default  : i->tempi = 6; break;
		}
		i = i->next; 
	}
	// Since the number of bonds does not necessarily correlate to the maximum number of rings (e.g.
	// when an atom is part of an alkyl chain, nbonds=2 but it is not part of any rings) find any atom
	// with nbonds=1 and mark off any bound neighbours with nbonds
	i = firstAtom_;
	for (n=0; n<nAtoms_; n++)
	{
		if (i->nBonds() == 1)
		{
			bref = i->bonds();
			markRingAtoms(bref->item->partner(i));
		}
		i = i->next;
	}

	// Calculate the *total* potential for ring formation, i.e. the sum of all tempi values.
	ringpotential = 0;
	i = firstAtom_;
	for (n=0; n<nAtoms_; n++)
	{
		msg.print(Messenger::Verbose,"Atom %i : potential = %i\n",n,i->tempi);
		ringpotential += i->tempi;
		i = i->next;
	}

	// Now, go through the atoms and find cyclic routes.
	i = firstAtom_;
	for (n=0; n<nAtoms_; n++)
	{
		if (i->tempi == 0) { i = i->next; continue; }
		// Loop over searches for different ring sizes
		for (rsize=3; rsize<=prefs.maxRingSize(); rsize++)
		{
			path.clear();
			path.setRequestedSize(rsize);
			// Call the recursive search to extend the path by this atom
			if (ringpotential >= rsize) ringSearch(i,&path,ringpotential);
		}
		i = i->next;
	}
	msg.exit("Pattern::findRings");
}

// Ring search
void Pattern::ringSearch(Atom *i, Ring *currentpath, int &ringpotential)
{
	// Extend the path (ring) passed by the atom 'i', searching for a path length of 'ringsize'
	msg.enter("Pattern::ringSearch");
	Refitem<Bond,int> *bref;
	Ring *r;
	Refitem<Atom,int> *lastra;
	bool done;
	// First off, if this atom has no more available bonds for inclusion in rings, just return
	if (i->tempi == 0)
	{
		//printf(" --- No vacant 'bonds' on atom - skipping...\n");
		msg.exit("Pattern::ringSearch");
		return;
	}
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
				if (i->findBond(currentpath->firstAtom()->item) != NULL)
				{
					msg.print(Messenger::Verbose," --- Storing current ring.\n");
					r = rings_.add();
					r->copy(currentpath);
					// Must now update atom 'tempi' values to reflect the inclusion of these atoms in
					// another ring, and also the total ringpotential variable
					Refitem<Atom,int> *ra = r->firstAtom();
					while (ra != NULL)
					{
						ra->item->tempi -= 1;
						ringpotential -= 1;
						ra = ra->next;
					}
					r->finish();
					r->print();
					done = TRUE;
				}
			}
			else
			{
				// Current path is not long enough, so extend it
				ringSearch(bref->item->partner(i),currentpath,ringpotential);
			}
			bref = bref->next;
			if (done) break;
		}
		// Return the list to its original state
		msg.print(Messenger::Verbose," --- Removing atom %s[%li] from current path...\n",elements.symbol(i),i);
		currentpath->removeAtom(currentpath->lastAtom());
	}
	else
	{
		//printf(" --- Atom is already in list, or adding it exceeds specified ringsize.\n");
	}
	msg.exit("Pattern::ringSearch");
}

void Pattern::augment()
{
	msg.enter("Pattern::augment");
	Atom *i;
	Refitem<Bond,int> *bref;
	int n, nHeavy;
	msg.print("Augmenting bonds in pattern %s...\n",name_.get());
	/*
	We do not reset the present bonding assignments, only check if they're correct. If we find an atom whose
	bond order is too high, we only decrease it if we can find a bound atom in a similar situation.
	So, for the atom 'i':
	-- If its total bond order is equal to its natural valency, do nothing and move on.
	-- If its total bond order is less, get the bound atom with the highest unoccupied valency and increase
		the bond as much as possible. If 'i' is still not satisfied, repeat until all bound atoms have been
		tried.
	-- If its total bond order is higher, search for an atom that also has a too-high bond order. If one is found,
		decrease the bond enough to re-balance. If we can't find one, stop and throw an error.

	Perform this task in three stages to make the whole process more robust. First, do it for rings where we
	only bond within the cycle. Then, do it for terminal atoms or heavy atoms bound to only one other heavy
	atom. Then, do it for the rest.
	*/
	// 
	// Calculate current bond orders for atoms in the pattern.
	i = firstAtom_;
	for (n=0; n<nAtoms_; n++)
	{
		i->tempi = i->totalBondOrder() - 2*elements.valency(i->element());
		//printf("%i\n",i->tempi);
		i = i->next;
	}
	// Stage 1 - Augment heavy atoms with only one heavy atom bond
	i = firstAtom_;
	for (n=0; n<nAtoms_; n++)
	{
		// Calculate number of heavy atoms attached
		nHeavy = 0;
		bref = i->bonds();
		while (bref != NULL)
		{
			if (bref->item->partner(i)->element() != 1) nHeavy ++;
			bref = bref->next;
		}
		if (nHeavy == 1 && i->tempi != 0)
		{
			for (bref = i->bonds(); bref != NULL; bref = bref->next)
			{
				if (i->tempi == 0) break;
				if (i->tempi < 0) parent_->augmentBond(bref->item,+1);
				else if (i->tempi > 0) parent_->augmentBond(bref->item,-1);
			}
		}
		i = i->next;
	}
	// Stage 2 - Augment within cycles
	for (Ring *r = rings_.first(); r != NULL; r = r->next)
	{
		// Check atoms bond order difference
		for (Refitem<Atom,int> *ra = r->firstAtom(); ra != NULL; ra = ra->next)
			if (ra->item->tempi != 0) r->augmentAtom(ra, parent_);
	}
	// Stage 3 - Second pass, augmenting all atoms
	i = firstAtom_;
	for (n=0; n<nAtoms_; n++)
	{
		//printf("%li  i->tempi = %i\n",i,i->tempi);
		if (i->tempi != 0)
		{
			for (bref = i->bonds(); bref != NULL; bref = bref->next)
			{
				//printf("%li    bond   i->tempi = %i\n",i,i->tempi);
				if (i->tempi == 0) break;
				if (i->tempi < 0) parent_->augmentBond(bref->item,+1);
				else if (i->tempi > 0) parent_->augmentBond(bref->item,-1);
			}
		}
		i = i->next;
	}
	propagateBondTypes();
	msg.exit("Pattern::augment");
}

void Pattern::initExpression(bool vdwOnly)
{
	// Create arrays for storage of FF data for atoms, bonds, angles etc.
	// NBonds can be calculated through a loop over all atoms
	// NAngles can be calculated from atomic nBonds data.
	// NTorsions can be calculated from the bond list and atomic nBonds data.
	msg.enter("Pattern::initExpression");
	Atom *i;
	Refitem<Bond,int> *bref;
	int n, atomId, nBonds, nAngles, nTorsions;
	nBonds = 0;
	nAngles = 0;
	nTorsions = 0;
	// We always create the atom array.
	atoms_.createEmpty(nAtoms_);
	if (vdwOnly)
	{
		noIntramolecular_ = TRUE;
		msg.print("Expression for pattern '%s' contains Atomtype terms only.\n", name_.get());
	}
	else
	{
		for (i = parent_->atoms(); i != NULL; i = i->next)
		{
			atomId = i->id();
			if ((atomId >= startAtom_) && (atomId <= endAtom_))
			{
				// Bond counter
				nBonds += i->nBonds();
				// Angle counter
				for (n=i->nBonds()-1; n>0; n--) nAngles += n;
				// Torsion counter slightly more complicated - need a second loop of bound atoms
				for (bref = i->bonds(); bref != NULL; bref = bref->next)
					nTorsions += (i->nBonds() - 1) * (bref->item->partner(i)->nBonds() - 1);
			}
		}
		// Some totals are double counted, so...
		nBonds /= 2;
		nTorsions /= 2;
		msg.print("Expression for pattern '%s' contains %i bonds, %i angles, and %i torsions.\n", name_.get(), nBonds, nAngles, nTorsions);
		bonds_.createEmpty(nBonds);
		angles_.createEmpty(nAngles);
		torsions_.createEmpty(nTorsions);
	}
	if (conMatrix_ != NULL) msg.print("Pattern::initExpression : Warning - connectivity matrix was already allocated.\n");
	conMatrix_ = new int*[nAtoms_];
	for (n=0; n<nAtoms_; n++) conMatrix_[n] = new int[nAtoms_];
	if (vdwScaleMatrix_ != NULL) msg.print("Pattern::initExpression : Warning - VDW scaling matrix was already allocated.\n");
	vdwScaleMatrix_ = new double*[nAtoms_];
	for (n=0; n<nAtoms_; n++) vdwScaleMatrix_[n] = new double[nAtoms_];
	if (elecScaleMatrix_ != NULL) msg.print("Pattern::initExpression : Warning - electrostatic scaling matrix was already allocated.\n");
	elecScaleMatrix_ = new double*[nAtoms_];
	for (n=0; n<nAtoms_; n++) elecScaleMatrix_[n] = new double[nAtoms_];
	msg.exit("Pattern::initExpression");
}

bool Pattern::fillExpression()
{
	// Fill the energy expression for the pattern.
	// The structure that we create will include a static array of pointers
	// to the original atomic elements, to ease the generation of the expression.
	msg.enter("Pattern::fillExpression");
	Atom *ai, *aj, *ak, *al;
	Refitem<Bond,int> *bref;
	ForcefieldBound *ffb;
	ForcefieldParams params;
	PatternAtom *pa;
	PatternBound *pb;
	Forcefield *xff;
	// Counters for incomplete aspects of the expression
	int iatoms = 0, ibonds = 0, iangles = 0, itorsions = 0;
	incomplete_ = FALSE;
	// Temp vars for type storage
	ForcefieldAtom *ti, *tj, *tk, *tl;
	// Lists of unique bound atoms (used by angle and torsion generation routines)
	//int bonding[nAtoms_][7];
	//int **bonding;
	int count, ii, jj, kk, ll;
	// Create **bonding array
	//bonding = new int*[nAtoms_];
	//for (ii=0; ii< nAtoms_; ii++) bonding[ii] = new int[7];
	List< ListItem<int> > *bonding;
	bonding = new List< ListItem<int> >[nAtoms_];
	// If there is no specified pattern forcefield, use the parent model's instead
	forcefield_ == NULL ? xff = parent_->forcefield() : xff = forcefield_;
	msg.print("Fleshing out expression for %i atoms in pattern '%s'...\n", totalAtoms_, name_.get());
	msg.print("... Using forcefield '%s'...\n",xff->name());
	// Construct the atom list.
	// If any atom has not been assigned a type, we *still* include it in the list
	ai = firstAtom_;
	count = 0;
	for (pa = atoms_.first(); pa != NULL; pa = pa->next)
	{
		count ++;
		pa->setAtom(ai);
		pa->setData(ai->type());
		if (ai->type() == 0)
		{
			msg.print("... No FF definition for atom %i (%s).\n", count+1, elements.symbol(ai));
			incomplete_ = TRUE;
			iatoms ++;
		}
		// If the forcefield is rule-based, generate the required parameters first
		if (xff->rules() != Rules::None) xff->generateVdw(ai);
		// Point to the data
		//pa->data = pa->type->get_params()->data();
		ai = ai->next;
	}
	// Generate intramolecular terms (if not disabled)
	if (!noIntramolecular_)
	{
		// Construct the bond list.
		// Use the atomic bond lists and convert them, filling in the forcefield data as we go.
		// Add only bonds where id(i) > id(j) to prevent double counting of bonds
		// Also, create the lists of bound atoms here for use by the angle and torsion functions.
		// Again, only add bonds involving atoms in the first molecule of the pattern.
		//for (count=0; count<nAtoms_; count++) bonding[count][0] = 0;
		ai = firstAtom_;
		count = 0;
		for (ii=0; ii<nAtoms_; ii++)
		{
			// Go through the list of bonds to this atom
			bref = ai->bonds();
			while (bref != NULL)
			{
				// Get relative IDs and check if i > j
				aj = bref->item->partner(ai);
				ti = ai->type();
				tj = aj->type();
				jj = aj->id() - startAtom_;
				// Quick check to ensure the bond is within the same molecule...
				if (jj > endAtom_)
				{
					msg.print("!!! Found bond between molecules. Check pattern.\n");
					msg.exit("Pattern::fillExpression");
					return FALSE;
				}
				if (jj > ii)
				{
					bonds_[count]->setAtomId(0,ii);
					bonds_[count]->setAtomId(1,jj);
					// Search for the bond data. If its a rule-based FF and we don't find any matching data,
					// generate it. If its a normal forcefield, flag the incomplete marker.
					ffb = xff->findBond(ti,tj);
					// If we found a match, point to it
					if (ffb != NULL) bonds_[count]->setData(ffb);
					else
					{
						// If not a rule-based FF, nullify pointer
						if (xff->rules() == Rules::None) bonds_[count]->setData(NULL);
						else
						{
							// Generate the new parameters required
							ffb = xff->generateBond(ai,aj);
							bonds_[count]->setData(ffb);
						}
					}
					// Check ffb - if it's still NULL we couldn't find a definition
					if (ffb == NULL)
					{
						msg.print("!!! No FF definition for bond %s-%s.\n", ti->equivalent(), tj->equivalent());
						incomplete_ = TRUE;
						ibonds ++;
					}
					else
					{
						params = bonds_[count]->data()->params();
						msg.print(Messenger::Verbose,"Bond %s-%s data : %f %f %f %f\n",ti->equivalent(), tj->equivalent(), params.data[0], params.data[1], params.data[2], params.data[3]);
					}
					// Update the bonding array counters
					bonding[ii].add()->data = jj;
					bonding[jj].add()->data = ii;
					//bonding[ii][0] ++;
					//bonding[jj][0] ++;
					// Add the bond partner to each of the atom's own lists
					//bonding[ii][bonding[ii][0]] = jj;
					//bonding[jj][bonding[jj][0]] = ii;
					count ++;
				}
				bref = bref->next;
			}
			ai = ai->next;
		}
		if (bonds_.nItems() != count)
		{
			msg.print("...INTERNAL ERROR: expected %i bonds, found %i\n", bonds_.nItems(), count);
			incomplete_ = TRUE;
		}
		else if (ibonds == 0) msg.print("... Found parameters for %i bonds.\n", bonds_.nItems());
		else msg.print("... Missing parameters for %i of %i bonds.\n", ibonds, bonds_.nItems());
		// Construct the angle list.
		// Use the list of bound atoms in the bonding[][] array generated above
		count = 0;
		// Loop over central atoms 'jj'
		for (jj=0; jj<nAtoms_; jj++)
		{
			for (ii=0; ii<bonding[jj].nItems(); ii++)
			{
				for (kk=ii+1; kk<bonding[jj].nItems(); kk++)
				{
					ai = atoms_[bonding[jj][ii]->data]->atom();
					aj = atoms_[jj]->atom();
					ak = atoms_[bonding[jj][kk]->data]->atom();
					ti = ai->type();
					tj = aj->type();
					tk = ak->type();
					angles_[count]->setAtomId(0,bonding[jj][ii]->data);
					angles_[count]->setAtomId(1,jj);
					angles_[count]->setAtomId(2,bonding[jj][kk]->data);
					// Search for the bond data. If its a rule-based FF and we don't find any matching data,
					// generate it. If its a normal forcefield, flag the incomplete marker.
					ffb = xff->findAngle(ti,tj,tk);
					if (ffb != NULL) angles_[count]->setData(ffb);
					else
					{
						// If not a rule-based FF, nullify pointer
						if (xff->rules() == Rules::None) angles_[count]->setData(NULL);
						else
						{
							// Generate the new parameters required
							ffb = xff->generateAngle(ai,aj,ak);
							angles_[count]->setData(ffb);
						}
					}
					// Check ffa and raise warning if NULL
					if (ffb == NULL)
					{
						msg.print("!!! No FF definition for angle %s-%s-%s.\n", ti->equivalent(), tj->equivalent(), tk->equivalent());
						incomplete_ = TRUE;
						iangles ++;
					}
					else
					{
						params = angles_[count]->data()->params();
						msg.print(Messenger::Verbose,"Angle %s-%s-%s data : %f %f %f %f\n", ti->equivalent(), tj->equivalent(), tk->equivalent(), params.data[0], params.data[1], params.data[2], params.data[3]);
					}
					count ++;
				}
			}
		}
		if (angles_.nItems() != count)
		{
			msg.print("...INTERNAL ERROR: expected %i angles, found %i\n", angles_.nItems(), count);
			incomplete_ = TRUE;
		}
		else if (iangles == 0) msg.print("... Found parameters for %i angles.\n", angles_.nItems());
		else msg.print("... Missing parameters for %i of %i angles.\n", iangles, angles_.nItems());
		// Construct the torsion list.
		// Loop over the bond list and add permutations of the bonding atoms listed for either atom j and k
		count = 0;
		// Loop over the bonds in the molecule as the basis, then we can never count the same torsion twice.
		for (pb = bonds_.first(); pb != NULL; pb = pb->next)
		{
			jj = pb->atomId(0);
			kk = pb->atomId(1);
			// Loop over list of atoms bound to jj
			for (ii=0; ii<bonding[jj].nItems(); ii++)
			{
				// Skip atom kk
				if (bonding[jj][ii]->data == kk) continue;
				// Loop over list of atoms bound to kk
				for (ll=0; ll<bonding[kk].nItems(); ll++)
				{
					// Skip atom jj
					if (bonding[kk][ll]->data == jj) continue;
	
					ai = atoms_[bonding[jj][ii]->data]->atom();
					aj = atoms_[jj]->atom();
					ak = atoms_[kk]->atom();
					al = atoms_[bonding[kk][ll]->data]->atom();
					ti = ai->type();
					tj = aj->type();
					tk = ak->type();
					tl = al->type();
					torsions_[count]->setAtomId(0,bonding[jj][ii]->data);
					torsions_[count]->setAtomId(1,jj);
					torsions_[count]->setAtomId(2,kk);
					torsions_[count]->setAtomId(3,bonding[kk][ll]->data);
	
					// Search for the bond data. If its a rule-based FF and we don't find any matching data,
					// generate it. If its a normal forcefield, flag the incomplete marker.
					ffb = xff->findTorsion(ti,tj,tk,tl);
					if (ffb != NULL) torsions_[count]->setData(ffb);
					else
					{
						// If not a rule-based FF, nullify pointer
						if (xff->rules() == Rules::None) torsions_[count]->setData(NULL);
						else
						{
							// Generate the new parameters required
							ffb = xff->generateTorsion(ai,aj,ak,al);
							torsions_[count]->setData(ffb);
						}
					}
					// Check fft and raise warning if NULL
					if (ffb == NULL)
					{
						msg.print("!!! No FF definition for torsion %s-%s-%s-%s.\n", ti->equivalent(), tj->equivalent(), tk->equivalent(), tl->equivalent());
						incomplete_ = TRUE;
						itorsions ++;
					}
					else
					{
						params = torsions_[count]->data()->params();
						msg.print(Messenger::Verbose,"Torsion %s-%s-%s-%s data : %f %f %f %f\n", ti->equivalent(), tj->equivalent(), tk->equivalent(), tl->equivalent(), params.data[0], params.data[1], params.data[2], params.data[3]);
					}
					count ++;
				}
			}
		}
		if (torsions_.nItems() != count)
		{
			msg.print("...INTERNAL ERROR: expected %i torsions, found %i\n", torsions_.nItems(), count);
			incomplete_ = TRUE;
		}
		else if (itorsions == 0) msg.print("... Found parameters for %i torsions.\n", torsions_.nItems());
		else msg.print("... Missing parameters for %i of %i torsions.\n", itorsions, torsions_.nItems());
	}
	// Print out a warning if the expression is incomplete.
	if (incomplete_) msg.print("!!! Expression is incomplete.\n");
	msg.exit("Pattern::fillExpression");
	return (incomplete_ ? FALSE : TRUE);
}
