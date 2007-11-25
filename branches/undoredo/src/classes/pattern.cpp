/*
	*** Molecule pattern
	*** src/classes/pattern.cpp
	Copyright T. Youngs 2007

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
#include "templates/vector3.h"
#include "model/model.h"
#include "base/elements.h"
#include "base/debug.h"

// Constructors
pattern::pattern()
{
	parent = NULL;
	id = 0;
	prev = NULL;
	next = NULL;
	nmols = 0;
	expectedmols = 0;
	natoms = 0;
	totalatoms = 0;
	startatom = 0;
	endatom = 0;
	firstatom = NULL;
	lastatom = NULL;
	fixed = FALSE;
	ff = NULL;
	conmat = NULL;
	incomplete = FALSE;
	test_atomlimit = FALSE;
	test_el = FALSE;
	test_bonding = FALSE;
	#ifdef MEMDEBUG
		memdbg.create[MD_PATTERN] ++;
	#endif
}

patatom::patatom()
{
	prev = NULL;
	next = NULL;
	data = NULL;
	i = NULL;
	#ifdef MEMDEBUG
		memdbg.create[MD_PATTERN_ATOM] ++;
	#endif
}

patbound::patbound()
{
	for (int i=0; i<MAXFFBOUNDTYPES; i++) id[i] = -1;
	data = NULL;
	prev = NULL;
	next = NULL;
	#ifdef MEMDEBUG
		memdbg.create[MD_PATTERN_BOUND] ++;
	#endif
}

// Destructors
pattern::~pattern()
{
	delete_expression();
	#ifdef MEMDEBUG
		memdbg.destroy[MD_PATTERN] ++;
	#endif
}

patatom::~patatom()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_PATTERN_ATOM] ++;
	#endif
}

patbound::~patbound()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_PATTERN_BOND] ++;
	#endif
}

// Initialise
void pattern::initialise(int patid, int start, int mols, int atomsmol)
{
	// Initialise atom pointers / values in pattern.
	dbg_begin(DM_CALLS,"pattern::initialise");
	if (parent == NULL)
	{
		printf("Owner model has not been set in pattern!\n");
		dbg_end(DM_CALLS,"pattern::initialise");
		return;
	}
	// Store parameters
	id = patid;
	nmols = mols;			// Number of molecules in pattern
	natoms = atomsmol;		// Number of atoms per molecule
	totalatoms = mols*natoms;	// Total number of atoms described by the pattern
	startatom = start;		// Starting atom (integer position in atom list)
	endatom = start + natoms - 1;	// Last atom in first molecule (integer position in atom list)
	// Set atom pointers
	if (startatom > parent->get_natoms())
	{
		// Can't get first atom (probably the pattern extends past natoms)
		msg(DM_NONE,"Initial atom in pattern is past end of model's atom list (%i).\n",endatom);
		firstatom = NULL;
	}
	else
	{
		// Get the first atom in the list
		atom *i = parent->get_atoms();
		// Skip past atoms until we get to startatom (ranges from 0 upwards)
		for (int n=0; n<startatom; n++) i = (atom*) i->next;
		firstatom = i;
	}
	msg(DM_NONE,"New pattern node : start=%i, mols=%i, atoms/mol=%i, totalatoms=%i, name=%s\n", startatom, nmols, natoms, totalatoms, name.get());
	dbg_end(DM_CALLS,"pattern::initialise");
}

// Empty the selected pattern
void pattern::empty()
{
	// Set all variables to reflect an empty pattern
	dbg_begin(DM_CALLS,"pattern::empty_pattern");
	// Zero everything except natoms
	firstatom = NULL;
	lastatom = NULL;
	nmols = 0;
	totalatoms = 0;
	dbg_end(DM_CALLS,"pattern::empty_pattern");
}

// Set contents of pattern
void pattern::set_contents(int newstartatom, int newnmols, int newnatoms)
{
	if (newstartatom != -1) startatom = newstartatom;
	if (newnmols != -1) nmols = newnmols;
	if (newnatoms != -1) natoms = newnatoms;
	totalatoms = nmols * natoms;
	endatom = startatom + natoms - 1;
}

/*
// Expression
*/

// Remove energy expression
void pattern::delete_expression()
{
	// Clear the energy expression for the pattern node
	dbg_begin(DM_CALLS,"pattern::delete_expression");
	atoms.clear();
	bonds.clear();
	angles.clear();
	torsions.clear();
	if (conmat != NULL)
	{
		for (int n=0; n<natoms; n++) delete[] conmat[n];
		delete[] conmat;
	}
	conmat = NULL;
	dbg_end(DM_CALLS,"pattern::delete_expression");
}

// Create connectivity matrix for molecules in pattern
void pattern::create_conmat()
{
	// Create (calculate) the connectivity matrix for this node
	dbg_begin(DM_CALLS,"pattern::create_conmat");
	int n,m,a1,a2,b1,b2;
	patbound *pb;
	for (n=0; n<natoms; n++)
		for (m=0; m<natoms; m++) conmat[n][m] = 0;

	// First, build up the bond matrix
	for (pb = bonds.first(); pb != NULL; pb = pb->next)
	{
		conmat[ pb->get_atomid(0) ] [ pb->get_atomid(1) ] = 1;
		conmat[ pb->get_atomid(1) ] [ pb->get_atomid(0) ] = 1;
	}

	// Print out the bonding matrix
/*	printf("Bonding Matrix\n");
	for (n=0; n<natoms; n++)
	{
		for (m=0; m<natoms; m++) printf (" %2i ",conmat[n][m]);
		printf("\n");
	} */

	// Now, transform into the connectivity matrix.
	for (a1=0; a1<natoms; a1++)
	{
		for (a2=0; a2<natoms; a2++)
		{
			if (conmat[a1][a2] != 0)
			{
				// A2 *is* bound directly to A1.
				// So, we may increase the distance of A2 from all *other* atoms that A1 is bound to by one hop.
				for (m=0; m<natoms; m++)
				{
					// We only potentially increase the distance if :
					//	1) The atom 'm' is *not equal* to a2 (i.e. is not itself)
					//	2) The atom 'm' we're looking at is also bound to a1.
					if ((m != a2) && (conmat[a1][m] != 0))
					if ((conmat[a1][m] != 0))
					{
						// We only actually increase the distance if :
						// 	1) Atom 'm' is not directly bound to a2 **OR**
						//	2) The combined distances of m->a1 and a1->a2 is less than m->a2
						// The last check means that only the minimum distance m->a2 persists at the end
						if ((conmat[m][a2] == 0) || (conmat[a1][m]+conmat[a1][a2] < conmat[m][a2]))
							conmat[m][a2] = conmat[a1][m] + conmat[a1][a2];
					}
				}
			}
		}
	}

/*	printf("Connectivity Matrix\n");
	for (n=0; n<natoms; n++)
	{
		for (m=0; m<natoms; m++) printf ("%2i",conmat[n][m]);
		printf("\n"); 
	} */
	dbg_end(DM_CALLS,"pattern::create_conmat");
}

// Validate pattern
bool pattern::validate()
{
	// Test the pattern for validity and internal consistency
	dbg_begin(DM_CALLS,"pattern::validate");
	bool result, ok;
	result = TRUE;
	int mnatoms = parent->get_natoms();
	int elcomp1[NELEMENTS+1], elcomp2[NELEMENTS+1], a, m;
	// Set all test flags to FALSE
	test_atomlimit = FALSE;
	test_el = FALSE;
	test_bonding = FALSE;
	// 1) Check number of atoms does not exceed number in model
	if (startatom+totalatoms > mnatoms)
	{
		msg(DM_NONE,"Pattern's last atom is beyond the number of atoms in the model.\n");
		msg(DM_NONE,"No pattern defined for model.\n");
		// Can't do much else if this is the case, so break early.
		dbg_end(DM_CALLS,"pattern::validate");
		return FALSE;
	}
	else test_atomlimit = TRUE;
	// 2) Elemental composition of individual molecules within pattern
	for (m=0; m<NELEMENTS+1; m++) elcomp1[m] = 0;
	if (nmols == 1) test_el = TRUE;
	else
	{
		atom *i = firstatom;
		for (m=0; m<nmols; m++)
		{
			ok = TRUE;
			if (m == 0)
			{
				// Calculate the reference atomic composition from molecule 1
				for (a=0; a<natoms; a++)
				{
					elcomp1[i->get_element()] ++;
					i = i->next;
				}
			}
			else
			{
				// Calculate the test atomic composition...
				for (a=0; a<NELEMENTS+1; a++) elcomp2[a] = 0;
				for (a=0; a<natoms; a++)
				{
					elcomp2[i->get_element()] ++; i = i->next;
				}
				// ... and test against reference
				for (a=0; a<NELEMENTS+1; a++)
					if (elcomp1[a] != elcomp2[a]) ok = FALSE;
			}
			if (!ok)
			{
				msg(DM_NONE,"pattern::validate : Failed element composition test at molecule %i.\n",m+1);
				result = FALSE;
				break;
			}
		}
	}
	// 3) Bonding within molecules in pattern
	//TODO
	if (!result) msg(DM_NONE,"No pattern defined for model.\n");
	dbg_end(DM_CALLS,"pattern::validate");
	return result;
}

// Calculate centre of geometry for molecule 'mol' in pattern, from config supplied
vec3<double> pattern::calculate_cog(model *srcmodel, int mol)
{
	// Calculate the centre of geometry for this molecule
	dbg_begin(DM_CALLS,"pattern::calculate_cog");
	int offset = startatom + mol*natoms;
	msg(DM_VERBOSE,"pattern::calculate_cog : Calculating for pattern '%s', molecule %i (starting at %i, nmols=%i)\n", name.get(), mol, offset, nmols);
	static vec3<double> cog, mim_i;
	unitcell *cell = &srcmodel->cell;
	cog.zero();
	atom **modelatoms = srcmodel->get_staticatoms();
	for (int a1=offset; a1<offset+natoms; a1++)
	{
		// Do minimum image w.r.t. first atom in molecule
		mim_i = cell->mim(modelatoms[a1]->r, modelatoms[offset]->r);
		cog += mim_i;
	}
	cog /= natoms;
	dbg_end(DM_CALLS,"pattern::calculate_cog");
	return cog;
}

// Calculate centre of mass for molecule 'mol' in pattern, from config supplied
vec3<double> pattern::calculate_com(model *srcmodel, int mol)
{
	// Calculate the centre of geometry for this molecule
	dbg_begin(DM_CALLS,"pattern::calculate_com");
	msg(DM_VERBOSE,"calculate_com : Calculating for pattern '%s', molecule %i (pattern:nmols=%i)\n", name.get(), mol, nmols);
	vec3<double> com;
	double massnorm = 0.0;
	static vec3<double> mim_i;
	int offset = startatom + mol*natoms;
	com.zero();
	msg(DM_VERBOSE,"molecule_com : Offset = %i\n",offset);
	unitcell *cell = &srcmodel->cell;
	atom **modelatoms = srcmodel->get_staticatoms();
	for (int a1=offset; a1<offset+natoms; a1++)
	{
		// Do minimum image w.r.t. first atom in molecule
		mim_i = cell->mim(modelatoms[a1]->r, modelatoms[offset]->r);
		com += mim_i * elements.mass(modelatoms[a1]->get_element());
		massnorm += elements.mass(modelatoms[a1]->get_element());
	}
	com /= massnorm;
	dbg_end(DM_CALLS,"pattern::calculate_com");
	return com;
}

/*
// Data Propagation Routines
*/

void pattern::propagate_atomtypes()
{
	// Copy type information contained in the first molecule in the pattern to all other molecules in the pattern, and the pattern's representative molecule
	dbg_begin(DM_CALLS,"pattern::propagate_atomtypes");
	atom *i, *j;
	int n, m;
	// Set 'j' to be the starting atom of the second molecule
	// Set representative molecule data at the same time
	j = firstatom;
	i = molecule.get_atoms();
	for (n=0; n<natoms; n++)
	{
		i->set_env(j->get_env());
		i->set_fftype(j->get_fftype());
		i = i->next;
		j = j->next;
	}
	// Loop over other molecules and copy the data
	for (n=1; n<nmols; n++)
	{
		// Set pointer 'i' to be the starting atom of the first molecule
		i = firstatom;
		for (m=0; m<natoms; m++)
		{
			j->set_env(i->get_env());
			j->set_fftype(i->get_fftype());
			i = i->next;
			j = j->next;
		}
	}
	dbg_end(DM_CALLS,"pattern::propagate_atomtypes");
}

void pattern::propagate_bondtypes()
{
	// Copy the bond type data in the first molecule of the pattern to all other molecules in the pattern.
	// With a loop over all other molecules, loop over the atoms of the first molecule. For each bond on this atom,
	// find the relative atom id and search for the corresponding atom in the n'th molecule.
	dbg_begin(DM_CALLS,"pattern::propagate_bondtypes");
	int n,m,o,offset;
	atom *i, *j, *k;
	refitem<bond> *bref;
	bond *b1, *b2;
	// Set the pointer 'j' to be the first atom of the second molecule
	j = firstatom;
	for (n=0; n<natoms; n++) j = j->next;
	// Loop over remaining molecules
	for (n=1; n<nmols; n++)
	{
		// Set 'i' to be the first atom of the first molecule
		i = firstatom;
		// Loop over atoms in molecule
		for (m=0; m<natoms; m++)
		{
			bref = i->get_bonds();
			while (bref != NULL)
			{
				// Get id offset between 'i' and the atom at the other end of the bond 'b'
				b1 = bref->item;
				offset = b1->get_partner(i)->get_id() - i->get_id();
				// Now get atom in current molecule with same id offset
				k = j;
				if (offset > 0) for (o=0; o<offset; o++) k = k->next;
				else for (o=0; o<abs(offset); o++) k = k->prev;
				// 'k' now points to the bond partner of 'j'
				b2 = j->find_bond(k);
				if (b2 == NULL) msg(DM_NONE,"Bizarre fatal error. Couldn't find bond.\n");
				else b2->type = b1->type;
				bref = bref->next;
			}
			i = i->next;
			j = j->next;
		}
	}
	dbg_end(DM_CALLS,"pattern::propagate_bondtypes");
}

/*
// Atomic Routines
*/

// Append atom
atom *pattern::append_copy(atom *source)
{
	// Append the supplied atom to the pattern's 'local' atom list
	dbg_begin(DM_CALLS,"pattern::append_atom[pattern]");
	atom *newatom = new atom;
	firstatom == NULL ? firstatom = newatom : lastatom->next = newatom;
	newatom->prev = lastatom;
	lastatom = newatom;
	newatom->copy(source);
	totalatoms ++;
	dbg_end(DM_CALLS,"pattern::append_atom[pattern]");
	return newatom;
}

// Delete atom
void pattern::delete_atom(atom *xatom)
{
	// Delete the supplied atom from the pattern's 'local' atom list
	dbg_begin(DM_CALLS,"pattern::delete_atom");
	xatom->prev == NULL ? firstatom = (atom*) xatom->next : xatom->prev->next = (atom*) xatom->next;
	xatom->next == NULL ? lastatom = (atom*) xatom->prev : xatom->next->prev = (atom*) xatom->prev;
	delete xatom;
	totalatoms --;
	dbg_end(DM_CALLS,"pattern::delete_atom");
}

// Delete atoms from end
void pattern::delete_atoms_from_end(int count)
{
	// Deletes a number 'n' of atoms from the end of the list (i.e. recently created ones)
	dbg_begin(DM_CALLS,"pattern::delete_atoms_from_end");
	for (int n=0; n<count; n++) delete_atom(lastatom);
	dbg_end(DM_CALLS,"pattern::delete_atoms_from_end");
}

// Clear 'tempi' values of all atoms in pattern
void pattern::reset_tempi(int value)
{
	// Sets the tempi variables of all atoms in the pattern to the value specified
	atom *i = firstatom;
	for (int m=0; m<natoms; m++)
	{
		i->tempi = value;
		i = i->next;
	}
}

/*
// Ring Routines
*/

// Mark atoms
void pattern::ring_markatoms(atom *i)
{
	// Recursive routine to knock-out atoms that have a lower ring forming potential than
	// their bond number suggests. If called with an atom whose potential is zero, we stop
	// since we've reached an already 'excluded' atom. Otherwise, check the bound neighbours
	// and reassess the potential of the atom. If its marked as zero potential, re-call the
	// routine on any neighbours that aren't already zero-marked.
	dbg_begin(DM_CALLS,"pattern::ring_markatoms");
	if (i->tempi == 0)
	{
		dbg_end(DM_CALLS,"pattern::ring_markatoms");
		return;
	}
	else
	{
		// Check the listed neighbours to set the potential of this atom
		int count = 0;
		refitem<bond> *bref = i->get_bonds();
		while (bref != NULL)
		{
			if (bref->item->get_partner(i)->tempi != 0) count++;
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
				bref = i->get_bonds();
				while (bref != NULL)
				{
					if (bref->item->get_partner(i)->tempi != 0) ring_markatoms(bref->item->get_partner(i));
					bref = bref->next;
				}
				break;
			case (2): i->tempi = 1; break;
			case (3): i->tempi = 3; break;
			default : i->tempi = 6; break;
		}
	}
	dbg_end(DM_CALLS,"pattern::ring_markatoms");
}

// Find rings
void pattern::find_rings()
{
	// Locate rings in the molecule of the current pattern.
	// Maintain a local array corresponding to whether specific atoms are 'done' or not - i.e., whether
	// they have been check as as much as they need to be checked.
	dbg_begin(DM_CALLS,"pattern::find_rings");
	int n, rsize, ringpotential;
	atom *i;
	refitem<bond> *bref;
	ring *r, path;
	// Set the initial states of the atoms. i->tempi maintains the maximum possible number of rings that
	// each atom can form based on the number of bonds it has.
	i = firstatom;
	for (n=0; n<natoms; n++)
	{
		switch (i->get_nbonds())
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
	i = firstatom;
	for (n=0; n<natoms; n++)
	{
		if (i->get_nbonds() == 1)
		{
			bref = i->get_bonds();
			ring_markatoms(bref->item->get_partner(i));
		}
		i = i->next;
	}

	// Calculate the *total* potential for ring formation, i.e. the sum of all tempi values.
	ringpotential = 0;
	i = firstatom;
	for (n=0; n<natoms; n++)
	{
		msg(DM_VERBOSE,"Atom %i : potential = %i\n",n,i->tempi);
		ringpotential += i->tempi;
		i = i->next;
	}

	// Now, go through the atoms and find cyclic routes.
	i = firstatom;
	for (n=0; n<natoms; n++)
	{
		if (i->tempi == 0) { i = i->next; continue; }
		// Loop over searches for different ring sizes
		for (rsize=3; rsize<=prefs.get_maxringsize(); rsize++)
		{
			path.atoms.clear();
			path.requested_size = rsize;
			// Call the recursive search to extend the path by this atom
			if (ringpotential >= rsize) ring_search(i,&path,ringpotential);
		}
		i = i->next;
	}
	dbg_end(DM_CALLS,"pattern::find_rings");
}

// Ring search
void pattern::ring_search(atom *i, ring *currentpath, int &ringpotential)
{
	// Extend the path (ring) passed by the atom 'i', searching for a path length of 'ringsize'
	dbg_begin(DM_CALLS,"pattern::ring_search");
	refitem<bond> *bref;
	ring *r;
	refitem<atom> *lastra;
	bool done;
	// First off, if this atom has no more available bonds for inclusion in rings, just return
	if (i->tempi == 0)
	{
		//printf(" --- No vacant 'bonds' on atom - skipping...\n");
		dbg_end(DM_CALLS,"pattern::ring_search");
		return;
	}
	// Otherwise, add it to the current path
	lastra = currentpath->atoms.last();
	if (currentpath->add_atom(i))
	{
		msg(DM_VERBOSE," --- Atom i added to path :  ");
		currentpath->print();
		// Adding the atom did not exceed the requested ring size, and is not a duplicate
		// Go through the list of atoms bound to 'i' and then:
		//  -- If natoms<=requestedsize && 'i' is bound to the first atom in the path, store the ring.
		//  Otherwise, if natoms<requestedsize then extend the ring by each of the bound atoms in turn.
		done = FALSE;
		bref = i->get_bonds();
		while (bref != NULL)
		{
			// Don't consider the atom we just came from...
			if (lastra != NULL)
				if (bref->item->get_partner(i) == lastra->item) { bref = bref->next; continue; }
			// Depending on the size check, extend or store current path.
			if (currentpath->atoms.size() == currentpath->requested_size)
			{
				// The correct number of atoms is in the current path. Does it form a cycle?
				if (i->find_bond(currentpath->atoms.first()->item) != NULL)
				{
					msg(DM_VERBOSE," --- Storing current ring.\n");
					r = rings.add();
					r->copy(currentpath);
					// Must now update atom 'tempi' values to reflect the inclusion of these atoms in
					// another ring, and also the total ringpotential variable
					refitem<atom> *ra = r->atoms.first();
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
				ring_search(bref->item->get_partner(i),currentpath,ringpotential);
			}
			bref = bref->next;
			if (done) break;
		}
		// Return the list to its original state
		msg(DM_VERBOSE," --- Removing atom %s[%li] from current path...\n",elements.symbol(i),i);
		currentpath->atoms.remove(currentpath->atoms.last());
	}
	else
	{
		//printf(" --- Atom is already in list, or adding it exceeds specified ringsize.\n");
	}
	dbg_end(DM_CALLS,"pattern::ring_search");
}
