/*
	*** Molecule pattern
	*** src/classes/pattern.h
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

#ifndef H_PATTERN_H
#define H_PATTERN_H

#include "model/model.h"
#include "classes/atom.h"
#include "templates/vector3.h"
#include "templates/list.h"
#include "classes/ring.h"
#include "energy/forms.h"
#include "classes/forcefield.h"

// Forward declarations
class energystore;
class atomtype;
class region;

// Structures to hold/point to forcefield descriptions in patterns.
class patatom
{
	public:
	// Constructor / Destructor
	patatom();
	~patatom();
	// List pointers
	patatom *prev, *next;

	/*
	// FF and atom data
	*/
	private:
	// Original FF type of atom
	ffatom *data;
	// Pointer to atom in parent xmodel
	atom *i;

	public:
	// Set ff type of pattern atom
	void set_data(ffatom *ffa) { data = ffa; }
	// Get ff type of pattern atom
	ffatom *get_data() { return data; }
	// Set pointer to atom in patterns representative molecule
	void set_atom(atom *a) { i = a; }
	// Get pointer to atom in patterns representative molecule
	atom *get_atom() { return i; }
};

class patbound
{
	public:
	// Constructor / Destructor
	patbound();
	~patbound();
	// List pointers
	patbound *prev, *next;

	/*
	// FF term data
	*/
	private:
	// Atoms involved in bond (referring to local molecule atom ids)
	int id[MAXFFBOUNDTYPES];
	// Pointer to function data / form
	ffbound *data;

	public:
	// Set atom id
	void set_atomid(int n, int i) { (n < MAXFFBOUNDTYPES ? id[n] = i : printf("OUTOFRANGE:patbound")); }
	int get_atomid(int n) { return id[n]; }
	// Set function data
	void set_data(ffbound *ffb) { data = ffb; }
	// Get function data
	ffbound *get_data() { return data; }
};

// Pattern Node
class pattern
{
	public:
	// Constructor / Destructor
	pattern();
	~pattern();
	// List pointers
	pattern *prev, *next;

	/*
	// Definition
	*/
	private:
	// Parent model
	model *parent;
	// Internal ID of the pattern (order in the pnode* list)
	int id;
	// Internally numbered atom IDs which this node ends at
	int endatom;
	// Number of atoms in each 'molecule'
	int natoms;
	// Internally numbered atom IDs which this node starts at
	int startatom;
	// Number of 'molecules' this pattern encompasses
	int nmols;
	// Expected number of molecules (used by disordered builder)
	int expectedmols;
	// Total number of atoms in the pattern
	int totalatoms;
	// Pointer to the first atom in the pattern
	atom *firstatom;
	// Pointer to last atom in pattern (used by some methods)
	atom *lastatom;
	// Atom limit test, element composition test
	bool test_atomlimit, test_el;
	// Bonding test
	bool test_bonding;
	// Remove atom from local list
	void delete_atom(atom*);
	// Used in various methods
	bool fixed;
	// Specific forcefield to use (otherwise use model->ffs)
	forcefield *ff;
	// Short name of the pattern (initially set to "n*m")
	dnchar name;

	public:
	// Basic model containing a representative molecule of the pattern
	model molecule;
	// Sets up variables in pattern
	void initialise(int, int, int, int);
	// Takes the supplied atom and places a copy in the local list 
	atom *append_copy(atom *source);
	// Delete a number of atoms from the end of the list
	void delete_atoms_from_end(int);
	// Perform checks to determine the validity of the pattern
	bool validate();
	// Sets the ID of the pattern
	void set_id(int i) { id = i; }
	// Returns then numerical ID of the pattern
	int get_id() { return id; }
	// Returns head of the atom list for this pattern (located in main model list)
	atom *get_firstatom() { return firstatom; }
	// Sets pointer to the first atom in this pattern (located in main model list)
	void set_firstatom(atom* i) { firstatom = i; }
	// Returns last of the atom list for this pattern (located in main model list)
	atom *get_lastatom() { return lastatom; }
	// Sets pointer to the last atom in this pattern (located in main model list)
	void set_lastatom(atom* i) { lastatom = i; }
	// Calculate the global atom number offset of the first atom of the molecule
	int get_offset(int mol) { return startatom + mol*natoms; }
	// Returns the number of atoms in one molecule of the pattern
	int get_natoms() { return natoms; }
	// Sets the starting atom of the model
	void set_startatom(int n) { startatom = n; }
	// Returns the starting atom id of the pattern
	int get_startatom() { return startatom; }
	// Sets the end atom of the model
	void set_endatom(int n) { endatom = n; }
	// Returns the ending atom id of the pattern
	int get_endatom() { return endatom; }
	// (Re)Calculate totalatoms
	void calc_totalatoms() { totalatoms = natoms * nmols; }
	// Returns the total number of atoms in the pattern
	int get_totalatoms() { return totalatoms; }
	// Resets the 'tempi' variables of all atoms in the pattern to the given integer
	void reset_tempi(int);
	// Sets the number of molecules in the pattern
	void set_nmols(int n) { nmols = n; }
	// Returns the number of molecules in the pattern
	int get_nmols() { return nmols; }
	// Sets the expected number of molecules in the pattern
	void set_expectedmols(int n) { expectedmols = n; }
	// Returns the expected number of molecules in the pattern
	int get_expectedmols() { return expectedmols; }
	// Sets the parent model
	void set_parent(model *m) { parent = m; }
	// Returns the model for which the pattern was created
	model *get_parent() { return parent; }
	// Sets the 'fixed' property of the pattern
	void set_fixed(bool b) { fixed = b; }
	// Returns whether the pattern is fixed
	bool is_fixed() { return fixed; }
	// Sets the name of the pattern 
	void set_name(const char *s) { name = s; }
	// Returns the pattern name
	const char *get_name() { return name.get(); }
	// Sets the forcefield to use in the pattern
	void set_ff(forcefield *newff) { ff = newff; }
	// Gets the forcefield associated with the pattern
	forcefield *get_ff() { return ff; }
	// Returns whether the atomlimit in the pattern is valid
	bool is_atomlimit_ok() { return test_atomlimit; }
	// Returns whether the element composition in the pattern molecules is uniform
	bool are_elements_ok() { return test_el; }
	// Returns whether the bonding in the pattern molecules is uniform
	bool is_bonding_ok() { return test_bonding; }
	// Sets variables to reflect an empty pattern (no atoms are physically deleted)
	void empty();
	// Sets startatom, nmols, and natoms (and calculates totalatoms)
	void set_contents(int,int,int);
	// Postfix increment
	pattern *operator++() { return (this->next); }

	/*
	// Expression
	*/
	private:
	// Connectivity matrix of the atoms
	int **conmat;
	// Flag for incomplete energy node
	bool incomplete;

	public:
	// List of atoms in the model
	list<patatom> atoms;
	// List of bonds in the model
	list<patbound> bonds;
	// List of angles in the model
	list<patbound> angles;
	// List of torsions in the model
	list<patbound> torsions;
	// Empty the arrays of the energy expression
	void delete_expression();
	// Create the shell of the energy expression
	void init_expression(model*);
	// Fill the energy expression with parameters
	bool fill_expression(model*);
	// Create the connectivity matrix
	void create_conmat();

	/*
	// Energy / Force Calculation
	*/
	public:
	void bond_energy(model*,energystore*);
	void angle_energy(model*,energystore*);
	void torsion_energy(model*,energystore*);
	void vdw_intrapattern_energy(model*,energystore*);
	void vdw_interpattern_energy(model*,pattern*,energystore*);
	void vdw_correct_energy(unitcell*,energystore*);
	void coulomb_intrapattern_energy(model*,energystore*);
	void coulomb_interpattern_energy(model*,pattern*,energystore*);
	void ewald_real_intrapattern_energy(model*,energystore*);
	void ewald_real_interpattern_energy(model*,pattern*,energystore*);
	void ewald_reciprocal_energy(model*,pattern*,int,energystore*);
	void ewald_correct_energy(model*,energystore*);
	void bond_forces(model*);
	void angle_forces(model*);
	void torsion_forces(model*);
	void vdw_intrapattern_forces(model*);
	void vdw_interpattern_forces(model*,pattern*);
	void coulomb_intrapattern_forces(model*);
	void coulomb_interpattern_forces(model*,pattern*);
	void ewald_real_intrapattern_forces(model*);
	void ewald_real_interpattern_forces(model*,pattern*);
	void ewald_reciprocal_forces(model*);
	void ewald_correct_forces(model*);

	/*
	// Typing
	*/
	private:
	// List of rings in one molecule of the pattern
	list<ring> rings;
	// Recursive prep - locates and marks atoms on their 'ring potential'
	void ring_markatoms(atom*);
	// Recursive ring-search routine
	void ring_search(atom*, ring*, int&);

	public:
	// Returns a pointer to the ring list structure
	list<ring>* get_ringlist() { return &rings; }
	// Returns the first ring in the ring list
	ring *get_rings() { return rings.first(); }
	// Automatically augment bond types in the pattern
	void augment_bonding();
	// Reset the atom environment flags
	void clear_hybrids();
	// Set atom hybridisations
	void assign_hybrids();
	// Assign forcefield atom types
	bool type_atoms(forcefield*);
	// Locate ring structures in the pattern
	void find_rings();
	// Augment atoms in pattern
	void augment();

	/*
	// Data Propagation
	*/
	public:
	void propagate_atomtypes();
	void propagate_bondtypes();

	/*
	// Properties
	*/
	public:
	// Calculate centre of geometry of molecule in specified config
	vec3<double> calculate_cog(model*, int);
	// Calculate centre of mass of molecule in specified config
	vec3<double> calculate_com(model*, int);
};

#endif
