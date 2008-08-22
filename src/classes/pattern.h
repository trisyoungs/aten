/*
	*** Molecule pattern
	*** src/classes/pattern.h
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

#ifndef ATEN_PATTERN_H
#define ATEN_PATTERN_H

#include "templates/vector3.h"
#include "templates/list.h"
#include "classes/forcefield.h"

// Forward declarations
class Energy;
class Atom;
class Atomtype;
class ComponentRegion;
class Model;
class Ring;

// Structures to hold/point to forcefield descriptions in patterns.
class PatternAtom
{
	public:
	// Constructor
	PatternAtom();
	// List pointers
	PatternAtom *prev, *next;

	/*
	// FF and atom data
	*/
	private:
	// Original FF type of atom
	ForcefieldAtom *data_;
	// Pointer to atom in parent xmodel
	Atom *atom_;

	public:
	// Set ff type of pattern atom
	void setData(ForcefieldAtom *ffa);
	// Get ff type of pattern atom
	ForcefieldAtom *data();
	// Set pointer to atom in first molecule
	void setAtom(Atom *a);
	// Get pointer to atom in first molecule
	Atom *atom();
};

class PatternBound
{
	public:
	// Constructor
	PatternBound();
	// List pointers
	PatternBound *prev, *next;

	/*
	// FF term data
	*/
	private:
	// Atoms involved in bond (referring to local molecule atom ids)
	int id_[MAXFFBOUNDTYPES];
	// Pointer to function data / form
	ForcefieldBound *data_;

	public:
	// Set atom id
	void setAtomId(int n, int i);
	// Return atom id
	int atomId(int n);
	// Set function data
	void setData(ForcefieldBound *ffb);
	// Get function data
	ForcefieldBound *data();
};

// Pattern Node
class Pattern
{
	public:
	// Constructor / Destructor
	Pattern();
	~Pattern();
	// List pointers
	Pattern *prev, *next;

	/*
	// Definition
	*/
	private:
	// Parent model
	Model *parent_;
	// Internal ID of the pattern (order in the pnode* list)
	int id_;
	// Internally numbered atom IDs which this node ends at
	int endAtom_;
	// Number of atoms in each 'molecule'
	int nAtoms_;
	// Internally numbered atom IDs which this node starts at
	int startAtom_;
	// Number of 'molecules' this pattern encompasses
	int nMolecules_;
	// Expected number of molecules (used by disordered builder)
	int nExpectedMols_;
	// Total number of atoms in the pattern
	int totalAtoms_;
	// Pointer to the first atom in the pattern
	Atom *firstAtom_;
	// Pointer to last atom in pattern (used by some methods)
	Atom *lastAtom_;
	// Atom limit test, element composition test
	bool testAtomLimit_, testElement_;
	// Bonding test
	bool testBonding_;
	// Used in various methods
	bool fixed_;
	// Specific forcefield to use (otherwise use model->ffs)
	Forcefield *forcefield_;
	// Short name of the pattern (initially set to "n*m")
	Dnchar name_;
	// Remove atom from local list
	void deleteAtom(Atom*);

	public:
	// Basic model containing a representative molecule of the pattern
	//Model *molecule;
	// Sets up variables in pattern
	void initialise(int, int, int, int);
	// Takes the supplied atom and places a copy in the local list 
	Atom *appendCopy(Atom *source);
	// Delete a number of atoms from the end of the list
	void deleteAtomsFromEnd(int);
	// Perform checks to determine the validity of the pattern
	bool validate();
	// Sets the ID of the pattern
	void setId(int i);
	// Returns then numerical ID of the pattern
	int id();
	// Returns head of the atom list for this pattern (located in main model list)
	Atom *firstAtom();
	// Sets pointer to the first atom in this pattern (located in main model list)
	void setFirstAtom(Atom* i);
	// Returns last of the atom list for this pattern (located in main model list)
	Atom *lastAtom();
	// Sets pointer to the last atom in this pattern (located in main model list)
	void setLastAtom(Atom* i);
	// Calculate the global atom number offset of the first atom of the molecule
	int offset(int mol);
	// Returns the number of atoms in one molecule of the pattern
	int nAtoms();
	// Sets the starting atom of the model
	void setStartAtom(int n);
	// Returns the starting atom id of the pattern
	int startAtom();
	// Sets the end atom of the model
	void setEndAtom(int n);
	// Returns the ending atom id of the pattern
	int endAtom();
	// (Re)Calculate totalatoms
	void calcTotalAtoms();
	// Returns the total number of atoms in the pattern
	int totalAtoms();
	// Resets the 'tempi' variables of all atoms in the pattern to the given integer
	void resetTempI(int);
	// Sets the number of molecules in the pattern
	void setNMolecules(int n);
	// Returns the number of molecules in the pattern
	int nMolecules();
	// Sets the expected number of molecules in the pattern
	void setNExpectedMolecules(int n);
	// Returns the expected number of molecules in the pattern
	int nExpectedMolecules();
	// Sets the parent model
	void setParent(Model *m);
	// Returns the model for which the pattern was created
	Model *parent();
	// Sets the 'fixed' property of the pattern
	void setFixed(bool b);
	// Returns whether the pattern is fixed
	bool isFixed();
	// Sets the name of the pattern 
	void setName(const char *s);
	// Returns the pattern name
	const char *name();
	// Sets the forcefield to use in the pattern
	void setForcefield(Forcefield *newff);
	// Gets the forcefield associated with the pattern
	Forcefield *forcefield();
	// Returns whether the atomlimit in the pattern is valid
	bool isAtomLimitOk();
	// Returns whether the element composition in the pattern molecules is uniform
	bool areElementsOk();
	// Returns whether the bonding in the pattern molecules is uniform
	bool isBondingOk();
	// Sets variables to reflect an empty pattern (no atoms are physically deleted)
	void empty();
	// Sets startatom, nmols, and natoms (and calculates totalatoms)
	void setContents(int,int,int);
	// Postfix increment
	Pattern *operator++();

	/*
	// Expression
	*/
	private:
	// Connectivity matrix of atoms in one molecule of the pattern
	int **conMatrix_;
	// Scaling matrix for VDW interactions between atoms in each molecule
	double **vdwScaleMatrix_;
	// Scaling matrix for electrostatic interactions between atoms in each molecule
	double **elecScaleMatrix_;
	// Flag for incomplete energy node
	bool incomplete_;
	// Flag for no intramolecular terms in expression
	bool noIntramolecular_;
	// List of atom(types) in one pattern molecule
	List<PatternAtom> atoms_;
	// List of bonds in one pattern molecule
	List<PatternBound> bonds_;
	// List of angles in one pattern molecule
	List<PatternBound> angles_;
	// List of torsions in one pattern molecule
	List<PatternBound> torsions_;

	public:
	// Empty the arrays of the energy expression
	void deleteExpression();
	// Create the shell of the energy expression
	void initExpression(bool vdwOnly = FALSE);
	// Fill the energy expression with parameters
	bool fillExpression();
	// Create the connectivity and scaling matrices
	void createMatrices();
	// Update scaling matrices
	void updateScaleMatrices();
	// Return number of bonds in one molecule of the pattern
	int nBonds();
	// Return number of angles in one molecule of the pattern
	int nAngles();
	// Return number of torsions in one molecule of the pattern
	int nTorsions();
	// Return first bonds of the pattern
	PatternBound *bonds();
	// Return first angle of the pattern
	PatternBound *angles();
	// Return first torsion of the pattern
	PatternBound *torsions();

	/*
	// Energy / Force Calculation
	*/
	public:
	void bondEnergy(Model*, Energy*, int molecule = -1);
	void angleEnergy(Model*, Energy*, int molecule = -1);
	void torsionEnergy(Model*, Energy*, int molecule = -1);
	void vdwIntraPatternEnergy(Model*, Energy*, int molecule = -1);
	void vdwInterPatternEnergy(Model*, Pattern*, Energy*, int molecule = -1);
	void vdwCorrectEnergy(Cell*, Energy*);
	void coulombIntraPatternEnergy(Model*, Energy*, int molecule = -1);
	void coulombInterPatternEnergy(Model*, Pattern*, Energy*, int molecule = -1);
	void ewaldRealIntraPatternEnergy(Model*, Energy*, int molecule = -1);
	void ewaldRealInterPatternEnergy(Model*, Pattern*, Energy*, int molecule = -1);
	void ewaldReciprocalEnergy(Model*, Pattern*, int, Energy*, int molecule = -1);
	void ewaldCorrectEnergy(Model*, Energy*, int molecule = -1);
	void bondForces(Model*);
	void angleForces(Model*);
	void torsionForces(Model*);
	void vdwIntraPatternForces(Model*);
	void vdwInterPatternForces(Model*, Pattern*);
	void coulombIntraPatternForces(Model*);
	void coulombInterPatternForces(Model*, Pattern*);
	void ewaldRealIntraPatternForces(Model*);
	void ewaldRealInterPatternForces(Model*, Pattern*);
	void ewaldReciprocalForces(Model*);
	void ewaldCorrectForces(Model*);

	/*
	// Typing
	*/
	private:
	// List of rings in one molecule of the pattern
	List<Ring> rings_;
	// Recursive prep - locates and marks atoms on their 'ring potential'
	void markRingAtoms(Atom*);
	// Recursive ring-search routine
	void ringSearch(Atom*, Ring*, int&);

	public:
	// Returns a pointer to the ring list structure
	List<Ring>* ringList();
	// Returns the first ring in the ring list
	Ring *rings();
	// Reset the atom environment flags
	void clearHybrids();
	// Set atom hybridisations
	void assignHybrids();
	// Assign forcefield atom types
	bool typeAtoms();
	// Locate ring structures in the pattern
	void findRings();
	// Augment atoms in pattern
	void augment();
	// Augment atoms in pattern
	void augmentOLD();
	// Return total bond order penalty of atoms in one molecule of the pattern
	int totalBondOrderPenalty();

	/*
	// Propagation / Selectors
	*/
	public:
	// Copy atomtypes for first molecule to all other molecules
	void propagateAtomtypes();
	// Copy bondtypes for first molecule to all other molecules
	void propagateBondTypes();
	// Select atom 'i' in all molecules
	void selectAtom(int id);

	/*
	// Properties
	*/
	public:
	// Calculate centre of geometry of molecule in specified config
	Vec3<double> calculateCog(Model*, int);
	// Calculate centre of mass of molecule in specified config
	Vec3<double> calculateCom(Model*, int);
};

#endif
