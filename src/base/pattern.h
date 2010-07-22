/*
	*** Molecule pattern
	*** src/base/pattern.h
	Copyright T. Youngs 2007-2010

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
#include "templates/reflist.h"
#include "base/constants.h"
#include "base/dnchar.h"

// Forward declarations
class Energy;
class Atom;
class ForcefieldAtom;
class ForcefieldBound;
class Forcefield;
class Atomtype;
class ComponentRegion;
class Model;
class Ring;
class Cell;

// Structures to hold/point to forcefield descriptions in patterns.
class PatternAtom
{
	public:
	// Constructor
	PatternAtom();
	// List pointers
	PatternAtom *prev, *next;

	/*
	// Forcefield and atom data
	*/
	private:
	// Original FF type of atom
	ForcefieldAtom *data_;
	// Pointer to atom in parent xmodel
	Atom *atom_;
	// Integer index of local forcefield bound data reference
	int forcefieldDataId_;

	public:
	// Set ff type of pattern atom
	void setData(ForcefieldAtom *ffa);
	// Get ff type of pattern atom
	ForcefieldAtom *data();
	// Set pointer to atom in first molecule
	void setAtom(Atom *a);
	// Get pointer to atom in first molecule
	Atom *atom();
	// Return integer index of unique atom data reference
	int forcefieldDataId() const;
	// Set integer index of unique atom data reference
	void setForcefieldDataId(int id);
};

class PatternBound
{
	public:
	// Constructor
	PatternBound();
	// List pointers
	PatternBound *prev, *next;
	// Friend Variable class
	friend class PatternBoundVariable;

	/*
	// Forcefield term data
	*/
	private:
	// Atoms involved in bond (referring to local molecule atom ids)
	int atomIds_[MAXFFBOUNDTYPES];
	// Pointer to function data / form
	ForcefieldBound *data_;
	// Integer index of local forcefield bound data reference
	int forcefieldDataId_;

	public:
	// Set atom id
	void setAtomId(int n, int i);
	// Return atom id
	int atomId(int n) const;
	// Set function data
	void setData(ForcefieldBound *ffb);
	// Get function data
	ForcefieldBound *data();
	// Return integer index of unique bound data reference
	int forcefieldDataId() const;
	// Set integer index of unique bound data reference
	void setForcefieldDataId(int id);
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
	int id() const;
	// Returns head of the atom list for this pattern (located in main model list)
	Atom *firstAtom();
	// Sets pointer to the first atom in this pattern (located in main model list)
	void setFirstAtom(Atom* i);
	// Returns last of the atom list for this pattern (located in main model list)
	Atom *lastAtom();
	// Sets pointer to the last atom in this pattern (located in main model list)
	void setLastAtom(Atom* i);
	// Calculate the global atom number offset of the first atom of the molecule
	int offset(int mol) const;
	// Returns the number of atoms in one molecule of the pattern
	int nAtoms() const;
	// Sets the starting atom of the model
	void setStartAtom(int n);
	// Returns the starting atom id of the pattern
	int startAtom() const;
	// Sets the end atom of the model
	void setEndAtom(int n);
	// Returns the ending atom id of the pattern
	int endAtom() const;
	// (Re)Calculate totalatoms
	void calcTotalAtoms();
	// Returns the total number of atoms in the pattern
	int totalAtoms() const;
	// Resets the 'tempi' variables of all atoms in the pattern to the given integer
	void resetTempI(int);
	// Sets the number of molecules in the pattern
	void setNMolecules(int n);
	// Returns the number of molecules in the pattern
	int nMolecules() const;
	// Sets the expected number of molecules in the pattern
	void setNExpectedMolecules(int n);
	// Returns the expected number of molecules in the pattern
	int nExpectedMolecules() const;
	// Sets the parent model
	void setParent(Model *m);
	// Returns the model for which the pattern was created
	Model *parent();
	// Sets the 'fixed' property of the pattern
	void setFixed(bool b);
	// Returns whether the pattern is fixed
	bool isFixed() const;
	// Sets the name of the pattern 
	void setName(const char *s);
	// Returns the pattern name
	const char *name() const;
	// Sets the forcefield to use in the pattern
	void setForcefield(Forcefield *newff);
	// Gets the forcefield associated with the pattern
	Forcefield *forcefield();
	// Returns whether the atomlimit in the pattern is valid
	bool isAtomLimitOk() const;
	// Returns whether the element composition in the pattern molecules is uniform
	bool areElementsOk() const;
	// Returns whether the bonding in the pattern molecules is uniform
	bool isBondingOk() const;
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
	// Reference list of bond terms in pattern
	Reflist<ForcefieldBound, int> forcefieldBonds_;
	// Reference list of angle terms in pattern
	Reflist<ForcefieldBound, int> forcefieldAngles_;
	// Reference list of torsion terms in pattern
	Reflist<ForcefieldBound, int> forcefieldTorsions_;
	// Reference list of atom types in pattern
	Reflist<ForcefieldAtom, int> forcefieldTypes_;
	// Add atom data
	void addAtomData(Atom *i, ForcefieldAtom *ffa);
	// Add bond data
	void addBondData(ForcefieldBound *ffb, int i, int j);
	// Add angle data
	void addAngleData(ForcefieldBound *ffb, int i, int j, int k);
	// Add torsion data
	void addTorsionData(ForcefieldBound *ffb, int i, int j, int k, int l);
	// Whether the positions of all molecules/atoms in the pattern are fixed in minimisations
	bool atomsFixed_;

	public:
	// Empty the arrays of the energy expression
	void deleteExpression();
	// Create the shell of the energy expression
	bool createExpression(bool vdwOnly = FALSE);
	// Create the connectivity and scaling matrices
	void createMatrices();
	// Update scaling matrices
	void updateScaleMatrices();
	// Return number of bonds in one molecule of the pattern
	int nBonds() const;
	// Return number of angles in one molecule of the pattern
	int nAngles() const;
	// Return number of torsions in one molecule of the pattern
	int nTorsions() const;
	// Return first bond of the pattern
	PatternBound *bonds();
	// Return first angle of the pattern
	PatternBound *angles();
	// Return first torsion of the pattern
	PatternBound *torsions();
	// Return selected bond of the pattern
	PatternBound *bond(int i);
	// Return selected angle of the pattern
	PatternBound *angle(int i);
	// Return selected torsion of the pattern
	PatternBound *torsion(int i);
	// Return number of unique bonds used in the pattern
	int nForcefieldBonds() const;
	// Return number of forcefield angles used in the pattern
	int nForcefieldAngles() const;
	// Return number of forcefield torsions used in the pattern
	int nForcefieldTorsions() const;
	// Return number of forcefield types used in the pattern
	int nForcefieldTypes() const;
	// Return first forcefield bond of the pattern
	Refitem<ForcefieldBound,int> *forcefieldBonds();
	// Return first forcefield angle of the pattern
	Refitem<ForcefieldBound,int> *forcefieldAngles();
	// Return first forcefield torsion of the pattern
	Refitem<ForcefieldBound,int> *forcefieldTorsions();
	// Return first forcefield type of the pattern
	Refitem<ForcefieldAtom,int> *forcefieldTypes();
	// Return selected forcefield bond of the pattern
	Refitem<ForcefieldBound,int> *forcefieldBond(int i);
	// Return selected forcefield angle of the pattern
	Refitem<ForcefieldBound,int> *forcefieldAngle(int i);
	// Return selected forcefield torsion of the pattern
	Refitem<ForcefieldBound,int> *forcefieldTorsion(int i);
	// Return selected forcefield type of the pattern
	Refitem<ForcefieldAtom,int> *forcefieldType(int i);
	// Return whether the positions of all molecules/atoms in the pattern are fixed in minimisations
	bool areAtomsFixed() const;
	// Set whether the positions of all molecules/atoms in the pattern are fixed in minimisations
	void setAtomsFixed(bool b);


	/*
	// Energy / Force Calculation
	*/
	public:
	// Calculate bond energy of pattern (or specific molecule)
	void bondEnergy(Model*, Energy*, int molecule = -1);
	// Calculate angle energy of pattern (or specific molecule)
	void angleEnergy(Model*, Energy*, int molecule = -1);
	// Calculate torsion energy (including impropers) of pattern (or specific molecule)
	void torsionEnergy(Model*, Energy*, int molecule = -1);
	// Calculate intrapattern Vdw energy (or for specific molecule)
	bool vdwIntraPatternEnergy(Model*, Energy*, int molecule = -1);
	// Calculate interpattern Vdw energy (or for specific molecule)
	bool vdwInterPatternEnergy(Model*, Pattern*, Energy*, int molecule = -1);
	// Calculate Vdw correction energy for pattern
	bool vdwCorrectEnergy(Cell*, Energy*);
	// Calculate intrapattern coulomb energy (or for specific molecule)
	void coulombIntraPatternEnergy(Model*, Energy*, int molecule = -1);
	// Calculate interpattern coulomb energy (or for specific molecule)
	void coulombInterPatternEnergy(Model*, Pattern*, Energy*, int molecule = -1);
	// Calculate intrapattern real-space Ewald energy (or for specific molecule)
	void ewaldRealIntraPatternEnergy(Model*, Energy*, int molecule = -1);
	// Calculate interpattern real-space Ewald energy (or for specific molecule)
	void ewaldRealInterPatternEnergy(Model*, Pattern*, Energy*, int molecule = -1);
	// Calculate reciprocal-space Ewald energy (or for specific molecule)
	void ewaldReciprocalEnergy(Model*, Pattern*, int, Energy*, int molecule = -1);
	// Calculate Ewald correction energy (or for specific molecule)
	void ewaldCorrectEnergy(Model*, Energy*, int molecule = -1);
	// Calculate bond forces in pattern
	void bondForces(Model*);
	// Calculate angle forces in pattern
	void angleForces(Model*);
	// Calculate torsion forces (including impropers) in pattern
	void torsionForces(Model*);
	// Calculate Vdw intrapattern forces
	bool vdwIntraPatternForces(Model*);
	// Calculate Vdw interpattern forces
	bool vdwInterPatternForces(Model*, Pattern*);
	// Calculate Coulomb intrapattern forces
	void coulombIntraPatternForces(Model*);
	// Calculate Coulomb interpattern forces
	void coulombInterPatternForces(Model*, Pattern*);
	// Calculate Ewald real-space intrapattern forces
	void ewaldRealIntraPatternForces(Model*);
	// Calculate Ewald real-space interpattern forces
	void ewaldRealInterPatternForces(Model*, Pattern*);
	// Calculate Ewald reciprocal-space forces
	void ewaldReciprocalForces(Model*);
	// Calculate Ewald force corrections
	void ewaldCorrectForces(Model*);


	/*
	// Typing
	*/
	private:
	// List of rings in one molecule of the pattern
	List<Ring> rings_;
	// Recursive ring-search routine
	bool ringSearch(Atom *i, Ring *currentpath);
	// Search existing ring list for existence of supplied ring
	bool isRingInList(Ring *source);

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
	// Return total bond order penalty of atoms in one molecule of the pattern
	int totalBondOrderPenalty();
	// Describe atoms in pattern, detect rings etc.
	void describeAtoms();


	/*
	// Propagation / Selectors
	*/
	public:
	// Copy atomtypes for first molecule to all other molecules
	void propagateAtomtypes();
	// Copy bondtypes for first molecule to all other molecules
	void propagateBondTypes();
	// Select atom 'i' in all molecules
	void selectAtom(int id, bool markonly = FALSE, bool deselect = FALSE);


	/*
	// Properties
	*/
	public:
	// Calculate centre of geometry of molecule in specified config
	Vec3<double> calculateCog(int, Model *source = NULL);
	// Calculate centre of mass of molecule in specified config
	Vec3<double> calculateCom(int, Model *source = NULL);
};

#endif
