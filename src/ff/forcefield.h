/*
	*** Molecular mechanics Forcefield
	*** src/classes/Forcefield.h
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

#ifndef ATEN_FORCEFIELD_H
#define ATEN_FORCEFIELD_H

#include "base/dnchar.h"
#include "base/forms.h"
#include "aten/prefs.h"

// Forward declarations
class Atom;
class ForcefieldAtom;
class ForcefieldBound;

// Forcefield
class Forcefield
{
	public:
	// Constructor / Destructor
	Forcefield();
	~Forcefield();
	// List pointers
	Forcefield *prev, *next;
        // Forcefield Commands
	enum ForcefieldCommand { UnknownCommand, NameCommand, UnitsCommand, RulesCommand, TypesCommand, GeneratorCommand, ConvertCommand, EquivalentsCommand, VdwCommand, BondsCommand, AnglesCommand, TorsionsCommand, VScaleCommand, EScaleCommand, nForcefieldCommands };
        static ForcefieldCommand forcefieldCommand(const char *s);

	/*
	// Specification
	*/
	private:
	// Title of Forcefield
	Dnchar name_;
	// Filename
	Dnchar filename_;
	// Generator values that have units of energy (and thus should be converted)
	bool energyGenerators_[MAXFFGENDATA];
	// Which rules the ff uses (if any)
	Rules::ForcefieldRules rules_;
	// Energy unit of the forcefield parameters
	Prefs::EnergyUnit energyUnit_;

	public:
	// Sets the name of the Forcefield
	void setName(const char *s);
	// Returns the name of the Forcefield
	const char *name();
	// Set filename
	void setFilename(const char *s);
	// Return filename
	const char *filename();
	// Returns the typing rules of the Forcefield
	Rules::ForcefieldRules rules();
	// Set conversion flag for energetic generator data
	void setEnergyGenerator(int n);
	// Set internal energy unit of forcefield
	void setEnergyUnit(Prefs::EnergyUnit eu);

	/*
	// Types
	*/
	private:
	// Atom type name and dispersion data array
	List<ForcefieldAtom> types_;

	public:
	// Returns the number of atom types specified in the Forcefield
	int nTypes();
	// Adds a new type to the forcefield
	ForcefieldAtom *addType();
	// Returns the head of tha atomtype list
	ForcefieldAtom *types();
	// Returns the n'th type in the list
	ForcefieldAtom *type(int n);
	// Get the atomtype specified by the ffid number passed
	ForcefieldAtom *findType(int);
	// Get the array entry of the atomtype specified by the atomname passed
	ForcefieldAtom *findType(const char*);
	// Returns the ForcefieldAtom with the typeId provided
	ForcefieldAtom *findByTypeId(int id, ForcefieldAtom *excluding = NULL);

	/*
	// VDW
	*/
	public:
	// Generate the VDW parameters (rule-based Forcefield)
	void generateVdw(Atom*);

	/*
	// Bonding Interactions
	*/
	private:
	// List pointers for bond data
	List<ForcefieldBound> bonds_;

	public:
	// Generate bond parameters (rule-based Forcefield)
	ForcefieldBound *generateBond(Atom*, Atom*);
	// Add bond term to the forcefield
	ForcefieldBound *addBond(BondFunctions::BondFunction form);
	// Return number of terms defined in bonds list
	int nBonds();
	// Returns the bond list
	ForcefieldBound *bonds();
	// Return the n'th bond in the list
	ForcefieldBound *bond(int n);
	// Retrieve bond data corresponding to specified atomtype id's
	ForcefieldBound *findBond(ForcefieldAtom*, ForcefieldAtom*);

	/*
	// Angle Interactions
	*/
	private:
	// List pointers for angle data
	List<ForcefieldBound> angles_;

	public:
	// Generate angle parameters (rule-based Forcefield)
	ForcefieldBound *generateAngle(Atom*, Atom*, Atom*);
	// Add angle term to the forcefield
	ForcefieldBound *addAngle(AngleFunctions::AngleFunction form);
	// Return number of terms defined in angles list
	int nAngles();
	// Returns the angle list
	ForcefieldBound *angles();
	// Return the n'th angle in the list
	ForcefieldBound *angle(int n);
	// Retrieve angle data corresponding to specified atomtype id's
	ForcefieldBound *findAngle(ForcefieldAtom*, ForcefieldAtom*, ForcefieldAtom*);

	/*
	// Torsion Interactions
	*/
	private:
	// List pointers for torsion data
	List<ForcefieldBound> torsions_;

	public:
	// Generate angle parameters (rule-based Forcefield)
	ForcefieldBound *generateTorsion(Atom*, Atom*, Atom*, Atom*);
	// Add torsion term to the forcefield
	ForcefieldBound *addTorsion(TorsionFunctions::TorsionFunction form);
	// Return number of terms defined in torsions list
	int nTorsions();
	// Returns the angle list
	ForcefieldBound *torsions();
	// Return the n'th torsion in the list
	ForcefieldBound *torsion(int n);
	// Retreve torsion data corresponding to specified atomtype id's
	ForcefieldBound *findTorsion(ForcefieldAtom*, ForcefieldAtom*, ForcefieldAtom*, ForcefieldAtom*);

	/*
	// Parameter Matching
	*/
	public:
	// Character-match the atomtype names supplied
	int matchType(const char*, const char*);
	// Character-match the atomtype names supplied
	int matchType(const Dnchar &a, const Dnchar &b);
	// Match names of atomtypes supplied to strings supplied
	int matchTypes(ForcefieldAtom*, ForcefieldAtom*, const char*, const char*);

	/*
	// File
	*/
	private:
	// Reads in the atom type definitions
	bool readTypes(ifstream&);
	// Reads in generator data for atoms (rule-based ff)
	bool readGenerator(ifstream&);
	// Reads in and applies equivalent atomtype names
	bool readEquivalents(ifstream&);
	// Reads in VDW parameters for atom types
	bool readVdw(ifstream&);
	// Reads in bond data
	bool readBonds(ifstream&);
	// Reads in angle data
	bool readAngles(ifstream&);
	// Reads in torsion data
	bool readTorsions(ifstream&);

	public:
	// Load Forcefield from the filename supplied
	bool load(const char*);

	/*
	// Misc
	*/
	public:
	// Convert the parameters in the FF to the internal working energy unit
	void convertParameters();
};

#endif

