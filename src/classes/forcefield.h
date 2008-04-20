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

#ifndef ATEN_FFIELD_H
#define ATEN_FFIELD_H

#include "classes/atom.h"
#include "classes/dnchar.h"
#include "energy/forms.h"
#include "base/prefs.h"

// Maximum data per ForcefieldParams node. Also define last two elements to be torsion 1-4 scaling factors
#define MAXFFPARAMDATA 6
#define MAXFFBOUNDTYPES 4
#define TF_ESCALE (MAXFFPARAMDATA-2)
#define TF_VSCALE (MAXFFPARAMDATA-1)

// Forward declarations
class Atomtype;

// Forcefield parameters
class ForcefieldParams
{
	public:
	// Storage for parameters used in functions
	double data[MAXFFPARAMDATA];
	// Constructor
	ForcefieldParams();
};

// Forcefield atom type
class ForcefieldAtom
{
	public:
	// Constructor / Destructor
	ForcefieldAtom();
	~ForcefieldAtom();
	// List pointers
	ForcefieldAtom *prev, *next;
	// Copy structure
	void copy(ForcefieldAtom *source);

	/*
	// Properties
	*/
	private:
	// Type of Van der Waals interactions in Forcefield
	Forms::VdwFunction vdwForm_;
	// Unique ffid of atom type in Forcefield
	int typeId_;
	// Name of atom type
	Dnchar name_;
	// Equivalent name of atom type for intramolecular searching
	Dnchar equivalent_;
	// Description of atom type
	Dnchar description_;
	// Original atomtype string used to create the atomtype
	Dnchar atomtypeString_;
	// Atomtype description
	Atomtype atomtype_;
	// Parameter data
	ForcefieldParams params_;
	// Generator data (if present in a rule-based Forcefield)
	double *generator_;
	// Atomic charge
	double charge_;
	// Parent forcefield
	Forcefield *parent_;

	/*
	// Set / Get
	*/
	public:
	// Set parent forcefield
	void setParent(Forcefield *ff);
	// Return parent forcefield
	Forcefield *parent();
	// Set functional form of VDW
	void setVdwForm(Forms::VdwFunction vf);
	// Returns the funcional VDW form
	Forms::VdwFunction vdwForm();
	// Set the type id
	void setTypeId(int i);
	// Returns the type id
	int typeId();
	// Set the charge of the type
	void setCharge(double q);
	// Returns the charge of the type
	double charge();
	// Set the name of the type
	void setName(const char *s);
	// Returns the name of the type
	const char *name();
	// Set the equivalent name of the type
	void setEquivalent(const char *s);
	// Returns the equivalent name of the type
	const char *equivalent();
	// Set the description of the type
	void setDescription(const char *s);
	// Returns the description of the type
	const char *description();
	// Returns the atomtype description
	Atomtype *atomtype();
	// Set the atomtype string (and calculate new atomtype)
	void setAtomtype(const char *s, Forcefield *parent, ForcefieldAtom *ffa);
	// Returns the original atomtype string
	const char *atomtypeString();
	// Returns ForcefieldParams structure
	ForcefieldParams &params();
	// Set generator data
	void setGenerator(int i, double d);
	// Initialise generator array
	void initialiseGenerator();
	// Return generator data array
	double *generator();
	// Return single generator value
	double generator(int i);
};

// Forcefield bound interaction type
class ForcefieldBound
{
	public:
	// List pointers
	ForcefieldBound *prev, *next;
	// Constructor
	ForcefieldBound();
	// Forcefield Bound Interaction Type
	enum BoundType { NoInteraction, BondInteraction, AngleInteraction, TorsionInteraction };

	private:
	// Type of bound interaction
	BoundType type_;
	// Form of bound interaction type
	union BoundForms
	{
		Forms::BondFunction bondFunc;
		Forms::AngleFunction angleFunc;
		Forms::TorsionFunction torsionFunc;
	} functionalForm_;
	// Forcefield types involved in this term
	Dnchar typeNames_[MAXFFBOUNDTYPES];
	// Pointer to parameter data
	ForcefieldParams params_;

	public:
	// Set the type of bound interaction
	void setType(BoundType fc);
	// Return the type of bound interaction
	BoundType type();
	// Return the functional form
	BoundForms functionalForm();
	// Set the bond functional form
	void setBondStyle(Forms::BondFunction bf);
	// Set the angle functional form
	void setAngleStyle(Forms::AngleFunction af);
	// Set the torsion functional form
	void setTorsionStyle(Forms::TorsionFunction tf);
	// Return the data[] array in *params
	ForcefieldParams &params();
	// Return the atom type 'n'
	const char *typeName(int n);
	// Set the atom type 'n'
	void setTypeName(int n, const char *s);
};

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
	// Number of generator data per atom (if rule-based)
	int nGenerators_;
	// Generator values that have units of energy (and thus should be converted)
	bool *energyGenerators_;
	// Which rules the ff uses (if any)
	Forms::ForcefieldRules rules_;

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
	Forms::ForcefieldRules rules();
	// Return the number of generators for each type
	int nGenerators();

	/*
	// Types
	*/
	private:
	// Atom type name and dispersion data array
	List<ForcefieldAtom> types_;

	public:
	// Returns the number of atom types specified in the Forcefield
	int nTypes();
	// Returns the head of tha atomtype list
	ForcefieldAtom *types();
	// Get the atomtype specified by the ffid number passed
	ForcefieldAtom *findType(int);
	// Get the array entry of the atomtype specified by the atomname passed
	ForcefieldAtom *findType(const char*);
	// Returns the type description of the ForcefieldAtom with the ffid provided
	Atomtype *typeOfId(int id);

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
	// Return number of terms defined in bonds list
	int nBonds();
	// Returns the bond list
	ForcefieldBound *bonds();
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
	// Return number of terms defined in angles list
	int nAngles();
	// Returns the angle list
	ForcefieldBound *angles();
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
	// Return number of terms defined in torsions list
	int nTorsions();
	// Returns the angle list
	ForcefieldBound *torsions();
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
	void convertParameters(Prefs::EnergyUnit);
	// Get the bond order of the bond ij (here for convenience)
	double bondOrder(Atom*, Atom*);
};

#endif

