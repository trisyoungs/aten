/*
	*** Molecular Mechanics Forcefield
	*** src/ff/forcefield.h
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

#ifndef ATEN_FORCEFIELD_H
#define ATEN_FORCEFIELD_H

#include "base/lineparser.h"
#include "base/prefs.h"
#include "base/neta.h"
#include "ff/forms.h"
#include "parser/program.h"
#include "templates/namemap.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Atom;
class ForcefieldAtom;
class ForcefieldBound;

// Forcefield
class Forcefield : public ListItem<Forcefield>
{
	public:
	// Constructor / Destructor
	Forcefield();
	~Forcefield();
        // Forcefield Commands
	enum ForcefieldCommand { AnglesCommand, BondsCommand, ConvertCommand, DataCommand, DefinesCommand, EScaleCommand, EquivalentsCommand, FunctionCommand, ImproperCommand, InterCommand, MessageCommand, NameCommand, TorsionsCommand, TypesCommand, UATypesCommand, UnitsCommand, UreyBradleyCommand, VdwCommand, VScaleCommand, nForcefieldCommands };
        static ForcefieldCommand forcefieldCommand(QString s);
	// Local parser
	LineParser ffparser;


	/*
	 * Specification
	 */
	private:
	// Title of Forcefield
	QString name_;
	// Filename
	QString filename_;
	// Energy unit of the forcefield parameters
	Prefs::EnergyUnit energyUnit_;

	public:
	// Sets the name of the Forcefield
	void setName(QString name);
	// Returns the name of the Forcefield
	QString name();
	// Set filename
	void setFilename(QString filename);
	// Return filename
	QString filename();
	// Return internal energy unit of forcefield
	Prefs::EnergyUnit energyUnit();
	// Set internal energy unit of forcefield
	void setEnergyUnit(Prefs::EnergyUnit eu);


	/*
	 * Types
	 */
	private:
	// List of type defines
	List<Neta> typeDefines_;
	// List of defined atom types
	List<ForcefieldAtom> types_;
	// List of data items provided for each atom type (if any)
	NameMapList<VTypes::DataType> typeData_;

	public:
	// Returns the number of atom types specified in the Forcefield
	int nTypes();
	// Adds a new type to the forcefield
	ForcefieldAtom* addType(int id = -1, QString name = QString(), QString equivalent = QString(), int element = 0, QString neta = QString(), QString description = QString());
	// Returns the head of tha atomtype list
	ForcefieldAtom* types();
	// Returns the n'th type in the list
	ForcefieldAtom* type(int n);
	// Get the atomtype specified by the ffid number passed
	ForcefieldAtom* findType(int id);
	// Find the named atomtype
	ForcefieldAtom* findType(QString name);
	// Returns the ForcefieldAtom with the typeId provided
	ForcefieldAtom* findByTypeId(int id, ForcefieldAtom* excluding = NULL);
	// Return number of type defines in forcefield
	int nTypeDefines();
	// Return type defines list
	Neta* typeDefines();
	// Find type define
	Neta* typeDefine(QString name);
	// Returns whether the specified forcefield type is contained in this forcefield
	bool containsType(ForcefieldAtom* type);


	/*
	 * Bonding Interactions
	 */
	private:
	// List of bond data
	List<ForcefieldBound> bonds_;

	public:
	// Add bond term to the forcefield
	ForcefieldBound* addBond(BondFunctions::BondFunction form, QString type1 = QString(), QString type2 = QString());
	// Return number of terms defined in bonds list
	int nBonds();
	// Returns the bond list
	ForcefieldBound* bonds();
	// Return the n'th bond in the list
	ForcefieldBound* bond(int n);
	// Retrieve bond data corresponding to specified atomtype id's
	ForcefieldBound* findBond(ForcefieldAtom* ffi, ForcefieldAtom* ffj);
	// Retrieve bond data corresponding to specified names
	ForcefieldBound* findBond(QString typei, QString typej);


	/*
	 * Angle Interactions
	 */
	private:
	// List of angle data
	List<ForcefieldBound> angles_;

	public:
	// Add angle term to the forcefield
	ForcefieldBound* addAngle(AngleFunctions::AngleFunction form, QString type1 = QString(), QString type2 = QString(), QString type3 = QString());
	// Return number of terms defined in angles list
	int nAngles();
	// Returns the angle list
	ForcefieldBound* angles();
	// Return the n'th angle in the list
	ForcefieldBound* angle(int n);
	// Retrieve angle data corresponding to specified atomtype id's
	ForcefieldBound* findAngle(ForcefieldAtom* ffi, ForcefieldAtom* ffj, ForcefieldAtom* ffk);
	// Retrieve angle data corresponding to specified names
	ForcefieldBound* findAngle(QString typei, QString typej, QString typek);
	

	/*
	 * Torsion Interactions
	 */
	private:
	// List of torsion data
	List<ForcefieldBound> torsions_;

	public:
	// Add torsion term to the forcefield
	ForcefieldBound* addTorsion(TorsionFunctions::TorsionFunction form, QString type1 = QString(), QString type2 = QString(), QString type3 = QString(), QString type4 = QString());
	// Return number of terms defined in torsions list
	int nTorsions();
	// Returns the torsion list
	ForcefieldBound* torsions();
	// Return the n'th torsion in the list
	ForcefieldBound* torsion(int n);
	// Retrieve torsion data corresponding to specified atomtype id's
	ForcefieldBound* findTorsion(ForcefieldAtom* ffi, ForcefieldAtom* ffj, ForcefieldAtom* ffk, ForcefieldAtom* ffl);
	// Retrieve torsion data corresponding to specified names
	ForcefieldBound* findTorsion(QString typei, QString typej, QString typek, QString typel);


	/*
	 * Improper Torsion Interactions
	 */
	private:
	// List of improper torsions data
	List<ForcefieldBound> impropers_;

	public:
	// Add improper torsion term to the forcefield
	ForcefieldBound* addImproper(TorsionFunctions::TorsionFunction form);
	// Return number of improper torsion terms defined in torsions list
	int nImpropers();
	// Returns the improper torsion list
	ForcefieldBound* impropers();
	// Return the n'th improper torsion in the list
	ForcefieldBound* improper(int n);
	// Retrieve improper torsion data corresponding to specified atomtype id's
	ForcefieldBound* findImproper(ForcefieldAtom*, ForcefieldAtom*, ForcefieldAtom*, ForcefieldAtom*);
	// Retrieve improper torsion data corresponding to specified names
	ForcefieldBound* findImproper(QString typei, QString typej, QString typek, QString typel);


	/*
	 * Urey-Bradley Interactions
	 */
	private:
	// List of Urey-Bradley data
	List<ForcefieldBound> ureyBradleys_;

	public:
	// Add Urey-Bradley term to the forcefield
	ForcefieldBound* addUreyBradley(BondFunctions::BondFunction form);
	// Return number of terms defined in Urey-Bradley list
	int nUreyBradleys();
	// Returns the Urey-Bradley list
	ForcefieldBound* ureyBradleys();
	// Return the n'th Urey-Bradley term in the list
	ForcefieldBound* ureyBradley(int n);
	// Retrieve Urey-Bradley data corresponding to specified atomtype id's
	ForcefieldBound* findUreyBradley(ForcefieldAtom*, ForcefieldAtom*, ForcefieldAtom*);
	// Retrieve Urey-Bradley data corresponding to specified names
	ForcefieldBound* findUreyBradley(QString typei, QString typej, QString typek);
	

	/*
	 * Parameter Generation
	 */
	private:
	// List of data values that have units of energy (and thus should be converted)
	QStringList energyData_;
	// Generator function program listings
	QStringList generatorFunctionText_;
	// Container for generator functions defined in this forcefield
	Program generatorFunctions_;
	// Pointer to vdw generation function (if one is defined)
	Tree* vdwGenerator_;
	// Pointer to bond generation function (if one is defined)
	Tree* bondGenerator_;
	// Pointer to angle generation function (if one is defined)
	Tree* angleGenerator_;
	// Pointer to torsion generation function (if one is defined)
	Tree* torsionGenerator_;

	public:
	// Add energy data value to list of those flagged as energies
	void addEnergyData(QString s);
	// Return list of energy data values
	QStringList energyData();
	// Return pointer to vdw generation function (if one is defined)
	Tree* vdwGenerator();
	// Return pointer to bond generation function (if one is defined)
	Tree* bondGenerator();
	// Return pointer to angle generation function (if one is defined)
	Tree* angleGenerator();
	// Return pointer to torsion generation function (if one is defined)
	Tree* torsionGenerator();
	// Generate VDW params for specified atom
	bool generateVdw(Atom* i);
	// Generate bond params for specified atoms
	ForcefieldBound* generateBond(Atom* i, Atom* j);
	// Generate angle params for specified atoms
	ForcefieldBound* generateAngle(Atom* i, Atom* j, Atom* k);
	// Generate torsion params for specified atoms
	ForcefieldBound* generateTorsion(Atom* i, Atom* j, Atom* k, Atom* l);


	/*
	 * Parameter Matching
	 */
	public:
	// Character-match the atomtype names supplied
	int matchType(QString source, QString target);
	// Match names of atomtypes supplied to strings supplied
	int matchTypes(ForcefieldAtom* ffi, ForcefieldAtom* ffj, QString typei, QString typej);
	// Match names of supplied typenames and test names 
	int matchTypes(QString testi, QString testj, QString typei, QString typej);


	/*
	 * File
	 */
	private:
	// Reads in any type defines
	bool readDefines();
	// Reads in the atom type definitions
	bool readTypes();
	// Reads in the united atom type definitions
	bool readUnitedAtomTypes();
	// Reads in extra data for atoms
	bool readData(QString vars);
	// Read in generator function definitions
	bool readFunctions();
	// Reads in and applies equivalent atomtype names
	bool readEquivalents();
	// Reads in intermolecular parameters for atom types
	bool readInter();
	// Reads in bond data
	bool readBonds();
	// Reads in angle data
	bool readAngles();
	// Reads in torsion data
	bool readTorsions();
	// Reads in improper data
	bool readImpropers();
	// Read in Urey-Bradley data
	bool readUreyBradley();

	public:
	// Load Forcefield from the filename supplied
	bool load(QString filename);
	// Save forcefield under its current filename
	bool save();


	/*
	 * Misc
	 */
	public:
	// Convert the parameters in the FF to the internal working energy unit
	void convertParameters();
};

ATEN_END_NAMESPACE

#endif

