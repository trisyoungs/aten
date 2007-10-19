/*
	*** Molecular mechanics forcefield
	*** src/classes/forcefield.h
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

#ifndef H_FFIELD_H
#define H_FFIELD_H

#include "classes/atom.h"
#include "classes/dnchar.h"
#include "energy/forms.h"
#include "base/prefs.h"

// Forcefield Components
enum ff_component { FFC_BOND, FFC_ANGLE, FFC_TORSION, FFC_NITEMS };

// Maximum data per ffparams node. Also define last two elements to be torsion 1-4 scaling factors
#define MAXFFPARAMDATA 6
#define MAXFFBOUNDTYPES 4
#define TF_ESCALE (MAXFFPARAMDATA-2)
#define TF_VSCALE (MAXFFPARAMDATA-1)

// Forcefield Dictionary
enum ff_dict { FFK_UNKNOWN, FFK_NAME, FFK_UNITS, FFK_RULES, FFK_TYPES, FFK_GENERATOR, FFK_EQUIVALENTS, FFK_VDW, FFK_BONDS, FFK_ANGLES, FFK_TORSIONS, FFK_VSCALE, FFK_ESCALE, FFK_NITEMS };

// Forward declarations
class atomtype;

// Forcefield parameters
class ffparams
{
	public:
	// Storage for parameters used in functions
	double data[MAXFFPARAMDATA];
	// Constructor / Destructor
	ffparams();
	~ffparams();
};

// Forcefield atom type
class ffatom
{
	public:
	// Constructor / Destructor
	ffatom();
	~ffatom();
	// List pointers
	ffatom *prev, *next;
	friend class forcefield;

	/*
	// Properties
	*/
	private:
	// Type of Van der Waals interactions in forcefield
	vdw_func vdwstyle;
	// Unique ffid of atom type in forcefield
	int ffid;
	// Name of atom type
	dnchar name;
	// Equivalent name of atom type for intramolecular searching
	dnchar equiv;
	// Atomtype description
	atomtype typedesc;
	// Pointer to parameter data
	ffparams params;
	// Generator data (if present in a rule-based forcefield)
	double *generator;
	// Atomic charge
	double q;
	public:
	// Set functional form of VDW
	void set_style(vdw_func vf) { vdwstyle = vf; }
	// Returns the funcional VDW form
	vdw_func get_style() { return vdwstyle; }

	/*
	// Get
	*/
	public:
	// Returns the functional form of the potential
	vdw_func get_funcform() { return vdwstyle; }
	// Returns the ffid of the type
	int get_ffid() { return ffid; }
	// Returns the charge of the type
	double get_charge() { return q; }
	// Returns the name of the type
	const char *get_name() { return name.get(); }
	// Returns the equivalent name of the type
	const char *get_equiv() { return equiv.get(); }
	// Returns the atomtype description
	atomtype *get_atomtype() { return &typedesc; }
	// Returns ffparams structure
	ffparams &get_params() { return params; }
};

// Forcefield bound interaction type
class ffbound
{
	private:
	// Type of bound interaction
	ff_component type;
	// Form of bound interaction type
	union bound_forms
	{
		bond_func bondfunc;
		angle_func anglefunc;
		torsion_func torsionfunc;
	} funcform;
	// Forcefield types involved in this term
	dnchar types[MAXFFBOUNDTYPES];
	// Pointer to parameter data
	ffparams params;
	public:
	// Set the type of bound interaction
	void set_type(ff_component fc) { type = fc; }
	// Return the type of bound interaction
	ff_component get_type() { return type; }
	// Return the functional form
	bound_forms get_funcform() { return funcform; }
	// Set the bond functional form
	void set_bond_style(bond_func bf) { funcform.bondfunc = bf; }
	// Set the angle functional form
	void set_angle_style(angle_func af) { funcform.anglefunc = af; }
	// Set the torsion functional form
	void set_torsion_style(torsion_func tf) { funcform.torsionfunc = tf; }
	// Return the data[] array in *params
	ffparams &get_params() { return params; };
	// List pointers
	ffbound *prev, *next;
	// Constructor / Destructor
	ffbound();
	~ffbound();
	// Return the atom type 'n'
	const char *get_type(int n) { return (n < MAXFFBOUNDTYPES ? types[n].get() : "OUTOFRANGE"); }
	friend class forcefield;
};

// Forcefield
class forcefield
{
	public:
	// Constructor / Destructor
	forcefield();
	~forcefield();
	// List pointers
	forcefield *prev, *next;

	/*
	// Specification
	*/
	private:
	// Title of forcefield
	dnchar ffname;
	// Location of forcefield
	dnchar path;
	// Number of generator data per atom (if rule-based)
	int ngendata;
	// Generator data values to do energy conversion on
	bool *convertgen;
	// Which rules the ff uses (if any)
	ff_rules rules;

	public:
	// Sets the name of the forcefield
	void set_name(const char *s) { ffname.set(s); }
	// Returns the name of the forcefield
	const char *get_name() { return ffname.get(); }
	// Returns the number of atom types specified in the forcefield
	int get_natomtypes() { return atomtypes.size(); }
	// Returns the typing rules of the forcefield
	ff_rules get_rules() { return rules; }

	/*
	// Types
	*/
	private:
	// Atom type name and dispersion data array (dim 'natomtypes')
	list<ffatom> atomtypes;

	public:
	// Get the atomtype specified by the ffid number passed
	ffatom *find_type(int);
	// Returns the head of tha atomtype list
	ffatom *get_atomtypes() { return atomtypes.first(); }
	// Get the array entry of the atomtype specified by the atomname passed
	ffatom *find_type(const char*);
	// Returns the type description of the ffatom with the ffid provided
	atomtype *get_atomtype_of_ffid(int);

	/*
	// VDW
	*/
	public:
	// Generate the VDW parameters (rule-based forcefield)
	void generate_vdw(atom*);

	/*
	// Bonding Interactions
	*/
	private:
	// List pointers for bond data
	list<ffbound> bonds;

	public:
	// Generate bond parameters (rule-based forcefield)
	ffbound *generate_bond(atom*, atom*);
	// Returns the bond list
	ffbound *get_bonds() { return bonds.first(); }
	// Retrieve bond data corresponding to specified atomtype id's
	ffbound *find_bond(ffatom*, ffatom*);

	/*
	// Angle Interactions
	*/
	private:
	// List pointers for angle data
	list<ffbound> angles;

	public:
	// Generate angle parameters (rule-based forcefield)
	ffbound *generate_angle(atom*, atom*, atom*);
	// Returns the angle list
	ffbound *get_angles() { return angles.first(); }
	// Retrieve angle data corresponding to specified atomtype id's
	ffbound *find_angle(ffatom*, ffatom*, ffatom*);

	/*
	// Torsion Interactions
	*/
	private:
	// List pointers for torsion data
	list<ffbound> torsions; 		

	public:
	// Generate angle parameters (rule-based forcefield)
	ffbound *generate_torsion(atom*, atom*, atom*, atom*);
	// Returns the angle list
	ffbound *get_torsions() { return torsions.first(); }
	// Retreve torsion data corresponding to specified atomtype id's
	ffbound *find_torsion(ffatom*, ffatom*, ffatom*, ffatom*);

	/*
	// Parameter Matching
	*/
	public:
	// Character-match the atomtype names supplied
	int match_type(const char*, const char*);
	// Character-match the atomtype names supplied
	int match_type(const dnchar &a, const dnchar &b) { return match_type(a.get(),b.get()); }
	// Match names of atomtypes supplied to strings supplied
	int match_types(ffatom*, ffatom*, const dnchar&, const dnchar&);

	/*
	// File
	*/
	private:
	// Reads in the atom type definitions
	bool read_types(ifstream&);
	// Reads in generator data for atoms (rule-based ff)
	bool read_generator(ifstream&);
	// Reads in and applies equivalent atomtype names
	bool read_equivalents(ifstream&);
	// Reads in VDW parameters for atom types
	bool read_vdw(ifstream&);
	// Reads in bond data
	bool read_bonds(ifstream&);
	// Reads in angle data
	bool read_angles(ifstream&);
	// Reads in torsion data
	bool read_torsions(ifstream&);

	public:
	// Load forcefield from the filename supplied
	bool load(const char*);

	/*
	// Misc
	*/
	public:
	// Convert the parameters in the FF to the internal working energy unit
	void convert_parameters(energy_unit);
	// Get the bond order of the bond ij (here for convenience)
	double get_bond_order(atom*, atom*);
};

#endif

