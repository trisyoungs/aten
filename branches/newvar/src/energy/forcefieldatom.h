/*
	*** Forcefield atom (type)
	*** src/classes/forcefieldatom.h
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

#ifndef ATEN_FORCEFIELDATOM_H
#define ATEN_FORCEFIELDATOM_H

#include "classes/atomtype.h"
#include "base/dnchar.h"
#include "energy/forms.h"

// Forward declarations
class Forcefield;

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
	VdwFunctions::VdwFunction vdwForm_;
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
	void setVdwForm(VdwFunctions::VdwFunction vf);
	// Returns the funcional VDW form
	VdwFunctions::VdwFunction vdwForm();
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

#endif

