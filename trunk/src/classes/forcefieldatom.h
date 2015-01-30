/*
	*** Forcefield atom (type)
	*** src/classes/forcefieldatom.h
	Copyright T. Youngs 2007-2015

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

#include "classes/neta.h"
#include "base/dnchar.h"
#include "ff/forms.h"
#include "parser/variablelist.h"

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
	Dnchar netaString_;
	// Atomtype description
	Neta neta_;
	// Parameter data
	double params_[MAXFFPARAMDATA];
	// Atomic charge
	double charge_;
	// Parent forcefield
	Forcefield *parent_;
	// Element that the type relates to, or -1 for custom element name / mass (for, e.g., UA forcefields)
	int element_;
	// Custom 'element' mass
	double elementMass_;
	// Variable list of data items
	VariableList data_;


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
	VdwFunctions::VdwFunction vdwForm() const;
	// Set the type id
	void setTypeId(int i);
	// Returns the type id
	int typeId() const;
	// Set the charge of the type
	void setCharge(double q);
	// Returns the charge of the type
	double charge() const;
	// Set the name of the type
	void setName(const char *s);
	// Returns the name of the type
	const char *name() const;
	// Set the equivalent name of the type
	void setEquivalent(const char *s);
	// Returns the equivalent name of the type
	const char *equivalent() const;
	// Set the description of the type
	void setDescription(const char *s);
	// Returns the description of the type
	const char *description() const;
	// Returns the atomtype description
	Neta *neta();
	// Set the NETA string (and calculate new atomtype)
	bool setNeta(const char *s, Forcefield *parent);
	// Returns the original atomtype string
	const char *netaString() const;
	// Set the parameter data specified
	void setParameter(int i, double d);
	// Return parameter data specified
	double parameter(int i) const;
	// Returns parameter array pointer
	double *parameters();
	// Set the element that the type relates to, or -1 for custom element name / mass (for, e.g., UA forcefields)
	void setElement(int n);
	// Return the element that the type relates to, or -1 for custom element name / mass (for, e.g., UA forcefields)
	int element() const;
	// Set custom 'element' mass
	void setElementMass(double d);
	// Custom 'element' mass (or natural element name)
	double elementMass() const;
	// Return whether this is a united-atom type (i.e. has had its mass set explicitly)
	bool isUnitedAtom() const;
	// Add associated data
	void addData(const char *name, double d);
	void addData(const char *name, int i);
	void addData(const char *name, const char *);
	// Retrieve named variable
	Variable *data(const char *s, bool reportError = TRUE);
	// Return variable list
	VariableList *data();
};

#endif

