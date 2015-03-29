/*
	*** Forcefield atom (type)
	*** src/base/forcefieldatom.h
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

#include "base/neta.h"
#include "ff/forms.h"
#include "parser/variablelist.h"
#include "base/namespace.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class Forcefield;

// Forcefield atom type
class ForcefieldAtom : public ListItem<ForcefieldAtom>
{
	public:
	// Constructor / Destructor
	ForcefieldAtom();
	~ForcefieldAtom();
	// Copy structure
	void copy(ForcefieldAtom* source);


	/*
	// Properties
	*/
	private:
	// Type of Van der Waals interactions in Forcefield
	VdwFunctions::VdwFunction vdwForm_;
	// Unique ffid of atom type in Forcefield
	int typeId_;
	// Name of atom type
	QString name_;
	// Equivalent name of atom type for intramolecular searching
	QString equivalent_;
	// Description of atom type
	QString description_;
	// Original atomtype string used to create the atomtype
	QString netaString_;
	// Atomtype description
	Neta neta_;
	// Parameter data
	double params_[MAXFFPARAMDATA];
	// Atomic charge
	double charge_;
	// Parent forcefield
	Forcefield* parent_;
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
	void setParent(Forcefield* ff);
	// Return parent forcefield
	Forcefield* parent();
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
	void setName(QString name);
	// Returns the name of the type
	QString name() const;
	// Set the equivalent name of the type
	void setEquivalent(QString s);
	// Returns the equivalent name of the type
	QString equivalent() const;
	// Set the description of the type
	void setDescription(QString s);
	// Returns the description of the type
	QString description() const;
	// Returns the atomtype description
	Neta* neta();
	// Set the NETA string (and calculate new atomtype)
	bool setNeta(QString neta, Forcefield* parent);
	// Returns the original NETA string
	QString netaString() const;
	// Set the parameter data specified
	void setParameter(int i, double d);
	// Return parameter data specified
	double parameter(int i) const;
	// Returns parameter array pointer
	double* parameters();
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
	void addData(QString name, double d);
	void addData(QString name, int i);
	void addData(QString name, QString s);
	// Retrieve named variable
	Variable* data(QString s, bool reportError = true);
	// Return variable list
	VariableList* data();
};

ATEN_END_NAMESPACE

#endif
