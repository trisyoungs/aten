/*
	*** Forcefield atom (type)
	*** src/classes/forcefieldatom.cpp
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

#include "base/elements.h"
#include "classes/forcefieldatom.h"
#include "classes/neta_parser.h"
#include "parser/double.h"
#include "parser/integer.h"
#include "parser/character.h"

// Constructor
ForcefieldAtom::ForcefieldAtom()
{
	// Private variables
	name_.set("Unnamed");
	typeId_ = -1;
	charge_ = 0.0;
	vdwForm_ = VdwFunctions::None;
	parent_ = NULL;
	neta_.setParentForcefieldAtom(this);
	element_ = 0;
	elementMass_ = -1.0;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor
ForcefieldAtom::~ForcefieldAtom()
{
}

// Set parent forcefield
void ForcefieldAtom::setParent(Forcefield *ff)
{
	parent_ = ff;
	neta_.setParentForcefield(ff);
}

// Return parent forcefield
Forcefield *ForcefieldAtom::parent()
{
	return parent_;
}

// Set functional form of VDW
void ForcefieldAtom::setVdwForm(VdwFunctions::VdwFunction vf)
{
	vdwForm_ = vf;
	// Copy default parameters to structure
	for (int i=0; i<MAXFFPARAMDATA; i++) params_[i] = VdwFunctions::VdwFunctions[vf].defaultValues[i];
}

// Returns the funcional VDW form
VdwFunctions::VdwFunction ForcefieldAtom::vdwForm() const
{
	return vdwForm_;
}

// Set the type id
void ForcefieldAtom::setTypeId(int i)
{
	typeId_ = i;
}

// Returns the type id
int ForcefieldAtom::typeId() const
{
	return typeId_;
}

// Set the charge of the type
void ForcefieldAtom::setCharge(double q)
{
	charge_ = q;
}

// Returns the charge of the type
double ForcefieldAtom::charge() const
{
	return charge_;
}

// Set the name of the type
void ForcefieldAtom::setName(const char *s)
{
	name_ = s;
}

// Returns the name of the type
const char *ForcefieldAtom::name() const
{
	return name_.get();
}

// Set the equivalent name of the type
void ForcefieldAtom::setEquivalent(const char *s)
{
	equivalent_ = s;
}

// Returns the equivalent name of the type
const char *ForcefieldAtom::equivalent() const
{
	return equivalent_.get();
}

// Set the description of the type
void ForcefieldAtom::setDescription(const char *s)
{
	description_ = s;
}

// Returns the description of the type
const char *ForcefieldAtom::description() const
{
	return description_.get();
}

// Set atomtype string and generate new type description
bool ForcefieldAtom::setNeta(const char *s, Forcefield *parent)
{
	netaString_ = s;
	// If supplied parent is NULL, use current parent (if not also NULL)
	if (parent == NULL)
	{
		parent = parent_;
		if (parent == NULL) printf("ForcefieldAtom::setNeta has no valid parent forcefield.\n");
	}
	return netaparser.createNeta(&neta_, s, parent);
}

// Return original typestring
const char *ForcefieldAtom::netaString() const
{
	return netaString_.get();
}

// Returns the atomtype description
Neta *ForcefieldAtom::neta()
{
	return &neta_;
}

// Set the parameter data specified
void ForcefieldAtom::setParameter(int i, double d)
{
	if ((i < 0) || (i >= MAXFFPARAMDATA)) printf("Data Id in ForcefieldAtom::setParameter (%i) is out of bounds.\n", i);
	else params_[i] = d;
}

// Return parameter data specified
double ForcefieldAtom::parameter(int i) const
{
	if ((i < 0) || (i >= MAXFFPARAMDATA)) printf("Data Id in ForcefieldAtom::parameter (%i) is out of bounds.\n", i);
	else return params_[i];
	return 0.0;
}

// Returns parameter array pointer
double *ForcefieldAtom::parameters()
{
	return params_;
}

// Set the element that the type relates to, or -1 for custom element name / mass (for, e.g., UA forcefields)
void ForcefieldAtom::setElement(int n)
{
	element_ = n;
}

// Return the element that the type relates to, or -1 for custom element name / mass (for, e.g., UA forcefields)
int ForcefieldAtom::element() const
{
	return element_;
}

// Set custom 'element' mass
void ForcefieldAtom::setElementMass(double d)
{
	elementMass_ = d;
}

// Custom 'element' mass (or natural element mass)
double ForcefieldAtom::elementMass() const
{
	return (elementMass_ < 0.0 ? elements().atomicMass(element_) : elementMass_);
}

// Return whether this is a united-atom type (i.e. has had its mass set explicitly)
bool ForcefieldAtom::isUnitedAtom() const
{
	return (elementMass_ > -0.5);
}

// Add associated data
void ForcefieldAtom::addData(const char *name, double d)
{
	// Does this data already exist?
	Variable *v = data_.find(name);
	if (v != NULL) msg.print("Warning: Data '%s' for forcefield atom already exists and will be overwritten.\n", name);
	v = new DoubleVariable(d, FALSE);
	v->setName(name);
	data_.take(v);
}

void ForcefieldAtom::addData(const char *name, int i)
{
	// Does this data already exist?
	Variable *v = data_.find(name);
	if (v != NULL) msg.print("Warning: Data '%s' for forcefield atom already exists and will be overwritten.\n", name);
	v = new IntegerVariable(i, FALSE);
	v->setName(name);
	data_.take(v);
}

void ForcefieldAtom::addData(const char *name, const char *s)
{
	// Does this data already exist?
	Variable *v = data_.find(name);
	if (v != NULL) msg.print("Warning: Data '%s' for forcefield atom already exists and will be overwritten.\n", name);
	v = new StringVariable(s, FALSE);
	v->setName(name);
	data_.take(v);
}

// Retrieve named variable
Variable *ForcefieldAtom::data(const char *s, bool reportError)
{
	Variable *v = data_.find(s);
	if ((v == NULL) && reportError) msg.print("Error: Forcefield atom '%s' does not contain any data named '%s'.\n", name_.get(), s);
	return v;
}

// Return variable list
VariableList *ForcefieldAtom::data()
{
	return &data_;	
}

// Copy structure
void ForcefieldAtom::copy(ForcefieldAtom *source)
{
	vdwForm_ = source->vdwForm_;
	typeId_ = source->typeId_;
	name_ = source->name_;
	equivalent_ = source->equivalent_;
	description_ = source->description_;
	for (int i=0; i<MAXFFPARAMDATA; i++) params_[i] = source->params_[i];
	//*generator_;
	charge_ = source->charge_;
}
