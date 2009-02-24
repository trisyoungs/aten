/*
	*** Forcefield atom (type)
	*** src/classes/forcefieldatom.cpp
	Copyright T. Youngs 2007-2009

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

#include "classes/forcefieldatom.h"

// Constructor
ForcefieldAtom::ForcefieldAtom()
{
	// Private variables
	name_.set("Unnamed");
	typeId_ = -1;
	charge_ = 0.0;
	vdwForm_ = VdwFunctions::None;
	generator_ = NULL;
	parent_ = NULL;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Destructor
ForcefieldAtom::~ForcefieldAtom()
{
	if (generator_ != NULL) delete[] generator_;
}

// Set parent forcefield
void ForcefieldAtom::setParent(Forcefield *ff)
{
	parent_ = ff;
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
VdwFunctions::VdwFunction ForcefieldAtom::vdwForm()
{
	return vdwForm_;
}

// Set the type id
void ForcefieldAtom::setTypeId(int i)
{
	typeId_ = i;
}

// Returns the type id
int ForcefieldAtom::typeId()
{
	return typeId_;
}

// Set the charge of the type
void ForcefieldAtom::setCharge(double q)
{
	charge_ = q;
}

// Returns the charge of the type
double ForcefieldAtom::charge()
{
	return charge_;
}

// Set the name of the type
void ForcefieldAtom::setName(const char *s)
{
	name_ = s;
}

// Returns the name of the type
const char *ForcefieldAtom::name()
{
	return name_.get();
}

// Set the equivalent name of the type
void ForcefieldAtom::setEquivalent(const char *s)
{
	equivalent_ = s;
}

// Returns the equivalent name of the type
const char *ForcefieldAtom::equivalent()
{
	return equivalent_.get();
}

// Set the description of the type
void ForcefieldAtom::setDescription(const char *s)
{
	description_ = s;
}

// Returns the description of the type
const char *ForcefieldAtom::description()
{
	return description_.get();
}

// Set atomtype string and generate new type description
bool ForcefieldAtom::setAtomtype(const char *s, Forcefield *parent, ForcefieldAtom *root)
{
	atomtypeString_ = s;
	return atomtype_.expand(s, parent, root);
}

// Return original typestring
const char *ForcefieldAtom::atomtypeString()
{
	return atomtypeString_.get();
}

// Returns the atomtype description
Atomtype *ForcefieldAtom::atomtype()
{
	return &atomtype_;
}

// Set the parameter data specified
void ForcefieldAtom::setParameter(int i, double d)
{
	if ((i < 0) || (i >= MAXFFPARAMDATA)) printf("Data Id in ForcefieldAtom::setParameter (%i) is out of bounds.\n", i);
	else params_[i] = d;
}

// Return parameter data specified
double ForcefieldAtom::parameter(int i)
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

// Initialise generator array
void ForcefieldAtom::initialiseGenerator()
{
	if (generator_ != NULL) msg.print("Warning - replacing existing generator data for typeId %i (%s)\n", typeId_, name_.get());
	generator_ = new double[MAXFFGENDATA];
}

// Set generator data
void ForcefieldAtom::setGenerator(int i, double d)
{
	// Check the limit of the position provided
	if ((i < 0) || (i > MAXFFGENDATA)) printf("setGenerator() - index %i is out of range.\n", i);
	else generator_[i] = d;
}

// Return generator data structure
double *ForcefieldAtom::generator()
{
	return generator_;
}

// Return single generator value
double ForcefieldAtom::generator(int i)
{
	// Check the limit of the position provided
	if ((i < 0) || (i > MAXFFGENDATA)) printf("generator() - index %i is out of range.\n", i);
	else return generator_[i];
	return 0.0;
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
