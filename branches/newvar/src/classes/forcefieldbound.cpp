/*
	*** Forcefield bound term
	*** src/classes/forcefieldbound.cpp
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

#include "classes/forcefieldbound.h"
#include "base/constants.h"
// #include "base/forms.h"
#include <stdlib.h>
#include <stdio.h>

// Constructor
ForcefieldBound::ForcefieldBound()
{
	// Private variables
	type_ = NoInteraction;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Set the type of bound interaction
void ForcefieldBound::setType(BoundType fc)
{
	type_ = fc;
}

// Return the type of bound interaction
ForcefieldBound::BoundType ForcefieldBound::type()
{
	return type_;
}

// Return the functional form (cast as a bond style)
BondFunctions::BondFunction ForcefieldBound::bondStyle()
{
	return (BondFunctions::BondFunction) functionalForm_;
}

// Return the functional form (cast as a angle style)
AngleFunctions::AngleFunction ForcefieldBound::angleStyle()
{
	return (AngleFunctions::AngleFunction) functionalForm_;
}

// Return the functional form (cast as a torsion style)
TorsionFunctions::TorsionFunction ForcefieldBound::torsionStyle()
{
	return (TorsionFunctions::TorsionFunction) functionalForm_;
}

// Set the bond functional form
void ForcefieldBound::setBondStyle(BondFunctions::BondFunction bf)
{
	functionalForm_ = bf;
	// Copy default parameters to structure
	for (int i=0; i<MAXFFPARAMDATA; i++) params_.data[i] = BondFunctions::BondFunctions[bf].defaultValues[i];
}

// Set the angle functional form
void ForcefieldBound::setAngleStyle(AngleFunctions::AngleFunction af)
{
	functionalForm_ = af;
	// Copy default parameters to structure
	for (int i=0; i<MAXFFPARAMDATA; i++) params_.data[i] = AngleFunctions::AngleFunctions[af].defaultValues[i];
}

// Set the torsion functional form
void ForcefieldBound::setTorsionStyle(TorsionFunctions::TorsionFunction tf)
{
	functionalForm_ = tf;
	// Copy default parameters to structure
	for (int i=0; i<MAXFFPARAMDATA; i++) params_.data[i] = TorsionFunctions::TorsionFunctions[tf].defaultValues[i];
}

// Return the data[] array in *params
ForcefieldParams &ForcefieldBound::params()
{
	return params_; 
}

// Return the atom type 'n'
const char *ForcefieldBound::typeName(int n)
{
	return (n < MAXFFBOUNDTYPES ? typeNames_[n].get() : "OUTOFRANGE");
}

// Set the atom type 'n'
void ForcefieldBound::setTypeName(int n, const char *s)
{
	// Check range
	if ((n < 0) || (n > MAXFFBOUNDTYPES)) printf("setAtomType - index %i is out of range.\n",n);
	else typeNames_[n] = s;
}
