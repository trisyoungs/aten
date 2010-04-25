/*
	*** Forcefield bound term
	*** src/classes/forcefieldbound.cpp
	Copyright T. Youngs 2007-2010

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
#include <stdlib.h>
#include <stdio.h>

// Interaction types
const char *ForcefieldBoundKeywords[] = { "none", "bond", "angle", "torsion", "improper" };
int ForcefieldBoundNAtoms[] = { 0, 2, 3, 4, 4 };
const char *ForcefieldBound::boundType(ForcefieldBound::BoundType bt)
{
	return ForcefieldBoundKeywords[bt];
}
int ForcefieldBound::boundTypeNAtoms(ForcefieldBound::BoundType bt)
{
	return ForcefieldBoundNAtoms[bt];
}

// Constructor
ForcefieldBound::ForcefieldBound()
{
	// Private variables
	type_ = NoInteraction;
	elecScale_ = 0.5;
	vdwScale_ = 0.5;

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
// Return the form text for the bound interaction
const char *ForcefieldBound::formText()
{
	switch (type_)
	{
		case (ForcefieldBound::BondInteraction):
			return BondFunctions::BondFunctions[bondStyle()].keyword;
			break;
		case (ForcefieldBound::AngleInteraction):
			return AngleFunctions::AngleFunctions[angleStyle()].keyword;
			break;
		case (ForcefieldBound::TorsionInteraction):
		case (ForcefieldBound::ImproperInteraction):
			return TorsionFunctions::TorsionFunctions[torsionStyle()].keyword;
			break;
		default:
			return "No Bound type defined.";
			break;
	}
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
	for (int i=0; i<MAXFFPARAMDATA; i++) params_[i] = BondFunctions::BondFunctions[bf].defaultValues[i];
}

// Set the angle functional form
void ForcefieldBound::setAngleStyle(AngleFunctions::AngleFunction af)
{
	functionalForm_ = af;
	// Copy default parameters to structure
	for (int i=0; i<MAXFFPARAMDATA; i++) params_[i] = AngleFunctions::AngleFunctions[af].defaultValues[i];
}

// Set the torsion functional form
void ForcefieldBound::setTorsionStyle(TorsionFunctions::TorsionFunction tf)
{
	functionalForm_ = tf;
	// Copy default parameters to structure
	for (int i=0; i<MAXFFPARAMDATA; i++) params_[i] = TorsionFunctions::TorsionFunctions[tf].defaultValues[i];
}

// Set the parameter data specified
void ForcefieldBound::setParameter(int i, double d)
{
	if ((i < 0) || (i >= MAXFFPARAMDATA)) printf("Data Id in ForcefieldAtom::setParameter (%i) is out of bounds.\n", i);
	else params_[i] = d;
}

// Return parameter data specified
double ForcefieldBound::parameter(int i)
{
	if ((i < 0) || (i >= MAXFFPARAMDATA)) printf("Data Id in ForcefieldAtom::parameter (%i) is out of bounds.\n", i);
	else return params_[i];
	return 0.0;
}

// Returns parameter array pointer
double *ForcefieldBound::parameters()
{
	return params_;
}

// Return the atom type 'n'
const char *ForcefieldBound::typeName(int n)
{
	return (n < MAXFFBOUNDTYPES ? typeNames_[n].get() : "OUTOFRANGE");
}

// Return the atom type array
Dnchar *ForcefieldBound::typeNames()
{
	return &typeNames_[0];
}

// Set the atom type 'n'
void ForcefieldBound::setTypeName(int n, const char *s)
{
	// Check range
	if ((n < 0) || (n > MAXFFBOUNDTYPES)) printf("setAtomType - index %i is out of range.\n",n);
	else typeNames_[n] = s;
}

// Set 1-4 scale factors
void ForcefieldBound::setScaleFactors(double escale, double vscale)
{
	elecScale_ = escale;
	vdwScale_ = vscale;
}

// Return electrostatic scale factor (if torsion)
double ForcefieldBound::elecScale()
{
	return elecScale_;
}

// Return VDW scale factor (if torsion)
double ForcefieldBound::vdwScale()
{
	return vdwScale_;
}
