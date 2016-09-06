/*
	*** Forcefield bound term
	*** src/base/forcefieldbound.cpp
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

#include "base/forcefieldbound.h"
#include "math/constants.h"
#include <stdlib.h>
#include <stdio.h>

ATEN_USING_NAMESPACE

// Interaction types
const char* ForcefieldBoundKeywords[] = { "none", "bond", "angle", "torsion", "improper", "ub" };
int ForcefieldBoundNAtoms[] = { 0, 2, 3, 4, 4, 2 };
const char* ForcefieldBound::boundType(ForcefieldBound::BoundType bt)
{
	return ForcefieldBoundKeywords[bt];
}
int ForcefieldBound::boundTypeNAtoms(ForcefieldBound::BoundType bt)
{
	return ForcefieldBoundNAtoms[bt];
}

// Constructor
ForcefieldBound::ForcefieldBound() : ListItem<ForcefieldBound>()
{
	// Private variables
	type_ = NoInteraction;
	elecScale_ = 0.5;
	vdwScale_ = 0.5;
}

// Set the type of bound interaction
void ForcefieldBound::setType(BoundType fc)
{
	type_ = fc;
}

// Return the type of bound interaction
ForcefieldBound::BoundType ForcefieldBound::type() const
{
	return type_;
}
// Return the form text for the bound interaction
QString ForcefieldBound::formText() const
{
	switch (type_)
	{
		case (ForcefieldBound::BondInteraction):
		case (ForcefieldBound::UreyBradleyInteraction):
			return BondFunctions::functionData[bondForm()].keyword;
			break;
		case (ForcefieldBound::AngleInteraction):
			return AngleFunctions::functionData[angleForm()].keyword;
			break;
		case (ForcefieldBound::TorsionInteraction):
		case (ForcefieldBound::ImproperInteraction):
			return TorsionFunctions::functionData[torsionForm()].keyword;
			break;
		default:
			return "No Bound type defined.";
			break;
	}
}

// Return the functional form (cast as a bond style)
BondFunctions::BondFunction ForcefieldBound::bondForm() const
{
	return (BondFunctions::BondFunction) form_;
}

// Return the functional form (cast as a angle style)
AngleFunctions::AngleFunction ForcefieldBound::angleForm() const
{
	return (AngleFunctions::AngleFunction) form_;
}

// Return the functional form (cast as a torsion style)
TorsionFunctions::TorsionFunction ForcefieldBound::torsionForm() const
{
	return (TorsionFunctions::TorsionFunction) form_;
}

// Set the bond functional form
void ForcefieldBound::setBondForm(BondFunctions::BondFunction bf)
{
	form_ = bf;
	// Copy default parameters to structure
	for (int i=0; i<MAXFFPARAMDATA; i++) params_[i] = BondFunctions::functionData[bf].defaultValues[i];
}

// Set the angle functional form
void ForcefieldBound::setAngleForm(AngleFunctions::AngleFunction af)
{
	form_ = af;
	// Copy default parameters to structure
	for (int i=0; i<MAXFFPARAMDATA; i++) params_[i] = AngleFunctions::functionData[af].defaultValues[i];
}

// Set the torsion functional form
void ForcefieldBound::setTorsionForm(TorsionFunctions::TorsionFunction tf)
{
	form_ = tf;
	// Copy default parameters to structure
	for (int i=0; i<MAXFFPARAMDATA; i++) params_[i] = TorsionFunctions::functionData[tf].defaultValues[i];
}

// Set the functional form by name, without changing any existing parameters
bool ForcefieldBound::setForm(QString form)
{
	// Based on the current type of the ForcefieldBound, convert text to enum
	int newform = -1;
	switch (type_)
	{
		case (ForcefieldBound::BondInteraction):
		case (ForcefieldBound::UreyBradleyInteraction):
			newform = BondFunctions::bondFunction(form);
			if (newform == BondFunctions::nBondFunctions) Messenger::print("Unrecognised functional form (%s) for %s interaction.", qPrintable(form), ForcefieldBoundKeywords[type_]);
			else setBondForm((BondFunctions::BondFunction) newform);
			break;
		case (ForcefieldBound::AngleInteraction):
			newform = AngleFunctions::angleFunction(form);
			if (newform == AngleFunctions::nAngleFunctions) Messenger::print("Unrecognised functional form (%s) for %s interaction.", qPrintable(form), ForcefieldBoundKeywords[type_]);
			else setAngleForm((AngleFunctions::AngleFunction) newform);
			break;
		case (ForcefieldBound::TorsionInteraction):
		case (ForcefieldBound::ImproperInteraction):
			newform = TorsionFunctions::torsionFunction(form);
			if (newform == TorsionFunctions::nTorsionFunctions) Messenger::print("Unrecognised functional form (%s) for %s interaction.", qPrintable(form), ForcefieldBoundKeywords[type_]);
			else setTorsionForm((TorsionFunctions::TorsionFunction) newform);
			break;
		default:
			printf("Internal Error - No bound type defined in ForcefieldBound structure.\n");
			break;
	}
	return (newform != -1);
}

// Set the parameter data specified
void ForcefieldBound::setParameter(int i, double d)
{
	if ((i < 0) || (i >= MAXFFPARAMDATA)) printf("Data Id in ForcefieldAtom::setParameter (%i) is out of bounds.\n", i);
	else params_[i] = d;
}

// Return parameter data specified
double ForcefieldBound::parameter(int i) const
{
	if ((i < 0) || (i >= MAXFFPARAMDATA)) printf("Data Id in ForcefieldBound::parameter (%i) is out of bounds.\n", i);
	else return params_[i];
	return 0.0;
}

// Return parameter data specified, converted into specified units (if it is an energetic parameter)
double ForcefieldBound::convertedParameter(int i, Prefs::EnergyUnit units) const
{
	if ((i < 0) || (i >= MAXFFPARAMDATA))
	{
		printf("Data Id in ForcefieldBound::convertParameter (%i) is out of bounds.\n", i);
		return 0.0;
	}

	switch (type_)
	{
		case (ForcefieldBound::BondInteraction):
		case (ForcefieldBound::UreyBradleyInteraction):
			if (BondFunctions::functionData[form_].isEnergyParameter[i]) return prefs.convertEnergyTo(params_[i], units);
			break;
		case (ForcefieldBound::AngleInteraction):
			if (AngleFunctions::functionData[form_].isEnergyParameter[i]) return prefs.convertEnergyTo(params_[i], units);
			break;
		case (ForcefieldBound::TorsionInteraction):
		case (ForcefieldBound::ImproperInteraction):
			if (TorsionFunctions::functionData[form_].isEnergyParameter[i]) return prefs.convertEnergyTo(params_[i], units);
			break;
    default:
      break;  
	}
	return params_[i];
}

// Returns parameter array pointer
double* ForcefieldBound::parameters()
{
	return params_;
}

// Return the atom type 'n'
QString ForcefieldBound::typeName(int n) const
{
	return (n < MAXFFBOUNDTYPES ? typeNames_[n] : "OUTOFRANGE");
}

// Return the atom type array
QString* ForcefieldBound::typeNames()
{
	return &typeNames_[0];
}

// Set the atom type 'n'
void ForcefieldBound::setTypeName(int n, QString name)
{
	// Check range
	if ((n < 0) || (n > MAXFFBOUNDTYPES)) printf("setAtomType - index %i is out of range.\n",n);
	else typeNames_[n] = name;
}

// Set 1-4 scale factors
void ForcefieldBound::setScaleFactors(double escale, double vscale)
{
	elecScale_ = escale;
	vdwScale_ = vscale;
}

// Set electrostatic scale factor
void ForcefieldBound::setElecScale(double d)
{
	elecScale_ = d;
}

// Return electrostatic scale factor (if torsion)
double ForcefieldBound::elecScale() const
{
	return elecScale_;
}

// Set Vdw scale factor
void ForcefieldBound::setVdwScale(double d)
{
	vdwScale_ = d;
}

// Return VDW scale factor (if torsion)
double ForcefieldBound::vdwScale() const
{
	return vdwScale_;
}

// Return if supplied names match those stored (in either 'direction')
bool ForcefieldBound::namesMatch(QString namei, QString namej)
{
	// Bond interaction
	if ((typeNames_[0] == namei) && (typeNames_[1] == namej)) return true;
	else if ((typeNames_[0] == namej) && (typeNames_[1] == namei)) return true;
	return false;
}

// Return if supplied names match those stored (in either 'direction')
bool ForcefieldBound::namesMatch(QString namei, QString namej, QString namek)
{
	// Angle interaction
	if (typeNames_[1] != namej) return false;
	if ((typeNames_[0] == namei) && (typeNames_[2] == namek)) return true;
	else if ((typeNames_[0] == namek) && (typeNames_[2] == namei)) return true;
	return false;
}

// Return if supplied names match those stored (in either 'direction')
bool ForcefieldBound::namesMatch(QString namei, QString namej, QString namek, QString namel)
{
	// Torsion interaction
	if ((typeNames_[1] == namej) && (typeNames_[2] == namek))
	{
		if ((typeNames_[0] == namei) && (typeNames_[3] == namel)) return true;
		else return false;
	}
	else if ((typeNames_[2] == namej) && (typeNames_[1] == namek))
	{
		if ((typeNames_[3] == namei) && (typeNames_[0] == namel)) return true;
		else return false;
	}
	return false;
}
