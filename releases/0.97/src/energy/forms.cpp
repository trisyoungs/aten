/*
	*** Forcefield term functional forms
	*** src/energy/forms.cpp
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

#include "energy/forms.h"

// Generation rules (for rule-based FFs)
const char *ForcefieldRulesStrings[Forms::nForcefieldRules] = { "No rules defined.", "UniversalFF (Rappe et al.)" };
const char *ForcefieldRulesKeywords[Forms::nForcefieldRules] = { "none", "uff" };
const char *Forms::forcefieldRules(Forms::ForcefieldRules i)
{
	return ForcefieldRulesStrings[i];
}
Forms::ForcefieldRules Forms::forcefieldRules(const char *s)
{
	return (Forms::ForcefieldRules) enumSearch("forcefield rules", Forms::nForcefieldRules, ForcefieldRulesKeywords, s);
}

// Electrostatic model
const char *ElecMethodKeywords[Forms::nElecMethods] = { "none", "coulomb", "ewald", "ewaldauto" };
const char *Forms::elecMethod(Forms::ElecMethod i)
{
	return ElecMethodKeywords[i];
}
Forms::ElecMethod Forms::elecMethod(const char *s)
{
	return (ElecMethod) enumSearch("electrostatics method", Forms::nElecMethods, ElecMethodKeywords,s);
}

// VDW potential forms
const char *VdwFunctionStrings[Forms::nVdwForms] = { "Unspecified", "Lennard-Jones 12-6", "Buckingham exp6" };
const char *VdwFunctionKeywords[Forms::nVdwForms] = { "none", "lj", "buck" };
const char *Forms::vdwFunctionString(Forms::VdwFunction i)
{
	return VdwFunctionStrings[i];
}
const char *Forms::vdwFunction(Forms::VdwFunction i)
{
	return VdwFunctionKeywords[i];
}
Forms::VdwFunction Forms::vdwFunction(const char *s)
{
	return (Forms::VdwFunction) enumSearch("VDW style", Forms::nVdwForms, VdwFunctionKeywords, s);
}

// Bond potential forms
const char *BondFunctionStrings[Forms::nBondFunctions] = { "Unspecified", "Harmonic", "Morse" };
const char *BondFunctionKeywords[Forms::nBondFunctions] = { "none", "harmonic", "morse" };
const char *Forms::bondFunction(Forms::BondFunction i)
{
	return BondFunctionKeywords[i];
}
Forms::BondFunction Forms::bondFunction(const char *s)
{
	return (Forms::BondFunction) enumSearch("bond style", Forms::nBondFunctions, BondFunctionKeywords, s);
}

// Angle potential forms
const char *AngleFunctionStrings[Forms::nAngleFunctions] = { "Unspecified", "Harmonic", "Cosine", "UFF Cosine" };
const char *AngleFunctionKeywords[Forms::nAngleFunctions] = { "none", "harmonic", "cos", "uff" };
const char *Forms::angleFunction(Forms::AngleFunction i)
{
	return AngleFunctionKeywords[i];
}
Forms::AngleFunction Forms::angleFunction(const char *s)
{
	return (Forms::AngleFunction) enumSearch("angle style", Forms::nAngleFunctions, AngleFunctionKeywords, s);
}

// Torsion potential forms
const char *TorsionFunctionStrings[Forms::nTorsionFunctions] = { "Unspecified", "Cosine", "Triple Cosine", "Quadruple Cosine", "Constant + Triple Cosine" };
const char *TorsionFunctionKeywords[Forms::nTorsionFunctions] = { "none", "cosine", "cos3", "cos4", "cosc" };
const char *Forms::torsionFunction(Forms::TorsionFunction i)
{
	return TorsionFunctionKeywords[i];
}
Forms::TorsionFunction Forms::torsionFunction(const char *s)
{
	return (Forms::TorsionFunction) enumSearch("torsion style", Forms::nTorsionFunctions, TorsionFunctionKeywords, s);
}
