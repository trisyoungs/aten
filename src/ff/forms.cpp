/*
	*** Forcefield term functional forms
	*** src/ff/forms.cpp
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

#include "ff/forms.h"
#include "base/sysfunc.h"
#include "base/messenger.h"

ATEN_USING_NAMESPACE

// Electrostatic model
const char* ElecMethodKeywords[Electrostatics::nElectrostatics] = { "none", "coulomb", "ewald", "ewaldauto" };
const char* Electrostatics::elecMethod(Electrostatics::ElecMethod em)
{
	return ElecMethodKeywords[em];
}
Electrostatics::ElecMethod Electrostatics::elecMethod(QString s, bool reportError)
{
	Electrostatics::ElecMethod em = (Electrostatics::ElecMethod) enumSearch("electrostatics method", Electrostatics::nElectrostatics, ElecMethodKeywords,s);
	if ((em == Electrostatics::nElectrostatics) && reportError) enumPrintValid(Electrostatics::nElectrostatics,ElecMethodKeywords);
	return em;
}

// VDW potential forms
FunctionData VdwFunctions::functionData[VdwFunctions::nVdwFunctions] = {
	{ "None", "none", 0,
		{ "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL" },
		{ "null", "null", "null", "null", "null", "null", "null", "null", "null", "null" },
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ "Inverse Power", "inversepower", 3,
		{ "Epsilon", "Radius", "Power" },
		{ "epsilon", "r", "n" },
		{ 1, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 1.0, 0.0, 0.0, 0.0 },
		{ CombinationRules::GeometricRule, CombinationRules::ArithmeticRule, CombinationRules::ArithmeticRule } },
	{ "Lennard-Jones 12-6", "lj", 2,
		{ "Epsilon", "Sigma" },
		{ "epsilon", "sigma" },
		{ 1, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },
		{ CombinationRules::GeometricRule, CombinationRules::ArithmeticRule, CombinationRules::ArithmeticRule } },
	{ "Lennard-Jones 12-6 (geometric rules)", "ljgeom", 2,
		{ "Epsilon", "Sigma" },
		{ "epsilon", "sigma" },
		{ 1, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },
		{ CombinationRules::GeometricRule, CombinationRules::GeometricRule, CombinationRules::ArithmeticRule } },
	{ "Lennard-Jones AB", "ljab", 2,
		{ "A", "B" },
		{ "a", "b" },
		{ 1, 1, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },
		{ CombinationRules::GeometricRule, CombinationRules::GeometricRule } },
	{ "Buckingham", "buck", 3,
		{ "A", "B", "C" },
		{ "a", "b", "c" },
		{ 1, 0, 1, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },
		{ CombinationRules::GeometricRule, CombinationRules::GeometricRule, CombinationRules::GeometricRule } },
	{ "Morse", "morse", 3,
		{ "D", "K", "Eq. Dist" },
		{ "d", "k", "eq" },
		{ 1, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },
		{ CombinationRules::GeometricRule, CombinationRules::ArithmeticRule, CombinationRules::GeometricRule } }
};
VdwFunctions::VdwFunction VdwFunctions::vdwFunction(QString s, bool reportError)
{
	int i;
	for (i=0; i < VdwFunctions::nVdwFunctions; i++) if (s == VdwFunctions::functionData[i].keyword) break;
	if ((i == VdwFunctions::nVdwFunctions) && reportError)
	{
		Messenger::print("Invalid VDW functional form '%s'.", qPrintable(s));
		printValid();
	}
	return (VdwFunctions::VdwFunction) i;
}
int VdwFunctions::vdwParameter(VdwFunction form, QString s, bool reportError)
{
	int i;
	for (i=0; i < VdwFunctions::functionData[form].nParameters; i++) if (s == VdwFunctions::functionData[form].parameterKeywords[i]) break;
	if ((i == VdwFunctions::functionData[form].nParameters) && reportError)
	{
		Messenger::print("Invalid parameter '%s' for VDW functional form '%s'.", qPrintable(s), VdwFunctions::functionData[form].name);
		Messenger::print("Valid parameters are:");
		for (int n=0; n< VdwFunctions::functionData[form].nParameters; n++) Messenger::print("%s ", VdwFunctions::functionData[form].parameterKeywords[n]);
		Messenger::print("");
	}
	return i;
}
void VdwFunctions::printValid()
{
	Messenger::print("Valid forms are:");
	for (int i=1; i< VdwFunctions::nVdwFunctions; i++) Messenger::print("%s ", VdwFunctions::functionData[i].keyword);
	Messenger::print("");
}

// Bond potential forms
FunctionData BondFunctions::functionData[BondFunctions::nBondFunctions] = {
	{ "None", "none", 0,
		{ "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL" },
		{ "null", "null", "null", "null", "null", "null", "null", "null", "null", "null" },
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ "Ignore", "ignore", 0,
		{ "NULL", "NULL", "NULL", "NULL", "NULL", "NULL" },
		{ "null", "null", "null", "null", "null", "null" },
		{ 0, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ "Constraint", "constraint", 2,
		{ "Force K", "Eq. Distance" },
		{ "k", "eq" },
		{ 1, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ "Harmonic", "harmonic", 2,
		{ "Force K", "Eq. Distance" },
		{ "k", "eq" },
		{ 1, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ "Morse", "morse", 3,
		{ "D", "K", "Eq. Distance" },
		{ "d", "k", "eq" },
		{ 1, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } }
};
BondFunctions::BondFunction BondFunctions::bondFunction(QString s, bool reportError)
{
	int i;
	for (i=0; i < BondFunctions::nBondFunctions; ++i) if (s == BondFunctions::functionData[i].keyword) break;
	if ((i == BondFunctions::nBondFunctions) && reportError)
	{
		Messenger::print("Invalid bond functional form '%s'.", qPrintable(s));
		printValid();
	}
	return (BondFunctions::BondFunction) i;
}
int BondFunctions::bondParameter(BondFunction form, QString s, bool reportError)
{
	int i;
	for (i=0; i < BondFunctions::functionData[form].nParameters; i++) if (s == BondFunctions::functionData[form].parameterKeywords[i]) break;
	if ((i == BondFunctions::functionData[form].nParameters) && reportError)
	{
		Messenger::print("Invalid parameter '%s' for bond functional form '%s'.", qPrintable(s), BondFunctions::functionData[form].name);
		Messenger::print("Valid parameters are:");
		for (int n=0; n< BondFunctions::functionData[form].nParameters; n++) Messenger::print("%s ", BondFunctions::functionData[form].parameterKeywords[n]);
		Messenger::print("");
	}
	return i;
}
void BondFunctions::printValid()
{
	Messenger::print("Valid forms are:");
	for (int i=1; i< BondFunctions::nBondFunctions; i++) Messenger::print("%s ", BondFunctions::functionData[i].keyword);
	Messenger::print("");
}

// Angle potential forms
FunctionData AngleFunctions::functionData[AngleFunctions::nAngleFunctions] = {
	{ "None", "none", 0,
		{ "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL" },
		{ "null", "null", "null", "null", "null", "null", "null", "null", "null", "null" },
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ "Ignore", "ignore", 0,
		{ "NULL", "NULL", "NULL", "NULL", "NULL", "NULL" },
		{ "null", "null", "null", "null", "null", "null" },
		{ 0, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ "Harmonic", "harmonic", 2,
		{ "Force K", "Eq. Angle" },
		{ "k", "eq" },
		{ 1, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ "Cosine", "cos", 4,
		{ "Force K", "Periodicity", "Eq. Angle", "Sign" },
		{ "k", "n", "eq", "s" },
		{ 1, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 1.0, 0.0, 0.0 } },
	{ "Cosine 2-Term", "cos2", 4,
		{ "Force K", "Coeff. 0", "Coeff. 1", " Coeff. 2" },
		{ "k", "c0", "c1", "c2" },
		{ 1, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ "Harmonic Cosine", "harmcos", 2,
		{ "Force K", "Eq. Angle" },
		{ "k", "eq" },
		{ 1, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ "Constraint (1-3 Bond)", "bondconstraint", 2,
		{ "Force K", "Eq. Distance" },
		{ "k", "eq" },
		{ 1, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } }
};
AngleFunctions::AngleFunction AngleFunctions::angleFunction(QString s, bool reportError)
{
	int i;
	for (i=0; i < AngleFunctions::nAngleFunctions; i++) if (s == AngleFunctions::functionData[i].keyword) break;
	if ((i == AngleFunctions::nAngleFunctions) && reportError)
	{
		Messenger::print("Invalid angle functional form '%s'.", qPrintable(s));
		printValid();
	}
	return (AngleFunctions::AngleFunction) i;
}
int AngleFunctions::angleParameter(AngleFunction form, QString s, bool reportError)
{
	int i;
	for (i=0; i < AngleFunctions::functionData[form].nParameters; i++) if (s == AngleFunctions::functionData[form].parameterKeywords[i]) break;
	if ((i == AngleFunctions::functionData[form].nParameters) && reportError)
	{
		Messenger::print("Invalid parameter '%s' for bond functional form '%s'.", qPrintable(s), AngleFunctions::functionData[form].name);
		Messenger::print("Valid parameters are:");
		for (int n=0; n< AngleFunctions::functionData[form].nParameters; n++) Messenger::print("%s ", AngleFunctions::functionData[form].parameterKeywords[n]);
		Messenger::print("");
	}
	return i;
}
void AngleFunctions::printValid()
{
	Messenger::print("Valid forms are:");
	for (int i=1; i< AngleFunctions::nAngleFunctions; i++) Messenger::print("%s ", AngleFunctions::functionData[i].keyword);
	Messenger::print("");
}

// Torsion potential forms
FunctionData TorsionFunctions::functionData[TorsionFunctions::nTorsionFunctions] = {
	{ "None", "none", 0,
		{ "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL" },
		{ "null", "null", "null", "null", "null", "null", "null", "null", "null", "null" },
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ "Ignore", "ignore", 0,
		{ "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL" },
		{ "null", "null", "null", "null", "null", "null", "null", "null", "null", "null" },
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ "Cosine", "cos", 4,
		{ "Force K", "Periodicity", "Eq. Angle", "Sign" },
		{ "k", "n", "eq", "s" },
		{ 1, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 1.0 } },
	{ "Triple Cosine", "cos3", 3,
		{ "Force K1", "Force K2", "Force K3" },
		{ "k1", "k2", "k3" },
		{ 1, 1, 1, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0 } },
	{ "Quadruple Cosine", "cos4", 4,
		{ "Force K1", "Force K2", "Force K3", "Force K4" },
		{ "k1", "k2", "k3", "k4" },
		{ 1, 1, 1, 1, 0, 0 }, { 0.0, 0.0, 0.0, 0.0 } },
	{ "Triple Cosine + Constant", "cos3c", 4,
		{ "Force K0", "Force K1", "Force K2", "Force K3" },
		{ "k0", "k1", "k2", "k3" },
		{ 1, 1, 1, 1, 0, 0 }, { 0.0, 0.0, 0.0, 0.0 } },
	{ "Cosine Product", "coscos", 3,
		{ "Force K", "Periodicity", "Eq. Angle" },
		{ "k", "n", "eq" },
		{ 1, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0 } },
	{ "Dreiding Cosine", "dreiding", 3,
		{ "Force K", "Periodicity", "Eq. Angle" },
		{ "k", "n", "eq" },
		{ 1, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0 } },
	{ "Pol9", "pol9", 9,
		{ "K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "K9" },
		{ "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9" },
		{ 1, 1, 1, 1, 1, 1, 1, 1, 1 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } }
};
TorsionFunctions::TorsionFunction TorsionFunctions::torsionFunction(QString s, bool reportError)
{
	int i;
	for (i=0; i < TorsionFunctions::nTorsionFunctions; i++) if (s == TorsionFunctions::functionData[i].keyword) break;
	if ((i == TorsionFunctions::nTorsionFunctions) && reportError)
	{
		Messenger::print("Invalid torsion functional form '%s'.", qPrintable(s));
		printValid();
	}
	return (TorsionFunctions::TorsionFunction) i;
}
int TorsionFunctions::torsionParameter(TorsionFunction form, QString s, bool reportError)
{
	int i;
	for (i=0; i < TorsionFunctions::functionData[form].nParameters; i++) if (s == TorsionFunctions::functionData[form].parameterKeywords[i]) break;
	if ((i == TorsionFunctions::functionData[form].nParameters) && reportError)
	{
		Messenger::print("Invalid parameter '%s' for bond functional form '%s'.", qPrintable(s), TorsionFunctions::functionData[form].name);
		Messenger::print("Valid parameters are:");
		for (int n=0; n< TorsionFunctions::functionData[form].nParameters; n++) Messenger::print("%s ", TorsionFunctions::functionData[form].parameterKeywords[n]);
		Messenger::print("");
	}
	return i;
}
void TorsionFunctions::printValid()
{
	Messenger::print("Valid forms are:");
	for (int i=1; i< TorsionFunctions::nTorsionFunctions; i++) Messenger::print("%s ", TorsionFunctions::functionData[i].keyword);
	Messenger::print("");
}
