/*
	*** Forcefield term functional forms
	*** src/ff/forms.cpp
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

#include <string.h>
#include "ff/forms.h"
#include "classes/prefs.h"
#include "base/sysfunc.h"
#include "base/messenger.h"

// Static Singleton
Forest Combine::combinationRules_;

// Combination rules
const char *CombinationRuleKeywords[Combine::nCombinationRules] = { "arithmetic", "geometric", "custom1", "custom2", "custom3" };
const char *CombinationRuleNames[Combine::nCombinationRules] = { "Arithmetic Mean [(a+b)/2]", "Geometric Mean [sqrt(a*b)]", "Custom Rule 1", "Custom Rule 2", "Custom Rule 3" };
Combine::CombinationRule Combine::combinationRule(const char *s, bool reporterror)
{
	Combine::CombinationRule cr = (Combine::CombinationRule) enumSearch("combination rule",Combine::nCombinationRules,CombinationRuleKeywords,s);
	if ((cr == Combine::nCombinationRules) && reporterror) enumPrintValid(Combine::nCombinationRules,CombinationRuleKeywords);
	return cr;
}
const char *Combine::combinationRule(CombinationRule cr)
{
	return CombinationRuleKeywords[cr];
}
const char *Combine::combinationRuleName(CombinationRule cr)
{
	return CombinationRuleNames[cr];
}

// Electrostatic model
const char *ElecMethodKeywords[Electrostatics::nElectrostatics] = { "none", "coulomb", "ewald", "ewaldauto" };
const char *Electrostatics::elecMethod(Electrostatics::ElecMethod i)
{
	return ElecMethodKeywords[i];
}
Electrostatics::ElecMethod Electrostatics::elecMethod(const char *s, bool reporterror)
{
	Electrostatics::ElecMethod em = (Electrostatics::ElecMethod) enumSearch("electrostatics method", Electrostatics::nElectrostatics, ElecMethodKeywords,s);
	if ((em == Electrostatics::nElectrostatics) && reporterror) enumPrintValid(Electrostatics::nElectrostatics,ElecMethodKeywords);
	return em;
}

// Regenerate combination rule function trees
bool Combine::regenerateEquations()
{
	msg.enter("Combine::regenerateEquations");
	Combine::CombinationRule cr;
	List<Dnchar> eqns;
	Dnchar *eqn;
	for (int n=0; n<Combine::nCombinationRules; ++n)
	{
		cr = (Combine::CombinationRule) n;
		eqn = eqns.add();
		eqn->print("double %s(double a, double b) { double c = 0.0; %s; return c; }", Combine::combinationRule(cr), prefs.combinationRule(cr));
	}
	bool success = combinationRules_.generateFromStringList(eqns.first(), "CombinationRules", TRUE);
	msg.exit("Combine::regenerateEquations");
	return success;
}

// Execute combination rule with parameters specified
double Combine::combine(Combine::CombinationRule cr, double a, double b)
{
	msg.enter("Combine::combineParameters");
	ReturnValue rv;
	if (!combinationRules_.executeGlobalFunction(Combine::combinationRule(cr), rv, "dd", a, b))
	{
		printf("Internal Error: Couldn't find function corresponding to combination rule.\n");
		msg.exit("Forcefield::combineParameters");
		return 0.0;
	}
	msg.exit("Combine::combineParameters");
	return rv.asDouble();
}

// VDW potential forms
FunctionData VdwFunctions::VdwFunctions[VdwFunctions::nVdwFunctions] = {
	{ "None", "none", 0,
		{ "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL" },
		{ "null", "null", "null", "null", "null", "null", "null", "null", "null", "null" },
		{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ "Inverse Power", "inversepower", 3,
		{ "Epsilon", "Radius", "Power" },
		{ "epsilon", "r", "n" },
		{ 1, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 1.0, 0.0, 0.0, 0.0 },
		{ Combine::GeometricRule, Combine::ArithmeticRule, Combine::ArithmeticRule } },
	{ "Lennard-Jones 12-6", "lj", 3,
		{ "Epsilon", "Sigma", "N" },
		{ "epsilon", "sigma", "n" },
		{ 1, 0, 0, 0, 0, 0 }, { 0.0, 0.0, 1.0, 0.0, 0.0, 0.0 },
		{ Combine::GeometricRule, Combine::ArithmeticRule, Combine::ArithmeticRule } },
	{ "Lennard-Jones AB", "ljab", 2,
		{ "A", "B" },
		{ "a", "b" },
		{ 1, 1, 0, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },
		{ Combine::GeometricRule, Combine::GeometricRule } },
	{ "Buckingham", "buck", 3,
		{ "A", "B", "C" },
		{ "a", "b", "c" },
		{ 1, 0, 1, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },
		{ Combine::GeometricRule, Combine::GeometricRule, Combine::GeometricRule } },
	{ "Morse", "morse", 3,
		{ "K", "Eq. Dist", "D" },
		{ "k", "eq", "d" },
		{ 1, 0, 1, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },
		{ Combine::GeometricRule, Combine::ArithmeticRule, Combine::GeometricRule } }
};
VdwFunctions::VdwFunction VdwFunctions::vdwFunction(const char *s, bool reporterror)
{
	int i;
	for (i=0; i < VdwFunctions::nVdwFunctions; i++)
		if (strcmp(VdwFunctions::VdwFunctions[i].keyword,s) == 0) break;
	if ((i == VdwFunctions::nVdwFunctions) && reporterror)
	{
		msg.print("Invalid VDW functional form '%s'.\n", s);
		printValid();
	}
	return (VdwFunctions::VdwFunction) i;
}
void VdwFunctions::printValid()
{
	msg.print("Valid forms are:\n   ");
	for (int i=1; i< VdwFunctions::nVdwFunctions; i++) msg.print("%s ",VdwFunctions::VdwFunctions[i].keyword);
	msg.print("\n");
}

// Bond potential forms
FunctionData BondFunctions::BondFunctions[BondFunctions::nBondFunctions] = {
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
		{ "Force K", "Eq. Distance", "E(Diss.)" },
		{ "k", "eq", "d" },
		{ 1, 0, 1, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } },
	{ "Morse2", "morse2", 3,
		{ "Force K", "Eq. Distance", "E(Diss.)" },
		{ "k", "eq", "d" },
		{ 1, 0, 1, 0, 0, 0 }, { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 } }
};
BondFunctions::BondFunction BondFunctions::bondFunction(const char *s, bool reporterror)
{
	int i;
	for (i=0; i < BondFunctions::nBondFunctions; i++)
		if (strcmp(BondFunctions::BondFunctions[i].keyword,s) == 0) break;
	if ((i == BondFunctions::nBondFunctions) && reporterror)
	{
		msg.print("Invalid bond functional form '%s'.\n", s);
		printValid();
	}
	return (BondFunctions::BondFunction) i;
}
void BondFunctions::printValid()
{
	msg.print("Valid forms are:\n   ");
	for (int i=1; i< BondFunctions::nBondFunctions; i++) msg.print("%s ",BondFunctions::BondFunctions[i].keyword);
	msg.print("\n");
}

// Angle potential forms
FunctionData AngleFunctions::AngleFunctions[AngleFunctions::nAngleFunctions] = {
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
AngleFunctions::AngleFunction AngleFunctions::angleFunction(const char *s, bool reporterror)
{
	int i;
	for (i=0; i < AngleFunctions::nAngleFunctions; i++)
		if (strcmp(AngleFunctions::AngleFunctions[i].keyword,s) == 0) break;
	if ((i == AngleFunctions::nAngleFunctions) && reporterror)
	{
		msg.print("Invalid angle functional form '%s'.\n", s);
		printValid();
	}
	return (AngleFunctions::AngleFunction) i;
}
void AngleFunctions::printValid()
{
	msg.print("Valid forms are:\n   ");
	for (int i=1; i< AngleFunctions::nAngleFunctions; i++) msg.print("%s ",AngleFunctions::AngleFunctions[i].keyword);
	msg.print("\n");
}

// Torsion potential forms
FunctionData TorsionFunctions::TorsionFunctions[TorsionFunctions::nTorsionFunctions] = {
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
TorsionFunctions::TorsionFunction TorsionFunctions::torsionFunction(const char *s, bool reporterror)
{
	int i;
	for (i=0; i < TorsionFunctions::nTorsionFunctions; i++)
		if (strcmp(TorsionFunctions::TorsionFunctions[i].keyword,s) == 0) break;
	if ((i ==  TorsionFunctions::nTorsionFunctions) && reporterror)
	{
		msg.print("Invalid torsion functional form '%s'.\n", s);
		printValid();
	}
	return (TorsionFunctions::TorsionFunction) i;
}
void TorsionFunctions::printValid()
{
	msg.print("Valid forms are:\n   ");
	for (int i=1; i< TorsionFunctions::nTorsionFunctions; i++) msg.print("%s ",TorsionFunctions::TorsionFunctions[i].keyword);
	msg.print("\n");
}

// Generation rules (for rule-based FFs)

const char *ForcefieldRulesStrings[Rules::nForcefieldRules] = { "None", "UFF", "Dreiding", "Dreiding/X6" };
const char *ForcefieldRulesKeywords[Rules::nForcefieldRules] = { "none", "uff", "dreiding", "dreidingx6" };
const char *Rules::forcefieldRules(Rules::ForcefieldRules i)
{
	return ForcefieldRulesStrings[i];
}
Rules::ForcefieldRules Rules::forcefieldRules(const char *s)
{
	return (Rules::ForcefieldRules) enumSearch("forcefield rules", Rules::nForcefieldRules, ForcefieldRulesKeywords, s);
}
