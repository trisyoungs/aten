/*
	*** Forcefield term functional forms
	*** src/energy/forms.h
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

#ifndef ATEN_FORMS_H
#define ATEN_FORMS_H

#include "base/sysfunc.h"

namespace Forms
{
	// Generation rules (for rule-based FFs)
	enum ForcefieldRules { NoRules, UffRules, nForcefieldRules };
	const char *forcefieldRules(ForcefieldRules);
	ForcefieldRules forcefieldRules(const char*);
	
	// Electrostatic model
	enum ElecMethod { NoElec, CoulombElec, EwaldElec, EwaldAutoElec, nElecMethods };
	const char *elecMethod(ElecMethod);
	ElecMethod elecMethod(const char*);
	
	// VDW potential forms
	enum VdwFunction { NoVdw, LjVdw, BuckinghamVdw, nVdwForms };
	const char *vdwFunctionString(VdwFunction);
	const char *vdwFunction(VdwFunction);
	VdwFunction vdwFunction(const char*);
	enum LjVdwParameters { LjVdwEpsilon, LjVdwSigma };
	enum BuckVdwParameters { BuckVdwA, BuckVdwB, BuckVdwC };
	
	// Bond potential forms
	enum BondFunction { NoBond, HarmonicBond, MorseBond, nBondFunctions };
	const char *bondFunction(BondFunction);
	BondFunction bondFunction(const char*);
	enum HarmonicBondParameters { HarmonicBondK, HarmonicBondEq };
	
	// Angle potential forms
	enum AngleFunction { NoAngle, HarmonicAngle, CosineAngle, UffCosine1Angle, UffCosine2Angle, nAngleFunctions };
	const char *angleFunction(AngleFunction);
	AngleFunction angleFunction(const char*);
	enum HarmonicAngleParameters { HarmonicAngleK, HarmonicAngleEq };
	enum CosineAngleParameters { CosineAngleK, CosineAngleS, CosineAngleEq };
	enum UffCosineAngleParameters { UffCosineAngleK, UffCosineAngleN, UffCosineAngleEq };
	
	// Torsion potential forms
	enum TorsionFunction { NoTorsion, CosineTorsion, Cos3Torsion, Cos4Torsion, Cos3CTorsion, nTorsionFunctions };
	const char *torsionFunction(TorsionFunction);
	TorsionFunction torsionFunction(const char*);
	enum CosineTorsionParameters { CosineTorsionK, CosineTorsionEq, CosineTorsionP };
	enum Cos3TorsionParameters { Cos3TorsionK1, Cos3TorsionK2, Cos3TorsionK3 };
	enum Cos4TorsionParameters { Cos4TorsionK1, Cos4TorsionK2, Cos4TorsionK3, Cos4TorsionK4 };
	enum CosCTorsionParameters { Cos3CTorsionK0, Cos3CTorsionK1, Cos3CTorsionK2, Cos3CTorsionK3 };
}

#endif
