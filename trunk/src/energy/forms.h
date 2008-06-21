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
#include "classes/forcefieldparams.h"

// Function Definition
class FunctionData
{
	public:
	// Long name of the function
	const char *name;
	// Keyword name for the function
	const char *keyword;
	// Parameter names
	const char *parameters[MAXFFPARAMDATA];
	// Parameter keywords
	const char *parameterKeywords[MAXFFPARAMDATA];
	// Flag indicating energetic parameter
	bool isEnergyParameter[MAXFFPARAMDATA];
	// Default values for parameters
	double defaultValues[MAXFFPARAMDATA];
};

// VDW Potential Functions
namespace VdwFunctions
{
	enum VdwFunction { None, InversePower, Lj, LjAB, Buckingham, Morse, nVdwFunctions };
	extern FunctionData VdwFunctions[];
	VdwFunction vdwFunction(const char*);
	enum LjParameter { LjEpsilon, LjSigma, LjN };
	enum LjABParameter { LjA, LjB };
	enum BuckinghamParameter { BuckinghamA, BuckinghamB, BuckinghamC };
	enum InversePowerParameter { InversePowerEpsilon, InversePowerR, InversePowerN  };
	enum MorseParameter { MorseK, MorseEq, MorseD };
}

// Bond Potential Functions
namespace BondFunctions
{
	enum BondFunction { None, Constraint, Harmonic, Morse, Morse2, nBondFunctions };
	extern FunctionData BondFunctions[];
	BondFunction bondFunction(const char*);
	enum HarmonicParameter { HarmonicK, HarmonicEq };
	enum ConstraintParameter { ConstraintK, ConstraintEq };
	enum MorseParameter { MorseK, MorseEq, MorseD };
	enum Morse2Parameter { Morse2K, Morse2Eq, Morse2D };
}

// Angle potential forms
namespace AngleFunctions
{
	enum AngleFunction { None, Harmonic, Cosine, UffCosine, Cos2, HarmonicCosine, nAngleFunctions };
	extern FunctionData AngleFunctions[];
	AngleFunction angleFunction(const char*);
	enum HarmonicParameter { HarmonicK, HarmonicEq };
	enum CosineParameter { CosineK, CosineN, CosineEq, CosineS };
	enum UffCosineParameter { UffCosineK, UffCosineN, UffCosineEq };
	enum Cos2Parameter { Cos2K, Cos2Eq, Cos2C0, Cos2C1, Cos2C2 };
	enum HarmonicCosineParameter { HarmonicCosineK, HarmonicCosineEq };
}

// Torsion potential forms
namespace TorsionFunctions
{
	enum TorsionFunction { None, Cosine, Cos3, Cos4, Cos3C, CosCos, Dreiding, nTorsionFunctions };
	extern FunctionData TorsionFunctions[];
	TorsionFunction torsionFunction(const char*);
	enum CosineParameter { CosineK, CosineN, CosineEq, CosineS, CosineEScale, CosineVScale };
	enum Cos3Parameter { Cos3K1, Cos3K2, Cos3K3, Cos3NULL, Cos3EScale, Cos3VScale };
	enum Cos4Parameter { Cos4K1, Cos4K2, Cos4K3, Cos4K4, Cos4EScale, Cos4VScale };
	enum Cos3CParameter { Cos3CK0, Cos3CK1, Cos3CK2, Cos3CK3, Cos3CEScale, Cos3CVScale };
	enum CosCosParameter { CosCosK, CosCosN, CosCosEq, CosCosNULL, CosCosEScale, CosCosVScale };
	enum DreidingParameter { DreidingK, DreidingN, DreidingEq, DreidingNULL, DreidingEScale, DreidingVScale };
}

// Electostatic calculation methods
namespace Electrostatics
{
	// Electrostatic model
	enum ElecMethod { None, Coulomb, Ewald, EwaldAuto, nElectrostatics };
	const char *elecMethod(ElecMethod);
	ElecMethod elecMethod(const char*);
}

// Generation rules (for rule-based FFs)
namespace Rules
{
	enum ForcefieldRules { None, Uff, DreidingLJ, DreidingX6, nForcefieldRules };
	const char *forcefieldRules(ForcefieldRules);
	ForcefieldRules forcefieldRules(const char*);
}

#endif
