/*
	*** Forcefield term functional forms
	*** src/ff/forms.h
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

#ifndef ATEN_FORMS_H
#define ATEN_FORMS_H

#include "math/constants.h"
#include "base/namespace.h"
#include "ff/combine.h"
#include <QString>

ATEN_BEGIN_NAMESPACE

// Function Definition
class FunctionData
{
	public:
	// Long name of the function
	const char* name;
	// Keyword name for the function
	const char* keyword;
	// Number of parameters used (including optional params)
	int nParameters;
	// Parameter names
	const char* parameters[MAXFFPARAMDATA];
	// Parameter keywords
	const char* parameterKeywords[MAXFFPARAMDATA];
	// Flag indicating energetic parameter
	bool isEnergyParameter[MAXFFPARAMDATA];
	// Default values for parameters
	double defaultValues[MAXFFPARAMDATA];
	// Combination rule to use (Vdw Functions only)
	CombinationRules::CombinationRule combinationRules[MAXFFPARAMDATA];
};

// VDW Potential Functions
class VdwFunctions
{
	public:
	// Functional Form enum
	enum VdwFunction { None, InversePower, Lj, LjGeometric, LjAB, Buckingham, Morse, nVdwFunctions };
	static VdwFunction vdwFunction(QString s, bool reportError = false);
	static int vdwParameter(VdwFunction, QString s, bool reportError = false);
	static void printValid();

	// Function data array
	static FunctionData functionData[];

	// Function data parameter enums
	enum LjParameter { LjEpsilon, LjSigma };
	enum LjABParameter { LjA, LjB };
	enum BuckinghamParameter { BuckinghamA, BuckinghamB, BuckinghamC };
	enum InversePowerParameter { InversePowerEpsilon, InversePowerR, InversePowerN  };
	enum MorseParameter { MorseD, MorseK, MorseEq };
};

// Bond Potential Functions
class BondFunctions
{
	public:
	// Functional Form enum
	enum BondFunction { None, Ignore, Constraint, Harmonic, Morse, nBondFunctions };
	static BondFunction bondFunction(QString s, bool reportError = false);
	static int bondParameter(BondFunction, QString s, bool reportError = false);
	static void printValid();

	// Function data array
	static FunctionData functionData[];

	// Function data parameter enums
	enum HarmonicParameter { HarmonicK, HarmonicEq };
	enum ConstraintParameter { ConstraintK, ConstraintEq };
	enum MorseParameter { MorseD, MorseK, MorseEq };
};

// Angle potential forms
class AngleFunctions
{
	public:
	// Functional Form enum
	enum AngleFunction { None, Ignore, Harmonic, Cosine, Cos2, HarmonicCosine, BondConstraint, nAngleFunctions };
	static AngleFunction angleFunction(QString s, bool reportError = false);
	static int angleParameter(AngleFunction, QString s, bool reportError = false);
	static void printValid();

	// Function data array
	static FunctionData functionData[];

	// Function data parameter enums
	enum HarmonicParameter { HarmonicK, HarmonicEq };
	enum CosineParameter { CosineK, CosineN, CosineEq, CosineS };
	enum Cos2Parameter { Cos2K, Cos2C0, Cos2C1, Cos2C2 };
	enum HarmonicCosineParameter { HarmonicCosineK, HarmonicCosineEq };
	enum BondConstraintParameter { BondConstraintK, BondConstraintEq };
};

// Torsion potential forms
class TorsionFunctions
{
	public:
	// Functional Form enum
	enum TorsionFunction { None, Ignore, Cosine, Cos3, Cos4, Cos3C, CosCos, Dreiding, Pol9, nTorsionFunctions };
	static TorsionFunction torsionFunction(QString s, bool reportError = false);
	static int torsionParameter(TorsionFunction, QString s, bool reportError = false);
	static void printValid();

	// Function data array
	static FunctionData functionData[];

	// Function data parameter enums
	enum CosineParameter { CosineK, CosineN, CosineEq, CosineS };
	enum Cos3Parameter { Cos3K1, Cos3K2, Cos3K3 };
	enum Cos4Parameter { Cos4K1, Cos4K2, Cos4K3, Cos4K4 };
	enum Cos3CParameter { Cos3CK0, Cos3CK1, Cos3CK2, Cos3CK3 };
	enum CosCosParameter { CosCosK, CosCosN, CosCosEq };
	enum DreidingParameter { DreidingK, DreidingN, DreidingEq };
	enum Pol9Parameter { Pol9K1, Pol9K2, Pol9K3, Pol9K4, Pol9K5, Pol9K6, Pol9K7, Pol9K8, Pol9K9 };
};

// Electostatic calculation methods
namespace Electrostatics
{
	// Electrostatic model
	enum ElecMethod { None, Coulomb, Ewald, EwaldAuto, nElectrostatics };
	const char* elecMethod(ElecMethod em);
	ElecMethod elecMethod(QString s, bool reportError = false);
}

ATEN_END_NAMESPACE

#endif
