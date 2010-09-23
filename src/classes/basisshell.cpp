/*
	*** Basis Shell Definition
	*** src/classes/basisshell.cpp
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

#include "classes/basisshell.h"
#include "base/sysfunc.h"

// Basis Function Types
const char *BasisShellTypeKeywords[BasisShell::nBasisShellTypes] = { "none", "s", "l", "p", "d", "f" };
BasisShell::BasisShellType BasisShell::basisShellType(const char *s, bool reporterror)
{
	BasisShell::BasisShellType bft = (BasisShell::BasisShellType) enumSearch("basis shell type", BasisShell::nBasisShellTypes, BasisShellTypeKeywords, s, reporterror);
	if ((bft == BasisShell::nBasisShellTypes) && reporterror) enumPrintValid(BasisShell::nBasisShellTypes, BasisShellTypeKeywords);
	return bft;
}
const char *BasisShell::basisShellType(BasisShell::BasisShellType bft)
{
	return BasisShellTypeKeywords[bft];
}

/*
// BasisPrimitive
*/

// Constructor
BasisPrimitive::BasisPrimitive()
{
	// Private variables
	exponent_ = 0.0;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Set basis function exponent
void BasisPrimitive::setExponent(double exponent)
{
	exponent_ = exponent;
}

// Return basis function exponent
double BasisPrimitive::exponent() const
{
	return exponent_;
}

// Add contraction coefficient
void BasisPrimitive::addCoefficient(double coeff)
{
	ListItem<double> *newitem = coefficients_.add();
	newitem->setValue(coeff);
}

// Return number of defined coefficients
int BasisPrimitive::nCoefficients() const
{
	return coefficients_.nItems();
}

// Return specified contraction coefficient
double BasisPrimitive::coefficient(int index)
{
	if ((index < 0) || (index >= coefficients_.nItems())) msg.print("Coefficient index %i is out of bounds.\n", index);
	else return coefficients_[index]->value();
	return 0.0;
}

/*
// BasisShell
*/

// Constructor
BasisShell::BasisShell()
{
	// Private variables
	type_ = BasisShell::NoType;
	atomId_ = -1;

	// Public variables
	prev = NULL;
	next = NULL;
}

// Set associated atom pointer
void BasisShell::setAtomId(int id)
{
	atomId_ = id;
}

// Return associated atom pointer
int BasisShell::atomId() const
{
	return atomId_;
}

// Set basis function type
void BasisShell::setType(BasisShellType bft)
{
	type_ = bft;
}

// Return number of related cartesian functions, based on shell type
int BasisShell::nCartesianFunctions()
{
	switch (type_)
	{
		case (BasisShell::NoType):
			return 0;
			break;
		case (BasisShell::SShellType):
			return 1;
			break;
		case (BasisShell::SPShellType):
			return 4;
			break;
		case (BasisShell::PShellType):
			return 3;
			break;
		case (BasisShell::DShellType):
			return 5;
			break;
		case (BasisShell::FShellType):
			return 7;
			break;
	}
	return -1;
}

// Return name of cartesian function
const char *BasisShell::cartesianFunctionName(int id)
{
	static const char *pshell[3] = { "P(X)", "P(Y)", "P(Z)" };
	static const char *lshell[4] = { "L(S)", "L(PX)", "L(PY)", "L(PZ)" };
	static const char *dshell[5] = { "D(Z2)", "D(XZ)", "D(YZ)", "D(XY)", "D(X2-Y2)" };
	static const char *fshell[7] = { "F(Z3)", "F(XZ2)", "F(YZ2)", "F(XYZ)", "FZ(X2-Y2)", "FX(X2-3Y2)", "FY(3Z2-Y2)" };
	switch (type_)
	{
		case (BasisShell::NoType):
			break;
		case (BasisShell::SShellType):
			if (id == 0) return "S";
			else printf("Cartesian function ID is out of range for an S shell.\n", id);
			break;
		case (BasisShell::SPShellType):
			if ((id < 0) || (id > 3)) printf("Cartesian function ID is out of range for an L shell.\n", id);
			else return lshell[id];
			break;
		case (BasisShell::PShellType):
			if ((id < 0) || (id > 2)) printf("Cartesian function ID is out of range for a P shell.\n", id);
			else return pshell[id];
			break;
		case (BasisShell::DShellType):
			if ((id < 0) || (id > 4)) printf("Cartesian function ID is out of range for a D shell.\n", id);
			else return dshell[id];
			break;
		case (BasisShell::FShellType):
			if ((id < 0) || (id > 6)) printf("Cartesian function ID is out of range for a F shell.\n", id);
			else return fshell[id];
			break;
	}
	return "NONAME";
}

// Return basis function type
BasisShell::BasisShellType BasisShell::type() const
{
	return type_;
}

// Add new primitive to shell
BasisPrimitive *BasisShell::addPrimitive()
{
	return primitives_.add();
}

// Return first primitive in list
BasisPrimitive *BasisShell::primitives()
{
	return primitives_.first();
}

// Return n'th defined primitive
BasisPrimitive *BasisShell::primitive(int id)
{
	if ((id < 0) || (id >= primitives_.nItems())) msg.print("Index %i is out of range for primitives list.\n", id);
	else return primitives_[id];
	return NULL;
}

// Return total number of defined primitives
int BasisShell::nPrimitives()
{
	return primitives_.nItems();
}
