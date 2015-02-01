/*
	*** Basis Shell Definition
	*** src/classes/basisshell.cpp
	Copyright T. Youngs 2007-2015

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
const char *BasisShellTypeKeywords[BasisShell::nBasisShellTypes] = { "none", "s", "p", "d", "f", "g", "l" };
BasisShell::BasisShellType BasisShell::basisShellType(const char *s, bool reportError)
{
	BasisShell::BasisShellType bft = (BasisShell::BasisShellType) enumSearch("basis shell type", BasisShell::nBasisShellTypes, BasisShellTypeKeywords, s, reportError);
	if ((bft == BasisShell::nBasisShellTypes) && reportError) enumPrintValid(BasisShell::nBasisShellTypes, BasisShellTypeKeywords);
	return bft;
}
const char *BasisShell::basisShellType(BasisShell::BasisShellType bft)
{
	return BasisShellTypeKeywords[bft];
}
// Cartesian function numbers and names
int NumberCartesianFunctions[BasisShell::nBasisShellTypes] = { 0, 1, 3, 6, 10, 15, 4 };
int BasisShell::nCartesianFunctions(BasisShellType bft)
{
	return NumberCartesianFunctions[bft];
}
const char *CartesianFunctionNames[BasisShell::nBasisShellTypes][11] = {
	{ "", "", "", "", "", "", "", "", "", "", "" },			// None
	{ "S", "", "", "", "", "", "", "", "", "", "" },		// S
	{ "P(X)", "P(Y)", "P(Z)", "", "", "", "", "", "", "", "" },	// P
	{ "D(X2)", "D(Y2)", "D(Z2)", "D(XY)", "D(XZ)", "D(YZ)", "", "", "", "", "" },	 // D
	{ "F(X3)", "F(Y3)", "F(Z3)", "", "", "", "", "", "", "", "" },	// F
	{ "", "", "", "", "", "", "", "", "", "", "" },		// G
	{ "", "", "", "", "", "", "", "", "", "", "" }		// L
};
const char *BasisShell::cartesianFunction(BasisShellType bft, int index)
{
	// Check limits of index
	if ((index < 0) || (index >= NumberCartesianFunctions[bft])) printf("Cartesian function ID %i is out of range for an '%s' shell.\n", index , BasisShellTypeKeywords[bft]);
	else return CartesianFunctionNames[bft][index];
	return "NONAME";
}
// Spherical function numbers and names
int NumberSphericalFunctions[BasisShell::nBasisShellTypes] = { 0, 1, 3, 5, 7, 9, 4 };
int BasisShell::nSphericalFunctions(BasisShellType bft)
{
	return NumberSphericalFunctions[bft];
}
const char *SphericalFunctionNames[BasisShell::nBasisShellTypes][11] = {
	{ "", "", "", "", "", "", "", "", "", "", "" },			// None
	{ "S", "", "", "", "", "", "", "", "", "", "" },		// S
	{ "P(X)", "P(Y)", "P(Z)", "", "", "", "", "", "", "", "" },	// P
	{ "D(Z2)", "D(XZ)", "D(YZ)", "D(XY)", "D(X2-Y2)", "", "", "", "", "", "" },	 // D
	{ "F(Z3)", "F(XZ2)", "F(YZ2)", "F(XYZ)", "FZ(X2-Y2)", "FX(X2-3Y2)", "FY(3Z2-Y2)", "", "", "", "" },	// F
	{ "", "", "", "", "", "", "", "", "", "", "" },		// G
	{ "", "", "", "", "", "", "", "", "", "", "" }		// L
};
const char *BasisShell::sphericalFunction(BasisShellType bft, int index)
{
	// Check limits of index
	if ((index < 0) || (index >= NumberSphericalFunctions[bft])) printf("Spherical function ID %i is out of range for an '%s' shell.\n", index, BasisShellTypeKeywords[bft]);
	else return SphericalFunctionNames[bft][index];
	return "NONAME";
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
