/*
	*** Basis Function Definition
	*** src/classes/basisfunction.cpp
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

#include "classes/basisfunction.h"
#include "base/sysfunc.h"

// Basis Function Types
const char *BasisFunctionTypeKeywords[BasisFunction::nBasisFunctionTypes] = { "s", "p", "d", "f" };
BasisFunction::BasisFunctionType BasisFunction::basisFunctionType(const char *s, bool reporterror)
{
	BasisFunction::BasisFunctionType bft = (BasisFunction::BasisFunctionType) enumSearch("basis function type", BasisFunction::nBasisFunctionTypes, BasisFunctionTypeKeywords, s, reporterror);
	if ((bft == BasisFunction::nBasisFunctionTypes) && reporterror) enumPrintValid(BasisFunction::nBasisFunctionTypes, BasisFunctionTypeKeywords);
	return bft;
}
const char *BasisFunction::basisFunctionType(BasisFunction::BasisFunctionType bft)
{
	return BasisFunctionTypeKeywords[bft];
}

// Constructor
BasisFunction::BasisFunction()
{
	// Private variables
	type_ = BasisFunction::NoType;
	exponent_ = 0.0;
	
	// Public variables
	prev = NULL;
	next = NULL;
}

