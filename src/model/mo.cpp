/*
	*** Model Molecular Orbital functions
	*** src/model/mo.cpp
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

#include "model/model.h"

// Add new basis function to the list
BasisShell *Model::addBasisShell()
{
	return basisShells_.add();
}

// Return the first basis function in the list
BasisShell *Model::basisShells()
{
	return basisShells_.first();
}

// Return total number of defined basis functions
int Model::nBasisShells()
{
	return basisShells_.nItems();
}

// Return total number of cartesian basis functions
int Model::nCartesianBasisFunctions()
{
	int result = 0;
	for (BasisShell *basis = basisShells_.first(); basis != NULL; basis = basis->next) result += basis->nCartesianFunctions();
	return result;
}

// Add new eigenvevtor to the list
Eigenvector *Model::addEigenvector()
{
	return eigenvectors_.add();
}

// Return the first eigenvevtor in the list
Eigenvector *Model::eigenvectors()
{
	return eigenvectors_.first();
}

// Return the n'th eigenvector in the list
Eigenvector *Model::eigenvector(int n)
{
	return eigenvectors_[n];
}

// Return total number of defined eigenvectors
int Model::nEigenvectors()
{
	return eigenvectors_.nItems();
}
