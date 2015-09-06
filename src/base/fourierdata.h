/*
	*** Fourier Data
	*** src/base/fourierdata.h
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

#ifndef ATEN_FOURIERDATA_H
#define ATEN_FOURIERDATA_H

#include "templates/vector3.h"
#include "base/namespace.h"
#include "templates/array.h"

ATEN_BEGIN_NAMESPACE

// Forward Declarations (Aten)
class UnitCell;
class Model;

// Fourier
class FourierData
{
	public:
	// Constructor / Destructor
	FourierData();
	~FourierData();

	private:
	// Cos and sin term arrays
	Array2D< Vec3<double> > rCos_, rSin_;
	// Number of terms (atoms) in arrays
	int nAtoms_;
	// Number of kvectors along each cell axis
	Vec3<int> kVec_;
	// Maximum kVector
	int kMax_;
	// Parameters used in Ewald sum.
	double alpha_, alphaSq_;

	public:
	// Return cosine array
	const Array2D< Vec3<double> >& rCos();
	// Return sine array
	const Array2D< Vec3<double> >& rSin();
	// Return number of kvectors along each cell axis
	Vec3<int> kVec() const;
	// Return maximum number of kvectors
	int kMax() const;
	// Return number of terms (atoms) in arrays
	int nAtoms() const;
	// Calculate selected range of atomic vectors from supplied config
	void calculate(Model* sourceModel, int startAtom = 0, int nAtoms = -1);
	// Prepares Fourier arrays for the model and kVec specified
	void prepare(Model* sourceModel, Vec3<int> kVec);
};

ATEN_END_NAMESPACE

#endif
