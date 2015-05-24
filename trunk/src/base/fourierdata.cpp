/*
	*** Fourier Data
	*** src/base/fourierdata.cpp
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

#include "base/fourierdata.h"
#include "model/model.h"

ATEN_BEGIN_NAMESPACE

// Static Singleton
FourierData fourier;

ATEN_END_NAMESPACE

ATEN_USING_NAMESPACE

// Constructor
FourierData::FourierData()
{
	nAtoms_ = 0;
	kMax_ = 0;
}

// Destructor
FourierData::~FourierData()
{
}

// Return cosine array
Array2D< Vec3<double> > FourierData::rCos() const
{
	return rCos_;
}

// Return sine array
Array2D< Vec3<double> > FourierData::rSin() const
{
	return rSin_;
}

// Return number of kvectors along each cell axis
Vec3<int> FourierData::kVec() const
{
	return kVec_;
}

// Return maximum number of kvectors
int FourierData::kMax() const
{
	return kMax_;
}

// Return number of terms (atoms) in arrays
int FourierData::nAtoms() const
{
	return nAtoms_;
}

// Calculate cos and sin terms for the specified atoms in the model
void FourierData::calculate(Model* sourceModel, int startAtom, int nAtomsToDo)
{
	Messenger::enter("FourierData:::calculate");

	Vec3<double> pos;
	Matrix fourierMat;
	int firstSin, n, k, sinPos, i;

	if (nAtomsToDo == -1) nAtomsToDo = sourceModel->nAtoms();
	if (sourceModel->nAtoms() != nAtomsToDo)
	{
		printf("Indescribable fourier error! Wrong number of atoms in supplied config.\n");
		Messenger::exit("FourierData:::calculate");
		return;
	}

	// Make sure model has a staticatoms space
	Atom** modelAtoms = sourceModel->atomArray();

	// Generate reciprocal space coordinates for atoms at each cartesian k-vector.
	// Only create positive k-vector positions for cos since is an even function. For sin calculate negative
	// also (odd function), where rSin_[k] runs from k=0,2*kmax+1, with k=kmax the central (zero) vector (=complex(1,0)).
	// Thus, rSin_[kmax+i] gives the i'th positive kvector and rsun[kmax-i] gives the i'th negative kvector.
	// Grab reciprocal cell and multiply by TWOPI
	fourierMat = sourceModel->cell().reciprocal() * TWOPI;
	for (i=startAtom; i<startAtom+nAtomsToDo; ++i)
	{
		// Set central (zero) vector elements to be cmplx(1.0,0.0)
		rCos_.ref(0,i).x = 1.0; rSin_.ref(kMax_,i).x = 0.0;
		rCos_.ref(0,i).y = 1.0; rSin_.ref(kMax_,i).y = 0.0;
		rCos_.ref(0,i).z = 1.0; rSin_.ref(kMax_,i).z = 0.0;
		// Calculate first vector in the positive k-direction
		pos.x = fourierMat.columnAsVec3(0).dp(modelAtoms[i]->r());
		pos.y = fourierMat.columnAsVec3(1).dp(modelAtoms[i]->r());
		pos.z = fourierMat.columnAsVec3(2).dp(modelAtoms[i]->r());
		rCos_.ref(1,i).x = cos(pos.x);
		rCos_.ref(1,i).y = cos(pos.y);
		rCos_.ref(1,i).z = cos(pos.z);
		rSin_.ref(kMax_+1,i).x = sin(pos.x);
		rSin_.ref(kMax_+1,i).y = sin(pos.y);
		rSin_.ref(kMax_+1,i).z = sin(pos.z);
		// Calculate vector in the negative k-direction for sin terms
		rSin_.ref(kMax_-1,i).x = -rSin_.ref(kMax_+1,i).x;
		rSin_.ref(kMax_-1,i).y = -rSin_.ref(kMax_+1,i).y;
		rSin_.ref(kMax_-1,i).z = -rSin_.ref(kMax_+1,i).z;
		// Calculate the extended reciprocal space position vectors (power expansion of first vectors).
		// TODO Timewaste: Assume, for now, that we have a cubic box so we do all three vector arrays at once, and up to kmax. 
		// Build up the kvector positions by multiplying by rCos_[+-1]/rSin_[+-1] each time
		firstSin = kMax_+1;
		for (n=1; n<kMax_; n++)
		{
			// Complex multiplication: (a + bi)(c + di) = (ac - bd) + (ad + bc)i
			// Positive k-direction (cos and sin)
			k = n+1;
			sinPos = kMax_ + n;
			rCos_.ref(k,i).x = rCos_.ref(1,i).x * rCos_.ref(n,i).x - rSin_.ref(firstSin,i).x * rSin_.ref(sinPos,i).x;
			rCos_.ref(k,i).y = rCos_.ref(1,i).y * rCos_.ref(n,i).y - rSin_.ref(firstSin,i).y * rSin_.ref(sinPos,i).y;
			rCos_.ref(k,i).z = rCos_.ref(1,i).z * rCos_.ref(n,i).z - rSin_.ref(firstSin,i).z * rSin_.ref(sinPos,i).z;
			rSin_.ref(kMax_+k,i).x = rCos_.ref(1,i).x * rSin_.ref(sinPos,i).x + rSin_.ref(firstSin,i).x * rCos_.ref(n,i).x;
			rSin_.ref(kMax_+k,i).y = rCos_.ref(1,i).y * rSin_.ref(sinPos,i).y + rSin_.ref(firstSin,i).y * rCos_.ref(n,i).y;
			rSin_.ref(kMax_+k,i).z = rCos_.ref(1,i).z * rSin_.ref(sinPos,i).z + rSin_.ref(firstSin,i).z * rCos_.ref(n,i).z;
			// Negative k-direction (sin)
			rSin_.ref(kMax_-k,i).x = -rSin_.ref(kMax_+k,i).x;
			rSin_.ref(kMax_-k,i).y = -rSin_.ref(kMax_+k,i).y;
			rSin_.ref(kMax_-k,i).z = -rSin_.ref(kMax_+k,i).z;
		}
	}
	Messenger::exit("FourierData:::calculate");
}

// Prepares Fourier arrays for the model and kVec specified
void FourierData::prepare(Model* sourceModel, Vec3<int> newKVec)
{
	// Set up arrays in the fourier class to handle all atoms / maximum kvectors specified.
	Messenger::enter("FourierData:::prepare");
	int newKMax = newKVec.max();

	// Don't delete the arrays, however, if the new nAtoms and kmax match...
	if ((nAtoms_ != sourceModel->nAtoms()) || (kMax_ != newKMax))
	{
		Messenger::print(Messenger::Verbose, "Clearing and recreating fourier arrays...");
		if (newKMax < 1) printf("FourierData:::create <<<< Bad 'newkmax' passed (< 1) >>>>\n");
		
		kMax_ = newKMax;
		kVec_ = newKVec;
		nAtoms_ = sourceModel->nAtoms();

		// Resize arrays
		rCos_.initialise(kMax_+1, nAtoms_);
// 		rCos_ = new Vec3<double>*[kMax_+1];
// 		for (int n=0; n<kMax_+1; ++n) rCos_[n] = new Vec3<double>[nAtoms_];
		rSin_.initialise(2*kMax_+1, nAtoms_);
// 		rSin_ = new Vec3<double>*[2*kMax_+1];
// 		for (int n=0; n<2*kMax_+1; ++n) rSin_[n] = new Vec3<double>[nAtoms_];
		Messenger::print(Messenger::Verbose, "Created Fourier space for %i atoms, kmax = %i, k(x,y,z) = (%i,%i,%i)",nAtoms_, kMax_, kVec_.x, kVec_.y, kVec_.z);
	}

	// Now we have suitable arrays, we can calculate and store the reciprocal coordinate vectors
	calculate(sourceModel);

	Messenger::exit("FourierData:::prepare");
}
