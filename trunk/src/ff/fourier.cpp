/*
	*** Fourier storage (reciprocal space vectors)
	*** src/ff/fourier.cpp
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

#include "ff/fourier.h"
#include "model/model.h"

// Singleton Declaration
FourierData fourier;

// Constructor
FourierData::FourierData()
{
	rCos = NULL;
	rSin = NULL;
	nAtoms = 0;
	kMax = 0;
	cell = NULL;
}

// Destructor
FourierData::~FourierData()
{
	clear();
}

// Clear fourier structure
void FourierData::clear()
{
	msg.enter("FourierData:::clear");
	if (rCos != NULL)
	{
		for (int n=0; n<kMax+1; n++) delete[] rCos[n];
		delete[] rCos;
		rCos = NULL;
	}
	if (rSin != NULL)
	{
		for (int n=0; n<2*kMax+1; n++) delete[] rSin[n];
		delete[] rSin;
		rSin = NULL;
	}
	// Set other vars to 'null' values
	kMax = 0;
	nAtoms = 0;
	msg.exit("FourierData:::clear");
}

// Create fourier arrays
void FourierData::create(int newnAtoms, Vec3<int> newkvec, int newkmax)
{
	// Create the rCos and rSin arrays to the specified dimensions.
	msg.enter("FourierData:::create");
	if (rCos != NULL) clear();
	if (newkmax < 1) printf("FourierData:::create <<<< Bad 'newkmax' passed (< 1) >>>>\n");
	kMax = newkmax;
	kVec = newkvec;
	nAtoms = newnAtoms;
	rCos = new Vec3<double>*[kMax+1];
	for (int n=0; n<kMax+1; n++) rCos[n] = new Vec3<double>[nAtoms];
	rSin = new Vec3<double>*[2*kMax+1];
	for (int n=0; n<2*kMax+1; n++) rSin[n] = new Vec3<double>[nAtoms];
	msg.print(Messenger::Verbose,"Created Fourier space for %i atoms, kmax = %i \n",nAtoms, kMax);
	msg.exit("FourierData:::create");
}

void FourierData::calculate(Model* srcmodel)
{
	// Calculate the atomic vectors for all atoms in the configuration
	calculate(srcmodel,0,nAtoms);
}

void FourierData::calculate(Model* srcmodel, int startatom, int atomstodo)
{
	// (Re-)Calculate the range of reciprocal space vectors of the coordinates in the supplied config.
	msg.enter("FourierData:::calculate");
	Vec3<double> pos;
	Matrix fouriermat;
	int firstsin, n, k, sinpos, i;
	if (srcmodel->nAtoms() != nAtoms)
	{
		printf("Indescribable fourier error! Wrong number of atoms in supplied config.\n");
		msg.exit("FourierData:::calculate");
		return;
	}
	// Make sure model has a staticatoms space
	Atom* *modelatoms = srcmodel->atomArray();
	// Generate reciprocal space coordinates for atoms at each cartesian k-vector.
	// Only create positive k-vector positions for cos since is an even function. For sin calculate negative
	// also (odd function), where rSin[k] runs from k=0,2*kmax+1, with k=kmax the central (zero) vector (=complex(1,0)).
	// Thus, rSin[kmax+i] gives the i'th positive kvector and rsun[kmax-i] gives the i'th negative kvector.
	// Grab reciprocal cell and multiply by TWOPI
	fouriermat = cell->reciprocal() * TWOPI;
	for (i=startatom; i<startatom+atomstodo; i++)
	{
		// Set central (zero) vector elements to be cmplx(1.0,0.0)
		rCos[0][i].x = 1.0; rSin[kMax][i].x = 0.0;
		rCos[0][i].y = 1.0; rSin[kMax][i].y = 0.0;
		rCos[0][i].z = 1.0; rSin[kMax][i].z = 0.0;
		// Calculate first vector in the positive k-direction
		pos.x = fouriermat.columnAsVec3(0).dp(modelatoms[i]->r());
		pos.y = fouriermat.columnAsVec3(1).dp(modelatoms[i]->r());
		pos.z = fouriermat.columnAsVec3(2).dp(modelatoms[i]->r());
		rCos[1][i].x = cos(pos.x);
		rCos[1][i].y = cos(pos.y);
		rCos[1][i].z = cos(pos.z);
		rSin[kMax+1][i].x = sin(pos.x);
		rSin[kMax+1][i].y = sin(pos.y);
		rSin[kMax+1][i].z = sin(pos.z);
		// Calculate vector in the negative k-direction for sin terms
		rSin[kMax-1][i].x = -rSin[kMax+1][i].x;
		rSin[kMax-1][i].y = -rSin[kMax+1][i].y;
		rSin[kMax-1][i].z = -rSin[kMax+1][i].z;
		// Calculate the extended reciprocal space position vectors (power expansion of first vectors).
		// TODO Timewaste: Assume, for now, that we have a cubic box so we do all three vector arrays at once, and up to kmax. 
		// Build up the kvector positions by multiplying by rCos[+-1]/rSin[+-1] each time
		firstsin = kMax+1;
		for (n=1; n<kMax; n++)
		{
			// Complex multiplication: (a + bi)(c + di) = (ac - bd) + (ad + bc)i
			// Positive k-direction (cos and sin)
			k = n+1;
			sinpos = kMax + n;
			rCos[k][i].x = rCos[1][i].x * rCos[n][i].x - rSin[firstsin][i].x * rSin[sinpos][i].x;
			rCos[k][i].y = rCos[1][i].y * rCos[n][i].y - rSin[firstsin][i].y * rSin[sinpos][i].y;
			rCos[k][i].z = rCos[1][i].z * rCos[n][i].z - rSin[firstsin][i].z * rSin[sinpos][i].z;
			rSin[kMax+k][i].x = rCos[1][i].x * rSin[sinpos][i].x + rSin[firstsin][i].x * rCos[n][i].x;
			rSin[kMax+k][i].y = rCos[1][i].y * rSin[sinpos][i].y + rSin[firstsin][i].y * rCos[n][i].y;
			rSin[kMax+k][i].z = rCos[1][i].z * rSin[sinpos][i].z + rSin[firstsin][i].z * rCos[n][i].z;
			// Negative k-direction (sin)
			rSin[kMax-k][i].x = -rSin[kMax+k][i].x;
			rSin[kMax-k][i].y = -rSin[kMax+k][i].y;
			rSin[kMax-k][i].z = -rSin[kMax+k][i].z;
		}
	}
	msg.exit("FourierData:::calculate");
}

void FourierData::prepare(Model* srcmodel, Vec3<int> newkvec)
{
	// Set up arrays in the fourier class to handle all atoms / maximum kvectors specified.
	msg.enter("FourierData:::prepare");
	int newkmax;
	newkmax = newkvec.max();
	// Don't delete the arrays, however, if the new nAtoms and kmax match...
	if ((nAtoms != srcmodel->nAtoms()) || (kMax != newkmax))
	{
		msg.print(Messenger::Verbose,"Clearing and recreating fourier arrays...\n");
		clear();
		create(srcmodel->nAtoms(), newkvec, newkmax);
	}
	cell = srcmodel->cell();
	// Now we have suitable arrays, we can calculate and store the reciprocal coordinate vectors
	calculate(srcmodel);
	msg.exit("FourierData:::prepare");
}
