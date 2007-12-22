/*
	*** Fourier storage (reciprocal space vectors)
	*** src/classes/fourier.cpp
	Copyright T. Youngs 2007

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

#include "classes/fourier.h"
#include "model/model.h"

fourier_data fourier;

// Constructor
fourier_data::fourier_data()
{
	rcos = NULL;
	rsin = NULL;
	natoms = 0;
	kmax = 0;
	cell = NULL;
	#ifdef MEMDEBUG
		memdbg.create[MD_FOURIER] ++;
	#endif
}

// Destructor
fourier_data::~fourier_data()
{
	#ifdef MEMDEBUG
		memdbg.destroy[MD_FOURIER] ++;
	#endif
}

// Clear fourier structure
void fourier_data::clear()
{
	dbg_begin(DM_CALLS,"fourier::clear");
	if (rcos != NULL)
	{
		for (int n=0; n<kmax+1; n++) delete[] rcos[n];
		delete[] rcos;
		rcos = NULL;
	}
	if (rsin != NULL)
	{
		for (int n=0; n<2*kmax+1; n++) delete[] rsin[n];
		delete[] rsin;
		rsin = NULL;
	}
	// Set other vars to 'null' values
	kmax = 0;
	natoms = 0;
	dbg_end(DM_CALLS,"fourier::clear");
}

// Create fourier arrays
void fourier_data::create(int newnatoms, vec3<int> newkvec, int newkmax)
{
	// Create the rcos and rsin arrays to the specified dimensions.
	dbg_begin(DM_CALLS,"fourier::create");
	if (rcos != NULL) clear();
	if (newkmax < 1) printf("fourier::create <<<< Bad 'newkmax' passed (< 1) >>>>\n");
	kmax = newkmax;
	kvec = newkvec;
	natoms = newnatoms;
	rcos = new vec3<double>*[kmax+1];
	for (int n=0; n<kmax+1; n++) rcos[n] = new vec3<double>[natoms];
	rsin = new vec3<double>*[2*kmax+1];
	for (int n=0; n<2*kmax+1; n++) rsin[n] = new vec3<double>[natoms];
	msg(DM_VERBOSE,"Created Fourier space for %i atoms, kmax = %i \n",natoms, kmax);
	dbg_end(DM_CALLS,"fourier::create");
}

void fourier_data::calculate(model *srcmodel)
{
	// Calculate the atomic vectors for all atoms in the configuration
	calculate(srcmodel,0,natoms);
}

void fourier_data::calculate(model *srcmodel, int startatom, int atomstodo)
{
	// (Re-)Calculate the range of reciprocal space vectors of the coordinates in the supplied config.
	dbg_begin(DM_CALLS,"fourier::calculate");
	vec3<double> pos;
	mat3<double> tempmat;
	int firstsin, n, k, sinpos, i;
	if (srcmodel->get_natoms() != natoms)
	{
		printf("Indescribable fourier error! Wrong number of atoms in supplied config.\n");
		dbg_end(DM_CALLS,"fourier::calculate");
		return;
	}
	// Make sure model has a staticatoms space
	atom **modelatoms = srcmodel->get_atomarray();
	// Generate reciprocal space coordinates for atoms at each cartesian k-vector.
	// Only create positive k-vector positions for cos since is an even function. For sin calculate negative
	// also (odd function), where rsin[k] runs from k=0,2*kmax+1, with k=kmax the central (zero) vector (=complex(1,0)).
	// Thus, rsin[kmax+i] gives the i'th positive kvector and rsun[kmax-i] gives the i'th negative kvector.
	for (i=startatom; i<startatom+atomstodo; i++)
	{
		// Set central (zero) vector elements to be cmplx(1.0,0.0)
		rcos[0][i].x = 1.0; rsin[kmax][i].x = 0.0;
		rcos[0][i].y = 1.0; rsin[kmax][i].y = 0.0;
		rcos[0][i].z = 1.0; rsin[kmax][i].z = 0.0;
		// Calculate first vector in the positive k-direction
		tempmat = cell->get_recip();
		pos.x = tempmat.rows[0].dp(modelatoms[i]->r());
		pos.y = tempmat.rows[1].dp(modelatoms[i]->r());
		pos.z = tempmat.rows[2].dp(modelatoms[i]->r());
		rcos[1][i].x = cos(pos.x);
		rcos[1][i].y = cos(pos.y);
		rcos[1][i].z = cos(pos.z);
		rsin[kmax+1][i].x = sin(pos.x);
		rsin[kmax+1][i].y = sin(pos.y);
		rsin[kmax+1][i].z = sin(pos.z);
		// Calculate vector in the negative k-direction for sin terms
		rsin[kmax-1][i].x = -rsin[kmax+1][i].x;
		rsin[kmax-1][i].y = -rsin[kmax+1][i].y;
		rsin[kmax-1][i].z = -rsin[kmax+1][i].z;
		// Calculate the extended reciprocal space position vectors (power expansion of first vectors).
		// TODO Timewaste: Assume, for now, that we have a cubic box so we do all three vector arrays at once, and up to kmax. 
		// Build up the kvector positions by multiplying by rcos[+-1]/rsin[+-1] each time
		firstsin = kmax+1;
		for (n=1; n<kmax; n++)
		{
			// Complex multiplication: (a + bi)(c + di) = (ac - bd) + (ad + bc)i
			// Positive k-direction (cos and sin)
			k = n+1;
			sinpos = kmax + n;
			rcos[k][i].x = rcos[1][i].x * rcos[n][i].x - rsin[firstsin][i].x * rsin[sinpos][i].x;
			rcos[k][i].y = rcos[1][i].y * rcos[n][i].y - rsin[firstsin][i].y * rsin[sinpos][i].y;
			rcos[k][i].z = rcos[1][i].z * rcos[n][i].z - rsin[firstsin][i].z * rsin[sinpos][i].z;
			rsin[kmax+k][i].x = rcos[1][i].x * rsin[sinpos][i].x + rsin[firstsin][i].x * rcos[n][i].x;
			rsin[kmax+k][i].y = rcos[1][i].y * rsin[sinpos][i].y + rsin[firstsin][i].y * rcos[n][i].y;
			rsin[kmax+k][i].z = rcos[1][i].z * rsin[sinpos][i].z + rsin[firstsin][i].z * rcos[n][i].z;
			// Negative k-direction (sin)
			rsin[kmax-k][i].x = -rsin[kmax+k][i].x;
			rsin[kmax-k][i].y = -rsin[kmax+k][i].y;
			rsin[kmax-k][i].z = -rsin[kmax+k][i].z;
		}
	}
	dbg_end(DM_CALLS,"fourier::calculate");
}

void fourier_data::prepare(model *srcmodel, vec3<int> newkvec)
{
	// Set up arrays in the fourier class to handle all atoms / maximum kvectors specified.
	dbg_begin(DM_CALLS,"fourier::prepare");
	int newkmax = newkvec.max();
	// Don't delete the arrays, however, if the new natoms and kmax match...
	if ((natoms != srcmodel->get_natoms()) || (kmax != newkmax))
	{
		msg(DM_VERBOSE,"Clearing and recreating fourier arrays...\n");
		clear();
		create(srcmodel->get_natoms(), newkvec, newkmax);
	}
	cell = srcmodel->get_cell();
	// Now we have suitable arrays, we can calculate and store the reciprocal coordinate vectors
	calculate(srcmodel);
	dbg_end(DM_CALLS,"fourier::prepare");
}
