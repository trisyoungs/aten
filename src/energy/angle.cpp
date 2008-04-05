/*
	*** Angle energy / force calculation
	*** src/energy/angle.cpp
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

#include <math.h>
#include "templates/vector3.h"
#include "classes/pattern.h"
#include "classes/energystore.h"
#include "classes/forcefield.h"
#include "classes/cell.h"
#include "model/model.h"

// Calculate angle energy of pattern (or individual molecule if 'molecule' != -1)
void Pattern::angleEnergy(Model *srcmodel, Energy *estore, int molecule)
{
	dbgBegin(Debug::Calls,"Pattern::angleEnergy");
	static int i,j,k,aoff,m1;
	static double forcek, n, s, eq, r, theta, dp, energy, c0, c1, c2;
	static ForcefieldParams params;
	static PatternBound *pb;
	static Vec3<double> vecij, veckj;
	energy = 0.0;
	aoff = (molecule == -1 ? startAtom_ : startAtom_ + molecule*nAtoms_);
	for (m1=(molecule == -1 ? 0 : molecule); m1<(molecule == -1 ? nMols_ : molecule+1); m1++)
	{
		for (pb = angles_.first(); pb != NULL; pb = pb->next)
		{
			// Grab atom indices and calculate angle (in radians)
			i = pb->atomId(0) + aoff;
			j = pb->atomId(1) + aoff;
			k = pb->atomId(2) + aoff;
			theta = srcmodel->angle(i,j,k);
			// Grab pointer to function data
			params = pb->data()->params();
			// Calculate energy contribution
			switch (pb->data()->functionalForm().angleFunc)
			{
				case (AF_UNSPECIFIED):
					printf("Pattern::angleEnergy <<<< Angle function is UNSPECIFIED >>>>\n");
					break;
				case (AF_HARMONIC): 
					// U(theta) = 0.5 * forcek * (theta - eq)**2
					forcek = params.data[AF_HARMONIC_K];
					eq = params.data[AF_HARMONIC_EQ] / DEGRAD;
					theta -= eq;
					energy += 0.5 * forcek * theta * theta;
					break;
				//case (AF_COSINE):
					// U(theta) = forcek * (1 + cos(s*theta - eq))
					//printf("Angle - cosine...\n");
					//break;
				case (AF_UFFCOSINE1):
					// U(theta) = (forcek / n*n) * (1 + cos(n*theta))
					forcek = params.data[AF_UFFCOSINE_K];
					n = params.data[AF_UFFCOSINE_N];
					//printf("Energy %8.4f %8.4f %8.4f\n",forcek,n,eq);
					energy += (forcek / (n*n)) * (1.0 + cos(n * theta));
					break;
				case (AF_UFFCOSINE2):
					// U(theta) = forcek * (C0 + C1 * cos(theta) + C2 * cos(2*theta))
					forcek = params.data[AF_UFFCOSINE_K];
					eq = params.data[AF_UFFCOSINE_EQ] / DEGRAD;
					c2 = 1.0 / (4 * sin(eq)*sin(eq));
					c1 = -4.0 * c2 * cos(eq);
					c0 = c2 * (2.0 * cos(eq)*cos(eq) + 1.0);
					energy += forcek * (c0 + c1 * cos(theta) + c2 * cos(2.0 * theta));
					break;
				default:
					printf("No equation coded for angle energy type %i.\n",pb->data()->functionalForm().angleFunc);
					break;
			}
		}
		aoff += nAtoms_;
	}
	// Increment energy for pattern
	estore->add(Energy::AngleEnergy,energy,id_);
	dbgEnd(Debug::Calls,"Pattern::angleEnergy");
}

// Calculate angle forces in pattern
void Pattern::angleForces(Model *srcmodel)
{
	dbgBegin(Debug::Calls,"Pattern::angleForcess");
	static int i,j,k,aoff,m1;
	static Vec3<double> vec_ij, vec_kj, fi, fk;
	static double forcek, eq, dp, theta, mag_ij, mag_kj, n, s, c0, c1, c2, cosx, sinx;
	static double du_dtheta, dtheta_dcostheta;
	static ForcefieldParams params;
	static PatternBound *pb;
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	aoff = startAtom_;
	for (m1=0; m1<nMols_; m1++)
	{
		for (pb = angles_.first(); pb != NULL; pb = pb->next)
		{
			// Grab atomic indices and calculate angle (in radians)
			i = pb->atomId(0) + aoff;
			j = pb->atomId(1) + aoff;
			k = pb->atomId(2) + aoff;
			// Minimum image w.r.t. atom j
			vec_ij = cell->mimd(modelatoms[i]->r(),modelatoms[j]->r());
			vec_kj = cell->mimd(modelatoms[k]->r(),modelatoms[j]->r());
			// Normalise vectors, calculate dot product and angle.
			mag_ij = vec_ij.magAndNormalise();
			mag_kj = vec_kj.magAndNormalise();
			dp = vec_ij.dp(vec_kj);
			theta = acos(dp);
			dtheta_dcostheta = -1.0 / sin(theta);
			params = pb->data()->params();
			// Generate forces
			switch (pb->data()->functionalForm().angleFunc)
			{
				case (AF_UNSPECIFIED):
					printf("Pattern::angleForcess <<<< Angle function is UNSPECIFIED >>>>\n");
					du_dtheta = 0.0;
					break;
				//case (AF_COSINE):
				//	printf("Angle - cosine...\n");
				//	break;
				case (AF_HARMONIC): 
					// F(theta) = forcek * (theta - eq)
					forcek = params.data[AF_HARMONIC_K];
					eq = params.data[AF_HARMONIC_EQ] / DEGRAD;
					du_dtheta = dtheta_dcostheta * forcek * (theta - eq);
					break;
				case (AF_UFFCOSINE1):
					// F(theta) = (forcek / 2*n) * (1 - sin(n*theta))
					forcek = params.data[AF_UFFCOSINE_K];
					n = params.data[AF_UFFCOSINE_N];
					du_dtheta = dtheta_dcostheta * (forcek / n) * sin(n*theta);
					break;
				case (AF_UFFCOSINE2):
					// F(theta) = forcek * (c0 - c1 * sin(theta) - c2 * sin(2*theta))
					forcek = params.data[AF_UFFCOSINE_K];
					eq = params.data[AF_UFFCOSINE_EQ] / DEGRAD;
					cosx = cos(eq);
					sinx = sin(eq);
					c2 = 1.0 / (4 * sinx*sinx);
					c1 = -4.0 * c2 * cosx;
					c0 = c2 * (2.0 * cosx*cosx + 1.0);
					du_dtheta = dtheta_dcostheta * forcek * (c0 - c1 * sin(theta) - 2.0 * c2 * sin(2.0 * theta));
					break;
				default:
					printf("No equation coded for angle forces type %i.\n",pb->data()->functionalForm().angleFunc);
					break;
			}
			// Calculate atomic forces
			fi = vec_kj - vec_ij * dp;
			fi *= du_dtheta / mag_ij;
			fk = vec_ij - vec_kj * dp;
			fk *= du_dtheta / mag_kj;
			// Add contributions into force arrays
			modelatoms[i]->f() -= fi;
			modelatoms[j]->f() += fi + fk;
			modelatoms[k]->f() -= fk;
		}
		aoff += nAtoms_;
	}
	dbgEnd(Debug::Calls,"Pattern::angleForcess");
}
