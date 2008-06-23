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
	static double coseq, delta;
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
			switch (pb->data()->angleStyle())
			{
				case (AngleFunctions::None):
					msg(Debug::None,"Warning: No function is specified for angle energy %i-%i-%i.\n", i, j, k);
					break;
				case (AngleFunctions::Harmonic): 
					// U(theta) = 0.5 * forcek * (theta - eq)**2
					forcek = params.data[AngleFunctions::HarmonicK];
					eq = params.data[AngleFunctions::HarmonicEq] / DEGRAD;
					theta -= eq;
					energy += 0.5 * forcek * theta * theta;
					break;
				case (AngleFunctions::Cosine):
					// U(theta) = forcek * (1 + s * cos(n*theta - eq))
					forcek = params.data[AngleFunctions::CosineK];
					eq = params.data[AngleFunctions::CosineEq] / DEGRAD;
					n = params.data[AngleFunctions::CosineN];
					s = params.data[AngleFunctions::CosineS];
					energy += forcek * (1.0 + s * cos(n * theta - eq));
					break;
				case (AngleFunctions::UffCosine):
					// U(theta) = (forcek / n*n) * (1 + cos(n*theta - eq))
					forcek = params.data[AngleFunctions::UffCosineK];
					eq = params.data[AngleFunctions::UffCosineEq] / DEGRAD;
					n = params.data[AngleFunctions::UffCosineN];
	printf("ANGLE CONTRIBUTION: forcek %f, eq %f, n %f, energy %f\n",forcek, eq, n, (forcek / (n*n)) * (1.0 + cos(n * theta)));
					//printf("Energy %8.4f %8.4f %8.4f\n",forcek,n,eq);
					energy += (forcek / (n*n)) * (1.0 + cos(n * theta));
					break;
				case (AngleFunctions::Cos2):
					// U(theta) = forcek * (C0 + C1 * cos(theta) + C2 * cos(2*theta))
					forcek = params.data[AngleFunctions::Cos2K];
					eq = params.data[AngleFunctions::Cos2Eq] / DEGRAD;
					c0 = params.data[AngleFunctions::Cos2C0];
					c1 = params.data[AngleFunctions::Cos2C1];
					c2 = params.data[AngleFunctions::Cos2C2];
					energy += forcek * (c0 + c1 * cos(theta) + c2 * cos(2.0 * theta));
					break;
				case (AngleFunctions::HarmonicCosine):
					// U(theta) = 0.5 * forcek * (cos(theta) - cos(eq)))**2
					forcek = params.data[AngleFunctions::HarmonicCosineK];
					coseq = cos(params.data[AngleFunctions::HarmonicCosineEq] / DEGRAD);
					delta = cos(theta) - coseq;
					energy += 0.5 * forcek * delta * delta;
					break;
				default:
					msg(Debug::None, "No equation coded for angle energy of type '%s'.\n", AngleFunctions::AngleFunctions[pb->data()->angleStyle()].name);
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
			switch (pb->data()->angleStyle())
			{
				case (AngleFunctions::None):
					msg(Debug::None,"Warning: No function is specified for angle force %i-%i-%i.\n", i, j, k);
					du_dtheta = 0.0;
					break;
				case (AngleFunctions::Harmonic): 
					// dU/d(theta) = forcek * (theta - eq)
					forcek = params.data[AngleFunctions::HarmonicK];
					eq = params.data[AngleFunctions::HarmonicEq] / DEGRAD;
					du_dtheta = forcek * (theta - eq);
					break;
				case (AngleFunctions::Cosine):
					// dU/d(theta) = -forcek * n * s * sin(n*theta - eq)
					forcek = params.data[AngleFunctions::CosineK];
					eq = params.data[AngleFunctions::CosineEq] / DEGRAD;
					n = params.data[AngleFunctions::CosineN];
					s = params.data[AngleFunctions::CosineS];
					du_dtheta = -forcek * n * s * sin(n * theta - eq);
					break;
				case (AngleFunctions::UffCosine):
					// dU/d(theta) = -(forcek / n) * sin(n*theta)
					forcek = params.data[AngleFunctions::UffCosineK];
					eq = params.data[AngleFunctions::UffCosineEq] / DEGRAD;
					n = params.data[AngleFunctions::UffCosineN];
					du_dtheta = -(forcek / n) * sin(n*theta);
					break;
				case (AngleFunctions::Cos2):
					// dU/d(theta) = -forcek * (c1 * sin(theta) + 2 * c2 * sin(2*theta))
					forcek = params.data[AngleFunctions::Cos2K];
					eq = params.data[AngleFunctions::Cos2Eq] / DEGRAD;
					c1 = params.data[AngleFunctions::Cos2C1];
					c2 = params.data[AngleFunctions::Cos2C2];
					du_dtheta = -forcek * (c1 * sin(theta) + 2.0 * c2 * sin(2.0 * theta));
					break;
				case (AngleFunctions::HarmonicCosine):
					// dU/d(theta) = forcek * (cos(theta) - cos(eq))) * -sin(theta)
					forcek = params.data[AngleFunctions::HarmonicCosineK];
					cosx = cos(params.data[AngleFunctions::HarmonicCosineEq] / DEGRAD);
					du_dtheta = -forcek * (cos(theta) - cosx) * sin(theta);
					break;
				default:
					msg(Debug::None, "No equation coded for angle force of type '%s'.\n", AngleFunctions::AngleFunctions[pb->data()->angleStyle()].name);
					break;
			}
			// Complete chain rule
			du_dtheta *= dtheta_dcostheta;
			// Calculate atomic forces
			fi = vec_kj - vec_ij * dp;
			fi *= -du_dtheta / mag_ij;
			fk = vec_ij - vec_kj * dp;
			fk *= -du_dtheta / mag_kj;
			// Add contributions into force arrays
			modelatoms[i]->f() += fi;
			modelatoms[j]->f() -= fi + fk;
			modelatoms[k]->f() += fk;
		}
		aoff += nAtoms_;
	}
	dbgEnd(Debug::Calls,"Pattern::angleForcess");
}
