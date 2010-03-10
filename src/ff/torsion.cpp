/*
	*** Torsion energy / force calculation
	*** src/ff/torsion.cpp
	Copyright T. Youngs 2007-2009

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
#include "classes/forcefieldbound.h"
#include "ff/forms.h"
#include "base/pattern.h"

// Torsion energy
void Pattern::torsionEnergy(Model *srcmodel, Energy *estore, int molecule)
{
	// Calculate the energy of the torsions in this pattern with coordinates from *xcfg
	msg.enter("Pattern::torsionEnergy");
	int n,i,j,k,l,aoff,m1;
	static double k0, k1, k2, k3, k4, k5, k6, k7, k8, k9, eq, phi, energy, period, s, chi;
	PatternBound *pb;
	ForcefieldBound *ffb;
	Vec3<double> vecij, veckj;
	energy = 0.0;
	aoff = (molecule == -1 ? startAtom_ : startAtom_ + molecule*nAtoms_);
	for (m1=(molecule == -1 ? 0 : molecule); m1<(molecule == -1 ? nMolecules_ : molecule + 1); m1++)
	{
		for (pb = torsions_.first(); pb != NULL; pb = pb->next)
		{
			// Grab atom indices and calculate torsion angle (in radians)
			i = pb->atomId(0) + aoff;
			j = pb->atomId(1) + aoff;
			k = pb->atomId(2) + aoff;
			l = pb->atomId(3) + aoff;
			phi = srcmodel->torsion(i,j,k,l);
			ffb = pb->data();
			// Calculate energy
			switch (pb->data()->torsionStyle())
			{
				case (TorsionFunctions::None):
					msg.print("Warning: No function is specified for torsion energy %i-%i-%i-%i.\n", i, j, k, l);
				case (TorsionFunctions::Ignore):
					break;
				case (TorsionFunctions::Cosine): 
					// U(phi) = forcek * (1 + s*cos(period*phi - eq))
					k1 = ffb->parameter(TorsionFunctions::CosineK);
					eq = ffb->parameter(TorsionFunctions::CosineEq) / DEGRAD;
					period = ffb->parameter(TorsionFunctions::CosineN);
					s = ffb->parameter(TorsionFunctions::CosineS);
					energy += k1 * (1.0 + s * cos(period*phi - eq));
					break;
				case (TorsionFunctions::Cos3):
					// U(phi) = 0.5 * ( k1*(1+cos(phi)) + k2*(1-cos(2*phi)) + k3*(1+cos(3*phi)) )
					k1 = ffb->parameter(TorsionFunctions::Cos3K1);
					k2 = ffb->parameter(TorsionFunctions::Cos3K2);
					k3 = ffb->parameter(TorsionFunctions::Cos3K3);
					energy += 0.5 * (k1 * (1.0 + cos(phi)) + k2 * (1.0 - cos(2.0*phi)) + k3 * (1.0 + cos(3.0*phi)));
					break;
				case (TorsionFunctions::Cos4):
					// U(phi) = 0.5 * ( k1*(1+cos(phi)) + k2*(1-cos(2*phi)) + k3*(1+cos(3*phi)) + k4*(1-cos(4*phi)) )
					k1 = ffb->parameter(TorsionFunctions::Cos4K1);
					k2 = ffb->parameter(TorsionFunctions::Cos4K2);
					k3 = ffb->parameter(TorsionFunctions::Cos4K3);
					k4 = ffb->parameter(TorsionFunctions::Cos4K4);
					energy += 0.5 * (k1*(1.0+cos(phi)) + k2*(1.0-cos(2.0*phi)) + k3*(1.0+cos(3.0*phi)) + k4*(1.0-cos(4.0*phi)) );
					break;
				case (TorsionFunctions::Cos3C):
					// U(phi) = k0 + 0.5 * ( k1*(1+cos(phi)) + k2*(1-cos(2*phi)) + k3*(1+cos(3*phi)) )
					k0 = ffb->parameter(TorsionFunctions::Cos3CK0);
					k1 = ffb->parameter(TorsionFunctions::Cos3CK1);
					k2 = ffb->parameter(TorsionFunctions::Cos3CK2);
					k3 = ffb->parameter(TorsionFunctions::Cos3CK3);
					energy += k0 + 0.5 * (k1*(1.0+cos(phi)) + k2*(1.0-cos(2.0*phi)) + k3*(1.0+cos(3.0*phi)) );
					break;
				case (TorsionFunctions::CosCos):
					// U(phi) = 0.5 * k * (1 - cos(n*eq) * cos(n*theta))
					k1 = ffb->parameter(TorsionFunctions::CosCosK);
					period = ffb->parameter(TorsionFunctions::CosCosN);
					eq = ffb->parameter(TorsionFunctions::CosCosEq) / DEGRAD;
					energy += 0.5 * k1 * (1.0 - cos(period*eq)*cos(period*phi));
					break;
				case (TorsionFunctions::Dreiding):
					// U(phi) = 0.5 * k * (1 - cos(n*(theta-eq))
					k1 = ffb->parameter(TorsionFunctions::DreidingK);
					period = ffb->parameter(TorsionFunctions::DreidingN);
					eq = ffb->parameter(TorsionFunctions::DreidingEq) / DEGRAD;
					energy += 0.5 * k1 * (1.0 - cos(n*(phi - eq)));
					break;
				case (TorsionFunctions::Pol9):
					// U(chi) = sum_{i=0,8} k_i (cos(chi))^i
					chi = PI - phi;
					k1 = ffb->parameter(TorsionFunctions::Pol9K1);
					k2 = ffb->parameter(TorsionFunctions::Pol9K2);
					k3 = ffb->parameter(TorsionFunctions::Pol9K3);
					k4 = ffb->parameter(TorsionFunctions::Pol9K4);
					k5 = ffb->parameter(TorsionFunctions::Pol9K5);
					k6 = ffb->parameter(TorsionFunctions::Pol9K6);
					k7 = ffb->parameter(TorsionFunctions::Pol9K7);
					k8 = ffb->parameter(TorsionFunctions::Pol9K8);
					k9 = ffb->parameter(TorsionFunctions::Pol9K9);
					energy += k1 + cos(chi)*(k2 + cos(chi)*(k3 + cos(chi)*(k4 + cos(chi)*(k5 + cos(chi)*(k6 + cos(chi)*(k7 + cos(chi)*(k8 + cos(chi)*k9)))))));
				default:
					msg.print( "No equation coded for torsion energy of type '%s'.\n",  TorsionFunctions::TorsionFunctions[ffb->torsionStyle()].name);
					break;
			}
			//printf("TENG - molstart = %i: %i-%i-%i-%i (%i-%i-%i-%i) = %f (tot = %f)\n",aoff,i,j,k,l,pb->atomId(0),pb->atomId(1),pb->atomId(2),pb->atomId(3), phi,energy);
		}
		aoff += nAtoms_;
	}
	// Increment energy for pattern
	estore->add(Energy::TorsionEnergy,energy,id_);
	msg.exit("Pattern::torsionEnergy");
}

// Returns a unit vector in the specified direction
Vec3<double> unit_vector(int n)
{
	Vec3<double> result;
	result.zero();
	result.set(n,1.0);
	return result;
}

// Return cyclic permutation of the integer provided
int cp(int n)
{
	return (n%3);
}

// Construct 'cross-product' vector of the supplied vector using cyclic permutations
Mat3<double> make_cp_mat(Vec3<double> *v)
{
	Mat3<double> result;
	Vec3<double> temp;
	for (int n=0; n<3; n++)
	{
		temp = unit_vector(cp(n+1)) * v->get(cp(n+2)) - unit_vector(cp(n+2)) * v->get(cp(n+1));
		result.set(n,temp.x,temp.y,temp.z);
	}
	return result;
}

// Torsion forces
void Pattern::torsionForces(Model *srcmodel)
{
	// Calculate force contributions from the torsions in this pattern with coordinates from *xcfg
	msg.enter("Pattern::torsionForces");
	int n,i,j,k,l,aoff,m1;
	static Vec3<double> rij, rkj, rlk, xpj, xpk, dcos_dxpj, dcos_dxpk, temp;
	static Mat3<double> dxpj_dij, dxpj_dkj, dxpk_dkj, dxpk_dlk;
	static double cosphi, phi, dp, forcek, period, eq, mag_ij, mag_kj, mag_lk, mag_xpj, mag_xpk, du_dphi, dphi_dcosphi;
	static Vec3<double> fi, fj, fk, fl;
	ForcefieldBound *ffb;
	static double k0, k1, k2, k3, k4, s;
	PatternBound *pb;
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	aoff = startAtom_;
	for (m1=0; m1<nMolecules_; m1++)
	{
		for (pb = torsions_.first(); pb != NULL; pb = pb->next)
		{
			// Calculate general components of the torsion forces
			// Grab atomic indices
			i = pb->atomId(0) + aoff;
			j = pb->atomId(1) + aoff;
			k = pb->atomId(2) + aoff;
			l = pb->atomId(3) + aoff;
			ffb = pb->data();
			// Calculate vectors between atoms
			rij = cell->mimd(modelatoms[i]->r(), modelatoms[j]->r());
			rkj = cell->mimd(modelatoms[k]->r(), modelatoms[j]->r());
			rlk = cell->mimd(modelatoms[l]->r(), modelatoms[k]->r());
			mag_ij = rij.magnitude();
			mag_kj = rkj.magnitude();
			mag_lk = rlk.magnitude();
// 			printf("i-j-k-l %i-%i-%i-%i MAGs %f %f %f\n",i,j,k,l, mag_ij,mag_kj, mag_lk);
			// Calculate cross products and torsion angle formed (in radians)
			xpj = rij * rkj;
			xpk = rlk * rkj;
			mag_xpj = xpj.magAndNormalise();
			mag_xpk = xpk.magAndNormalise();
			dp = xpj.dp(xpk);
			if (dp < -1.0) dp = -1.0;
			else if (dp > 1.0) dp = 1.0;
			phi = acos(dp);
// 			printf("i-j-k-l %i-%i-%i-%i DP %16.13f %16.13f %16.13f %16.13f\n",i,j,k,l, mag_xpj, mag_xpk, dp, phi);
			if (phi < 0.0) { if (phi > -1e-8) phi = -1e-8; }
			else if (phi < 1e-8) phi = 1e-8;
			// Derivative w.r.t. change in torsion angle
			dphi_dcosphi = -1.0 / sin(phi);
			// Pathological case where phi = 0...
			if (phi < 1E-8) dphi_dcosphi = 0.0;

			/* Construct derivatives of perpendicular axis (cross product) w.r.t. component vectors.
			E.g.
			    d (rij x rkj) 
			    ------------- = rij[cp(n+2)] * U[cp(n+1)] - rij[cp(n+1)] * U[cp(n+2)]
			       d rkj[n]  
			
			where cp is a cylic permutation spanning {0,1,2} == {x,y,z}, and U[n] is a unit vector in the n direction.
			So,
			    d (rij x rkj) 
			    ------------- = rij[2] * U[1] - rij[1] * U[2]
			       d rkj[0]  
					  = rij[z] * (0,1,0) - rij[y] * (0,0,1)

					  = (0,rij[z],0) - (0,0,rij[y])

					  = (0,rij[z],-rij[y])
			*/
			dxpj_dij = make_cp_mat(&rkj);
			temp = -rij;
			dxpj_dkj = make_cp_mat(&temp);
			temp = -rlk;
			dxpk_dkj = make_cp_mat(&temp);
			dxpk_dlk = make_cp_mat(&rkj);
			// Construct derivatives of cos(phi) w.r.t. perpendicular axes
			dcos_dxpj = (xpk - xpj * dp) / mag_xpj;
			dcos_dxpk = (xpj - xpk * dp) / mag_xpk;

			// Generate derivative of energy function (placed in 'du_dphi')
			switch (pb->data()->torsionStyle())
			{
				case (TorsionFunctions::None):
					msg.print("Warning: No function is specified for torsion force %i-%i-%i-%i.\n", i, j, k, l);
				case (TorsionFunctions::Ignore):
					du_dphi = 0.0;
					break;
				case (TorsionFunctions::Cosine): 
					// dU/dphi = forcek * period * s * -sin(period*phi - eq)
					forcek = ffb->parameter(TorsionFunctions::CosineK);
					eq = ffb->parameter(TorsionFunctions::CosineEq) / DEGRAD;
					period = ffb->parameter(TorsionFunctions::CosineN);
					s = ffb->parameter(TorsionFunctions::CosineS);
					du_dphi = dphi_dcosphi * period * forcek * s * -sin(period*phi - eq);
					break;
				case (TorsionFunctions::Cos3):
					// dU/dphi = 0.5 * ( -k1*sin(phi) + 2 * k2*sin(2*phi) - 3 * k3*(sin(3*phi)) )
					k1 = ffb->parameter(TorsionFunctions::Cos3K1);
					k2 = ffb->parameter(TorsionFunctions::Cos3K2);
					k3 = ffb->parameter(TorsionFunctions::Cos3K3);
					du_dphi = dphi_dcosphi * 0.5 * ( -k1*sin(phi) + 2.0*k2*sin(2.0*phi) - 3.0*k3*sin(3.0*phi));
					break;
				case (TorsionFunctions::Cos3C):
					// dU/dphi = 0.5 * ( -k1*sin(phi) + 2 * k2*sin(2*phi) - 3 * k3*(sin(3*phi)) )
					k1 = ffb->parameter(TorsionFunctions::Cos3CK1);
					k2 = ffb->parameter(TorsionFunctions::Cos3CK2);
					k3 = ffb->parameter(TorsionFunctions::Cos3CK3);
					du_dphi = dphi_dcosphi * 0.5 * ( -k1*sin(phi) + 2.0*k2*sin(2.0*phi) - 3.0*k3*sin(3.0*phi));
					break;
				case (TorsionFunctions::Cos4):
					// dU/dphi = 0.5 * ( -k1*sin(phi) + 2 * k2*sin(2*phi) - 3 * k3*(sin(3*phi)) + 4 * k4*(sin(4*phi)))
					k1 = -ffb->parameter(TorsionFunctions::Cos4K1);
					k2 = 2.0 * ffb->parameter(TorsionFunctions::Cos4K2);
					k3 = -3.0 * ffb->parameter(TorsionFunctions::Cos4K3);
					k4 = 4.0 * ffb->parameter(TorsionFunctions::Cos4K4);
					du_dphi = dphi_dcosphi * 0.5 * ( k1*sin(phi) + k2*sin(2.0*phi) + k3*sin(3.0*phi) + k4*sin(4.0*phi));
					break;
				case (TorsionFunctions::CosCos):
					// dU/dphi = 0.5 * k * n * cos(n*eq) * sin(n*phi)
					k1 = ffb->parameter(TorsionFunctions::CosCosK);
					period = ffb->parameter(TorsionFunctions::CosCosN);
					eq = ffb->parameter(TorsionFunctions::CosCosEq) / DEGRAD;
					du_dphi = dphi_dcosphi * 0.5 * k1 * period * cos(period*eq)*sin(period*phi);
					break;
				case (TorsionFunctions::Dreiding):
					// dU/dphi = 0.5 * k * (1 - cos(n*(theta-eq))
					k1 = ffb->parameter(TorsionFunctions::DreidingK);
					period = ffb->parameter(TorsionFunctions::DreidingN);
					eq = ffb->parameter(TorsionFunctions::DreidingEq) / DEGRAD;
					du_dphi = dphi_dcosphi * 0.5 * k1 * sin(n*(phi - eq));
					break;
				default:
					printf("No equation coded for torsion force of type '%s'.\n",  TorsionFunctions::TorsionFunctions[ffb->torsionStyle()].name);
					break;
			}
// 			printf("i-j-k-l %i-%i-%i-%i %f %f %f\n",i,j,k,l, phi, dphi_dcosphi, du_dphi);
			// Calculate forces
			fi.x = -du_dphi * dcos_dxpj.dp(dxpj_dij.rows[0]);
			fi.y = -du_dphi * dcos_dxpj.dp(dxpj_dij.rows[1]);
			fi.z = -du_dphi * dcos_dxpj.dp(dxpj_dij.rows[2]);

			fj.x = -du_dphi * ( dcos_dxpj.dp( -dxpj_dij.rows[0] - dxpj_dkj.rows[0] ) - dcos_dxpk.dp(dxpk_dkj.rows[0]) );
			fj.y = -du_dphi * ( dcos_dxpj.dp( -dxpj_dij.rows[1] - dxpj_dkj.rows[1] ) - dcos_dxpk.dp(dxpk_dkj.rows[1]) );
			fj.z = -du_dphi * ( dcos_dxpj.dp( -dxpj_dij.rows[2] - dxpj_dkj.rows[2] ) - dcos_dxpk.dp(dxpk_dkj.rows[2]) );

			fk.x = -du_dphi * ( dcos_dxpk.dp( dxpk_dkj.rows[0] - dxpk_dlk.rows[0] ) + dcos_dxpj.dp(dxpj_dkj.rows[0]) );
			fk.y = -du_dphi * ( dcos_dxpk.dp( dxpk_dkj.rows[1] - dxpk_dlk.rows[1] ) + dcos_dxpj.dp(dxpj_dkj.rows[1]) );
			fk.z = -du_dphi * ( dcos_dxpk.dp( dxpk_dkj.rows[2] - dxpk_dlk.rows[2] ) + dcos_dxpj.dp(dxpj_dkj.rows[2]) );

			fl.x = -du_dphi * dcos_dxpk.dp(dxpk_dlk.rows[0]);
			fl.y = -du_dphi * dcos_dxpk.dp(dxpk_dlk.rows[1]);
			fl.z = -du_dphi * dcos_dxpk.dp(dxpk_dlk.rows[2]);

			modelatoms[i]->f() -= fi;
			modelatoms[j]->f() -= fj;
			modelatoms[k]->f() -= fk;
			modelatoms[l]->f() -= fl;

		}
		aoff += nAtoms_;
	}
	msg.exit("Pattern::torsionForces");
}
