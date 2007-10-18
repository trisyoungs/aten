/*
	*** Ewald sum energy / force calculation
	*** src/energy/ewald.cpp

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

// Ref: "Long-Range Interactions in Many-Particle Simulation", P. Gibbon and G. Sutmann
//	In "Quantum Simulations of Complex Many-Body Systems; From Theory to Algorithms"
//	NIC Series, Vol. 10, ISBN 3-00-009057, pp. 467-506, 2002.

#include <math.h>
#include "classes/pattern.h"
#include "classes/fourier.h"
#include "classes/forcefield.h"
#include "templates/vector3.h"
#include "classes/energystore.h"
#include "base/prefs.h"
#include "base/mathfunc.h"

void prefs_data::ewald_estimate_parameters(unitcell *cell)
{
	// Estimate alpha and kmax parameters based on a given precision value
	dbg_begin(DM_CALLS,"pattern::ewald_estimate_parameters");
	if (prefs.valid_ewaldauto)
	{
		dbg_end(DM_CALLS,"pattern::ewald_estimate_parameters");
		return;
	}
        // Estimate ewald_alpha
	// The precision is stored as 'prei' multiplied by 10^prepow
        double tolerance = sqrt( fabs( log(ewald_precision*elec_cut) ) );
        ewald_alpha = sqrt( fabs( log(ewald_precision*elec_cut*tolerance) ) )/elec_cut;
        // Estimate kmax
        tolerance = sqrt( -log( ewald_precision*elec_cut*( (2.0*tolerance*ewald_alpha)*(2.0*tolerance*ewald_alpha) ) ) );
	// TODO Ewald estimates for other cell types
	int k;
	switch (cell->get_type())
	{
		case (CT_CUBIC):
			k = (int) round(0.25 + cell->get_lengths().x*ewald_alpha*tolerance/PI);
			ewald_kvec.set(k,k,k);
			break;
		default:
			printf("No estimation of parameters is available yet for this cell type.\n");
			break;
	}
	msg(DM_NONE,"pattern::ewald_estimate_parameters : For precision = %6.4e, alpha = %8.6f and kmax = %i %i %i.\n", ewald_precision, ewald_alpha, ewald_kvec.x, ewald_kvec.y, ewald_kvec.z);
	valid_ewaldauto = TRUE;
	dbg_end(DM_CALLS,"pattern::ewald_estimate_parameters");
}

// Ewald Energy Real-space contributions
//				       N   N		    erfc(alpha * |rij + n|) 
// 		   E(real) = 0.5 * E'  E   E  q(i) * q(j) * -----------------------
// (p 474)			   n  i=1 j=1			   |rij + n|
// 'n' is box vector - here we only consider the minimimum image coordinates of the atoms in the central box (n=0)
// Factor of 1/2 is not required in the summation since the sums go from i=0,N-1 and j=i,N

void pattern::ewald_real_intrapattern_energy(model *srcmodel, energystore *estore)
{
	// Calculate a real-space contribution to the Ewald sum.
	// Internal interaction of atoms in individual molecules within the pattern is considered.
	dbg_begin(DM_CALLS,"pattern::ewald_real_intrapattern_energy");
	static int n,i,j,aoff,m1;
	static vec3<double> mim_i;
	static double rij, energy_inter, energy_intra, energy, cutoff, alpha;
	patbound *pb;
	cutoff = prefs.get_elec_cutoff();
	alpha = prefs.get_ewald_alpha();
	energy_inter = 0.0;
	energy_intra = 0.0;
	atom **modelatoms = srcmodel->get_staticatoms();
	unitcell *cell = &srcmodel->cell;
	aoff = startatom;
	for (m1=0; m1<nmols; m1++)
	{
		// Loop over atom pairs that are either unbound or separated by more than three bonds
		for (i=0; i<natoms-1; i++)
		{
			for (j=i+1; j<natoms; j++)
			{
				if ((conmat[i][j] > 3) || (conmat[i][j] == 0))
				{
					mim_i = cell->mimd(modelatoms[i+aoff]->r, modelatoms[j+aoff]->r);
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					energy  = (modelatoms[i+aoff]->get_charge() * modelatoms[j+aoff]->get_charge()) * cserfc(alpha*rij) / rij;
					conmat[i][j] == 0 ? energy_inter += energy : energy_intra += energy;
				}
			}
		}
		// Add on scaled contributions from torsion
		for (pb = torsions.first(); pb != NULL; pb = pb->next)
		{
			i = pb->get_atomid(0) + aoff;
			j = pb->get_atomid(3) + aoff;
			mim_i = cell->mimd(modelatoms[i]->r, modelatoms[j]->r);
			rij = mim_i.magnitude();
			if (rij > cutoff) continue;
			energy  = (modelatoms[i]->get_charge() * modelatoms[j]->get_charge()) * cserfc(alpha*rij) / rij;
			energy *= pb->get_data()->get_params().data[TF_ESCALE];
			energy_intra += energy;
		}
		aoff += natoms;
	}
	energy_intra *= prefs.elec_convert;
	energy_inter *= prefs.elec_convert;
	estore->add(ET_EWALDREALINTRA,energy_intra,id);
	estore->add(ET_EWALDREALINTER,energy_inter,id,id);
	//estore->ewald_real_intra[id] += energy_intra;
	//estore->ewald_real_inter[id][id] += energy_inter;
	dbg_end(DM_CALLS,"pattern::ewald_real_intrapattern_energy");
}

void pattern::ewald_real_interpattern_energy(model *srcmodel, pattern *xpnode, energystore *estore)
{
	// Calculate the real-space Ewald contribution to the energy from interactions between different molecules
	// of this pnode and the one supplied. Contributions to the sum from the inner loop of atoms (a2) is summed into
	// 'energy; before multiplication by the charge of the second atom (a1)
	dbg_begin(DM_CALLS,"pattern::ewald_real_interpattern_energy");
	static int n1,n2,i,j,aoff1,aoff2,m1,m2,start,finish,atomi,atomj;
	static vec3<double> mim_i;
	static double rij, energy_inter, energy, cutoff, alpha;
	cutoff = prefs.get_elec_cutoff();
	alpha = prefs.get_ewald_alpha();
	atom **modelatoms = srcmodel->get_staticatoms();
	unitcell *cell = &srcmodel->cell;
	energy_inter = 0.0;
	aoff1 = startatom;
	 // When we are considering the same node with itself, calculate for "m1=1,T-1 m2=2,T"
        this == xpnode ? finish = nmols - 1 : finish = nmols;
	for (m1=0; m1<finish; m1++)
	{
		this == xpnode ? start = m1 + 1 : start = 0;
	       	aoff2 = xpnode->startatom + start*xpnode->natoms;
		for (m2=start; m2<xpnode->nmols; m2++)
		{
			for (i=0; i<natoms; i++)
			{
				atomi = i + aoff1;
				energy = 0.0;
				for (j=0; j<xpnode->natoms; j++)
		  		{
					atomj = j + aoff2;
					mim_i = cell->mimd(modelatoms[atomi]->r, modelatoms[atomj]->r);
					rij = mim_i.magnitude();
					if (rij < cutoff) energy  += (modelatoms[atomj]->get_charge() * cserfc(alpha*rij) / rij);
				}
				energy *= modelatoms[atomi]->get_charge();
				energy_inter += energy;
			}
			aoff2 += xpnode->natoms;
		}
		aoff1 += natoms;
	}
	energy_inter = energy_inter * prefs.elec_convert;
	estore->add(ET_EWALDREALINTER,energy_inter,id,xpnode->id);
	//estore->ewald_real_inter[id][xpnode->id] += energy_inter;
	dbg_end(DM_CALLS,"pattern::ewald_real_interpattern_energy");
}

// Ewald Reciprocal energy contributions
//			    2*pi    N   N					    -ksq	 1
//		E(recip) =  ---- E' E   E  q(i) * q(j) * exp(ik.(rj - ri)) * exp( --------- ) * ---
//			    L**3 k i=1 j=1					  4*alphasq	ksq

void pattern::ewald_reciprocal_energy(model *srcmodel, pattern *firstp, int npats, energystore *estore)
{
	// Calculate the reciprocal contribution of all atoms to the Ewald sum.
	// Only needs to be called once from an arbitrary pattern.
	dbg_begin(DM_CALLS,"pattern::ewald_reciprocal_energy");
	static int kx, ky, kz, i, n, kmax, finalatom;
	static vec3<double> kvec;
	static mat3<double> rcell;
	static double cutoff, mag, magsq, exp1, alphasq, pos, xycos, xysin, xyzcos, xyzsin;
	static double factor, alpha, energy_inter;
	double sumcos[npats], sumsin[npats];
	alpha = prefs.get_ewald_alpha();
	atom **modelatoms = srcmodel->get_staticatoms();

	// Get reciprocal volume and cell vectors
	factor = fourier.cell->get_rvolume() * prefs.elec_convert;

	// TODO Assume a cubic cell for now. Other cell types!
	rcell = fourier.cell->get_recip().transpose();
	cutoff = fourier.kvec.x * rcell.rows[0].x * 1.05;
	alphasq = alpha * alpha;
	kmax = fourier.kmax;
	for (kx=-fourier.kvec.x; kx<=fourier.kvec.x; kx++)
	for (ky=-fourier.kvec.y; ky<=fourier.kvec.y; ky++)
	for (kz=-fourier.kvec.z; kz<=fourier.kvec.z; kz++)
	{
		if ((kx == 0) && (ky == 0) && (kz == 0)) continue;
		// TODO Cubic / orthorhombic cell assumed!
		kvec.x = kx * rcell.rows[0].x;
		kvec.y = ky * rcell.rows[1].y;
		kvec.z = kz * rcell.rows[2].z;
		mag = kvec.magnitude();
		if (mag > cutoff) continue;
		magsq = mag * mag;
		// Now sum contributions over atoms, broken up into patterns
		pattern *p = firstp;
		while (p != NULL)
		{
			sumcos[p->id] = 0.0;
			sumsin[p->id] = 0.0;
			finalatom = p->startatom + p->totalatoms;
			for (i=p->startatom; i<finalatom; i++)
			{
				// Calculate k-vector (x*y)
				xycos = fourier.rcos[abs(kx)][i].x * fourier.rcos[abs(ky)][i].y -
					fourier.rsin[kmax+kx][i].x * fourier.rsin[kmax+ky][i].y;
				xysin = fourier.rcos[abs(kx)][i].x * fourier.rsin[kmax+ky][i].y +
					fourier.rsin[kmax+kx][i].x * fourier.rcos[abs(ky)][i].y;
				// Calculate k-vector (xy*z);
				xyzcos = xycos * fourier.rcos[abs(kz)][i].z - xysin * fourier.rsin[kmax+kz][i].z;
				xyzsin = xycos * fourier.rsin[kmax+kz][i].z + xysin * fourier.rcos[abs(kz)][i].z;
				sumcos[p->id] += modelatoms[i]->get_charge() * xyzcos;
				sumsin[p->id] += modelatoms[i]->get_charge() * xyzsin;
			}
			p = p->next;
		}
		// Calculate energy contributions from the interactions of patterns
		exp1=exp(-magsq/(4.0*alphasq))/magsq * factor;
		for (i=0; i<npats; i++)
			for (n=i; n<npats; n++)
			{
				estore->add(ET_EWALDRECIPINTER,energy_inter,i,n);
				energy_inter = exp1*(sumcos[i]*sumcos[n] + sumsin[i]*sumsin[n]);
				//estore->ewald_recip_inter[i][n] += exp1*(sumcos[i]*sumcos[n] + sumsin[i]*sumsin[n]);
			}
	}
	dbg_end(DM_CALLS,"pattern::ewald_reciprocal_energy");
}

// Ewald Corrections
//			    alpha   N
//		E(self) = - ------  E  q(i) * q(i) 
//			    sqrtpi i=1
//
//				    * *		      erf(alpha(rij))
//		E(molecular) = - E  E E q(i) * q(j) * ---------------
//				 m  i j			    rij
// Sums over i=* and j=* indicate excluded interactions, i.e. bond i-j, angle i-x-j and torsion i-x-x-j.

void pattern::ewald_correct_energy(model *srcmodel, energystore *estore)
{
	// Calculate corrections to the Ewald sum energy
	dbg_begin(DM_CALLS,"pattern::ewald_correct_energy");
	static int aoff, m1, i, j;
	static double molcorrect, energy, qprod, rij, chargesum, alpha;
	alpha = prefs.get_ewald_alpha();
	static vec3<double> mim_i;
	atom **modelatoms = srcmodel->get_staticatoms();
	unitcell *cell = &srcmodel->cell;

	// Correct the reciprocal Ewald energy for charges interacting with themselves
	chargesum = 0.0;
	aoff = startatom;
	for (m1=0; m1<nmols; m1++)
	{
		for (i=0; i<natoms; i++) chargesum += (modelatoms[i+aoff]->get_charge() * modelatoms[i+aoff]->get_charge());
		aoff += natoms;
	}
	energy = (alpha/SQRTPI) * chargesum * prefs.elec_convert;
	estore->add(ET_EWALDSELF,energy,id);
	//estore->ewald_self_correct[id] += energy;
	// Correct the reciprocal Ewald energy for molecular interactions, i.e. bond, angle, torsion exclusions
	molcorrect = 0.0;
	aoff = startatom;
	for (m1=0; m1<nmols; m1++)
	{
		for (i=0; i<natoms-1; i++)
			for (j=i+1; j<natoms; j++)
			{
				if ((conmat[i][j] < 4) && (conmat[i][j] != 0))
				{
					// A molecular correction is needed for this atom pair.
					// For torsions (conmat == 3) scale by escale.
					qprod = modelatoms[i+aoff]->get_charge() * modelatoms[j+aoff]->get_charge();
					if (conmat[i][j] == 3) qprod *= 0.5;
					mim_i = cell->mimd(modelatoms[i+aoff]->r, modelatoms[j+aoff]->r);
					rij = mim_i.magnitude();
					molcorrect += qprod *( cserf(alpha*rij)/rij );
				}
			}
		aoff += natoms;
	}
	energy = molcorrect * prefs.elec_convert;
	estore->add(ET_EWALDMOL,energy,id);
	//estore->ewald_mol_correct[id] += energy;
	dbg_end(DM_CALLS,"pattern::ewald_correct_energy");
}

// Ewald Real-space forces
//			    N-1  N  q(i) * q(j)				2*alpha*rij			       -->
//		F(real) = E' E   E  ----------- * ( erfc(alpha * rij) + ----------- * exp(-(alpha*rij)**2) ) * rij
//			  n i=1 j>i   rij**3				   sqrtpi
 
void pattern::ewald_real_intrapattern_forces(model *srcmodel)
{
	// Calculate a real-space forces in the Ewald sum.
	// Internal interaction of atoms in individual molecules within the pattern is considered.
	dbg_begin(DM_CALLS,"pattern::ewald_real_intrapattern_forces");
	static int n, i, j, aoff, m1, atomi, atomj;
	static vec3<double> mim_i, tempf, f_i;
	static double rij, factor, qqrij3, alpharij, cutoff, alpha;
	patbound *pb;
	cutoff = prefs.get_elec_cutoff();
	alpha = prefs.get_ewald_alpha();
	atom **modelatoms = srcmodel->get_staticatoms();
	unitcell *cell = &srcmodel->cell;

	aoff = startatom;
	for (m1=0; m1<nmols; m1++)
	{
		// Add force contributions for atom pairs that are unbond or separated by at least three bonds
		for (i=0; i<natoms; i++)
		{
			atomi = i+aoff;
			// Copy i's forces from the main array into a temporary array
			f_i = modelatoms[atomi]->f;
			for (j=i+1; j<natoms; j++)
			{
				atomj = j+aoff;
				if ((conmat[i][j] > 3) || (conmat[i][j] == 0))
				{
					mim_i = cell->mimd(modelatoms[atomi]->r, modelatoms[atomj]->r);
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					alpharij = alpha * rij;
					factor = cserfc(alpharij) + 2.0*alpharij/SQRTPI * exp(-(alpharij*alpharij));
					qqrij3 = (modelatoms[atomi]->get_charge() * modelatoms[atomj]->get_charge()) / (rij * rij * rij);
					factor = factor * qqrij3 * prefs.elec_convert;
					// Sum forces
					tempf = mim_i * factor;
					//tempf.y = mim_i.y * factor;
					//tempf.z = mim_i.z * factor;
					f_i += tempf;
					modelatoms[atomj]->f -= tempf;
				}
			}
			// Re-store forces on atom i
			modelatoms[atomi]->f = f_i;
		}
		// Add on scaled contributions from torsions
		for (pb = torsions.first(); pb != NULL; pb = pb->next)
		{
			i = pb->get_atomid(0) + aoff;
			j = pb->get_atomid(3) + aoff;
			mim_i = cell->mimd(modelatoms[i]->r, modelatoms[j]->r);
			rij = mim_i.magnitude();
			if (rij > cutoff) continue;
			alpharij = alpha * rij;
			factor = cserfc(alpharij) + 2.0*alpharij/SQRTPI * exp(-(alpharij*alpharij));
			qqrij3 = (modelatoms[i]->get_charge() * modelatoms[j]->get_charge()) / (rij * rij * rij);
			factor = factor * qqrij3 * prefs.elec_convert;
			factor *= pb->get_data()->get_params().data[TF_ESCALE];
			// Sum forces
			tempf = mim_i * factor;
			modelatoms[i]->f += tempf;
			modelatoms[j]->f -= tempf;
		}
		aoff += natoms;
	}
	dbg_end(DM_CALLS,"pattern::ewald_real_intrapattern_forces");
}

void pattern::ewald_real_interpattern_forces(model *srcmodel, pattern *xpnode)
{
	// Calculate the real-space Ewald forces from interactions between different molecules
	// of this pnode and the one supplied. 
	dbg_begin(DM_CALLS,"pattern::ewald_real_interpattern_forces");
	int n1,n2,i,j,aoff1,aoff2,m1,m2,start,finish,atomi,atomj;
	static vec3<double> mim_i, f_i, tempf;
	static double rij, factor, alpharij, qqrij3, cutoff, alpha;
	cutoff = prefs.get_elec_cutoff();
	alpha = prefs.get_ewald_alpha();
	atom **modelatoms = srcmodel->get_staticatoms();
	unitcell *cell = &srcmodel->cell;

	aoff1 = startatom;
	 // When we are considering the same node with itself, calculate for "m1=1,T-1 m2=2,T"
        this == xpnode ? finish = nmols - 1 : finish = nmols;
	for (m1=0; m1<finish; m1++)
	{
		this == xpnode ? start = m1 + 1 : start = 0;
	       	aoff2 = xpnode->startatom + start*xpnode->natoms;
		for (m2=start; m2<xpnode->nmols; m2++)
		{
			for (i=0; i<natoms; i++)
			{
				atomi = i + aoff1;
				// Copy the current forces on i
				f_i = modelatoms[atomi]->f;
				for (j=0; j<xpnode->natoms; j++)
		  		{
					atomj = j + aoff2;
					mim_i = cell->mimd(modelatoms[atomi]->r ,modelatoms[atomj]->r);
					rij = mim_i.magnitude();
					if (rij < cutoff)
					{
						alpharij = alpha * rij;
						factor = cserfc(alpharij) + 2.0*alpharij/SQRTPI * exp(-(alpharij*alpharij));
						qqrij3 = (modelatoms[atomi]->get_charge() * modelatoms[atomj]->get_charge()) / (rij * rij * rij);
						factor = factor * qqrij3 * prefs.elec_convert;
						// Sum forces
						tempf.x = mim_i.x * factor;
						tempf.y = mim_i.y * factor;
						tempf.z = mim_i.z * factor;
						f_i += tempf;
						modelatoms[atomj]->f -= tempf;
					}
				}
				// Store the new forces on atom i
				modelatoms[atomi]->f = f_i;
			}
			aoff2 += xpnode->natoms;
		}
		aoff1 += natoms;
	}
	dbg_end(DM_CALLS,"pattern::ewald_real_interpattern_forces");
}

// Reciprocal space forces
//				 N
//		  F(recip) = E   E q(j) 
//			    k/=0 j

void pattern::ewald_reciprocal_forces(model *srcmodel)
{
	// Calculate the reciprocal force contribution to the Ewald sum.
	// Must be called for the first pattern in the list only!
	dbg_begin(DM_CALLS,"pattern::ewald_reciprocal_forces");
	static int kx, ky, kz, i, n, kmax;
	static vec3<double> kvec;
	static mat3<double> rcell;
	static double cutoff, mag, magsq, exp1, alphasq, factor, force, pos, sumcos, sumsin, xycos, xysin, alpha;
	double xyzcos[srcmodel->get_natoms()], xyzsin[srcmodel->get_natoms()];
	alpha = prefs.get_ewald_alpha();
	atom **modelatoms = srcmodel->get_staticatoms();

	// Grab the reciprocal unit cell
	rcell = fourier.cell->get_recip();

	// Assume a cubic cell for now.
	cutoff = fourier.kvec.x * rcell.rows[0].x * 1.05;
	alphasq = alpha * alpha;
	kmax = fourier.kmax;
	factor = 2.0 * fourier.cell->get_rvolume() * prefs.elec_convert;

	for (kx=-fourier.kvec.x; kx<=fourier.kvec.x; kx++)
	for (ky=-fourier.kvec.y; ky<=fourier.kvec.y; ky++)
	for (kz=-fourier.kvec.z; kz<=fourier.kvec.z; kz++)
	{
		if ((kx == 0) && (ky == 0) && (kz == 0)) continue;
		// TODO Cubic cell assumed!
		kvec.x = kx * rcell.rows[0].x;
		kvec.y = ky * rcell.rows[1].y;
		kvec.z = kz * rcell.rows[2].z;
		mag = kvec.magnitude();
		if (mag > cutoff) continue;
		magsq = mag * mag;
		sumcos = 0.0;
		sumsin = 0.0;
		for (i=0; i<fourier.natoms; i++)
		{
			// Calculate k-vector (x*y)
			xycos = fourier.rcos[abs(kx)][i].x * fourier.rcos[abs(ky)][i].y -
				fourier.rsin[kmax+kx][i].x * fourier.rsin[kmax+ky][i].y;
			xysin = fourier.rcos[abs(kx)][i].x * fourier.rsin[kmax+ky][i].y +
				fourier.rsin[kmax+kx][i].x * fourier.rcos[abs(ky)][i].y;
			// Calculate k-vector (xy*z);
			xyzcos[i] = (xycos * fourier.rcos[abs(kz)][i].z - xysin * fourier.rsin[kmax+kz][i].z) * modelatoms[i]->get_charge();
			xyzsin[i] = (xycos * fourier.rsin[kmax+kz][i].z + xysin * fourier.rcos[abs(kz)][i].z) * modelatoms[i]->get_charge();
			sumcos += xyzcos[i];
			sumsin += xyzsin[i];
		}
	//printf("%i %i %i %i %12.8f %12.8f\n",kx,ky,kz,i,sumcos,sumsin);
		// Calculate forces
		exp1= exp(-magsq/(4.0*alphasq))/magsq;
		for (i=0; i<fourier.natoms; i++)
		{
			force = exp1 * (xyzsin[i]*sumcos - xyzcos[i]*sumsin) * factor;
	//printf("force = %20.14e\n",force);
			modelatoms[i]->f += kvec * force;
	//if (i == 0) printf("%i %i %i  %8.4f %8.4f %8.4f %8.4f\n",kx,ky,kz,force,kvec.x,kvec.y,kvec.z);
		}
	}
	dbg_end(DM_CALLS,"pattern::ewald_reciprocal_forces");
}

void pattern::ewald_correct_forces(model *srcmodel)
{
	// Correct the Ewald forces due to bond / angle / torsion exclusions
	dbg_begin(DM_CALLS,"pattern::ewald_correct_forces");
	static int n, i, j, aoff, m1, atomi, atomj;
	static vec3<double> mim_i, tempf, f_i;
	static double rij, factor, qqrij3, alpharij, cutoff, alpha;
	patbound *pb;
	cutoff = prefs.get_elec_cutoff();
	alpha = prefs.get_ewald_alpha();
	atom **modelatoms = srcmodel->get_staticatoms();
	unitcell *cell = &srcmodel->cell;

	aoff = startatom;
	for (m1=0; m1<nmols; m1++)
	{
		// Subtract forces from intramolecular bonds and angles
		for (i=0; i<natoms-1; i++)
		{
			atomi = i+aoff;
			// Copy i's forces from the main array into a temporary array
			f_i = modelatoms[atomi]->f;
			for (j=i+1; j<natoms; j++)
			{
				atomj = j+aoff;
				if ((conmat[i][j] < 3) && (conmat[i][j] > 0))
				{
					mim_i = cell->mimd(modelatoms[atomi]->r, modelatoms[atomj]->r);
					rij = mim_i.magnitude();
					if (rij < cutoff)
					{
						alpharij = alpha * rij;
						//factor = erf(alpharij) - 2.0*alpharij/SQRTPI * exp(-(alpharij*alpharij));
						factor = cserf(alpharij) - 2.0*alpharij/SQRTPI * exp(-(alpharij*alpharij));
						qqrij3 = (modelatoms[atomi]->get_charge() * modelatoms[atomj]->get_charge()) / (rij * rij * rij);
						factor = factor * qqrij3 * prefs.elec_convert;
						// Sum forces
						tempf = mim_i * factor;
						f_i -= tempf;
						modelatoms[atomj]->f += tempf;
					}
				}
			}
			// Re-store forces on atom i
			modelatoms[atomi]->f = f_i;
		}
		// Subtract scaled contributions from torsions
		for (pb = torsions.first(); pb != NULL; pb = pb->next)
		{
			i = pb->get_atomid(0) + aoff;
			j = pb->get_atomid(3) + aoff;
			mim_i = cell->mimd(modelatoms[i]->r, modelatoms[j]->r);
			rij = mim_i.magnitude();
			if (rij > cutoff) continue;
			alpharij = alpha * rij;
			factor = cserfc(alpharij) + 2.0*alpharij/SQRTPI * exp(-(alpharij*alpharij));
			qqrij3 = (modelatoms[i]->get_charge() * modelatoms[j]->get_charge()) / (rij * rij * rij);
			factor = factor * qqrij3 * prefs.elec_convert;
			factor *= pb->get_data()->get_params().data[TF_ESCALE];
			// Sum forces
			tempf = mim_i * factor;
			modelatoms[i]->f -= tempf;
			modelatoms[j]->f += tempf;
		}
		aoff += natoms;
	}
	dbg_end(DM_CALLS,"pattern::ewald_correct_forces");
}

