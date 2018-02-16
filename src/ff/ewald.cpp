/*
	*** Ewald sum energy / force calculation
	*** src/ff/ewald.cpp
	Copyright T. Youngs 2007-2018

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
#include "templates/vector3.h"
#include "base/fourierdata.h"
#include "base/pattern.h"
#include "base/prefs.h"
#include "model/model.h"

ATEN_USING_NAMESPACE

// Estimate alpha and kmax parameters based on a given precision value
void Prefs::estimateEwaldParameters(UnitCell& cell)
{
	Messenger::enter("Prefs::estimateEwaldParameterss");
	if (prefs.hasValidEwaldAuto())
	{
		Messenger::exit("Prefs::estimateEwaldParameters");
		return;
	}
        // Estimate ewaldAlpha - uses same method as in DL_POLY. No reference? Page 108 in v2.18 manual.
        double tolerance = sqrt( fabs( log(ewaldPrecision_.value()*elecCutoff_) ) );
        ewaldAlpha_ = sqrt( fabs( log(ewaldPrecision_.value()*elecCutoff_*tolerance) ) )/elecCutoff_;
        // Estimate kmax
        tolerance = sqrt( -log( ewaldPrecision_.value()*elecCutoff_*( (2.0*tolerance*ewaldAlpha_)*(2.0*tolerance*ewaldAlpha_) ) ) );
	ewaldKMax_.x = (int) floor(0.25 + cell.lengths().x*ewaldAlpha_*tolerance/PI + 0.5);
	ewaldKMax_.y = (int) floor(0.25 + cell.lengths().y*ewaldAlpha_*tolerance/PI + 0.5);
	ewaldKMax_.z = (int) floor(0.25 + cell.lengths().z*ewaldAlpha_*tolerance/PI + 0.5);
	Messenger::print("Ewald parameters estimated at alpha = %8.6f and kmax = %i %i %i for a precision of %6.4e.", ewaldAlpha_, ewaldKMax_.x, ewaldKMax_.y, ewaldKMax_.z, ewaldPrecision_.value());
	validEwaldAuto_ = true;
	Messenger::exit("Prefs::estimateEwaldParameters");
}

// Ewald Energy Real-space contributions
//				       N   N		    erfc(alpha * |rij + n|) 
// 		   E(real) = 0.5 * E'  E   E  q(i) * q(j) * -----------------------
// (p 474)			   n  i=1 j=1			   |rij + n|
// 'n' is box vector - here we only consider the minimimum image coordinates of the atoms in the central box (n=0)
// Factor of 1/2 is not required in the summation since the sums go from i=0,N-1 and j=i,N

void Pattern::ewaldRealIntraPatternEnergy(Model* srcModel, EnergyStore* estore, int molecule)
{
	// Calculate a real-space contribution to the Ewald sum.
	// Internal interaction of atoms in individual molecules within the pattern is considered.
	Messenger::enter("Pattern::ewaldRealIntraPatternEnergy");
	int i,j,aoff,m1,con;
	Vec3<double> vec_ij;
	double rij, energy_inter, energy_intra, energy, cutoff, alpha;
	cutoff = prefs.elecCutoff();
	alpha = prefs.ewaldAlpha();
	energy_inter = 0.0;
	energy_intra = 0.0;
	Atom** modelatoms = srcModel->atomArray();
	UnitCell& cell = srcModel->cell();
	aoff = startAtom_;
	for (m1=(molecule == -1 ? 0 : molecule); m1<(molecule == -1 ? nMolecules_ : molecule+1); m1++)
	{
		// Loop over atom pairs that are either unbound or separated by more than two bonds
		for (i=0; i<nAtoms_-1; i++)
		{
			for (j=i+1; j<nAtoms_; j++)
			{
				con = conMatrix_[i][j];
				if ((con > 2) || (con == 0))
				{
					vec_ij = cell.mimVector(modelatoms[i+aoff]->r(), modelatoms[j+aoff]->r());
					rij = vec_ij.magnitude();
					if (rij > cutoff) continue;
					energy  = (modelatoms[i+aoff]->charge() * modelatoms[j+aoff]->charge()) * AtenMath::erfc(alpha*rij) / rij;
					con == 0 ? energy_inter += energy : energy_intra += (con == 3 ? energy * elecScaleMatrix_[i][j] : energy);
				}
			}
		}
		aoff += nAtoms_;
	}
	energy_intra *= prefs.elecConvert();
	energy_inter *= prefs.elecConvert();
	estore->add(EnergyStore::EwaldRealIntraEnergy,energy_intra,id_);
	estore->add(EnergyStore::EwaldRealInterEnergy,energy_inter,id_,id_);
	Messenger::exit("Pattern::ewaldRealIntraPatternEnergy");
}

void Pattern::ewaldRealInterPatternEnergy(Model* srcModel, Pattern* xpnode, EnergyStore* estore, int molecule)
{
	// Calculate the real-space Ewald contribution to the energy from interactions between different molecules
	// of this pnode and the one supplied. Contributions to the sum from the inner loop of atoms (a2) is summed into
	// 'energy; before multiplication by the charge of the second atom (a1)
	Messenger::enter("Pattern::ewaldRealInterPatternEnergy");
	static int i,j,aoff1,aoff2,m1,m2,finish1,start2,finish2,atomi,atomj;
	static Vec3<double> vec_ij;
	static double rij, energy_inter, energy, cutoff, alpha;
	cutoff = prefs.elecCutoff();
	alpha = prefs.ewaldAlpha();
	Atom** modelatoms = srcModel->atomArray();
	UnitCell& cell = srcModel->cell();
	energy_inter = 0.0;
	aoff1 = startAtom_;
	// When we are considering the same node with itself, calculate for "m1=1,T-1 m2=2,T"
	if ((this == xpnode) && (molecule == -1)) finish1 = nMolecules_ - 1;
	else finish1 = nMolecules_;
	for (m1=0; m1<finish1; m1++)
	{
		if (molecule == -1)
		{
			start2 = (this == xpnode ? m1 + 1 : 0);
			finish2 = xpnode->nMolecules_;
		}
		else
		{
			start2 = molecule;
			finish2 = molecule + 1;
			// If the patterns are the same we must exclude molecule == m1
			if ((this == xpnode) && (molecule == m1)) { aoff1 += nAtoms_; continue; }
		}
		aoff2 = xpnode->startAtom_ + start2*xpnode->nAtoms_;
		for (m2=start2; m2<finish2; m2++)
		{
			for (i=0; i<nAtoms_; i++)
			{
				atomi = i + aoff1;
				energy = 0.0;
				for (j=0; j<xpnode->nAtoms_; j++)
				{
					atomj = j + aoff2;
					vec_ij = cell.mimVector(modelatoms[atomi]->r(), modelatoms[atomj]->r());
					rij = vec_ij.magnitude();
					if (rij < cutoff) energy  += (modelatoms[atomj]->charge() * AtenMath::erfc(alpha*rij) / rij);
				}
				energy *= modelatoms[atomi]->charge();
				energy_inter += energy;
			}
			aoff2 += xpnode->nAtoms_;
		}
		aoff1 += nAtoms_;
	}
	energy_inter = energy_inter * prefs.elecConvert();
	estore->add(EnergyStore::EwaldRealInterEnergy,energy_inter,id_,xpnode->id_);
	Messenger::exit("Pattern::ewaldRealInterPatternEnergy");
}

// Ewald Reciprocal energy contributions
//			    2*pi    N   N					    -ksq	 1
//		E(recip) =  ---- E' E   E  q(i) * q(j) * exp(ik.(rj - ri)) * exp( --------- ) * ---
//			    L**3 k i=1 j=1					  4*alphasq	ksq

void Pattern::ewaldReciprocalEnergy(Model* srcModel, Pattern* firstp, int npats, EnergyStore* estore, int molecule)
{
	// Calculate the reciprocal contribution of all atoms to the Ewald sum.
	// Only needs to be called once from an arbitrary pattern.
	Messenger::enter("Pattern::ewaldReciprocalEnergy");
	int kx, ky, kz, i, n, finalatom;
	Vec3<double> k, cross_ab, cross_bc, cross_ca, perpl;
	Matrix rcell;
	double cutoffsq, magsq, exp1, alphasq, xycos, xysin, xyzcos, xyzsin, rvolume;
	double factor, alpha, energy_inter;
	double* sumcos, *sumsin;

	// Grab fourier data
	int kmax = srcModel->fourierData().kMax();
	Vec3<int> kVec = srcModel->fourierData().kVec();
	const Array2D< Vec3<double> >& rCos = srcModel->fourierData().rCos();
	const Array2D< Vec3<double> >& rSin = srcModel->fourierData().rSin();

	alpha = prefs.ewaldAlpha();
	Atom** modelatoms = srcModel->atomArray();
	sumcos = new double[npats];
	sumsin = new double[npats];

	// TODO
	if (molecule != -1) printf("Ewald reciprocal energy is not yet complete for indvidual molecule|system calculations.\n");

	// Get reciprocal volume and cell vectors
	rvolume = srcModel->cell().reciprocalVolume();
	factor = rvolume * TWOPI * prefs.elecConvert();

	// Cutoff is the shortest component of kVec * perpendicular reciprocal cell lengths
	rcell = srcModel->cell().reciprocal();
	cross_ab = rcell.columnAsVec3(0) * rcell.columnAsVec3(1);
	cross_bc = rcell.columnAsVec3(1) * rcell.columnAsVec3(2);
	cross_ca = rcell.columnAsVec3(2) * rcell.columnAsVec3(0);
// 	printf("CVolume = %f\n", srcModel->cell().volume());
// 	printf("CrossAB "); cross_ab.print();
// 	printf("CrossBC "); cross_bc.print();
// 	printf("CrossCA "); cross_ca.print();
// 	printf("RVolume = %f, mags = %f %f %f\n", rvolume, cross_ab.magnitude(), cross_bc.magnitude(), cross_ca.magnitude());
	perpl.set(rvolume / cross_ab.magnitude(), rvolume / cross_bc.magnitude(), rvolume / cross_ca.magnitude());
	perpl.x *= kVec.x;
	perpl.y *= kVec.y;
	perpl.z *= kVec.z;

	cutoffsq = perpl.min() * 1.05 * TWOPI;
	cutoffsq *= cutoffsq;
	alphasq = alpha * alpha;

	for (kx=-kVec.x; kx<=kVec.x; ++kx)
	for (ky=-kVec.y; ky<=kVec.y; ++ky)
	for (kz=-kVec.z; kz<=kVec.z; ++kz)
	{
		if ((kx == 0) && (ky == 0) && (kz == 0)) continue;
	/*	kvec.x = kx * rcell.rows[0].x;	    Old code assuming cubic / orthorhombic cell
		kvec.y = ky * rcell.rows[1].y;
		kvec.z = kz * rcell.rows[2].z;*/
		k.set(kx,ky,kz);
		k = (rcell * TWOPI) * k;
		magsq = k.x*k.x + k.y*k.y + k.z*k.z;
		//printf("Mag = %f, cutoff = %f\n",mag,cutoff);
		if (magsq > cutoffsq) continue;
		// Now sum contributions over atoms, broken up into patterns
		Pattern* p = firstp;
		while (p != NULL)
		{
			sumcos[p->id_] = 0.0;
			sumsin[p->id_] = 0.0;
			finalatom = p->startAtom_ + p->totalAtoms_;
			for (i=p->startAtom_; i<finalatom; i++)
			{
				// Calculate k-vector (x*y)
				xycos = rCos.constRef(abs(kx),i).x * rCos.constRef(abs(ky),i).y -
					rSin.constRef(kmax+kx,i).x * rSin.constRef(kmax+ky,i).y;
				xysin = rCos.constRef(abs(kx),i).x * rSin.constRef(kmax+ky,i).y +
					rSin.constRef(kmax+kx,i).x * rCos.constRef(abs(ky),i).y;
				// Calculate k-vector (xy*z);
				xyzcos = xycos * rCos.constRef(abs(kz),i).z - xysin * rSin.constRef(kmax+kz,i).z;
				xyzsin = xycos * rSin.constRef(kmax+kz,i).z + xysin * rCos.constRef(abs(kz),i).z;
				sumcos[p->id_] += modelatoms[i]->charge() * xyzcos;
				sumsin[p->id_] += modelatoms[i]->charge() * xyzsin;
			}
			p = p->next;
		}
		// Calculate energy contributions from the interactions of patterns
		exp1=exp(-magsq/(4.0*alphasq))/magsq * factor;
		for (i=0; i<npats; i++)
			for (n=i; n<npats; n++)
			{
				energy_inter = exp1*(sumcos[i]*sumcos[n] + sumsin[i]*sumsin[n]);
				estore->add(EnergyStore::EwaldRecipInterEnergy,energy_inter,i,n);
			}
	}
	delete sumcos;
	delete sumsin;
	Messenger::exit("Pattern::ewaldReciprocalEnergy");
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

void Pattern::ewaldCorrectEnergy(Model* srcModel, EnergyStore* estore, int molecule)
{
	// Calculate corrections to the Ewald sum energy
	Messenger::enter("Pattern::ewaldCorrectEnergy");
	static int aoff, m1, i, j, con;
	static double molcorrect, energy, qprod, rij, chargesum, alpha;
	alpha = prefs.ewaldAlpha();
	static Vec3<double> vec_ij;
	Atom** modelatoms = srcModel->atomArray();
	UnitCell& cell = srcModel->cell();

	// TODO
	if (molecule != -1) printf("Ewald energy correction is not yet complete for indvidual molecule|system calculations.\n");

	// Correct the reciprocal Ewald energy for charges interacting with themselves
	chargesum = 0.0;
	aoff = startAtom_;
	for (m1=0; m1<nMolecules_; m1++)
	{
		for (i=0; i<nAtoms_; i++) chargesum += (modelatoms[i+aoff]->charge() * modelatoms[i+aoff]->charge());
		aoff += nAtoms_;
	}
	energy = (alpha/SQRTPI) * chargesum * prefs.elecConvert();
	estore->add(EnergyStore::EwaldSelfEnergy,energy,id_);

	// Correct the reciprocal Ewald energy for molecular interactions, i.e. bond, angle, torsion exclusions
	molcorrect = 0.0;
	aoff = startAtom_;
	for (m1=0; m1<nMolecules_; m1++)
	{
		for (i=0; i<nAtoms_-1; i++)
			for (j=i+1; j<nAtoms_; j++)
			{
				con = conMatrix_[i][j];
				if ((con < 4) && (con != 0))
				{
					// A molecular correction is needed for this atom pair.
					// Take values from scaling matrix to determine degree of subtraction...
					qprod = modelatoms[i+aoff]->charge() * modelatoms[j+aoff]->charge();
					qprod *= (1.0 - elecScaleMatrix_[i][j]);
					vec_ij = cell.mimVector(modelatoms[i+aoff]->r(), modelatoms[j+aoff]->r());
					rij = vec_ij.magnitude();
					molcorrect += qprod *( AtenMath::erf(alpha*rij)/rij );
				}
			}
		aoff += nAtoms_;
	}
	energy = molcorrect * prefs.elecConvert();
	estore->add(EnergyStore::EwaldMolecularEnergy,energy,id_);
	Messenger::exit("Pattern::ewaldCorrectEnergy");
}

// Ewald Real-space forces
//			    N-1  N  q(i) * q(j)				2*alpha*rij			       -->
//		F(real) = E' E   E  ----------- * ( erfc(alpha * rij) + ----------- * exp(-(alpha*rij)**2) ) * rij
//			  n i=1 j>i   rij**3				   sqrtpi
 
void Pattern::ewaldRealIntraPatternForces(Model* srcModel)
{
	// Calculate real-space forces in the Ewald sum.
	// Internal interaction of atoms in individual molecules within the pattern is considered.
	Messenger::enter("Pattern::ewaldRealIntraPatternForces");
	int i, j, aoff, m1, atomi, atomj, con;
	Vec3<double> vec_ij, tempf, f_i;
	double rij, factor, qqrij3, alpharij, cutoff, alpha;
	cutoff = prefs.elecCutoff();
	alpha = prefs.ewaldAlpha();
	Atom** modelatoms = srcModel->atomArray();
	UnitCell& cell = srcModel->cell();

	aoff = startAtom_;
	for (m1=0; m1<nMolecules_; m1++)
	{
		// Add force contributions for atom pairs that are unbond or separated by at least three bonds
		for (i=0; i<nAtoms_; i++)
		{
			atomi = i+aoff;
			// Copy i's forces from the main array into a temporary array
			f_i = modelatoms[atomi]->f();
			for (j=i+1; j<nAtoms_; j++)
			{
				atomj = j+aoff;
				con = conMatrix_[i][j];
				if ((con > 2) || (con == 0))
				{
					vec_ij = cell.mimVector(modelatoms[atomi]->r(), modelatoms[atomj]->r());
					rij = vec_ij.magnitude();
					if (rij > cutoff) continue;
					alpharij = alpha * rij;
					factor = AtenMath::erfc(alpharij) + 2.0*alpharij/SQRTPI * exp(-(alpharij*alpharij));
					qqrij3 = (modelatoms[atomi]->charge() * modelatoms[atomj]->charge()) / (rij * rij * rij);
					factor = factor * qqrij3 * prefs.elecConvert();
					if (con == 3) factor *= elecScaleMatrix_[i][j];
					// Sum forces
					tempf = vec_ij * factor;
					f_i -= tempf;
					modelatoms[atomj]->f() += tempf;
				}
			}
			// Re-store forces on atom i
			modelatoms[atomi]->f() = f_i;
		}
		aoff += nAtoms_;
	}
	Messenger::exit("Pattern::ewaldRealIntraPatternForces");
}

void Pattern::ewaldRealInterPatternForces(Model* srcModel, Pattern* xpnode)
{
	// Calculate the real-space Ewald forces from interactions between different molecules
	// of this pattern and the one supplied. 
	Messenger::enter("Pattern::ewaldRealInterPatternForces");
	int i, j, aoff1, aoff2, m1, m2, start, finish, atomi, atomj;
	Vec3<double> vec_ij, f_i, tempf;
	double rij, factor, alpharij, qqrij3, cutoff, alpha;
	cutoff = prefs.elecCutoff();
	alpha = prefs.ewaldAlpha();
	Atom** modelatoms = srcModel->atomArray();
	UnitCell& cell = srcModel->cell();

	aoff1 = startAtom_;
	 // When we are considering the same node with itself, calculate for "m1=1,T-1 m2=2,T"
	this == xpnode ? finish = nMolecules_ - 1 : finish = nMolecules_;
	for (m1=0; m1<finish; m1++)
	{
		this == xpnode ? start = m1 + 1 : start = 0;
		aoff2 = xpnode->startAtom_ + start*xpnode->nAtoms_;
		for (m2=start; m2<xpnode->nMolecules_; m2++)
		{
			for (i=0; i<nAtoms_; i++)
			{
				atomi = i + aoff1;
				// Copy the current forces on i
				f_i = modelatoms[atomi]->f();
				for (j=0; j<xpnode->nAtoms_; j++)
				{
					atomj = j + aoff2;
					vec_ij = cell.mimVector(modelatoms[atomi]->r() ,modelatoms[atomj]->r());
					rij = vec_ij.magnitude();
					if (rij < cutoff)
					{
						alpharij = alpha * rij;
						factor = AtenMath::erfc(alpharij) + 2.0*alpharij/SQRTPI * exp(-(alpharij*alpharij));
						qqrij3 = (modelatoms[atomi]->charge() * modelatoms[atomj]->charge()) / (rij * rij * rij);
						factor = factor * qqrij3 * prefs.elecConvert();
						// Sum forces
						tempf = vec_ij * factor;
						f_i -= tempf;
						modelatoms[atomj]->f() += tempf;
					}
				}
				// Store the new forces on atom i
				modelatoms[atomi]->f() = f_i;
			}
			aoff2 += xpnode->nAtoms_;
		}
		aoff1 += nAtoms_;
	}
	
	Messenger::exit("Pattern::ewaldRealInterPatternForces");
}

// Reciprocal space forces
//				 N
//		  F(recip) = E   E q(j) 
//			    k/=0 j

void Pattern::ewaldReciprocalForces(Model* srcModel)
{
	// Calculate the reciprocal-space force contribution to the Ewald sum.
	// Must be called for the first pattern in the list only!
	Messenger::enter("Pattern::ewaldReciprocalForces");
	int kx, ky, kz, i;
	Vec3<double> k, cross_ab, cross_bc, cross_ca, perpl;
	Matrix rcell;
	double cutoffsq, magsq, exp1, alphasq, factor, force, sumcos, sumsin, xycos, xysin, alpha, rvolume;
	double* xyzcos, *xyzsin;

	// Grab fourier data
	int kmax = srcModel->fourierData().kMax();
	int nFourierAtoms = srcModel->fourierData().nAtoms();
	Vec3<int> kVec = srcModel->fourierData().kVec();
	const Array2D< Vec3<double> >& rCos = srcModel->fourierData().rCos();
	const Array2D< Vec3<double> >& rSin = srcModel->fourierData().rSin();

	alpha = prefs.ewaldAlpha();
	Atom** modelatoms = srcModel->atomArray();
	xyzcos = new double[srcModel->nAtoms()];
	xyzsin = new double[srcModel->nAtoms()];

	// Get reciprocal volume and cell vectors
	rvolume = srcModel->cell().reciprocalVolume();
	factor = 2.0 * rvolume * TWOPI * prefs.elecConvert();

	// Cutoff is the shortest component of kVec * perpendicular reciprocal cell lengths
	rcell = srcModel->cell().reciprocal();
	cross_ab = rcell.columnAsVec3(0) * rcell.columnAsVec3(1);
	cross_bc = rcell.columnAsVec3(1) * rcell.columnAsVec3(2);
	cross_ca = rcell.columnAsVec3(2) * rcell.columnAsVec3(0);
	perpl.set(rvolume / cross_ab.magnitude(), rvolume / cross_bc.magnitude(), rvolume / cross_ca.magnitude());
	perpl.x *= kVec.x;
	perpl.y *= kVec.y;
	perpl.z *= kVec.z;

	cutoffsq = perpl.min() * 1.05 * TWOPI;
	cutoffsq *= cutoffsq;
	alphasq = alpha * alpha;
	//printf("Cutoffsq = %f  (%f)\n",cutoffsq,sqrt(cutoffsq));

	for (kx=-kVec.x; kx<=kVec.x; ++kx)
	for (ky=-kVec.y; ky<=kVec.y; ++ky)
	for (kz=-kVec.z; kz<=kVec.z; ++kz)
	{
		if ((kx == 0) && (ky == 0) && (kz == 0)) continue;
		// Calculate magnitude of this vector
		k.set(kx,ky,kz);
		k = (rcell * TWOPI) * k;
		magsq = k.x*k.x + k.y*k.y + k.z*k.z;
		if (magsq > cutoffsq) continue;
		sumcos = 0.0;
		sumsin = 0.0;
		for (i=0; i<nFourierAtoms; ++i)
		{
			// Calculate k-vector (x*y)
			xycos = rCos.constRef(abs(kx),i).x * rCos.constRef(abs(ky),i).y -
				rSin.constRef(kmax+kx,i).x * rSin.constRef(kmax+ky,i).y;
			xysin = rCos.constRef(abs(kx),i).x * rSin.constRef(kmax+ky,i).y +
				rSin.constRef(kmax+kx,i).x * rCos.constRef(abs(ky),i).y;
			// Calculate k-vector (xy*z);
			xyzcos[i] = (xycos * rCos.constRef(abs(kz),i).z - xysin * rSin.constRef(kmax+kz,i).z) * modelatoms[i]->charge();
			xyzsin[i] = (xycos * rSin.constRef(kmax+kz,i).z + xysin * rCos.constRef(abs(kz),i).z) * modelatoms[i]->charge();
			sumcos += xyzcos[i];
			sumsin += xyzsin[i];
		}
// 	printf("%i %i %i %i %12.8f %12.8f\n",kx,ky,kz,i,sumcos,sumsin);
		// Calculate forces
		exp1= exp(-magsq/(4.0*alphasq))/magsq;
		for (i=0; i<nFourierAtoms; ++i)
		{
			force = exp1 * (xyzsin[i]*sumcos - xyzcos[i]*sumsin) * factor;
	//printf("force = %20.14e\n",force);
			modelatoms[i]->f() += k * force;
	//if (i == 0) printf("%i %i %i  %8.4f %8.4f %8.4f %8.4f\n",kx,ky,kz,force,kvec.x,kvec.y,kvec.z);
		}
	}

	Messenger::exit("Pattern::ewaldReciprocalForces");
}

void Pattern::ewaldCorrectForces(Model* srcModel)
{
	// Correct the Ewald forces due to bond / angle / torsion exclusions
	Messenger::enter("Pattern::ewaldCorrectForces");
	static int i, j, aoff, m1, atomi, atomj, con;
	static Vec3<double> vec_ij, tempf, f_i;
	static double rij, factor, qqrij3, alpharij, cutoff, alpha;
	cutoff = prefs.elecCutoff();
	alpha = prefs.ewaldAlpha();
	Atom** modelatoms = srcModel->atomArray();
	UnitCell& cell = srcModel->cell();

	aoff = startAtom_;
	for (m1=0; m1<nMolecules_; m1++)
	{
		// Subtract forces from intramolecular bonds, angles, and torsions
		for (i=0; i<nAtoms_-1; i++)
		{
			atomi = i+aoff;
			// Copy i's forces from the main array into a temporary array
			f_i = modelatoms[atomi]->f();
			for (j=i+1; j<nAtoms_; j++)
			{
				atomj = j+aoff;
				con = conMatrix_[i][j];
				if ((con < 4) && (con > 0))
				{
					vec_ij = cell.mimVector(modelatoms[atomi]->r(), modelatoms[atomj]->r());
					rij = vec_ij.magnitude();
					if (rij > cutoff) continue;
					// Calculate force to subtract
					alpharij = alpha * rij;
					factor = AtenMath::erf(alpharij) - 2.0*alpharij/SQRTPI * exp(-(alpharij*alpharij));
					qqrij3 = (modelatoms[atomi]->charge() * modelatoms[atomj]->charge()) / (rij * rij * rij);
					factor = factor * qqrij3 * prefs.elecConvert();
					factor *= (1.0 - elecScaleMatrix_[i][j]);
					// Sum forces (correcting force, so adding to f_i and subtracting from f_j)
					tempf = vec_ij * factor;
					f_i += tempf;
					modelatoms[atomj]->f() -= tempf;
				}
			}
			// Re-store forces on atom i
			modelatoms[atomi]->f() = f_i;
		}
		aoff += nAtoms_;
	}
	Messenger::exit("Pattern::ewaldCorrectForces");
}
