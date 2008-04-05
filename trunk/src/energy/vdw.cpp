/*
	*** van der Waals energy / force calculation
	*** src/energy/vdw.cpp
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

#include "classes/pattern.h"
#include "classes/forcefield.h"
#include "model/model.h"
#include "templates/vector3.h"
#include "classes/energystore.h"
#include "base/prefs.h"
#include <math.h>

// Intrapattern VDW energy
void Pattern::vdwIntraPatternEnergy(Model *srcmodel, EnergyStore *estore, int lonemolecule)
{
	// Calculate the internal VDW contributions with coordinates from *xcfg
	// Consider only the intrapattern interactions between atoms in individual molecules within the pattern.
	dbgBegin(Debug::Calls,"Pattern::vdwIntraPatternEnergy");
	static int n,aoff,m1,i,j, start1, finish1;
	static Vec3<double> mim_i;
	static double sigma, sigmar6, sigmar2, epsilon, rij, energy_inter, energy_intra, cutoff, vrs;
	static ForcefieldParams paramsi, paramsj;
	PatternAtom *pai, *paj;
	PatternBound *pb;
	cutoff = prefs.vdwCutoff();
	vrs = prefs.vdwScale();
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	energy_inter = 0.0;
	energy_intra = 0.0;
	start1 = (lonemolecule == -1 ? 0 : lonemolecule);
	finish1 = (lonemolecule == -1 ? nMols_ : lonemolecule+1);
	aoff = startAtom_ + start1*nAtoms_;
	for (m1=start1; m1<finish1; m1++)
	{
		// Calculate energies of atom pairs that are unbound or separated by more than three bonds
		i = -1;
		for (pai = atoms_.first(); pai != atoms_.last(); pai = pai->next)
		{
			i++;
			paramsi = atoms_[i]->data()->params();
			j = i;
			for (paj = pai->next; paj != NULL; paj = paj->next)
			{
				j++;
				if ((conMat_[i][j] > 3) || (conMat_[i][j] == 0))
				{
					mim_i = cell->mimd(modelatoms[i+aoff]->r(), modelatoms[j+aoff]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					paramsj = atoms_[j]->data()->params();
					// TODO Check for conflicting VDW types
					switch (atoms_[j]->data()->vdwForm())
					{
						case (VF_UNSPECIFIED):
							printf("Pattern::vdwIntraPatternEnergy <<<< VDW function is UNSPECIFIED >>>>\n");
							break;
						case (VF_LJ): // U = 4 * eps * [ (s/r)**12 - (s/r)**6 ]
							epsilon = 4.0 * sqrt( paramsi.data[VF_LJ_EPS] * paramsj.data[VF_LJ_EPS] );
							sigma = ( paramsi.data[VF_LJ_SIGMA] + paramsj.data[VF_LJ_SIGMA] ) * 0.5 * vrs;
							sigmar2 = (sigma / rij);
							sigmar2 *= sigmar2;
							sigmar6 = sigmar2 * sigmar2 * sigmar2;
							conMat_[i][j] == 0 ? energy_inter += epsilon * (sigmar6*sigmar6 - sigmar6)
								: energy_intra += epsilon * (sigmar6*sigmar6 - sigmar6);
							break;
					}
				}
			}
		}
		// Add scaled contributions from torsions
		for (pb = torsions_.first(); pb != NULL; pb = pb->next)
		{
			i = pb->atomId(0);
			j = pb->atomId(3);
			mim_i = cell->mimd(modelatoms[i+aoff]->r(), modelatoms[j+aoff]->r());
			rij = mim_i.magnitude();
			if (rij > cutoff) continue;
			paramsi = atoms_[i]->data()->params();
			paramsj = atoms_[j]->data()->params();
			// TODO Check for conflicting VDW types
			switch (atoms_[j]->data()->vdwForm())
			{
				case (VF_UNSPECIFIED):
					printf("Pattern::vdwIntraPatternEnergy <<<< VDW function is UNSPECIFIED >>>>\n");
					break;
				case (VF_LJ): // U = 4 * eps * [ (s/r)**12 - (s/r)**6 ]
					epsilon = 4.0 * sqrt( paramsi.data[VF_LJ_EPS] * paramsj.data[VF_LJ_EPS] );
					sigma = ( paramsi.data[VF_LJ_SIGMA] + paramsj.data[VF_LJ_SIGMA] ) * 0.5 * vrs;
					epsilon *= pb->data()->params().data[TF_VSCALE];
					sigmar2 = (sigma / rij);
					sigmar2 *= sigmar2;
					sigmar6 = sigmar2 * sigmar2 * sigmar2;
					energy_intra -= epsilon * (sigmar6*sigmar6 - sigmar6);
					break;
			}
		}
		aoff += nAtoms_;
	}
	// Add totals into EnergyStore
	estore->add(ET_VDWINTRA,energy_intra,id_);
	estore->add(ET_VDWINTER,energy_inter,id_,id_);
	//printf("TOTAL = %f %f\n",energy_intra,energy_inter);
	dbgEnd(Debug::Calls,"Pattern::vdwIntraPatternEnergy");
}

// Interpattern VDW energy
void Pattern::vdwInterPatternEnergy(Model *srcmodel, Pattern *otherPattern, EnergyStore *estore, int molId)
{
	// Calculate the VDW contribution to the energy from interactions between molecules of this pattern and the one supplied
	dbgBegin(Debug::Calls,"Pattern::vdwInterPatternEnergy");
	static int n1,n2,i,j,aoff1,aoff2,m1,m2,finish1,start1,start2,finish2;
	static Vec3<double> mim_i;
	static double sigma, sigmar2, sigmar6, epsilon, rij, energy_inter, cutoff, vrs;
	PatternAtom *pai, *paj;
	static ForcefieldParams paramsi, paramsj;
	cutoff = prefs.vdwCutoff();
	vrs = prefs.vdwScale();
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	energy_inter = 0.0;
	// Outer loop over molecules in *this* pattern
	// When we are considering the same node with itself, calculate for "m1=1,T-1 m2=2,T"
	if (molId == -1)
	{
		start1 = 0;
		finish1 = (this == otherPattern ? nMols_ - 1 : nMols_);
	}
	else
	{
		start1 = molId;
		finish1 = molId + 1;
	}
	aoff1 = startAtom_ + start1 * nAtoms_;
	for (m1=start1; m1<finish1; m1++)
	{
		// Inner loop - over *all* molecules in 'otherPattern'
		if (this == otherPattern)
		{
			// Same pattern - if a specific molecule was given then we loop over all molecules.
			// If not, loop over m1+1 to nMols_.
			if (molId == -1)
			{
				start2 = m1 + 1;
				finish2 = nMols_;
			}
			else
			{
				start2 = 0;
				finish2 = nMols_;
			}
		}
		else
		{
			// Simple - go over all molecules in the dissimilar pattern
			start2 = 0;
			finish2 = otherPattern->nMols_;
		}

		//if (m1 == 0) printf("IPE - finish1 = %i, start2 = %i, finish2 = %i\n",finish1,start2,finish2);
		aoff2 = otherPattern->startAtom_ + start2*otherPattern->nAtoms_;
		//printf("  VDWINTER2 %i %i %i\n",start2,finish2,aoff2);
		for (m2=start2; m2<finish2; m2++)
		{
			if ((this == otherPattern) && (molId == m2)) { aoff2 += nAtoms_; continue; }
			//printf("      m1/m2=%i/%i  aoff1/aoff2=%i/%i \n",m1,m2,aoff1,aoff2);
			i = -1;
			for (pai = atoms_.first(); pai != NULL; pai = pai->next)
			{
				i++;
				paramsi = pai->data()->params();
				j = -1;
				for (paj = otherPattern->atoms_.first(); paj != NULL; paj = paj->next)
				{
					j++;

					mim_i = cell->mimd(modelatoms[i+aoff1]->r(), modelatoms[j+aoff2]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					paramsj = paj->data()->params();
					// TODO Check for conflicting VDW types
					switch (atoms_[i]->data()->vdwForm())
					{
						case (VF_UNSPECIFIED):
							printf("Pattern::vdwInterPatternEnergy <<<< VDW function is UNSPECIFIED >>>>\n");
							break;
						case (VF_LJ): 
							epsilon = 4.0*sqrt(paramsi.data[VF_LJ_EPS] * paramsj.data[VF_LJ_EPS]);
							sigma = 0.5 * (paramsi.data[VF_LJ_SIGMA] + paramsj.data[VF_LJ_SIGMA]) * vrs;
							sigmar2 = (sigma / rij);
							sigmar2 *= sigmar2;
							sigmar6 = sigmar2 * sigmar2 * sigmar2;
							energy_inter += epsilon * (sigmar6*sigmar6 - sigmar6);
							break;
					}
				}
			}
			aoff2 += otherPattern->nAtoms_;
		}
		aoff1 += nAtoms_;
	}
	estore->add(ET_VDWINTER,energy_inter,id_,otherPattern->id_);
	dbgEnd(Debug::Calls,"Pattern::vdwInterPatternEnergy");
}

// Intrapattern VDW forces
void Pattern::vdwIntraPatternForces(Model *srcmodel)
{
	// Calculate the internal VDW contributions with coordinates from *xcfg
	// Consider only the intrapattern interactions between atoms in individual molecules within the pattern.
	// 'aoff' stores the atom number offset (molecule offset) but is *only* used for lookups in the coordinate
	// arrays since assuming the pattern definition is correct then the sigmas/epsilons in molecule 0 represent
	// those of all molecules.
	dbgBegin(Debug::Calls,"Pattern::vdwIntraPatternForces");
	static int n,i,j,aoff,m1;
	static Vec3<double> mim_i, f_i, tempf;
	static double sigma, sigmar2, sigmar6, epsilon, rij, factor, cutoff, vrs;
	static ForcefieldParams paramsi, paramsj;
	PatternAtom *pai, *paj;
	PatternBound *pb;
	cutoff = prefs.vdwCutoff();
	vrs = prefs.vdwScale();
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	aoff = startAtom_;
	for (m1=0; m1<nMols_; m1++)
	{
		// Add contributions from atom pairs that are unbound or separated by more than three bonds
		i = -1;
		for (pai = atoms_.first(); pai != atoms_.last(); pai = pai->next)
		{
			i++;
			paramsi = pai->data()->params();
			// Store temporary forces to avoid unnecessary array lookups
			f_i = modelatoms[i+aoff]->f();
			j = i;
			for (paj = pai->next; paj != NULL; paj = paj->next)
			{
				j++;
				if ((conMat_[i][j] > 3) || (conMat_[i][j] == 0))
				{
					mim_i = cell->mimd(modelatoms[i+aoff]->r(), modelatoms[j+aoff]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					paramsj = paj->data()->params();
					// TODO Check for conflicting VDW types
					switch (atoms_[j]->data()->vdwForm())
					{
						case (VF_UNSPECIFIED):
							printf("Pattern::vdwIntraPatternForces <<<< VDW function is UNSPECIFIED >>>>\n");
							break;
						case (VF_LJ): 
							epsilon = 48.0 * sqrt( paramsi.data[VF_LJ_EPS] * paramsj.data[VF_LJ_EPS] );
							sigma = ( paramsi.data[VF_LJ_SIGMA] + paramsj.data[VF_LJ_SIGMA] ) * 0.5 * vrs;
							sigmar2 = (sigma / rij);
							sigmar2 *= sigmar2;
							sigmar6 = sigmar2 * sigmar2 * sigmar2;
							factor = epsilon * sigmar6 * (sigmar6 - 0.5);
							factor = factor / (rij*rij);
							break;
					}
					// Add the forces (mim_i contains dx, dy, dz between i and j)
					tempf = mim_i * factor;
					f_i += tempf;
					modelatoms[j+aoff]->f() -= tempf;
				}
			}
			// Put the temporary forces back into the main array
			modelatoms[i+aoff]->f() = f_i;
		}
		// Add scaled contributions from torsions
		for (pb = torsions_.first(); pb != NULL; pb = pb->next)
		{
			i = pb->atomId(0);
			j = pb->atomId(3);
			mim_i = cell->mimd(modelatoms[i+aoff]->r(), modelatoms[j+aoff]->r());
			rij = mim_i.magnitude();
			if (rij > cutoff) continue;
			paramsi = atoms_[i]->data()->params();
			paramsj = atoms_[j]->data()->params();
			// TODO Check for conflicting VDW types
			switch (atoms_[j]->data()->vdwForm())
			{
				case (VF_UNSPECIFIED):
					printf("Pattern::vdwIntraPatternForces <<<< VDW function is UNSPECIFIED >>>>\n");
					break;
				case (VF_LJ): // U = 4 * eps * [ (s/r)**12 - (s/r)**6 ]
					epsilon = 48.0 * sqrt( paramsi.data[VF_LJ_EPS] * paramsj.data[VF_LJ_EPS] );
					epsilon *= pb->data()->params().data[TF_VSCALE];
					sigma = ( paramsi.data[VF_LJ_SIGMA] + paramsj.data[VF_LJ_SIGMA] ) * 0.5 * vrs;
					sigmar2 = (sigma / rij);
					sigmar2 *= sigmar2;
					sigmar6 = sigmar2 * sigmar2 * sigmar2;
					factor = epsilon * sigmar6 * (sigmar6 - 0.5);
					factor = factor / (rij*rij);
					break;
			}
			tempf = mim_i * factor;
			modelatoms[i+aoff]->f() += tempf;
			modelatoms[j+aoff]->f() -= tempf;
		}
		aoff += nAtoms_;
	}
	dbgEnd(Debug::Calls,"Pattern::vdwIntraPatternForces");
}

// Interpattern VDW forces
void Pattern::vdwInterPatternForces(Model *srcmodel, Pattern *xpnode)
{
	// Calculate the VDW forces from interactions between different molecules
	// of this pnode and the one supplied
	dbgBegin(Debug::Calls,"Pattern::vdwInterPatternForces");
	static int n1,n2,i,j,aoff1,aoff2,m1,m2,start,finish;
	static Vec3<double> mim_i, f_i, tempf;
	static double sigma, sigmar2, sigmar6, epsilon, rij, factor, cutoff, vrs;
	PatternAtom *pai, *paj;
	static ForcefieldParams paramsi, paramsj;
	cutoff = prefs.vdwCutoff();
	vrs = prefs.vdwScale();
	Atom **modelatoms = srcmodel->atomArray();
	Cell *cell = srcmodel->cell();
	aoff1 = startAtom_;
	// TODO Move loops so that we can load temporary forces for i then calculate all other forces on it in one go.
	 // When we are considering the same node with itself, calculate for "m1=1,T-1 m2=2,T"
        this == xpnode ? finish = nMols_ - 1 : finish = nMols_;
	for (m1=0; m1<finish; m1++)
	{
		this == xpnode ? start = m1 + 1 : start = 0;
		aoff2 = xpnode->startAtom_ + start*nAtoms_;
		for (m2=start; m2<xpnode->nMols_; m2++)
		{
			i = -1;
			for (pai = atoms_.first(); pai != NULL; pai = pai->next)
			{
				i++;
				paramsi = pai->data()->params();
				f_i = modelatoms[i+aoff1]->f();
				j = -1;
				for (paj = xpnode->atoms_.first(); paj != NULL; paj = paj->next)
				{
					j++;
					mim_i = cell->mimd(modelatoms[i+aoff1]->r(), modelatoms[j+aoff2]->r());
					rij = mim_i.magnitude();
					if (rij > cutoff) continue;
					paramsj = paj->data()->params();
					// TODO Check for conflicting VDW types
					switch (atoms_[j]->data()->vdwForm())
					{
						case (VF_UNSPECIFIED):
							printf("Pattern::vdwInterPatternForces <<<< VDW function is UNSPECIFIED >>>>\n");
							break;
						case (VF_LJ): 
							epsilon = 48.0*sqrt(paramsi.data[VF_LJ_EPS] * paramsj.data[VF_LJ_EPS]);
							sigma = 0.5 * (paramsi.data[VF_LJ_SIGMA] + paramsj.data[VF_LJ_SIGMA]) * vrs;
							sigmar2 = (sigma / rij);
							sigmar2 *= sigmar2;
							sigmar6 = sigmar2 * sigmar2 * sigmar2;
							factor = epsilon * sigmar6 * (sigmar6 - 0.5);
							factor = factor / (rij*rij);
							break;
					}
					// Add the forces (mim_i contains dx, dy, dz between i and j)
					tempf = mim_i * factor;
					f_i += tempf;
					modelatoms[j+aoff2]->f() -= tempf;
				}
				// Store temporary force array back into main force array
				modelatoms[i+aoff1]->f() = f_i;
			}
			aoff2 += xpnode->nAtoms_;
		}
		aoff1 += nAtoms_;
	}
	dbgEnd(Debug::Calls,"Pattern::vdwInterPatternForces");
}

//
// VDW Long Range Correction to Energy
// Frenkel and Smit, Academic Press, 1996, p32	    // Eq in this edition is wrong
//
//               /inf
// U(lr) = 0.5 * |     dr 4 * pi * r**2 * rho(r) * u(r)
//               /rcut
//
// Assume p(r) is equal to the (bulk) number density at r > rcut.
//
void Pattern::vdwCorrectEnergy(Cell *cell, EnergyStore *estore)
{
	// Calculate the long-range correction to the VDW energy
	dbgBegin(Debug::Calls,"Pattern::vdwCorrectEnergy");
	static int i, j;
	static Pattern *p1, *p2;
	static double energy, rho, cutoff, dudr, sigma, epsilon, sigmar3, sigmar9, volume, vrs;
	PatternAtom *pai, *paj;
	static ForcefieldParams paramsi, paramsj;
	cutoff = prefs.vdwCutoff();
	vrs = prefs.vdwScale();
	// The way the patterns are stored does not give direct access to the number of different
	// atom types used *or* the number densities of each. So, assume each atom in the pattern 
	// molecule is a unique VDW type and that the number density is nMols_/cellvolume
	volume = cell->volume();
	energy = 0.0;
	for (p1 = this; p1 != NULL; p1 = p1->next)
	{
		for (p2 = this; p2 != NULL; p2 = p2->next)
		{
			rho = (p1->nMols_ * p2->nMols_) /volume;
			i = 0;
			for (pai = p1->atoms_.first(); pai != NULL; pai = pai->next)
			{
				paramsi = pai->data()->params();
				j = 0;
				for (paj = p2->atoms_.first(); paj != NULL; paj = paj->next)
				{
					paramsj = paj->data()->params();
					// TODO Check for conflicting VDW types
					switch (p2->atoms_[j]->data()->vdwForm())
					{
						case (VF_UNSPECIFIED):
							printf("Pattern::vdwCorrectEnergy <<<< VDW function is UNSPECIFIED >>>>\n");
							break;
						case (VF_LJ):	// U = 4/3 * eps * sigma**3 * ( 1/3 * (s/r)**9 - (s/r)**3
							epsilon = sqrt(paramsi.data[VF_LJ_EPS] * paramsj.data[VF_LJ_EPS]);
							sigma = 0.5 * (paramsi.data[VF_LJ_SIGMA] + paramsj.data[VF_LJ_SIGMA]) * vrs;
							sigmar9 = sigma / cutoff;
							sigmar3 = sigmar9 * sigmar9 * sigmar9;
							sigmar9 = sigmar3 * sigmar3 * sigmar3;
							dudr = (4.0/3.0) * epsilon * ( sigmar9/3.0 - sigmar3 );
							dudr *= (sigma * sigma * sigma);
							break;
						default:
							msg(Debug::None,"VDW tail correction not implemented for LJ form %s.\n", text_from_VF(atoms_[j]->data()->vdwForm()));
							break;
					}
					energy += 2.0 * PI * rho * dudr;
				}
			}
		}
	}
	estore->add(ET_VDWTAIL,energy,-1);
	dbgEnd(Debug::Calls,"Pattern::vdwCorrectEnergy");
}

